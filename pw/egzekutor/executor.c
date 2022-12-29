#include "err.h"
#include "utils.h"
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>
#include "pipeline-utils.h"

enum {
    max_command_length = 512,
    max_n_tasks = 4096,
    max_line_length = 1024
};

struct LineReader {
    int pipe_dsc;
    pthread_mutex_t mutex;
    char *line;
};
typedef struct LineReader LineReader;

struct Task {
    long task_id;
    pid_t pid;
    LineReader stdout_reader;
    LineReader stderr_reader;
    pthread_t stdout_reader_thread;
    pthread_t stderr_reader_thread;
    bool ended;
};
typedef struct Task Task;

struct Tasks {
    Task tasks[max_n_tasks];
    pthread_t threads[max_n_tasks];
    long tasks_deployed;
};
typedef struct Tasks Tasks;

static void *line_reader_main(void *data) {
    LineReader *reader = data;
    FILE *stream = fdopen(reader->pipe_dsc, "r");
    size_t buff_size = max_line_length;
    char *tmp_line = (char *) malloc(max_line_length);

    while (getline(&tmp_line, &buff_size, stream) != -1) {
        pthread_mutex_lock(&reader->mutex);

        strcpy(reader->line, tmp_line);
        reader->line = trim_new_line_char(reader->line);
//        printf("Read new line: %s\n", reader->line);

        pthread_mutex_unlock(&reader->mutex);
    }

    free(tmp_line);
    fclose(stream);

    return 0;
}

void reader_init(LineReader *reader, int pipe_dsc) {
    reader->pipe_dsc = pipe_dsc;
    reader->line = (char *) malloc(max_line_length);
    reader->line[0] = '\0';
    ASSERT_ZERO(pthread_mutex_init(&reader->mutex, NULL));
}

void reader_destroy(LineReader *reader) {
    free(reader->line);
    ASSERT_ZERO(pthread_mutex_destroy(&reader->mutex));
}

void *task_main(void *data) {
    Task *task = data;

    pthread_create(&task->stdout_reader_thread, NULL, line_reader_main, &task->stdout_reader);
    pthread_create(&task->stderr_reader_thread, NULL, line_reader_main, &task->stderr_reader);

    pthread_join(task->stdout_reader_thread, NULL);
    pthread_join(task->stderr_reader_thread, NULL);

    int status;
    waitpid(task->pid, &status, 0);
    task->ended = true;
    if (WIFSIGNALED(status)) {
        printf("Task %ld ended: signalled.\n", task->task_id);
    } else if (WIFEXITED(status)) {
        printf("Task %ld ended: status %d.\n", task->task_id, WEXITSTATUS(status));
    }

    return 0;
}


long task_init(Tasks *tasks, int out_pipe_dsc, int err_pipe_dsc, pid_t pid) {
    long task_id = tasks->tasks_deployed++;
    Task *task = &tasks->tasks[task_id];
    task->pid = pid;
    task->task_id = task_id;
    task->ended = false;

    reader_init(&task->stdout_reader, out_pipe_dsc);
    reader_init(&task->stderr_reader, err_pipe_dsc);

    pthread_create(&tasks->threads[task_id], NULL, task_main, task);
    return task_id;
}

void task_destroy(Tasks *tasks, long task_id){
    Task *task = &tasks->tasks[task_id];
    reader_destroy(&task->stdout_reader);
    reader_destroy(&task->stderr_reader);
    ASSERT_ZERO(pthread_join(tasks->threads[task_id], NULL));
}

long task_kill(Tasks *tasks, long task_id){
    kill(tasks->tasks[task_id].pid, SIGINT);
}

void tasks_init(Tasks *tasks) {
    tasks->tasks_deployed = 0;
}

void tasks_destroy(Tasks *tasks){
    for (int i = 0; i < tasks->tasks_deployed; ++i){
        task_destroy(tasks, i);
    }
}

void print_thread_line(size_t task_no, LineReader *listener_data) {
    pthread_mutex_lock(&listener_data->mutex);
    printf("Task %zu stdout: '%s'.\n", task_no, listener_data->line);
    pthread_mutex_unlock(&listener_data->mutex);
}

void task_exec(Tasks *tasks, char **argv) {
    trim_new_line_char_v(argv);
    int pipe_dsc_out[2];
    int pipe_dsc_err[2];
    ASSERT_SYS_OK(pipe(pipe_dsc_out));
    ASSERT_SYS_OK(pipe(pipe_dsc_err));

    pid_t pid = fork();
    ASSERT_SYS_OK(pid);
    if (!pid) {
        // Close read descriptors.
        ASSERT_SYS_OK(close(pipe_dsc_out[0]));
        ASSERT_SYS_OK(close(pipe_dsc_err[0]));

        ASSERT_SYS_OK(dup2(pipe_dsc_out[1], STDOUT_FILENO));
        ASSERT_SYS_OK(dup2(pipe_dsc_err[1], STDERR_FILENO));
        ASSERT_SYS_OK(close(pipe_dsc_out[1]));
        ASSERT_SYS_OK(close(pipe_dsc_err[1]));  // Close the original copies.

        ASSERT_SYS_OK(execvp(argv[0], argv));
    } else {
        // Close write descriptors.
        ASSERT_SYS_OK(close(pipe_dsc_out[1]));
        ASSERT_SYS_OK(close(pipe_dsc_err[1]));

        long task_id = task_init(tasks, pipe_dsc_out[0], pipe_dsc_err[0], pid);
        printf("Task %ld started: pid %d.\n", task_id, pid);
    }
}

int main() {
    char buff[max_command_length];
    char **words;
    long task_id;

    Tasks tasks;
    tasks_init(&tasks);

    while (read_line(buff, max_command_length, stdin)) {
        if (buff[0] == '\n') continue;

        words = split_string(buff);
        char *command_name = trim_new_line_char(words[0]);

        if (strcmp(command_name, "run") == 0) {
            task_exec(&tasks, words + 1);

        } else if (strcmp(command_name, "quit") == 0) {
            tasks_destroy(&tasks);
            free_split_string(words);
            break;

        } else if (strcmp(command_name, "sleep") == 0) {
            long milliseconds = strtol(words[1], NULL, 10);
            usleep(milliseconds * 1000);

        } else {
            task_id = strtol(words[1], NULL, 10);
            if (strcmp(command_name, "out") == 0) {
                print_thread_line(task_id, &tasks.tasks[task_id].stdout_reader);

            } else if (strcmp(command_name, "err") == 0) {
                print_thread_line(task_id, &tasks.tasks[task_id].stderr_reader);

            } else if (strcmp(command_name, "kill") == 0) {
                task_kill(&tasks, task_id);

            }

        }

        free_split_string(words);
    }

    return 0;
}