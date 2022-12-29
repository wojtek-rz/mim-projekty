#include "err.h"
#include "utils.h"
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>
#include "pipeline-utils.h"

const size_t max_command_length = 512;
const size_t max_n_tasks = 4096;
const size_t max_line_length = 1024;

struct LineReader{
    int pipe_dsc;
    pthread_mutex_t mutex;
    char *line;
};
typedef struct LineReader LineReader;

struct Task{
    long task_id;
    pid_t pid;
    LineReader stdout_reader;
    LineReader stderr_reader;
    pthread_t stdout_reader_thread;
    pthread_t stderr_reader_thread;
};
typedef struct Task Task;

struct Tasks {
    Task tasks[max_n_tasks];
    pthread_t threads[max_n_tasks];
    long tasks_deployed;
};
typedef struct Tasks Tasks;


void *line_reader_main(void *data){
    LineReader *reader = data;
    FILE * stream = fdopen(reader->pipe_dsc, "r");
    size_t buff_size = max_line_length;
    char *tmp_line = (char*)malloc(max_line_length);
    tmp_line[0] = '\0';

    while (getline(&tmp_line, &buff_size, stream) != -1){
        pthread_mutex_lock(&reader->mutex);

        strcpy(reader->line, tmp_line);
        trim_new_line_char(reader->line);
//        printf("Read new line: %s\n",reader->line);

        pthread_mutex_unlock(&reader->mutex);
    }

    fclose(stream);
    free(tmp_line);
    return 0;
}

void *task_main(void *data){
    Task *task = data;

    pthread_create(&task->stdout_reader_thread, NULL, line_reader_main, &task->stdout_reader);
    pthread_create(&task->stderr_reader_thread, NULL, line_reader_main, &task->stderr_reader);

    pthread_join(task->stdout_reader_thread, NULL);
    pthread_join(task->stderr_reader_thread, NULL);

    int status;
    waitpid(task->pid, &status, 0);
    if (WIFSIGNALED(status)){
        printf("Task %ld ended: signalled.\n", task->task_id);
    } else if (WIFEXITED(status)){
        printf("Task %ld ended: status %d.\n", task->task_id, WEXITSTATUS(status));
    }

    return 0;
}

void init_reader(LineReader *reader, int pipe_dsc){
    reader->pipe_dsc = pipe_dsc;
    ASSERT_ZERO(pthread_mutex_init(&reader->mutex, NULL));
    reader->line = (char*)malloc(max_line_length);
}

void init_task(long task_id, int stdout_pipe_dsc, int stderr_pipe_dsc, pid_t pid){
    pthread_t *thread_stdout = &threads[task_id];
    Task *task = &tasks[task_id];
    task->pid = pid;
    task->task_id = task_id;

    init_reader(&tasks->stdout_reader, stdout_pipe_dsc);
    init_reader(&tasks->stderr_reader, stderr_pipe_dsc);

    pthread_create(thread_stdout, NULL, task_main, task);
}

void print_thread_line(size_t task_no, LineReader *listener_data){
    pthread_mutex_lock(&listener_data->mutex);
    printf("Task %zu stdout: '%s'.\n", task_no, listener_data->line);
    pthread_mutex_unlock(&listener_data->mutex);
}


void run(char ** argv){
    trim_new_line_char_v(argv);
    int pipe_dsc_stdout[2];
    int pipe_dsc_stderr[2];
    ASSERT_SYS_OK(pipe(pipe_dsc_stdout));
    ASSERT_SYS_OK(pipe(pipe_dsc_stderr));

    pid_t pid = fork();
    ASSERT_SYS_OK(pid);
    if(!pid) {
        // Close read descriptors.
        ASSERT_SYS_OK(close(pipe_dsc_stdout[0]));
        ASSERT_SYS_OK(close(pipe_dsc_stderr[0]));

        ASSERT_SYS_OK(dup2(pipe_dsc_stdout[1], STDOUT_FILENO));
        ASSERT_SYS_OK(dup2(pipe_dsc_stderr[1], STDERR_FILENO));
        ASSERT_SYS_OK(close(pipe_dsc_stdout[1]));
        ASSERT_SYS_OK(close(pipe_dsc_stderr[1]));  // Close the original copies.

        ASSERT_SYS_OK(execvp(argv[0], argv));
//        ASSERT_SYS_OK(execlp("./task1", "./task1", NULL));
    } else {
        // Close write descriptors.
        ASSERT_SYS_OK(close(pipe_dsc_stdout[1]));
        ASSERT_SYS_OK(close(pipe_dsc_stderr[1]));

        init_task(tasks_deployed, pipe_dsc_stdout[0], pipe_dsc_stderr[0], pid);
        printf("Task %ld started: pid %d.\n", tasks_deployed, pid);
        tasks_deployed++;
    }
}

int main(){
    char buff[MAX_COMMAND_LENGTH];
    char **words;
    int i;
    long task_id;

    while (read_line(buff, MAX_COMMAND_LENGTH, stdin)){
        words = split_string(buff);

        if (strcmp(words[0], "run") == 0){
            run(words + 1);
        } else if (strcmp(words[0], "quit") == 0){
            break;
        } else if (strcmp(words[0], "sleep") == 0){
            long milliseconds = strtol(words[1], NULL, 10);
            usleep(milliseconds * 1000);
        }
        else {
            task_id = strtol(words[1], NULL, 10);
            if (strcmp(words[0], "out") == 0){
                print_thread_line(task_id, &tasks[task_id].stdout_reader);
            } else if (strcmp(words[0], "err") == 0){
                print_thread_line(task_id, &tasks[task_id].stderr_reader);
            } else if (strcmp(words[0], "kill") == 0){
                kill(tasks[task_id].pid, SIGINT);
            }
        }

        free_split_string(words);
    }

    return 0;
}