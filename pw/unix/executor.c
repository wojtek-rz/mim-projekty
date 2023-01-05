#include "err.h"
#include "utils.h"
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>
#include <ctype.h>
#include "pipeline-utils.h"

enum {
    max_command_length = 511,
    max_n_tasks = 4096,
    max_line_length = 1024
};

struct LineReader;
typedef struct LineReader LineReader;
struct Task;
typedef struct Task Task;
struct EndedTasksQueue;
typedef struct EndedTasksQueue EndedTasksQueue;
struct Tasks;
typedef struct Tasks Tasks;

struct EndedTasksQueue {
    pthread_mutex_t mutex;
    long task_id_q[max_n_tasks];
    long return_code_q[max_n_tasks];
    bool all_ended[max_n_tasks];
    bool prevent_immediate_print;
    int n;
};

void ended_tasks_init(EndedTasksQueue *et){
    ASSERT_ZERO(pthread_mutex_init(&et->mutex, NULL));
    et->n = 0;
}

void destroy_ended_tasks(EndedTasksQueue *et){
    ASSERT_ZERO(pthread_mutex_destroy(&et->mutex));
}

void print_out_ended_task(long taks_id, long return_code){
    if (return_code == -1){
        printf("Task %ld ended: signalled.\n", taks_id);
    } else {
        printf("Task %ld ended: status %ld.\n", taks_id, return_code);
    }
}

void add_to_ended(EndedTasksQueue *et, long task_id, long return_code){
    ASSERT_ZERO(pthread_mutex_lock(&et->mutex));

    if (et->prevent_immediate_print){
        et->task_id_q[et->n] = task_id;
        et->return_code_q[et->n] = return_code;
        et->n++;
        et->all_ended[task_id] = true;
    } else {
        print_out_ended_task(task_id, return_code);
    }

    ASSERT_ZERO(pthread_mutex_unlock(&et->mutex));
}

void start_blocking_print(EndedTasksQueue *et){
    ASSERT_ZERO(pthread_mutex_lock(&et->mutex));
    et->prevent_immediate_print = true;

    ASSERT_ZERO(pthread_mutex_unlock(&et->mutex));
}

void stop_blocking_print(EndedTasksQueue *et){
    ASSERT_ZERO(pthread_mutex_lock(&et->mutex));
    et->prevent_immediate_print = false;

    for (size_t i = 0; i < et->n; i++){
        print_out_ended_task(et->task_id_q[i], et->return_code_q[i]);
    }
    et->n = 0;

    ASSERT_ZERO(pthread_mutex_unlock(&et->mutex));

}

bool check_if_ended(EndedTasksQueue *et, long task_id){
    ASSERT_ZERO(pthread_mutex_lock(&et->mutex));
    bool val = et->all_ended[task_id];
    ASSERT_ZERO(pthread_mutex_unlock(&et->mutex));
    return val;
}


struct LineReader {
    int pipe_dsc;
    pthread_mutex_t mutex;
    char line[max_line_length];
};

static void *line_reader_main(void *data) {
    LineReader *reader = data;
    FILE *stream = fdopen(reader->pipe_dsc, "r");
    char tmp_line[max_line_length];

    while (fgets(tmp_line, max_line_length, stream) != NULL) {
        ASSERT_ZERO(pthread_mutex_lock(&reader->mutex));

        strcpy(reader->line, tmp_line);
        trim_new_line_char(reader->line);
//        printf("Read new line: %s\n", reader->line);

        ASSERT_ZERO(pthread_mutex_unlock(&reader->mutex));
    }
    close(reader->pipe_dsc);

    return 0;
}

void reader_init(LineReader *reader, int pipe_dsc) {
    reader->pipe_dsc = pipe_dsc;
    reader->line[0] = '\0';
    ASSERT_ZERO(pthread_mutex_init(&reader->mutex, NULL));
}

void reader_destroy(LineReader *reader) {
    ASSERT_ZERO(pthread_mutex_destroy(&reader->mutex));
}

struct Task {
    long task_id;
    pid_t pid;
    LineReader stdout_reader;
    LineReader stderr_reader;
    pthread_t stdout_reader_thread;
    pthread_t stderr_reader_thread;
    EndedTasksQueue *ended_tasks;
};

void *task_main(void *data) {
    Task *task = data;

    int status;
    waitpid(task->pid, &status, 0);

    if (WIFSIGNALED(status)) {
        add_to_ended(task->ended_tasks, task->task_id, -1);
    } else if (WIFEXITED(status)) {
        add_to_ended(task->ended_tasks, task->task_id, WEXITSTATUS(status));
    }

    return 0;
}

struct Tasks {
    Task tasks[max_n_tasks];
    pthread_t threads[max_n_tasks];
    EndedTasksQueue ended_tasks;
    long tasks_deployed;
};

long task_init(Tasks *tasks, int out_pipe_dsc, int err_pipe_dsc, pid_t pid) {
    long task_id = tasks->tasks_deployed++;
    Task *task = &tasks->tasks[task_id];
    task->pid = pid;
    task->task_id = task_id;
    task->ended_tasks = &tasks->ended_tasks;

    reader_init(&task->stdout_reader, out_pipe_dsc);
    reader_init(&task->stderr_reader, err_pipe_dsc);

    pthread_create(&tasks->threads[task_id], NULL, task_main, task);
    pthread_create(&task->stdout_reader_thread, NULL, line_reader_main, &task->stdout_reader);
    pthread_create(&task->stderr_reader_thread, NULL, line_reader_main, &task->stderr_reader);

    return task_id;
}

void task_destroy(Tasks *tasks, long task_id){
    Task *task = &tasks->tasks[task_id];

    if (!check_if_ended(&tasks->ended_tasks, task_id)){
        kill(task->pid, SIGTERM);
    }

    ASSERT_ZERO(pthread_join(task->stdout_reader_thread, NULL));
    ASSERT_ZERO(pthread_join(task->stderr_reader_thread, NULL));
    ASSERT_ZERO(pthread_join(tasks->threads[task_id], NULL));
    reader_destroy(&task->stdout_reader);
    reader_destroy(&task->stderr_reader);
}

void tasks_init(Tasks *tasks) {
    tasks->tasks_deployed = 0;
    ended_tasks_init(&tasks->ended_tasks);
}

void tasks_destroy(Tasks *tasks){
    for (int i = 0; i < tasks->tasks_deployed; ++i){
        task_destroy(tasks, i);
    }
//    print_and_remove_ended(&tasks->ended_tasks);
    destroy_ended_tasks(&tasks->ended_tasks);
}

void print_task_out_line(Tasks *tasks, size_t task_no) {
    Task *task = &tasks->tasks[task_no];
    pthread_mutex_lock(&task->stdout_reader.mutex);
    printf("Task %zu stdout: '%s'.\n", task_no, task->stdout_reader.line);
    pthread_mutex_unlock(&task->stdout_reader.mutex);
}

void print_task_err_line(Tasks *tasks, size_t task_no) {
    LineReader *lr = &tasks->tasks[task_no].stderr_reader;
    pthread_mutex_lock(&lr->mutex);
    printf("Task %zu stderr: '%s'.\n", task_no, lr->line);
    pthread_mutex_unlock(&lr->mutex);
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

Tasks tasks;

int main() {
    char buff[max_command_length];
    char **words;
    long task_id;

    tasks_init(&tasks);

    while (fgets(buff, max_command_length, stdin) != NULL) {
        if (isspace(buff[0])) continue;
//        fprintf(stderr, "read line: %s\n", buff);
        words = split_string(buff);
        char *command_name = trim_new_line_char(words[0]);

//        char buf2[max_command_length];
//        strcpy(buf2, buff);
//        buf2[strlen(buf2) - 1] = '\0';
//        fprintf(stderr, "Processing line '%s'\n", buf2);
        start_blocking_print(&tasks.ended_tasks);

        if (strcmp(command_name, "run") == 0) {
            task_exec(&tasks, words + 1);

        } else if (strcmp(command_name, "quit") == 0) {
            free_split_string(words);
            stop_blocking_print(&tasks.ended_tasks);
            break;

        } else if (strcmp(command_name, "sleep") == 0) {
            long milliseconds = strtol(words[1], NULL, 10);
            usleep(milliseconds * 1000);

        } else {
            task_id = strtol(words[1], NULL, 10);
            if (strcmp(command_name, "out") == 0) {
                print_task_out_line(&tasks, task_id);
            } else if (strcmp(command_name, "err") == 0) {
                print_task_err_line(&tasks, task_id);
            } else if (strcmp(command_name, "kill") == 0) {
                kill(tasks.tasks[task_id].pid, SIGINT);
            }
        }

        free_split_string(words);
        stop_blocking_print(&tasks.ended_tasks);
    }

    tasks_destroy(&tasks);

    return 0;
}