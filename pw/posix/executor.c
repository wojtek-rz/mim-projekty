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
    max_command_length = 511,
    max_n_tasks = 4096,
    max_line_length = 1022
};

struct LineReader;
typedef struct LineReader LineReader;
struct Task;
typedef struct Task Task;
struct WriteLock;
typedef struct WriteLock WriteLock;
struct Tasks;
typedef struct Tasks Tasks;

struct WriteLock {
    pthread_mutex_t mutex;
    pthread_cond_t priority_waiting;
    pthread_cond_t normal_waiting;
    int n_priority_waiting;
    int n_normal_waiting;
    bool occupied;
};

void write_lock_init(WriteLock *wl){
    ASSERT_ZERO(pthread_mutex_init(&wl->mutex, NULL));
    ASSERT_ZERO(pthread_cond_init(&wl->priority_waiting, NULL));
    ASSERT_ZERO(pthread_cond_init(&wl->normal_waiting, NULL));
    wl->n_priority_waiting = 0;
    wl->n_normal_waiting = 0;
    wl->occupied = false;
}

void write_lock_destroy(WriteLock *wl){
    ASSERT_ZERO(pthread_cond_destroy(&wl->priority_waiting));
    ASSERT_ZERO(pthread_cond_destroy(&wl->normal_waiting));
    ASSERT_ZERO(pthread_mutex_destroy(&wl->mutex));
}

void priority_acquire(WriteLock *wl){
    ASSERT_ZERO(pthread_mutex_lock(&wl->mutex));
    wl->n_priority_waiting++;
    while (wl->occupied){
        ASSERT_ZERO(pthread_cond_wait(&wl->priority_waiting, &wl->mutex));
    }
    wl->n_priority_waiting--;
    wl->occupied = true;

    ASSERT_ZERO(pthread_mutex_unlock(&wl->mutex));
}

void normal_acquire(WriteLock *wl){
    ASSERT_ZERO(pthread_mutex_lock(&wl->mutex));
    wl->n_normal_waiting++;
    while (wl->occupied || wl->n_priority_waiting > 0){
        ASSERT_ZERO(pthread_cond_wait(&wl->normal_waiting, &wl->mutex));
    }
    wl->n_normal_waiting--;
    wl->occupied = true;

    ASSERT_ZERO(pthread_mutex_unlock(&wl->mutex));
}

void release(WriteLock *wl){
    ASSERT_ZERO(pthread_mutex_lock(&wl->mutex));
    wl->occupied = false;

    if (wl->n_priority_waiting > 0)
        ASSERT_ZERO(pthread_cond_signal(&wl->priority_waiting));
    else if (wl->n_normal_waiting > 0)
        ASSERT_ZERO(pthread_cond_signal(&wl->normal_waiting));

    ASSERT_ZERO(pthread_mutex_unlock(&wl->mutex));
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

    fclose(stream);

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
    WriteLock *write_lock;
    bool ended;
};

void *task_main(void *data) {
    Task *task = data;

    pthread_create(&task->stdout_reader_thread, NULL, line_reader_main, &task->stdout_reader);
    pthread_create(&task->stderr_reader_thread, NULL, line_reader_main, &task->stderr_reader);

    pthread_join(task->stdout_reader_thread, NULL);
    pthread_join(task->stderr_reader_thread, NULL);

    int status;
    waitpid(task->pid, &status, 0);

    priority_acquire(task->write_lock);
    task->ended = true;
    if (WIFSIGNALED(status)) {
        printf("Task %ld ended: signalled.\n", task->task_id);
    } else if (WIFEXITED(status)) {
        printf("Task %ld ended: status %d.\n", task->task_id, WEXITSTATUS(status));
    }
    release(task->write_lock);

    return 0;
}

struct Tasks {
    Task tasks[max_n_tasks];
    pthread_t threads[max_n_tasks];
    WriteLock write_lock;
    long tasks_deployed;
};

long task_init(Tasks *tasks, int out_pipe_dsc, int err_pipe_dsc, pid_t pid) {
    long task_id = tasks->tasks_deployed++;
    Task *task = &tasks->tasks[task_id];
    task->pid = pid;
    task->task_id = task_id;
    task->ended = false;
    task->write_lock = &tasks->write_lock;

    reader_init(&task->stdout_reader, out_pipe_dsc);
    reader_init(&task->stderr_reader, err_pipe_dsc);

    pthread_create(&tasks->threads[task_id], NULL, task_main, task);
    return task_id;
}

void task_destroy(Tasks *tasks, long task_id){
    Task *task = &tasks->tasks[task_id];

    if (!task->ended){
        kill(task->pid, SIGTERM);
    }

    ASSERT_ZERO(pthread_join(tasks->threads[task_id], NULL));
    reader_destroy(&task->stdout_reader);
    reader_destroy(&task->stderr_reader);
}

void tasks_init(Tasks *tasks) {
    tasks->tasks_deployed = 0;
    write_lock_init(&tasks->write_lock);
}

void tasks_destroy(Tasks *tasks){
    for (int i = 0; i < tasks->tasks_deployed; ++i){
        task_destroy(tasks, i);
    }
    write_lock_destroy(&tasks->write_lock);
}

void print_task_out_line(Tasks *tasks, size_t task_no) {
    LineReader *lr = &tasks->tasks[task_no].stdout_reader;
    pthread_mutex_lock(&lr->mutex);
    printf("Task %zu stdout: '%s'.\n", task_no, lr->line);
    pthread_mutex_unlock(&lr->mutex);
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
        if (buff[0] == '\n') continue;
//        printf("read line: %s\n", buff);
        words = split_string(buff);
        char *command_name = trim_new_line_char(words[0]);

        normal_acquire(&tasks.write_lock);
//        char buf2[max_command_length];
//        strcpy(buf2, buff);
//        buf2[strlen(buf2) - 1] = '\0';
//        printf("Processing line '%s'\n", buf2);

        if (strcmp(command_name, "run") == 0) {
            task_exec(&tasks, words + 1);

        } else if (strcmp(command_name, "quit") == 0) {
            free_split_string(words);
            release(&tasks.write_lock);
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
        release(&tasks.write_lock);
    }

    tasks_destroy(&tasks);

    return 0;
}