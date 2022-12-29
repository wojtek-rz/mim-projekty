#include<stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include "err.h"
#include "pipeline-utils.h"

// Aby nie wypisywało debugu i nie czekało, trzeba zmienić na 0.
const int debug = 1;

int main(int argc, char* argv[]){
    int pipe_dsc[2];
    pid_t pid;

    if (argc > 1){
        for (size_t i = 1; i < argc - 1; i++){
            ASSERT_SYS_OK(pipe(pipe_dsc));
            ASSERT_SYS_OK(pid = fork());

            // W procedzie dziecku zamieniamy stdout -> pipeout, a stdin uznajemy, że jest dobre.
            // A w naszym procesie zamieniamy stdin -> pipein,
            // wtedy w następnym obrocie pętli nasz proces będzie miał dobrze ustawione wejście.
            // Ostatni proces nie tworzy pipe, stąd jest poza pętlą.
            if (pid == 0){
                ASSERT_SYS_OK(close(pipe_dsc[0]));

                ASSERT_SYS_OK(dup2(pipe_dsc[1], STDOUT_FILENO));
                ASSERT_SYS_OK(close(pipe_dsc[1]));

                if (debug){
                    fprintf(stderr, "============================\n");
                    print_open_descriptors();
                }
                ASSERT_SYS_OK(execlp(argv[i], argv[i], NULL));
            } else {
                if (debug) sleep(1);
                ASSERT_SYS_OK(close(pipe_dsc[1]));

                ASSERT_SYS_OK(dup2(pipe_dsc[0], STDIN_FILENO));
                ASSERT_SYS_OK(close(pipe_dsc[0]));
            }
        }

        ASSERT_SYS_OK(pid = fork());

        if (pid == 0){
            if (debug){
                fprintf(stderr, "============================\n");
                print_open_descriptors();
            }
            ASSERT_SYS_OK(execlp(argv[argc - 1], argv[argc - 1], NULL));
        } else {
            if (debug) sleep(1);
            // Procesy coś sobie wypiszą i same zamkną pipe'y (miejmy nadzieję). Możemy na nie poczekać.
            for (int i = 1; i < argc; i++){
                int res = wait(NULL);
                ASSERT_SYS_OK(res);
                if (debug){
                    fprintf(stderr, "poczekano na zakonczenie procesu: %d\n", res);
                }
            }
                
        }
    }


    return 0;
}