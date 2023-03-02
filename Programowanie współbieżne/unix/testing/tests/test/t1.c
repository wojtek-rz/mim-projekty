#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

static int count = 0;

void sig_handler(int signo) { fprintf(stderr, "singint # %d\n", count++); }

int cur_time() {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

int main() {
  signal(SIGINT, sig_handler);
  fprintf(stderr, "Nice to meet you\n");

  int end = 3000 + cur_time();

  while (cur_time() < end) {
    int x = cur_time();
    usleep(end - x);
  }

  fprintf(stdout, "Goodbye\n");
  return 0;
}
