#include <ncurses.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <math.h>
#include <stdio.h>

#define BUFFERSIZE 256

int main(int argc, char *argv[])
{
    int max_x, max_y, ch,
        cpu_time_now, cpu_time, cpu_time_old, *cpu_hist, cpu_count = 0;
    long cpus = sysconf(_SC_NPROCESSORS_ONLN);

    cpu_hist = calloc(sizeof(int), BUFFERSIZE);
    if (!cpu_hist)
        exit(1);

    initscr();
    noecho();
    cbreak();
    nodelay(stdscr, true);
    curs_set(FALSE);

    while(1) {
        ch = getch();
        if (ch) {
            switch(ch) {
            case 'q':
                endwin();
                free(cpu_hist);
                exit(0);
            }
        }

        cpu_time_now = clock();
        cpu_time = cpu_time_old ? cpu_time_now - cpu_time_old : 0;
        cpu_time_old = cpu_time_now;

        cpu_hist[cpu_count] = cpu_time / cpus;

        getmaxyx(stdscr, max_y, max_x);
        clear();

        for (int i = 0; i <= max_x; i++) {
            int c = ((cpu_count - i) >= 0) ? cpu_count - i
                                           : cpu_count - i + BUFFERSIZE;
            if (cpu_hist[c]) {
                int h = max_y - max_y * cpu_hist[c] / 100;
                for (int y = max_y; y - h; y--)
                    mvprintw(y, max_x - i, ":");
            }
        }
        refresh();

        cpu_count = (cpu_count < BUFFERSIZE) ? cpu_count + 1 : 0;

        usleep(100000);
    }
}

