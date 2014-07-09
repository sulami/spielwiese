#include <ncurses.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#define BUFFERSIZE 256
#define STATLEN 128

/* string splitting function from some stackoverflow post */
char **str_split(char* a_str, const char a_delim)
{
    char **result    = 0;
    size_t count     = 0;
    char *tmp        = a_str;
    char *last_comma = 0;
    char delim[2];
    delim[0] = a_delim;
    delim[1] = 0;

    /* Count how many elements will be extracted. */
    while (*tmp) {
        if (a_delim == *tmp) {
            count++;
            last_comma = tmp;
        }
        tmp++;
    }

    /* Add space for trailing token. */
    count += last_comma < (a_str + strlen(a_str) - 1);

    /* Add space for terminating null string so caller
       knows where the list of returned strings ends. */
    count++;

    result = malloc(sizeof(char *) * count);

    if (result) {
        size_t idx  = 0;
        char *token = strtok(a_str, delim);

        while (token) {
            assert(idx < count);
            *(result + idx++) = strdup(token);
            token = strtok(0, delim);
        }
        *(result + idx) = 0;
    }

    return result;
}

static unsigned long idle(void)
{
    char *cpustats = calloc(sizeof(char), STATLEN),
         **tmp = malloc(sizeof(char) * STATLEN);
    unsigned long retval;
    FILE *stat = fopen("/proc/stat", "r");
    int b = 0, e;

    fgets(cpustats, STATLEN, stat);
    fclose(stat);

    tmp = str_split(cpustats, ' ');
    retval = strtoul(*(tmp + 4), 0, 10);

    return retval;
}

int main(int argc, char *argv[])
{
    int max_x, max_y, ch, cpu_count = 0;
    unsigned long cpu, cpu_old = 0;
    unsigned long *cpu_hist;
    unsigned long cpus = sysconf(_SC_NPROCESSORS_ONLN);

    cpu_hist = calloc(sizeof(long), BUFFERSIZE);
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

        cpu = idle();
        cpu_hist[cpu_count] = 100 - (cpu - cpu_old) * 10 / cpus;
        cpu_old = cpu;

        getmaxyx(stdscr, max_y, max_x);
        clear();

        for (int i = 0; i <= max_x; i++) {
            int c = ((cpu_count - i) >= 0) ? cpu_count - i
                                           : cpu_count - i + BUFFERSIZE;
            if (cpu_hist[c]) {
                int h = max_y - max_y * cpu_hist[c] / 100;
                for (int y = h; y <= max_y; y++)
                    mvprintw(y, max_x - i, "|");
            }
        }
        mvprintw(0, 0, "%3d%%", cpu_hist[cpu_count]);
        refresh();

        cpu_count = (cpu_count < BUFFERSIZE) ? cpu_count + 1 : 0;

        usleep(100000);
    }
}

