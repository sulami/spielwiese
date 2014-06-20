#include <ncurses.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    unsigned int max_y, max_x;

    initscr();
    noecho();
    cbreak();
    nodelay(stdscr, true);
    curs_set(FALSE);

    while(1) {
        if (getch() == 'q') {
            endwin();
            exit(0);
        }

        getmaxyx(stdscr, max_y, max_x);

        clear();
        refresh();

        usleep(30000);
    }
}

