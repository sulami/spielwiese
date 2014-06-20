#include <ncurses.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

int main(int argc, char *argv[]) {
    unsigned int max_y, max_x;
    time_t t;
    struct tm *tt;
    char h[3], m[3], s[3];

    initscr();
    noecho();
    cbreak();
    nodelay(stdscr, true);
    curs_set(FALSE);

    if(!has_colors() || !can_change_color())
        exit(1);

    start_color();

    while(1) {
        if (getch() == 'q') {
            endwin();
            exit(0);
        }

        time(&t);
        tt = localtime(&t);
        strftime(h, 3, "%H", tt);
        strftime(m, 3, "%M", tt);
        strftime(s, 3, "%S", tt);

        init_color(COLOR_BLACK, atoi(h) * 10, atoi(m) * 10, atoi(s) * 10);
        init_pair(1, COLOR_WHITE, COLOR_BLACK);
        attron(COLOR_PAIR(1));

        getmaxyx(stdscr, max_y, max_x);

        clear();
        mvprintw(max_y / 2, max_x / 2 - 3, "#");
        mvprintw(max_y / 2, max_x / 2 - 2, h);
        mvprintw(max_y / 2, max_x / 2, m);
        mvprintw(max_y / 2, max_x / 2 + 2, s);
        refresh();

        usleep(30000);
    }
}

