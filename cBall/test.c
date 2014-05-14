#include <ncurses.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
    initscr();
    noecho();
    curs_set(FALSE);

    int x = 0;
    int y = 0;
    int max_x = 0;
    int max_y = 0;
    int direction_x = 1;
    int direction_y = 1;

    while(1) {
        clear();
        mvprintw(y, x, "O");
        refresh();

        x = x + direction_x;
        y = y + direction_y;

        getmaxyx(stdscr, max_y, max_x);
        if (x >= max_x || x < 0) {
            direction_x *= -1;
        }
        if (y >= max_y || y < 0) {
            direction_y *= -1;
        }

        usleep(30000);
    }

    endwin();
}

