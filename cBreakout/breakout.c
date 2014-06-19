#include <ncurses.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    initscr();
    noecho();
    cbreak();
    nodelay(stdscr, true);
    curs_set(FALSE);

    int x = 0;
    int y = 0;
    int max_x = 0;
    int max_y = 0;
    int direction_x = 1;
    int direction_y = 1;
    int plw = 2;

    getmaxyx(stdscr, max_y, max_x);

    int player = max_x / 2;

    while(1) {
        int ch, i;

        ch = getch();

        if (ch) {
            switch(ch) {
            case 'h':
                player = player > plw ? player - 1 : player;
                break;
            case 'l':
                player = player < (max_x - plw - 1) ? player + 1 : player;
                break;
            case 'q':
                endwin();
                exit(0);
            }
        }

        clear();
        for (i = player - plw; i <= player + plw; i++)
            mvprintw(max_y - 1, i, "-");
        mvprintw(y, x, "O");
        refresh();

        getmaxyx(stdscr, max_y, max_x);

        if (y >= max_y - 2) {
            if ((x - player) <= plw && (x - player) >= -plw) {
                direction_y *= -1;
            } else {
                endwin();
                exit(0);
            }
        }

        if (x >= max_x || x < 0)
            direction_x *= -1;

        if (y < 0)
            direction_y *= -1;

        x = x + direction_x;
        y = y + direction_y;

        usleep(30000);
    }
}

