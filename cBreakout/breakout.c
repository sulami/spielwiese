#include <ncurses.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    initscr();
    noecho();
    cbreak();
    nodelay(stdscr, true);
    curs_set(FALSE);

    int l, m;
    int x = 0;
    int y = 0;
    int max_x = 0;
    int max_y = 0;
    int direction_x = 1;
    int direction_y = 1;
    int plw = 2;

    getmaxyx(stdscr, max_y, max_x);

    int player = max_x / 2;
    bool scrn[max_x][max_y];

    /* init empty field */
    for (l = 0; l < max_x; l++) {
        for (m = 0; m < max_y; m++) {
            scrn[l][m] = false;
        }
    }

    /* add blocks, x: 10-90%, y: 10-30% */
    for (l = max_x / 10; max_x * 9 / 10 > l; l++) {
        for (m = max_y / 10; max_y * 3 / 10 > m; m++) {
            scrn[l][m] = true;
        }
    }

    while(1) {
        int ch, i, j;

        /* get input */
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
        /* player */
        for (i = player - plw; i <= player + plw; i++)
            mvprintw(max_y - 1, i, "-");
        /* blocks */
        for (i = 0; i < max_x; i++) {
            for (j = 0; j < max_y; j++) {
                if (scrn[i][j])
                    mvprintw(j, i, "X");
            }
        }
        /* ball */
        mvprintw(y, x, "O");
        refresh();

        /* we hit bottom */
        if (y >= max_y - 2) {
            if ((x - player) <= plw && (x - player) >= -plw) {
                direction_y *= -1;
            } else {
                endwin();
                exit(0);
            }
        }

        /* we hit the sides */
        if (x >= max_x || x < 0)
            direction_x *= -1;

        /* we hit the top */
        if (y < 0)
            direction_y *= -1;

        x = x + direction_x;
        y = y + direction_y;

        usleep(30000);
    }
}

