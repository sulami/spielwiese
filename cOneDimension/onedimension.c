#include <ncurses.h>
#include <stdlib.h>
#include <unistd.h>

static int max_x, max_y, ch, x;

void event_loop()
{
    ch = getch();
    if (ch) {
        switch(ch) {
        case 'z':
            x -= 1;
            break;
        case 'm':
            x += 1;
            break;
        case 'q':
            endwin();
            exit(0);
        }
    }

    getmaxyx(stdscr, max_y, max_x);

    clear();
    mvprintw(max_y / 2, x, "O");
    refresh();
}

int main(int argc, char *argv[])
{
    initscr();
    raw();
    noecho();
    nodelay(stdscr, true);
    curs_set(FALSE);
    getmaxyx(stdscr, max_y, max_x);

    x = max_x / 2;

    while(1) {
        event_loop();
        usleep(30000);
    }

    endwin();
    return 0;
}

