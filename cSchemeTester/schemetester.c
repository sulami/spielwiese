#include <ncurses.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char *argv[])
{
    int i;
    int main, off;

    if (argc != 3)
        exit(1);

    main = atoi(argv[1]);
    off = atoi(argv[2]);

    initscr();
    noecho();
    curs_set(FALSE);

    if(!has_colors() || !can_change_color())
        exit(1);

    start_color();

    init_color(COLOR_BLACK,     off,    off,    off  );
    init_color(COLOR_RED,       main,   off,    off  );
    init_color(COLOR_GREEN,     off,    main,   off  );
    init_color(COLOR_YELLOW,    main,   main,   off  );
    init_color(COLOR_BLUE,      off,    off,    main );
    init_color(COLOR_MAGENTA,   main,   off,    main );
    init_color(COLOR_CYAN,      off,    main,   main );
    init_color(COLOR_WHITE,     main,   main,   main );

    init_pair(1, COLOR_BLACK, COLOR_BLACK);
    init_pair(2, COLOR_RED, COLOR_BLACK);
    init_pair(3, COLOR_GREEN, COLOR_BLACK);
    init_pair(4, COLOR_YELLOW, COLOR_BLACK);
    init_pair(5, COLOR_BLUE, COLOR_BLACK);
    init_pair(6, COLOR_MAGENTA, COLOR_BLACK);
    init_pair(7, COLOR_CYAN, COLOR_BLACK);
    init_pair(8, COLOR_WHITE, COLOR_BLACK);

    clear();

    for (i = 0; i < 8; i++) {
        attron(COLOR_PAIR(i + 1));
        mvprintw(i,  0, "Color%i", i);
        attroff(COLOR_PAIR(i + 1));
    }

    refresh();

    while (1) {
        if (getch() == 'q') {
            use_default_colors();
            use_default_colors();
            endwin();
            exit(0);
        }
    }
}

