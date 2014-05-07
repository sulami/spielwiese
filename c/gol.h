#include <stdlib.h>

struct line {
    int *col;
};

struct screen {
    struct line *lines;
};

struct screen *cgol_gen_start() {
    struct screen *screen = malloc(sizeof(struct screen));
    if (screen != NULL) {
        screen->lines = malloc(sizeof(struct line));
        return(screen);
    } else {
        return(NULL);
    }
}

int cgol_live() {
    return(0);
}

