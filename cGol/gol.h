#include <stdlib.h>
#include <time.h>

void *cgol_gen_start(unsigned int width, unsigned int height);
int cgol_live(void);

void *cgol_gen_start(unsigned int width, unsigned int height) {
    int (*screen)[height][width] = malloc(sizeof(int) * width * height);
    if (screen != NULL) {
        srand((unsigned int)time(NULL));
        unsigned int i;
        for (i=0; i<height; i++) {
            unsigned int j;
            for (j=0; j<width; j++) {
                int randint = rand() % 2;
                (*screen)[i][j] = randint;
            }
        }
        return(screen);
    } else {
        return(NULL);
    }
}

int cgol_live() {
    return(0);
}

/*
vim: ft=c:
*/

