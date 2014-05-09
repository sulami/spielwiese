#include <stdlib.h>
#include <time.h>

int ***cgol_gen_start(unsigned int width, unsigned int height);
int cgol_live(void);

int ***cgol_gen_start(unsigned int width, unsigned int height) {
    int **screen = malloc(sizeof(int) * width);
    if (screen != NULL) {
        srand((unsigned int)time(NULL));
        unsigned int i;
        for (i=0; i<height; i++) {
            screen[i] = malloc(sizeof(int) * height);
            unsigned int j;
            for (j=0; j<width; j++) {
                int randint = rand() % 2;
                screen[i][j] = randint;
            }
        }
        int ***ptr = &screen;
        return(ptr);
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

