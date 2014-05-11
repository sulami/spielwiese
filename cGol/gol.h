#include <stdlib.h>
#include <time.h>

void *cgol_gen_start(unsigned int width, unsigned int height);
int *cgol_next_gen(int *screen, unsigned int width, unsigned int height);
int cgol_live(void);

void *cgol_gen_start(unsigned int width, unsigned int height) {
    int (*screen)[height][width] = malloc(sizeof(int) * width * height);
    if (screen != NULL) {
        srand((unsigned int)time(NULL));
        for (unsigned int i=0; i<height; i++) {
            for (unsigned int j=0; j<width; j++) {
                (*screen)[i][j] = rand() % 2;
            }
        }
        return(screen);
    } else {
        return(NULL);
    }
}

int *cgol_next_gen(int *screen, unsigned int width, unsigned int height) {
    int *tmp = malloc(sizeof(int) * width * height);
    for (unsigned int i=0; i<height; i++) {
        for (unsigned int j=0; j<width; j++) {
            int neighbours_alive = 0;
            if (i > 0 && j > 0) {
                *(screen + (i - 1) * width + j - 1) ? neighbours_alive++ : 0 ;
            }
            if (i > 0) {
                *(screen + (i - 1) * width + j) ? neighbours_alive++ : 0 ;
            }
            if (i > 0 && j < (width - 1)) {
                *(screen + (i - 1) * width + j + 1) ? neighbours_alive++ : 0 ;
            }
            if (j > 0) {
                *(screen + i * width + j - 1) ? neighbours_alive++ : 0 ;
            }
            if (j < (width - 1)) {
                *(screen + i * width + j + 1) ? neighbours_alive++ : 0 ;
            }
            if (i < (height - 1) && j > 0) {
                *(screen + (i + 1) * width + j - 1) ? neighbours_alive++ : 0 ;
            }
            if (i < (height - 1)) {
                *(screen + (i + 1) * width + j) ? neighbours_alive++ : 0 ;
            }
            if (i < (height - 1) && j < (width - 1)) {
                *(screen + (i + 1) * width + j + 1) ? neighbours_alive++ : 0 ;
            }
            if (neighbours_alive == 2 && *(screen + i * width + j)) {
                *(tmp + i * width + j) = 1;
            } else if (neighbours_alive == 3) {
                *(tmp + i * width + j) = 1;
            } else {
                *(tmp + i * width + j) = 0;
            }
        }
    }
    free(screen);
    return(tmp);
}

int cgol_live() {
    return(0);
}

/*
vim: ft=c:
*/

