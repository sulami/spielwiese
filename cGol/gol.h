#include <stdlib.h>
#include <time.h>
#include <unistd.h>

void *cgol_gen_start(unsigned int width, unsigned int height);
int *cgol_next_gen(int *screen, unsigned int width, unsigned int height);
void cgol_print_screen(int *screen, unsigned int width, unsigned int height);
void cgol_live(unsigned int width, unsigned int height, int duration);

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

            /* top left */
            if (i > 0 && j > 0) {
                *(screen + (i - 1) * width + j - 1) ? neighbours_alive++ : 0 ;
            } else if (i > 0) { /* left border */
                *(screen + i * width) ? neighbours_alive++ : 0 ;
            } else if (j > 0) { /* top border */
                *(screen + (height - 1) * width + j - 1) ?
                    neighbours_alive++ : 0 ;
            } else { /* top left corner */
                *(screen + height * width - 1) ? neighbours_alive++ : 0 ;
            }

            /* top */
            if (i > 0) {
                *(screen + (i - 1) * width + j) ? neighbours_alive++ : 0 ;
            } else { /* top border */
                *(screen + (height - 1) * width + j) ? neighbours_alive++ : 0 ;
            }

            /* top right */
            if (i > 0 && j < (width - 1)) {
                *(screen + (i - 1) * width + j + 1) ? neighbours_alive++ : 0 ;
            } else if (i > 0) { /* right border */
                *(screen + (i - 1) * width) ? neighbours_alive++ : 0 ;
            } else if (j < (width -1)) { /* top border */
                *(screen + (height - 1) * width + j + 1) ?
                    neighbours_alive++ : 0 ;
            } else { /* top right corner */
                *(screen + height * width - width) ? neighbours_alive++ : 0 ;
            }

            /* left */
            if (j > 0) {
                *(screen + i * width + j - 1) ? neighbours_alive++ : 0 ;
            } else { /* left border */
                *(screen + i * width + width) ? neighbours_alive++ : 0 ;
            }

            /* right */
            if (j < (width - 1)) {
                *(screen + i * width + j + 1) ? neighbours_alive++ : 0 ;
            } else { /* right border */
                *(screen + i * width) ? neighbours_alive++ : 0 ;
            }

            /* bottom left */
            if (i < (height - 1) && j > 0) {
                *(screen + (i + 1) * width + j - 1) ? neighbours_alive++ : 0 ;
            } else if (i < (height - 1)) { /* left border */
                *(screen + (i + 1) * width + width - 1) ?
                    neighbours_alive++ : 0 ;
            } else if (j > 0) { /* bottom border */
                *(screen + j - 1) ? neighbours_alive++ : 0 ;
            } else { /* bottom left corner */
                *(screen + width - 1) ? neighbours_alive++ : 0 ;
            }

            /* bottom */
            if (i < (height - 1)) {
                *(screen + (i + 1) * width + j) ? neighbours_alive++ : 0 ;
            } else { /* bottom border */
                *(screen + i) ? neighbours_alive++ : 0 ;
            }

            /* bottom right */
            if (i < (height - 1) && j < (width - 1)) {
                *(screen + (i + 1) * width + j + 1) ? neighbours_alive++ : 0 ;
            } else if (i < (height - 1)) { /* right border */
                *(screen + (i + 1) * width) ? neighbours_alive++ : 0 ;
            } else if (j < (width - 1)) { /* bottom border */
                *(screen + j + 1) ? neighbours_alive++ : 0 ;
            } else { /* bottom right corner */
                *(screen) ? neighbours_alive++ : 0 ;
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

void cgol_print_screen(int *screen, unsigned int width, unsigned int height) {
    system("clear");
    for (unsigned int i=0; i<height; i++) {
        for (unsigned int j=0; j<width; j++) {
            if (*(screen + i * width + j)){
                printf("█");
            } else {
                printf(" ");
            }
        }
        printf("\n");
    }
}

void cgol_live(unsigned int width, unsigned int height, int duration) {
    int *screen = cgol_gen_start(width, height);
    while (duration) {
        cgol_print_screen(screen, width, height);
        screen = cgol_next_gen(screen, width, height);
        duration--;
        usleep(100000);
    }
}

/*
vim: ft=c:
*/

