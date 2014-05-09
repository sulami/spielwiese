#include <stdlib.h>
#include <time.h>

int ***cgol_gen_start(unsigned int width, unsigned int height);
int cgol_live(void);

int ***cgol_gen_start(unsigned int width, unsigned int height) {
    int **screen = malloc(sizeof(int));
    if (screen != NULL) {
        srand((unsigned int)time(NULL));
        unsigned int i;
        for (i=0; i<height; i++) {
            screen[i] = malloc(sizeof(int) * height);
            unsigned int j;
            for (j=0; j<width; j++) {
                screen[i][j] = (int)rand();
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

