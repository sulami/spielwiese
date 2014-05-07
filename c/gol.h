#include <stdlib.h>
#include <time.h>

int ***cgol_gen_start(int width, int height) {
    int ***screen = malloc(sizeof(int) * width * height);
    if (screen != NULL) {
        srand(time(NULL));
        int i;
        for (i=0; i<height; i++) {
            int j;
            for (j=0; i<width; j++) {
                *screen[i][j] = rand();
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

