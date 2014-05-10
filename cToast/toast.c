#include <stdlib.h>
#include <assert.h>

struct toast {
    void *butter;
    void *nobutter;
    void *obutter;
};

struct toast *newtoast(void);

struct toast *newtoast() {
    struct toast *toast = malloc(sizeof(toast));
    if (toast != NULL) {
        toast->butter = NULL;
        toast->nobutter = NULL;
        toast->obutter = NULL;
        return(toast);
    } else {
        return(NULL);
    }
}

int main() {
    struct toast *toast = newtoast();
    assert(toast->butter == NULL);
    assert(toast->nobutter == NULL);
    assert(toast->obutter == NULL);
    return(0);
}

