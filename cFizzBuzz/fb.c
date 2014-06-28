#include <stdio.h>

int main()
{
    int i;

    for (i = 1; i <= 100; i++) {
        if (!(i % 3))
            printf("Fizz");
        if (!(i % 5))
            printf("Buzz");
        if (!(i % 3) || !(i % 5)) {
            printf("\n");
            continue;
        }
        printf("%i\n", i);
    }

    return 0;
}

