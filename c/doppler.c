#include <stdio.h>

int doppelt(input)
{
    int result = input * 2;
    printf(result);
    return 0;
}

int main(void)
{
    int i = 4;
    doppelt(i);
    return 0;
}

