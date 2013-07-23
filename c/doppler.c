#include <stdio.h>

int doppelt (int input);

int main(void)
{
    int i;
    printf("Input number to double: ");
    scanf("%i", &i);
    printf("%i\n", doppelt(i));
    return 0;
}

int doppelt(int input)
{
    int result = input * 2;
    return result;
}

