#include <stdio.h>

int main(void)
{
    char s[128];
    printf("Input first message: ");
    scanf("%s", &s);
    printf("Echo: %s\n", s);
    return 0;
}
