#include <stdio.h>
#include <string.h>

void rotate(char i);

int main(void)
{
    char input;
    printf("Enter input char: ");
    scanf("%c", &input);
    rotate(input);
}

void rotate(char i)
{
    int n;
    if ( (i >= 65 && i <= 77) | (i >= 97 && i <= 109) ) /* Aa-Mn */
    {
        for ( n = 0; n < 13; ++n )
        {
            i += 1;
        }
        printf("Output: %c\n", i);
    } else if ( (i >= 78 && i <= 90) | (i >= 110 && i <= 122) ) { /* Nn-Zz */
        for ( n = 0; n < 13; ++n )
        {
            i -= 1;
        }
        printf("Output: %c\n", i);
    } else { /* Non-alphabet chars */
        printf("The entered char is not a letter.\n");
    }
}
