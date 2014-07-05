/*
 * Encrypts argv[2] by using argv[1]
 * NOT FOR ACTUAL USE
 * Licensend under BSD
 * by sulami
 * >> https://sulami.github.io/
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
    char *result;

    if (argc != 3) {
        printf("Please specify exactly 2 arguments, key and message\n");
        return 1;
    }

    result = malloc(sizeof(char) * strlen(argv[2]));
    if (!result) {
        printf("Error allocating memory\n");
        return 1;
    }

    for (unsigned long i = 0; i < strlen(argv[2]); i++) {
        for (unsigned long j = i; j > strlen(argv[2]); j -= strlen(argv[2]));
        result[i] = argv[2][i] ^ (argv[1][i % strlen(argv[1])]);
    }

    printf("%s\n", result);

    return 0;
}

