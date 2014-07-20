/*
 * Stupid simple XOR-cipher
 */

#include <stdlib.h>
#include <errno.h>

char xor_char(char in, char key);

char xor_char(char in, char key)
{
    return in ^ key;
}

