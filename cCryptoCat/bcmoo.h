/*
 * Blcok cipher modes of operation for use with my crappy ciphers
 */

#include "xor.h"

char *enc_cbc(char *in, unsigned int cipher, unsigned int bs, char *iv);

#define CIPHER_NULL   0
#define CIPHER_XOR    1

/*
 * CBC mode - XOR the IV with the first plainblock, then the cipherblocks with
 * the next following plainblock
 *
 * TAKES:
 * char *in:            input string
 * unsigned int cipher: cipher algorithm (see above)
 * unsigned int bs:     blocksize
 * char *iv:            initialization vector
 */
char *enc_cbc(char *in, unsigned int cipher, unsigned int bs, char *iv)
{
    return in;
}

