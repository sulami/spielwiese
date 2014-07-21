/*
 * Blcok cipher modes of operation for use with my crappy ciphers
 */

#include <errno.h>
#include <string.h>
#include "xor.h"

char *enc_cbc(char *in, unsigned int cipher, unsigned int bs, char *iv);

#define CIPHER_NULL   0
#define CIPHER_XOR    1

/*
 * Add padding to plaintext blocks according to PKCS7 (RFC 5652), filling up to
 * a whole block with the amount of bytes padding added.
 *
 * TAKES:
 * char *in:        input string
 * unsigned int bs: blocksize
 */
static void pad_blocks(char *in, unsigned int bs)
{
    /* TODO: realloc *in, add the padding */
}

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
    char *out;

    if (!in || !bs || !iv)
        return (char *)-EINVAL;

    if (strlen(in) % bs)
        pad_blocks(in, bs);

    out = malloc(sizeof(in));
    if (!out)
        return (char *)-ENOMEM;

    for (unsigned int i = 0; i < strlen(in) / bs; i++) {
        /* TODO: xor iv/cipherblocks, send them to the proper cipher */
    }

    return in;
}

