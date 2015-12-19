#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned int
hamming(char *a, char *b)
{
	unsigned int i, acc = 0;

	for (i = 0; i < strlen(a); ++i) {
		unsigned int dist = 0;
		char val = a[i] ^ b[i];

		while (val != 0) {
			dist++;
			val &= val - 1;
		}

		acc += dist;
	}

	return acc;
}

int
main(int argc, char *argv[])
{
	if (argc != 3 || strlen(argv[1]) != strlen(argv[2]))
		return EINVAL;

	printf("%d\n", hamming(argv[1], argv[2]));

	return 0;
}

