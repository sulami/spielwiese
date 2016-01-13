#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
from_hex_to_decimal(const char hex)
{
	if ('0' <= hex && hex <= '9')
		return hex - '0';

	if (hex >= 'A' && hex <= 'Z')
		return hex - 'A'  + 10;

	return hex - 'a' + 10;
}

int
main(int argc, char *argv[])
{
	unsigned int *input;

	input = malloc(sizeof(char) * 255);

	scanf("%x", input);

	printf("%s\n", input);

	return 0;
}

