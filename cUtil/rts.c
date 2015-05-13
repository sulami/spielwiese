/*
 * In place string reversal. Should work with all kinds of strings. Licensed
 * under ISC, © Robin 'sulami' Schroer.
 *
 * Compile with `-lm -std=c99`.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int
main()
{
	char b;
	int n;
	char s[] = "Lagerregal";
	n = strlen(s);

	for (int i = 0; i < ceil(n/2); i++) {
		b = s[i];
		s[i] = s[n-i-1];
		s[n-i-1] = b;
	}

	puts(s);

	return 0;
}

