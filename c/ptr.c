#include <stdio.h>

int main() {

	/* pointers to pointers to ints */

	int a, *b, *c;
	a = 88;
	b = &a;
	c = &b;

	printf("a: %d\n", a);
	printf("b: %d\n", b);
	printf("c: %d\n", c);

	printf("&a: %d\n", &a);
	printf("&b: %d\n", &b);
	printf("&c: %d\n", &c);

	printf("*b: %d\n", *b);
	printf("*c: %d\n", *c);

	return 0;
}
