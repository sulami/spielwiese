#include <stdio.h>

int factorial(int i) {
	if (i > 1) {
		return i * factorial(i-1);
	} else {
		return i;
	}
	return 0;
}

int main() {
	int zahlen[10] = { 4, 6, 2, 10, 1, 0, 13, 25, 7, 6 };
	int i;
	for (i = 0; i < 10; i++) {
		printf("%d! = %i\n", zahlen[i], factorial(zahlen[i]));
	}
}
