#include <stdio.h>

#define EINVAL			22

int dosmth() {
	return -EINVAL;
}

int main() {
	if (dosmth())
		printf("dosmth triggered.\n");
	else
		printf("dosmth not triggered.\n");
	return 0;
}
