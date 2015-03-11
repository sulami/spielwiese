#include <stdlib.h>
#include <stdio.h>

#define MAX(x,y) (x >= y ? x : y)

/* sorry, no doc here, time is precious */

struct node {
	char data;
	struct node *l;
	struct node *r;
};

struct node *
new_node(char data)
{
	struct node *n = malloc(sizeof(struct node));
	if (!n)
		return NULL;

	n->data = data;
	n->l = NULL;
	n->r = NULL;

	return n;
}

int
height(struct node *n)
{
	if (!n)
		return 0;

	return 1 + MAX(height(n->l), height(n->r));
}

int
width(struct node *n)
{
	if (!n)
		return 0;

	int lh = height(n->l);
	int rh = height(n->r);

	int lw = width(n->l);
	int rw = width(n->r);

	return MAX(lh + rh + 1, MAX(lw, rw));
}

int
main()
{
	char *line;
	size_t size;

	while (getline(&line, &size, stdin) != -1) {
	}

	return 0;
}

