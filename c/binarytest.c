/* This tests the binary search tree class */
#include <stdio.h>
#include "binarytree.h"

int main(int argc, const char *argv[])
{
    char action;
    while (action != 'e') {
        switch (action) {
            case 'n':
                printf("new node\n");
                printf("(n)ew, (l)ookup, (e)xit: ");
                break;
            case 'l':
                printf("lookup\n");
                printf("(n)ew, (l)ookup, (e)xit: ");
                break;
            default:
                printf("(n)ew, (l)ookup, (e)xit: ");
        }
        scanf(" %c", &action);
    }
    return 0;
}

