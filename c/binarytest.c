/* This tests the binary search tree class */
#include <stdio.h>
#include "binarytree.h"

int main(int argc, const char *argv[])
{
    char action;
    printf("(n)ew, (l)ookup, (e)xit: ");
    while (action != 'e') {
        scanf(" %c", &action);
        switch (action) {
            case 'n':
                printf("new node\n");
                printf("(n)ew, (l)ookup, (e)xit: ");
                break;
            case 'l':
                printf("lookup\n");
                printf("(n)ew, (l)ookup, (e)xit: ");
                break;
            case 'e':
                break;
            default:
                printf("(n)ew, (l)ookup, (e)xit: ");
        }
    }
    return 0;
}

