#include <stdio.h>
#include <string.h>

#define VERSION "0.1"
#define DEFAULTSERVER "irc.freenode.net"
#define DEFAULTPORT 6667

int main(int argc, char *argv[])
{
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            if (!strcmp(argv[i], "--version")) {
                printf("%s\n", VERSION);
                return 0;
            }
        }
    }
    return 0;
}

