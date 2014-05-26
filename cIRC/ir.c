#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VERSION         "0.1"
#define HELP            "Options:\n--version\n-s <server>\n-p <port>\n-u <user>"
#define DEFAULTSERVER   "irc.freenode.net"
#define DEFAULTPORT     6667
#define DEFAULTUSER     "sulami"

static char *server;
static int port;
static char *user;

int main(int argc, char *argv[])
{
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            if (!strcmp(argv[i], "-s")) {
                i++;
                server = argv[i];
            } else if (!strcmp(argv[i], "-p")) {
                i++;
                port = atoi(argv[i]);
            } else if (!strcmp(argv[i], "-u")) {
                i++;
                user = argv[i];
            } else if (!strcmp(argv[i], "--version")) {
                printf("IR.c version %s\n", VERSION);
                return 0;
            } else if (!strcmp(argv[i], "--help")) {
                printf("%s\n", HELP);
                return 0;
            }
        }
    }
    if (!server)
        server = DEFAULTSERVER;
    if (!port)
        port = DEFAULTPORT;
    if (!user)
        user = DEFAULTUSER;
    printf("server: %s, port: %d, user: %s\n", server, port, user);
    return 0;
}

