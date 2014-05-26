#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>

#define VERSION         "0.1"
#define HELP            "Options:\n--version\n-s <server>\n-p <port>\n-u <user>"
#define DEFAULTSERVER   "82.96.64.4" /* Freenode */
#define DEFAULTPORT     6667
#define DEFAULTUSER     "sulami"

static char *server;
static unsigned short port;
static char *user;

int main(int argc, char *argv[])
{
    static int sock;
    static struct sockaddr_in conn;
    static char server_msg[2000];

    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            if (!strcmp(argv[i], "-s")) {
                i++;
                server = argv[i];
            } else if (!strcmp(argv[i], "-p")) {
                i++;
                port = (unsigned short)strtoul(argv[i], NULL, 0);
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
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        printf("Error creating socket.\n");
        return -1;
    }

    conn.sin_addr.s_addr = inet_addr(server);
    conn.sin_family = AF_INET;
    conn.sin_port = htons(port);

    if (connect(sock, (struct sockaddr *)&conn, sizeof(conn)) < 0) {
        printf("Error connecting to server.\n");
        return -2;
    }

    while (1) {
        recv(sock, server_msg, 2000, 0);
        printf("%s\n", server_msg);
    }

    shutdown(sock, 2);

    return 0;
}

