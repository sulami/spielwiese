#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <pthread.h>

#define VERSION         "0.1"
#define DEFAULTSERVER   "82.96.64.4" /* Freenode */
#define DEFAULTPORT     6667
#define DEFAULTNICK     "sulami"
#define DEFAULTUSER     "sulami 0 * :Robin Schroer"

/*
 * irc_conn(): Establish a connection to a given server.
 *
 * TAKES:   char *server            = server ip
 *          unsigned short port     = server port
 *
 * RETURNS: socket pointer on success
 *          -1 on socket creation error
 *          -2 on connection error
 */
static int irc_conn(char *server, unsigned short port)
{
    static int sock;
    static struct sockaddr_in conn;

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

    return sock;
}

/*
 * irc_recv(): Receiving loop on a given socket
 *
 * TAKES:   int sock    = socket
 */
void *irc_recv(int sock)
{
    static char server_msg[2000];

    while(1) {
        recv(sock, server_msg, 2000, 0);
        printf("%s\n", server_msg);
    }
}

/*
 * irc_send(): Sends a message over a socket
 *
 * TAKES:   int sock        = socket
 *          char *message   = message to send
 */
static void irc_send(int sock, char *message)
{
    send(sock, message, strlen(message), 0);
}

int main(int argc, char *argv[])
{
    int sock, rc;
    static char *server, *nick, *user;
    static unsigned short port;
    pthread_t threads[2];

    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            if (!strcmp(argv[i], "-s")) {
                i++;
                server = argv[i];
            } else if (!strcmp(argv[i], "-p")) {
                i++;
                port = (unsigned short)strtoul(argv[i], NULL, 0);
            } else if (!strcmp(argv[i], "-n")) {
                i++;
                nick = argv[i];
            } else if (!strcmp(argv[i], "-u")) {
                i++;
                user = argv[i];
            } else if (!strcmp(argv[i], "--version")) {
                printf("IR.c version %s\n", VERSION);
                return 0;
            }
        }
    }

    if (!server)
        server = DEFAULTSERVER;
    if (!port)
        port = DEFAULTPORT;
    if (!nick)
        nick = DEFAULTNICK;
    if (!user)
        user = DEFAULTUSER;

    sock = irc_conn(server, port);
    if (sock < 0)
        exit(sock);

    irc_send(sock, "NICK sulami");
    irc_send(sock, "USER sulami 0 * :Robin Schroer");

    rc = pthread_create(threads, 0, irc_recv, (void *)sock);

    pthread_exit(NULL);

    shutdown(sock, 2);

    return 0;
}

