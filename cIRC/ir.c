#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>

struct irc_connection {
    int sock;
    char *ip;
    unsigned short port;
    struct sockaddr_in *conn;
    char *nick;
    char *user;
};

static struct irc_connection ircc = {
    .sock = 0,
    .ip = "82.96.64.4", /* Freenode */
    .port = 6667,
    .conn = NULL,
    .nick = "sulami",
    .user = "sulami",
};

/* irc_send(): Sends a message over a socket */
static void irc_send(char *message)
{
    send(ircc.sock, message, strlen(message), 0);
}

/* irc_conn(): Establish a connection to a given server. */
static void irc_conn()
{
    struct sockaddr_in connection;
    int retval;

    ircc.sock = socket(AF_INET, SOCK_STREAM, 0);

    if (ircc.sock < 0) {
        printf("Error creating socket.\n");
        exit(ircc.sock);
    }

    ircc.conn = &connection;
    ircc.conn->sin_addr.s_addr = inet_addr(ircc.ip);
    ircc.conn->sin_family = AF_INET;
    ircc.conn->sin_port = htons(ircc.port);

    retval = connect(ircc.sock, ircc.conn, sizeof(*ircc.conn));
    if (retval < 0) {
        printf("Error connecting to server.\n");
        exit(retval);
    }

    irc_send("NICK sulami\n");
    irc_send("USER sulami 0 * :Robin Schroer\n");
}

/* irc_recv(): Receiving loop on a given socket */
static void *irc_recv()
{
    char server_msg[2000];

    while(1) {
        recv(ircc.sock, &server_msg, 2000, 0);
        printf("%s", server_msg);
    }
}

/* irc_input(): Sending loop to read messages and send them off */
static void *irc_input()
{
    char buf[2000];

    while(1) {
        scanf("%s", buf);
        irc_send(buf);
    }
}

int main(int argc, char *argv[])
{
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            if (!strcmp(argv[i], "-s")) {
                i++;
                ircc.ip = argv[i];
            } else if (!strcmp(argv[i], "-p")) {
                i++;
                ircc.port = (unsigned short)strtoul(argv[i], NULL, 0);
            } else if (!strcmp(argv[i], "-n")) {
                i++;
                ircc.nick = argv[i];
            } else if (!strcmp(argv[i], "-u")) {
                i++;
                ircc.user = argv[i];
            } else {
                printf("Unknown option \"%s\"\n", argv[i]);
                return -1;
            }
        }
    }

    irc_conn();

    irc_recv();

    shutdown(ircc.sock, 2);
    return 0;
}

