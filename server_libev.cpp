#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

#include <errno.h>

#define EV_MULTIPLICITY 1
#include <ev.h>

#define ECHO_PORT 5000
#define MAX_BACKLOG 1024
#define RCVBUFSIZE 256
#define MAX_EVENTS 1024

int echo(int sock) {
    char buf[RCVBUFSIZE];
    size_t len;

    if ((len = recv(sock, buf, RCVBUFSIZE, 0)) < 0) {
        perror("recv(2)");
        return -1;
    }

    if (len == 0) {
        return -1;
    }

    if (send(sock, buf, len, 0) < 0) {
        perror("send(2)");
        return -1;
    }

    return len;
}

void event_client(EV_P_ struct ev_io *w, int revents) {
    if (echo(w->fd) < 1) {
        close(w->fd);
        ev_io_stop(EV_A_ w);
        free(w);
    }
}

void event_server(EV_P_ struct ev_io *w, int revents) {
    int sock, flags;
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);
    static long connected=0;

    for (;;) {
        if ((sock = accept(w->fd, (struct sockaddr*) &addr, &len)) < 0) {
            switch (errno) {
            case EINTR:
            case EAGAIN:
                break;
            default:
                perror("accept");
            }
            break;
        }

        connected++;
        if ((flags = fcntl(sock, F_GETFL, 0)) < 0 || fcntl(sock, F_SETFL, flags | O_NONBLOCK) < 0) {
            perror("fcntl(2)");
            return;
        }

        ev_io *watcher = (ev_io*)calloc(1, sizeof(ev_io));
        ev_init(watcher, event_client);
        ev_io_set(watcher, sock, EV_READ);
        ev_io_start(EV_DEFAULT, watcher);
    }
    fprintf(stderr, "Connected %d clients.\n", connected);
}

int main() {
    int sock, flags, yes=1;
    struct sockaddr_in addr;
    struct ev_loop *loop = EV_DEFAULT;
    ev_io watcher;

    if ((sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
        perror("socket");
        return 0;
    }

    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(ECHO_PORT);

    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);
    if (bind(sock, (struct sockaddr*) &addr, sizeof(addr)) < 0) {
        perror("bind(2)");
        return -1;
    }

    if (listen(sock, MAX_BACKLOG) < 0) {
        perror("listen(2)");
        return -1;
    }

    if ((flags = fcntl(sock, F_GETFL, 0)) < 0 || fcntl(sock, F_SETFL, flags | O_NONBLOCK) < 0) {
        perror("fcntl(2)");
        return -1;
    }

    ev_init(&watcher, event_server);
    ev_io_set(&watcher, sock, EV_READ);
    ev_io_start(loop, &watcher);
    ev_run(loop, 0);

    close(sock);

    return 0;
}

