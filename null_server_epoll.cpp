#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <string>

#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/epoll.h>
#include <sys/time.h>
#include <netinet/in.h>

#define DEFAULT_PORT (5000)
#define MAX_EVENTS   (64)

using namespace std;

static volatile unsigned long long process_count;

struct conn {
    int sock;
    char *buf;
    size_t alloced, head, tail;
    bool read_end;
    bool error;

    conn(int sock) :
        sock(sock), buf(0), head(0), tail(0), read_end(false), error(false)
    {
        alloced = 32;
        buf = (char*)malloc(alloced);
        if (!buf) {
            puts("No memory.\n");
            exit(-1);
        }
    }
    ~conn() { close(sock); }

    void read() {
        for (;;) {
            if (alloced - tail < 64) {
                if (alloced - (tail - head) < 128) {
                    alloced *= 2;
                    buf = (char*)realloc(buf, alloced);
                    if (!buf) {
                        puts("No memory(realloc)");
                        exit(-1);
                    }
                } else {
                    memmove(buf, buf+head, tail-head);
                    tail -= head;
                    head = 0;
                }
            }
            //printf("reading: %d, %p, %ld\n", sock, buf+tail, alloced-tail);
            int n = ::read(sock, buf+tail, alloced-tail);
            if (n < 0) {
                if (errno == EAGAIN) {
                    break;
                }
                perror("read");
                error = true;
                return;
            }
            if (n == 0) {
                read_end = true;
                return;
            }
            tail += n;
        }
    }
    int write() {
        while (head < tail) {
            int n = ::write(sock, buf+head, tail-head);
            if (n < 0) {
                if (errno == EAGAIN) break;
                perror("write");
                error = true;
                return -1;
            }
            // n >= 0
            head += n;
        }
        return tail-head;
    }
    void handle() {
        if (error) return;
        read();
        if (error) return;
        write();
    }
    int done() const {
        return error || (read_end && (tail == head));
    }
};

static void setnonblocking(int fd)
{
    int flag = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flag | O_NONBLOCK);
}

static int setup_server_socket(int port)
{
    int sock;
    struct sockaddr_in sin;
    int yes=1;

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        perror("socket");
        exit(-1);
    }
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);

    memset(&sin, 0, sizeof sin);

    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_ANY);
    sin.sin_port = htons(port);

    if (bind(sock, (struct sockaddr *) &sin, sizeof sin) < 0) {
        close(sock);
        perror("bind");
        exit(-1);
    }

    if (listen(sock, 5) < 0) {
        close(sock);
        perror("listen");
        exit(-1);
    }

    return sock;
}

int main(int argc, char *argv[])
{
    struct epoll_event ev;
    struct epoll_event events[MAX_EVENTS];
    int listener, epfd;

    int opt, port=DEFAULT_PORT;

    while (-1 != (opt = getopt(argc, argv, "p:"))) {
        switch (opt) {
        case 'p':
            port = atoi(optarg);
            break;
        default:
            fprintf(stderr, "Unknown option: %c\n", opt);
            return 1;
        }
    }

    if ((epfd = epoll_create(128)) < 0) {
        perror("epoll_create");
        exit(-1);
    }

    listener = setup_server_socket(port);

    memset(&ev, 0, sizeof ev);
    ev.events = EPOLLIN;
    ev.data.fd = listener;
    epoll_ctl(epfd, EPOLL_CTL_ADD, listener, &ev);

    printf("Listening port %d\n", port);

    unsigned long proc = 0;
    struct timeval tim, tim_prev;
    gettimeofday(&tim_prev, NULL);
    tim_prev = tim;

    for (;;) {
        int i;
        int nfd = epoll_wait(epfd, events, MAX_EVENTS, -1);

        for (i = 0; i < nfd; i++) {
            if (events[i].data.fd == listener) {
                struct sockaddr_in client_addr;
                socklen_t client_addr_len = sizeof client_addr;

                int client = accept(listener, (struct sockaddr *) &client_addr, &client_addr_len);
                if (client < 0) {
                    perror("accept");
                    continue;
                }
                proc++;
                close(client);

                //setnonblocking(client);
		//memset(&ev, 0, sizeof ev);
                //ev.events = EPOLLIN | EPOLLET;
                //ev.data.ptr = (void*)new conn(client);
                //epoll_ctl(epfd, EPOLL_CTL_ADD, client, &ev);
                //printf("connected: %d\n", client);
            } else {
                conn *pc = (conn*)events[i].data.ptr;
                pc->handle();
                proc++;
                if (pc->done()) {
                    epoll_ctl(epfd, EPOLL_CTL_DEL, pc->sock, &ev);
                    delete pc;
                }
            }
        }

        if (proc > 100000) {
            proc = 0;
            gettimeofday(&tim, NULL);
            timersub(&tim, &tim_prev, &tim_prev);
            long long d = tim_prev.tv_sec;
            d *= 1000;
            d += tim_prev.tv_usec / 1000;
            printf("%lld msec per 100000 req\n", d);
            printf("%lld reqs per sec\n", 100000LL*1000/d);
            tim_prev = tim;
        }
    }
    return 0;
}

