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
#include <netinet/in.h>

#include <pthread.h>

#define DEFAULT_PORT (5000)
#define MAX_EVENTS   (64)

using namespace std;

static int listener; //fd

struct conn {
    int sock;
    char *buf;
    size_t alloced, head, tail;
    bool read_end;
    bool error;

    conn(int sock) :
        sock(sock), buf(0), head(0), tail(0), read_end(false), error(false)
    {
        alloced = 2048;
        buf = (char*)malloc(alloced);
        if (!buf) {
            puts("No memory.\n");
            exit(-1);
        }
    }
    ~conn() { close(sock); }

    void read() {
        for (;;) {
            if (alloced - tail < 512) {
                if (alloced - (tail - head) < 1024) {
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

static int setup_server_socket(int port, bool block=false)
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

void* worker(void *data)
{
    char *buf = (char*)malloc(2048);
    if (!buf) {
        fprintf(stderr, "worker: no memory.\n");
        return NULL;
    }
    for (;;) {
restart:
        struct sockaddr_in client_addr;
        socklen_t client_addr_len = sizeof client_addr;
        int client = accept(listener, (struct sockaddr*)&client_addr, &client_addr_len);
        if (client < 0) {
            perror("accept");
            continue;
        }

        for (;;) {
            int n = ::read(client, buf, 2048);
            if (n > 0) {
                int m = 0;
                while (m < n) {
                    int o = ::write(client, buf+m, n-m);
                    if (o < 0) {
                        perror("write");
                        close(client);
                        goto restart;
                    }
                    m += o;
                }
            }
            if (n < 0) {
                perror("read");
            }
            close(client);
            goto restart;
        }
    }
    return NULL;
}

int main(int argc, char *argv[])
{
    struct epoll_event ev;
    struct epoll_event events[MAX_EVENTS];
    int epfd;

    int opt, port=DEFAULT_PORT;
    int num_thread=1;

    while (-1 != (opt = getopt(argc, argv, "p:t:"))) {
        switch (opt) {
        case 'p':
            port = atoi(optarg);
            break;
        case 't':
            num_thread = atoi(optarg);
            break;
        default:
            fprintf(stderr, "Unknown option: %c\n", opt);
            return 1;
        }
    }

    listener = setup_server_socket(port);
    printf("Listening port %d\n", port);

    pthread_t *threads = (pthread_t*)malloc(sizeof(pthread_t) * num_thread);

    for (int i = 0; i < num_thread; ++i) {
        pthread_create(&threads[i], NULL, worker, NULL);
    }

    for (;;) {
        sleep(1);
    }

    return 0;
}

