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

using namespace std;

static int listener; //fd
static volatile unsigned long process_count;

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

    if (listen(sock, 1024) < 0) {
        close(sock);
        perror("listen");
        exit(-1);
    }

    return sock;
}

void* worker(void *data)
{
    char *buf = (char*)malloc(128);
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
            int n = ::read(client, buf, 128);
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
                __sync_fetch_and_add(&process_count, 1);
                continue;
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

void *busy_loop(void* notused)
{
    int a=1,b=1,c;
    for (;;) {
        c = a+b;
        a = b;
        b = c;
    }
    return NULL;
}

int main(int argc, char *argv[])
{
    int opt, port=DEFAULT_PORT;
    int num_thread=1;
    int num_busy=0;

    while (-1 != (opt = getopt(argc, argv, "p:c:b:"))) {
        switch (opt) {
        case 'p':
            port = atoi(optarg);
            break;
        case 'c':
            num_thread = atoi(optarg);
            break;
        case 'b':
            num_busy = atoi(optarg);
            break;
        default:
            fprintf(stderr, "Unknown option: %c\n", opt);
            return 1;
        }
    }

    listener = setup_server_socket(port);
    printf("Listening port (-p): %d\n", port);
    printf("Worker thread (-c): %d\n", num_thread);
    printf("Busy thread (-b): %d\n", num_busy);

    pthread_t *threads = (pthread_t*)malloc(sizeof(pthread_t) * num_thread);

    for (int i = 0; i < num_thread; ++i) {
        pthread_create(&threads[i], NULL, worker, NULL);
    }
    for (int i = 0; i < num_busy; ++i) {
        pthread_t t;
        pthread_create(&t, NULL, busy_loop, NULL);
    }

    unsigned long prev=0, count=0;
    for (;;) {
        sleep(1);
        count = __sync_fetch_and_add(&process_count, 0);
        printf("processed %lu requests.\n", count-prev);
        prev = count;
    }

    return 0;
}

