/*
 ** client.c -- a stream socket client demo
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>

#include <arpa/inet.h>

#include <pthread.h>



#define PORT "5000" // the port client will be connecting to 

#define MAXDATASIZE (100) // max number of bytes we can get at once 

#define NUMLOOP (1000)
#define NUMTHREAD (1)

#define BIND_SOURCE_PORT (0)

int g_nloop;
int g_nhello;
int g_noverwrap;

// get sockaddr, IPv4 or IPv6:
void *get_in_addr(struct sockaddr *sa)
{
    if (sa->sa_family == AF_INET) {
        return &(((struct sockaddr_in*)sa)->sin_addr);
    }

    return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

void prepare(int sock)
{
    //int yes=1;

    //setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);

#if BIND_SOURCE_PORT
    {
        struct sockaddr_in sin;
        memset(&sin, 0, sizeof sin);
        sin.sin_family = AF_INET;
        sin.sin_addr.s_addr = htonl(INADDR_ANY);
        sin.sin_port = htons(20000);
        bind(sock, (struct sockaddr*)&sin, sizeof(sin));
    }
#endif
}

void* do_connect(struct addrinfo *servinfo)
{
    struct addrinfo *p = servinfo;
    int sockfd, numbytes;  
    char buf[MAXDATASIZE];
    int *socks = NULL;
    int i, j, k;

    socks = malloc(sizeof(int)*g_noverwrap);

    for (i = 0; i < g_nloop; ++i) {
        // loop through all the results and connect to the first we can

        k = 0;
        for (k=0; k<g_noverwrap; ++k) {
            for (; p != NULL; p = p->ai_next) {
                if ((sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) == -1) {
                    perror("client: socket");
                    continue;
                }

                prepare(sockfd);
                if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
                    close(sockfd);
                    perror("client: connect");
                    continue;
                }
                socks[k] = sockfd;
                break;
            }
            if (!p) break;
        }

        if (p == NULL) {
            continue;
        }
#if 1
        for (j=0; j<g_nhello; ++j) {
            for (k=0; k<g_noverwrap; ++k) {
                sockfd = socks[k];
                send(sockfd, "hello\n", 6, 0);
            }
            for (k=0; k<g_noverwrap; ++k) {
                sockfd = socks[k];
                if ((numbytes = recv(sockfd, buf, MAXDATASIZE-1, 0)) < 0) {
                    perror("recv");
                    exit(1);
                }
                if (numbytes != 6) {
                    printf("Recieved %d bytes\n", numbytes);
                }
            }
        }

        for (k=0; k<g_noverwrap; ++k) {
            close(socks[k]);
        }
#endif
    }
    free(socks);
    return NULL;
}

int main(int argc, char *argv[])
{
    struct addrinfo hints, *servinfo, *p;
    int rv;
    int opt;

    int nthread = NUMTHREAD;
    g_nloop = NUMLOOP;
    g_nhello = 0;
    g_noverwrap = 1;

    const char *port = PORT;
    const char *host = NULL;

    while (-1 != (opt = getopt(argc, argv, "n:h:c:p:o:"))) {
        switch (opt) {
        case 'n':
            g_nloop = atoi(optarg);
            break;
        case 'h':
            g_nhello = atoi(optarg);
            break;
        case 'c':
            nthread = atoi(optarg);
            break;
        case 'p':
            port = optarg;
            break;
        case 'o':
            g_noverwrap = atoi(optarg);
            break;
        default:
            fprintf(stderr, "Unknown option: %c\n", opt);
            return 1;
        }
    }

    if (optind >= argc) {
        fprintf(stderr,"usage: client [-n loops] [-c threads] [-p port] hostname\n");
        return 2;
    }

    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    if ((rv = getaddrinfo(argv[optind], port, &hints, &servinfo)) != 0) {
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
        return 1;
    }

    if (nthread == 1) {
        do_connect(servinfo);
    }
    else {
        void* res;
        pthread_t *threads = malloc(sizeof(pthread_t)*nthread);
        int i;
        for (i = 0; i < nthread; ++i) {
            rv = pthread_create(&threads[i], NULL, (void*)do_connect, (void*)servinfo);
            if (rv == -1) {
                perror("Failed to create thread");
                return 3;
            }
        }
        for (i = 0; i < nthread; ++i) {
            rv = pthread_join(threads[i], &res);
            if (rv == -1) {
                perror("Failed to join a thread.");
                return 4;
            }
        }
        free(threads);
    }

    freeaddrinfo(servinfo); // all done with this structure

    return 0;
}

