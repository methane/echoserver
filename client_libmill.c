// client_libmill.c
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <libmill.h>

void client(ipaddr addr, int messages, chan results)
{
    int sent = 0;

    tcpsock s = tcpconnect(addr, -1);
    if (s == NULL) {
        perror("tcpconnect");
        goto out;
    }

    while (messages--) {
        tcpsend(s, "hello\n", 6, -1);
        if (errno != 0) {
            perror("tcpsend");
            goto out;
        }

        tcpflush(s, -1);
        if (errno != 0) {
            perror("tcpflush");
            goto out;
        }

        char buf[100];
        size_t sz = tcprecvuntil(s, buf, sizeof(buf), "\n", 1, -1);
        if (errno != 0) {
            perror("tcprecvuntil");
            goto out;
        }
        buf[sz - 1] = 0;
        if (sz != 6) {
            fprintf(stderr, "unexpedted return: %s\n", buf);
            goto out;
        }

        sent++;
    }

out:
    tcpclose(s);
    chs(results, int, sent);
}

int main(int argc, char *argv[])
{
    int opt;
    int clients = 100;
    int messages = 10000;
    int port = 5000;
    const char *host;

    while (-1 != (opt = getopt(argc, argv, "c:h:p:"))) {
        switch (opt) {
        case 'c':
            clients = atoi(optarg);
            break;
        case 'h':
            messages = atoi(optarg);
            break;
        case 'p':
            port = atoi(optarg);
            break;
        default:
            fprintf(stderr, "Unknown option: %c\n", opt);
            return 2;
        }
    }

    if (optind >= argc) {
        fprintf(stderr, "usage: client-libmill [-c clients] [-h messages per client] [-p port] host\n");
        return 2;
    }

    host = argv[optind];

    ipaddr addr = ipremote(host, port, 0, -1);

    chan results = chmake(int, 0);

    int64_t start_time = now();

    for (int i = 0; i < clients; ++i)
        go(client(addr, messages, results));

    int success = 0;

    for (int i = 0; i < clients; ++i)
        success += chr(results, int);

    int64_t elapsed = now() - start_time;

    printf("Throughput: %.2lf [#/sec]\n",
           success * 1000.0 / elapsed);

    return 0;
}
