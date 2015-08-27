// server_libmill.c
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libmill.h>

void echoserver(tcpsock as)
{
    char buf[256] = {0};
    size_t sz;

    for (;;) {
        sz = tcprecvuntil(as, buf, sizeof(buf), "\n", 1, -1);
        if (errno != 0)
            break;
        tcpsend(as, buf, sz, -1);
        if (errno != 0)
            break;
        tcpflush(as, -1);
        if (errno != 0)
            break;
    }

    tcpclose(as);
}

int main(int argc, char **argv)
{
    int port = 5000;
    if (argc > 1)
        port = atoi(argv[1]);

    ipaddr addr = iplocal(NULL, port, 0);
    tcpsock ls = tcplisten(addr, 10);
    if (!ls) {
        perror("Can't open listening socket");
        return 1;
    }
    while (1) {
        tcpsock as = tcpaccept(ls, -1);
        if (!as)
            continue;
        go(echoserver(as));
    }
    return 0;
}
