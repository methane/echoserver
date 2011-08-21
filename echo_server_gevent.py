#!/usr/bin/env python
from gevent.server import StreamServer

def handler(sock, addr):
    try:
        recv=sock.recv
        send=sock.send
        while 1:
            buf = recv(4096)
            if not buf:
                return
            send(buf)
    finally:
        sock.close()

def main():
    server = StreamServer(('', 5000), handler)
    server.serve_forever()

if __name__ == '__main__':
    main()
