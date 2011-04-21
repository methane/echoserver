from gevent.server import StreamServer

def handler(sock, addr):
    try:
        while 1:
            buf = sock.recv(4096)
            if not buf:
                return
            sock.send(buf)
    finally:
        sock.close()

def main():
    server = StreamServer(('', 5000), handler)
    server.serve_forever()

if __name__ == '__main__':
    main()
