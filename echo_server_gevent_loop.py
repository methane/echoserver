#!/usr/bin/env python
# echo_server_tornado.py ported to gevent event loop
import errno
import socket
from gevent.core import loop

NONBLOCKING = (errno.EAGAIN, errno.EWOULDBLOCK)
loop = loop()

def conn_handler(sock):
    buf = [""]
    wait_write = [0]

    fd = sock.fileno()
    def _close():
        watcher.stop()
        sock.close()

    def handle(fd):
        _buf = buf[0]
        try:
            _buf += sock.recv(1024)
            if not _buf:
                return _close()
        except socket.error as e:
            if e.args[0] not in NONBLOCKING:
                print e
                return _close()
        try:
            sent = sock.send(_buf)
            buf[0] = _buf = _buf[sent:]
            if _buf and not wait_write[0]:
                watcher.stop()
                watcher.fd = 1 | 2
                watcher.start()
                wait_write[0] = 1
            elif wait_write[0]:
                watcher.stop()
                watcher.fd = 1 | 2
                watcher.start()
                wait_write[0] = 0
        except socket.error as e:
            if e.args[0] not in NONBLOCKING:
                print e
                return _close()

    watcher = loop.io(sock.fileno(), 1)
    watcher.start(handle, fd)


class Connection(object):
    __slots__ = ('_conn', '_buf')

    def __init__(self, connection):
        self._conn = connection
        self._buf = ""
        self.watcher = loop(connection.fileno(), 1)
        self.watcher.start(self.handle)

    def handle(self):
        try:
            self._buf += self._conn.recv(1024)
            if not self._buf:
                return self._close()
        except socket.error as e:
            if e.args[0] not in NONBLOCKING:
                print e
                return self._close()
        try:
            sent = self._conn.send(self._buf)
            self._buf = self._buf[sent:]
        except socket.error as e:
            if e.args[0] not in NONBLOCKING:
                print e
                return self._close()

    def _close(self):
        self.watcher.stop()
        self._conn.close()
        self._conn = None


def connection_ready(sock):
    while True:
        try:
            connection, address = sock.accept()
        except socket.error, e:
            if e[0] not in (errno.EWOULDBLOCK, errno.EAGAIN):
                raise
            return
        connection.setblocking(0)
        #Connection(connection)
        conn_handler(connection)

if __name__ == '__main__':
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.setblocking(0)
    sock.bind(("", 5000))
    sock.listen(1024)

    loop.io(sock.fileno(), 1).start(connection_ready, sock)
    loop.run()
    print "exited cleanly"
