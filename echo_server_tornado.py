import errno
import functools
import socket
from tornado import ioloop, iostream

NONBLOCKING = (errno.EAGAIN, errno.EWOULDBLOCK)

def conn_handler(sock):
    buf = [""]
    wait_write = [0]

    def close():
        io_loop.remove_handler(sock.fileno())
        sock.close()

    def handle(fd, events):
        _buf = buf[0]
        try:
            _buf += sock.recv(1024)
            if not _buf:
                return close()
        except socket.error as e:
            if e.args[0] not in NONBLOCKING:
                print e
                return close()
        try:
            sent = sock.send(_buf)
            buf[0] = _buf = _buf[sent:]
            if _buf and not wait_write[0]:
                io_loop.modify_handler(fd, io_loop.READ | io_loop.WRITE)
                wait_write[0] = 1
            elif wait_write[0]:
                io_loop.modify_handler(fd, io_loop.READ | io_loop.WRITE)
                wait_write[0] = 0
        except socket.error as e:
            if e.args[0] not in NONBLOCKING:
                print e
                return _close()

    io_loop.add_handler(sock.fileno(), handle, io_loop.READ)



class Connection(object):
    __slots__ = ('_conn', '_buf')

    def __init__(self, connection):
        self._conn = connection
        self._buf = ""
        io_loop.add_handler(connection.fileno(), self.handle, io_loop.READ)

    def handle(self, fd, events):
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
        io_loop.remove_handler(self._conn.fileno())
        self._conn.close()
        self._conn = None


def connection_ready(sock, fd, events):
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
    sock.listen(128)

    io_loop = ioloop.IOLoop.instance()
    callback = functools.partial(connection_ready, sock)
    io_loop.add_handler(sock.fileno(), callback, io_loop.READ)
    try:
        io_loop.start()
    except KeyboardInterrupt:
        io_loop.stop()
        print "exited cleanly"
