var net = require('net');

var server = net.createServer(function (socket) {socket.pipe(socket);});
server.listen(5000, "0.0.0.0");
