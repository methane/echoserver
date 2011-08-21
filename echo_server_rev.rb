#!/usr/bin/env ruby

require 'rubygems'
require 'rev'

HOST = ''
PORT = 5000

class EchoServerConnection < Rev::TCPSocket
    def on_read(data)
        write data
    end
end

server = Rev::TCPServer.new(HOST, PORT, EchoServerConnection)
server.attach(Rev::Loop.default)

puts "Echo server listening on #{HOST}:#{PORT}"
Rev::Loop.default.run
