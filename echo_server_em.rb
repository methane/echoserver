#!/usr/bin/env ruby

require 'rubygems'
require 'eventmachine'

HOST = ''
PORT = 5000

module Echo
  def receive_data(data)
      send_data data
  end
end

EventMachine.run { 
  puts "Echo server listening on #{HOST}:#{PORT}"
  EventMachine.start_server HOST, PORT, Echo
}
