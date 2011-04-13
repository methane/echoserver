CXXFLAGS=-O2 -g -Wall -pthread

all: echo_server_epoll echo_server_thread

echo_server_epoll: echo_server_epoll.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<


echo_server_thread: echo_server_thread.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

