CXXFLAGS=-O2 -g -Wall -pthread

all: echo_server_epoll echo_server_thread

echo_server_epoll: echo_server_epoll.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

echo_server_thread: echo_server_thread.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

null_server_epoll: null_server_epoll.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

null_server_thread: null_server_thread.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<


