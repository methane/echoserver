CXXFLAGS=-O2 -g -Wall -pthread -lrt
CFLAGS = -O2 -g -Wall -pthread -lrt --std=gnu99

all: echo_server_epoll echo_server_thread client

echo_server_epoll: echo_server_epoll.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

echo_server_thread: echo_server_thread.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

echo_server_libev: echo_server_libev.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< -lev

null_server_epoll: null_server_epoll.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

null_server_thread: null_server_thread.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

echo_server_go: echo_server_go.go
	6g -o echo_server_go.6 $<
	6l -o $@ echo_server_go.6

echo_server_haskell: echo_server_haskell.hs
	ghc6 -threaded -O --make -o $@ $<

echo_server_erlang: echo_server_erlang.erl
	erlc $<

client: client.c
	$(CC) $(CFLAGS) $< -o $@
