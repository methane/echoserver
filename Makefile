CXXFLAGS=-O2 -g -Wall

echo_server_epoll: echo_server_epoll.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<
