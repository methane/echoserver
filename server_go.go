package main

import (
	"io"
	"log"
	"net"
)

func echo_handler(conn net.Conn) {
	defer conn.Close()
	io.Copy(conn, conn)
}

func main() {
	psock, e := net.Listen("tcp", ":5000")
	if e != nil {
		log.Fatal(e)
		return
	}
	for {
		conn, e := psock.Accept()
		if e != nil {
			log.Fatal(e)
			return
		}
		go echo_handler(conn)
	}
}
