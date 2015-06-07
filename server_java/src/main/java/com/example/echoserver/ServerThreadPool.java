package com.example.echoserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

class ServerThreadPool {

    public static void main(String[] args) throws IOException {
        ServerSocket server = new ServerSocket(5000);
        int threadPoolSize = Runtime.getRuntime().availableProcessors() * 8;
        ExecutorService executor = Executors.newFixedThreadPool(threadPoolSize);
        while (true) {
            Socket socket = server.accept();
            executor.execute(new ConnectionHandler(socket));
        }
    }
}
