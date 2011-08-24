Echo servers in some languages
=================================

いろんな言語＋フレームワークで echo server を書いてみました。

いくつかのコードはサンプルプログラムにあったそのままのコードを書いていて、
epoll, thread 版は自分で作成したものです。

各 echo server の実装は全く同じものではないのであくまでも参考ですが、
パフォーマンスはこんなかんじです。

test environment
-----------------

client and server are connected by Gigabit Ethernet.

client (sag14)
^^^^^^^^^^^^^^^
Debian 5.0.4 (i386)
Linux sag14 2.6.26.5 #2 SMP Mon Sep 29 14:17:40 JST 2008 i686 GNU/Linux
Intel(R) Core(TM)2 CPU          6300  @ 1.86GHz (dual)

server (sag15)
^^^^^^^^^^^^^^^
Debian 6.0.2 (amd64)
Linux sag15 2.6.32-5-amd64 #1 SMP Mon Mar 7 21:35:22 UTC 2011 x86_64 GNU/Linux
Intel(R) Core(TM)2 CPU          6300  @ 1.86GHz (dual)

test command
-------------

bench.sh does kick client 3 times with following option.

::

   ./client -c50 -o2 -h10000 sag15

C++ epoll
---------

server::

   ./echo_server_epoll

result::

   Throughput: 117036.86 [#/sec]
   Throughput: 118702.63 [#/sec]
   Throughput: 119129.38 [#/sec]

with forking.

server::

   ./echo_server_epoll -f2

result::

   Throughput: 146602.29 [#/sec]
   Throughput: 132542.38 [#/sec]
   Throughput: 149737.19 [#/sec]


C++ thread
-----------

server::

   ./echo_server_thread -c120

result::

   Throughput: 122903.04 [#/sec]
   Throughput: 121306.08 [#/sec]
   Throughput: 122511.62 [#/sec]


Haskell
----------

GHC 7.0.3

server::

   ./echo_server_haskell

result::

   Throughput: 80391.53 [#/sec]
   Throughput: 82943.94 [#/sec]
   Throughput: 76325.74 [#/sec]


Erlang
-------------

server::

   $ erl
   Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]
   Eshell V5.8  (abort with ^G)
   1> c(echo_server_erlang, [native, {hipe, ['O3']}]).
   {ok,echo_server_erlang}
   2> echo_server_erlang:listen(5000).

result::

   Throughput: 61698.85 [#/sec]
   Throughput: 73994.04 [#/sec]
   Throughput: 72668.51 [#/sec]


Go (r59)
-------------

server::

   $ ./echo_server_go

result::

   Throughput: 43505.17 [#/sec]
   Throughput: 43346.11 [#/sec]
   Throughput: 43198.26 [#/sec]

server::

   $ GOMAXPROCS=3 ./echo_server_go

result::

   Throughput: 52087.16 [#/sec]
   Throughput: 52070.02 [#/sec]
   Throughput: 52068.27 [#/sec]


pypy 1.6 + Tornado
---------------------

server::

   ~/pypy-1.6/bin/pypy echo_server_tornado.py 

result::

   Throughput: 79193.30 [#/sec]
   Throughput: 81063.83 [#/sec]
   Throughput: 81442.70 [#/sec]


pypy 1.6 + twisted
-------------------

server::

   ~/pypy-1.6/bin/pypy echo_server_twisted.py 

result::

   Throughput: 37630.81 [#/sec]
   Throughput: 49274.60 [#/sec]
   Throughput: 41053.66 [#/sec]


node.js  0.5.4
---------------

server::

   ~/local/node-0.5.4/bin/node echo_server_nodejs.js


result::

   Throughput: 34713.88 [#/sec]
   Throughput: 35965.09 [#/sec]
   Throughput: 36288.78 [#/sec]



Ruby 1.9.1 + EventMachine 0.12.10
-----------------------------------

server::

   $ ruby1.9.1 echo_server_em.rb

result::

   Throughput: 74124.61 [#/sec]
   Throughput: 73578.20 [#/sec]
   Throughput: 75241.61 [#/sec]



Ruby 1.9.1 + rev 0.3.2
-------------------------

server::

   $ ruby1.9.1 echo_server_rev.rb

result::

   Throughput: 32372.56 [#/sec]
   Throughput: 32647.37 [#/sec]
   Throughput: 32517.97 [#/sec]



Python 2.7.2 + Tornado
-------------------------

server::

   ~/python2.7/bin/python echo_server_tornado.py

result::

   Throughput: 59626.30 [#/sec]
   Throughput: 50793.45 [#/sec]
   Throughput: 51566.35 [#/sec]


Python 2.7.2 + gevent
-------------------------------

server::

   ~/python2.7/bin/python echo_server_gevent.py

result for gevent 0.13.6::

   Throughput: 17751.24 [#/sec]
   Throughput: 17607.05 [#/sec]
   Throughput: 17537.34 [#/sec]

result for gevent 1.0a2::

   Throughput: 19433.81 [#/sec]
   Throughput: 19455.66 [#/sec]
   Throughput: 19371.97 [#/sec]


Python 2.7.2 + Twisted
----------------------

server::

   ~/python2.7/bin/python echo_server_twidted.py

result::

   Throughput: 14339.96 [#/sec]
   Throughput: 13982.39 [#/sec]
   Throughput: 13841.22 [#/sec]


..
   vim: paste sw=3 expandtab
