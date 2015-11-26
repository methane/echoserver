#!/bin/sh

if [ -z $1 ]; then
    host="sag15"
else
    host=$1
fi

[ $host = "localhost" ] || host $host || exit

./client -c50 -o2 -h10000 $host
./client -c50 -o2 -h10000 $host
./client -c50 -o2 -h10000 $host
