#!/bin/sh

nfpf=60
sleep=`expr $nfpf / 2 + $nfpf % 2`
wait=$nfpf

#./writeDataTest rate=15000 frames=1200 outfile=tmp/junk nfpf=$nfpf &
#sleep $sleep
#./bulkLoad user=root rdbms=mysql dbname=monitor pk=1 trans=1 directory=$PWD/tmp tabletype=innodb wait=3 &
#./bulkLoad rdbms=postgres dbname=monitor pk=1 trans=1 directory=$PWD/tmp &
#./bulkLoad rdbms=mysql user=root dbname=monitor pk=0 trans=1 directory=$PWD/tmp tabletype=myisam &
./bulkLoad rdbms=mysql user=root dbname=monitor pk=1 trans=1 directory=$PWD/tmp tabletype=innodb &
sleep 2
#./integrateData rdbms=postgres dbname=monitor
#./integrateData user=root rdbms=mysql dbname=monitor tabletype=myisam
./integrateData user=root rdbms=mysql dbname=monitor tabletype=innodb

