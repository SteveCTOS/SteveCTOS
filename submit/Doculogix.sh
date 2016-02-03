# $1 is the Year & Month numbers e.g 1510 = Sep 2015

echo Doculogix Submit Routine

cd /ctools/spl

rm /ctools/estate/*
rm /ctools/image/*

cd /ctools/ctj
cp dbprintco01 /ctools/image/InCo01
cp drlaserco01 /ctools/image/StCo01
cp drlaserco01 /ctools/image/CtjStat
cp dremailco01 /ctools/estate/$1Co01

cd /ctools/grc
cp dbprintco02 /ctools/image/InCo02
cp drlaserco02 /ctools/image/StCo02
cp drlaserco02 /ctools/image/GrcStat
cp dremailco02 /ctools/estate/$1Co02

cd /ctools/ctn
cp dbprintco03 /ctools/image/InCo03
cp drlaserco03 /ctools/image/StCo03
cp drlaserco03 /ctools/image/CtnStat
cp dremailco03 /ctools/estate/$1Co03

cd /ctools/orx
cp dbprintco05 /ctools/image/InCo05
cp drlaserco05 /ctools/image/StCo05

cd /ctools/sfj
cp dbprintco07 /ctools/image/InCo07
cp drlaserco07 /ctools/image/StCo07
cp drlaserco07 /ctools/image/SfjStat
cp dremailco07 /ctools/estate/$1Co07

cd /ctools/csc
cp dbprintco08 /ctools/image/InCo08
cp drlaserco08 /ctools/image/StCo08
cp drlaserco08 /ctools/image/CscStat
cp dremailco08 /ctools/estate/$1Co08
