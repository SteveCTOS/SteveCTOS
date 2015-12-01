# $1 is the Year & Month numbers e.g 1510 = Sep 2015

echo Doculogix Submit Routine

cd /ctools/spl

rm /ctools/estate/*
rm /ctools/image/*

cp DBPrintCo01 /ctools/image/InCo01
cp DrLaserCo01 /ctools/image/StCo01
cp DrLaserCo01 /ctools/image/CtjStat
cp DrEMailCo01 /ctools/estate/$1Co01

cp DBPrintCo02 /ctools/image/InCo02
cp DrLaserCo02 /ctools/image/StCo02
cp DrLaserCo02 /ctools/image/GrcStat
cp DrEMailCo02 /ctools/estate/$1Co02

cp DBPrintCo03 /ctools/image/InCo03
cp DrLaserCo03 /ctools/image/StCo03
cp DrLaserCo03 /ctools/image/CtnStat
cp DrEMailCo03 /ctools/estate/$1Co03

cp DBPrintCo05 /ctools/image/InCo05

cp DBPrintCo07 /ctools/image/InCo07
cp DrLaserCo07 /ctools/image/StCo07
cp DrLaserCo07 /ctools/image/SfjStat
cp DrEMailCo07 /ctools/estate/$1Co07

cp DBPrintCo08 /ctools/image/InCo08
cp DrLaserCo08 /ctools/image/StCo08
cp DrLaserCo08 /ctools/image/CscStat
cp DrEMailCo08 /ctools/estate/$1Co08

