echo Your Daily Backup is in Process,  Please be patient... 

mkdir /media/ctools/USB/data01
mkdir /media/ctools/USB/data02
mkdir /media/ctools/USB/data03
mkdir /media/ctools/USB/data04
mkdir /media/ctools/USB/data05
mkdir /media/ctools/USB/data06
mkdir /media/ctools/USB/data07
mkdir /media/ctools/USB/data08
mkdir /media/ctools/USB/data09
mkdir /media/ctools/USB/data10
mkdir /media/ctools/USB/data11
mkdir /media/ctools/USB/data12

echo Copying Data01.....
cp -a /ctools/data01/* /media/ctools/USB/data01
echo Copying Data02.....
cp -a /ctools/data02/* /media/ctools/USB/data02
echo Copying Data03.....
cp -a /ctools/data03/* /media/ctools/USB/data03
echo Copying Data04.....
cp -a /ctools/data04/* /media/ctools/USB/data04
echo Copying Data05.....
cp -a /ctools/data05/* /media/ctools/USB/data05
echo Copying Data06.....
cp -a /ctools/data06/* /media/ctools/USB/data06
echo Copying Data07.....
cp -a /ctools/data07/* /media/ctools/USB/data07
echo Copying Data08.....
cp -a /ctools/data08/* /media/ctools/USB/data08
echo Copying Data09.....
cp -a /ctools/data09/* /media/ctools/USB/data09
echo Copying Data10.....
cp -a /ctools/data10/* /media/ctools/USB/data10
echo Copying Data11.....
cp -a /ctools/data11/* /media/ctools/USB/data11
echo Copying Data12.....
cp -a /ctools/data12/* /media/ctools/USB/data12

rm /ctools/lock/_db.*

Submit MainCont TapeBackup.Sub01

lp -d ctj-splXtra -o cpi=17 /ctools/spl/ctj-aa1
lp -d ctj-splXtra -o cpi=17 /ctools/spl/ctj-aa2

echo Moving Faxes to FaxSent folder......
cp /ctools/fax/*.pdf /ctools/faxsent/
rm /ctools/fax/*

echo COPYING DATA TO BACKUP DISK ON SERVER
ll /media/ctools/BACKUPHDD/ctools/*

cp -a /ctools/data* /media/ctools/BACKUPHDD/ctools
