echo Your Friday Backup is in Process,  Please be patient... 

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

cp /ctools/data01/* /media/ctools/USB/data01
cp /ctools/data02/* /media/ctools/USB/data02
cp /ctools/data03/* /media/ctools/USB/data03
cp /ctools/data04/* /media/ctools/USB/data04
cp /ctools/data05/* /media/ctools/USB/data05
cp /ctools/data06/* /media/ctools/USB/data06
cp /ctools/data07/* /media/ctools/USB/data07
cp /ctools/data08/* /media/ctools/USB/data08
cp /ctools/data09/* /media/ctools/USB/data09
cp /ctools/data10/* /media/ctools/USB/data10
cp /ctools/data11/* /media/ctools/USB/data11
cp /ctools/data12/* /media/ctools/USB/data12

#cd /ctools/ps01
#echo TapebackupvolMessage2 
#Zip Weekend1.Zip *-*
#rm *-*
#rm /ctools/spl/*
#cd /ctools/bin

echo Removing Spl and Lock files.....
rm /ctools/lock/*
rm /ctools/spl/*

Submit MainCont TapeBackupVol.Sub01

echo Running TapeBackupVol.Sub01 Complete.......

lp -d ctj-splXtra /ctools/dev/source/print/Compressed.Key
lp -d ctj-splXtra /ctools/spl/ctj-aa1
lp -d ctj-splXtra /ctools/spl/ctj-aa2

echo COPYING COMPANY DATA TO BACKUP DISK ON SERVER
cp -a /ctools/data* /media/ctools/BACKUPHDD/ctools/

echo COPYING COMPLETE DRIVES DATA TO BACKUP DISK ON SERVER
cp -a /ctools/* /media/ctools/BACKUPHDD/ctools/

