echo  
echo Your Daily Backup is in Process,  Please be patient... 

<<<<<<< HEAD
mkdir /media/ctools/BACKUP/data01
mkdir /media/ctools/BACKUP/data02
mkdir /media/ctools/BACKUP/data03
mkdir /media/ctools/BACKUP/data04
mkdir /media/ctools/BACKUP/data05
mkdir /media/ctools/BACKUP/data06
mkdir /media/ctools/BACKUP/data07
mkdir /media/ctools/BACKUP/data08
mkdir /media/ctools/BACKUP/data09
mkdir /media/ctools/BACKUP/data10
mkdir /media/ctools/BACKUP/data11
mkdir /media/ctools/BACKUP/data12

cp /ctools/data01/* /media/ctools/BACKUP/data01
cp /ctools/data02/* /media/ctools/BACKUP/data02
cp /ctools/data03/* /media/ctools/BACKUP/data03
cp /ctools/data05/* /media/ctools/BACKUP/data05
cp /ctools/data06/* /media/ctools/BACKUP/data06
cp /ctools/data07/* /media/ctools/BACKUP/data07
cp /ctools/data08/* /media/ctools/BACKUP/data08
cp /ctools/data09/* /media/ctools/BACKUP/data09
cp /ctools/data10/* /media/ctools/BACKUP/data10
cp /ctools/data11/* /media/ctools/BACKUP/data11
cp /ctools/data12/* /media/ctools/BACKUP/data12
=======
mkdir /media/ctools/BACKUPUSB/data01
mkdir /media/ctools/BACKUPUSB/data02
mkdir /media/ctools/BACKUPUSB/data03
mkdir /media/ctools/BACKUPUSB/data04
mkdir /media/ctools/BACKUPUSB/data05
mkdir /media/ctools/BACKUPUSB/data06
mkdir /media/ctools/BACKUPUSB/data07
mkdir /media/ctools/BACKUPUSB/data08
mkdir /media/ctools/BACKUPUSB/data09
mkdir /media/ctools/BACKUPUSB/data10
mkdir /media/ctools/BACKUPUSB/data11
mkdir /media/ctools/BACKUPUSB/data12

cp /ctools/data01/* /media/ctools/BACKUPUSB/data01
cp /ctools/data02/* /media/ctools/BACKUPUSB/data02
cp /ctools/data03/* /media/ctools/BACKUPUSB/data03
cp /ctools/data05/* /media/ctools/BACKUPUSB/data05
cp /ctools/data06/* /media/ctools/BACKUPUSB/data06
cp /ctools/data07/* /media/ctools/BACKUPUSB/data07
cp /ctools/data08/* /media/ctools/BACKUPUSB/data08
cp /ctools/data09/* /media/ctools/BACKUPUSB/data09
cp /ctools/data10/* /media/ctools/BACKUPUSB/data10
cp /ctools/data11/* /media/ctools/BACKUPUSB/data11
cp /ctools/data12/* /media/ctools/BACKUPUSB/data12
>>>>>>> 61db135ae8c7bfa4d31cad00a2dd1337ebe36da6

Submit MainCont TapeBackup.Sub01

lp -d ctj-spl /ctools/dev/source/print/Compressed.Key
lp -d ctj-spl /ctools/spl/ctj-aa1
lp -d ctj-spl /ctools/spl/ctj-aa2

echo COPYING DATA TO BACKUP DISK ON SERVER
cp -a /ctools/data* /media/ctools/BACKUPHDD/ctools/
