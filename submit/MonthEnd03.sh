rm /ctools/ctn/*

Submit MainCont MonthEnd.Sub03

Submit MainCont MonthEnd2.Sub03

cd /ctools/ps03
cat /ctools/ps03/* >> /ctools/ctn/slp22
rm /ctools/ps03/*

cp /ctools/spl/drstateco03 /ctools/ctn/drstateco03
cp /ctools/spl/drlaserco03 /ctools/ctn/drlaserco03
cp /ctools/spl/drnomalco03 /ctools/ctn/drnomalco03
cp /ctools/spl/dremailco03 /ctools/ctn/dremailco03
cp /ctools/ctn/sl11 /ctools/spl/ctndrage
cp /ctools/spl/dbprintco03 /ctools/ctn/dbprintco03

mkdir /media/ctools/BACKUPUSB/data03

#cp /ctools/data01/* /media/ctools/BACKUPUSB/data01
#cp /ctools/data02/* /media/ctools/BACKUPUSB/data02
#cp /ctools/data03/* /media/ctools/BACKUPUSB/data03
cp /ctools/data05/* /media/ctools/BACKUPUSB/data05
#cp /ctools/data06/* /media/ctools/BACKUPUSB/data06
#cp /ctools/data07/* /media/ctools/BACKUPUSB/data07
#cp /ctools/data08/* /media/ctools/BACKUPUSB/data08
#cp /ctools/data09/* /media/ctools/BACKUPUSB/data09
#cp /ctools/data10/* /media/ctools/BACKUPUSB/data10
#cp /ctools/data11/* /media/ctools/BACKUPUSB/data11
#cp /ctools/data12/* /media/ctools/BACKUPUSB/data12


echo CTN MONTH-END DONE........
