rm /ctools/krs/*

Submit MainCont MonthEnd.Sub12

Submit MainCont MonthEnd2.Sub12

cd /ctools/ps12
cat /ctools/ps12/* >> /ctools/krs/slp22 
rm /ctools/ps12/*
cd /ctools/bin

cp /ctools/spl/DrStateCo12 /ctools/krs/drstateco12
cp /ctools/spl/ctoolsDrLaserCo12 /ctools/krs/drlaserco12
#cp /ctools/spl/drnomalco12 /ctools/krs/drnomalco12
#cp /ctools/spl/dremailco12 /ctools/krs/dremailco12
#cp /ctools/krs/sl11 /ctools/spl/krsdrage
cp /ctools/spl/DBPrintCo12 /ctools/krs/DBPrintCo12

mkdir /media/ctools/BACKUPUSB/data12
#cp /ctools/data01/* /media/ctools/BACKUPUSB/data01
#cp /ctools/data02/* /media/ctools/BACKUPUSB/data02
#cp /ctools/data03/* /media/ctools/BACKUPUSB/data03
#cp /ctools/data05/* /media/ctools/BACKUPUSB/data05
#cp /ctools/data06/* /media/ctools/BACKUPUSB/data06
#cp /ctools/data07/* /media/ctools/BACKUPUSB/data07
#cp /ctools/data08/* /media/ctools/BACKUPUSB/data08
#cp /ctools/data09/* /media/ctools/BACKUPUSB/data09
#cp /ctools/data10/* /media/ctools/BACKUPUSB/data10
#cp /ctools/data11/* /media/ctools/BACKUPUSB/data11
cp /ctools/data12/* /media/ctools/BACKUPUSB/data12

echo KRS MONTH-END DONE........
