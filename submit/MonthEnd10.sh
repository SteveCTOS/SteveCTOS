rm /ctools/qtm/*

Submit MainCont MonthEnd.Sub10

Submit MainCont MonthEnd2.Sub10

cd /ctools/ps10
cat /ctools/ps10/* >> /ctools/qtm/slp22
rm /ctools/ps10/*
cd /ctools/bin

cp /ctools/spl/DrStateCo10 /ctools/qtm/drstateco10
cp /ctools/spl/DrLaserCo10 /ctools/qtm/drlaserco10
#cp /ctools/spl/drnomalco10 /ctools/qtm/drnomalco10
#cp /ctools/spl/dremailco10 /ctools/qtm/dremailco10
cp /ctools/qtm/sl11 /ctools/spl/qtmdrage
cp /ctools/spl/DBPrintCo10 /ctools/qtm/DBPrintCo10

mkdir /media/ctools/BACKUPUSB/data10
#cp /ctools/data01/* /media/ctools/BACKUPUSB/data01
#cp /ctools/data02/* /media/ctools/BACKUPUSB/data02
#cp /ctools/data03/* /media/ctools/BACKUPUSB/data03
#cp /ctools/data05/* /media/ctools/BACKUPUSB/data05
#cp /ctools/data06/* /media/ctools/BACKUPUSB/data06
#cp /ctools/data07/* /media/ctools/BACKUPUSB/data07
#cp /ctools/data08/* /media/ctools/BACKUPUSB/data08
#cp /ctools/data09/* /media/ctools/BACKUPUSB/data09
cp /ctools/data10/* /media/ctools/BACKUPUSB/data10
#cp /ctools/data11/* /media/ctools/BACKUPUSB/data11
#cp /ctools/data12/* /media/ctools/BACKUPUSB/data12


echo QTM MONTH-END DONE........
