rm /ctools/hky/*

Submit MainCont MonthEnd.Sub11

Submit MainCont MonthEnd2.Sub11

cd /ctools/ps11
cat /ctools/ps11/* >> /ctools/hky/slp22
rm /ctools/ps11/*
cd /ctools/bin

cp /ctools/spl/DrStateCo11 /ctools/hky/drstateco11
cp /ctools/spl/DrLaserCo11 /ctools/hky/drlaserco11
#cp /ctools/spl/drnomalco11 /ctools/hky/drnomalco11
#cp /ctools/spl/dremailco11 /ctools/hky/dremailco11
cp /ctools/hky/sl11 /ctools/spl/hkydrage
cp /ctools/spl/DBPrintCo11 /ctools/hky/DBPrintCo11

mkdir /media/ctools/BACKUPUSB/data11
#cp /ctools/data01/* /media/ctools/BACKUPUSB/data01
#cp /ctools/data02/* /media/ctools/BACKUPUSB/data02
#cp /ctools/data03/* /media/ctools/BACKUPUSB/data03
#cp /ctools/data05/* /media/ctools/BACKUPUSB/data05
#cp /ctools/data06/* /media/ctools/BACKUPUSB/data06
#cp /ctools/data07/* /media/ctools/BACKUPUSB/data07
#cp /ctools/data08/* /media/ctools/BACKUPUSB/data08
#cp /ctools/data09/* /media/ctools/BACKUPUSB/data09
#cp /ctools/data10/* /media/ctools/BACKUPUSB/data10
cp /ctools/data11/* /media/ctools/BACKUPUSB/data11
#cp /ctools/data12/* /media/ctools/BACKUPUSB/data12


echo HKY MONTH-END DONE........
