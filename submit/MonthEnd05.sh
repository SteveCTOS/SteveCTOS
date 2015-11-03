rm /ctools/orx/*

Submit MainCont MonthEnd.Sub05

Submit MainCont MonthEnd2.Sub05

cd /ctools/ps05
cat /ctools/ps05/* >> /ctools/orx/slp22
rm /ctools/ps05/*

cp /ctools/spl/DrStateCo05 /ctools/orx/drstateco05
cp /ctools/spl/DrLaserCo05 /ctools/orx/drlaserco05
#cp /ctools/spl/drnomalco05 /ctools/orx/drnomalco05
#cp /ctools/spl/dremailco05 /ctools/orx/dremailco05
#cp /ctools/orx/sl11 /ctools/spl/orxdrage
cp /ctools/spl/DBPrintCo05 /ctools/orx/DBPrintCo05

echo Backing up data05 information......
mkdir /media/ctools/BACKUPUSB/data05
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

echo ORX MONTH-END DONE........
