rm /ctools/grc/*

Submit MainCont MonthEnd.Sub02

cd /ctools/ps02
cat /ctools/ps02/* >> /ctools/grc/slp22
rm /ctools/ps02/*

cp /ctools/spl/DrStateCo02 /ctools/grc/drstateco02
cp /ctools/spl/DrLaserCo02 /ctools/grc/drlaserco02
cp /ctools/spl/DrNoMalCo02 /ctools/grc/drnomalco02
cp /ctools/spl/DrEmailCo02 /ctools/grc/dremailco02
#cp /ctools/grc/sl11 /ctools/spl/grcdrage
cp /ctools/spl/DBPrintCo02 /ctools/grc/dbprintco02

echo Backing up data02 information......
mkdir /media/ctools/RMB/data02
#cp /ctools/data01/* /media/ctools/RMB/data01
cp /ctools/data02/* /media/ctools/RMB/data02
#cp /ctools/data03/* /media/ctools/RMB/data03
#cp /ctools/data05/* /media/ctools/RMB/data05
#cp /ctools/data06/* /media/ctools/RMB/data06
#cp /ctools/data07/* /media/ctools/RMB/data07
#cp /ctools/data08/* /media/ctools/RMB/data08
#cp /ctools/data09/* /media/ctools/RMB/data09
#cp /ctools/data10/* /media/ctools/RMB/data10
#cp /ctools/data11/* /media/ctools/RMB/data11
#cp /ctools/data12/* /media/ctools/RMB/data12

lp -d ctj-spl /ctools/grc/st28*

echo GRC MONTH-END DONE........
