rm /ctools/ctj/*

Submit MainCont MonthEnd.Sub01

Submit MainCont MonthEnd2.Sub01

cd /ctools/ps01
cat /ctools/ps01/* >> /ctools/ctj/slp22
rm /ctools/ps01/*

cp /ctools/spl/DrStateCo01 /ctools/ctj/drstateco01
cp /ctools/spl/DrLaserCo01 /ctools/ctj/drlaserco01
cp /ctools/spl/DrNoMalCo01 /ctools/ctj/drnomalco01
cp /ctools/spl/DrEmailCo01 /ctools/ctj/dremailco01
cp /ctools/ctj/sl11 /ctools/spl/ctjdrage
cp /ctools/spl/DBPrintco01 /ctools/ctj/dbprintco01

cp /ctools/data01/* /media/ctools/RMB/data01
cp /ctools/data02/* /media/ctools/RMB/data02
cp /ctools/data03/* /media/ctools/RMB/data03
cp /ctools/data05/* /media/ctools/RMB/data05
cp /ctools/data06/* /media/ctools/RMB/data06
cp /ctools/data07/* /media/ctools/RMB/data07
cp /ctools/data08/* /media/ctools/RMB/data08
cp /ctools/data09/* /media/ctools/RMB/data09
cp /ctools/data10/* /media/ctools/RMB/data10
cp /ctools/data11/* /media/ctools/RMB/data11
cp /ctools/data12/* /media/ctools/RMB/data12

echo CTJ MONTH-END DONE........
