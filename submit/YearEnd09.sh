rm /ctools/sfi/*

Submit MainCont MonthEnd.Sub09

cd /ctools/ps09
cat /ctools/ps09/* >> /ctools/sfi/slp22
rm /ctools/ps09/*

cp /ctools/spl/drstateco09 /ctools/sfi/drstateco09
cp /ctools/spl/drlaserco09 /ctools/sfi/drlaserco09
#cp /ctools/spl/drnomalco09 /ctools/sfi/drnomalco09
#cp /ctools/spl/dremailco09 /ctools/sfi/dremailco09
cp /ctools/sfi/sl11 /ctools/spl/sfidrage
cp /ctools/spl/dbprintco09 /ctools/sfi/dbprintco09

#cp /ctools/data01/* /media/ctools/RMB/data01
#cp /ctools/data02/* /media/ctools/RMB/data02
#cp /ctools/data03/* /media/ctools/RMB/data03
cp /ctools/data05/* /media/ctools/RMB/data05
#cp /ctools/data06/* /media/ctools/RMB/data06
#cp /ctools/data07/* /media/ctools/RMB/data07
#cp /ctools/data08/* /media/ctools/RMB/data08
#cp /ctools/data09/* /media/ctools/RMB/data09
#cp /ctools/data10/* /media/ctools/RMB/data10
#cp /ctools/data11/* /media/ctools/RMB/data11
#cp /ctools/data12/* /media/ctools/RMB/data12


echo SFI YEAR-END DONE........
