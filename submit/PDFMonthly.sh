# $1 is the Year & Month numbers e.g 1510 = Oct 2015
# e.g. 
#     PDFMonthly.sh 1510
echo New PDF Copy Submit Routine

cd /ctools/bin

echo Making New Directory to Copy Files Into .....
mkdir  /BACKUPHDD/pdf/pdf01/$1/
#mkdir /BACKUPHDD/pdf/pdf02/$1/
mkdir  /BACKUPHDD/pdf/pdf03/$1/
#mkdir /BACKUPHDD/pdf/pdf04/$1/
#mkdir /BACKUPHDD/pdf/pdf05/$1/
#mkdir /BACKUPHDD/pdf/pdf06/$1/
mkdir  /BACKUPHDD/pdf/pdf07/$1/
#mkdir /BACKUPHDD/pdf/pdf08/$1/
mkdir  /BACKUPHDD/pdf/pdf09/$1/
mkdir  /BACKUPHDD/pdf/pdf10/$1/
#mkdir /BACKUPHDD/pdf/pdf11/$1/
mkdir  /BACKUPHDD/pdf/pdf12/$1/

echo Copying PDFs from Linux Server to Backup Drive .....
cp  /ctools/pdf01/* /BACKUPHDD/pdf/pdf01/$1/
#cp  /ctools/pdf02/* /BACKUPHDD/pdf/pdf02/$1/
cp  /ctools/pdf03/* /BACKUPHDD/pdf/pdf03/$1/
#cp  /ctools/pdf04/* /BACKUPHDD/pdf/pdf04/$1/
#cp  /ctools/pdf05/* /BACKUPHDD/pdf/pdf05/$1/
#cp  /ctools/pdf06/* /BACKUPHDD/pdf/pdf06/$1/
cp  /ctools/pdf07/* /BACKUPHDD/pdf/pdf07/$1/
#cp  /ctools/pdf08/* /BACKUPHDD/pdf/pdf08/$1/
cp  /ctools/pdf09/* /BACKUPHDD/pdf/pdf09/$1/
cp  /ctools/pdf10/* /BACKUPHDD/pdf/pdf10/$1/
#cp  /ctools/pdf11/* /BACKUPHDD/pdf/pdf11/$1/
cp  /ctools/pdf12/* /BACKUPHDD/pdf/pdf12/$1/

echo PDF update to BACKUPHDD is finished ........

