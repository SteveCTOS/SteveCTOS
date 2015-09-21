# Enter ALL to get All as $1 the companies renamed to the Month below.
# Enter a Month Name like Jun, Aug etc. as $2 input.
<<<<<<< HEAD
#for example    Submit MainCont GlMonthly.Sub ALL Jun
=======
# example.  From the command line run the following:
# GlMonthly.sh ALL Jun

>>>>>>> 59def9fcb1400b8435a45747be4fdd1d3d816821
Submit MainCont GlMonthly.Sub
#Submit MainCont GlMonthTemp

python renamegl.py $1 $2

echo   CT Group GL Monthly Reports Run Successfully 

