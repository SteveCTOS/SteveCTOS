# Enter ALL to get All as $1 the companies renamed to the Month below.
# Enter a Month Name like Jun, Aug etc. as $2 input.
# example.  From the command line run the following:
# GlMonthly.sh ALL Jun

Submit MainCont GlMonthly.Sub
#Submit MainCont GlMonthTemp

python renamegl.py $1 $2

echo   CT Group GL Monthly Reports Run Successfully 

