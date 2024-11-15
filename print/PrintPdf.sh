text2pdf $1 -s7Courier-Bold -x625 -y828 -l66 -c135 -V16 -H41 > $1.pdf
 
 if [ "$3" = "15"  ]
    then
     lp -d $2 $1.pdf
 fi


#text2pdf /ctools/spl/test -fCourier-Bold -x588 -y828 -l66 -c88 -V16 -H41 > /ctools/spl/QuoteTest.pdf


#pdftk /ctools/fax/$1Quote.temp1.pdf background /ctools/dev/source/print/$1QuoteOverlay1.pdf output /ctools/fax/$1Quote$3.pdf

#if [ "$4" = "01" ] && [ "$6" = "P" ]
#   then
#     lp -d $2 /ctools/fax/$1Quote$3.pdf
# fi

#if [ "$4" = "01" ] && [ "$5" = "F" ]
#   then
#     sendfax -n -d $7 /ctools/fax/$1Quote$3.pdf
# fi

#lp -d $2 $3
#--- the following pdftk cats the first statement to a single file from two
#--- this is basically for testing faxes with only one statement
##pdftk QuoteCo01.pdf cat 1-1 output single.pdf

#FORMAT FOR PRINTING IS:
#lp -d W-PRINTERNAME W-PRINTFILE

#--So far the variables which will be specified from inside the COBOL programs are:
#---$1 = The company number from 01 to 12     e.g. 07
#---$2 = The Printername to print the file to e.g. MP140
#---$3 = The Quote file name e.g. Q90555.05.15 specified in COBOL
#---$4 = The page-count.  IF page-cnt=01 then print, If=02 OR HIGHER then don't print- use PrintQuote2
#---$5 = WS-AUTO-FAX.     IF = F then send by fax, E=EMail
#---$6 = WS-ANSWER.       IF = P then print PDF
#---$7 = WS-fax-number.  Used ONLY if sending by fax.
#--- add any extra commands here - perhaps cups printing
#--- or sendfax via hylaFAX
