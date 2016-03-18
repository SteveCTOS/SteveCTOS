        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlInStLy.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectStMaster".
         Copy "SelectSlRegLy".
         Copy "SelectStTransLy".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdRegisterLy.
           COPY ChlfdStTransLy.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-ANSWER1           PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-STOCK-CHECK       PIC X(15) VALUE " ".
       77  WS-STOCK-CHECK-SAVE  PIC X(15) VALUE " ".
       77  WS-ACCOUNT-NUMBER    PIC 9(7) VALUE 0.
       77  WS-ORDERQTY          PIC 9(5) VALUE 0.
       77  WS-SHIPQTY           PIC S9(4) VALUE 0.
       77  WS-CREDQTY           PIC S9(4) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1   PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1 PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  WS-SALEDATE.
           03 WS-SALEYY         PIC 9999.
           03 WS-SALEMM         PIC 99.
           03 WS-SALEDD         PIC 99.
       01  WS-SPLIT-DESC.
           03  WS-SP-1          PIC X(5) VALUE " ".
           03  WS-SP-REST       PIC X(10) VALUE " ".
       01  WS-SPLIT-INPUT-DESC.
           03  WS-SP-I-1        PIC X(5) VALUE " ".
           03  WS-SP-I-REST     PIC X(10) VALUE " ".
       01  WS-STDESC.
           03  WS-DESC1          PIC X(20) VALUE " ".
           03  WS-DESC2          PIC X(20) VALUE " ".
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** INVOICED ORDERS INQUIRY BY STOCK - L/YR **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(10) VALUE "STOCK :".
           03  H2-STOCK       PIC X(20).
           03  FILLER         PIC X(13) VALUE "DESCRIPTION:".
           03  H2-DESC1       PIC X(20).
           03  H2-DESC2       PIC X(58).
       01  HEAD3.
           03  FILLER         PIC X(8) VALUE "ACCOUNT".
           03  FILLER         PIC X(26) VALUE "NAME ".
           03  FILLER         PIC X(24) VALUE "ORDER READY SHIPD".
           03  FILLER         PIC X(29) VALUE "PRICE P/ORDER NUMBER".
           03  FILLER         PIC X(18) VALUE "ORDER    DATE".
           03  FILLER         PIC X(17) VALUE "INV      DATE".
           03  H3-STOCK       PIC X(10) VALUE " ".
       01  DETAIL-LINE.
           03  D-ACCOUNT      PIC X(8).
           03  D-NAME         PIC X(26).
           03  D-ORDERQTY     PIC Z(4)9.
           03  FILLER         PIC X VALUE " ".
           03  D-SHIPQTY      PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SHIPPEDQTY   PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PRICE        PIC Z(7)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PONO         PIC X(22).
           03  D-1STINV       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-1STDATE      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-2NDINV       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-2NDDATE      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-ST-NO        PIC X(15).
       01  TOTAL-LINE.
           03  FILLER         PIC X(12).
           03  T-NAME         PIC X(22) VALUE "TOTALS FOR STOCK ITEM:".
           03  T-ORDERQTY     PIC Z(4)9.
           03  FILLER         PIC X VALUE " ".
           03  T-SHIPQTY      PIC Z(4)9.
           03  FILLER         PIC X(85) VALUE " ".
       Copy "WsDateInfo".
      *
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 0310 TO POS
           DISPLAY "** Sales Invoice Enquiry By Stock - L/YR **"
           AT POS
           MOVE 0410 TO POS
           DISPLAY "*******************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 2510 TO POS
           DISPLAY "Program Loading, Please be patient..." AT POS.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0420 TO POS
            DISPLAY "** LAST YEAR INFO **" AT POS.
            
            MOVE " " TO WS-SPLIT-INPUT-DESC
                        WS-SPLIT-DESC
                        H3-STOCK
                        D-ST-NO.
            MOVE 0 TO WS-ORDERQTY
                      WS-SHIPQTY.
            MOVE 0   TO WS-ACCOUNT-NUMBER.
            MOVE "N" TO WS-ANSWER WS-ANSWER1.
            PERFORM ERROR1-020.

            MOVE "STOCK" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-STOCK-NEXT
                 GO TO GET-010.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-STOCK-PREVIOUS
                 GO TO GET-010.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ST-STOCKNUMBER SPLIT-STOCK.
            IF ST-STOCKNUMBER = 0 OR = "   "
                CLOSE INCR-LY-REGISTER
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                OPEN I-O INCR-LY-REGISTER
                OPEN I-O STOCK-MASTER
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF SP-1STCHAR = "/"
               MOVE "Y" TO WS-ANSWER1
               MOVE ST-STOCKNUMBER TO WS-SPLIT-INPUT-DESC
               MOVE 2510 TO POS
               DISPLAY "NB !!  ONLY ITEMS THAT BEGIN WITH THE 1ST 5" &
               " DIGITS WILL BE PRINTED." AT POS.
            IF SP-1STCHAR NOT = "/"
               PERFORM READ-STOCK
               GO TO GET-020.
       GET-010.
            MOVE "STOCK"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD
            MOVE 15             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-020.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.

            MOVE "DESC"          TO F-FIELDNAME
            MOVE 4               TO F-CBFIELDNAME
            MOVE WS-STDESC       TO F-NAMEFIELD
            MOVE 40              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-030.
            IF ST-DESCRIPTION1 = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
       GET-031.
            MOVE "                        " TO F-NAMEFIELD
            MOVE "ACCNUM" TO F-FIELDNAME
            MOVE 6        TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"01"
                GO TO GET-000.
            MOVE 7        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = "  "
                MOVE 0 TO DR-ACCOUNT-NUMBER WS-ACCOUNT-NUMBER
                GO TO GET-035.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER WS-ACCOUNT-NUMBER.
            PERFORM READ-DEBTOR.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "INVALID" TO F-NAMEFIELD
               MOVE "ACCNUM" TO F-FIELDNAME
               MOVE 6        TO F-CBFIELDNAME
               MOVE 7        TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
               CALL "LOCKKBD" USING F-FIELDNAME
               GO TO GET-031.

            MOVE "ACCNAME" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE DR-NAME   TO F-NAMEFIELD.
            MOVE 40        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-035.
           MOVE "                        " TO F-NAMEFIELD
           MOVE "DATE" TO F-FIELDNAME
           MOVE 4      TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO GET-000.
           MOVE 10       TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           IF F-NAMEFIELD = "  "
               MOVE 0 TO WS-SALEDATE
               GO TO GET-040.
           MOVE F-NAMEFIELD TO ALPHA-RATE.
           PERFORM DATE-CHECKING.
           IF SIGN-FOUND = 9
              GO TO GET-035.
           MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
           MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
           MOVE DISPLAY-DATE    TO F-NAMEFIELD.
           PERFORM WRITE-FIELD-ALPHA.
           PERFORM CONVERT-SPLIT-FORMAT.
           MOVE SPLIT-DATE TO WS-SALEDATE WS-DATE.
           PERFORM CHECK-DATE-VALID.
           IF SIGN-FOUND = 9
              GO TO GET-035.
           PERFORM ERROR-020.
            
           MOVE "DATE"       TO F-FIELDNAME
           MOVE 4            TO F-CBFIELDNAME
           MOVE WS-SALEDATE  TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO F-NAMEFIELD.
           MOVE 10           TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-ALPHA.
       GET-040.
            MOVE " " TO F-EXIT-CH.

            PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
               CLOSE STOCK-TRANSLY-FILE
               GO TO GET-999.
            IF F-INDEX < 15
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Invoiced Orders."
                  AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE 3010 TO POS
                DISPLAY "                                        "
                   AT POS
                GO TO GET-999.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           PERFORM OPEN-006.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
           MOVE 0   TO SUB-2.
       RDTR-005.
           MOVE "Y"            TO STTR-LY-ST-COMPLETE.
           MOVE ST-STOCKNUMBER TO STTR-LY-STOCK-NUMBER.
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-ST-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 = 23 OR 35 OR 49
                GO TO RDTR-999.
           IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "STTRANSLY FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO RDTR-005.
           MOVE " " TO F-EXIT-CH.
       RDTR-010.
           IF F-EXIT-CH = " "
            READ STOCK-TRANSLY-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
            READ STOCK-TRANSLY-FILE PREVIOUS
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = " "
            IF WS-STTRANSLY-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF WS-STTRANSLY-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-000.
           IF WS-STTRANSLY-ST1 NOT = 0
            MOVE "STTRANSLY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO RDTR-010.
           IF STTR-LY-TYPE NOT = 1 AND NOT = 6
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           IF STTR-LY-ST-COMPLETE NOT = "Y"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-999.
           MOVE STTR-LY-STOCK-NUMBER TO SPLIT-STOCK.
           IF F-EXIT-CH = " "
            IF WS-ANSWER1 = "Y"
            IF SP-1STCHAR NOT = "/"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF WS-ANSWER1 = "Y"
            IF SP-1STCHAR NOT = "/"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-000.
           IF WS-ANSWER1 = "N"
            IF F-EXIT-CH = " "
             IF STTR-LY-STOCK-NUMBER NOT = ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-999.
           IF WS-ANSWER1 = "N"
            IF F-EXIT-CH = 1
             IF STTR-LY-STOCK-NUMBER NOT = ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-000.
           IF WS-ACCOUNT-NUMBER NOT = 0
            IF STTR-LY-ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           IF WS-SALEDATE NOT = 0
            IF STTR-LY-DATE < WS-SALEDATE
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           PERFORM ERROR-020.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2910 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' for Previous,"
                 AT POS
                ADD 43 TO POS
                DISPLAY "OR 'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Invoiced Orders."
                   AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1 TO F-EXIT-CH
                MOVE 1 TO F-INDEX.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                CLOSE STOCK-TRANSLY-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                MOVE 3010 TO POS
                DISPLAY "                                        "
                   AT POS
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           PERFORM READ-ORDER-REGISTER.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           PERFORM ERROR-020
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY "Printing In Progress, Please Be Patient." AT POS
           MOVE 0  TO PAGE-CNT SUB-2
                     WS-ORDERQTY
                     WS-SHIPQTY.
           MOVE 66 TO LINE-CNT.

           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.

           PERFORM OPEN-006.
           MOVE "Y"                    TO STTR-LY-ST-COMPLETE.
           IF WS-ANSWER1 = "Y"
              MOVE WS-SPLIT-INPUT-DESC TO STTR-LY-STOCK-NUMBER
           ELSE
              MOVE ST-STOCKNUMBER      TO STTR-LY-STOCK-NUMBER.
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-ST-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "STTRANSLY FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO PRR-900.
       PRR-002.
            READ STOCK-TRANSLY-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 = 10
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO PRR-900.
           IF WS-STTRANSLY-ST1 NOT = 0
            MOVE "STTRANSLY BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO PRR-002.
           IF STTR-LY-TYPE NOT = 1 AND NOT = 6
               GO TO PRR-002.
           IF STTR-LY-ST-COMPLETE NOT = "Y"
               GO TO PRR-900.
           MOVE STTR-LY-STOCK-NUMBER TO SPLIT-STOCK WS-SPLIT-DESC.
           IF WS-ANSWER1 = "N"
            IF STTR-LY-STOCK-NUMBER < ST-STOCKNUMBER
               GO TO PRR-002.
           IF WS-ANSWER1 = "Y"
            IF STTR-LY-STOCK-NUMBER < WS-SPLIT-INPUT-DESC
               GO TO PRR-002.
           IF WS-ANSWER1 = "N"
             IF STTR-LY-STOCK-NUMBER > ST-STOCKNUMBER
               GO TO PRR-900.
           IF WS-ANSWER1 = "Y"
            IF WS-SP-1 NOT = WS-SP-I-1
               GO TO PRR-900.
           IF WS-ANSWER1 = "Y"
            IF SP-1STCHAR NOT = "/"
               GO TO PRR-900.
           IF WS-ACCOUNT-NUMBER NOT = 0
            IF STTR-LY-ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
               GO TO PRR-002.
           IF WS-SALEDATE NOT = 0
            IF STTR-LY-DATE < WS-SALEDATE
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           IF STTR-LY-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
           MOVE STTR-LY-ACCOUNT-NUMBER TO DR-ACCOUNT-NUMBER
              READ DEBTOR-MASTER
                  INVALID KEY
                  MOVE "UNKNOWN"        TO D-NAME.
           IF STTR-LY-REFERENCE1 NOT = INCR-LY-INVOICE
              PERFORM READ-ORDER-REGISTER.
           MOVE DR-ACCOUNT-NUMBER       TO D-ACCOUNT
           MOVE DR-NAME                 TO D-NAME
           MOVE INCR-LY-PORDER             TO D-PONO.
           IF STTR-LY-TYPE = 1
              MOVE STTR-LY-ORDERQTY        TO D-ORDERQTY.
           MOVE STTR-LY-SHIPQTY            TO D-SHIPQTY.
           IF STTR-LY-TYPE = 1
              MOVE STTR-LY-SHIPPEDQTY      TO D-SHIPPEDQTY.
           MOVE STTR-LY-PRICE              TO D-PRICE
           MOVE INCR-LY-BO-INV-NO          TO D-1STINV
           MOVE INCR-LY-BO-DATE            TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-1STDATE
           MOVE STTR-LY-REFERENCE1         TO D-2NDINV
           MOVE STTR-LY-DATE               TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-2NDDATE.
           IF WS-ANSWER1 = "Y"
               MOVE STTR-LY-STOCK-NUMBER   TO D-ST-NO.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           IF STTR-LY-TYPE = 1
              ADD STTR-LY-ORDERQTY            TO WS-ORDERQTY
              ADD STTR-LY-SHIPQTY             TO WS-SHIPQTY
           ELSE
              SUBTRACT STTR-LY-SHIPQTY FROM WS-SHIPQTY.
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-060.
           ADD 1         TO PAGE-CNT
           MOVE PAGE-CNT TO H1-PAGE.

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.

           IF PAGE-CNT = 1
               WRITE PRINT-REC FROM HEAD1
           ELSE
               WRITE PRINT-REC FROM HEAD1 AFTER PAGE.
           MOVE " " TO PRINT-REC.
           IF WS-ANSWER1 = "Y"
              MOVE WS-SPLIT-INPUT-DESC TO H2-STOCK
           ELSE
              MOVE ST-STOCKNUMBER      TO H2-STOCK.
           MOVE ST-DESCRIPTION1        TO H2-DESC1
           MOVE ST-DESCRIPTION2        TO H2-DESC2
           WRITE PRINT-REC FROM HEAD2 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC.
           IF WS-ANSWER1 = "Y"
              MOVE "NON STOCK" TO H3-STOCK.
           WRITE PRINT-REC FROM HEAD3 AFTER 1
           MOVE " " TO PRINT-REC
           WRITE PRINT-REC
           MOVE " " TO PRINT-REC
           MOVE 5 TO LINE-CNT.
       PRR-900.
           MOVE WS-ORDERQTY TO T-ORDERQTY
           MOVE WS-SHIPQTY  TO T-SHIPQTY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE STOCK-TRANSLY-FILE
           CLOSE PRINT-FILE.
           
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       READ-DEBTOR SECTION.
       RD-010.
           START DEBTOR-MASTER KEY NOT < DR-KEY.
       RD-015.
           READ DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-015.
       RD-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
           MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY.
       RS-010.
           READ STOCK-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER
                MOVE "UNKNOWN" TO ST-DESCRIPTION1
                GO TO RS-999.
            IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STOCK-ST1
                GO TO RS-010.
       RS-999.
            EXIT.
      *
       START-STOCK SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
              START STOCK-MASTER KEY NOT LESS ST-KEY.
       ST-ST-999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       R-ST-NX-005. 
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       READ-STOCK-PREVIOUS SECTION.
       RPREV-005. 
           READ STOCK-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               PERFORM START-STOCK
               GO TO RPREV-005.
       RPREV-999.
             EXIT.
      *
       READ-ORDER-REGISTER SECTION.
       ROR-000.
            IF STTR-LY-REFERENCE1 = INCR-LY-INVOICE
             IF STTR-LY-TYPE = INCR-LY-TRANS
                 GO TO ROR-999.
           MOVE STTR-LY-TYPE       TO INCR-LY-TRANS.
           MOVE STTR-LY-REFERENCE1 TO INCR-LY-INVOICE.
           START INCR-LY-REGISTER KEY NOT < INCR-LY-KEY.
       ROR-010.
           READ INCR-LY-REGISTER
               INVALID KEY NEXT SENTENCE.
           IF WS-INCR-LY-ST1 = 23 OR 35 OR 49
               MOVE 0 TO INCR-LY-INVOICE
                         INCR-LY-DATE
                         INCR-LY-BO-INV-NO
                         INCR-LY-BO-DATE
               MOVE "*REGISTER NOT FOUND*" TO INCR-LY-PORDER
               GO TO ROR-999.
           IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REGISTERLY RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-LY-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO ROR-010.
       ROR-999.
            EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            IF SP-1STCHAR = "/"
                MOVE STTR-LY-STOCK-NUMBER TO ST-STOCKNUMBER
                MOVE STTR-LY-DESC1        TO ST-DESCRIPTION1
                MOVE STTR-LY-DESC2        TO ST-DESCRIPTION2
                PERFORM GET-010 THRU GET-020.

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE STTR-LY-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO"  TO F-FIELDNAME.
            MOVE 8           TO F-CBFIELDNAME.
            MOVE INCR-LY-PORDER TO F-NAMEFIELD.
            MOVE 14          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDERQTY"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME.
            IF STTR-LY-TYPE = 1
              MOVE STTR-LY-ORDERQTY TO F-EDNAMEFIELDQTY
              MOVE 5             TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-QTY
            ELSE
              MOVE " "           TO F-NAMEFIELD
              MOVE 5             TO F-CBFIELDLENGTH
              PERFORM WRITE-FIELD-ALPHA.

            MOVE "SHIPQTY"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            MOVE 5            TO F-CBFIELDLENGTH.
            IF STTR-LY-TYPE = 1
               MOVE STTR-LY-SHIPQTY TO F-EDNAMEFIELDQTY
               PERFORM WRITE-FIELD-QTY
            ELSE
               MOVE STTR-LY-SHIPQTY TO WS-CREDQTY
               COMPUTE WS-CREDQTY = WS-CREDQTY * -1
               MOVE WS-CREDQTY   TO F-EDNAMEFIELDQTYCRED
               PERFORM WRITE-FIELD-QTYCRED.

            MOVE "PRICE"    TO F-FIELDNAME.
            MOVE 5          TO F-CBFIELDNAME.
            MOVE STTR-LY-PRICE TO F-EDNAMEFIELD99Mil.
            MOVE 11         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR-LY-BO-INV-NO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "ORDDATE"       TO F-FIELDNAME.
            MOVE 7               TO F-CBFIELDNAME.
            MOVE 8               TO F-CBFIELDLENGTH.
            IF STTR-LY-TYPE = 1
               MOVE INCR-LY-BO-DATE    TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE WS-CONVERT-DATE TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE SPACES TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE INCR-LY-INVOICE TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "TOT-ORD"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            IF STTR-LY-TYPE = 1
               ADD STTR-LY-ORDERQTY TO WS-ORDERQTY.
            MOVE WS-ORDERQTY  TO F-EDNAMEFIELDQTY
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY

            MOVE "INVDATE"       TO F-FIELDNAME.
            MOVE 7               TO F-CBFIELDNAME.
            MOVE INCR-LY-DATE       TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE WS-CONVERT-DATE TO F-NAMEFIELD.
            MOVE 8               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TOT-SHP"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME.
            IF STTR-LY-TYPE = 1
               ADD STTR-LY-SHIPQTY        TO WS-SHIPQTY
            ELSE
               SUBTRACT STTR-LY-SHIPQTY FROM WS-SHIPQTY.
            MOVE 6           TO F-CBFIELDLENGTH
            MOVE WS-SHIPQTY  TO F-EDNAMEFIELDNUMNEG
            PERFORM WRITE-FIELD-NUMNEG.

            IF SP-1STCHAR = "/"
             IF F-INDEX < 15
                PERFORM USER-FILL-FIELD.
            IF SP-1STCHAR = "/"
             IF F-EXIT-CH NOT = X"04" AND NOT = X"07"
                MOVE " " TO F-EXIT-CH.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-999.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 7       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PORDERNO" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 14         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDERQTY" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            MOVE " "        TO F-NAMEFIELD.
            MOVE 5          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SHIPQTY" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 5         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRICE" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 11      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 6       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ORDDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 8         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV"  TO F-FIELDNAME.
            MOVE 3      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 6      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "INVDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 8         TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR-MASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-000.
       OPEN-003.
            OPEN I-O INCR-LY-REGISTER.
            IF WS-INCR-LY-ST1 NOT = 0
               MOVE "REGISTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO OPEN-003.
       OPEN-005.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STOCK-ST1
               GO TO OPEN-005.
           GO TO OPEN-009.
       OPEN-006.
           OPEN I-O STOCK-TRANSLY-FILE.
           IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "ST-TRANS BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO OPEN-006.
       OPEN-009.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlInStIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-MASTER
                  INCR-LY-REGISTER
                  STOCK-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteField99Mil".
       Copy "WriteFieldQty".
       Copy "WriteFieldQtyCred".
       Copy "WriteFieldNumNeg".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB.
