        IDENTIFICATION DIVISION.
        PROGRAM-ID. StBoStIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectDrMaster".
          Copy "SelectStMaster".
          Copy "SelectSlRegister".
          Copy "SelectStTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdStock.
           COPY ChlfdRegister.
           COPY ChlfdStTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(255).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NONSTOCK          PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-ORDERQTY          PIC 9(5) VALUE 0.
       77  WS-SHIPQTY           PIC 9(5) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-STDESC.
           03  WS-DESC1          PIC X(20) VALUE " ".
           03  WS-DESC2          PIC X(20) VALUE " ".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1      PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STTRANS-STATUS.
           03  WS-STTRANS-ST1   PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** BACKORDERS INQUIRY BY STOCK **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(10) VALUE "STOCK :".
           03  H3-STOCK       PIC X(20).
           03  FILLER         PIC X(13) VALUE "DESCRIPTION:".
           03  H3-DESC1       PIC X(20).
           03  H3-DESC2       PIC X(58).
       01  HEAD3.
           03  FILLER         PIC X(8) VALUE "ACCOUNT".
           03  FILLER         PIC X(40) VALUE "NAME ".
           03  FILLER         PIC X(19) VALUE "ORDER READY SHPD".
           03  FILLER         PIC X(36) VALUE 
           "PRICE     COST P/ORDER NUMBER".
           03  FILLER         PIC X(14) VALUE "ORDER    INV".
           03  FILLER         PIC X(18) VALUE " DATE       DATE".
           03  FILLER         PIC X(10) VALUE "   TRANS".
       01  DETAIL-LINE.
           03  D-ACCOUNT      PIC X(8).
           03  D-NAME         PIC X(40).
           03  D-ORDERQTY     PIC Z(4)9.
           03  D-SHIPQTY      PIC Z(4)9.
           03  D-SHIPPEDQTY   PIC Z(4)9.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-COST         PIC Z(5)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PONO         PIC X(20).
      *     03  D-PONO         PIC X(21).
           03  D-1STINV       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-2NDINV       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-1STDATE      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-2NDDATE      PIC X(10).
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-TRANS        PIC Z(5)9.
       01  TOTAL-LINE.
           03  FILLER         PIC X(26).
           03  T-NAME         PIC X(22) VALUE "TOTALS FOR STOCK ITEM:".
           03  T-ORDERQTY     PIC Z(4)9.
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
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 0310 TO POS
           DISPLAY "**BACK-ORDER BY STOCK NUMBER INQUIRY ***" AT POS
           MOVE 0410 TO POS
           DISPLAY "****************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO WS-ANSWER WS-NONSTOCK.
            MOVE 0 TO WS-ORDERQTY
                      WS-SHIPQTY.
            MOVE "STOCK" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
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
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF SP-1STCHAR = "/"
               MOVE "Y" TO WS-NONSTOCK.
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
            
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE "ONHAND"     TO F-FIELDNAME
            MOVE 6            TO F-CBFIELDNAME
            MOVE ST-QTYONHAND TO F-EDNAMEFIELDINV
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.
            
            MOVE "RESERVE"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE ST-QTYONRESERVE TO F-EDNAMEFIELDINV
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.
       GET-030.
            IF ST-DESCRIPTION1 = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            MOVE " " TO F-EXIT-CH.

            PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
                CLOSE STOCK-TRANS-FILE
                GO TO GET-999.
            IF F-INDEX < 15
             IF F-EXIT-CH NOT = X"07"
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Back Orders"
                  AT POS
               ADD 40 TO POS
               DISPLAY "For This Stock Number." AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F" AND NOT = X"04"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR-020
            PERFORM ERROR1-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                OPEN I-O STOCK-TRANS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE STOCK-TRANS-FILE
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O STOCK-TRANS-FILE.
           IF WS-STTRANS-ST1 NOT = 0
              CLOSE STOCK-TRANS-FILE
              GO TO RDTR-000.
       RDTR-001.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDTR-005.
           MOVE "N"            TO STTR-ST-COMPLETE
           MOVE ST-STOCKNUMBER TO STTR-STOCK-NUMBER
           MOVE 0              TO STTR-ST-DATE.
           START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANS-ST1 NOT = 0
              CLOSE STOCK-TRANS-FILE
              PERFORM RDTR-999.
           MOVE " " TO F-EXIT-CH.
       RDTR-010.
           IF F-EXIT-CH = " "
             READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
             READ STOCK-TRANS-FILE PREVIOUS
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = " "
           IF WS-STTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
           IF WS-STTRANS-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-000.
           IF WS-STTRANS-ST1 NOT = 0
               MOVE 2910 TO POS
               MOVE 
            "ST-TRANS BUSY ON READ NEXT, SEE NEXT LINE, <ESC> TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RDTR-010.
            IF STTR-ST-COMPLETE NOT = "N"
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
           IF WS-NONSTOCK = "Y"
            IF SP-1STCHAR NOT = "/"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           IF WS-NONSTOCK = "N"
            IF STTR-STOCK-NUMBER NOT = ST-STOCKNUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           IF STTR-TYPE NOT = 4 AND NOT = 7
               MOVE 2910 TO POS
               MOVE "Reading Next Valid Transaction..." TO WS-MESSAGE
               DISPLAY WS-MESSAGE AT POS
               GO TO RDTR-010.
           IF STTR-COMPLETE = "Y" OR = "L"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANS-FILE
               GO TO RDTR-999.
           IF WS-MESSAGE NOT = " "
               PERFORM ERROR1-020.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2905 TO POS
              DISPLAY "Press 'Pgdn' For More, 'PgUp' for Previous,"
                 AT POS
                ADD 44 TO POS
                DISPLAY "OR 'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Back Orders"
                   AT POS
                ADD 40 TO POS
                DISPLAY "For This Stock Number." AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020
            PERFORM ERROR1-020.
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
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR-020
                PERFORM ERROR1-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           PERFORM READ-ORDER-REGISTER
           PERFORM SCROLLING
           ADD 1 TO F-INDEX
           GO TO RDTR-010.
       RDTR-999.
            MOVE "STOCK"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD
            MOVE 15             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       RDTR-9999.
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
               MOVE 
                  "STOCK BUSY ON READ, SEE NEXT LINE, <ESC> TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO RS-010.
       RS-999.
            EXIT.
      *
       START-STOCK SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
              START STOCK-MASTER KEY NOT LESS ST-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       R-ST-NX-000.
             MOVE 0 TO WS-STOCK-ST1.
       R-ST-NX-005. 
             READ STOCK-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-STOCK-ST1 = 10
              MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM START-STOCK
                 GO TO R-ST-NX-999.
             IF WS-STOCK-ST1 = 0
                 GO TO R-ST-NX-999
             ELSE
               MOVE "STOCK BUSY READ NEXT, NEXT LINE, <ESC> TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 PERFORM START-STOCK
                 GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       READ-STOCK-PREVIOUS SECTION.
       RPREV-000.
             MOVE 0 TO WS-STOCK-ST1.
       RPREV-005. 
             READ STOCK-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
             IF WS-STOCK-ST1 = 10
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM START-STOCK
                 GO TO RPREV-999.
             IF WS-STOCK-ST1 = 0
                 GO TO RPREV-999
             ELSE
                 MOVE 0 TO WS-STOCK-ST1
                 PERFORM START-STOCK
                 GO TO RPREV-005.
       RPREV-999.
             EXIT.
      *
       READ-ORDER-REGISTER SECTION.
       ROR-000.
            IF STTR-REFERENCE1 = INCR-INVOICE
             IF STTR-TYPE = INCR-TRANS
                 GO TO ROR-999.
            MOVE STTR-REFERENCE1 TO INCR-INVOICE
            MOVE STTR-TYPE       TO INCR-TRANS.
            START INCR-REGISTER KEY NOT < INCR-KEY.
       ROR-010.
            READ INCR-REGISTER
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO INCR-INVOICE
                          INCR-DATE
                          INCR-BO-INV-NO
                          INCR-BO-DATE
                MOVE "*REGISTER NOT FOUND*" TO INCR-PORDER
                GO TO ROR-999.
            IF WS-INCR-ST1 NOT = 0
                MOVE 0 TO WS-INCR-ST1
                GO TO ROR-010.
       ROR-999.
            EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-0000.
           PERFORM ERROR-020
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY "Printing In Progress, Please Be Patient." AT POS
           MOVE 0 TO PAGE-CNT
                     WS-ORDERQTY
                     WS-SHIPQTY.
           MOVE 66 TO LINE-CNT.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE
           MOVE "N"            TO STTR-ST-COMPLETE
           MOVE ST-STOCKNUMBER TO STTR-STOCK-NUMBER.
           START STOCK-TRANS-FILE KEY NOT < STTR-ST-KEY
                INVALID KEY NEXT SENTENCE.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-900.
       PRR-002.
            READ STOCK-TRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANS-ST1 = 10
               MOVE 0 TO WS-STTRANS-ST1
               GO TO PRR-900.
            IF WS-STTRANS-ST1 NOT = 0
               MOVE 
            "ST-TRANS PRINT BUSY READ NEXT, NEXT LINE, <ESC> TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANS-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO PRR-002.
           IF STTR-ST-COMPLETE NOT = "N"
               GO TO PRR-900.
           IF STTR-TYPE NOT = 4 AND NOT = 7
               GO TO PRR-002.
            IF STTR-ST-COMPLETE NOT = "N"
               GO TO PRR-900.
           MOVE STTR-STOCK-NUMBER TO SPLIT-STOCK.
           IF STTR-STOCK-NUMBER < ST-STOCKNUMBER
               GO TO PRR-002.
           IF WS-NONSTOCK = "N"
             IF STTR-STOCK-NUMBER > ST-STOCKNUMBER
               GO TO PRR-900.
           IF WS-NONSTOCK = "Y"
            IF SP-1STCHAR NOT = "/"
               GO TO PRR-900.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           IF STTR-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE STTR-ACCOUNT-NUMBER TO D-ACCOUNT
                                          DR-ACCOUNT-NUMBER
               START DEBTOR-MASTER KEY NOT < DR-KEY
           READ DEBTOR-MASTER
                INVALID KEY
               MOVE "UNKNOWN"           TO D-NAME.
           IF STTR-REFERENCE1 NOT = INCR-INVOICE
              PERFORM READ-ORDER-REGISTER.
           MOVE DR-NAME                 TO D-NAME
           MOVE INCR-PORDER             TO D-PONO
           MOVE STTR-ORDERQTY           TO D-ORDERQTY
           MOVE STTR-SHIPQTY            TO D-SHIPQTY
           MOVE STTR-SHIPPEDQTY         TO D-SHIPPEDQTY
           MOVE STTR-PRICE              TO D-PRICE
           MOVE STTR-COST-VALUE         TO D-COST
           MOVE STTR-REFERENCE1         TO D-1STINV
           MOVE STTR-DATE               TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-1STDATE
           MOVE STTR-INV-NO             TO D-2NDINV
           MOVE INCR-BO-DATE            TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-2NDDATE
           MOVE STTR-TRANSACTION-NUMBER TO D-TRANS
           WRITE PRINT-REC FROM DETAIL-LINE
           COMPUTE STTR-ORDERQTY = STTR-ORDERQTY - STTR-SHIPPEDQTY
           ADD STTR-ORDERQTY            TO WS-ORDERQTY
           ADD STTR-SHIPQTY             TO WS-SHIPQTY

           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-060.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC BEFORE PAGE
                WRITE PRINT-REC FROM COMPANY-LINE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            MOVE ST-STOCKNUMBER  TO H3-STOCK
            MOVE ST-DESCRIPTION1 TO H3-DESC1
            MOVE ST-DESCRIPTION2 TO H3-DESC2
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 6 TO LINE-CNT.
       PRR-900.
           MOVE WS-ORDERQTY TO T-ORDERQTY
           MOVE WS-SHIPQTY  TO T-SHIPQTY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            IF SP-1STCHAR = "/"
                MOVE STTR-STOCK-NUMBER TO ST-STOCKNUMBER
                MOVE STTR-DESC1        TO ST-DESCRIPTION1
                MOVE STTR-DESC2        TO ST-DESCRIPTION2
                PERFORM GET-010 THRU GET-020.

            MOVE "ACCNO"             TO F-FIELDNAME
            MOVE 5                   TO F-CBFIELDNAME
            MOVE STTR-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PORDERNO"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE INCR-PORDER TO F-NAMEFIELD
            MOVE 14          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ORDERQTY"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            COMPUTE STTR-ORDERQTY = STTR-ORDERQTY - STTR-SHIPPEDQTY
            MOVE STTR-ORDERQTY TO F-EDNAMEFIELDQTY
            MOVE 5             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY

            MOVE "SHIPQTY"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            MOVE STTR-SHIPQTY TO F-EDNAMEFIELDQTY
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY

            MOVE "PULLED"    TO F-FIELDNAME
            MOVE 6           TO F-CBFIELDNAME
            IF INCR-PULL-DATE = 0
                MOVE " "   TO F-NAMEFIELD
            ELSE
                MOVE "P"   TO F-NAMEFIELD.
            MOVE 1         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PRICE"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE STTR-PRICE TO F-EDNAMEFIELD99Mil
            MOVE 11         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99mil

            MOVE "ORDER"      TO F-FIELDNAME
            MOVE 5            TO F-CBFIELDNAME
            MOVE INCR-INVOICE TO F-EDNAMEFIELDNUM
            MOVE 6            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "ORDDATE"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE INCR-DATE       TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE WS-CONVERT-DATE TO F-NAMEFIELD
            MOVE 8               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "INV"          TO F-FIELDNAME
            MOVE 3              TO F-CBFIELDNAME
            MOVE INCR-BO-INV-NO TO F-EDNAMEFIELDNUM
            MOVE 6              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "TOT-ORD"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME
            ADD STTR-ORDERQTY TO WS-ORDERQTY
            MOVE WS-ORDERQTY  TO F-EDNAMEFIELDQTY
            MOVE 5            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY

            MOVE "TOT-SHP"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            ADD STTR-SHIPQTY TO WS-SHIPQTY
            MOVE WS-SHIPQTY  TO F-EDNAMEFIELDQTY
            MOVE 5           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-QTY

            MOVE "INVDATE"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE INCR-BO-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE WS-CONVERT-DATE TO F-NAMEFIELD
            MOVE 8               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

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
                GO TO CLTR-900.
            MOVE "ACCNO" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 7       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PORDERNO" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 14         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ORDERQTY" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 5          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "SHIPQTY" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PULLED"    TO F-FIELDNAME
            MOVE 6           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 1         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PRICE" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 11      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ORDER" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 6       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ORDDATE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 8         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "INV" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE " "   TO F-NAMEFIELD
            MOVE 6     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "INVDATE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 8         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            ADD 1 TO SUB-1 F-INDEX
            GO TO CLTR-010.
       CLTR-900.
            MOVE "TOT-ORD" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "TOT-SHP"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE " "       TO F-NAMEFIELD
            MOVE 5         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTORS BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-003.
            OPEN I-O INCR-REGISTER.
            IF WS-INCR-ST1 NOT = 0
               MOVE 0 TO WS-INCR-ST1
               MOVE "REGISTER FILE BUSY ON OPEN 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-003.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK-MASTER FILE BUSY ON OPEN 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
           MOVE Ws-Co-Name to CO-NAME.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StBoStIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-MASTER
                  INCR-REGISTER
                  STOCK-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldNumeric".
       Copy "WriteField99Mil".
       Copy "WriteFieldQty".
       Copy "WriteFieldDate".
       Copy "WriteFieldInv".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB.
