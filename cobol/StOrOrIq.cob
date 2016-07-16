        IDENTIFICATION DIVISION.
        PROGRAM-ID. StOrOrIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStOrders".
          Copy "SelectSlParameter".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdOutOrd.
           COPY ChlfdParam.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-DELIVERVIA        PIC X(20) VALUE " ".
       77  WS-DEL-SUB           PIC 9 VALUE 0.    
       77  WS-ORDER-NUMBER      PIC X(20) VALUE " ".
       77  WS-NEXT-ORDER        PIC X(20) VALUE " ".
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  TOTAL-CNT            PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-WRITE             PIC X VALUE " ".
       01  WS-OUTORD-STATUS.
           03  WS-OUTORD-ST1      PIC 99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1       PIC 99.
       01  WS-SLPARAMETER-STATUS.
           03  WS-SLPARAMETER-ST1 PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR         PIC X.
           03  SP-REST            PIC X(14).
       01  STORE-DEL.
         02  WS-DEL-OCCUR OCCURS 10.
           03  WS-DEL-TYPE        PIC X.
           03  WS-DEL-CODE        PIC X.
           03  WS-DEL-TERM        PIC X(20).
       01  HEAD1.
           03  FILLER         PIC X(5) VALUE "DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** ORDERS ON SUPPLIERS INQUIRY BY ORDER **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(9) VALUE "ORDER #:".
           03  H2-ORDER       PIC X(40) VALUE " ".
           03  FILLER         PIC X(17) VALUE "ORDER CONFIRMED:".
           03  D-CONFIRMED    PIC X(66).
       01  HEAD3.
           03  FILLER         PIC X(17) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(42) VALUE "DESCRIPTION".
           03  FILLER         PIC X(24) VALUE "DELIVER VIA".
           03  FILLER         PIC X(8) VALUE "QTY".
           03  FILLER         PIC X(26) VALUE "ORD DATE     DUE DATE".
           03  FILLER         PIC X(15) VALUE "SUPPLIER".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(17).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(22).
           03  D-DELIVER      PIC X(22).
           03  D-ORDERQTY     PIC Z(4)9.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-ORDDATE      PIC X(10).
           03  FILLER         PIC X(3) VALUE " ".
           03  D-DUEDATE      PIC X(10).
           03  FILLER         PIC X(4) VALUE " ".
           03  D-SUPPLIER     PIC X(15).
       01  TOTAL-LINE.
           03  FILLER         PIC X(16) VALUE "ITEMS ON ORDER#:".
           03  T-TOTALQTY     PIC Z(4)9.
           03  FILLER         PIC X(111) VALUE " ".
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
           DISPLAY "** STOCK ORDER INQUIRY BY ORDER NUMBER ***" AT POS
           MOVE 0410 TO POS
           DISPLAY "******************************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-015.
           MOVE 2510 TO POS
           DISPLAY "Program now loading, Please be patient.." AT POS.
           PERFORM OPEN-FILES.
           PERFORM READ-DELIVERY-FILE.
           PERFORM CLEAR-SCREEN.
       CONTROL-040.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-040.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "N" TO WS-ANSWER.

            MOVE 2905 TO POS
            DISPLAY 
            "Press 'PgDn' For Next P/O#, 'PgUp' For Previous P/O#,"
             AT POS
            MOVE 3005 TO POS
            DISPLAY " Or Enter Purchase Order Number." AT POS

            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-NEXT-ORDER TO WS-ORDER-NUMBER
                 PERFORM START-ORDER
             IF WS-OUTORD-ST1 = 88
                 MOVE "STARTING P-ORDER IS TOO HIGH, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-000
             ELSE
                 PERFORM READ-ORDER-NEXT
                 GO TO GET-010.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-ORDER-PREVIOUS
                 GO TO GET-010.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-ORDER-NUMBER.
            IF WS-ORDER-NUMBER = 0 OR = "   "
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM DISPLAY-FORM
                GO TO GET-000.
           PERFORM START-ORDER.
           IF WS-OUTORD-ST1 = 88
                MOVE "STARTING P-ORDER IS TOO HIGH, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO GET-000.
           PERFORM READ-ORDER-NEXT.
           IF OO-ORDER-NUMBER NOT = WS-ORDER-NUMBER
                MOVE OO-ORDER-NUMBER TO WS-NEXT-ORDER
                MOVE 2910 TO POS
                DISPLAY
           "Invalid ORDER-NUMBER Entered, 'ESC to Clear screen OR"
                AT POS
                MOVE 3010 TO POS
                DISPLAY
                "'PgDn' to see the NEXT ORDER OR 'PgUp' for Previous."
                AT POS
                GO TO GET-000.
       GET-010.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.

            MOVE "ORDER" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE OO-ORDER-NUMBER TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE OO-ORDERDATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE OO-DELIVERY-METHOD TO WS-DEL-SUB.
            IF WS-DEL-SUB = 0
                MOVE 1 TO WS-DEL-SUB.
            MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
            MOVE WS-DELIVERVIA TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLIER" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE OO-SUPPLIER-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONFIRMED" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            IF OO-UPDATED = "Y"
                MOVE "YES" TO F-NAMEFIELD
            ELSE
                MOVE "NO " TO F-NAMEFIELD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-020.
            MOVE " " TO F-EXIT-CH.

            PERFORM READ-TRANSACTIONS.
       GET-900.
            PERFORM ERROR1-020
            PERFORM ERROR-020.

            IF WS-ANSWER = "Y"
                GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = X"07"
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Supplier Orders"
                  AT POS
               ADD 44 TO POS
               DISPLAY "For This Stock Number." AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F" AND NOT = X"04"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                OPEN I-O OUTSTANDING-ORDERS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-999.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-005.
           MOVE 1   TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
           MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER.
           MOVE " "             TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-OUTORD-ST1 = 23 OR 35 OR 49
                GO TO RDTR-999.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ST-ORDER FILE BUSY ON START, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
               GO TO RDTR-005.
           MOVE " " TO F-EXIT-CH.
       RDTR-010.
           IF F-EXIT-CH = " "
             READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
             READ OUTSTANDING-ORDERS PREVIOUS
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = " "
            IF WS-OUTORD-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF WS-OUTORD-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDTR-005.
           IF WS-OUTORD-ST1 NOT = 0
               MOVE "ST-ORDER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-OUTORD-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-OUTORD-ST1
              GO TO RDTR-010.
           IF OO-QUANTITY NOT > 0
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction.." AT POS
               GO TO RDTR-010.
           IF OO-STOCK-NUMBER = " "
               GO TO RDTR-010.
           IF OO-ORDER-NUMBER NOT = WS-ORDER-NUMBER
               MOVE OO-ORDER-NUMBER TO WS-NEXT-ORDER
               MOVE 1 TO F-INDEX
               GO TO RDTR-999.
           PERFORM ERROR-020.
       RDTR-020.
           IF F-INDEX > 15
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' For Previous,"
                 AT POS
                ADD 44 TO POS
                DISPLAY "OR 'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Supplier Orders"
                   AT POS
                ADD 44 TO POS
                DISPLAY "For This Stock Number." AT POS
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
                PERFORM READ-UNTIL-NEXT-ORDER
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
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " "   AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           PERFORM READ-STOCK
           PERFORM SCROLLING
           ADD 1 TO F-INDEX
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       RS-000.
            MOVE OO-STOCK-NUMBER TO ST-STOCKNUMBER.
            START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY NEXT SENTENCE.
       RS-010.
            READ STOCK-MASTER
                INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "UNKNOWN ITEM       " TO ST-DESCRIPTION1
                MOVE "                   " TO ST-DESCRIPTION2
                GO TO RS-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
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
       START-ORDER SECTION.
       ST-ST-000.
           MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER.
           MOVE " "             TO OO-STOCK-NUMBER.
           START OUTSTANDING-ORDERS KEY NOT < OO-KEY
                 INVALID KEY NEXT SENTENCE.
          IF WS-OUTORD-ST1 NOT = 0
               MOVE 88 TO WS-OUTORD-ST1.
       ST-ST-999.
             EXIT.
      *
       READ-ORDER-NEXT SECTION.
       R-ST-NX-005. 
             IF WS-OUTORD-ST1 NOT = 0
                 GO TO R-ST-NX-999.
             READ OUTSTANDING-ORDERS NEXT
                 AT END NEXT SENTENCE.
             IF OO-STOCK-NUMBER = " "
                 GO TO R-ST-NX-005.
             IF OO-ORDER-NUMBER = " "
                 GO TO R-ST-NX-005.
             IF WS-OUTORD-ST1 = 10
              MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM START-ORDER
                 GO TO R-ST-NX-999.
             IF WS-OUTORD-ST1 = 0
                 GO TO R-ST-NX-999.
             PERFORM START-ORDER.
             IF WS-OUTORD-ST1 = 88
                 GO TO R-ST-NX-999.

            MOVE "ST-ORDERS BUSY ON READ-NEXT, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-OUTORD-ST1
                 GO TO R-ST-NX-005.
       R-ST-NX-999.
             EXIT.
      *
       READ-ORDER-PREVIOUS SECTION.
       RPREV-005. 
             IF WS-OUTORD-ST1 NOT = 0
                 GO TO RPREV-999.
             READ OUTSTANDING-ORDERS PREVIOUS
                 AT END NEXT SENTENCE.
             IF OO-STOCK-NUMBER = " "
                 GO TO RPREV-005.
             IF OO-ORDER-NUMBER = " "
                 GO TO RPREV-005.
             IF WS-OUTORD-ST1 = 10
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM START-ORDER
                 GO TO RPREV-999.
             IF WS-OUTORD-ST1 = 0
                 GO TO RPREV-999.
             PERFORM START-ORDER.
             IF WS-OUTORD-ST1 = 88
                 GO TO RPREV-999.
            MOVE "ST-ORDERS BUSY ON READ-PREV, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-OUTORD-ST1
                 GO TO RPREV-005.
       RPREV-999.
             EXIT.
      *
       READ-UNTIL-NEXT-ORDER SECTION.
       RU-NEXT-001. 
             MOVE 3010 TO POS
             DISPLAY "Reading to end of OrderNumber......" AT POS.
       RU-NEXT-005. 
             READ OUTSTANDING-ORDERS NEXT
                 AT END NEXT SENTENCE.
             IF WS-OUTORD-ST1 NOT = 0
                 GO TO RU-NEXT-900.
             IF OO-ORDER-NUMBER = WS-ORDER-NUMBER
                 GO TO RU-NEXT-005.
       RU-NEXT-900.
             MOVE OO-ORDER-NUMBER TO WS-NEXT-ORDER.
             PERFORM ERROR-020.
       RU-NEXT-999.
             EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            MOVE 0  TO PAGE-CNT TOTAL-CNT.
            MOVE 66 TO LINE-CNT.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
       PRR-001.
            PERFORM ERROR1-020.
            MOVE WS-ORDER-NUMBER TO OO-ORDER-NUMBER.
            MOVE " "             TO OO-STOCK-NUMBER.
            START OUTSTANDING-ORDERS KEY NOT < OO-KEY
               INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ OUTSTANDING-ORDERS NEXT
               AT END NEXT SENTENCE.
            IF WS-OUTORD-ST1 = 10
               MOVE 0 TO WS-OUTORD-ST1
               GO TO PRR-900.
            IF WS-OUTORD-ST1 NOT = 0
            MOVE "ST-ORDERS BUSY ON READ-NEXT-PRN, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-OUTORD-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-OUTORD-ST1
                 GO TO PRR-002.
           IF OO-QUANTITY NOT > 0
               GO TO PRR-002.
            IF OO-ORDER-NUMBER < WS-ORDER-NUMBER
               GO TO PRR-002.
            IF OO-ORDER-NUMBER > WS-ORDER-NUMBER
               MOVE OO-ORDER-NUMBER TO WS-NEXT-ORDER
               GO TO PRR-900.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           PERFORM READ-STOCK.
           MOVE ST-STOCKNUMBER           TO D-STOCK.
           MOVE ST-DESCRIPTION1          TO D-DESC1.
           MOVE ST-DESCRIPTION2          TO D-DESC2.
           MOVE OO-QUANTITY              TO D-ORDERQTY.
           MOVE OO-ORDERDATE             TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE             TO D-ORDDATE.
           IF OO-DUEDATE > 0
              MOVE OO-DUEDATE            TO SPLIT-DATE
              PERFORM CONVERT-DATE-FORMAT
              MOVE DISPLAY-DATE          TO D-DUEDATE
           ELSE
              MOVE " "                   TO D-DUEDATE.
           MOVE OO-SUPPLIER-NUMBER       TO D-SUPPLIER.
           MOVE OO-DELIVERY-METHOD       TO WS-DEL-SUB.
           MOVE WS-DEL-TERM (WS-DEL-SUB) TO WS-DELIVERVIA.
           MOVE WS-DELIVERVIA            TO D-DELIVER.
           WRITE PRINT-REC FROM DETAIL-LINE.
           MOVE " " TO PRINT-REC.
           ADD 1 TO LINE-CNT TOTAL-CNT.
           GO TO PRR-002.
       PRR-060.
            ADD 1         TO PAGE-CNT.
            MOVE PAGE-CNT TO H1-PAGE.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
               WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC FROM HEAD1 AFTER 1.
            MOVE " " TO PRINT-REC.
            MOVE OO-ORDER-NUMBER TO H2-ORDER.
           IF OO-UPDATED = "Y"
               MOVE "YES" TO D-CONFIRMED
           ELSE
               MOVE "NO " TO D-CONFIRMED.
            WRITE PRINT-REC FROM HEAD2 AFTER 1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            WRITE PRINT-REC FROM HEAD3 AFTER 1.
            MOVE " " TO PRINT-REC.
            WRITE PRINT-REC.
            MOVE " " TO PRINT-REC.
            MOVE 5 TO LINE-CNT.
       PRR-900.
            MOVE " " TO PRINT-REC.
            MOVE TOTAL-CNT TO T-TOTALQTY.
            WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.
            MOVE " " TO PRINT-REC.
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
            MOVE "DUEDATE"    TO F-FIELDNAME
            MOVE 7            TO F-CBFIELDNAME.
            IF OO-DUEDATE > 0
               MOVE OO-DUEDATE   TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE DISPLAY-DATE TO F-NAMEFIELD
            ELSE
               MOVE " "          TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE OO-QUANTITY TO F-EDNAMEFIELDQTY.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-QTY.

            MOVE "STOCKNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION1 TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC2" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE ST-DESCRIPTION2 TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-999.
            MOVE "DUEDATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            MOVE " "       TO F-NAMEFIELD.
            MOVE 10        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 5 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "STOCKNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC2" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE " " TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       READ-DELIVERY-FILE SECTION.
       RDELIV-000.
            MOVE 1 TO SUB-1
                      PA-RECORD.
            MOVE 3 TO PA-TYPE.
       RDELIV-010.
            READ PARAMETER-FILE NEXT
                AT END NEXT SENTENCE.
            IF WS-SLPARAMETER-ST1 = 10
                GO TO RDELIV-900.
            IF PA-TYPE < 3
               GO TO RDELIV-010.
            IF PA-TYPE > 3
                GO TO RDELIV-900.
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE "PARAMETER FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-SLPARAMETER-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-SLPARAMETER-ST1
               GO TO RDELIV-010.
            IF PARAMETER-REC = "           "
               GO TO RDELIV-010.           
            MOVE PARAMETER-REC TO WS-DEL-OCCUR (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 = 10
               PERFORM ERROR-020
               GO TO RDELIV-900.
            GO TO RDELIV-010.
       RDELIV-900. 
            CLOSE PARAMETER-FILE.
       RDELIV-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK WS-ST1 NOT = 0 ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-010.
           OPEN I-O PARAMETER-FILE
            IF WS-SLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-SLPARAMETER-ST1
               MOVE "PARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-010.
           MOVE Ws-Co-Name TO CO-NAME.
       OPEN-020.
           OPEN I-O OUTSTANDING-ORDERS.
           IF WS-OUTORD-ST1 NOT = 0
              MOVE 0 TO WS-OUTORD-ST1
              MOVE "ORDERS FILE BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-020.
       OPEN-106.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-110.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StOrOrIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER
                  OUTSTANDING-ORDERS.
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
       Copy "WriteFieldQty".
       Copy "WriteFieldDate".
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
      * END-OF-JOB.
