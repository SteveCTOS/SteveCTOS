        IDENTIFICATION DIVISION.
        PROGRAM-ID. BmKitsIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       Copy "SelectStMaster".
       Copy "SelectBmMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdToolkit.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "StMastIq".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-PRINT-COSTS       PIC X VALUE " ".
       77  WS-PRINTANSWER       PIC X(20) VALUE " ".
       77  WS-ACCEPT            PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-STOCKNUMBER       PIC X(15) VALUE " ".
       77  WS-TOOLKIT-NUMBER    PIC X(15) VALUE " ".
       77  WS-TOTAL-PRICE       PIC 9(6)V99 VALUE 0.
       77  WS-PRICE             PIC 9(6)V99 VALUE 0.
       77  WS-KIT-PRICE         PIC 9(6)V99 VALUE 0.
       77  WS-TOTAL-COST        PIC 9(6)V99 VALUE 0.
       77  WS-COST              PIC 9(6)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(3)V99 VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-STDESC.
           03  WS-DESC1         PIC X(20) VALUE " ".
           03  WS-DESC2         PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-TOOLKIT-STATUS.
           03  WS-TOOLKIT-ST1   PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(30) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** TOOLKIT ITEM INQUIRY BY KIT **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(10) VALUE "TOOLKIT:".
           03  H3-KIT         PIC X(16).
           03  FILLER         PIC X(13) VALUE "DESCRIPTION:".
           03  H3-DESC1       PIC X(20).
           03  H3-DESC2       PIC X(24).
           03  FILLER         PIC X(12) VALUE "QTY ON HAND:".
           03  H3-QTY         PIC Z(5)9.
           03  FILLER         PIC X(3) VALUE " ".
           03  FILLER         PIC X(14) VALUE "TOOLKIT PRICE:".
           03  H3-PRICE       PIC Z(5)9.99.
           03  FILLER         PIC X(4) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(45) VALUE "DESCRIPTION".
           03  FILLER         PIC X(12) VALUE "QTY P/KIT".
           03  FILLER         PIC X(12) VALUE "QTY ONHAND".
           03  FILLER         PIC X(20) VALUE "ITEM PRICE".
           03  FILLER         PIC X(25) VALUE "COST".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(18).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(24).
           03  D-QTY-PER-KIT  PIC Z(5)9.
           03  FILLER         PIC X(10) VALUE " ".
           03  D-QTY-ONHAND   PIC Z(5)9.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X(5) VALUE " ".
           03  D-COST         PIC Z(5)9.99 BLANK WHEN ZERO.
           03  FILLER         PIC X(21) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(78).
           03  FILLER         PIC X(10) VALUE "* TOTALS *".
           03  T-PRICE        PIC Z(5)9.99.
           03  FILLER         PIC X(5) VALUE " ".
           03  T-COST         PIC Z(5)9.99 BLANK WHEN ZERO.
           03  FILLER         PIC X(5) VALUE " ".
           03  T-MARGIN       PIC Z(5)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(7) VALUE " %".
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
           DISPLAY "** BILL OF MATERIAL ENQUIRY BY KIT NUMBER **" AT POS
           MOVE 0410 TO POS
           DISPLAY "********************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE 2910 TO POS
           DISPLAY "Program now loading, Please be patient.." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0     TO WS-PRICE WS-TOTAL-PRICE WS-TOTAL-COST.
            MOVE "N"   TO WS-ANSWER.
            MOVE "KIT" TO F-FIELDNAME.
            MOVE 3     TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                 MOVE TO-TOOLKIT-NUMBER TO WS-STOCKNUMBER
                 PERFORM START-STOCK-NEXT
                 PERFORM READ-STOCK-NEXT
                 MOVE ST-STOCKNUMBER TO WS-TOOLKIT-NUMBER
                                        TO-TOOLKIT-NUMBER
                 PERFORM GET-010
                 GO TO GET-020.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-STOCK-PREVIOUS
                 MOVE ST-STOCKNUMBER TO WS-TOOLKIT-NUMBER
                                        TO-TOOLKIT-NUMBER
                 PERFORM GET-010
                 GO TO GET-020.
            MOVE 15 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO TO-TOOLKIT-NUMBER
                                WS-TOOLKIT-NUMBER
                                ST-STOCKNUMBER.
            IF TO-TOOLKIT-NUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-005
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            PERFORM READ-STOCK.
            IF ST-DESCRIPTION1 = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            GO TO GET-020.
       GET-010.
            MOVE "KIT"             TO F-FIELDNAME.
            MOVE 3                 TO F-CBFIELDNAME.
            MOVE TO-TOOLKIT-NUMBER TO F-NAMEFIELD.
            MOVE 15                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-015.
            MOVE TO-TOOLKIT-NUMBER TO ST-STOCKNUMBER.
            PERFORM READ-STOCK.
       GET-020.
            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESC"      TO F-FIELDNAME
            MOVE 4           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KIT-QTY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE ST-QTYONHAND TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.

            MOVE "KIT-PRICE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE ST-PRICE TO F-EDNAMEFIELDPRICE.
            MOVE 9 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-PRICE.

            MOVE " " TO F-EXIT-CH.

            PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
               MOVE TO-TOOLKIT-NUMBER TO ST-STOCKNUMBER
               START STOCK-MASTER KEY > ST-STOCKNUMBER
               PERFORM READ-STOCK-NEXT
               MOVE WS-STOCKNUMBER TO TO-TOOLKIT-NUMBER
               CLOSE TOOLKITS
               GO TO GET-999.
            IF F-INDEX < 15
             IF F-EXIT-CH NOT = X"07"
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print The Kit List." AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.

               PERFORM ERROR1-020
               PERFORM ERROR-020.

            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F" AND NOT = X"04"
                MOVE 1 TO F-INDEX
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-900.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                CLOSE TOOLKITS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                OPEN I-O TOOLKITS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE TOOLKITS
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-999.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O TOOLKITS.
           IF WS-TOOLKIT-ST1 NOT = 0
              CLOSE TOOLKITS
              MOVE "TOOLKITS BUSY ON OPEN, GOING TO RETRY IN 1 SECOND"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-020
              CALL "C$SLEEP" USING 1
              GO TO RDTR-000.
           MOVE 1 TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDTR-005.
           MOVE ST-STOCKNUMBER TO TO-TOOLKIT-NUMBER.
           MOVE " "            TO TO-COMPONENT-NUMBER.
           START TOOLKITS KEY NOT < TO-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 23 OR 35 OR 49
              GO TO RDTR-999.
           IF WS-TOOLKIT-ST1 NOT = 0
              CLOSE TOOLKITS
              MOVE "TOOLKITS BUSY ON START, GOING TO RETRY IN 1 SECOND"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR-020
              CALL "C$SLEEP" USING 1
              GO TO RDTR-000.
       RDTR-010.
           READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
           IF WS-TOOLKIT-ST1 = 10
               MOVE 1 TO F-INDEX
               CLOSE TOOLKITS
               GO TO RDTR-999.
           IF WS-TOOLKIT-ST1 NOT = 0
               MOVE "TOOLKIT BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               GO TO RDTR-010.
           IF TO-COMPONENT-NUMBER = " "
               GO TO RDTR-010.
           IF TO-TOOLKIT-NUMBER NOT = WS-TOOLKIT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE TOOLKITS
               GO TO RDTR-999.
           MOVE 2910 TO POS.
           DISPLAY "                                           " AT POS.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2910 TO POS
                DISPLAY "Press 'PgDn' For More," AT POS
                ADD 24 TO POS
                DISPLAY "'ESC' To Clear The Screen !  " AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print Kit List." AT POS
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
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                GO TO RDTR-999.
           IF F-EXIT-CH = X"1F"
                MOVE "Printing In Progress, Please Be Patient."
                   TO WS-MESSAGE
                PERFORM ERROR-000
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C"
                    AND NOT = X"07" AND NOT = " " AND NOT = X"1F"
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           MOVE TO-COMPONENT-NUMBER TO ST-STOCKNUMBER.
           PERFORM READ-STOCK.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
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
               MOVE "STOCK BUSY ON READ - RS-010, 'ESC' TO RETRY."
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
           START STOCK-MASTER KEY NOT < ST-STOCKNUMBER
               INVALID KEY NEXT SENTENCE.
       ST-ST-999.
             EXIT.
      *
       START-STOCK-NEXT SECTION.
       ST-ST-NX000.
           MOVE WS-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-STOCKNUMBER
               INVALID KEY NEXT SENTENCE.
       ST-ST-NX999.
             EXIT.
      *
       READ-STOCK-NEXT SECTION.
       R-ST-NX-000.
             MOVE 0 TO WS-STOCK-ST1.
       R-ST-NX-005. 
             READ STOCK-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-STOCK-ST1 = 10
                MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER
                GO TO R-ST-NX-999.
             IF WS-STOCK-ST1 NOT = 0
             MOVE
               "STOCK BUSY ON READ-NEXT - R-ST-NX-010, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 PERFORM START-STOCK
                 MOVE 0 TO WS-STOCK-ST1
               ELSE 
                 GO TO R-ST-NX-005.
            MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
       R-ST-NX-999.
             EXIT.
      *
       READ-STOCK-PREVIOUS SECTION.
       RDPREV-000.
             MOVE 0 TO WS-STOCK-ST1.
       RDPREV-005. 
             READ STOCK-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
             IF WS-STOCK-ST1 NOT = 0
                 MOVE 0 TO WS-STOCK-ST1
                 PERFORM START-STOCK
                 GO TO RDPREV-005.
            MOVE ST-STOCKNUMBER TO WS-STOCKNUMBER.
       RDPREV-999.
             EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
            PERFORM ERROR-020.
            PERFORM ERROR1-020.
            MOVE 0 TO PAGE-CNT WS-PRICE WS-TOTAL-PRICE
                               WS-COST  WS-TOTAL-COST
            MOVE 66 TO LINE-CNT.
            PERFORM CLEAR-010.
            MOVE "N" TO WS-PRINT-COSTS.
            MOVE 2910 TO POS.
            DISPLAY "PRINT COSTS, Y/N =[ ]" AT POS.
            ADD 19 TO POS.
            ACCEPT WS-PRINT-COSTS AT POS.
            IF WS-PRINT-COSTS NOT = "N" AND NOT = "Y"
                 GO TO PRR-000.
           PERFORM ERROR1-020.
           
           MOVE "Printing In Progress, Please Be Patient." TO WS-MESSAGE
           PERFORM ERROR-000.
       PRR-001.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE WS-TOOLKIT-NUMBER TO TO-TOOLKIT-NUMBER.
            MOVE " "               TO TO-COMPONENT-NUMBER.
            START TOOLKITS KEY NOT < TO-KEY.
       PRR-002.
            READ TOOLKITS NEXT
               AT END NEXT SENTENCE.
            IF WS-TOOLKIT-ST1 = 10
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO PRR-900.
            IF WS-TOOLKIT-ST1 NOT = 0
            MOVE
               "TOOLKIT BUSY ON READ-NEXT - PR-002, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-TOOLKIT-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
               MOVE 0 TO WS-TOOLKIT-ST1
               GO TO PRR-002.
            IF TO-COMPONENT-NUMBER = " "
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER < WS-TOOLKIT-NUMBER
               GO TO PRR-002.
            IF TO-TOOLKIT-NUMBER > WS-TOOLKIT-NUMBER
               GO TO PRR-900.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE TO-COMPONENT-NUMBER    TO ST-STOCKNUMBER
                                           D-STOCK
           PERFORM READ-STOCK.
           MOVE ST-DESCRIPTION1         TO D-DESC1
           MOVE ST-DESCRIPTION2         TO D-DESC2
           MOVE TO-QUANTITY             TO D-QTY-PER-KIT
           MOVE ST-QTYONHAND            TO D-QTY-ONHAND
           MOVE ST-PRICE                TO D-PRICE.
           IF WS-PRINT-COSTS = "Y"
               MOVE ST-AVERAGECOST      TO D-COST
           ELSE
               MOVE " "                 TO D-COST.
           
           COMPUTE WS-PRICE = ST-PRICE * TO-QUANTITY
           ADD WS-PRICE                 TO WS-TOTAL-PRICE.
           
           COMPUTE WS-COST = ST-AVERAGECOST * TO-QUANTITY
           ADD WS-COST                  TO WS-TOTAL-COST.
           
           WRITE PRINT-REC FROM DETAIL-LINE
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
               WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 2
            MOVE " " TO PRINT-REC
            MOVE WS-TOOLKIT-NUMBER   TO H3-KIT
            MOVE WS-TOOLKIT-NUMBER   TO ST-STOCKNUMBER
            PERFORM READ-STOCK
            MOVE ST-DESCRIPTION1     TO H3-DESC1
            MOVE ST-DESCRIPTION2     TO H3-DESC2
            MOVE ST-QTYONHAND        TO H3-QTY
            MOVE ST-PRICE            TO H3-PRICE  WS-KIT-PRICE
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC
            MOVE " " TO PRINT-REC
            MOVE 7 TO LINE-CNT.
       PRR-900.
           MOVE WS-TOTAL-PRICE TO T-PRICE.
           IF WS-PRINT-COSTS = "Y"
               MOVE WS-TOTAL-COST  TO T-COST
               COMPUTE WS-MARGIN = (((WS-KIT-PRICE - WS-TOTAL-COST)
                       / WS-TOTAL-COST) * 100)
               MOVE WS-MARGIN TO T-MARGIN.
           WRITE PRINT-REC FROM TOTAL-LINE.

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
            MOVE "STOCKNUMBER"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE ST-STOCKNUMBER TO F-NAMEFIELD
            MOVE 15             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "ST-DESC"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            MOVE WS-STDESC   TO F-NAMEFIELD
            MOVE 40          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "QTY-PER-KIT" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE TO-QUANTITY   TO F-EDNAMEFIELDKITQTY
            MOVE 3             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-KITQTY

            MOVE "QTY-ON-HAND" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE ST-QTYONHAND  TO F-EDNAMEFIELDINV
            MOVE 6             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV

            MOVE "PRICE"  TO F-FIELDNAME
            MOVE 5        TO F-CBFIELDNAME
            MOVE ST-PRICE TO F-EDNAMEFIELDPRICE
            MOVE 9        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE

            COMPUTE WS-PRICE = ST-PRICE * TO-QUANTITY
            ADD WS-PRICE        TO WS-TOTAL-PRICE
            MOVE "TOT-PRICE"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE WS-TOTAL-PRICE TO F-EDNAMEFIELDPRICE
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE.

            COMPUTE WS-COST = ST-AVERAGECOST * TO-QUANTITY
            ADD WS-COST        TO WS-TOTAL-COST
            MOVE "TOT-COST"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE WS-TOTAL-COST TO F-EDNAMEFIELDPRICE
            MOVE 9             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.
       CLTR-010.
            IF SUB-1 > 15
                GO TO CLTR-900.
            MOVE "STOCKNUMBER" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 15            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ST-DESC"  TO F-FIELDNAME
            MOVE 7          TO F-CBFIELDNAME
            MOVE " "        TO F-NAMEFIELD
            MOVE 40         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "QTY-PER-KIT" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 3             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "QTY-ON-HAND" TO F-FIELDNAME
            MOVE 11            TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 6             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PRICE" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE " "     TO F-NAMEFIELD
            MOVE 9       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            ADD 1 TO SUB-1 F-INDEX.
            GO TO CLTR-010.
       CLTR-900.
            MOVE "TOT-PRICE" TO F-FIELDNAME
            MOVE 9           TO F-CBFIELDNAME
            MOVE " "         TO F-NAMEFIELD
            MOVE 9           TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-005.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCKMASTER BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "BmKitsIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE STOCK-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldInv".
       Copy "WriteFieldKitQty".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPrice".
       Copy "WriteFieldQty".
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
       Copy "PrintReportInfo".
       Copy "GetUserPrintName".
       Copy "SendReportToPrinter".
      *
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
      * END-OF-JOB.
