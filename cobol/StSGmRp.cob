        IDENTIFICATION DIVISION.
        PROGRAM-ID. StSGmRp.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectStMaster".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-PRINTANSWER       PIC X(10) VALUE " ".
       77  LINE-CNT             PIC 9(3) VALUE 66.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  WS-ANSWER1           PIC X(15) VALUE " ".
       77  WS-ANSWER2           PIC X(15) VALUE " ".
       77  WS-ANSWER3           PIC X VALUE " ".
       77  WS-STORE             PIC X(3) VALUE " ".
       77  WS-SALESQTY          PIC S9(6) VALUE 0.
       77  WS-SALESAMT          PIC S9(7)V99 VALUE 0.
       77  WS-COST              PIC S9(7)V99 VALUE 0.
       77  WS-MARGIN            PIC S9(7)V99 VALUE 0.
       77  WS-PERC              PIC S9(4)V99.
       77  WS-SALESQTY-YTD      PIC S9(6) VALUE 0.
       77  WS-SALESAMT-YTD      PIC S9(8)V99 VALUE 0.
       77  WS-COST-YTD          PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-YTD        PIC S9(8)V99 VALUE 0.
       77  WS-PERC-YTD          PIC S9(4)V99.
       77  WS-SALES-DIS             PIC Z(7)9.99-.
       77  TOT-SALESQTY         PIC S9(6) VALUE 0.
       77  TOT-SALESAMT         PIC S9(8)V99 VALUE 0.
       77  TOT-COST             PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN           PIC S9(8)V99 VALUE 0.
       77  WS-SALESQTY-LAST     PIC S9(6) VALUE 0.
       77  WS-SALESAMT-LAST     PIC S9(8)V99 VALUE 0.
       77  WS-COST-LAST         PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN-LAST       PIC S9(8)V99 VALUE 0.
       77  WS-PERC-LAST         PIC S9(4)V99.
       77  TOT-SALESQTY-LAST    PIC S9(6) VALUE 0.
       77  TOT-SALESAMT-LAST    PIC S9(8)V99 VALUE 0.
       77  TOT-COST-LAST        PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN-LAST      PIC S9(8)V99 VALUE 0.
       77  TOT-PERC-LAST        PIC S9(4)V99.
       77  TOT-PERC             PIC S9(4)V99.
       77  TOT-SALESQTY-YTD     PIC S9(6) VALUE 0.
       77  TOT-SALESAMT-YTD     PIC S9(8)V99 VALUE 0.
       77  TOT-COST-YTD         PIC S9(8)V99 VALUE 0.
       77  TOT-MARGIN-YTD       PIC S9(8)V99 VALUE 0.
       77  TOT-PERC-YTD         PIC S9(4)V99.
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
      *     03  WS-STOCK-ST2     PIC X.
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(12) VALUE " ".
           03  FILLER         PIC X(15) VALUE "S A L E S   A N".
           03  FILLER         PIC X(17) VALUE " D   G R O S S   ".
           03  FILLER         PIC X(14) VALUE "M A R G I N   ".
           03  FILLER         PIC X(41) VALUE "A N A L Y S I S".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(8) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(29) VALUE " ".
           03  FILLER         PIC X(61) VALUE ALL "*".
           03  FILLER         PIC X(42) VALUE " ".
       01  HEAD3.
           03  FILLER         PIC X(89) VALUE " ".
           03  FILLER         PIC X(14) VALUE "PERIOD / YEAR ".
           03  FILLER         PIC X(27) VALUE "/ LAST YEAR TO DATE".
       01  HEAD4.
           03  FILLER         PIC X(18) VALUE "STOCK NUMBER".
           03  FILLER         PIC X(50) VALUE "DESCRIPTION".
           03  FILLER         PIC X(17) VALUE "SALES QTY".
           03  FILLER         PIC X(13) VALUE "SALES".
           03  FILLER         PIC X(11) VALUE "COSTS".
           03  FILLER         PIC X(12) VALUE "MARGIN".
           03  FILLER         PIC X(11) VALUE "%".
       01  DETAIL-LINE.
           03  D-STOCK        PIC X(18) VALUE " ".
           03  D-DESC1        PIC X(20) VALUE " ".
           03  D-DESC2        PIC X(30) VALUE " ".
           03  D-SALESQTY     PIC Z(5)9-.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-SALESAMT     PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-COSTSAMT     PIC Z(7)9.99-.
      *     03  FILLER         PIC X(1) VALUE " ".
           03  D-MARGIN       PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
       01  TOTAL-LINE.
           03  FILLER         PIC X(57) VALUE " ".
           03  T-CATEGORY     PIC X(10) VALUE " ".
           03  T-SALESQTY     PIC Z(6)9-.
           03  FILLER         PIC X(4) VALUE " ".
           03  T-SALESAMT     PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-COST         PIC Z(7)9.99-.
           03  T-MARGIN       PIC Z(7)9.99-.
           03  FILLER         PIC X(1) VALUE " ".
           03  T-PERC         PIC Z(3)9.99-.
           03  FILLER         PIC X(9) VALUE " ".
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM CLEAR-SCREEN.
           MOVE 317 TO POS.
           DISPLAY "** SALES AND GROSS MARGIN REPORT **" AT POS.
           MOVE 417 TO POS.
           DISPLAY "***********************************" AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 1010 TO POS.
           DISPLAY "   FROM STOCK NUMBER : [               ]" AT POS.
           MOVE 1034 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 33        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER1.

      *     ACCEPT WS-ANSWER1 AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-005.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-010.
       CONTROL-015.
           MOVE 1210 TO POS.
           DISPLAY "     TO STOCK NUMBER : [               ]" AT POS.
           MOVE 1234 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 33        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER2.

      *     ACCEPT WS-ANSWER2 AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO CONTROL-010.
           IF WS-ANSWER2 = " "
              GO TO CONTROL-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO CONTROL-020
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-015.
       CONTROL-020.
           MOVE 1410 TO POS.
           DISPLAY "CATEGORY TOTALS ONLY : [ ]" AT POS.
           MOVE 1434 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 11        TO CDA-ROW.
           MOVE 33        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ANSWER3.

      *     ACCEPT WS-ANSWER3 AT POS.
           IF W-ESCAPE-KEY = 4
               GO TO CONTROL-015.
           IF WS-ANSWER3 NOT = "Y" AND NOT = "N"
               MOVE " " TO WS-ANSWER3
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5 OR = X"1B"
               GO TO CONTROL-022
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO CONTROL-020.
       CONTROL-022.
           MOVE 2810 TO POS.
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.
           DISPLAY "Report Is Being Compiled, Please Be Patient."
              AT POS.
           PERFORM OPEN-FILES
           PERFORM PRINT-ROUTINE.
           PERFORM END-OFF.
      *
       PRINT-ROUTINE SECTION.
       PRR-000.
           MOVE WS-ANSWER1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       PRR-005.
           READ STOCK-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              PERFORM PRR-025
              GO TO PRR-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO PRR-005.
           IF ST-STOCKNUMBER < WS-ANSWER1
              GO TO PRR-005.
           IF ST-STOCKNUMBER > WS-ANSWER2
              PERFORM PRR-025
              GO TO PRR-999.

           MOVE 2510 TO POS
           DISPLAY "STOCKNUMBER BEING READ:" AT POS
           ADD 25 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.

           IF WS-STORE = "   "
              MOVE ST-CATEGORY TO WS-STORE.
       PRR-010.
            IF LINE-CNT < 60
               GO TO PRR-020.
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
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC FROM HEAD4 AFTER 1
            MOVE " " TO PRINT-REC
            WRITE PRINT-REC AFTER 1
            MOVE 8 TO LINE-CNT.
       PRR-020.
           IF ST-CATEGORY NOT = WS-STORE
               PERFORM PRR-025.
           ADD ST-SALESUNITMTD   TO WS-SALESQTY
                                    TOT-SALESQTY.
           ADD ST-SALESUNITSYTD  TO WS-SALESQTY-YTD
                                    TOT-SALESQTY-YTD.
           ADD ST-SALESUNITSLAST TO WS-SALESQTY-LAST
                                    TOT-SALESQTY-LAST.
           ADD ST-SALESRANDSMTD  TO WS-SALESAMT
                                    TOT-SALESAMT.
           ADD ST-SALESRANDSYTD  TO WS-SALESAMT-YTD
                                    TOT-SALESAMT-YTD.
           ADD ST-SALESRANDSLAST TO WS-SALESAMT-LAST
                                    TOT-SALESAMT-LAST.
           ADD ST-SALESCOSTMTD   TO WS-COST
                                    TOT-COST.
           ADD ST-SALESCOSTYTD   TO WS-COST-YTD
                                    TOT-COST-YTD.
           ADD ST-SALESCOSTLAST  TO WS-COST-LAST
                                    TOT-COST-LAST.
       PRR-022.
           IF WS-ANSWER3 = "Y"
               GO TO PRR-005.
           MOVE ST-STOCKNUMBER   TO D-STOCK.
           MOVE ST-DESCRIPTION1  TO D-DESC1.
           MOVE ST-DESCRIPTION2  TO D-DESC2.
           MOVE ST-SALESUNITMTD  TO D-SALESQTY.
           MOVE ST-SALESRANDSMTD TO D-SALESAMT.
           MOVE ST-SALESCOSTMTD  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSMTD - ST-SALESCOSTMTD).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTMTD) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.

           MOVE " "              TO D-STOCK
                                    D-DESC1
                                    D-DESC2.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           MOVE ST-SALESUNITSYTD TO D-SALESQTY.
           MOVE ST-SALESRANDSYTD TO D-SALESAMT.
           MOVE ST-SALESCOSTYTD  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSYTD - ST-SALESCOSTYTD).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTYTD) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           MOVE 0                 TO WS-MARGIN
                                     WS-PERC.
           MOVE ST-SALESUNITSLAST TO D-SALESQTY.
           MOVE ST-SALESRANDSLAST TO D-SALESAMT.
           MOVE ST-SALESCOSTLAST  TO D-COSTSAMT.
           COMPUTE WS-MARGIN ROUNDED =
               (ST-SALESRANDSLAST - ST-SALESCOSTLAST).
           MOVE WS-MARGIN        TO D-MARGIN.
           COMPUTE WS-PERC ROUNDED =
               (WS-MARGIN / ST-SALESCOSTLAST) * 100.
           MOVE WS-PERC          TO D-PERC.
           WRITE PRINT-REC FROM DETAIL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
           MOVE 0                TO WS-MARGIN
                                    WS-PERC.
           
           GO TO PRR-005.
       PRR-025.
           MOVE WS-STORE    TO T-CATEGORY.
           MOVE WS-SALESQTY TO T-SALESQTY.
           MOVE WS-SALESAMT TO T-SALESAMT.
           MOVE WS-COST     TO T-COST.
           COMPUTE WS-MARGIN = (WS-SALESAMT - WS-COST).
           MOVE WS-MARGIN   TO T-MARGIN.
           COMPUTE WS-PERC ROUNDED = (WS-MARGIN / WS-COST) * 100.
           MOVE WS-PERC     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.

           MOVE WS-SALESQTY-YTD TO T-SALESQTY.
           MOVE WS-SALESAMT-YTD TO T-SALESAMT.
           MOVE WS-COST-YTD     TO T-COST.
           COMPUTE WS-MARGIN-YTD = (WS-SALESAMT-YTD - WS-COST-YTD).
           MOVE WS-MARGIN-YTD   TO T-MARGIN.
           COMPUTE WS-PERC-YTD ROUNDED =
               (WS-MARGIN-YTD / WS-COST-YTD) * 100.
           MOVE WS-PERC-YTD     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           MOVE WS-SALESQTY-LAST TO T-SALESQTY.
           MOVE WS-SALESAMT-LAST TO T-SALESAMT.
           MOVE WS-COST-LAST     TO T-COST.
           COMPUTE WS-MARGIN-LAST = (WS-SALESAMT-LAST - WS-COST-LAST).
           MOVE WS-MARGIN-LAST   TO T-MARGIN.
           COMPUTE WS-PERC-LAST ROUNDED =
               (WS-MARGIN-LAST / WS-COST-LAST) * 100.
           MOVE WS-PERC-LAST     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           WRITE PRINT-REC AFTER 1.

           ADD 4 TO LINE-CNT.
           MOVE 1610 TO POS.
           DISPLAY "Category: " AT POS.
           MOVE 1620 TO POS.
           DISPLAY WS-STORE AT POS.
           MOVE 1710 TO POS.
           DISPLAY "Sales For The Month : R" AT POS.
           MOVE 1733 TO POS.
           MOVE WS-SALESAMT TO WS-SALES-DIS.
           DISPLAY WS-SALES-DIS AT POS.
           MOVE 0 TO WS-SALESQTY
                     WS-SALESAMT
                     WS-COST
                     WS-MARGIN
                     WS-PERC
                     WS-SALESQTY-YTD
                     WS-SALESAMT-YTD
                     WS-COST-YTD
                     WS-MARGIN-YTD
                     WS-PERC-YTD
                     WS-SALESQTY-LAST
                     WS-SALESAMT-LAST
                     WS-COST-LAST
                     WS-MARGIN-LAST
                     WS-PERC-LAST.
           IF WS-STORE = "XRN"
             IF WS-ANSWER3 = "Y"
               PERFORM END-000.
           MOVE ST-CATEGORY TO WS-STORE.  
       PRR-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-022.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-022.
       OPEN-040.
           MOVE Ws-Co-Name TO CO-NAME.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           MOVE "TOTALS***"  TO T-CATEGORY.
           MOVE TOT-SALESQTY TO T-SALESQTY.
           MOVE TOT-SALESAMT TO T-SALESAMT.
           MOVE TOT-COST     TO T-COST.
           COMPUTE TOT-MARGIN = (TOT-SALESAMT - TOT-COST).
           MOVE TOT-MARGIN   TO T-MARGIN.
           COMPUTE TOT-PERC ROUNDED =
               (TOT-MARGIN / TOT-COST) * 100.
           MOVE TOT-PERC     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2.

           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           MOVE 0   TO TOT-MARGIN
                       TOT-PERC.

           MOVE TOT-SALESQTY-YTD TO T-SALESQTY.
           MOVE TOT-SALESAMT-YTD TO T-SALESAMT.
           MOVE TOT-COST-YTD     TO T-COST.
           COMPUTE TOT-MARGIN-YTD =
               (TOT-SALESAMT-YTD - TOT-COST-YTD).
           MOVE TOT-MARGIN-YTD   TO T-MARGIN.
           COMPUTE TOT-PERC-YTD ROUNDED =
               (TOT-MARGIN-YTD / TOT-COST-YTD) * 100.
           MOVE TOT-PERC-YTD     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC
                       TOTAL-LINE.
           MOVE 0   TO TOT-MARGIN
                       TOT-PERC.

           MOVE TOT-SALESQTY-LAST TO T-SALESQTY.
           MOVE TOT-SALESAMT-LAST TO T-SALESAMT.
           MOVE TOT-COST-LAST     TO T-COST.
           COMPUTE TOT-MARGIN-LAST =
               (TOT-SALESAMT-LAST - TOT-COST-LAST).
           MOVE TOT-MARGIN-LAST   TO T-MARGIN.
           COMPUTE TOT-PERC-LAST ROUNDED =
               (TOT-MARGIN-LAST / TOT-COST-LAST) * 100.
           MOVE TOT-PERC-LAST     TO T-PERC.
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1.
           MOVE " " TO PRINT-REC.
           WRITE PRINT-REC AFTER 1.
           ADD 4 TO LINE-CNT.
       END-450.
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.
       END-500.
           CLOSE PRINT-FILE.
           PERFORM SEND-REPORT-TO-PRINTER.
           CLOSE STOCK-MASTER.
       END-900.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB.
