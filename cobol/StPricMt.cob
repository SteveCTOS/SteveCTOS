        IDENTIFICATION DIVISION.
        PROGRAM-ID. StPricMt.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStockLookup".
          Copy "SelectSlDaily".
           SELECT PRICE-MASTER ASSIGN TO "/main/spl/Prices"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRICE-KEY
               FILE STATUS IS WS-PRICE-STATUS.
           SELECT PRICE-IMP-MASTER ASSIGN TO "/main/spl/PriceSequ"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STOCK-STATUS.
           SELECT STOCK-IMP-MASTER ASSIGN TO "/main/spl/StockSequ"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STOCK-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStockLookup.
           COPY ChlfdPriceUpdate.
           COPY ChlfdPriceImport.
           COPY ChlfdStockLookupImport.
           COPY ChlfdDaily.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF            PIC X(3) VALUE "   ".
           77  WS-ACCEPT         PIC X(10).
           77  WS-TEMP           PIC X.
           77  WS-DEL-DELAY      PIC 99.
           77  WS-ONHAND         PIC 9(6).
           77  WS-AVECOST        PIC 9(6)V99.
           77  WS-RANGE1         PIC X(15) VALUE " ".
           77  WS-RANGE2         PIC X(15) VALUE " ".
           77  WS-UP-DESC        PIC X VALUE "N".
           77  WS-UP-COST        PIC X VALUE "Y".
           77  WS-IMP-UPDATE     PIC X VALUE " ".
           77  WS-USE-LOOKUP-DATA PIC X VALUE " ".
           77  WS-FOR-LOC        PIC X VALUE " ".
           01  WS-DESC.
               03  WS-DESC1    PIC X(20) VALUE " ".
               03  WS-DESC2    PIC X(20) VALUE " ".
           01  WS-STOCK-PREFIX.
               03  WS-SP1      PIC X(4) VALUE " ".
               03  WS-SP2      PIC X(11) VALUE " ".
           01  WS-STOCK-CHECKING.
               03  WS-SC1      PIC X(3) VALUE " ".
               03  WS-SC2      PIC X(12) VALUE " ".
           01  WS-STOCK-STATUS.
               03  WS-STOCK-ST1  PIC X.
               03  WS-STOCK-ST2  PIC 99 COMP-X.
           01  WS-LOOK-STATUS.
               03  WS-LOOK-ST1  PIC X.
               03  WS-LOOK-ST2  PIC 99 COMP-X.
           01  WS-PRICE-STATUS.
               03  WS-PRICE-ST1  PIC X.
               03  WS-PRICE-ST2  PIC 99 COMP-X.
           01  WS-DAILY-STATUS.
               03  WS-DAILY-ST1  PIC X.
               03  WS-DAILY-ST2  PIC 99 COMP-X.
               
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      * COPY CHLFDDateConversion.
      *
       PROCEDURE DIVISION USING Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
           PERFORM CLEAR-SCREEN.
           MOVE 0320 TO POS
           DISPLAY "*** STOCK PRICE IMPORT PROGRAM ****" AT POS
           MOVE 0420 TO POS
           DISPLAY "***********************************" AT POS.
       CONTROL-000.
           PERFORM GET-DATA.
           PERFORM OPEN-FILES.
           
      ***************************************************************
      * USED TO IMPORT FROM THE EXCEL FILE <SPL>PRICESEQU.  THIS IS *
      * FOR STOCK ITEMS WHERE THE PART NUMBERS ARE VERY SIMILAR TO  *
      * OUR OWN NUMBERS.  ONLY SAY KEN NEEDS TO BE ADDED IN FRONT OF*
      * THE SUPPLIERS PART NUMBER.                                  *
      ***************************************************************
           IF WS-IMP-UPDATE = "I"
              PERFORM IMPORT-DATA
              PERFORM END-OFF. 
           IF WS-IMP-UPDATE = "B"
              PERFORM IMPORT-DATA.
              
      ****************************************************************
      * USED TO WRITE A LOOKUP TABLE WITH OUR PART NUMBERS AND BLANK *
      * SUPPLIER NUMBERS SO THEY CAN BE ENTERED MANUALLY LATER IN AN *
      * EDIT PROGRAM.                                                *
      ****************************************************************
           IF WS-IMP-UPDATE = "W"
              PERFORM WRITE-LOOKUP
              GO TO CONTROL-900.
              
      ****************************************************************
      * USED TO WRITE A LOOKUP TABLE WITH OUR PART NUMBERS TAKEN FROM*
      * SUPPLIER NUMBERS IN AN EXCEL SPREADSHEET, PUT THRU PYTHON &  *
      * THEN USED IN THE UPDATE SECTION OF THIS PROGRAM.             *
      ****************************************************************
           IF WS-IMP-UPDATE = "S"
              PERFORM IMPORT-LOOKUP
              GO TO CONTROL-900.
              
           PERFORM UPDATE-PRICES.
       CONTROL-900.
           PERFORM END-OFF.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-RANGE1 WS-RANGE2.
           MOVE 0401 TO POS
           DISPLAY "ADDED PREFIX:" AT POS
           MOVE 0501 TO POS
           DISPLAY "BKM, CK, FLK, WLR" AT POS
           
           MOVE 0701 TO POS
           DISPLAY "NO ADDED PREFIX:" AT POS
           MOVE 0801 TO POS
           DISPLAY "BRN, GDR, KEN," AT POS.
           MOVE 0901 TO POS
           DISPLAY "MGL, MI, SCS" AT POS.
       GET-005.
           MOVE 821 TO POS.
           DISPLAY "FROM STOCK NUMBER: [               ]" AT POS
           MOVE 841 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 15        TO CDA-DATALEN.
           MOVE 5         TO CDA-ROW.
           MOVE 40        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-RANGE1.

      *     ACCEPT WS-RANGE1 AT POS.
           IF W-ESCAPE-KEY = 3 OR 4
              EXIT PROGRAM.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-010
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-005.
       GET-010.
           MOVE 921 TO POS.
           DISPLAY "  TO STOCK NUMBER: [               ]" AT POS
           MOVE 941 TO POS.
           ACCEPT WS-RANGE2 AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-000.
            IF WS-RANGE2 = " "
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-012
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-010.
       GET-012.
           MOVE 1210 TO POS
           DISPLAY "I=IMPORT PRICE MUST BE /main/spl/PriceSequ" AT POS
           MOVE 1310 TO POS
           DISPLAY "S=LOOKUP TABLE MUST BE /main/spl/StockSequ" AT POS
           MOVE 1110 TO POS.
           DISPLAY
           "I=IMPORT, U=UPDATE, B=I & U, W=BLANK Lookup, S=EXCEL" &
           "LOOKUP: [ ]"
            AT POS
           ADD 62 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 9         TO CDA-ROW.
           MOVE 71        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-IMP-UPDATE.

      *     ACCEPT WS-IMP-UPDATE AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-010.
           IF WS-IMP-UPDATE NOT = "I" AND NOT = "U" AND NOT = "B"
                        AND NOT = "W" AND NOT = "S"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-012.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-015
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-012.
       GET-015.
           IF WS-IMP-UPDATE = "W" OR = "S" OR = "I"
               GO TO GET-999.
           MOVE 1510 TO POS
           DISPLAY "UPDATE DESCRIPTIONS ON STOCK FILE : [ ]"
           AT POS
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 12        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UP-DESC
           
      *     ACCEPT WS-UP-DESC AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-012.
           IF WS-UP-DESC NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-015.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
              GO TO GET-020
           ELSE
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-015.
       GET-020.
           MOVE 1710 TO POS.
           DISPLAY "F=FOREIGN, L=LOCAL PRICES         : [ ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 14        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-FOR-LOC.

      *     ACCEPT WS-FOR-LOC AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-015.
           IF WS-FOR-LOC NOT = "F" AND NOT = "L"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-020.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-025
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-020.
       GET-025.
           MOVE "N" TO WS-UP-COST.
           MOVE " " TO ALPHA-RATE.
           MOVE 1910 TO POS.
           DISPLAY "UPDATE LAST COST ON STOCK FILE    : [ ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 16        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-UP-COST.

      *     ACCEPT WS-UP-COST AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-020.
           IF WS-UP-COST NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-025.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-030
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-025.
       GET-030.
           MOVE " " TO ALPHA-RATE.
           MOVE 2010 TO POS.
           DISPLAY "UPDATE USING LOOKUP TABLE. Y OR N : [ ]" AT POS.
           ADD 37 TO POS.

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 17        TO CDA-ROW.
           MOVE 46        TO CDA-COL.
           MOVE CDA-GREEN TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-USE-LOOKUP-DATA.

      *     ACCEPT WS-USE-LOOKUP-DATA AT POS.
           IF W-ESCAPE-KEY = 4
              GO TO GET-025.
           IF WS-USE-LOOKUP-DATA NOT = "N" AND NOT = "Y"
              DISPLAY " " AT 3079 WITH BELL
              GO TO GET-030.
           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO GET-999
           ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-030.
       GET-999.
            EXIT.
      *
       IMPORT-DATA SECTION.
       ID-000.
           PERFORM CHECK-PREFIX-IN-STOCK.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK PRICES BEING IMPORTED...." AT POS.
       ID-005.
           READ PRICE-IMP-MASTER
              AT END
                 GO TO ID-999.
       ID-010.
           PERFORM MERGE-PARTS.
           MOVE WS-STOCK-PREFIX            TO PRICE-ST-NUM.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK ITEM BEING IMPORTED:      " AT POS
           ADD 27 TO POS
           DISPLAY PRICE-ST-NUM AT POS.
           
           MOVE PRICE-IMP-DESCRIPTION      TO PRICE-DESCRIPTION
           PERFORM ENTER-UNIT-OF-SALE.
           
           MOVE PRICE-IMP-OLD-DEALER-PRICE TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           COMPUTE NUMERIC-RATE = NUMERIC-RATE / 100
           MOVE NUMERIC-RATE               TO PRICE-OLD-DEALER-PRICE
           
           MOVE PRICE-IMP-NEW-DEALER-PRICE TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           COMPUTE NUMERIC-RATE = NUMERIC-RATE / 100
           MOVE NUMERIC-RATE               TO PRICE-NEW-DEALER-PRICE
           
           MOVE PRICE-IMP-DEALER-DISCOUNT  TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           COMPUTE NUMERIC-RATE = NUMERIC-RATE / 100.
           MOVE NUMERIC-RATE               TO PRICE-DEALER-DISCOUNT

           MOVE PRICE-IMP-OLD-LIST-PRICE   TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           COMPUTE NUMERIC-RATE = NUMERIC-RATE / 100.
           MOVE NUMERIC-RATE               TO PRICE-OLD-LIST-PRICE.

           MOVE PRICE-IMP-NEW-LIST-PRICE   TO ALPHA-RATE
           PERFORM DECIMALISE-RATE
           COMPUTE NUMERIC-RATE = NUMERIC-RATE / 100
           MOVE NUMERIC-RATE               TO PRICE-NEW-LIST-PRICE.
       ID-020.
           WRITE PRICE-RECORD.
           IF WS-PRICE-ST1 = 23 OR 35 OR 49
      *           MOVE "INVALID WRITE OF PRICE UPDATE, ITEM EXISTS"
      *            TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           MOVE WS-PRICE-ST1 TO WS-MESSAGE
      *           PERFORM ERROR1-000
      *           MOVE WS-PRICE-ST2 TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           PERFORM ERROR1-020
                 GO TO ID-005.
           IF WS-PRICE-ST1 NOT = 0
      *           MOVE "INVALID WRITE OF PRICE UPDATE" TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           MOVE WS-PRICE-ST1 TO WS-MESSAGE
      *           PERFORM ERROR1-000
      *           MOVE WS-PRICE-ST2 TO WS-MESSAGE
      *           PERFORM ERROR-MESSAGE
      *           PERFORM ERROR1-020
                 GO TO ID-020.
           GO TO ID-005.
       ID-999.
           EXIT.
      *
       ENTER-UNIT-OF-SALE SECTION.
       EUOS-005.
           MOVE SPACES                 TO ALPHA-RATE DATA-RATE.
           MOVE PRICE-IMP-UNIT-OF-SALE TO ALPHA-RATE.
           MOVE 7 TO SUB-1
           MOVE 1 TO SUB-2.
       EUOS-010.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2)
           IF SUB-1 < 10
              ADD 1 TO SUB-1 SUB-2
              GO TO EUOS-010.
           MOVE DATA-RATE TO PRICE-UNIT-OF-SALE.
       EUOS-999.
           EXIT.
      *
       MERGE-PARTS SECTION.
       MP-010.
           MOVE SPACES           TO ALPHA-RATE DATA-RATE.
           MOVE PRICE-IMP-KEY    TO WS-SP2
                                    WS-STOCK-CHECKING
           MOVE WS-STOCK-PREFIX  TO ALPHA-RATE.
           MOVE 1 TO SUB-1 SUB-2.
           
           IF WS-EOF = "BRN"
            IF WS-SC1 = "BRN"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1 TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "GDR"
            IF WS-SC1 = "GDR"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1 TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "KEN"
            IF WS-SC1 = "KEN"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1 TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "MGL"
            IF WS-SC1 = "MGL"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1 TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "MI "
            IF WS-SC1 = "MI "
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1 TO DATA-RATE
              MOVE 3 TO SUB-1
              MOVE 4 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "SCS"
            IF WS-SC1 = "SCS"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE WS-SC1 TO DATA-RATE
              MOVE 4 TO SUB-1
              MOVE 5 TO SUB-2
              GO TO MP-050.
           IF WS-EOF = "BKM" OR = "CK " OR = "FLK" OR = "WLR"
              MOVE SPACES            TO ALPHA-RATE
              MOVE WS-STOCK-CHECKING TO ALPHA-RATE
              MOVE SPACES TO DATA-RATE
              MOVE 1      TO SUB-1
              MOVE 1      TO SUB-2
              GO TO MP-015.
              
            GO TO MP-900.
       MP-015.
            IF SUB-1 < 15
             IF AL-RATE (SUB-1) = " "
               ADD 1 TO SUB-1
               GO TO MP-015.
       MP-050.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-1 < 15
              ADD 1 TO SUB-1 SUB-2
              GO TO MP-050.
           MOVE DATA-RATE TO ALPHA-RATE.
       MP-900.
           MOVE ALPHA-RATE TO WS-STOCK-PREFIX.
           MOVE SPACES     TO ALPHA-RATE.
       MP-999.
           EXIT.
      *
       CHECK-PREFIX-IN-STOCK SECTION.
       CPIAS-010.
           MOVE WS-RANGE1 TO WS-EOF.
           IF WS-EOF = "KEN"
             GO TO CPIAS-020.
           IF WS-EOF = "BRN"
             GO TO CPIAS-030.
           IF WS-EOF = "GDR"
             GO TO CPIAS-042.
           IF WS-EOF = "MGL"
             GO TO CPIAS-050.
           IF WS-EOF = "MI "
             GO TO CPIAS-060.
           IF WS-EOF = "SCS"
             GO TO CPIAS-065.

           IF WS-EOF = "BKM" OR = "CK " OR = "FLK" OR = "WLR"
              MOVE " " TO WS-STOCK-PREFIX.
           GO TO CPIAS-999.
       CPIAS-020.
           MOVE "KEN " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-030.
           MOVE "BRN " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-042.
           MOVE "GDR " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-050.
           MOVE "MGL " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-060. 
           MOVE "MI " TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-065. 
           MOVE "SCS" TO WS-STOCK-PREFIX
              GO TO CPIAS-999.
       CPIAS-999.
           EXIT.
      *
       IMPORT-LOOKUP SECTION.
       ILUP-000.
           MOVE 2510 TO POS
           DISPLAY "STOCK LOOKUP BEING IMPORTED...." AT POS.
           MOVE WS-RANGE1 TO WS-EOF.
       ILUP-005.
           READ STOCK-IMP-MASTER
              AT END
                 GO TO ILUP-999.
       ILUP-010.
           IF STOCK-IMP-SUPPLIER = " "
               GO TO ILUP-005.
           MOVE STOCK-IMP-ST-NUM   TO STLOOK-STOCKNUMBER
           PERFORM REMOVE-LEADING-ZEROES.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK ITEM BEING IMPORTED:       " AT POS
           ADD 27 TO POS
           DISPLAY STOCK-IMP-ST-NUM AT POS.
       ILUP-020.
           WRITE STLOOK-RECORD.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE "INVALID WRITE, STOCK LOOKUP, ITEM EXISTS."
                  TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 PERFORM ERROR1-020
                 GO TO ILUP-025.
           IF WS-STOCK-ST1 NOT = 0
                 MOVE "INVALID WRITE OF STOCK LOOKUP." TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 PERFORM ERROR1-020
                 GO TO ILUP-020.
           GO TO ILUP-005.
       ILUP-025.
           REWRITE STLOOK-RECORD.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
                 MOVE "INVALID REWRITE OF STOCK LOOKUP, ITEM EXISTS."
                  TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 PERFORM ERROR1-020
                 GO TO ILUP-020.
           IF WS-STOCK-ST1 NOT = 0
                 MOVE "INVALID WRITE OF STOCK LOOKUP." TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 PERFORM ERROR1-020
                 GO TO ILUP-025.
           GO TO ILUP-005.
       ILUP-999.
           EXIT.
      *
       REMOVE-LEADING-ZEROES SECTION.
       RLZ-005.
           MOVE SPACES                TO ALPHA-RATE DATA-RATE
           MOVE STOCK-IMP-SUPPLIER    TO ALPHA-RATE
           MOVE SPACES                TO STLOOK-SUPPLIERNUMBER.
           IF WS-EOF = "WLR"
              MOVE 11 TO SUB-1
              MOVE 1 TO SUB-2
              GO TO RLZ-010.
           IF WS-EOF = "CK "
              MOVE 1 TO SUB-1
                        SUB-2
              GO TO RLZ-008.

              MOVE 1 TO SUB-1
                        SUB-2.
       RLZ-008.
           IF AL-RATE (SUB-1) = "0"
               ADD 1 TO SUB-1
            IF SUB-1 < 30 
               GO TO RLZ-008.
       RLZ-010.
           MOVE AL-RATE (SUB-1) TO DAT-RATE (SUB-2).
           IF SUB-1 < 30
               ADD 1 TO SUB-1 SUB-2
               GO TO RLZ-010.
       RLZ-900.
           MOVE DATA-RATE TO STLOOK-SUPPLIERNUMBER.
       RLZ-999.
           EXIT.
      *
       WRITE-LOOKUP SECTION.
       WRITE-002.
           MOVE WS-RANGE1 TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
              INVALID KEY NEXT SENTENCE.
       WRITE-005.
           READ STOCK-MASTER NEXT
              AT END NEXT SENTENCE.
           IF WS-STOCK-ST1 = 10
              GO TO WRITE-999.
           IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "Stock Files Busy on Read, Press 'CANCEL' To Retry"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRITE-005.
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO WRITE-005.
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO WRITE-999.
              
           MOVE 2620 TO POS
           DISPLAY "StockNumber Being Read:      " AT POS
           ADD 24 TO POS
           DISPLAY ST-STOCKNUMBER AT POS.
       WRITE-010.
      *
      * THIS SECTION ADDED SO THAT WE CAN ADD NEW STOCK NUMBERS TO
      * THE LOOKUP TABLE SO THAT WE CAN UPDATE THE LOOKUP TABLE
      * WITHOUT OVER WRITING WHAT IS IN THE LOOKUP TABLE ALREADY.
      * WRITE-010 THRU WRITE-015.
      *
           MOVE ST-STOCKNUMBER   TO STLOOK-STOCKNUMBER
           START STLOOK-MASTER KEY NOT < STLOOK-KEY
              INVALID KEY NEXT SENTENCE.
       WRITE-015.
           READ STLOOK-MASTER
              INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
              GO TO WRITE-020.
           IF WS-STOCK-ST1 = 0
              GO TO WRITE-050.
           IF WS-STOCK-ST1 NOT = 0
              MOVE 0 TO WS-STOCK-ST1
              MOVE "Stock LOOKUP Busy on Read, 'CANCEL' To Retry."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO WRITE-015.
       WRITE-020.
           MOVE ST-STOCKNUMBER   TO STLOOK-STOCKNUMBER
           MOVE " "              TO STLOOK-SUPPLIERNUMBER.
           WRITE STLOOK-RECORD
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               GO TO WRITE-050.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "WRITING OF NEW LOOKUP LIST IN ERROR." TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO WRITE-020.
       WRITE-050.
           GO TO WRITE-005.
       WRITE-999.
           EXIT.
      *
       READ-LOOKUP-FILE SECTION.
       RLF-005.
           MOVE ST-STOCKNUMBER TO STLOOK-STOCKNUMBER.
           
           START STLOOK-MASTER KEY NOT < STLOOK-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-LOOK-ST1 NOT = 0
      *          MOVE
      *     "ST-LOOKUP RECORD NOT THERE ON START, SKIPPING FILE."
      *          TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
                MOVE "UNKNOWN" TO PRICE-KEY
                GO TO RLF-999.
       RLF-010.
           READ STLOOK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-LOOK-ST1 = 23 OR 35 OR 49
      *          MOVE
      *     "ST-LOOKUP RECORD NOT THERE ON READ, SKIPPING FILE."
      *          TO WS-MESSAGE
      *          PERFORM ERROR1-000
      *          MOVE STLOOK-SUPPLIERNUMBER TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          PERFORM ERROR1-020
                MOVE "UNKNOWN" TO PRICE-KEY
                GO TO RLF-999.
           IF WS-LOOK-ST1 NOT = 0
              MOVE 0 TO WS-LOOK-ST1
              MOVE "StLOOKUP File Busy on Read, 'ESC' To Retry"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RLF-010.
                 
           MOVE 2560 TO POS
           DISPLAY STLOOK-SUPPLIERNUMBER AT POS.
      *     CALL "&LOCKKBD" USING F-FIELDNAME.

           IF STLOOK-SUPPLIERNUMBER = SPACES
               MOVE "UNKNOWN"             TO PRICE-KEY
           ELSE
               MOVE STLOOK-SUPPLIERNUMBER TO PRICE-KEY.
       RLF-999.
           EXIT.
      *
       READ-PRICE-MASTER SECTION.
       RPM-005.
           START PRICE-MASTER KEY NOT < PRICE-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-PRICE-ST1 NOT = 0
      *          MOVE
      *    "ST-PRICE RECORD NOT THERE ON START, SKIPPING FILE."
      *          TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
                MOVE "UNKNOWN" TO PRICE-KEY
                GO TO RPM-999.
       RPM-010.
           READ PRICE-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-PRICE-ST1 = 23 OR 35 OR 49
      *          MOVE
      *     "ST-PRICE RECORD NOT THERE ON READ, SKIPPING FILE."
      *          TO WS-MESSAGE
      *          PERFORM ERROR1-000
      *          MOVE PRICE-KEY TO WS-MESSAGE
      *          PERFORM ERROR-MESSAGE
      *          PERFORM ERROR1-020
                MOVE "UNKNOWN" TO PRICE-KEY
                GO TO RPM-999.
           IF WS-PRICE-ST1 NOT = 0
              MOVE 0 TO WS-PRICE-ST1
              MOVE "STLOOK FILE BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RPM-010.
                 
           MOVE 2660 TO POS
           DISPLAY PRICE-KEY AT POS.
      *     CALL "&LOCKKBD" USING F-FIELDNAME.

       RPM-999.
           EXIT.
      *
        UPDATE-PRICES SECTION.
        UP-000.
           MOVE 2510 TO POS
           DISPLAY "STOCK PRICES BEING UPDATED ........" AT POS.

           IF WS-USE-LOOKUP-DATA = "Y"
              MOVE WS-RANGE1 TO ST-KEY
              MOVE " "       TO PRICE-KEY
           ELSE
              MOVE WS-RANGE1 TO PRICE-KEY ST-KEY.
           
           START PRICE-MASTER KEY NOT < PRICE-KEY
                 INVALID KEY
                   MOVE "NO RECORDS ON PRICE LIST FILE " TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   GO TO END-900.
           START STOCK-MASTER KEY NOT < ST-KEY
                 INVALID KEY
                   MOVE "NO RECORDS ON ST FILE " TO WS-MESSAGE
                   PERFORM ERROR-MESSAGE
                   GO TO END-900.
        UP-005.
           READ STOCK-MASTER NEXT WITH LOCK
               AT END 
               DISPLAY " FILE AT END, FINISHING"
               GO TO UP-999.
           
           IF ST-STOCKNUMBER < WS-RANGE1
              GO TO UP-005.    
           IF ST-STOCKNUMBER > WS-RANGE2
              GO TO UP-999.
                 
           MOVE 2510 TO POS
           DISPLAY "STOCK ITEM BEING UPDATED:       " AT POS
           ADD 27 TO POS
           DISPLAY ST-KEY AT POS.
           
           IF WS-USE-LOOKUP-DATA = "Y"
              PERFORM READ-LOOKUP-FILE
           ELSE
              MOVE ST-KEY TO PRICE-KEY.
              
           IF WS-USE-LOOKUP-DATA = "Y"
            IF PRICE-KEY = "UNKNOWN"
             MOVE "D" TO ST-ANALYSIS
             GO TO UP-035.

           PERFORM READ-PRICE-MASTER.
      *****************************************************
      * IF THE PRICE RECORD IS NOT VALID THEN WE MOVE "D" *
      * TO ST-ANALYSIS ON STOCK-MASTER.                   *
      *****************************************************
           IF WS-PRICE-ST1 NOT = 0
             MOVE "D" TO ST-ANALYSIS
             GO TO UP-035.
           IF WS-USE-LOOKUP-DATA = "Y"
            IF PRICE-KEY = "UNKNOWN"
             MOVE "D" TO ST-ANALYSIS
            GO TO UP-035.
           
           IF WS-FOR-LOC = "L"
            IF PRICE-NEW-LIST-PRICE = ST-PRICE
              GO TO UP-025.
              
           IF WS-FOR-LOC = "F"
            IF PRICE-NEW-LIST-PRICE = ST-FOREIGNCOST
              IF PRICE-DEALER-DISCOUNT NOT = ST-SUPPLIERDISC
                 MOVE PRICE-DEALER-DISCOUNT TO ST-SUPPLIERDISC
                 MOVE PRICE-UNIT-OF-SALE    TO ST-UNITOFMEASURE
                 GO TO UP-022
              ELSE
                 GO TO UP-005.
        UP-022.
           IF WS-FOR-LOC = "L"
              MOVE ST-PRICE               TO ST-OLDPRICE
              MOVE PRICE-NEW-LIST-PRICE   TO ST-PRICE
              MOVE WS-DATE                TO ST-LASTPRICECHANGE
              MOVE PRICE-NEW-DEALER-PRICE TO ST-LASTCOST
           ELSE
              MOVE PRICE-NEW-LIST-PRICE   TO ST-FOREIGNCOST.
        UP-025.
           IF WS-UP-DESC = "Y"
              MOVE PRICE-DESCRIPTION     TO WS-DESC
              MOVE WS-DESC1              TO ST-DESCRIPTION1
              MOVE WS-DESC2              TO ST-DESCRIPTION2.

           MOVE PRICE-DEALER-DISCOUNT    TO ST-SUPPLIERDISC.
           MOVE PRICE-UNIT-OF-SALE       TO ST-UNITOFMEASURE.

           IF WS-FOR-LOC = "L"
            IF ST-AVERAGECOST = 0
                 MOVE PRICE-NEW-DEALER-PRICE TO ST-AVERAGECOST.
           IF WS-FOR-LOC = "L"
            IF ST-LASTCOST = 0
                 MOVE PRICE-NEW-DEALER-PRICE TO ST-LASTCOST.
               
           IF WS-FOR-LOC = "L"
            IF WS-UP-COST = "Y"
             IF ST-QTYONHAND = 0   
              IF ST-QTYONRESERVE = 0
               IF PRICE-NEW-DEALER-PRICE NOT = ST-AVERAGECOST
                 MOVE PRICE-NEW-DEALER-PRICE  TO ST-AVERAGECOST.
           IF WS-FOR-LOC = "L"
            IF WS-UP-COST = "Y"
              IF PRICE-NEW-DEALER-PRICE NOT = ST-LASTCOST
                 MOVE PRICE-NEW-DEALER-PRICE  TO ST-LASTCOST.
        UP-035.
           IF ST-UNITOFMEASURE = "0001"
               MOVE "EACH" TO ST-UNITOFMEASURE.
           REWRITE STOCK-RECORD
                 INVALID KEY
                 MOVE "INVALID REWRITE OF STOCK-RECORD" TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 MOVE WS-STOCK-ST1 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 PERFORM ERROR-MESSAGE.
           GO TO UP-005.
        UP-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O STOCK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
             MOVE  "STOCK BUSY ON OPEN, 'ESC' TO RETRY." 
              TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO OPEN-000.
       OPEN-003.
           IF WS-IMP-UPDATE = "U"
               GO TO OPEN-005.
           OPEN I-O PRICE-IMP-MASTER.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "ERROR ON OPENING IMPORT FILE - /spl/PriceSequ."
              TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO OPEN-003.
       OPEN-005.
           OPEN I-O PRICE-MASTER.
           IF WS-PRICE-ST1 NOT = 0
             MOVE "ERROR ON OPENING PRICE LIST - /spl/Prices."
              TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             GO TO OPEN-005.
       OPEN-010.
           OPEN I-O STLOOK-MASTER.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "ERROR ON OPENING EXCEL FILE - /data01/StockLookup."
              TO WS-MESSAGE
             MOVE WS-STOCKLOOKUP TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR1-000
             GO TO OPEN-010.
       OPEN-015.
           IF WS-IMP-UPDATE NOT = "S"
               GO TO OPEN-020.
           OPEN I-O STOCK-IMP-MASTER.
           IF WS-STOCK-ST1 NOT = 0
             MOVE "ERROR ON OPENING EXCEL FILE - /spl/StockSequ."
              TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE WS-STOCK-ST1 TO WS-MESSAGE
             PERFORM ERROR1-000
             GO TO OPEN-015.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT.
        OPEN-999.
           EXIT.
      *
        END-OFF SECTION.
        END-000.
           CLOSE STOCK-MASTER
                 PRICE-MASTER
                 STLOOK-MASTER.
        END-900.
           EXIT PROGRAM.
        END-999.
              EXIT.
      *
       Copy "GetSystemY2KDate".
       Copy "GetReportY2KDate".
       Copy "GetUserMailName".
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
       Copy "CTOSCobolAccept".

       
      * END-OF-JOB.
