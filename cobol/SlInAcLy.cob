        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlInAcLy.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
        SPECIAL-NAMES.
        CLASS WS-VALID-EMAIL IS
          '@' '_' '.' '-' '#'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.

        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         Copy "SelectDrMaster".
         Copy "SelectSlRegLy".
         Copy "SelectStTransLy".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdRegisterLy.
           COPY ChlfdStTransLy.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "DrNameIq".
       77  WS-INV-INQ-PROGRAM   PIC X(8) VALUE "SlInOrLy".
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-TYPE-OF-TRANS     PIC X VALUE " ".
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-ACCOUNTNUMBER     PIC X(7) VALUE " ".
       77  WS-PRINTANSWER       PIC X(12) VALUE " ".
       77  WS-ORDERQTY          PIC 9(5) VALUE 0.
       77  WS-SHIPQTY           PIC S9(4) VALUE 0.
       77  WS-CREDQTY           PIC S9(4) VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       77  WS-BODY-LINE         PIC Z(5)9.
       01  WS-SCROLL-NUMBERS.
           03  WS-SCROLL-NUM OCCURS 50000.
             05  WS-STTR-TYPE   PIC 99.
             05  WS-STTR-REF    PIC 9(6).
             05  WS-STTR-TRANS  PIC 9(6).
             05  WS-STTR-REF2   PIC 9(6).
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1    PIC 99.
       01  WS-INCR-LY-STATUS.
           03  WS-INCR-LY-ST1   PIC 99.
       01  WS-STTRANSLY-STATUS.
           03  WS-STTRANSLY-ST1 PIC 99.
       01  WS-SALEDATE.
           03 WS-SALEYY         PIC 9999.
           03 WS-SALEMM         PIC 99.
           03 WS-SALEDD         PIC 99.
       01  SPLIT-STOCK.
           03  SP-1STCHAR       PIC X.
           03  SP-REST          PIC X(14).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(75) VALUE
           "** INVOICED ORDERS INQUIRY BY ACCOUNT - L/YR **".
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC ZZ9.
           03  FILLER         PIC X(2) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(1) VALUE " ".
           03  FILLER         PIC X(10) VALUE "ACCOUNT :".
           03  H3-ACCOUNT     PIC X(10).
           03  FILLER         PIC X(7) VALUE "NAME :".
           03  H3-NAME        PIC X(52).
       01  HEAD3.
           03  FILLER         PIC X(16) VALUE "STOCK".
           03  FILLER         PIC X(30) VALUE "DESCRIPTION".
           03  FILLER         PIC X(23) VALUE "ORDER READY SHIPD".
           03  FILLER         PIC X(28) VALUE "PRICE P/ORDER NUMBER".
           03  FILLER         PIC X(18) VALUE "ORDER    DATE".
           03  FILLER         PIC X(17) VALUE "INV      DATE".
           03  FILLER         PIC X(6) VALUE " ".
       01  DETAIL-LINE.
           03  D-STOCKNO      PIC X(16).
           03  D-DESC1        PIC X(20).
           03  D-DESC2        PIC X(10).
           03  D-ORDERQTY     PIC Z(4)9.
           03  FILLER         PIC X VALUE " ".
           03  D-SHIPQTY      PIC Z(4)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-SHIPPEDQTY   PIC Z(4)9.
           03  D-PRICE        PIC Z(7)9.99.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-PONO         PIC X(21).
           03  D-1STINV       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-1STDATE      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-2NDINV       PIC Z(5)9.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-2NDDATE      PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-TRANS        PIC Z(5)9.
       01  TOTAL-LINE.
           03  FILLER         PIC X(24).
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
           DISPLAY "** Sales Invoice Enquiry By Account - L/YR **"
           AT POS
           MOVE 0410 TO POS
           DISPLAY "*********************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
       CONTROL-010.
           MOVE 2510 TO POS
           DISPLAY "Program Loading, Please be patient..." AT POS.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONTROL-020.
           PERFORM DISPLAY-FORM
           PERFORM GET-DATA
           GO TO CONTROL-020.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE 0420 TO POS
            DISPLAY "** LAST YEAR INFO **" AT POS.
            
            MOVE 0  TO PAGE-CNT
                      WS-ORDERQTY
                      WS-SHIPQTY.
            MOVE "N" TO WS-ANSWER.
            PERFORM OPEN-005.
            PERFORM CLEAR-MEMORY.

            MOVE 2905 TO POS
            DISPLAY 
           "Press 'PgDn' For Next Account, 'PgUp' For Previous Account,"
             AT POS
            MOVE 3005 TO POS
            DISPLAY " Or Enter Account Number." AT POS

            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
               PERFORM READ-DEBTOR-NEXT
               GO TO GET-010.
            IF F-EXIT-CH = X"05"
               PERFORM READ-DEBTOR-PREVIOUS
               GO TO GET-010.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER.
            IF DR-ACCOUNT-NUMBER = 0
                CLOSE DEBTOR-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM CLEAR-SCREEN
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                CLOSE STOCK-TRANSLY-FILE
                GO TO GET-000.
            PERFORM READ-DEBTORS.
            GO TO GET-020.
        GET-010.
            MOVE "ACCNO"           TO F-FIELDNAME
            MOVE 5                 TO F-CBFIELDNAME
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD
            MOVE 7                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
        GET-020.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
        
            MOVE "NAME"  TO F-FIELDNAME
            MOVE 4       TO F-CBFIELDNAME
            MOVE DR-NAME TO F-NAMEFIELD
            MOVE 40      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ADD1" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME
            MOVE DR-ADDRESS1 TO F-NAMEFIELD
            MOVE 25 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ADD2" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME
            MOVE DR-ADDRESS2 TO F-NAMEFIELD
            MOVE 25 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ADD3" TO F-FIELDNAME
            MOVE 4 TO F-CBFIELDNAME
            MOVE DR-ADDRESS3 TO F-NAMEFIELD
            MOVE 25 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            IF DR-NAME = "UNKNOWN"
                DISPLAY " " AT 3079 WITH BELL
                CLOSE STOCK-TRANSLY-FILE
                GO TO GET-000.
       GET-035.
           MOVE "                        " TO F-NAMEFIELD
           MOVE "DATE" TO F-FIELDNAME
           MOVE 4      TO F-CBFIELDNAME
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"01"
               GO TO GET-000.
           IF F-EXIT-CH = X"07"
               CLOSE STOCK-TRANSLY-FILE
               PERFORM DISPLAY-FORM
               GO TO GET-000.
           MOVE 10        TO F-CBFIELDLENGTH.
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
           MOVE 10            TO F-CBFIELDLENGTH.
           PERFORM WRITE-FIELD-ALPHA.
       GET-040.
            MOVE " " TO F-EXIT-CH.
            CLOSE STOCK-TRANSLY-FILE.

            PERFORM READ-ALL-TRANSACTIONS.
            PERFORM FILL-BODY.
            IF F-EXIT-CH = X"07" OR = X"09" OR = X"1F"
                PERFORM CLEAR-TRANSACTIONS
                MOVE 1   TO F-INDEX SUB-1
                MOVE "Y" TO WS-ANSWER
                GO TO GET-999.

            GO TO GET-999.

      *      MOVE " " TO F-EXIT-CH.
      *      PERFORM READ-TRANSACTIONS.
       GET-900.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.

            IF WS-ANSWER = "Y"
                CLOSE STOCK-TRANSLY-FILE
                GO TO GET-999.
            IF F-INDEX < 15
               MOVE 2905 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3005 TO POS
               DISPLAY " Or Press 'F10' To Print All Invoiced Orders."
               AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"07"
                CLOSE STOCK-TRANSLY-FILE
                PERFORM CLEAR-TRANSACTIONS
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                CLOSE STOCK-TRANSLY-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE STOCK-TRANSLY-FILE
                PERFORM ERROR1-020
                PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       FILL-BODY SECTION.
       FILL-000.
           PERFORM OPEN-005.

           MOVE 1 TO F-INDEX.
           MOVE 1 TO SUB-1 SUB-2 SUB-3.
           PERFORM SCROLL-PREVIOUS-PAGE.
       FILL-001.
           MOVE 0420 TO POS
           DISPLAY "** LAST YEAR INFO **" AT POS.
            
           MOVE 2702 TO POS
           DISPLAY "Press 'PgDn' For More, 'PgUp' For Prev,"
           AT POS
           ADD 40 TO POS
           DISPLAY "'F12' OR 'F11' to Scroll Up/Down," AT POS
           MOVE 2803 TO POS
           DISPLAY 
        "'ESC' OR 'TAB' To Clear The Screen, 'F10' To Print All" &
           " Transactions." AT POS.
           MOVE 2950 TO POS
           DISPLAY "'Alt-Z' = Zoom on trans." AT POS.
       FILL-010.
           MOVE 3015 TO POS 
           DISPLAY "Current Line#: " AT POS
           ADD 16 TO POS
           MOVE SUB-1 TO WS-BODY-LINE
           DISPLAY WS-BODY-LINE AT POS.

           IF SUB-1 < 1
              MOVE 1 TO SUB-1 F-INDEX.

            MOVE "STOCKNO"         TO F-FIELDNAME.
            MOVE 7                 TO F-CBFIELDNAME.
            MOVE 15                TO F-CBFIELDLENGTH.
            PERFORM USER-FILL-FIELD.
            PERFORM READ-FIELD-ALPHA.
      *UP-ARROW
           IF F-EXIT-CH = X"01" AND F-INDEX = 1
            IF SUB-1 = 1
              GO TO FILL-010
            ELSE
              PERFORM SCROLL-PREVIOUS
              MOVE 15 TO F-INDEX
              GO TO FILL-010.
           IF F-EXIT-CH = X"01" AND F-INDEX > 1
              SUBTRACT 1 FROM F-INDEX 
                              SUB-1
            IF F-INDEX > 0
              GO TO FILL-010
            ELSE
              MOVE 1 TO F-INDEX
              PERFORM SCROLL-PREVIOUS
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
      *DOWN-ARROW
           IF F-EXIT-CH = X"0B" AND F-INDEX < 15
            IF SUB-1 NOT = SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *SCROLL-UP
           IF F-EXIT-CH = X"11"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT
              MOVE 1 TO F-INDEX
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *SCROLL-DOWN
           IF F-EXIT-CH = X"13"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-PREVIOUS
             IF SUB-1 < 16
              MOVE F-INDEX TO SUB-1
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *NEXT-PAGE
           IF F-EXIT-CH = X"0C"
            IF SUB-1 NOT > SUB-9
              PERFORM SCROLL-NEXT-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-010
            ELSE
              GO TO FILL-010.
      *PREV-PAGE
           IF F-EXIT-CH = X"05"
              PERFORM SCROLL-PREVIOUS-PAGE
              MOVE 1 TO F-INDEX
              GO TO FILL-010.
      *TAB - <ALT-F8>
           IF F-EXIT-CH = X"09"
              GO TO FILL-900.
      *FINISH - <End>
           IF F-EXIT-CH = X"04"
              CLOSE STOCK-TRANSLY-FILE
              PERFORM END-OFF.
      *ESC
           IF F-EXIT-CH = X"07"
              GO TO FILL-900.
      * <f10> to print
           IF F-EXIT-CH = X"1F"
                CLOSE STOCK-TRANSLY-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE " " TO WS-MESSAGE
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO FILL-900.
      ***********************************************
      *ZOOMBOX MODE                                 *
      * <CODE-z> = X"FA"  <CODE-SHIFT-Z> = X"DA"    *
      ***********************************************
      *IN CTOS: <CODE-Z>;  IN LINUX: <ALT-Z>
           IF F-EXIT-CH = X"FA" OR = X"DA"
            IF WS-STTR-TYPE (SUB-1) = 1 OR = 4 OR = 6
                PERFORM ADD-TYPE-TO-NUMBER
                MOVE ALPHA-RATE TO WS-LINK-ACCOUNT
            ELSE
                MOVE 0          TO WS-LINK-ACCOUNT.

           IF F-EXIT-CH = X"FA" OR = X"DA"
                MOVE SUB-1        TO SUB-1SAVE
                MOVE F-INDEX      TO F-INDEXSAVE
                PERFORM CLEAR-SCREEN
                CALL WS-INV-INQ-PROGRAM USING WS-LINKAGE
                CANCEL WS-INV-INQ-PROGRAM
                MOVE 0 TO WS-LINK-ACCOUNT
                PERFORM CLEAR-SCREEN
                PERFORM DISPLAY-FORM
                PERFORM GET-010 THRU GET-020
                PERFORM FILL-001
                PERFORM CALC-POS-OF-CURSOR
                SUBTRACT 1 FROM SUB-9
                PERFORM RDALL-920
                GO TO FILL-010.

           MOVE 7 TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
      *RETURN
           IF F-EXIT-CH = X"0A" AND F-INDEX < 15
             IF SUB-1 NOT = SUB-9
              ADD 1 TO F-INDEX SUB-1
              GO TO FILL-010
             ELSE
              GO TO FILL-010.
       FILL-050.
           ADD 1 TO SUB-1 F-INDEX.
           IF SUB-1 > 10000
               MOVE "10,000 LINES ARE UP, 'ESC' TO <TAB>."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-900.
           IF F-INDEX < 16
               GO TO FILL-010.
           SUBTRACT 1 FROM SUB-1.
           IF SUB-1 < 1
              MOVE 1 TO SUB-1.
           PERFORM SCROLL-NEXT.
           MOVE 1 TO F-INDEX.
           GO TO FILL-010.
       FILL-900.
           CLOSE STOCK-TRANSLY-FILE.
       FILL-999.
           EXIT.
      *
       ADD-TYPE-TO-NUMBER SECTION.
       ATTN-001.
           MOVE " " TO WS-TYPE-OF-TRANS WS-MESSAGE.
      *     PERFORM ERROR-020.
           MOVE 3040 TO POS
           DISPLAY "ENTER: 1=INV, 4=P/SLIP, 6=C/NOTE: [ ]" AT POS
           MOVE 3076 TO POS.

           IF WS-STTR-TYPE (SUB-1) = 1
               MOVE "1"              TO CDA-DATA.
           IF WS-STTR-TYPE (SUB-1) = 4
               MOVE "4"              TO CDA-DATA.
           IF WS-STTR-TYPE (SUB-1) = 6
               MOVE "6"              TO CDA-DATA.
           MOVE 1                    TO CDA-DATALEN.
           MOVE 27                   TO CDA-ROW.
           MOVE 74                   TO CDA-COL.
           MOVE CDA-WHITE            TO CDA-COLOR.
           MOVE 'F'                  TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-TYPE-OF-TRANS.

           IF WS-TYPE-OF-TRANS NOT = "1" AND NOT = "4" AND NOT = "6"
                MOVE "YOU MUST ENTER EITHER 1 OR 4 OR 6, RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR1-MESSAGE
                GO TO ATTN-001.

           IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO ATTN-005
           ELSE
               MOVE 3010 TO POS
               DISPLAY " " AT 3079 WITH BELL
               GO TO ATTN-001.
       ATTN-005.
           MOVE SPACES TO ALPHA-RATE
                           DATA-RATE.
           IF WS-STTR-TYPE (SUB-1) = 1
            IF WS-TYPE-OF-TRANS = "1"
               MOVE 1 TO AL-RATE (1)
               MOVE WS-STTR-REF (SUB-1) TO DATA-RATE
               GO TO ATTN-006.
           IF WS-STTR-TYPE (SUB-1) = 1
            IF WS-TYPE-OF-TRANS = "4"
               MOVE WS-STTR-REF2 (SUB-1) TO DATA-RATE
               MOVE 4 TO AL-RATE (1)
               GO TO ATTN-006.
           IF WS-STTR-TYPE (SUB-1) = 6
            IF WS-TYPE-OF-TRANS = "6"
               MOVE 6 TO AL-RATE (1)
               MOVE WS-STTR-REF (SUB-1) TO DATA-RATE
               GO TO ATTN-006.
           IF WS-STTR-TYPE (SUB-1) = 6
            IF WS-TYPE-OF-TRANS = "1"
               MOVE WS-STTR-REF2 (SUB-1) TO DATA-RATE
               MOVE 1 TO AL-RATE (1)
               GO TO ATTN-006.
       ATTN-006.
           MOVE 1 TO SUB-10
           MOVE 2 TO SUB-15.
       ATTN-010.
           IF SUB-15 < 8
              MOVE DAT-RATE (SUB-10) TO AL-RATE (SUB-15)
              ADD 1 TO SUB-10 SUB-15
              GO TO ATTN-010.
       ATTN-999.
           EXIT.
      *
       CALC-POS-OF-CURSOR SECTION.
       CPOC-010.
            COMPUTE SUB-1 = SUB-1SAVE - F-INDEXSAVE.
            IF SUB-1 < 0
               MOVE 1 TO SUB-1.
            PERFORM SCROLL-NEXT.
       CPOC-500.
            MOVE SUB-1SAVE   TO SUB-1
            MOVE F-INDEXSAVE TO F-INDEX.
       CPOC-999.
           EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           PERFORM OPEN-005.
           MOVE 1 TO F-INDEX.
           MOVE 0 TO SUB-2.
       RDTR-005.
           MOVE "Y"               TO STTR-LY-AC-COMPLETE
           MOVE DR-ACCOUNT-NUMBER TO STTR-LY-ACCOUNT-NUMBER.
           MOVE WS-SALEDATE       TO STTR-LY-AC-DATE.
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-AC-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 NOT = 0
                CLOSE STOCK-TRANSLY-FILE
                GO TO RDTR-999.
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
           IF F-EXIT-CH = " "
            IF STTR-LY-AC-COMPLETE NOT = "Y"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-999.
           IF F-EXIT-CH = 1
            IF STTR-LY-AC-COMPLETE NOT = "Y"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-000.
           MOVE STTR-LY-STOCK-NUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           IF WS-SALEDATE NOT = 0
            IF STTR-LY-DATE < WS-SALEDATE
               MOVE 2910 TO POS
               DISPLAY "Reading Next Valid Transaction...." AT POS
               GO TO RDTR-010.
           IF STTR-LY-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO RDTR-999.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       RDTR-020.
           IF F-INDEX > 15
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' For Previous,"
                 AT POS
                ADD 43 TO POS
                DISPLAY " Or 'Esc' To Clear The Screen !" AT POS
                MOVE 3005 TO POS
                DISPLAY " Or Press 'F10' To Print All Invoiced Orders."
                   AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
           PERFORM ERROR1-020.
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
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO RDTR-999.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = "   " AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           IF STTR-LY-REFERENCE1 NOT = INCR-LY-INVOICE
              PERFORM READ-ORDER-REGISTER.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RDALL-000.
           CLOSE STOCK-TRANSLY-FILE.

           PERFORM OPEN-005.
           MOVE 1 TO F-INDEX.
           MOVE 0 TO SUB-2.
       RDALL-005.
           MOVE "Y"               TO STTR-LY-AC-COMPLETE
           MOVE DR-ACCOUNT-NUMBER TO STTR-LY-ACCOUNT-NUMBER.
           MOVE WS-SALEDATE       TO STTR-LY-AC-DATE.
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-AC-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 NOT = 0
                CLOSE STOCK-TRANSLY-FILE
                GO TO RDALL-999.
           MOVE " " TO F-EXIT-CH.
       RDALL-010.
          READ STOCK-TRANSLY-FILE NEXT
               AT END NEXT SENTENCE.
    
          IF WS-STTRANSLY-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDALL-900.
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
               GO TO RDALL-010.
           IF STTR-LY-TYPE NOT = 1 AND NOT = 6
               GO TO RDALL-010.
           IF STTR-LY-AC-COMPLETE NOT = "Y"
               MOVE 1 TO F-INDEX
               GO TO RDALL-900.
           MOVE STTR-LY-STOCK-NUMBER TO SPLIT-STOCK.
           IF SP-1STCHAR = "*"
               GO TO RDALL-010.
           IF WS-SALEDATE NOT = 0
            IF STTR-LY-DATE < WS-SALEDATE
               GO TO RDALL-010.
    
           IF STTR-LY-ACCOUNT-NUMBER NOT = DR-ACCOUNT-NUMBER
               MOVE 1 TO F-INDEX
               GO TO RDALL-900.
                        
           PERFORM ERROR-020.
       RDALL-020.
           MOVE STTR-LY-TYPE               TO WS-STTR-TYPE (SUB-1)
           MOVE STTR-LY-REFERENCE1         TO WS-STTR-REF (SUB-1).
           MOVE STTR-LY-TRANSACTION-NUMBER TO WS-STTR-TRANS (SUB-1).

           IF SUB-1 < 50000
              ADD 1 TO SUB-1
              PERFORM RDALL-910
              GO TO RDALL-010.
              
           MOVE "THERE ARE MORE THAN 50,000 ITEMS ON THIS ORDER."
             TO WS-MESSAGE
             PERFORM ERROR1-000
           MOVE "PRESS 'Esc' TO EXIT THE READ-ALL SECTION."
             TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
       RDALL-900.
           SUBTRACT 1 FROM SUB-1
           MOVE SUB-1 TO SUB-9.
           IF SUB-9 < 0
               MOVE 0 TO SUB-9.
       RDALL-910.
            MOVE "TOT-SHP"   TO F-FIELDNAME
            MOVE 7           TO F-CBFIELDNAME
            IF STTR-LY-TYPE = 1
               ADD STTR-LY-SHIPQTY        TO WS-SHIPQTY
            ELSE
               SUBTRACT STTR-LY-SHIPQTY FROM WS-SHIPQTY.
            MOVE 6           TO F-CBFIELDLENGTH
            MOVE WS-SHIPQTY  TO F-EDNAMEFIELDNUMNEG
            PERFORM WRITE-FIELD-NUMNEG.
       RDALL-920.
           MOVE 2912 TO POS.
           DISPLAY "Total # of Lines:" AT POS
           ADD 19 TO POS.
           MOVE SUB-9 TO WS-BODY-LINE.
           DISPLAY WS-BODY-LINE AT POS.
           ADD 1 TO SUB-9.
       RDALL-950.
           CLOSE STOCK-TRANSLY-FILE.
       RDALL-999.
           EXIT.
      *
       READ-ORDER-ONLY SECTION.
       RDONLY-005.
           IF SUB-1 > SUB-9
               GO TO RDONLY-999.
               
           MOVE WS-STTR-TYPE (SUB-1)   TO STTR-LY-TYPE
           MOVE WS-STTR-REF  (SUB-1)   TO STTR-LY-REFERENCE1
           MOVE WS-STTR-TRANS  (SUB-1) TO STTR-LY-TRANSACTION-NUMBER.
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY
                INVALID KEY NEXT SENTENCE.
           IF WS-STTRANSLY-ST1 NOT = 0
                CLOSE STOCK-TRANSLY-FILE
                GO TO RDONLY-999.
           MOVE " " TO F-EXIT-CH.
       RDONLY-010.
            READ STOCK-TRANSLY-FILE NEXT
               AT END NEXT SENTENCE.
          IF WS-STTRANSLY-ST1 = 10
               MOVE 1 TO F-INDEX
               GO TO RDONLY-999.
           IF WS-STTRANSLY-ST1 NOT = 0
            MOVE "STTRANSLY BUSY ON READ-ONLY, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STTRANSLY-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO RDONLY-010.

           IF STTR-LY-REFERENCE1 NOT = INCR-LY-INVOICE
              PERFORM READ-ORDER-REGISTER.
           MOVE INCR-LY-BO-INV-NO TO WS-STTR-REF2 (SUB-1).
       RDONLY-999.
           EXIT.
      *
       READ-DEBTORS SECTION.
       RD-000.
           MOVE DR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
           START DEBTOR-MASTER KEY NOT < DR-KEY
               INVALID KEY NEXT SENTENCE.
       RD-010.
            READ DEBTOR-MASTER
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER
                MOVE " " TO DR-NAME DR-ADDRESS1 DR-ADDRESS2
                         DR-ADDRESS3
                MOVE "UNKNOWN" TO DR-NAME
                MOVE 0 TO DR-POST-CODE
                GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR RECORD BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RD-010.
       RD-999.
            EXIT.
      *
       START-DEBTOR SECTION.
       DR-DR-000.
           MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER.
           START DEBTOR-MASTER KEY NOT LESS DR-KEY
              INVALID KEY NEXT SENTENCE.
       DR-DR-999.
             EXIT.
      *
       READ-DEBTOR-NEXT SECTION.
       R-DR-NX-000.
           MOVE 0 TO SUB-2.
       R-DR-NX-005. 
           READ DEBTOR-MASTER NEXT
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO R-DR-NX-005.
       R-DR-NX-999.
             EXIT.
      *
       READ-DEBTOR-PREVIOUS SECTION.
       RPREV-000.
           MOVE 0 TO SUB-2.
       RPREV-005. 
           READ DEBTOR-MASTER PREVIOUS
               AT END NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTOR BUSY ON START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RPREV-005.
       RPREV-999.
             EXIT.
      *
       READ-ORDER-REGISTER SECTION.
       ROR-000.
            MOVE STTR-LY-REFERENCE1 TO INCR-LY-INVOICE.
            MOVE STTR-LY-TYPE       TO INCR-LY-TRANS.
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
       PRINT-ROUTINE SECTION.
       PRR-000.
           PERFORM ERROR-020
           MOVE 2910 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 3010 TO POS
           DISPLAY "Printing In Progress, Please Be Patient." AT POS
           MOVE 0  TO PAGE-CNT
                     WS-ORDERQTY
                     WS-SHIPQTY.
           MOVE 66 TO LINE-CNT.
           
           PERFORM GET-USER-PRINT-NAME.
           OPEN OUTPUT PRINT-FILE.

           PERFORM OPEN-005.
           MOVE "Y"               TO STTR-LY-AC-COMPLETE.
           MOVE DR-ACCOUNT-NUMBER TO STTR-LY-ACCOUNT-NUMBER.
           START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-AC-KEY
                INVALID KEY NEXT SENTENCE.
       PRR-002.
            READ STOCK-TRANSLY-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-STTRANSLY-ST1 = 10
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO PRR-900.
           IF WS-STTRANSLY-ST1 NOT = 0
              MOVE "STTRANS BUSY ON READ-NEXT, IN 1 SEC GOING TO RETRY."
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
           IF STTR-LY-AC-COMPLETE NOT = "Y"
               MOVE 1 TO F-INDEX
               CLOSE STOCK-TRANSLY-FILE
               GO TO PRR-999.
            MOVE STTR-LY-STOCK-NUMBER TO SPLIT-STOCK.
            IF SP-1STCHAR = "*"
               GO TO PRR-002.
            IF STTR-LY-ACCOUNT-NUMBER < DR-ACCOUNT-NUMBER
               GO TO PRR-002.
            IF STTR-LY-ACCOUNT-NUMBER > DR-ACCOUNT-NUMBER
               GO TO PRR-900.
           IF WS-SALEDATE NOT = 0
            IF STTR-LY-DATE < WS-SALEDATE
               GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           IF STTR-LY-REFERENCE1 NOT = INCR-LY-INVOICE
              PERFORM READ-ORDER-REGISTER.
           MOVE STTR-LY-STOCK-NUMBER       TO D-STOCKNO
           MOVE STTR-LY-DESC1              TO D-DESC1
           MOVE STTR-LY-DESC2              TO D-DESC2
           MOVE INCR-LY-PORDER             TO D-PONO.
           IF STTR-LY-TYPE = 1
              MOVE STTR-LY-ORDERQTY           TO D-ORDERQTY.
           MOVE STTR-LY-SHIPQTY            TO D-SHIPQTY.
           IF STTR-LY-TYPE = 1
              MOVE STTR-LY-SHIPPEDQTY         TO D-SHIPPEDQTY.
           MOVE STTR-LY-PRICE              TO D-PRICE
           MOVE INCR-LY-BO-INV-NO          TO D-1STINV
           MOVE INCR-LY-BO-DATE            TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-1STDATE
           MOVE STTR-LY-REFERENCE1         TO D-2NDINV
           MOVE STTR-LY-DATE               TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE            TO D-2NDDATE
           MOVE STTR-LY-TRANSACTION-NUMBER TO D-TRANS
           WRITE PRINT-REC FROM DETAIL-LINE
           MOVE " " TO PRINT-REC DETAIL-LINE
           IF STTR-LY-TYPE = 1
              ADD STTR-LY-ORDERQTY            TO WS-ORDERQTY
              ADD STTR-LY-SHIPQTY             TO WS-SHIPQTY
           ELSE
              SUBTRACT STTR-LY-SHIPQTY FROM WS-SHIPQTY.
           ADD 1 TO LINE-CNT.
           GO TO PRR-002.
       PRR-060.
            ADD 1         TO PAGE-CNT
            MOVE PAGE-CNT TO H1-PAGE

           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC.

            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " "      TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " "      TO PRINT-REC
            MOVE DR-ACCOUNT-NUMBER TO H3-ACCOUNT
            MOVE DR-NAME           TO H3-NAME
            WRITE PRINT-REC FROM HEAD2 AFTER 1
            MOVE " "      TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " "      TO PRINT-REC
            WRITE PRINT-REC
            MOVE " "      TO PRINT-REC
            MOVE 5        TO LINE-CNT.
       PRR-900.
           MOVE WS-ORDERQTY TO T-ORDERQTY
           MOVE WS-SHIPQTY  TO T-SHIPQTY
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2
           PERFORM GET-USER-MAIL-NAME
           PERFORM GET-REPORT-Y2K-DATE
           PERFORM PRINT-REPORT-INFO.

           CLOSE PRINT-FILE.
           
           PERFORM SEND-REPORT-TO-PRINTER.
       PRR-999.
           EXIT.
      *
       SCROLL-NEXT SECTION.
       NEXT-000.
            ADD 1  TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 F-INDEX.
            MOVE 1 TO F-INDEX.
            PERFORM CLEAR-TRANSACTIONS.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 49985
                MOVE 49985 TO SUB-1.
       NEXT-010.
            PERFORM SCROLLING.
       NEXT-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO NEXT-010.
            IF SUB-1 > 49985  
                GO TO NEXT-025.
            MOVE 1 TO F-INDEX.
       NEXT-025.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 49985
              IF SUB-25 > 49985
               COMPUTE F-INDEX = 15 - (50001 - SUB-9)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 15
                MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-999.
             EXIT.
      *
       SCROLL-NEXT-PAGE SECTION.
       NEXT-PAGE-000.
            ADD 15  TO SUB-1.
            IF SUB-1 > SUB-9
               MOVE SUB-9 TO SUB-1.
            IF SUB-1 < 1
               MOVE 1 TO SUB-1 F-INDEX.
            MOVE 1 TO F-INDEX.
            PERFORM CLEAR-TRANSACTIONS.
            MOVE 1 TO F-INDEX.
            IF SUB-1 > 49985
                 MOVE 49985 TO SUB-1.
       NEXT-PAGE-010.
            PERFORM SCROLLING.
       NEXT-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO NEXT-PAGE-010.
            IF SUB-1 > 49985 
                GO TO NEXT-PAGE-025.
            MOVE 1 TO F-INDEX.
       NEXT-PAGE-025.
            SUBTRACT 15 FROM SUB-1.
            IF SUB-1 > 49985
              IF SUB-25 > 49985
               COMPUTE F-INDEX = 15 - (50001 - SUB-9)
               MOVE SUB-25 TO SUB-1
            ELSE
               MOVE 1 TO F-INDEX. 
            IF F-INDEX > 15
               MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.

            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       NEXT-PAGE-999.
             EXIT.
      *
       SCROLL-PREVIOUS-PAGE SECTION.
       PREV-PAGE-000.
            PERFORM CLEAR-TRANSACTIONS.
            SUBTRACT 15 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-PAGE-010.
            PERFORM SCROLLING.
       PREV-PAGE-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO PREV-PAGE-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 15 FROM SUB-1.
       PREV-PAGE-025.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       PREV-PAGE-999.
            EXIT.
      *
       SCROLL-PREVIOUS SECTION.
       PREV-000.
            PERFORM CLEAR-TRANSACTIONS.
            SUBTRACT 15 FROM SUB-1.
            MOVE 1 TO F-INDEX.
            IF SUB-1 < 1
                 MOVE 1 TO SUB-1.
       PREV-010.
            PERFORM SCROLLING.
       PREV-020.
            ADD 1 TO F-INDEX SUB-1.
            IF F-INDEX < 16
                GO TO PREV-010.
            MOVE 1 TO F-INDEX.
            SUBTRACT 1 FROM SUB-1.
       PREV-025.
            IF SUB-1 < 1
                MOVE 1 TO SUB-1.
            MOVE 3015 TO POS.
            DISPLAY "Current Line#: " AT POS
            ADD 16 TO POS.
            MOVE SUB-1 TO WS-BODY-LINE.
            DISPLAY WS-BODY-LINE AT POS.
       PREV-999.
            EXIT.
      *
       SCROLLING SECTION.
       SCROLL-000.
            IF SUB-1 < SUB-9
               PERFORM READ-ORDER-ONLY
            ELSE
               GO TO SCROLL-999.

            MOVE "STOCKNO"         TO F-FIELDNAME
            MOVE 7                 TO F-CBFIELDNAME
            MOVE STTR-LY-STOCK-NUMBER TO F-NAMEFIELD
            MOVE 15                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PORDERNO"  TO F-FIELDNAME
            MOVE 8           TO F-CBFIELDNAME
            MOVE INCR-LY-PORDER TO F-NAMEFIELD
            MOVE 13          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "QTY"        TO F-FIELDNAME
            MOVE 3            TO F-CBFIELDNAME.
            MOVE 5            TO F-CBFIELDLENGTH
            IF STTR-LY-TYPE = 1
               MOVE STTR-LY-SHIPQTY TO F-EDNAMEFIELDQTY
               MOVE 5            TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-QTY
            ELSE
               MOVE STTR-LY-SHIPQTY TO WS-CREDQTY
               COMPUTE WS-CREDQTY = WS-CREDQTY * -1
               MOVE WS-CREDQTY   TO F-EDNAMEFIELDQTYCRED
               PERFORM WRITE-FIELD-QTYCRED.

            MOVE "PRICE"    TO F-FIELDNAME
            MOVE 5          TO F-CBFIELDNAME
            MOVE STTR-LY-PRICE TO F-EDNAMEFIELD99Mil
            MOVE 11         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil

            MOVE "ORDER"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME
            MOVE INCR-LY-BO-INV-NO TO F-EDNAMEFIELDNUM
            MOVE 6              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "ORDDATE"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE 8               TO F-CBFIELDLENGTH.
            IF STTR-LY-TYPE = 1
               MOVE INCR-LY-BO-DATE    TO SPLIT-DATE
               PERFORM CONVERT-DATE-FORMAT
               MOVE WS-CONVERT-DATE    TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE SPACES             TO F-NAMEFIELD
               PERFORM WRITE-FIELD-ALPHA.

            MOVE "INV"           TO F-FIELDNAME
            MOVE 3               TO F-CBFIELDNAME
            MOVE INCR-LY-INVOICE TO F-EDNAMEFIELDNUM
            MOVE 6               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-NUMERIC

            MOVE "INVDATE"       TO F-FIELDNAME
            MOVE 7               TO F-CBFIELDNAME
            MOVE INCR-LY-DATE    TO SPLIT-DATE
            PERFORM CONVERT-DATE-FORMAT
            MOVE WS-CONVERT-DATE TO F-NAMEFIELD
            MOVE 8               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

      *      MOVE "TOT-SHP"   TO F-FIELDNAME
      *      MOVE 7           TO F-CBFIELDNAME
      *      IF STTR-LY-TYPE = 1
      *         ADD STTR-LY-SHIPQTY        TO WS-SHIPQTY
      *      ELSE
      *         SUBTRACT STTR-LY-SHIPQTY FROM WS-SHIPQTY.
      *      MOVE 6           TO F-CBFIELDLENGTH
      *      MOVE WS-SHIPQTY  TO F-EDNAMEFIELDNUMNEG
      *      PERFORM WRITE-FIELD-NUMNEG.
       SCROLL-999.
             EXIT.
      *
       CLEAR-MEMORY SECTION.
       CMS-005.
            MOVE 1 TO SUB-1.
            MOVE 0 TO SUB-9.
       CMS-010.
            IF WS-STTR-TYPE (SUB-1) NOT = 0
                MOVE 0 TO WS-STTR-TYPE (SUB-1)
                          WS-STTR-REF (SUB-1)
                          WS-STTR-TRANS (SUB-1)
                          WS-STTR-REF2 (SUB-1)
            ELSE
                GO TO CMS-900.
            IF SUB-1 < 10000
               ADD 1 TO SUB-1
               GO TO CMS-010.
       CMS-900.
            MOVE 1 TO SUB-1.
       CMS-999.
            EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO F-INDEX.
       CLTR-010.
            IF F-INDEX > 15
                GO TO CLTR-999.
            MOVE " "       TO F-NAMEFIELD

            MOVE "STOCKNO" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE 15        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PORDERNO" TO F-FIELDNAME
            MOVE 8          TO F-CBFIELDNAME
            MOVE 13         TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "QTY" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE 5     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "PRICE" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE 11      TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ORDER" TO F-FIELDNAME
            MOVE 5       TO F-CBFIELDNAME
            MOVE 6       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "ORDDATE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "INV" TO F-FIELDNAME
            MOVE 3     TO F-CBFIELDNAME
            MOVE 6     TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            MOVE "INVDATE" TO F-FIELDNAME
            MOVE 7         TO F-CBFIELDNAME
            MOVE 10        TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            ADD 1 TO F-INDEX
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE "DEBTORS BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO OPEN-000.
       OPEN-003.
            OPEN I-O INCR-LY-REGISTER.
            IF WS-INCR-LY-ST1 NOT = 0
               MOVE "SL-REGISTER-LY BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-INCR-LY-ST1
               GO TO OPEN-003.
            GO TO OPEN-006.
       OPEN-005.
            OPEN I-O STOCK-TRANSLY-FILE.
            IF WS-STTRANSLY-ST1 NOT = 0
               MOVE "ST-TRANS-LY FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-STTRANSLY-ST1
               GO TO OPEN-005.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "SlInAcIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE DEBTOR-MASTER
                  INCR-LY-REGISTER.
       END-900.
            EXIT PROGRAM.
      *       STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldNumeric".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteField99Mil".
       Copy "WriteFieldQty".
       Copy "WriteFieldQtyCred".
       Copy "WriteFieldNumNeg".
       Copy "WriteFieldDate".
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
       Copy "Error1Message".
       Copy "ErrorMessage".
       Copy "CTOSCobolAccept".
      *                     
      * END-OF-JOB.
