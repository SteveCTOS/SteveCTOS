        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlTranIq.
        AUTHOR.    CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
        Copy "SelectGlTrans".
           SELECT PRINT-FILE ASSIGN TO WS-PRINTER
                ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlParam.
           COPY ChlfdGlTrans.
      *
       FD  PRINT-FILE.
       01  PRINT-REC.
           03  FILLER           PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  WS-INQUIRY-PROGRAM   PIC X(8) VALUE "GlMastIq".
       77  WS-CURRENTPER        PIC 99 VALUE 0.
       77  WS-DRTR-TYPE         PIC 99 VALUE 0.
       77  WS-NO-OF-TRANS       PIC 9(5) VALUE 0.
       77  WS-ANSWER            PIC X VALUE " ".
       77  WS-END               PIC X VALUE " ".
       77  WS-NEWINPUT          PIC X VALUE " ".
       77  WS-GLMASTERNUMBER    PIC X(15) VALUE " ".
       77  WS-DESC-SORT         PIC X(11) VALUE " ".
       77  WS-TRANS-AMT         PIC S9(8)V99 VALUE 0.
       77  PAGE-CNT             PIC 9(3) VALUE 0.
       77  LINE-CNT             PIC 9(2) VALUE 66.
       77  LINES-DISPLAYED      PIC 9(5) VALUE 0.
       77  WS-WORK-FIELD        PIC 9(5) VALUE 0.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1      PIC 99.
       01  WS-GLTRANS-STATUS.
           03  WS-GLTRANS-ST1     PIC 99.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1 PIC 99.
       01  WS-PERIOD.
           03  WS-1ST-CHAR        PIC X.
           03  WS-PER             PIC 99.
       01  WS-TRANS-TYPE.
           03  FILLER        PIC X(7) VALUE "INVOICE".
           03  FILLER        PIC X(7) VALUE "PAYMENT".
           03  FILLER        PIC X(7) VALUE "       ".
           03  FILLER        PIC X(7) VALUE "JRN-DR ".
           03  FILLER        PIC X(7) VALUE "JRN-CR ".
           03  FILLER        PIC X(7) VALUE "C/NOTE ".
           03  FILLER        PIC X(7) VALUE "INTRST ".
           03  FILLER        PIC X(7) VALUE "DISCNT ".
           03  FILLER        PIC X(7) VALUE "FOREX  ".
       01  WS-TRANS-DESCRED REDEFINES WS-TRANS-TYPE.
           03  WS-TRANS-DESC   PIC X(7) OCCURS 9.
       01  WS-GLNUMBER.
           03  WS-HEAD-SUB.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-RESTOFACCOUNT    PIC X(6).
       01  WS-GLDESCRIPTION.
           03  WS-DESC-INFO.
               05  WS-DESC-FILLER      PIC X(4).
               05  WS-DESC-ACC         PIC X(7).
           03  WS-DESC-REST            PIC X(14).
       01  HEAD1.
           03  FILLER         PIC X(7) VALUE "  DATE".
           03  H1-DATE        PIC X(10).
           03  FILLER         PIC X(28) VALUE " ".
           03  FILLER         PIC X(45) VALUE
           "** GLMASTER TRANSACTION INQUIRY **".
           03  FILLER         PIC X(7) VALUE "PERIOD:".
           03  H1-PERIOD      PIC X(23).
           03  FILLER         PIC X(5) VALUE "PAGE:".
           03  H1-PAGE        PIC Z9.
           03  FILLER         PIC X(5) VALUE " ".
       01  HEAD2.
           03  FILLER         PIC X(20) VALUE "ACCOUNT            :".
           03  H3-ACC         PIC X(112).
       01  HEAD2-1.
           03  FILLER         PIC X(20) VALUE "NAME               :".
           03  H3-NAME        PIC X(112).
       01  HEAD2-2.
           03  H3-DESC        PIC X(20).
           03  H3-BALANCE     PIC Z(7)9.99-.
           03  FILLER         PIC X(100) VALUE " ".
       01  HEAD2-3.
           03  FILLER         PIC X(20) VALUE "PERIOD READ        :".
           03  H3-PERIOD-READ PIC X(112).
       01  HEAD3.
           03  FILLER         PIC X(15) VALUE "PRD  TRANS#".
           03  FILLER         PIC X(13) VALUE "JRN No:".
           03  FILLER         PIC X(23) VALUE "TYPE        DATE".
           03  FILLER         PIC X(12) VALUE "DEBIT AMT".
           03  FILLER         PIC X(12) VALUE "CREDIT AMT".
           03  FILLER         PIC X(26) VALUE "DESCRIPTION".
           03  FILLER         PIC X(17) VALUE " ".
       01  DETAIL-LINE.
           03  D-PERIOD       PIC X(5).
           03  D-TRANS        PIC Z(5)9.
           03  FILLER         PIC X(4) VALUE " ".
           03  D-JRNNO        PIC X(10).
           03  FILLER         PIC X(1) VALUE " ".
           03  D-TYPE         PIC X(10).
           03  FILLER         PIC X VALUE " ".
           03  D-DATE         PIC X(10).
           03  FILLER         PIC X(2) VALUE " ".
           03  D-DRAMT        PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-CRAMT        PIC Z(7)9.99- BLANK WHEN ZERO.
           03  FILLER         PIC X(1) VALUE " ".
           03  D-DESC         PIC X(25).
           03  FILLER         PIC X(17) VALUE " ".
        01  TOTAL-LINE.
           03  FILLER         PIC X(22) VALUE " ".
           03  FILLER         PIC X(27) VALUE
             "TOTAL AMOUNT FOR PERIOD:".
           03  T-RANDAMT      PIC Z(7)9.99-.
           03  FILLER         PIC X(76) VALUE " ".
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
           DISPLAY "** GENERAL LEDGER TRANSACTION ENQUIRY PROGRAM **"
           AT POS
           MOVE 0410 TO POS
           DISPLAY "************************************************"
           AT POS.
       CONTROL-003.
           Copy "PrinterAccept".
           MOVE 2910 TO POS
           DISPLAY "Program now loading, Please be patient....." AT POS.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM DISPLAY-FORM-GL-TOP-INFO.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE "                               " TO F-NAMEFIELD.
            MOVE "N" TO WS-ANSWER.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            IF F-EXIT-CH = X"0C"
                 PERFORM READ-GLMASTER-NEXT
             IF WS-END = "Y"
                 GO TO GET-000
             ELSE
                 GO TO GET-010.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-GLMASTER-PREVIOUS
                 GO TO GET-010.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-NAMEFIELD = "   "
                CLOSE GL-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                PERFORM DISPLAY-FORM-GL-TOP-INFO
                GO TO GET-000.
           IF ALPHA-RATE > SPACES
                PERFORM NUMBER-CHECK.
           MOVE WS-GLNO-CHECK TO WS-GLNUMBER WS-GLMASTERNUMBER.
           IF WS-GLSUBHEADER = "    "
                MOVE "YOU CAN ONLY ENQUIRE ON A DETAIL ACCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-000.
           IF WS-RESTOFACCOUNT = "      "
                MOVE "YOU CAN ONLY ENQUIRE ON A DETAIL ACCOUNT."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-000.
            MOVE "ACCNO"     TO F-FIELDNAME.
            MOVE 5           TO F-CBFIELDNAME.
            MOVE WS-GLNUMBER TO F-NAMEFIELD.
            MOVE 12          TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            PERFORM READ-GLMASTER.
            GO TO GET-020.
       GET-010.
            MOVE "ACCNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE GL-NUMBER TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-020.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GL-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            IF GL-DESCRIPTION = "UNKNOWN"
                MOVE 2801 TO POS
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GL-BALANCE TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
       GET-025.
            MOVE "                               " TO F-NAMEFIELD.
            MOVE "GLDESCIQ" TO F-FIELDNAME.
            MOVE 8          TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11         TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO WS-DESC-SORT.
            IF F-EXIT-CH = X"01"
                 GO TO GET-000.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0C"
                     AND NOT = X"1F" AND NOT = X"05"
                  DISPLAY " " AT 3079 WITH BELL
                  GO TO GET-025.
       GET-030.
            MOVE "                               " TO F-NAMEFIELD.
            MOVE "READPERIOD" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"01"
                 GO TO GET-025.
            IF F-EXIT-CH = X"07"
                 GO TO GET-999.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0C"
                     AND NOT = X"1F" AND NOT = X"05"
                  DISPLAY " " AT 3079 WITH BELL
                  GO TO GET-030.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO WS-PER.
            IF WS-PER = 0
               MOVE "  "   TO F-NAMEFIELD
            ELSE
               MOVE WS-PER TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
            MOVE " " TO WS-1ST-CHAR.
            IF F-EXIT-CH = X"1F"
                 MOVE 3010 TO POS
                 DISPLAY "Printing In Progress, Please Be Patient."
                    AT POS
                 OPEN I-O GLTRANS-FILE
                 PERFORM PRINT-ROUTINE
                 PERFORM CLEAR-TRANSACTIONS
                 CLOSE GLTRANS-FILE
                 PERFORM ERROR1-020
                 PERFORM ERROR-020
                 GO TO GET-999.
       GET-040.
            MOVE " " TO F-EXIT-CH.

            PERFORM READ-ALL-TRANSACTIONS.
            MOVE "TOTTRANS"     TO F-FIELDNAME.
            MOVE 8              TO F-CBFIELDNAME.
            MOVE WS-NO-OF-TRANS TO F-EDNAMEFIELDCRED.
            MOVE 5              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-CRED.

            MOVE "TRANSAMT" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE WS-TRANS-AMT TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

           MOVE 0 TO LINES-DISPLAYED.
           IF WS-NO-OF-TRANS = 0
                GO TO GET-900.
           PERFORM READ-TRANSACTIONS.
       GET-900.
            IF WS-ANSWER = "Y"
               CLOSE GLTRANS-FILE
               GO TO GET-999.
            IF F-INDEX < 15
             IF F-INDEX NOT = X"07"
               PERFORM ERROR-020
               MOVE 2910 TO POS
               DISPLAY "Press 'ESC' To Clear The Screen." AT POS
               MOVE 3010 TO POS
               DISPLAY "Or Press 'F10' To Print All Transactions" AT POS
               MOVE 15 TO F-INDEX
               PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH NOT = X"07" AND NOT = X"1F" AND NOT = X"04"
                MOVE 1 TO F-INDEX
                GO TO GET-900.
            PERFORM ERROR1-020.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                PERFORM CLEAR-TRANSACTIONS
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO GET-999.
            IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                OPEN I-O GLTRANS-FILE
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                CLOSE GLTRANS-FILE
                PERFORM ERROR1-020
                PERFORM ERROR-020.
       GET-999.
            EXIT.
      *
       READ-TRANSACTIONS SECTION.
       RDTR-000.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS-ST1 ERROR IN OPENING, RDTR-000"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              CLOSE GLTRANS-FILE
              GO TO RDTR-000.
           MOVE 1 TO F-INDEX.
           MOVE "Y" TO WS-NEWINPUT.
       RDTR-005.
           MOVE WS-GLNUMBER TO GLTRANS-ACCOUNT-NUMBER.
           MOVE 0           TO GLTRANS-DATE.
           START GLTRANS-FILE KEY NOT < GLTRANS-ACC-DATE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
               MOVE "DESC" TO F-FIELDNAME
               MOVE 4      TO F-CBFIELDNAME
               MOVE 25     TO F-CBFIELDLENGTH
               MOVE 0      TO F-INDEX
               CLOSE GLTRANS-FILE
               GO TO RDTR-900.
       RDTR-010.
           IF F-EXIT-CH NOT = 1
            READ GLTRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
            READ GLTRANS-FILE PREVIOUS
               AT END NEXT SENTENCE.
           IF F-EXIT-CH = 1
            IF WS-GLTRANS-ST1 = 10
               MOVE 0 TO F-INDEX
               MOVE " " TO F-EXIT-CH
               CLOSE GLTRANS-FILE
               GO TO RDTR-000.
           IF F-EXIT-CH NOT = 1
            IF WS-GLTRANS-ST1 = 10
               MOVE 0 TO F-INDEX
               MOVE " " TO F-EXIT-CH
               CLOSE GLTRANS-FILE
               GO TO RDTR-900.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GL-TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO RDTR-010.

           IF F-EXIT-CH = 1
           IF GLTRANS-ACCOUNT-NUMBER NOT = GL-NUMBER
               MOVE 0 TO F-INDEX
               CLOSE GLTRANS-FILE
               MOVE " " TO F-EXIT-CH
               GO TO RDTR-000.
           IF F-EXIT-CH NOT = 1
           IF GLTRANS-ACCOUNT-NUMBER NOT = GL-NUMBER
               MOVE 0 TO F-INDEX
               CLOSE GLTRANS-FILE
               MOVE " " TO F-EXIT-CH
               GO TO RDTR-900.
           IF GLTRANS-FUTURE = "F"
               GO TO RDTR-010.
           IF WS-PER NOT = 0
            IF GLTRANS-NO NOT = WS-PER
              MOVE 2910 TO POS
              DISPLAY "Reading Next Trans For Period Selected..." AT POS
              GO TO RDTR-010.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.

           IF WS-DESC-SORT = " "
              GO TO RDTR-020.
           MOVE GLTRANS-LINE-DESC TO WS-GLDESCRIPTION.
           IF WS-DESC-SORT NOT = WS-DESC-INFO
              GO TO RDTR-010.
       RDTR-020. 
           IF F-INDEX > 15
                MOVE 2905 TO POS
                DISPLAY "Press 'PgDn' For More, OR 'PgUp' For Previous,"
                 AT POS
                ADD 47 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE 3010 TO POS
                DISPLAY "Or Press 'F10' To Print All Transactions"
                   AT POS
                ADD 41 TO POS
                DISPLAY "For This Account Number." AT POS
                MOVE 15 TO F-INDEX
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            PERFORM ERROR1-020
            PERFORM ERROR-020
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
                GO TO RDTR-900.
           IF F-EXIT-CH = X"1F"
                MOVE 3010 TO POS
                DISPLAY "Printing In Progress, Please Be Patient."
                   AT POS
                PERFORM PRINT-ROUTINE
                PERFORM CLEAR-TRANSACTIONS
                MOVE "Y" TO WS-ANSWER
                PERFORM ERROR-020
                GO TO RDTR-900.
           IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                    AND NOT = X"07" AND NOT = " " AND NOT = X"1F"
                    AND NOT = 1
                MOVE 16 TO F-INDEX
                GO TO RDTR-020.
           IF F-EXIT-CH NOT = 1
               ADD 1 TO LINES-DISPLAYED
           ELSE
               SUBTRACT 1 FROM LINES-DISPLAYED.
           IF F-EXIT-CH = 1
            IF LINES-DISPLAYED = 1
               MOVE 0 TO LINES-DISPLAYED.
           PERFORM SCROLLING.
           ADD 1 TO F-INDEX.
           GO TO RDTR-010.
       RDTR-900.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
       RDTR-999.
           EXIT.
      *
       READ-ALL-TRANSACTIONS SECTION.
       RALT-000.
           OPEN I-O GLTRANS-FILE.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GLTRANS-ST1 ERROR IN OPENING, RALT-000"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              CLOSE GLTRANS-FILE
              GO TO RALT-000.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
           MOVE 2910 TO POS.
           DISPLAY "Reading All transactions......" AT POS.
       RALT-005.
           MOVE 0 TO WS-NO-OF-TRANS WS-TRANS-AMT.
           MOVE GL-NUMBER TO GLTRANS-ACCOUNT-NUMBER.
           MOVE 0         TO GLTRANS-DATE.
           START GLTRANS-FILE KEY NOT < GLTRANS-ACC-DATE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 23 OR 35 OR 49
               GO TO RALT-900.
       RALT-010.
           READ GLTRANS-FILE NEXT
               AT END NEXT SENTENCE.
           IF WS-GLTRANS-ST1 = 10
               GO TO RALT-900.
           IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GL-TRANS BUSY ON READ-NEXT, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO RALT-010.
           IF GLTRANS-ACCOUNT-NUMBER NOT = GL-NUMBER
               GO TO RALT-900.
           IF GLTRANS-FUTURE = "F"
               GO TO RALT-010.
           IF WS-PER NOT = 0
            IF GLTRANS-NO NOT = WS-PER
               GO TO RALT-010.

           IF WS-DESC-SORT = " "
              GO TO RALT-020.
           MOVE GLTRANS-LINE-DESC TO WS-GLDESCRIPTION.
           IF WS-DESC-SORT NOT = WS-DESC-INFO
              GO TO RALT-010.
       RALT-020.
           ADD GLTRANS-AMOUNT TO WS-TRANS-AMT.
           ADD 1              TO WS-NO-OF-TRANS.
           GO TO RALT-010.
       RALT-900.
           PERFORM ERROR1-020.
           PERFORM ERROR-020.
           CLOSE GLTRANS-FILE.
       RALT-999.
           EXIT.
      *
       READ-GLMASTER SECTION.
       RD-010.
           MOVE WS-GLMASTERNUMBER TO GL-NUMBER.
           START GL-MASTER KEY NOT < GL-KEY.
       RD-015.
           READ GL-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49
               MOVE "UNKNOWN" TO GL-DESCRIPTION
               GO TO RD-999.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE"GLMASTER BUSY ON READ, RD-015, 'ESC' TO RETRY."
               TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO RD-015.
           MOVE GL-NUMBER TO WS-GLNUMBER.
       RD-999.
           EXIT.
      *
       START-GLMASTER SECTION.
       ST-ST-000.
              MOVE WS-GLMASTERNUMBER TO GL-NUMBER.
              START GL-MASTER KEY NOT < GL-NUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-GLMASTER-NEXT SECTION.
       R-ST-NX-000.
             MOVE "N" TO WS-END
             PERFORM ERROR-020.
       R-ST-NX-005. 
             READ GL-MASTER NEXT
                 AT END NEXT SENTENCE.
             IF WS-GLMAST-ST1 = 10
              MOVE "END OF NEXT-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM START-GLMASTER
                 MOVE "Y" TO WS-END
                 GO TO R-ST-NX-999.
             IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MASTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-GLMAST-ST1
                 PERFORM START-GLMASTER
                 GO TO R-ST-NX-005.
       R-ST-NX-010.
           MOVE GL-NUMBER TO WS-GLNUMBER.
           IF WS-GLSUBHEADER = "    "
                 GO TO R-ST-NX-005.
           IF WS-RESTOFACCOUNT = "      "
                 GO TO R-ST-NX-005.
           MOVE GL-NUMBER TO WS-GLNUMBER.
       R-ST-NX-999.
             EXIT.
      *
       READ-GLMASTER-PREVIOUS SECTION.
       RDPREV-000.
             MOVE 0 TO WS-GLMAST-ST1.
             PERFORM ERROR-020.
       RDPREV-005. 
             READ GL-MASTER PREVIOUS
                 AT END NEXT SENTENCE.
             IF WS-GLMAST-ST1 = 10
              MOVE "END OF NEXT-PAGE SEQUENCE, 'ESC' TO EXIT."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM START-GLMASTER
                 GO TO RDPREV-999.
             IF WS-GLMAST-ST1 NOT = 0
              MOVE "GL-MASTER BUSY ON READ-PREV, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR1-000
                 MOVE WS-GLMAST-ST1 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 PERFORM ERROR1-020
                 MOVE 0 TO WS-GLMAST-ST1
                 PERFORM START-GLMASTER
                 GO TO RDPREV-005.
       RDPREV-010.
           MOVE GL-NUMBER TO WS-GLNUMBER.
           IF WS-GLSUBHEADER = "    "
                 GO TO RDPREV-005.
           IF WS-RESTOFACCOUNT = "      "
                 GO TO RDPREV-005.
           MOVE GL-NUMBER TO WS-GLNUMBER.
       RDPREV-999.
             EXIT.
      *
       PRINT-ROUTINE SECTION.
       PRR-001.
            MOVE 0  TO PAGE-CNT WS-TRANS-AMT
            MOVE 66 TO LINE-CNT.
            PERFORM GET-USER-PRINT-NAME.
            OPEN OUTPUT PRINT-FILE.
            MOVE GL-NUMBER TO GLTRANS-ACCOUNT-NUMBER.
            MOVE 0         TO GLTRANS-DATE.
            START GLTRANS-FILE KEY NOT < GLTRANS-ACC-DATE
              INVALID KEY NEXT SENTENCE.
            IF WS-GLTRANS-ST1 NOT = 0
               GO TO PRR-900.
       PRR-002.
            READ GLTRANS-FILE NEXT
               AT END NEXT SENTENCE.
            IF WS-GLTRANS-ST1 = 10
               MOVE 0 TO WS-GLTRANS-ST1
               GO TO PRR-900.
            IF WS-GLTRANS-ST1 NOT = 0
              MOVE "GL-TRANS BUSY ON OPEN I-O, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLTRANS-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLTRANS-ST1
              GO TO PRR-002.
           IF GLTRANS-ACCOUNT-NUMBER < GL-NUMBER
               GO TO PRR-002.
           IF GLTRANS-ACCOUNT-NUMBER > GL-NUMBER
               GO TO PRR-900.
           IF GLTRANS-FUTURE = "F"
               GO TO PRR-002.
           IF WS-PER NOT = 0
            IF GLTRANS-NO NOT = WS-PER
               GO TO PRR-002.

           IF WS-DESC-SORT = " "
              GO TO PRR-010.
           MOVE GLTRANS-LINE-DESC TO WS-GLDESCRIPTION.
           IF WS-DESC-SORT NOT = WS-DESC-INFO
              GO TO PRR-002.
       PRR-010.
            IF LINE-CNT < 61
               GO TO PRR-020.
           PERFORM PRR-060.
       PRR-020.
           MOVE GLTRANS-PERIOD               TO D-PERIOD
           MOVE GLTRANS-TRANS                TO D-TRANS
           MOVE GLTRANS-REFERENCE            TO D-JRNNO
           MOVE WS-TRANS-DESC (GLTRANS-TYPE) TO D-TYPE
           MOVE GLTRANS-DATE                 TO SPLIT-DATE
           PERFORM CONVERT-DATE-FORMAT
           MOVE DISPLAY-DATE                 TO D-DATE.
           IF GLTRANS-AMOUNT < 0
              MOVE 0                         TO D-DRAMT
              MOVE GLTRANS-AMOUNT            TO D-CRAMT
           ELSE
              MOVE 0                         TO D-CRAMT
              MOVE GLTRANS-AMOUNT            TO D-DRAMT.
           MOVE GLTRANS-LINE-DESC            TO D-DESC
           WRITE PRINT-REC FROM DETAIL-LINE
           ADD GLTRANS-AMOUNT TO WS-TRANS-AMT
           MOVE " " TO PRINT-REC
           ADD 1 TO LINE-CNT
           GO TO PRR-002.
       PRR-060.
            ADD 1                   TO PAGE-CNT
            MOVE PAGE-CNT           TO H1-PAGE
            MOVE GLPA-CURRENT-GLPER TO H1-PERIOD.
           IF WS-PRINT-TYPE = 2
               MOVE WS-PRINT-COMP TO PRINT-REC
               WRITE PRINT-REC
               MOVE " " TO PRINT-REC.
            IF PAGE-CNT = 1
                WRITE PRINT-REC FROM COMPANY-LINE
            ELSE
                WRITE PRINT-REC FROM COMPANY-LINE AFTER PAGE.
            MOVE " "                TO PRINT-REC
            WRITE PRINT-REC FROM HEAD1 AFTER 1
            MOVE " "                TO PRINT-REC
            MOVE GL-NUMBER          TO H3-ACC
            WRITE PRINT-REC FROM HEAD2 AFTER 2
            MOVE " "                TO PRINT-REC
            MOVE GL-DESCRIPTION     TO H3-NAME
            WRITE PRINT-REC FROM HEAD2-1 AFTER 1
            MOVE " " TO PRINT-REC.
            MOVE "CURRENT BALANCE    :" TO H3-DESC
            MOVE GL-BALANCE             TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE "PERIOD OPEN BALANCE:" TO H3-DESC
            MOVE GL-OPEN-PER-BAL        TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE "YEAR OPEN BALANCE  :" TO H3-DESC
            MOVE GL-OPEN-YEAR-BAL       TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC
            MOVE "L/Y OPEN BALANCE   :" TO H3-DESC
            MOVE GL-LAST-YEAR-BAL       TO H3-BALANCE
            WRITE PRINT-REC FROM HEAD2-2 AFTER 1
            MOVE " "                    TO PRINT-REC.
            IF WS-PER = 0
                MOVE "ALL PERIODS"  TO H3-PERIOD-READ
            ELSE
                MOVE WS-PER         TO H3-PERIOD-READ.
            WRITE PRINT-REC FROM HEAD2-3 AFTER 1
            MOVE " "                TO PRINT-REC
            WRITE PRINT-REC
            WRITE PRINT-REC FROM HEAD3 AFTER 1
            MOVE " "                TO PRINT-REC
            WRITE PRINT-REC
            MOVE " "                TO PRINT-REC
            MOVE 11 TO LINE-CNT.
       PRR-900.
           IF LINE-CNT = 66
               PERFORM PRR-060.
           MOVE WS-TRANS-AMT TO T-RANDAMT
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1
           MOVE " "              TO PRINT-REC.

           IF WS-DESC-SORT NOT = " "
           MOVE "ONLY A SPECIFIC SUPPLIER NUMBER PRINTED." TO PRINT-REC
           WRITE PRINT-REC AFTER 1.

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
            IF F-INDEX < 1 
               MOVE 1  TO F-INDEX.
            MOVE "LINES"         TO F-FIELDNAME
            MOVE 5               TO F-CBFIELDNAME
            MOVE LINES-DISPLAYED TO F-EDNAMEFIELDCRED
            MOVE 5               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-CRED.

            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GLTRANS-PERIOD TO F-NAMEFIELD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "JRN" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE GLTRANS-REFERENCE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE WS-TRANS-DESC (GLTRANS-TYPE) TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE GLTRANS-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RAND" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLTRANS-AMOUNT TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE GLTRANS-LINE-DESC TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       SCROLL-999.
             EXIT.
      *
       CLEAR-TRANSACTIONS SECTION.
       CLTR-000.
            MOVE 1 TO SUB-1 F-INDEX.

            MOVE "LINES" TO F-FIELDNAME.
            MOVE 5       TO F-CBFIELDNAME.
            MOVE " "     TO F-NAMEFIELD.
            MOVE 5       TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       CLTR-010.
            IF SUB-1 > 15
                PERFORM ERROR1-020
                PERFORM ERROR-020
                GO TO CLTR-999.
            MOVE "PERIOD" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            MOVE " "      TO F-NAMEFIELD.
            MOVE 3        TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "JRN" TO F-FIELDNAME.
            MOVE 3     TO F-CBFIELDNAME.
            MOVE " "   TO F-NAMEFIELD.
            MOVE 10    TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TYPE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 7      TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 10     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "RAND" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 12     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC" TO F-FIELDNAME.
            MOVE 4      TO F-CBFIELDNAME.
            MOVE " "    TO F-NAMEFIELD.
            MOVE 25     TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            ADD 1 TO SUB-1 F-INDEX.
            GO TO CLTR-010.
       CLTR-999.
            EXIT.
      *
       READ-PARAMETER SECTION.
       RP-000.
           MOVE 1 TO GLPA-RECORD.
           READ GLPARAMETER-FILE
               INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               DISPLAY "NO PARAMETER RECORD!!!!"
               CALL "LOCKKBD" USING F-FIELDNAME
               EXIT PROGRAM.
           IF WS-GLPARAMETER-ST1 NOT = 0
              MOVE "PARAMETER BUSY ON READ, 'ESC' TO RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO RP-000.
       RP-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O GL-MASTER.
            IF WS-GLMAST-ST1 NOT = 0
              MOVE "GLMASTER BUSY ON OPEN, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLMAST-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLMAST-ST1
              GO TO OPEN-000.
       OPEN-005.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0 
              MOVE "GLPARAMETER FILE BUSY ON OPEN,  'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-GLPARAMETER-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-GLPARAMETER-ST1
              GO TO OPEN-005.
           PERFORM READ-PARAMETER.
           MOVE GLPA-NAME TO CO-NAME.
           PERFORM ENTER-PERIOD-DATES.
           PERFORM OPEN-006.
           CLOSE GLPARAMETER-FILE.
       OPEN-006.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE WS-DATE TO SPLIT-DATE.
           PERFORM CONVERT-DATE-FORMAT.
           MOVE DISPLAY-DATE TO H1-DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "GlTranIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE GL-MASTER.
       END-900.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldChange".
       Copy "WriteFieldCred".
       Copy "WriteFieldRec".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
       Copy "DisplayFormGLTopInfo".
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
      * END-OF-JOB.
