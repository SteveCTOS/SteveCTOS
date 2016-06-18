        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlNameIq.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
      *
        DATA DIVISION.
        FILE SECTION.
            COPY ChlfdGlMast.

       WORKING-STORAGE SECTION.
       77  WS-SHORTNAME         PIC X(10) VALUE " ".
       77  WS-WORK              PIC X(25) VALUE " ".
       77  WS-SUB1              PIC 9(4) VALUE 0.
       77  WS-ACC               PIC X(12).
       77  WS-1ST               PIC X VALUE " ".
       77  WS-MIDDLE            PIC X(79) VALUE " ".
       01  WS-LINE-PRINT.
           03  WS-NAME          PIC X(42) VALUE " ".
           03  WS-TYPE          PIC X(44) VALUE " ".
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1    PIC 99.
       01  WS-GLACC.
           03  WS-SUBHEADER.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-REST             PIC X(6).
       01  WS-SPLIT-ACCOUNT.
           03  WS-SP-1          PIC X VALUE " ".
           03  WS-SP-REST       PIC X(24) VALUE " ".
       01  WS-SPLIT-INPUT-ACC.
           03  WS-SP-I-1        PIC X VALUE " ".
           03  WS-SP-I-REST     PIC X(9) VALUE " ".
       Copy "WsDateInfo".
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
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM READ-MASTER-DISPLAY.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO WS-SPLIT-INPUT-ACC
                        WS-SPLIT-ACCOUNT.
            MOVE "Y" TO WS-1ST.
            MOVE "SHORTNAME" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            MOVE 12          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-000.
            
            PERFORM ERROR-020.
            MOVE SPACES TO ALPHA-RATE.
            IF F-EXIT-CH = X"1D"
               MOVE F-NAMEFIELD TO ALPHA-RATE
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO GL-NUMBER WS-ACC
               MOVE "SHORTNAME" TO F-FIELDNAME
               MOVE 9           TO F-CBFIELDNAME
               MOVE WS-ACC      TO F-NAMEFIELD
               MOVE 12          TO F-CBFIELDLENGTH
               PERFORM WRITE-FIELD-ALPHA
            ELSE
               MOVE F-NAMEFIELD TO WS-SHORTNAME.
      *     MOVE 0           TO F-EXIT-CH.
       GET-999.
           EXIT.
      *
       READ-MASTER-DISPLAY SECTION.
       READ-000.
            PERFORM OPEN-000.
            IF F-EXIT-CH = X"0A"
                MOVE WS-SHORTNAME TO GL-DESCRIPTION WS-SPLIT-INPUT-ACC
               START GL-MASTER KEY NOT < GL-DESCRIPTION
                  INVALID KEY NEXT SENTENCE.
            IF F-EXIT-CH = X"1D"
               MOVE WS-ACC       TO GL-NUMBER
               START GL-MASTER KEY NOT < GL-KEY
                   INVALID KEY NEXT SENTENCE.
                   
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MASTER BUSY ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               CLOSE GL-MASTER
               GO TO READ-999.

            MOVE 0 TO F-EXIT-CH.
            MOVE 0 TO SUB-2 SUB-3.
            MOVE 800 TO WS-SUB1.
        READ-010.
            IF F-EXIT-CH = 0
              READ GL-MASTER NEXT
                AT END
                MOVE "END OF FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM CLEAR-MIDDLE
                CLOSE GL-MASTER
                GO TO READ-999.
            IF F-EXIT-CH = 1
              READ GL-MASTER PREVIOUS.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GL-MASTER BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO READ-010.
      *          PERFORM CLEAR-MIDDLE
      *          CLOSE GL-MASTER
      *          GO TO READ-999.
            IF WS-1ST = "Y"
              IF GL-DESCRIPTION NOT = " "
                MOVE GL-DESCRIPTION TO WS-SPLIT-INPUT-ACC
                MOVE "N" TO WS-1ST.
            MOVE GL-DESCRIPTION TO WS-SPLIT-ACCOUNT.
        READ-020.
            ADD 1 TO SUB-3.
        READ-025.
            IF SUB-3 > 20
                MOVE 2910 TO POS
             DISPLAY "Press 'PgDn' For More, Or 'PgUp' to See Previous,"
                 AT POS
                MOVE 3020 TO POS
                DISPLAY "'ESC' To Clear The Screen !" AT POS
                MOVE " " TO WS-SHORTNAME
                PERFORM USER-FILL-FIELD.
            PERFORM ERROR1-020
            PERFORM ERROR-020.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTNAME
                MOVE 0 TO WS-GLMAST-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE GL-MASTER
                GO TO READ-999.
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                GO TO READ-025.
            IF GL-NUMBER = 0
                GO TO READ-010.
      *      DISPLAY "�AF" AT WS-SUB1.
            ADD 1 TO WS-SUB1.
            DISPLAY GL-NUMBER AT WS-SUB1 WITH REVERSE-VIDEO.
            ADD 16 TO WS-SUB1.
      *      DISPLAY "�AA" AT WS-SUB1.
            ADD 1 TO WS-SUB1.

            MOVE GL-DESCRIPTION  TO WS-NAME.
            IF GL-P-B = "P"
               MOVE "PROFIT & LOSS" TO WS-TYPE
            ELSE
               MOVE "BALANCE SHEET" TO WS-TYPE.
            DISPLAY WS-LINE-PRINT AT WS-SUB1.
            ADD 82 TO WS-SUB1.
            MOVE " " TO WS-LINE-PRINT.
            DISPLAY WS-LINE-PRINT AT WS-SUB1.

            GO TO READ-010.
        READ-999.
            EXIT.
      *
       CLEAR-MIDDLE SECTION.
       CM-010.
           MOVE " " TO WS-MIDDLE.
           MOVE 801 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 901 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1001 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1101 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1201 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1301 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1401 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1501 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1601 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1701 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1801 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1901 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2001 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2101 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2201 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2301 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2401 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2501 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2601 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2701 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2801 TO POS
           DISPLAY WS-MIDDLE AT POS.
       CM-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-0000.
            GO TO OPEN-010.
       OPEN-000.
            OPEN I-O GL-MASTER.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMaster BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-GLMAST-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-GLMAST-ST1
               GO TO OPEN-000.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "GlNameIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
          CLOSE GL-MASTER.
      *     STOP RUN.
          EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "NumberCheck".
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
      *
      * END-OF-JOB
