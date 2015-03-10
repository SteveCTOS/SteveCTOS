        IDENTIFICATION DIVISION.
        PROGRAM-ID. CRCATRXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CR-CAMS-TRANS-FILE ASSIGN TO 
                               "CrCamsTrans"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CR-CAMS-TRANS-KEY
               FILE STATUS IS WS-CR-CAMS-TRANS-STATUS.
           SELECT CR-CAMS-TRANS-ASCII ASSIGN TO 
                     "CrCamsTransASCII"
               FILE STATUS IS WS-CR-CAMS-TRANS-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrCAMSTrans.
           COPY ChlfdCrCAMSTransASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-MESSAGE    PIC X(60) VALUE " ".
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-CR-CAMS-TRANS-STATUS.
               03  WS-STAT1  PIC 99.
      *
        PROCEDURE DIVISION.
        CONTROL-PARAGRAPH SECTION.
           PERFORM A-ACCEPT.
           PERFORM A-INIT.
           IF WS-ACCEPT = "E"
               PERFORM B-EXPORT
           ELSE
               PERFORM B-IMPORT.
          PERFORM C-END.
           STOP RUN.
        CONTROL-000.
           EXIT. 
      *
       A-ACCEPT SECTION.
       A-001.
           MOVE 0810 TO POS.
           DISPLAY "** CR-CAMS-TRANS EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY "ENTER E=EXPORT TO ASCII, I=IMPORT FROM ASCII: [ ]"
              AT POS
           MOVE 1057 TO POS
           ACCEPT WS-ACCEPT AT POS.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-001.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           OPEN OUTPUT CR-CAMS-TRANS-FILE.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
              MOVE 0 TO CR-CAMS-TRANS-NUM
              START CR-CAMS-TRANS-FILE KEY NOT < CR-CAMS-TRANS-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND CR-CAMS-TRANS-ASCII
           ELSE
              OPEN INPUT CR-CAMS-TRANS-ASCII.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ CR-CAMS-TRANS-FILE NEXT
               AT END 
               DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY CR-CAMS-TRANS-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE CR-CAMS-TRANS-REC    TO ASCII-REC.
        BE-010.
      *     WRITE ASCII-REC
      *           INVALID KEY
             DISPLAY "INVALID WRITE FOR ASCII FILE...."
             DISPLAY WS-STAT1
             STOP RUN.
             GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ CR-CAMS-TRANS-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE AT 1505
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.

           MOVE ASCII-REC    TO CR-CAMS-TRANS-REC.
        BI-010.
           WRITE CR-CAMS-TRANS-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             CLOSE CR-CAMS-TRANS-FILE
                   CR-CAMS-TRANS-ASCII
             CALL "C$SLEEP" USING 3
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE CR-CAMS-TRANS-FILE
                 CR-CAMS-TRANS-ASCII.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
