        IDENTIFICATION DIVISION.
        PROGRAM-ID. CBTRANXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CBTRANS-FILE ASSIGN TO "CbTrans"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CBTRANS-KEY
               ALTERNATE RECORD KEY IS CBTRANS-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS CBTRANS-PERIOD WITH DUPLICATES
               FILE STATUS IS WS-CBTRANS-STATUS.
           SELECT CBTRANS-ASCII ASSIGN TO "CbTransASCII"
               FILE STATUS IS WS-CBTRANS-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCbTrans.
           COPY ChlfdCbTransASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-CBTRANS-STATUS.
               03  WS-STAT1  PIC 9.
               03  WS-STAT2  PIC 9.     
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
           DISPLAY "** CBTRANS EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT CBTRANS-FILE.
           IF WS-ACCEPT = "E"
              MOVE 0 TO CBTRANS-KEY
              START CBTRANS-FILE KEY NOT < CBTRANS-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND CBTRANS-ASCII
           ELSE
              OPEN INPUT CBTRANS-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ CBTRANS-FILE NEXT
               AT END 
               DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY CBTRANS-DATE.
           
           ADD 1 TO WS-COUNT.

           MOVE CBTRANS-REC    TO ASCII-REC.
        BE-010.
      *     WRITE ASCII-REC
      *           INVALID KEY
             DISPLAY "INVALID WRITE FOR ASCII FILE...."
             DISPLAY WS-STAT1
             DISPLAY WS-STAT2
             STOP RUN.
             GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ CBTRANS-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-REC    TO CBTRANS-REC.
        BI-010.
           WRITE CBTRANS-REC
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             DISPLAY WS-STAT2
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE CBTRANS-FILE
                 CBTRANS-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
