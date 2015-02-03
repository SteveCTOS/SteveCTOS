        IDENTIFICATION DIVISION.
        PROGRAM-ID. CBTRANXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CRTR-FILE ASSIGN TO "CrTrans"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRTR-KEY
               ALTERNATE RECORD KEY IS CRTR-PERIOD WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR-ACC-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR-REFERENCE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR-INV-NO WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRTR-DNOTE-NO WITH DUPLICATES
               FILE STATUS IS WS-CRTR-STATUS.
           SELECT CRTR-ASCII ASSIGN TO "CrTransASCII"
               FILE STATUS IS WS-CRTR-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrTrans.
           COPY ChlfdCrTransASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-CRTR-STATUS.
               03  WS-STAT1  PIC X.
               03  WS-STAT2  PIC X.     
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
           DISPLAY "** CRTRANS EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT CRTR-FILE.
           IF WS-ACCEPT = "E"
              MOVE " " TO CRTR-KEY
              START CRTR-FILE KEY NOT < CRTR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND CRTR-ASCII
           ELSE
              OPEN INPUT CRTR-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ CRTR-FILE NEXT
               AT END 
               DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY CRTR-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE CRTR-REC    TO ASCII-REC.
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
           READ CRTR-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-REC    TO CRTR-REC.
        BI-010.
           WRITE CRTR-REC
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
           CLOSE CRTR-FILE
                 CRTR-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
