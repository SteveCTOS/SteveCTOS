        IDENTIFICATION DIVISION.
        PROGRAM-ID. CBJRNLXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CRJRN-FILE ASSIGN TO "CrJrn"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRJRN-KEY
               ALTERNATE RECORD KEY IS 
                               CRJRN-CRACC-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRJRN-INV-NO WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRJRN-DNOTE-NO WITH DUPLICATES
               FILE STATUS IS WS-CRJRN-STATUS.
           SELECT CRJRN-ASCII ASSIGN TO "CrJrnASCII"
               FILE STATUS IS WS-CRJRN-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdCrJrn.
           COPY ChlfdCrJrnASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-CRJRN-STATUS.
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
           DISPLAY "** CRJRN EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT CRJRN-FILE.
           IF WS-ACCEPT = "E"
              MOVE " " TO CRJRN-KEY
              START CRJRN-FILE KEY NOT < CRJRN-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND CRJRN-ASCII
           ELSE
              OPEN INPUT CRJRN-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ CRJRN-FILE NEXT
               AT END 
               DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY CRJRN-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE CRJRN-REC    TO ASCII-REC.
        BE-010.
      *      WRITE ASCII-REC
      *            INVALID KEY
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
           READ CRJRN-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-REC    TO CRJRN-REC.
        BI-010.
           WRITE CRJRN-REC
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
           CLOSE CRJRN-FILE
                 CRJRN-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
