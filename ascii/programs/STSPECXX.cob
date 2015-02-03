        IDENTIFICATION DIVISION.
        PROGRAM-ID. STSPECXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STPR-MASTER ASSIGN TO "StSpecPr"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STPR-KEY
               FILE STATUS IS WS-STOCK-STATUS.
           SELECT STPR-ASCII ASSIGN TO "StSpecPrASCII"
               FILE STATUS IS WS-STOCK-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStPrice.
           COPY ChlfdStPriceASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-STOCK-STATUS.
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
           DISPLAY "** ST-SPECIAL EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT STPR-MASTER.
           IF WS-ACCEPT = "E"
               MOVE " " TO STPR-STOCKNUMBER
              START STPR-MASTER KEY NOT < STPR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND STPR-ASCII
           ELSE
              OPEN INPUT STPR-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ STPR-MASTER NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY STPR-STOCKNUMBER.
           
           ADD 1 TO WS-COUNT.

           MOVE STPR-RECORD    TO ASCII-RECORD.
        BE-010.
      *     WRITE ASCII-RECORD
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
           READ STPR-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-RECORD    TO STPR-RECORD.
        BI-010.
           WRITE STPR-RECORD
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
           CLOSE STPR-MASTER
                 STPR-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
      
