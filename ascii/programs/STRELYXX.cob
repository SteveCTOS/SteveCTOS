        IDENTIFICATION DIVISION.
        PROGRAM-ID. STRELYXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STKRECEIPTSLY-FILE ASSIGN TO
                              "StReceiptLy"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STRELY-KEY
               ALTERNATE RECORD KEY IS
                          STRELY-STOCK-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS 
                          STRELY-REFERENCE-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS
                          STRELY-ORDER-NUMBER WITH DUPLICATES
               FILE STATUS IS WS-RECE-STATUS.
           SELECT STKRECEIPTSLY-ASCII ASSIGN TO
                     "StReceiptLyASCII"
               FILE STATUS IS WS-RECE-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStkReceiptsLy.
           COPY ChlfdStkReceiptsLyASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-RECE-STATUS.
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
           DISPLAY "** STRECEIPTLYSLY EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT STKRECEIPTSLY-FILE.
           IF WS-ACCEPT = "E"
               MOVE 0 TO STRELY-KEY
              START STKRECEIPTSLY-FILE KEY NOT < STRELY-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND STKRECEIPTSLY-ASCII
           ELSE
              OPEN INPUT STKRECEIPTSLY-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ STKRECEIPTSLY-FILE NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY STRELY-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE STOCK-RECEIPTSLY-REC    TO ASCII-REC.
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
           READ STKRECEIPTSLY-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-REC    TO STOCK-RECEIPTSLY-REC.
        BI-010.
           WRITE STOCK-RECEIPTSLY-REC
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
           CLOSE STKRECEIPTSLY-FILE
                 STKRECEIPTSLY-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
