        IDENTIFICATION DIVISION.
        PROGRAM-ID. DRMASTXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT DEBTOR-MASTER ASSIGN TO "DrMaster"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DR-KEY
               ALTERNATE RECORD KEY IS DR-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS 
                             DR-SALES-YTD WITH DUPLICATES
               ALTERNATE RECORD KEY IS 
                             DR-SALES-LAST WITH DUPLICATES 
               ALTERNATE RECORD KEY IS DR-DISCOUNT-CODE WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR-SUPPLY-Y-N WITH DUPLICATES
               ALTERNATE RECORD KEY IS DR-SALESMAN WITH DUPLICATES
               ALTERNATE RECORD KEY IS 
                             DR-CORPORATE-GROUP WITH DUPLICATES
               FILE STATUS IS WS-DEBTOR-STATUS.
           SELECT DEBTOR-ASCII ASSIGN TO "DrMasterASCII"
               FILE STATUS IS WS-DEBTOR-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           COPY ChlfdDebtorASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-DEBTOR-STATUS.
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
           DISPLAY "** DEBTOR EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT DEBTOR-MASTER.
           IF WS-ACCEPT = "E"
               MOVE 0 TO DR-ACCOUNT-NUMBER
              START DEBTOR-MASTER KEY NOT < DR-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND DEBTOR-ASCII
           ELSE
              OPEN INPUT DEBTOR-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ DEBTOR-MASTER NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY DR-ACCOUNT-NUMBER.
           
           ADD 1 TO WS-COUNT.

           MOVE DEBTOR-RECORD    TO ASCII-RECORD.
        BE-010.
      *     WRITE ASCII-RECORD
      *           INVALID KEY
             DISPLAY "INVALID WRITE FOR ASCII FILE...."
             DISPLAY WS-STAT1
             DISPLAY WS-STAT2
             STOP RUN.
      *     IF WS-COUNT < 500
             GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ DEBTOR-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-RECORD    TO DEBTOR-RECORD.
        BI-010.
           WRITE DEBTOR-RECORD
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
           CLOSE DEBTOR-MASTER
                 DEBTOR-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
