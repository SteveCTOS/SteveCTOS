        IDENTIFICATION DIVISION.
        PROGRAM-ID. STTRLYXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STOCK-TRANSLY-FILE ASSIGN TO "StTransLy"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STTR-LY-KEY
               ALTERNATE RECORD KEY IS STTR-LY-ST-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS STTR-LY-AC-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS STTR-LY-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS
                               STTR-LY-COMPLETE WITH DUPLICATES
               ALTERNATE RECORD KEY IS
                               STTR-LY-STOCK-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS 
                               STTR-LY-ACCOUNT-NUMBER WITH DUPLICATES
               FILE STATUS IS WS-STTR-LY-STATUS.
           SELECT STOCK-TRANSLY-ASCII ASSIGN TO
                           "StTransLyASCII"
               FILE STATUS IS WS-STTR-LY-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStTransLy.
           COPY ChlfdStTransLyASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-CHECK-ST.
               03  WS-CHECK-1    PIC X.
               03  WS-CHECK-BAL  PIC X(14).
           01  WS-STTR-LY-STATUS.
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
           DISPLAY "** ST-TRANSLY EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT STOCK-TRANSLY-FILE.
           IF WS-ACCEPT = "E"
               MOVE 0 TO STTR-LY-KEY
              START STOCK-TRANSLY-FILE KEY NOT < STTR-LY-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND STOCK-TRANSLY-ASCII
           ELSE
              OPEN INPUT STOCK-TRANSLY-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ STOCK-TRANSLY-FILE NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY STTR-LY-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE STOCK-TRANSLY-REC    TO ASCII-REC.
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
           READ STOCK-TRANSLY-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
      *     DISPLAY ASCII-MESSAGE.
               
           DISPLAY ASCII-KEY.
           
           MOVE ASCII-KEY            TO STTR-LY-KEY.
           MOVE ASCII-ST-KEY         TO STTR-LY-ST-KEY WS-CHECK-ST
           MOVE ASCII-AC-KEY         TO STTR-LY-AC-KEY
           MOVE ASCII-INV-NO         TO STTR-LY-INV-NO
           MOVE ASCII-DATE           TO STTR-LY-DATE
           MOVE ASCII-COMPLETE       TO STTR-LY-COMPLETE.
           
           IF WS-CHECK-1 = "*"
               MOVE ASCII-BALANCE    TO COMMENT-LY-FIELDS
           ELSE 
               MOVE ASCII-BALANCE    TO DATA-LY-FIELDS.
           

       *    MOVE ASCII-REC    TO STOCK-TRANSLY-REC.
        BI-010.
           WRITE STOCK-TRANSLY-REC
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
           CLOSE STOCK-TRANSLY-FILE
                 STOCK-TRANSLY-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
