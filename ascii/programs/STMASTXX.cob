        IDENTIFICATION DIVISION.
        PROGRAM-ID. StMastXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STOCK-MASTER ASSIGN TO "[WIN]<DATA>STMASTER"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ST-KEY
               ALTERNATE RECORD KEY IS ST-ALT-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS ST-SUPPLIER WITH DUPLICATES
               FILE STATUS IS WS-STOCK-STATUS.
           SELECT STOCK-ASCII ASSIGN TO "[WIN]<ASCII>STMASTERASCII"
               FILE STATUS IS WS-STOCK-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "/main/ctos/source/cobol/fd/ChlfdStock".
           COPY CHLFDSTOCKASCII.
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
           DISPLAY "** STOCK EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN I-O STOCK-MASTER.
           IF WS-ACCEPT = "E"
               MOVE " " TO ST-STOCKNUMBER
              START STOCK-MASTER KEY NOT < ST-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND STOCK-ASCII
           ELSE
              OPEN INPUT STOCK-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ STOCK-MASTER NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY ST-STOCKNUMBER.
           
           ADD 1 TO WS-COUNT.

           MOVE STOCK-RECORD    TO ASCII-RECORD.
        BE-010.
           WRITE ASCII-RECORD
                 INVALID KEY
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
           READ STOCK-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-RECORD    TO STOCK-RECORD.
        BI-010.
           WRITE STOCK-RECORD
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
           CLOSE STOCK-MASTER
                 STOCK-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
