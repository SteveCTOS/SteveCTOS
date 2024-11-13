        IDENTIFICATION DIVISION.
        PROGRAM-ID. StRecLMv.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT STOCK-RECEIPTS-REC ASSIGN TO Ws-StReceipt
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS           STRE-KEY
               ALTERNATE RECORD KEY IS STRE-STOCK-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS STRE-REFERENCE-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS STRE-ORDER-NUMBER WITH DUPLICATES
                     FILE STATUS IS WS-STRE-STATUS.
                     
           SELECT STOCK1-RECEIPTS-REC ASSIGN TO Ws-StReceiptTemp
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS           STRE1-KEY
               ALTERNATE RECORD KEY IS STRE1-STOCK-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS STRE1-REFERENCE-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS STRE1-ORDER-NUMBER WITH DUPLICATES
                     FILE STATUS IS WS-STRE-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdStkReceipts".
           COPY "ChlfdStkReceipts1".
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-COUNT      PIC 9(6) VALUE 0.
           01  WS-CHECK-ST.
               03  WS-CHECK-1    PIC X.
               03  WS-CHECK-BAL  PIC X(14).
           01  WS-STRE-STATUS.
               03  WS-STAT1      PIC 99.
       Copy "WsDateInfo".
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
        PROCEDURE DIVISION Using Ws-Linkage.
        CONTROL-PARAGRAPH SECTION.
           PERFORM CLEAR-SCREEN.
           PERFORM A-ACCEPT.
           PERFORM A-INIT.
           IF WS-ACCEPT = "E"
               PERFORM B-EXPORT
           ELSE
               PERFORM B-IMPORT.
          PERFORM C-END.
          EXIT PROGRAM.
      *     STOP RUN.
        CONTROL-000.
           EXIT. 
      *
       A-ACCEPT SECTION.
       A-AC001.
           MOVE 0820 TO POS.
           DISPLAY "** ST-RECEIPT EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY 
           "ENTER E=EXPORT TO StReceiptTemp, I=IMPORT FROM Temp: [ ]"
              AT POS
           MOVE 1110 TO POS
           DISPLAY "OR ENTER X=EXIT THE PROGRAM." AT POS
           MOVE 1062 TO POS

           MOVE ' '       TO CDA-DATA.
           MOVE 1         TO CDA-DATALEN.
           MOVE 7         TO CDA-ROW.
           MOVE 61        TO CDA-COL.
           MOVE CDA-WHITE TO CDA-COLOR.
           MOVE 'F'       TO CDA-ATTR.
           PERFORM CTOS-ACCEPT.
           MOVE CDA-DATA TO WS-ACCEPT.

            IF W-ESCAPE-KEY = 4
               EXIT PROGRAM.
            IF W-ESCAPE-KEY = 0 OR 1 OR 2 OR 5
               GO TO A-AC010
            ELSE
               DISPLAY " " AT 3079 WITH BELL
               GO TO A-AC001.
        A-AC010.
      *     DISPLAY WS-ACCEPT AT 1075.
           IF WS-ACCEPT = "X"
              EXIT PROGRAM.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-AC001.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           MOVE Ws-StReceipt TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           MOVE Ws-StReceiptTemp TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "I"
               OPEN OUTPUT STOCK-RECEIPTS-REC
           ELSE
               OPEN I-O STOCK-RECEIPTS-REC.
           
      *     MOVE WS-ACCEPT TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO STRE-KEY
              START STOCK-RECEIPTS-REC KEY NOT < STRE-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN OUTPUT STOCK1-RECEIPTS-REC
           ELSE
              OPEN I-O STOCK1-RECEIPTS-REC.
           
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
            IF WS-STAT1 NOT = 0
               MOVE "EXCLUDING TEMP FILE FOR THIS COMPANY" TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM C-END
               EXIT PROGRAM.
      *         STOP RUN.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ STOCK-RECEIPTS-REC NEXT WITH LOCK
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY STRE-KEY  AT 1505
           DISPLAY STRE-REFERENCE-DATE AT 1520
           ADD 1 TO WS-COUNT
           DISPLAY  WS-COUNT AT 2510.

           MOVE STOCK-RECEIPT-REC    TO STOCK1-RECEIPT-REC.
      *     MOVE STRE-COMPLETE      TO STRE1-COMPLETE
      *                                STRE1-AC-COMPLETE
      *                                STRE1-ST-COMPLETE.
        BE-010.
           WRITE STOCK1-RECEIPT-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM-EXPORT FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE STRE1-KEY TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             ADD 1 TO STRE-TRANSACTION-NUMBER
                      STRE-REFERENCE1
             START STOCK-RECEIPTS-REC KEY > STRE-KEY.
                      
      *       DISPLAY "INVALID WRITE FOR ASCII FILE...."
      *       DISPLAY WS-STAT1
      *         EXIT PROGRAM.
      *       STOP RUN.

           GO TO BE-005.
        BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ STOCK1-RECEIPTS-REC NEXT WITH LOCK
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY STRE1-KEY AT 1505
           DISPLAY STRE1-REFERENCE-DATE AT 1520
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           MOVE STOCK1-RECEIPT-REC    TO STOCK-RECEIPT-REC.
        BI-010.
           WRITE STOCK-RECEIPT-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM1 FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
      *       DISPLAY "INVALID WRITE FOR ISAM FILE..."
      *       DISPLAY WS-STAT1
      *       CLOSE STOCK-RECEIPTS-REC
      *             STOCK1-RECEIPTS-REC
      *       CALL "C$SLEEP" USING 3
      *         EXIT PROGRAM.
      *       STOP RUN.
      
      *      MOVE SPACES TO STOCK-RECEIPT-REC.
            
            GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE STOCK-RECEIPTS-REC
                 STOCK1-RECEIPTS-REC.
           MOVE "FINISHED, CLOSING AND EXIT" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
        C-EXIT.
           EXIT.
      *
       Copy "MenuClearScreen".
       Copy "CTOSCobolAccept".
       COPY "ErrorMessage".
       COPY "Error1Message".
      * END-OF-JOB.
