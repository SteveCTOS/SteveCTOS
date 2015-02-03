        IDENTIFICATION DIVISION.
        PROGRAM-ID. GLTRANXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT GLTRANS-FILE ASSIGN TO "GlTrans"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GLTRANS-KEY
               ALTERNATE RECORD KEY IS GLTRANS-PERIOD WITH DUPLICATES
               ALTERNATE RECORD KEY IS GLTRANS-ACC-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS GLTRANS-NO WITH DUPLICATES
               FILE STATUS IS WS-GLTRANS-STATUS.
           SELECT GLTRANS-ASCII ASSIGN TO "GlTransASCII"
               FILE STATUS IS WS-GLTRANS-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlTrans.
           COPY ChlfdGlTransASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-GLTRANS-STATUS.
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
           DISPLAY "** GLTRANS EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT GLTRANS-FILE.
           IF WS-ACCEPT = "E"
               MOVE " " TO GLTRANS-KEY
              START GLTRANS-FILE KEY NOT < GLTRANS-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND GLTRANS-ASCII
           ELSE
              OPEN INPUT GLTRANS-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ GLTRANS-FILE NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY GLTRANS-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE GLTRANS-REC    TO ASCII-REC.
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
           READ GLTRANS-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-REFERENCE.

           MOVE ASCII-REFERENCE       TO GLTRANS-REFERENCE.
           MOVE ASCII-TRANS           TO GLTRANS-TRANS.
           MOVE ASCII-TYPE            TO GLTRANS-TYPE.
           MOVE ASCII-FUTURE          TO GLTRANS-FUTURE.
           MOVE ASCII-NO              TO GLTRANS-NO.
           MOVE ASCII-DATE            TO GLTRANS-DATE.
           MOVE ASCII-ACCOUNT-NUMBER  TO GLTRANS-ACCOUNT-NUMBER.
           MOVE ASCII-AMOUNT          TO GLTRANS-AMOUNT.
           MOVE ASCII-LINE-DESC       TO GLTRANS-LINE-DESC.
        BI-010.
           WRITE GLTRANS-REC
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
           CLOSE GLTRANS-FILE
                 GLTRANS-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
