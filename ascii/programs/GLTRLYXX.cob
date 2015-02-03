        IDENTIFICATION DIVISION.
        PROGRAM-ID. GLTRANXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT GLTRANS-LY-FILE ASSIGN TO "GlTransLy"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GLTRANS-LY-KEY
               ALTERNATE RECORD KEY IS GLTRANS-LY-NO WITH DUPLICATES
               ALTERNATE RECORD KEY IS 
                                 GLTRANS-LY-ACC-DATE WITH DUPLICATES
               FILE STATUS IS WS-GLTRANS-LY-STATUS.
           SELECT GLTRANS-LY-ASCII ASSIGN TO 
                    "GlTransLyASCII"
               FILE STATUS IS WS-GLTRANS-LY-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlTransLy.
           COPY ChlfdGlTransLyASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           01  WS-GLTRANS-LY-STATUS.
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
           DISPLAY "** GLTRANSLY EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT GLTRANS-LY-FILE.
           IF WS-ACCEPT = "E"
               MOVE " " TO GLTRANS-LY-KEY
              START GLTRANS-LY-FILE KEY NOT < GLTRANS-LY-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND GLTRANS-LY-ASCII
           ELSE
              OPEN INPUT GLTRANS-LY-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ GLTRANS-LY-FILE NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY GLTRANS-LY-KEY.
           
           ADD 1 TO WS-COUNT.

           MOVE GLTRANS-LY-REC    TO ASCII-REC.
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
           READ GLTRANS-LY-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-REFERENCE


           MOVE ASCII-REFERENCE       TO GLTRANS-LY-REFERENCE.
           MOVE ASCII-TRANS           TO GLTRANS-LY-TRANS.
           MOVE ASCII-TYPE            TO GLTRANS-LY-TYPE.
           MOVE ASCII-NO              TO GLTRANS-LY-NO.
           MOVE ASCII-DATE            TO GLTRANS-LY-DATE.
           MOVE ASCII-ACCNO           TO GLTRANS-LY-ACCNO.
           MOVE ASCII-AMOUNT          TO GLTRANS-LY-AMOUNT.
           MOVE ASCII-LINE-DESC       TO GLTRANS-LY-LINE-DESC.
        BI-010.
           WRITE GLTRANS-LY-REC
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
           CLOSE GLTRANS-LY-FILE
                 GLTRANS-LY-ASCII.
        C-EXIT.
           EXIT.
      * END-OF-JOB.
