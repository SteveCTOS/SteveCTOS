        IDENTIFICATION DIVISION.
        PROGRAM-ID. GLMALYXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT GL-LY-MASTER ASSIGN TO "GlMasterLy"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GL-LY-KEY
               ALTERNATE RECORD KEY IS GL-LY-DESCRIPTION WITH DUPLICATES
               FILE STATUS IS WS-GL-LY-STATUS.
           SELECT GL-LY-ASCII ASSIGN TO
                              "GlMasterLyASCII"
               FILE STATUS IS WS-GL-LY-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdGlMastLy.
           COPY ChlfdGlMastLyASCII.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           77  WS-COUNT      PIC 9(4) VALUE 0.
           77  WS-MESSAGE    PIC X(79) VALUE " ".
           01  WS-GL-LY-STATUS.
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
           DISPLAY "** GL-LY EXPORT / IMPORT OF DATA **" AT POS
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
           OPEN OUTPUT GL-LY-MASTER.
           IF WS-ACCEPT = "E"
               MOVE 0 TO GL-LY-NUMBER
              START GL-LY-MASTER KEY NOT < GL-LY-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN EXTEND GL-LY-ASCII
           ELSE
              OPEN INPUT GL-LY-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           READ GL-LY-MASTER NEXT
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY GL-LY-NUMBER.
           
           ADD 1 TO WS-COUNT.

           MOVE GL-LY-RECORD    TO ASCII-RECORD.
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
           READ GL-LY-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY ASCII-MESSAGE.

           MOVE ASCII-RECORD    TO GL-LY-RECORD.
        BI-010.
           WRITE GL-LY-RECORD
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             DISPLAY WS-STAT2
             MOVE "CLOSING ON ERROR" TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             STOP RUN.
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE GL-LY-MASTER
                 GL-LY-ASCII.
        C-EXIT.
           EXIT.
        COPY "ErrorMessage".
      * END-OF-JOB.
