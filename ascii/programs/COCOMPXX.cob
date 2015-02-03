        IDENTIFICATION DIVISION.
        PROGRAM-ID. CoCompXX.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT COMPANY-MENU ASSIGN TO "CoCompany"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PTY-KEY
               FILE STATUS IS WS-COMPANY-STATUS.
           SELECT COMPANY-ASCII ASSIGN TO
                            "CoCompanyASCII"
               FILE STATUS IS WS-COMPANY-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdCompany".
           COPY "ChlfdCompanyASCII".
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-MESSAGE    PIC X(79) VALUE " ".
           77  POS           PIC 9(4) VALUE 0.
           01  WS-COMPANY-STATUS.
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
           DISPLAY "** COMPANY EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY "ENTER E=EXPORT TO ASCII, I=IMPORT FROM ASCII: [ ]"
              AT POS
           MOVE 1057 TO POS
           ACCEPT WS-ACCEPT AT POS.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-001.
              
           MOVE WS-ACCEPT TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.      
              
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           OPEN OUTPUT COMPANY-MENU.
      *     MOVE " " TO PTY-KEY.
      *     START COMPANY-MENU KEY NOT < PTY-KEY
      *           INVALID KEY
      *           DISPLAY "NO RECORDS ON FILE "
      *           STOP RUN.

           IF WS-ACCEPT = "E"
              OPEN EXTEND COMPANY-ASCII
           ELSE
              OPEN INPUT COMPANY-ASCII.
        A-EXIT.
           EXIT.
      *
        B-EXPORT SECTION.
        BE-005.
           MOVE "DOING NOTHING HERE" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE
           GO TO BE-EXIT.
      *    READ COMPANY-MENU NEXT
      *         AT END 
      *       GO TO BE-EXIT.
      *         
      *     DISPLAY PTY-NUMBER.
      *
      *     MOVE COMPANY-RECORD    TO ASCII-RECORD.
      * BE-010.
      *     WRITE ASCII-RECORD
      *           INVALID KEY
      *       DISPLAY "INVALID WRITE FOR ASCII FILE...."
      *       DISPLAY WS-STAT1
      *       DISPLAY WS-STAT2
      *       STOP RUN.
      *     GO TO BE-005.
       BE-EXIT.
           EXIT.
      *
        B-IMPORT SECTION.
        BI-005.
           READ COMPANY-ASCII NEXT
               AT END 
             GO TO BI-EXIT.
               
           MOVE ASCII-NAME TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.

      *     MOVE ASCII-RECORD    TO COMPANY-RECORD.
           
           MOVE ASCII-NUMBER      TO PTY-NUMBER
           MOVE ASCII-NAME        TO PTY-CO-NAME
           MOVE ASCII-VOL         TO PTY-VOL-DIR.
        BI-010.
           WRITE COMPANY-RECORD
                 INVALID KEY
             DISPLAY "INVALID WRITE FOR ISAM FILE..."
             DISPLAY WS-STAT1
             DISPLAY WS-STAT2
             STOP RUN.
             
           MOVE "WRITE DONE, GOING TO BI-005" TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
             
           GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE COMPANY-MENU
                 COMPANY-ASCII.
        C-EXIT.
           EXIT.
           COPY "ErrorMessage".
      * END-OF-JOB.
