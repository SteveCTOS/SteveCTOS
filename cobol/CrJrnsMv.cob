        IDENTIFICATION DIVISION.
        PROGRAM-ID. CrJrnsMv.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT CrJrn-FILE ASSIGN TO Ws-CrJrn
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CrJrn-KEY
               ALTERNATE RECORD KEY IS 
                        CRJRN-CRACC-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRJRN-INV-NO WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRJRN-DNOTE-NO WITH DUPLICATES
               FILE STATUS IS WS-CRJRN-STATUS.
                     
           SELECT CrJrn1-FILE ASSIGN TO Ws-CrJrnTemp
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CRJRN1-KEY
               ALTERNATE RECORD KEY IS 
                          CRJRN1-CRACC-NUMBER WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRJRN1-INV-NO WITH DUPLICATES
               ALTERNATE RECORD KEY IS CRJRN1-DNOTE-NO WITH DUPLICATES
               FILE STATUS IS WS-CRJRN-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY "ChlfdCrJrn".
           COPY "ChlfdCrJrn1".
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           77  WS-ACCEPT     PIC X VALUE " ".
           77  WS-COUNT      PIC 9(6) VALUE 0.
           01  WS-CHECK-ST.
               03  WS-CHECK-1    PIC X.
               03  WS-CHECK-BAL  PIC X(14).
           01  WS-CrJrn-STATUS.
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
           MOVE 0810 TO POS.
           DISPLAY "** CR-JRN EXPORT / IMPORT OF DATA **" AT POS
           MOVE 1010 TO POS
           DISPLAY 
           "ENTER E=EXPORT TO CrJrnTemp, I=IMPORT FROM Temp:   [ ] "
              AT POS
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
           DISPLAY WS-ACCEPT AT 1075.
           IF WS-ACCEPT = "X"
              EXIT PROGRAM.
           IF WS-ACCEPT NOT = "E" AND NOT = "I"
              GO TO A-AC001.
        A-AC-EXIT.
           EXIT.
      *
        A-INIT SECTION.
        A-000.
           MOVE Ws-CrJrn TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           MOVE Ws-CrJrnTemp TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "I"
               OPEN OUTPUT CrJrn-FILE
           ELSE
               OPEN I-O CrJrn-FILE.
           
      *     MOVE WS-ACCEPT TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
           MOVE WS-STAT1 TO WS-MESSAGE
           PERFORM ERROR-MESSAGE.
           
           IF WS-ACCEPT = "E"
               MOVE 0 TO CrJrn-KEY
              START CrJrn-FILE KEY NOT < CrJrn-KEY.
            
           IF WS-ACCEPT = "E"
              OPEN OUTPUT crjrn1-FILE
           ELSE
              OPEN I-O crjrn1-FILE.
           
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
        BE-001.
           MOVE 1 TO CrJrn-TYPE
                     CrJrn-TRANS.
        BE-005.
           READ CrJrn-FILE NEXT WITH LOCK
               AT END 
             DISPLAY WS-COUNT
             GO TO BE-EXIT.
               
           DISPLAY CrJrn-KEY   AT 1505
           DISPLAY CrJrn-INV-DATE AT 1520.
           DISPLAY "ACC NUM:" AT 1535
           DISPLAY CrJrn-CRACC-NUMBER AT 1544.
           
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
       
           CALL "C$SLEEP" USING 1.
           IF CrJrn-CrACC-NUMBER = 1300003
               GO TO BE-005.
           

           IF CrJrn-KEY NOT > 0
               MOVE "KEY < 0, GOING TO RE-READ." TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               GO TO BE-005.
           IF CrJrn-CRACC-NUMBER NOT > 0
               MOVE "ACC NUM < 0, GOING TO RE-READ." TO WS-MESSAGE
               PERFORM ERROR1-MESSAGE
               GO TO BE-005.

           MOVE CrJrn-REC    TO CrJrn1-Rec.
        BE-010.
           WRITE CrJrn1-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM-EXPORT FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             MOVE crjrn1-KEY TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020
             ADD 1 TO CrJrn-TRANS
                      START CrJrn-FILE KEY > CrJrn-KEY.
                      
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
           READ crjrn1-FILE NEXT WITH LOCK
               AT END 
             GO TO BI-EXIT.
               
           DISPLAY crjrn1-KEY AT 1505
           DISPLAY crjrn1-INV-DATE AT 1420.
           ADD 1 TO WS-COUNT
           DISPLAY WS-COUNT AT 2510.
           
           MOVE crjrn1-REC    TO CrJrn-REC.
        BI-010.
           WRITE CrJrn-REC
                 INVALID KEY
             MOVE "INVALID WRITE FOR ISAM1 FILE..." TO WS-MESSAGE
             PERFORM ERROR1-000
             MOVE WS-STAT1 TO WS-MESSAGE
             PERFORM ERROR-MESSAGE
             PERFORM ERROR1-020.
      *       DISPLAY "INVALID WRITE FOR ISAM FILE..."
      *       DISPLAY WS-STAT1
      *       CLOSE CrJrn-FILE
      *             crjrn1-FILE
      *       CALL "C$SLEEP" USING 3
      *         EXIT PROGRAM.
      *       STOP RUN.
      
      *      MOVE SPACES TO CrJrn-REC.
            
            GO TO BI-005.
        BI-EXIT.
           EXIT.
      *    
        C-END SECTION.
        C-000.
           CLOSE CrJrn-FILE
                 crjrn1-FILE.
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
