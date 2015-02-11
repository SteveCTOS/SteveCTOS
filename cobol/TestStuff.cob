       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestStuff.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-USERNAME             PIC X(30) VALUE SPACES.
       01  W-ENTER                PIC X.
       01  W-PRINTCOMMAND.
           03  W-PRINTCOM1A        PIC X(6) VALUE SPACES.
           03  W-PRINTCOM1         PIC X(95) VALUE SPACES.
           03  W-PRINTCOM2         PIC X(50) VALUE SPACES.
       01  W-STATUS               PIC 9(4) BINARY COMP.
       01  WS-PRINTER             PIC X(5) VALUE "MP140".
       01  WS-PRINT-FILE          PIC X(50) VALUE 
                   "/ctools/spl/steve.prn".
      *
       PROCEDURE DIVISION.
       000-Main.
          ACCEPT W-USERNAME FROM ENVIRONMENT "USERNAME".
          DISPLAY "USERNAME: " W-USERNAME.
          
      *    MOVE "lp -d" WS-PRINTER &
      *      "/ctools/dev/source/cobol/TestStuff.cob" TO W-PRINTCOMMAND.
      *    MOVE "/ctools/dev/source/cobol/TestStuff.cob" TO

          MOVE "lp -d "       TO W-PRINTCOM1A
          MOVE WS-PRINTER     TO W-PRINTCOM1
          MOVE WS-PRINT-FILE  TO W-PRINTCOM2.
          
          DISPLAY "PRINT COMMAND: " W-PRINTCOMMAND.
          ACCEPT W-ENTER.
           CALL "SYSTEM" USING W-PRINTCOMMAND 
               RETURNING W-STATUS
          END-CALL.
          DISPLAY "STATUS of CALL: " W-STATUS.
          ACCEPT W-ENTER.
          STOP RUN.
           
 
