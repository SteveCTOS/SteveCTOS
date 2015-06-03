        IDENTIFICATION DIVISION.
        PROGRAM-ID. BaUpMenu.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT PARAMETER-FILE ASSIGN TO Ws-Parameter
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS WS-PARAMETER-STATUS
               RECORD KEY IS PA-KEY.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdParam.

       WORKING-STORAGE SECTION.
       77  WS-QUES-PAUSE-BACKUP   PIC X VALUE " ".
       77  W-BACKUP-DELAY         PIC 9(4) COMP-X.
       Copy "WsMenuDateInfo".
       01  WS-PARAMETER-STATUS.
           03  WS-PARAMETER-ST1     PIC X.
           03  WS-PARAMETER-ST2     PIC X.
       LINKAGE SECTION.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
        CONTROL-PARAGRAPH SECTION.
        CONTROL-000.
            PERFORM OPEN-FILES
            PERFORM CLEAR-SCREEN.
        CONT-010.
            PERFORM DISPLAY-FORM
            PERFORM GET-DATA
            GO TO CONT-010.
      *
       GET-DATA SECTION.
       GET-005.
            MOVE 2 TO WS-MODE.
       GET-010.
            MOVE "                      " TO F-NAMEFIELD
            MOVE "SELECTION" TO F-FIELDNAME
            MOVE 9 TO F-CBFIELDNAME
            PERFORM USER-FILL-FIELD.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"1B" AND NOT = X"1D"
                DISPLAY " " AT 3079 WITH BELL
                GO TO GET-010.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = "  "
                GO TO GET-010.
            MOVE F-NAMEFIELD TO WS-NUMBER-CHECK.
            IF WS-NU2 = " "
               MOVE WS-NU1 TO WS-NU2
               MOVE " "    TO WS-NU1
               MOVE WS-NUMBER-CHECK TO WS-ANSWER
            ELSE
               MOVE F-NAMEFIELD TO WS-ANSWER.
            IF WS-ANSWER NOT = " 1" AND NOT = " 2" AND NOT = " 3"
                     AND NOT = " 4" AND NOT = " 5" AND NOT = " 6"
                     AND NOT = " 7" AND NOT = " 8" AND NOT = " 9"
                     AND NOT = "10"
                     AND NOT = "XX"
                MOVE "Selection Must Be Between 1 & 10, Re-Enter."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.

            IF WS-ANSWER = "XX"
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION
            MOVE 90        TO WS-OPTION
            MOVE "N"       TO WS-PASSWORD-VALID.
            IF F-EXIT-CH = X"1D"
                MOVE "Y"   TO WS-OVER-RIDE
            ELSE
                MOVE "N"   TO WS-OVER-RIDE.
         Perform Check-Password.
      *    Move "N" to Ws-1stRead.
      *    MOVE "PassWrds" TO WS-PROGRAM
      *    CALL WS-PROGRAM USING Ws-Linkage.

            IF WS-PASSWORD-VALID = "N"
                MOVE "INVALID PASSWORD ENTERED, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010.
      *      CANCEL WS-PROGRAM.
           IF WS-ANSWER = "35"
                MOVE "CoStffIq.Int" TO WS-PROGRAM.
            IF WS-ANSWER = " " OR = "0"
                GO TO GET-005.
            IF WS-ANSWER = " 1"
                Move "Backup.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 2"
                Move "Restore.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 3"
                Move "TapeBackup.Sub" TO Ws-Data-Name
                PERFORM CHECK-FOR-BACKUP-PAUSE
                PERFORM GET-100.
            IF WS-ANSWER = " 4"
                Move "TapeRestore.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 5"
                Move "TapeBackupVol.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 6"
                Move "TapeBackupFax.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 7"
                Move "TapeBackupMonth.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 8"
                Move "FormatFloppy.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = " 9"
                Move "TapeErase.Sub" TO Ws-Data-Name
                PERFORM GET-100.
            IF WS-ANSWER = "10"
                Move "TapeRetension.Sub" TO Ws-Data-Name
                PERFORM GET-100.
       GET-100.
            Perform Check-Data-Names.
      *      CALL "&OPENFILELL" USING F-ERROR1
      *                               F-FH
      *                               F-FILENAME
      *                               F-CBFILENAME
      *                               F-FILENAME
      *                               F-INTEGERZERO
      *                               F-OPENMODE.
            IF F-ERROR1 NOT = 0
                DISPLAY "ERROR IN SUBMIT FILE."
                DISPLAY F-ERROR1
                STOP RUN.
      *      CALL "&SETSYSINMODE" USING WS-CTOS-ERROR
      *                                 WS-MODE
      *                                 F-FH.
            STOP RUN.
       GET-999.
            EXIT.
      *
       CHECK-FOR-BACKUP-PAUSE SECTION.
       CFBP-005.
           IF WS-QUES-PAUSE-BACKUP NOT = "Y"
                GO TO CFBP-999.
       CFBP-010.
           MOVE 2710 TO POS
           DISPLAY 
           "The Backup Has Been Set On a 30 Min DELAY, You May leave."
            AT POS.
       
           CALL "C$SLEEP" USING 1800.
      *     MOVE 18000 TO W-BACKUP-DELAY.
      *     CALL "&DELAY" USING W-ERC
      *                         W-BACKUP-DELAY.
       CFBP-999.
           EXIT.
      *
       READ-INVQUES-FILE SECTION.
       RINVQUES-000.
            MOVE 1 TO PA-RECORD.
            MOVE 6 TO PA-TYPE.
            START PARAMETER-FILE KEY NOT < PA-KEY.
       RINVQUES-010.
            READ PARAMETER-FILE
                INVALID KEY NEXT SENTENCE.
            IF WS-PARAMETER-ST1 = "2"
               MOVE "N" TO WS-QUES-PAUSE-BACKUP
               GO TO RINVQUES-999.
            IF WS-PARAMETER-ST1 NOT = "0"
               MOVE " " TO WS-PARAMETER-ST1
               MOVE "Parameter Busy RINVQUES, Press 'CANCEL' To Retry."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RINVQUES-010.
       RINVQUES-900.
            MOVE INVQUES-PAUSE-BACKUP TO WS-QUES-PAUSE-BACKUP.
       RINVQUES-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O PARAMETER-FILE.
           IF WS-PARAMETER-ST1 NOT = "0" 
              MOVE " " TO WS-PARAMETER-ST1
              MOVE "PARAMETER FILE BUSY, BE PATIENT!" TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO OPEN-000.
           PERFORM READ-INVQUES-FILE.
           CLOSE PARAMETER-FILE.
        OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "BaUpMenu"      TO F-FORMNAME
            MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
      *      STOP RUN.
            EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "PassChck.cob".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "MenuClearScreen".
       Copy "ErrorMessage".
       Copy "DisplayProgNum".
       Copy "CheckMenuDataNames".
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
