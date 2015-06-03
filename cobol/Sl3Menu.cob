        IDENTIFICATION DIVISION.
        PROGRAM-ID. Sl3Menu.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       Copy "WsMenuDateInfo".

       LINKAGE SECTION.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
          PERFORM OPEN-FILES
          PERFORM CLEAR-SCREEN.
       CONTROL-010.
          PERFORM DISPLAY-FORM
          PERFORM GET-DATA
          GO TO CONTROL-010.
      *
       GET-DATA SECTION.
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
                     AND NOT = "10" AND NOT = "11" AND NOT = "12"
                     AND NOT = "13" AND NOT = "14" AND NOT = "15"
                     AND NOT = "16" AND NOT = "17" AND NOT = "18"
                     AND NOT = "19" AND NOT = "20" AND NOT = "21"
                     AND NOT = "XX"
                MOVE "Selection Must Be Between 1 & 21, Re-Enter."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.
     
            IF WS-ANSWER = "XX"
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION
            MOVE 32        TO WS-OPTION
            MOVE "N"       TO WS-PASSWORD-VALID.
            IF F-EXIT-CH = X"1D"
                MOVE "Y"   TO WS-OVER-RIDE
            ELSE
                MOVE "N"   TO WS-OVER-RIDE.
         Perform Check-Password.
      *     MOVE "PassWrds" TO WS-PROGRAM.
      *     Call Ws-Program Using Ws-Linkage.

            IF WS-PASSWORD-VALID = "N"
                MOVE "INVALID PASSWORD ENTERED, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-010.
           PERFORM DISPLAY-PR-NO.
      *     CANCEL WS-PROGRAM.
           IF WS-ANSWER = "35"
                MOVE "CoStffIq.Int" TO WS-PROGRAM.
            IF WS-ANSWER = " 1"
                MOVE "SlAnCoRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 2"
                MOVE "SlMastRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 3"
                MOVE "SlDistRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 4"
                MOVE "SlGstxRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 5"
                MOVE "SlInCrRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 6"
                MOVE "SlInRgRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 7"
                MOVE "SlOrFiRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 8"
                MOVE "SlOrRgRp" TO WS-PROGRAM.
            IF WS-ANSWER = " 9"
                MOVE "SlPOrdRp" TO WS-PROGRAM.
            IF WS-ANSWER = "10"
                MOVE "SlOrSuRp" TO WS-PROGRAM.
            IF WS-ANSWER = "11"
                MOVE "SlQuRgRp" TO WS-PROGRAM.
            IF WS-ANSWER = "12"
                MOVE "SlSManRp" TO WS-PROGRAM.
            IF WS-ANSWER = "13"
                MOVE "SlSoByRp" TO WS-PROGRAM.
            IF WS-ANSWER = "14"
                MOVE "SlDyExRp" TO WS-PROGRAM.
            IF WS-ANSWER = "15"
                MOVE "SlPuByRp" TO WS-PROGRAM.
            IF WS-ANSWER = "16"
                MOVE "SlCashRp" TO WS-PROGRAM.
            IF WS-ANSWER = "17"
                MOVE "SlSpecRp" TO WS-PROGRAM.
            IF WS-ANSWER = "18"
                MOVE "SlDelvRp" TO WS-PROGRAM.
            IF WS-ANSWER = "19"
                MOVE "SlSaleRp" TO WS-PROGRAM.
            IF WS-ANSWER = "20"
                MOVE "SlRgAcRp" TO WS-PROGRAM.
            IF WS-ANSWER = "21"
                MOVE "SlChckRp" TO WS-PROGRAM.
       GET-050.
           Call Ws-Program Using Ws-Linkage.
            PERFORM CLEAR-SCREEN
            CANCEL WS-PROGRAM
            PERFORM DISPLAY-FORM
            MOVE " " TO F-NAMEFIELD WS-ANSWER
            GO TO GET-010.
       GET-999.
            EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "SL3MENU" TO F-FORMNAME
            MOVE 7         To F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
      *     STOP RUN.
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
       Copy "CTOSCobolAccept".
      *
      * END-OF-JOB
