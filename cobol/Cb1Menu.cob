       IDENTIFICATION DIVISION.
       PROGRAM-ID. Cb1Menu.
       AUTHOR. CHRISTENSEN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
       SOURCE-COMPUTER. B20.
       OBJECT-COMPUTER. B20.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       Copy "WsMenuDateInfo".
       01  W-CRTSTATUS           PIC 9(4) value 0.
       Linkage Section.
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
            MOVE 2 TO F-CBFIELDLENGTH
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
                     AND NOT = "16" AND NOT = "XX"
            IF WS-ANSWER = "XX"
                MOVE "Selection Must Be Between 1 & 16, Re-Enter."
                TO WS-MESSAGE
                PERFORM ERROR-000
                MOVE "  " TO F-NAMEFIELD WS-ANSWER
                GO TO GET-010.

            IF WS-ANSWER = "XX"
               MOVE "35"   TO WS-ANSWER.
            MOVE WS-ANSWER TO WS-SELECTION
            MOVE 70        TO WS-OPTION
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
                GO TO GET-010
           ELSE
                PERFORM ERROR-020.

      *     CANCEL WS-PROGRAM.
           IF WS-ANSWER = "35"
                MOVE "CoStffIq" TO WS-PROGRAM.
           IF WS-ANSWER = " 1"
                MOVE "CbMastMt" TO WS-PROGRAM.
           IF WS-ANSWER = " 2"
                MOVE "CbMastIq" TO WS-PROGRAM.
           IF WS-ANSWER = " 3"
                MOVE "CbMaLyIq" TO WS-PROGRAM.
           IF WS-ANSWER = " 4"
                 MOVE "CbTrans" TO WS-PROGRAM.
           IF WS-ANSWER = " 5"
                MOVE "CbTranIq" TO WS-PROGRAM.
           IF WS-ANSWER = " 6"
                MOVE "CbTrFuIq" TO WS-PROGRAM.
           IF WS-ANSWER = " 7"
                MOVE "CbTrLyIq" TO WS-PROGRAM.
           IF WS-ANSWER = " 8"
                MOVE "CbBankTr" TO WS-PROGRAM.
           IF WS-ANSWER = " 9"
                MOVE "CbStAlMt" TO WS-PROGRAM.
           IF WS-ANSWER = "10"
                MOVE "CbStatRp" TO WS-PROGRAM.
           IF WS-ANSWER = "11"
                MOVE "CbTrOsRp" TO WS-PROGRAM.
           IF WS-ANSWER = "12"
                MOVE "CbGlPost" TO WS-PROGRAM.
           IF WS-ANSWER = "13"
                MOVE "CbCAMSMt" TO WS-PROGRAM.
           IF WS-ANSWER = "14"
                MOVE "CbCAMSAl" TO WS-PROGRAM.
           IF WS-ANSWER = "15"
                MOVE "CbPerEnd" TO WS-PROGRAM.
           IF WS-ANSWER = "16"
                Move "CbMaint.Sub" TO Ws-Data-Name
                Go To GET-015.

           Call Ws-Program Using Ws-Linkage.
           PERFORM CLEAR-SCREEN
           CANCEL WS-PROGRAM
           PERFORM DISPLAY-FORM
           MOVE " " TO F-NAMEFIELD WS-ANSWER
           GO TO GET-010.
       GET-015.
            Perform Check-Data-Names.
            CALL "OPENFILELL" USING F-ERROR1
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
            IF F-ERROR1 NOT = 0
                DISPLAY "OPENFILE CBMAINT.SUB ERROR"
                DISPLAY F-ERROR1
                STOP RUN.
            CALL "SETSYSINMODE" USING WS-CTOS-ERROR
                                      WS-MODE
                                      F-FH.
            STOP RUN.
       GET-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-010.
            Move Ws-Forms-Name   TO F-FileName
            Move Ws-cbForms-name TO F-CbFileName.
            MOVE "Cb1Menu"       TO F-FORMNAME
            MOVE 7               TO F-CBFORMNAME.
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
       Copy "PassChck.Src".
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "MenuClearScreen".
       Copy "ErrorMessage".
       Copy "DisplayProgNum".
       Copy "CheckMenuDataNames".
      *
      * END-OF-JOB
