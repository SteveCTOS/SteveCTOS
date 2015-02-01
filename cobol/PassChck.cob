      *
       Check-Password-Info Section.
       CPI-005.
           Move 1 to Sub-1.
       CPI-010.
           If Ws-MenuNumber (Sub-1) Not = Ws-Option
            If Sub-1 < 35
              Add 1 to Sub-1
              Go To CPI-010.
       CPI-020.
           Move Ws-Selection                 To Sub-3.
           Move Ws-Pa-Number (Sub-1 Sub-3)   To Ws-PasswordNeeded
           Move Ws-Pa-Priority (Sub-1 Sub-3) To Ws-PriorityNeeded.
       CPI-999.
           Exit.
      *
       CHECK-PASSWORD SECTION.
       CP-020.
          IF WS-OVER-RIDE = "Y"
              MOVE 0 TO   Ws-PriorityNeeded
                          WS-LASTPRIORITY
                          WS-LASTOPTION
              MOVE " " TO WS-LASTPASSWORD
                          WS-PASSWORDNEEDED.
           MOVE 1 TO SUB-1 Sub-3.
           Move 2710 To Pos
           Display "                                        " At Pos.
           Perform Check-Password-Info.
           IF WS-PasswordNeeded = "    "
               MOVE "Y" TO WS-PASSWORD-VALID
               GO TO CP-900.
       CP-450.
           IF WS-LASTOPTION = 99
            IF WS-LASTPRIORITY NOT < Ws-PriorityNeeded
              MOVE "Y" TO WS-PASSWORD-VALID
              GO TO CP-900.
           IF WS-LASTPASSWORD = Ws-PasswordNeeded
              MOVE "Y" TO WS-PASSWORD-VALID
              GO TO CP-900.
       CP-500.
           MOVE " " TO WS-PASSWORD-KEY.
           MOVE 2725 TO POS
           DISPLAY "Enter a PASSWORD :" AT POS
           MOVE 2745 TO POS
           MOVE 1 TO SUB-2.
       CP-550.
           PERFORM READ-KBD.
           IF W-ESCAPE-KEY = X"0E" OR = X"08"
           IF SUB-2 > 1
               MOVE " " TO WS-PA-KEY (SUB-2)
               SUBTRACT 1 FROM SUB-2 POS
               MOVE " " TO WS-PASSWORD-VALID WS-PA-KEY (SUB-2)
               DISPLAY WS-PASSWORD-VALID AT POS
               GO TO CP-550
           ELSE
               GO TO CP-550.
           IF W-ESCAPE-KEY = X"0A" OR = X"1B"
               GO TO CP-800.
           MOVE W-ESCAPE-KEY TO WS-PA-KEY (SUB-2)
           MOVE "#" TO WS-PASSWORD-VALID
           DISPLAY WS-PASSWORD-VALID AT POS.
           IF SUB-2 NOT > 10
               ADD 1 TO SUB-2 POS
               GO TO CP-550.
       CP-800.
           IF WS-PASSWORD-KEY = Ws-PasswordNeeded
              MOVE "Y" TO WS-PASSWORD-VALID
              GO TO CP-860.
           Move Ws-Option To Ws-OptionSave
           MOVE 99 TO Ws-Option.
       CP-810.
           Perform CPI-005 Thru CPI-010.
           MOVE "N" TO WS-PASSWORD-VALID
           MOVE 1 TO SUB-2.
       CP-850.
           IF WS-PASSWORD-KEY = Ws-Pa-Number (Sub-1 SUB-2)
            IF Ws-Pa-Priority (Sub-1 SUB-2) NOT < Ws-PriorityNeeded
              MOVE "Y" TO WS-PASSWORD-VALID
              MOVE Ws-Pa-Priority (Sub-1 SUB-2) TO Ws-LastPriority
              GO TO CP-860.
           IF SUB-2 NOT > 34
              ADD 1 TO SUB-2
              GO TO CP-850.
       CP-860.
           IF WS-PASSWORD-VALID = "N"
               MOVE 0   TO WS-LASTOPTION
                           WS-LASTPRIORITY
               MOVE " " TO WS-LASTPASSWORD
                           WS-PASSWORD-KEY
           ELSE
               MOVE Ws-Option         TO WS-LASTOPTION
               MOVE Ws-OptionSave     TO WS-OPTION
               MOVE WS-PASSWORD-KEY   TO WS-LASTPASSWORD.
       CP-900.
           PERFORM ERROR-020.
           MOVE 2710 TO POS
           DISPLAY WS-MESSAGE AT POS.
       CP-999.
           EXIT.
      *
        READ-KBD SECTION.
        READ-KBD000.
            CALL "&READKBD" USING F-ERROR1
                                  W-ESCAPE-KEY.
            IF F-ERROR1 NOT = 0
                DISPLAY "READKBD ERROR"
                DISPLAY F-ERROR1
                STOP RUN.
        READ-KBD999.
             EXIT.
      *
      *END-OF-JOB
