
       OPEN-023.
            OPEN I-O INVOICE-CH-MASTER.
            IF WS-INV-CH-ST1 NOT = 0
               MOVE "INVOICE-CH-STATUS ERC ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INV-CH-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE WS-INVCHSTATUS TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE Ws-InvChStatus TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-INV-CH-ST1
               
               PERFORM OPEN-INV-CH-STATUS-FIRST-TIME
               
               GO TO OPEN-023.
       OPEN-023A.
               GO TO OPEN-024.






       OPEN-023.
      * CODE 23= RECORD IN FILE NOT FOUND
      * CODE 35= THE DATAFILE IS NOT THERE AT ALL.
            OPEN I-O INVOICE-CH-MASTER.
            IF WS-INV-CH-ST1 = 47 OR = 48 OR = 49
              MOVE 
          "INVOICE-CH-MASTER OPEN I-O ERC, SEE NEXT LINE." TO WS-MESSAGE
              PERFORM ERROR1-010
              MOVE WS-INV-CH-ST1 TO Ws-MESSAGE
              PERFORM ERROR-MESSAGE
	    	IF SUB-10 < 10
              CLOSE INVOICE-CH-MASTER
              GO TO OPEN-023
            ELSE
              MOVE 0 TO SUB-10
              CLOSE INVOICE-CH-MASTER
              GO TO OPEN-024.
            IF WS-INV-CH-ST1 NOT = 0
              MOVE 
          "INVOICE-CH-MASTER OPEN I-O ERC, SEE NEXT LINE." TO WS-MESSAGE
              PERFORM ERROR1-010
              MOVE WS-INV-CH-ST1 TO Ws-MESSAGE
              PERFORM ERROR-MESSAGE
              ADD 1 TO SUB-10
           IF WS-INV-CH-ST1 = 0
              MOVE 
         "THE INVOICE-CH-MASTER DATA-FILE HAS OPENED I-O WELL"
               TO WS-MESSAGE
              PERFORM ERROR-MESSAGE.
       OPEN-023B.
              GO TO OPEN-030.
       OPEN-024.
            IF WS-INV-CH-ST1 = 35
               MOVE "OPEN I-O ERC35. FILE NOT THERE." TO WS-MESSAGE
               PERFORM ERROR1-010
               MOVE "GOING TO OPEN OUTPUT TO OPEN FOR FIRST TIME."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               PERFORM ERROR-MESSAGE
               OPEN OUTPUT INVOICE-CH-MASTER
               MOVE
          "OPEN OUTPUT INV STATUS FILE NOW OPEN, 'ESC' TO SEE ERC."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INV-CH-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               CLOSE INVOICE-CH-MASTER
               GO TO OPEN-023.

            IF WS-INV-CH-ST1 NOT = 0
               MOVE "INVOICE STATUS ERC ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INV-CH-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INV-CH-ST1
               GO TO OPEN-023.
