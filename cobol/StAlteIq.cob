        IDENTIFICATION DIVISION.
        PROGRAM-ID. StAlteIq.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        REPOSITORY. 
           FUNCTION ALL INTRINSIC.
        SPECIAL-NAMES.
          CRT STATUS IS W-CRTSTATUS.
        SPECIAL-NAMES.
        CLASS WS-VALID-EMAIL IS
          '@' '_' '.' '-' '#'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.

        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStCatalogue".
          Copy "SelectStAlternative".
          Copy "SelectStSpecPr".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStAlternative.
           COPY ChlfdStCatalogue.
           COPY ChlfdStPrice.

       WORKING-STORAGE SECTION.
       77  NEW-STOCKNO        PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-STOCKNUMBER     PIC X(15) VALUE " ".
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "StMastIq".
       01  WS-STDESC.
           03  WS-DESC1       PIC X(20) VALUE " ".
           03  WS-DESC2       PIC X(20) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1   PIC 99.
       01  WS-STALT-STATUS.
           03  WS-STALT-ST1   PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1    PIC 99.
       01  WS-STCAT-STATUS.
           03  WS-STCAT-ST1   PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".
      *
       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
           PERFORM DISPLAY-FORM.
       CONTROL-010.
           PERFORM GET-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO STALT-RECORD.
            MOVE "N" TO NEW-STOCKNO
                        WS-END.
       GET-001.              
            MOVE "STOCKNUMBER" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO STALT-STOCKNUMBER.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-SCREEN-FORM
                PERFORM READ-ALTERNATIVE-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM DISPLAY-FORM 
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-SCREEN-FORM
                PERFORM READ-ALTERNATIVE-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM DISPLAY-FORM 
               GO TO GET-000.
           IF F-EXIT-CH = X"07"
               PERFORM DISPLAY-FORM 
               GO TO GET-001.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
      *      IF STALT-STOCKNUMBER = 0 OR = "   "
      *          GO TO GET-000.
            IF STALT-STOCKNUMBER = 0 OR = "   "
                CLOSE STOCK-MASTER
                CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                CANCEL WS-INQUIRY-PROGRAM
                PERFORM OPEN-000
                PERFORM DISPLAY-FORM
                GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM CLEAR-SCREEN-FORM.
            PERFORM READ-ALTERNATIVE.
            IF WS-STOCK-ST1 = 88
                GO TO GET-999.
            IF NEW-STOCKNO = "Y"
                PERFORM GET-003
                GO TO GET-999.
       GET-003.
            MOVE "STOCKNUMBER"     TO F-FIELDNAME
            MOVE 11                TO F-CBFIELDNAME
            MOVE STALT-STOCKNUMBER TO F-NAMEFIELD
            MOVE 25                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.
            
            MOVE "DESCRIPTION"  TO F-FIELDNAME
            MOVE 11             TO F-CBFIELDNAME
            MOVE WS-STDESC      TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
            
            MOVE 1 TO SUB-1 F-INDEX.
       GET-005.
            IF STALT-NUMBER (SUB-1) > " "
               MOVE STALT-NUMBER (SUB-1) TO ST-STOCKNUMBER
               PERFORM READ-STOCK
            ELSE
               GO TO GET-999.
       GET-010.
            PERFORM READ-CATALOGUE-REF.

            MOVE "CAT-PAGE"        TO F-FIELDNAME
            MOVE 8                 TO F-CBFIELDNAME.
            IF STCAT-PAGE-NUM > " "
               MOVE STCAT-PAGE-NUM TO F-NAMEFIELD
            ELSE
               MOVE "NONE"         TO F-NAMEFIELD.
            MOVE 5                 TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NUMBER"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE STALT-NUMBER (SUB-1) TO F-NAMEFIELD
            MOVE 25                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.


            MOVE SPACES          TO WS-STDESC
            MOVE ST-DESCRIPTION1 TO WS-DESC1
            MOVE ST-DESCRIPTION2 TO WS-DESC2.

            MOVE "DESC"         TO F-FIELDNAME
            MOVE 4              TO F-CBFIELDNAME
            MOVE WS-STDESC      TO F-NAMEFIELD
            MOVE 38             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA

            PERFORM READ-SPECIAL-PRICES.

            MOVE "SPECIAL"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME.
            IF STPR-PRICE > 0
                MOVE "Y"        TO F-NAMEFIELD
            ELSE
                MOVE "N"        TO F-NAMEFIELD.
            MOVE 1              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ONHAND"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME
            MOVE ST-QTYONHAND   TO F-EDNAMEFIELDINV
            MOVE 6              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-INV.

            MOVE "PRICE"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME
            MOVE ST-PRICE       TO F-EDNAMEFIELDPRICE
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-PRICE.
       GET-020.
            IF SUB-1 < 10
               ADD 1 TO SUB-1 F-INDEX
               GO TO GET-005.
       GET-999.
            EXIT.
      *
       CLEAR-SCREEN-FORM SECTION.
       CSF-001.
            MOVE 1 TO SUB-1 F-INDEX.
       CSF-005.
            MOVE "CAT-PAGE"           TO F-FIELDNAME
            MOVE 8                    TO F-CBFIELDNAME
            MOVE " "                  TO F-NAMEFIELD
            MOVE 5                    TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       
            MOVE "NUMBER"             TO F-FIELDNAME
            MOVE 6                    TO F-CBFIELDNAME
            MOVE " "                  TO F-NAMEFIELD
            MOVE 25                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DESC"        TO F-FIELDNAME
            MOVE 4             TO F-CBFIELDNAME
            MOVE " "           TO F-NAMEFIELD
            MOVE 38            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SPECIAL"           TO F-FIELDNAME
            MOVE 7                   TO F-CBFIELDNAME
            MOVE " "                 TO F-NAMEFIELD
            MOVE 1                   TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ONHAND"       TO F-FIELDNAME
            MOVE 6              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 6              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRICE"        TO F-FIELDNAME
            MOVE 5              TO F-CBFIELDNAME
            MOVE " "            TO F-NAMEFIELD
            MOVE 9              TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       CSF-020.
            IF SUB-1 < 10
               ADD 1 TO SUB-1 F-INDEX
               GO TO CSF-005.
       CSF-999.
            EXIT.
      *
       READ-STOCK SECTION.
       R-ST-000.
             START STOCK-MASTER KEY NOT < ST-KEY.
       R-ST-010.
             READ STOCK-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                GO TO R-ST-999.
             IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-ST-010.
       R-ST-999.
             EXIT.
      *
       READ-SPECIAL-PRICES SECTION.
       SPR-000.
           MOVE STALT-NUMBER (SUB-1) TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
       SPR-005.
           READ STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO STPR-PRICE
               GO TO SPR-999.
           IF WS-STPR-ST1 NOT = 0
              Move "SPECIAL PRICES BUSY ON READ, 'ESC' to RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STPR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STPR-ST1
               GO TO SPR-005.
       SPR-999.
           EXIT.
      *
       READ-CATALOGUE-REF SECTION.
       RCREF-000.
           MOVE STALT-NUMBER (SUB-1) TO STCAT-STOCKNUMBER.
           START STCAT-MASTER KEY NOT < STCAT-KEY
               INVALID KEY NEXT SENTENCE.
           IF WS-STCAT-ST1 NOT = 0
               MOVE " " TO STCAT-PAGE-NUM
               GO TO RCREF-999.
       RCREF-005.
           READ STCAT-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STCAT-ST1 = 23 OR 35 OR 49
               MOVE " " TO STCAT-PAGE-NUM
               GO TO RCREF-999.
           IF WS-STCAT-ST1 NOT = 0
              Move "ST-CATALOGUE PAGE BUSY ON READ, 'ESC' to RETRY."
              TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STCAT-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STCAT-ST1
               GO TO RCREF-005.
       RCREF-999.
           EXIT.
      *
       READ-ALTERNATIVE SECTION.
       R-CAT-000.
             MOVE STALT-STOCKNUMBER TO WS-STOCKNUMBER
                                       ST-STOCKNUMBER.
             PERFORM READ-STOCK.
             IF WS-STOCK-ST1 = 23 OR 35 OR 49
                MOVE "THIS IS NOT A VALID STOCK-NUMBER." TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STOCK-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 88 TO WS-STOCK-ST1
                GO TO R-CAT-999.
             START STALT-MASTER KEY NOT < STALT-KEY
                INVALID KEY NEXT SENTENCE.
       R-CAT-010.
             READ STALT-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-STALT-ST1 = 23 OR 35 OR 49
                MOVE "Y" TO NEW-STOCKNO
                MOVE WS-STOCKNUMBER TO STALT-STOCKNUMBER
                GO TO R-CAT-999.
             IF WS-STALT-ST1 NOT = 0
                MOVE "ST-CAT RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                GO TO R-CAT-010.
       R-CAT-999.
             EXIT.
      *
       START-ALTERNATIVE SECTION.
       ST-ST-000.
              MOVE WS-STOCKNUMBER TO STALT-STOCKNUMBER.
              START STALT-MASTER KEY NOT < STALT-STOCKNUMBER.
       ST-ST-999.
             EXIT.
      *
       READ-ALTERNATIVE-NEXT SECTION.
       RSN-005. 
           READ STALT-MASTER NEXT
             AT END 
               MOVE " " TO STALT-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-STALT-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-STALT-ST1
               MOVE "ST-ALTERNATIVE BUSY READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                GO TO RSN-005.
           IF WS-STALT-ST1 NOT = 0
               MOVE "ST-ALTERNATIVE BUSY READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                PERFORM START-ALTERNATIVE
                GO TO RSN-005.
           MOVE "N" TO NEW-STOCKNO.
           MOVE STALT-STOCKNUMBER TO WS-STOCKNUMBER
                                     ST-STOCKNUMBER.
           PERFORM READ-STOCK.
       RSN-999.
             EXIT.
      *
       READ-ALTERNATIVE-PREVIOUS SECTION.
       RPREV-005. 
           READ STALT-MASTER PREVIOUS
             AT END 
               MOVE " " TO STALT-STOCKNUMBER
                           WS-STOCKNUMBER
               MOVE "Y" TO WS-END
               MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RPREV-999.
           IF WS-STALT-ST1 = 23 OR 35 OR 49 OR 51
               MOVE "ST-ALTERNATIVE BUSY READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                GO TO RPREV-005.
           IF WS-STALT-ST1 NOT = 0
               MOVE "ST-ALTERNATIVE BUSY READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STALT-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-STALT-ST1
                PERFORM START-ALTERNATIVE
                GO TO RPREV-005.
           MOVE "N" TO NEW-STOCKNO.
           MOVE STALT-STOCKNUMBER TO WS-STOCKNUMBER
                                     ST-STOCKNUMBER.
           PERFORM READ-STOCK.
       RPREV-999.
             EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
               MOVE 0 TO WS-STOCK-ST1
               MOVE "STOCK FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-002.
            OPEN I-O STALT-MASTER.
            IF WS-STALT-ST1 NOT = 0
               MOVE 0 TO WS-STALT-ST1
               MOVE "ST-ALTERNATIVE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-002.
       OPEN-005.
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-006.
            OPEN I-O STCAT-MASTER.
            IF WS-STCAT-ST1 NOT = 0
               MOVE 0 TO WS-STCAT-ST1
               MOVE "ST-CATALOGUE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-006.
       OPEN-020.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StAlteMt"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 STPR-MASTER
                 STCAT-MASTER
                 STALT-MASTER.
           EXIT PROGRAM.
       END-999.
           EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldPrice".
       Copy "WriteFieldInv".
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
