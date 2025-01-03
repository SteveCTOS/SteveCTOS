        IDENTIFICATION DIVISION.
        PROGRAM-ID. DrMasSMt.
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
         Copy "SelectDrMaster".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdDebtor.
           
       WORKING-STORAGE SECTION.
       77  NEW-DEBTORNO       PIC X VALUE " ".      
       77  WS-END             PIC X VALUE " ".      
       77  WS-ACCOUNTNUMBER   PIC 9(7) VALUE 0.
       77  WS-ACC-SAVE        PIC 9(7) VALUE 0.
       77  WS-ALL-ENTERED     PIC X VALUE " ".
       77  WS-INQUIRY-PROGRAM PIC X(8) VALUE "DrNameIq".
       01  WS-DEBTOR-STATUS.
           03  WS-DEBTOR-ST1  PIC 99.
       Copy "WsDateInfo".
      **************************************************************
      * FORMS WORK FIELDS
      **************************************************************
       Copy "FormsInfo".
       Linkage Section.
       Copy "ChlfdLinkage".

       Procedure Division Using Ws-Linkage.
       CONTROL-PARAGRAPH SECTION.
       CONTROL-000.
           PERFORM OPEN-FILES
           PERFORM CLEAR-SCREEN.
       CONT-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM FILL-DATA.
           GO TO CONT-010.
      *
        GET-DATA SECTION.
        GET-000.
            MOVE 0 TO DEBTOR-RECORD.
            MOVE "N" TO NEW-DEBTORNO
                        WS-END.
        GET-001.
            MOVE SPACES TO F-NAMEFIELD.
            MOVE "ACCOUNTNUMBER" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO DR-ACCOUNT-NUMBER.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-ACC-SAVE TO DR-ACCOUNT-NUMBER
                 PERFORM START-DEBTOR
                 PERFORM READ-DEBTOR-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-DEBTOR-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-003
              ELSE
                 PERFORM CLEAR-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF DR-ACCOUNT-NUMBER = 0
                 CLOSE DEBTOR-MASTER
                 PERFORM CLEAR-SCREEN
                 CALL WS-INQUIRY-PROGRAM USING WS-LINKAGE
                 CANCEL WS-INQUIRY-PROGRAM
                 PERFORM CLEAR-SCREEN
                 PERFORM OPEN-000
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO GET-000.

            PERFORM ERROR-020.
            PERFORM READ-DEBTOR.
            IF NEW-DEBTORNO = "Y"
                 MOVE "ENTER A VALID ACCOUNT NUMBER, PRESS 'ESC'"
                 TO WS-MESSAGE
                 PERFORM ERROR-000
                 GO TO GET-999.
            GO TO GET-005.
        GET-003.
            MOVE "ACCOUNTNUMBER" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE DR-ACCOUNT-NUMBER TO F-NAMEFIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "NAME1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-POST-CODE TO F-EDNAMEFIELDPOST.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.

            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-DEL-ADDRESS1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-DEL-ADDRESS2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-DEL-ADDRESS3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEPHONE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-TELEPHONE TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TELEX" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-TELEX TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE DR-GSTNO TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESANAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-SALES-ANALYSIS TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "CREDITLIMIT"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE DR-CREDIT-LIMIT TO F-EDNAMEFIELDCREDIT
            MOVE 7               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-CREDIT.

            MOVE "ACCBALANCE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-BALANCE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "BALANCELASTSTATE" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            MOVE DR-BAL-LAST-STATE TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "CURRENTBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-CURRENT TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "30DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-30DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "60DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-60DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "90DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-90DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "120DAYBAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-120DAY TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "DATECREATED"   TO F-FIELDNAME.
            MOVE 11              TO F-CBFIELDNAME.
            MOVE DR-DATE-CREATED TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE    TO F-NAMEFIELD.
            MOVE 10              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTSALE"    TO F-FIELDNAME.
            MOVE 12                TO F-CBFIELDNAME.
            MOVE DR-DATE-LAST-SALE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE      TO F-NAMEFIELD.
            MOVE 10                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATELASTPAY"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE DR-DATE-LAST-PAY TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE     TO F-NAMEFIELD.
            MOVE 10               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-SALES-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-SALES-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "SALESLAST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-SALES-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTPTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-COST-PTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTYTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE DR-COST-YTD TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "COSTLAST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-COST-LAST TO F-EDNAMEFIELDNUM6.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC6.

            MOVE "DISCOUNTCODE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DR-DISCOUNT-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
 
            MOVE "TERMSCODE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-TERMS-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERYCODE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            MOVE DR-DELIVERY-CODE TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SUPPLY-Y-N" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE DR-SUPPLY-Y-N TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALESMAN" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE DR-SALESMAN TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "B-O" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE DR-PART-ORDERS TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CORPGROUP" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE DR-CORPORATE-GROUP TO F-EDNAMEFIELDGROUP.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-GROUP.

            MOVE "ACC-CONTACT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE DR-ACCOUNTS-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALES-CONTACT" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            MOVE DR-SALES-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "S-EMAIL"      TO F-FIELDNAME
            MOVE 7              TO F-CBFIELDNAME
            MOVE DR-SALES-EMAIL TO F-NAMEFIELD
            MOVE 40             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-PHONE"    TO F-FIELDNAME
            MOVE 9              TO F-CBFIELDNAME
            MOVE DR-ACC-PHONE   TO F-NAMEFIELD
            MOVE 20             TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ACC-FAX"        TO F-FIELDNAME
            MOVE 7                TO F-CBFIELDNAME
            MOVE DR-ACC-FAX       TO F-NAMEFIELD
            MOVE 20               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "A-EMAIL"        TO F-FIELDNAME
            MOVE 7                TO F-CBFIELDNAME
            MOVE DR-ACC-EMAIL     TO F-NAMEFIELD
            MOVE 40               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF NEW-DEBTORNO = "Y"
               GO TO FILL-999.
            IF WS-END = "Y"
               GO TO FILL-999.
            GO TO FILL-155.

            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME1" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DR-NAME = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A CHARACTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-175.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
               GO TO FILL-001.

            PERFORM ERROR-020.
       FILL-010.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "POSTADD1" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-ADDRESS1.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DR-ADDRESS1 = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A CHARACTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-010.
            PERFORM ERROR-020.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "POSTADD2" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-ADDRESS2.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DR-ADDRESS2 = " "
               MOVE "THIS FIELD MAY NOT BE BLANK, ENTER A CHARACTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-015.
            PERFORM ERROR-020.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "POSTADD3" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-ADDRESS3.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-020.
            PERFORM ERROR-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "POSTCODE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPOST
                                 DR-POST-CODE.
            PERFORM WRITE-FIELD-POST.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-025.
            PERFORM ERROR-020.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELADD1" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-DEL-ADDRESS1.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-030.
            PERFORM ERROR-020.
       FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELADD2" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-DEL-ADDRESS2.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-035.
            PERFORM ERROR-020.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELADD3" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-DEL-ADDRESS3.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-040.
            PERFORM ERROR-020.
       FILL-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TELEPHONE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-TELEPHONE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
                 MOVE "INVALID KEY." TO WS-MESSAGE
                PERFORM ERROR-000
                GO TO FILL-045.
            PERFORM ERROR-020.
       FILL-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TELEX" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-TELEX.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-050.
            PERFORM ERROR-020.
       FILL-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-GSTNO.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-055.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-055.
            PERFORM ERROR-020.
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESANAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 DR-SALES-ANALYSIS.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DR-SALES-ANALYSIS = 6 OR = 52
             IF DR-GSTNO NOT = "EXPORT"
               MOVE "IF ANALYSIS = 6 OR 52, GST-NO MUST = EXPORT."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-055.
            IF DR-GSTNO = "EXPORT"
             IF DR-SALES-ANALYSIS NOT = 6 AND NOT = 52
               MOVE "IF GST-NO = EXPORT, ANALYSIS MUST BE 6 OR 52."
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-055.
            IF DR-SALES-ANALYSIS NOT > 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A VALID NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-060.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-060.
            PERFORM ERROR-020.
       FILL-065.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CREDITLIMIT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE  TO F-EDNAMEFIELDACC
                                  DR-CREDIT-LIMIT.
            PERFORM WRITE-FIELD-ACCOUNT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DR-CREDIT-LIMIT NOT > 0
               MOVE "THIS FIELD MUST BE > 0, ENTER A VALID NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-065.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-065.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-065.
            PERFORM ERROR-020.
       FILL-070.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCBALANCE" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-BALANCE.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-065.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-070.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-070.
            PERFORM ERROR-020.
       FILL-075.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BALANCELASTSTATE" TO F-FIELDNAME.
            MOVE 16 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-BAL-LAST-STATE.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-070.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-075.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-075.
            PERFORM ERROR-020.
       FILL-080.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CURRENTBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-CURRENT.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-075.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-080.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-080.
            PERFORM ERROR-020.
       FILL-085.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "30DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-30DAY.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-080.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-085.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-085.
            PERFORM ERROR-020.
       FILL-090.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "60DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-60DAY.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-085.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-090.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-090.
            PERFORM ERROR-020.
       FILL-095.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "90DAYBAL" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-90DAY.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-090.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-095.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-095.
            PERFORM ERROR-020.
       FILL-100.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "120DAYBAL" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-120DAY.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-095.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-100.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-100.
            PERFORM ERROR-020.
       FILL-102.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATECREATED" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-102.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO DR-DATE-CREATED.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-102.

            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-100.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-102.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-102.
            PERFORM ERROR-020.
       FILL-105.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATELASTSALE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-105.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO DR-DATE-LAST-SALE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-105.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-102.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-105.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-105.
            PERFORM ERROR-020.
       FILL-110.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATELASTPAY" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-110.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO DR-DATE-LAST-PAY.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-110.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-105.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-110.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-110.
            PERFORM ERROR-020.
       FILL-115.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESPTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-SALES-PTD.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-110.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-115.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-115.
            PERFORM ERROR-020.
       FILL-120.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESYTD" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-SALES-YTD.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-115.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-120.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-120.
            PERFORM ERROR-020.
       FILL-122.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESLAST" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-SALES-LAST.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-120.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-122.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-122.
            PERFORM ERROR-020.
       FILL-125.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COSTPTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-COST-PTD.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-122.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-125.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-125.
            PERFORM ERROR-020.
       FILL-130.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COSTYTD" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-COST-YTD.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-125.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-130.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-130.
            PERFORM ERROR-020.
       FILL-132.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COSTLAST" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM6
                                 DR-COST-LAST.
            PERFORM WRITE-FIELD-NUMERIC6.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-130.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-132.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-132.
            PERFORM ERROR-020.
       FILL-135.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNTCODE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = " "
               MOVE 0 TO F-NAMEFIELD.
            IF F-NAMEFIELD NOT = " " AND NOT = "1" AND NOT = "2"
                       AND NOT = "3" AND NOT = "4" AND NOT = "5"
                       AND NOT = "6" AND NOT = "7" AND NOT = "8"
                       AND NOT = "9" AND NOT = "0"
               MOVE 
             "DISCOUNT CODE MUST BE BETWEEN 0 AND 9, PLEASE RE-ENTER."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO FILL-135.
            MOVE F-NAMEFIELD TO F-EDNAMEFIELDCODE
                                DR-DISCOUNT-CODE.
            PERFORM WRITE-FIELD-CODE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
               
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-135.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-135.
            PERFORM ERROR-020.
       FILL-140.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TERMSCODE" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-EDNAMEFIELDCODE
                                DR-TERMS-CODE.
            PERFORM WRITE-FIELD-CODE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EDNAMEFIELDCODE NOT > "0"
               MOVE "THIS FIELD MUST BE BETWEEN 1 & 9, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-140.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-135.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-140.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-140.
            PERFORM ERROR-020.
       FILL-145.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELIVERYCODE" TO F-FIELDNAME.
            MOVE 12 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO F-EDNAMEFIELDCODE
                                DR-DELIVERY-CODE.
            PERFORM WRITE-FIELD-CODE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EDNAMEFIELDCODE NOT > "0"
               MOVE "THIS FIELD MUST BE BETWEEN 1 & 9, ENTER A NUMBER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-145.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-140.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-145.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-145.
            PERFORM ERROR-020.
       FILL-150.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SUPPLY-Y-N" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-SUPPLY-Y-N.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF DR-SUPPLY-Y-N NOT = "Y" AND NOT = "N" AND NOT = "S"
               MOVE "THIS FIELD MUST BE EITHER 'Y, N OR S', RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-150.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-145.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-150.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-150.
            PERFORM ERROR-020.
       FILL-155.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALESMAN" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-SALESMAN.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
               
            IF F-EXIT-CH = X"01"
               GO TO FILL-195.
      *      IF F-EXIT-CH = X"01"
      *         GO TO FILL-150.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-155.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-155.
            PERFORM ERROR-020.
       FILL-160.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "B-O" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-PART-ORDERS.
            IF DR-PART-ORDERS NOT = "N" AND NOT = "Y"
               MOVE "THIS FIELD MUST BE EITHER N OR Y, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-160.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-155.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-160.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-160.
            PERFORM ERROR-020.
       FILL-165.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CORPGROUP" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE
            PERFORM DECIMALISE-RATE
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDGROUP DR-CORPORATE-GROUP.
            PERFORM WRITE-FIELD-GROUP.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-160.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-165.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-165.
            PERFORM ERROR-020.
       FILL-170.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACC-CONTACT" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-ACCOUNTS-CONTACT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-165.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-170.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-170.
            PERFORM ERROR-020.
       FILL-175.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALES-CONTACT" TO F-FIELDNAME.
            MOVE 13 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-SALES-CONTACT.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-170.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-175.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-175.
            PERFORM ERROR-020.
       FILL-185.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACC-PHONE" TO F-FIELDNAME.
            MOVE 9           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-ACC-PHONE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
               
            IF F-EXIT-CH = X"01"
               GO TO FILL-175.
      *      IF DR-ACC-PHONE = " "
      *         MOVE "ACCOUNT NUMBER NOT > SPACES." TO WS-MESSAGE
      *         PERFORM ERROR-000
      *         GO TO FILL-185.
            IF NEW-DEBTORNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               GO TO FILL-185.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-185.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-185.
            PERFORM ERROR-020.
       FILL-190.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACC-FAX"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-ACC-FAX.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
               
            IF F-EXIT-CH = X"01"
               GO TO FILL-185.
      *      IF DR-ACC-FAX = " "
      *         MOVE "ACCOUNT FAX NUMBER NOT > SPACES." TO WS-MESSAGE
      *         PERFORM ERROR-000
      *         GO TO FILL-190.
            IF NEW-DEBTORNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               GO TO FILL-190.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-190.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-190.
            PERFORM ERROR-020.
       FILL-192.
            MOVE "THIS FIELD MUST BE ENTERED IN lower case ONLY"
              TO WS-MESSAGE
              PERFORM ERROR1-000.
       
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "S-EMAIL" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40        TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-SALES-EMAIL.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
               
            IF F-EXIT-CH = X"01"
               GO TO FILL-190.
      *      IF DR-SALES-EMAIL = " "
      *         MOVE "SALES EMAIL NOT > SPACES." TO WS-MESSAGE
      *         PERFORM ERROR-000
      *         GO TO FILL-192.

            PERFORM ERROR1-020.

            IF NEW-DEBTORNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               GO TO FILL-192.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-192.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-192.
            PERFORM ERROR-020.
       FILL-195.
            MOVE "THIS FIELD MUST BE ENTERED IN lower case ONLY"
              TO WS-MESSAGE
              PERFORM ERROR1-000.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "A-EMAIL"   TO F-FIELDNAME.
            MOVE 7           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20          TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO DR-ACC-EMAIL.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
               
            IF F-EXIT-CH = X"01"
               GO TO FILL-192.
      *      IF DR-A-EMAIL = " "
      *         MOVE "ACCOUNT EMAIL NOT > SPACES." TO WS-MESSAGE
      *         PERFORM ERROR-000
      *         GO TO FILL-195.
               
            MOVE "Y" TO WS-ALL-ENTERED.
            PERFORM ERROR1-020.
               
            IF NEW-DEBTORNO = "Y"
             IF WS-ALL-ENTERED NOT = "Y"
              IF F-EXIT-CH NOT = X"0A"
               GO TO FILL-195.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-NEXT
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM READ-DEBTOR-PREVIOUS
               PERFORM GET-003 THRU GET-005
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-DEBTOR-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-195.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               MOVE "INVALID KEY." TO WS-MESSAGE
               PERFORM ERROR-000
               GO TO FILL-195.

            PERFORM ERROR-020.
            GO TO FILL-155.
      *      GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-DEBTOR-RECORD SECTION.
       DDR-000.
           MOVE "DELETE NOT ALLOWED IN THIS PROGRAM." TO WS-MESSAGE.
           PERFORM ERROR-MESSAGE.
           GO TO DDR-999.
       DDR-010.
           DELETE DEBTOR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR BUSY ON DELETE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO DDR-010.
       DDR-999.
           EXIT.
      *
       RELEASE-DEBTOR-RECORD SECTION.
       REL-000.
           UNLOCK DEBTOR-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-DEBTOR-RECORD SECTION.
       RDR-005.
            IF NEW-DEBTORNO = "Y"
               GO TO RDR-020.
       RDR-010.
            REWRITE DEBTOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR BUSY ON REWRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RDR-020.
            GO TO RDR-999.
       RDR-020.
            WRITE DEBTOR-RECORD
                INVALID KEY NEXT SENTENCE.
            IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR BUSY ON WRITE, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RDR-010.
       RDR-999.
            EXIT.
      *
       READ-DEBTOR SECTION.
       RD-000.
           MOVE 0 TO WS-DEBTOR-ST1.
           MOVE DR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER.
           START DEBTOR-MASTER KEY NOT < DR-ACCOUNT-NUMBER.
        RD-010.
           READ DEBTOR-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-DEBTOR-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-DEBTORNO
                MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER
                GO TO RD-999.
           IF WS-DEBTOR-ST1 NOT = 0
                MOVE "DEBTOR BUSY ON READ, PRESS 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-DEBTOR-ST1
                GO TO RD-010.
           MOVE "N" TO NEW-DEBTORNO.
           MOVE DR-ACCOUNT-NUMBER TO WS-ACC-SAVE.
       RD-999.
             EXIT.
      *
       START-DEBTOR SECTION.
       ST-DR-000.
              MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER.
              START DEBTOR-MASTER KEY NOT < DR-ACCOUNT-NUMBER.
       ST-DR-999.
             EXIT.
      *
       READ-DEBTOR-NEXT SECTION.
       RDNX-001.
           MOVE 0 TO WS-DEBTOR-ST1.
       RDNX-005. 
           READ DEBTOR-MASTER NEXT WITH LOCK
            AT END
              MOVE 0 TO DR-ACCOUNT-NUMBER
                        WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
              PERFORM CLEAR-FORM
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO RDNX-999.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE "DEBTOR BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR-020
               MOVE 0 TO WS-DEBTOR-ST1
               GO TO RDNX-005.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 
               "DEBTOR BUSY ON READ-START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-ACCOUNTNUMBER
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO RDNX-005.
           MOVE DR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER
                                     WS-ACC-SAVE.
           MOVE "N" TO NEW-DEBTORNO.
       RDNX-999.
           EXIT.
      *
       READ-DEBTOR-PREVIOUS SECTION.
       RDPR-001.
           MOVE 0 TO WS-DEBTOR-ST1.
       RDPR-005. 
           READ DEBTOR-MASTER PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO DR-ACCOUNT-NUMBER
                        WS-ACCOUNTNUMBER
              MOVE "Y" TO WS-END
              PERFORM CLEAR-FORM
           MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
              TO WS-MESSAGE
              PERFORM ERROR-000
              GO TO RDPR-999.
           IF WS-DEBTOR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTORS BUSY ON READ-PREV, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPR-005.
           IF WS-DEBTOR-ST1 NOT = 0
               MOVE 
               "DEBTOR BUSY ON PREV-START, IN 1 SEC GOING TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-DEBTOR-ST1 TO WS-MESSAGE
               PERFORM ERROR-000
               CALL "C$SLEEP" USING 1
               PERFORM ERROR1-020
               PERFORM ERROR-020
               MOVE 0 TO WS-ACCOUNTNUMBER
               MOVE 0 TO WS-DEBTOR-ST1
               PERFORM START-DEBTOR
               GO TO RDPR-005.
           MOVE DR-ACCOUNT-NUMBER TO WS-ACCOUNTNUMBER
                                     WS-ACC-SAVE.
           MOVE "N" TO NEW-DEBTORNO.
       RDPR-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO DR-NAME
                         DR-ADDRESS1
                         DR-ADDRESS2
                         DR-ADDRESS3
                         DR-DEL-ADDRESS1
                         DR-DEL-ADDRESS2
                         DR-DEL-ADDRESS3
                         DR-TELEPHONE
                         DR-TELEX
                         DR-GSTNO
                         DR-DISCOUNT-CODE
                         DR-TERMS-CODE
                         DR-DELIVERY-CODE
                         DR-SUPPLY-Y-N
                         DR-SALESMAN
                         DR-PART-ORDERS
                         DR-ACCOUNTS-CONTACT
                         DR-SALES-CONTACT
                         DR-SALES-EMAIL
                         DR-ACC-PHONE
                         DR-ACC-FAX
                         DR-ACC-EMAIL.
             MOVE 0 TO   DR-ACCOUNT-NUMBER
                         DR-POST-CODE
                         DR-SALES-ANALYSIS
                         DR-CREDIT-LIMIT
                         DR-BALANCE
                         DR-BAL-LAST-STATE
                         DR-CURRENT
                         DR-30DAY
                         DR-60DAY
                         DR-90DAY
                         DR-120DAY
                         DR-DATE-CREATED
                         DR-DATE-LAST-SALE
                         DR-DATE-LAST-PAY
                         DR-SALES-PTD
                         DR-SALES-YTD
                         DR-SALES-LAST
                         DR-COST-PTD
                         DR-COST-YTD
                         DR-COST-LAST
                         DR-CORPORATE-GROUP.
             MOVE WS-ACCOUNTNUMBER TO DR-ACCOUNT-NUMBER.
       CLSC-999.
             EXIT.      
      *
        OPEN-FILES SECTION.
        OPEN-000.
            OPEN I-O DEBTOR-MASTER.
            IF WS-DEBTOR-ST1 NOT = 0
               MOVE 0 TO WS-DEBTOR-ST1
               MOVE "DEBTOR FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *      ACCEPT WS-DATE FROM DATE.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "DrMaster" TO F-FORMNAME
           MOVE 8          TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
        END-OFF SECTION.
        END-000.
            CLOSE DEBTOR-MASTER.
      *      STOP RUN.
            EXIT PROGRAM.
        END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldAccount".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldNumeric5".
       Copy "WriteFieldNumeric6".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldAccount".
       Copy "WriteFieldCredit".
       Copy "WriteFieldPost".
       Copy "WriteFieldCode".
       Copy "WriteFieldGroup".
       Copy "GetSystemY2KDate".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
