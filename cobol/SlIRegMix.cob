        IDENTIFICATION DIVISION.
        PROGRAM-ID. SlIRegMix.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION. 
        FILE-CONTROL.
         Copy "SelectSlRegister".
         Copy "SelectSlRegTemp".

        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdRegister.
           COPY ChlfdRegTemp.
      *
       WORKING-STORAGE SECTION.
       77  NEW-ORDER          PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-INVOICE         PIC 9(6) VALUE 0.
       77  WS-TRANS           PIC 9 VALUE 0.
       01  SPLIT-TIME.
           03  SPLIT-HR       PIC 99.
           03  SPLIT-MIN      PIC 99.
           03  SPLIT-SEC      PIC 99.
       01  WS-INCR-STATUS.
           03  WS-INCR-ST1    PIC 99.
       01  WS-INCR1-STATUS.
           03  WS-INCR1-ST1    PIC 99.
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
           PERFORM CLEAR-SCREEN.
       CONTROL-001.
           MOVE "SlIRegIq"      TO F-FORMNAME
           PERFORM OPEN-FILES.
           PERFORM CLEAR-SCREEN.
       CONTROL-010.
      * THIS VErSION, TODAY 05/11/2026, HAS BEEN CREATED TO MANUALLY
      * IMPORT SPECIFIC iNVOICES, C/NOTES, p=sLIPS ^ QUOTES. ETC
      * WHEN TPPY HAVE GONE MISSING FROM THE REGISTER DATA SET 
      * THE WAY IT WORKS IS:
      *1.  THIS VERSION HAS BEEN CHANGED TO FIRST READ THE REGISTER 
      * DATA SET TEMP
      *    RECORD OF THE COMPANY WHICH ONE SUSPECTS HAS THE MISSING
      *    DATA FILES. 
      *    THE SEARCHABLE DATA FILE "/CTOOLS/DATA01/SLREGTEMP"
      *   THE COMPANY MAIN DATA FILE IS "/CTOOLS/DATA01/SLREGISTER
      *
      * 2. THE LIVE DATA WILL NOW BE IN POSITION ONE SO ONE ENTERS 
      *    FOR E.G. THE INVOICE NUMBER YOU WISH TO CAPTURE.  PRESS 
      *    ENTER TO GET THE PROGRAM TO SEARCH THE TEMP DATA FILE TO
      *    SEE IF IT  EXISTS,  IF IT EXISTS THEN IT'S DISPLAYED AND WHEN
      *    THE  <GO> KEY IS PRESSED , WRITES THIS NEW RECORD INTO THE 
      *    COMPANY THE REGISTER RECORD  WHICH FOR NOW IS
      *    /CTOOLS/DATA01/SLREGISTER
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM FILL-DATA.
           MOVE "SlIRegIq"      TO F-FORMNAME
           PERFORM OPEN-010 THRU OPEN-020.
           GO TO CONTROL-010.
       CONTROL-999.
           EXIT.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO INCR-REC
            MOVE "N" TO NEW-ORDER
                        WS-END.
            MOVE 2910 TO POS
            DISPLAY
          "TRANS TYPE: 1=INV, 3=REPAIR, 4=PSLIP, 6=CRD, 7=B/M, 8=QUOTE."
             AT POS.
            MOVE 3010 TO POS
            DISPLAY
          "*** THIS PROGRAM USED ONLY FOR ADDING BACK TRANS LOST ***"
             AT POS.
       GET-001.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO INCR1-INVOICE.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-INVOICE TO INCR1-INVOICE
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF INCR1-INVOICE NOT > 0
                 MOVE "PLEASE ENTER A VALID Number, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-001.
        GET-002.              
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TRANS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO INCR1-TRANS
                                 F-EDNAMEFIELDIND1.
            PERFORM WRITE-FIELD-INDEX1.
            IF F-EXIT-CH = X"0C"
                 MOVE WS-INVOICE TO INCR1-INVOICE
                 PERFORM START-TRANS
                 PERFORM READ-TRANS-NEXT
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"05"
                 PERFORM READ-TRANS-PREVIOUS
              IF WS-END NOT = "Y"
                 GO TO GET-004
              ELSE
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-000.
            IF F-EXIT-CH = X"04"
                 PERFORM CLEAR-SCREEN
                 PERFORM END-OFF.
            IF F-EXIT-CH = X"07"
                 PERFORM CLEAR-FORM
                 PERFORM DISPLAY-FORM
                 GO TO GET-001.
            IF INCR1-INVOICE NOT > 0
                 MOVE "PLEASE ENTER A VALID Number, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                 PERFORM ERROR-MESSAGE
                 GO TO GET-002.
            PERFORM READ-TRANS.
            IF INCR1-TRANS = 0
                PERFORM CLEAR-FORM
                GO TO GET-999.
            IF INCR1-TRANS = 7
               GO TO GET-020.
            GO TO GET-005.
        GET-004.
            IF F-FORMNAME NOT = "SlIRegIq"
               MOVE "SlIRegIq" TO F-FORMNAME
               PERFORM OPEN-010 THRU OPEN-020
               PERFORM DISPLAY-FORM.

            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-INVOICE TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.

            MOVE "TRANS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR1-TRANS TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
            
      *DISPLAY OF REGISTER FILE FOR ALL CODES EXCEPT 7
      * 7 = B/MATERIAL
        GET-005.
            IF INCR1-TRANS = 7
               GO TO GET-020.
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-ACCOUNT TO F-EDNAMEFIELDACC.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ACCOUNT.

            MOVE "PORDER" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR1-PORDER TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR1-GSTNO TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE INCR1-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "SALES" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR1-SALES TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "INVCREDAMT"     TO F-FIELDNAME
            MOVE 10               TO F-CBFIELDNAME
            MOVE INCR1-INVCRED-AMT TO F-EDNAMEFIELD99Mil
            MOVE 11               TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "TAX"    TO F-FIELDNAME
            MOVE 3        TO F-CBFIELDNAME
            MOVE INCR1-TAX TO F-EDNAMEFIELD99Mil
            MOVE 11       TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ADDONS"    TO F-FIELDNAME
            MOVE 6           TO F-CBFIELDNAME
            MOVE INCR1-ADDONS TO F-EDNAMEFIELD99Mil
            MOVE 11          TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "DISCOUNT"    TO F-FIELDNAME
            MOVE 8             TO F-CBFIELDNAME
            MOVE INCR1-DISCOUNT TO F-EDNAMEFIELD99Mil
            MOVE 11            TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "INVCREDCOST"     TO F-FIELDNAME
            MOVE 11                TO F-CBFIELDNAME
            MOVE INCR1-INVCRED-COST TO F-EDNAMEFIELD99Mil
            MOVE 11                TO F-CBFIELDLENGTH
            PERFORM WRITE-FIELD-99Mil.

            MOVE "SBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR1-SB-TYPE TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DRTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE INCR1-DRTRANS-NO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "PBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR1-PULLBY TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-DATE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF INCR1-PULL-DATE = 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR1-PULL-DATE TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-TIME" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF INCR1-PULL-TIME = 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR1-PULL-TIME TO ALPHA-RATE
                PERFORM TIME-CHECKING
                MOVE WS-DATE-CHECK TO F-NAMEFIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRINTED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-PRINTED TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELAREA" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-AREA TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUM" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-COPY-NUMBER TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "PART-ORDERS"    TO F-FIELDNAME.
            MOVE 11               TO F-CBFIELDNAME.
            MOVE INCR1-PART-ORDERS TO F-NAMEFIELD.
            MOVE 1                TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR1-NAME TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR1-ADD1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR1-ADD2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR1-ADD3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CODE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR1-CODE TO F-EDNAMEFIELDPOST.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-POST.

            MOVE "DEL1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR1-DEL1 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR1-DEL2 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            MOVE INCR1-DEL3 TO F-NAMEFIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "TERMS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR1-TERMS TO F-NAMEFIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PHONE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR1-PHONE TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CONTACT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-CONTACT TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DELIVERY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            MOVE INCR1-DELIVERY TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BIN" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE INCR1-BIN TO F-NAMEFIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COMMENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-COMMENT TO F-NAMEFIELD.
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BOINVNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-BO-INV-NO TO F-EDNAMEFIELDNUM.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-NUMERIC.

            MOVE "BO-DATE"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE INCR1-BO-DATE TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "ADDPOST" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-ADDPOST TO F-EDNAMEFIELD99Mil.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ADDFREIGHT" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE INCR1-ADDFREIGHT TO F-EDNAMEFIELD99Mil.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ADDLABOUR" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            MOVE INCR1-ADDLABOUR TO F-EDNAMEFIELD99Mil.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "ADDMISC" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-ADDMISC TO F-EDNAMEFIELD99Mil.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "LINENO" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR1-LINENO TO F-EDNAMEFIELDLINE.
            MOVE 3 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-LINE.
       GET-010.
            GO TO GET-999.
            
      *START OF DISPLAY FOR B/MATERIAL REGISTER INFO
       GET-020.
            PERFORM FILL-2000.
            IF NEW-ORDER = "Y"
               GO TO GET-999.
       GET-025.
            IF F-FORMNAME NOT = "SlRgBmIq"
               MOVE "SlRgBmIq" TO F-FORMNAME
               PERFORM OPEN-010 THRU OPEN-020
               PERFORM DISPLAY-FORM.

            MOVE "INVOICE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-INVOICE TO F-EDNAMEFIELDINV.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INV.

            MOVE "TRANS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR1-TRANS TO F-EDNAMEFIELDIND1.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-INDEX1.
            
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-ACCOUNT TO F-EDNAMEFIELDACC.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ACCOUNT.

            MOVE "PORDER" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR1-PORDER TO F-NAMEFIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE INCR1-GSTNO TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DATE"       TO F-FIELDNAME.
            MOVE 4            TO F-CBFIELDNAME.
            MOVE INCR1-DATE    TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE INCR1-PULLBY TO F-NAMEFIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-DATE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF INCR1-PULL-DATE = 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR1-PULL-DATE TO SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "DEL-TIME" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            IF INCR1-PULL-TIME = 0
                MOVE " " TO F-NAMEFIELD
            ELSE
                MOVE INCR1-PULL-TIME TO ALPHA-RATE
                PERFORM TIME-CHECKING
                MOVE WS-DATE-CHECK TO F-NAMEFIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PRINTED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-PRINTED TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "COPYNUM" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE INCR1-COPY-NUMBER TO F-EDNAMEFIELDANAL.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ANALYSIS.

            MOVE "KITNAME"    TO F-FIELDNAME.
            MOVE 7            TO F-CBFIELDNAME.
            MOVE INCR1-KITNAME TO F-NAMEFIELD.
            MOVE 15           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITQTY"    TO F-FIELDNAME.
            MOVE 6           TO F-CBFIELDNAME.
            MOVE INCR1-KITQTY TO F-EDNAMEFIELDGROUP.
            MOVE 3           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-GROUP.

            MOVE "KITSHPDQTY"    TO F-FIELDNAME.
            MOVE 10              TO F-CBFIELDNAME.
            MOVE INCR1-KITSHPDQTY TO F-EDNAMEFIELDGROUP.
            MOVE 3               TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-GROUP.

            MOVE "KITDESC1"    TO F-FIELDNAME.
            MOVE 8             TO F-CBFIELDNAME.
            MOVE INCR1-KITDESC1 TO F-NAMEFIELD.
            MOVE 20            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITDESC2"    TO F-FIELDNAME.
            MOVE 8             TO F-CBFIELDNAME.
            MOVE INCR1-KITDESC2 TO F-NAMEFIELD.
            MOVE 20            TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "KITPRICE"    TO F-FIELDNAME.
            MOVE 8             TO F-CBFIELDNAME.
            MOVE INCR1-KITPRICE TO F-EDNAMEFIELD99Mil.
            MOVE 11             TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-99Mil.

            MOVE "KITCOMMENT"    TO F-FIELDNAME.
            MOVE 10              TO F-CBFIELDNAME.
            MOVE INCR1-KITCOMMENT TO F-NAMEFIELD.
            MOVE 40              TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
           IF WS-END = "Y"
                 GO TO FILL-999.
           IF INCR1-TRANS = 7
               GO TO FILL-2000.
      *
      *FILL-IN-ALL-OTHER-FIELDS.
      *
       FILL-2000.
            IF F-FORMNAME NOT = "SlIRegIq"
               MOVE "SlIRegIq" TO F-FORMNAME
               PERFORM OPEN-010 THRU OPEN-020
               PERFORM DISPLAY-FORM
               PERFORM GET-004
               PERFORM GET-005.
               
            PERFORM ERROR1-020.
            MOVE 2910 TO POS.
            DISPLAY 
            "THESE ARE INV, REPAIR, P/SLIP, QUOTE & C/NOTE FIELDS."
            AT POS.
           
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ACCOUNT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 7 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDACC
                                 INCR1-ACCOUNT.
            PERFORM WRITE-FIELD-ACCOUNT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-145.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
       FILL-012.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PORDER" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-PORDER.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-012.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-012.
       FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "GSTNO" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-GSTNO.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-012.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
       FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DATE"  TO F-FIELDNAME.
            MOVE 4       TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10      TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-020.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO INCR1-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-020.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
       FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SALES" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 INCR1-SALES.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF INCR1-SALES = 0
               MOVE "THIS FIELD MUST BE > 0, PLEASE RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
       FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVCREDAMT" TO F-FIELDNAME.
            MOVE 10           TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 INCR1-INVCRED-AMT.
            PERFORM WRITE-FIELD-99Mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
       FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TAX"        TO F-FIELDNAME.
            MOVE 3            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 INCR1-TAX.
            PERFORM WRITE-FIELD-99Mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
       FILL-040.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADDONS"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 INCR1-ADDONS.
            PERFORM WRITE-FIELD-99Mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-035.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-040.
       FILL-045.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DISCOUNT"   TO F-FIELDNAME.
            MOVE 8            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11           TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD  TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELD99Mil
                                 INCR1-DISCOUNT.
            PERFORM WRITE-FIELD-99Mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-040.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-045.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
       FILL-050.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "INVCREDCOST" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11            TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD   TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE  TO F-EDNAMEFIELD99Mil
                                  INCR1-INVCRED-COST.
            PERFORM WRITE-FIELD-99Mil.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-045.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-050.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-050.
       FILL-055.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "SBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-SB-TYPE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF INCR1-SB-TYPE = "  "
               MOVE "THIS FIELD MUST BE > SPACES, RE-ENTER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-050.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-055.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-055.
       FILL-060.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DRTRANSNO" TO F-FIELDNAME.
            MOVE 9 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 INCR1-DRTRANS-NO.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-055.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-060.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-060.
       FILL-061.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PBTYPE" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-PULLBY.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-060.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-061.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-061.
       FILL-063.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL-DATE" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-NAMEFIELD = " "
               MOVE 0 TO INCR1-PULL-DATE
               GO TO FILL-064.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-063.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO INCR1-PULL-DATE.
      *      PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-063.
       FILL-064.
            IF F-EXIT-CH = X"01"
               GO TO FILL-061.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-063.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-063.
       FILL-067.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL-TIME" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 8 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-NAMEFIELD = " "
                MOVE 0 TO INCR1-PULL-TIME
                GO TO FILL-068.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM TIME-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-067.
            MOVE WS-NEW-DATE TO INCR1-PULL-TIME.
            MOVE WS-DATE-CHECK TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
       FILL-068.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-063.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-067.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-067.
       FILL-070.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PRINTED" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-PRINTED.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-067.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-070.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-070.
       FILL-072.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELAREA" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-AREA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-070.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-072.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-072.
       FILL-073.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COPYNUM" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 2 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDANAL
                                 INCR1-COPY-NUMBER.
            PERFORM WRITE-FIELD-ANALYSIS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-072.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-073.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-073.
       FILL-074.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PART-ORDERS" TO F-FIELDNAME.
            MOVE 11            TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1             TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-PART-ORDERS.
            PERFORM WRITE-FIELD-ALPHA.
            IF INCR1-PART-ORDERS NOT = "N" AND NOT = "Y"
               MOVE "THE FIELD MUST BE EITHER N OR Y, RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-074.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-073.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-074.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-074.
       FILL-075.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "NAME" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-NAME.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-074.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-075.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-075.
       FILL-080.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADD1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-ADD1.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-075.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-080.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-080.
       FILL-085.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADD2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-ADD2.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-080.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-085.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-085.
       FILL-090.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "ADD3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-ADD3.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-085.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-090.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-090.
       FILL-095.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CODE" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 4 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDPOST
                                 INCR1-CODE.
            PERFORM WRITE-FIELD-POST.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-090.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-095.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-095.
       FILL-100.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL1" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-DEL1.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-095.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-100.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-100.
       FILL-105.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL2" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-DEL2.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-100.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-105.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-105.
       FILL-110.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DEL3" TO F-FIELDNAME.
            MOVE 4 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 25 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-DEL3.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-105.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-110.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-110.
       FILL-115.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "TERMS" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 11 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-TERMS.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-110.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-115.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-115.
       FILL-117.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PHONE" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-PHONE.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-115.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-117.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-117.
       FILL-120.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CONTACT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-CONTACT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-117.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-120.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-120.
       FILL-125.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "DELIVERY" TO F-FIELDNAME.
            MOVE 8 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 20 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-DELIVERY.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-115.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-125.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-125.
       FILL-130.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BIN" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-BIN.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-125.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-130.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-130.
       FILL-135.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "COMMENT" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 30 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO INCR1-COMMENT.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"01"
               GO TO FILL-130.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-135.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-135.
       FILL-140.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BOINVNO" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 6 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDNUM
                                 INCR1-BO-INV-NO.
            PERFORM WRITE-FIELD-NUMERIC.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"01"
               GO TO FILL-135.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-TRANS
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"04"
               MOVE
            "PRESS 'ESC' TO CLEAR THE CURRENT INPUT BEFORE 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-140.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-140.
       FILL-145.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BO-DATE" TO F-FIELDNAME.
            MOVE 7         TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-EXIT-CH = X"07"
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-145.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO INCR1-BO-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-145.
            IF F-EXIT-CH = X"01"
               GO TO FILL-140.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-NEXT
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001
             ELSE
               PERFORM GET-025
               GO TO FILL-2000.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-TRANS-RECORD
               PERFORM READ-TRANS-PREVIOUS
             IF INCR1-TRANS NOT = 7
               PERFORM GET-004
               PERFORM GET-005
               GO TO FILL-001.
       FILL-999.
            EXIT.
      *
       DELETE-TRANS SECTION.
       DO-000.
      *      IF NEW-ORDER = "Y"
      * FOR NOW WE WILL NOT BE DELETING ANY RECORD OUT OF THE BACKUP
      *  DATA SET.
      
              GO TO DO-999.
       DO-010.
            DELETE INCR-REGISTER
               INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER RECORD BUSY ON DELETE, BE PATIENT"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO DO-010.
       DO-999.
           EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO INCR1-GSTNO
                         INCR1-SB-TYPE
                         INCR1-PRINTED
                         INCR1-AREA
                         INCR1-NAME
                         INCR1-ADD1
                         INCR1-ADD2
                         INCR1-ADD3
                         INCR1-DEL1
                         INCR1-DEL2
                         INCR1-DEL3
                         INCR1-TERMS
                         INCR1-PHONE
                         INCR1-CONTACT
                         INCR1-PORDER
                         INCR1-DELIVERY
                         INCR1-BIN
                         INCR1-COMMENT
                         INCR1-PART-ORDERS.
             MOVE 0   TO INCR1-INVOICE
                         INCR1-TRANS
                         INCR1-COPY-NUMBER
                         INCR1-ACCOUNT
                         INCR1-DATE
                         INCR1-INVCRED-AMT
                         INCR1-TAX
                         INCR1-ADDONS
                         INCR1-DISCOUNT
                         INCR1-INVCRED-COST
                         INCR1-DRTRANS-NO
      *                   INCR1-STTRANS-NO
                         INCR1-CODE
                         INCR1-BO-INV-NO
                         INCR1-BO-DATE
                         INCR1-ADDPOST
                         INCR1-ADDFREIGHT
                         INCR1-ADDLABOUR
                         INCR1-ADDMISC
                         INCR1-LINENO.
             MOVE WS-INVOICE TO INCR1-INVOICE.
             MOVE WS-TRANS   TO INCR1-TRANS.
             IF WS-INCR1-ST1 = 51
                UNLOCK INCR1-REGISTER.
       CLSC-999.
             EXIT.      
      *
      *THIS IS THE AMENDED SECTION WHERE WE WRITE THE SL HEADER
      * RECORD FROM THE SLREGTEMP DATA SET 
      *
        REWRITE-TRANS-RECORD SECTION.
        RDR-005.
             MOVE INCR1-REC TO INCR-REC.
        ROR-010.
            IF NEW-ORDER = "Y"
               GO TO ROR-020.
            REWRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER RECORD BUSY ON REWRITE, BE PATIENT"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO ROR-010.
            GO TO ROR-999.
       ROR-020.
            WRITE INCR-REC
                INVALID KEY NEXT SENTENCE.
            IF WS-INCR-ST1 NOT = 0
               MOVE "REGISTER RECORD BUSY ON WRITE, BE PATIENT"
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-INCR-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               
               PERFORM ERROR1-020
               MOVE 0 TO WS-INCR-ST1
               GO TO ROR-020.
       ROR-999.
            EXIT.
      *
       READ-TRANS SECTION.
       RO-000.
           MOVE 0 TO WS-INCR1-ST1.
           MOVE INCR1-INVOICE TO WS-INVOICE.
           MOVE INCR1-TRANS   TO WS-TRANS.
           START INCR1-REGISTER KEY NOT < INCR1-KEY
              INVALID KEY NEXT SENTENCE.
       RO-010.
           READ INCR1-REGISTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
           IF WS-INCR1-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-INCR1-ST1
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-ORDER
                GO TO RO-999.
           IF WS-INCR1-ST1 NOT = 0
                MOVE "REGISTER BUSY ON READ-LOCK, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR1-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR1-ST1
                GO TO RO-010.
           MOVE "N" TO NEW-ORDER.
           MOVE INCR1-INVOICE TO WS-INVOICE.
           MOVE INCR1-TRANS   TO WS-TRANS.
       RO-999.
           EXIT.
      *
       START-TRANS SECTION.
       ST-OO-000.
           MOVE WS-INVOICE TO INCR1-INVOICE.
           MOVE WS-TRANS   TO INCR1-TRANS.
           START INCR1-REGISTER KEY NOT < INCR1-KEY
                 INVALID KEY NEXT SENTENCE.
       ST-OO-999.
             EXIT.
      *
       READ-TRANS-NEXT SECTION.
       RONX-001.
           MOVE 0 TO WS-INCR1-ST1.
       RONX-005. 
           READ INCR1-REGISTER NEXT WITH LOCK
            AT END
              MOVE 0 TO INCR1-INVOICE
                        WS-INVOICE
                        INCR1-TRANS
                        WS-TRANS
              CLOSE INCR1-REGISTER
              MOVE "Y" TO WS-END
              MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              OPEN I-O INCR1-REGISTER
              GO TO RONX-999.
           IF WS-INCR1-ST1 NOT = 0
                MOVE "REGISTER BUSY ON READ-NEXT-LOCK, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR1-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR1-ST1
                PERFORM START-TRANS
                GO TO RONX-005.
           MOVE INCR1-INVOICE TO WS-INVOICE.
           MOVE INCR1-TRANS   TO WS-TRANS.
           MOVE "N" TO NEW-ORDER.
       RONX-999.
           EXIT.
      *
       READ-TRANS-PREVIOUS SECTION.
       RPREV-001.
           MOVE 0 TO WS-INCR1-ST1.
       RPREV-005. 
           READ INCR1-REGISTER PREVIOUS WITH LOCK
            AT END
              MOVE 0 TO INCR1-INVOICE
                          WS-INVOICE
                        INCR1-TRANS
                          WS-TRANS
              CLOSE INCR1-REGISTER
              MOVE "Y" TO WS-END
              MOVE "END OF PREV-PAGE SEQUENCE, ENTER A NEW IDENTIFIER."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              OPEN I-O INCR1-REGISTER
              GO TO RPREV-999.
           IF WS-INCR1-ST1 NOT = 0
                MOVE "REGISTER BUSY ON READ-PREV-LOCK, 'ESC' TO RETRY"
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR1-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR1-ST1
                PERFORM START-TRANS
                GO TO RPREV-005.
           MOVE INCR1-INVOICE TO WS-INVOICE.
           MOVE INCR1-TRANS   TO WS-TRANS.
           MOVE "N" TO NEW-ORDER.
        RPREV-999.
           EXIT.
      *
         OPEN-FILES SECTION.
         OPEN-000.
         OPEN I-O INCR-REGISTER.
           IF WS-INCR-ST1 NOT = 0
                MOVE "REGISTER FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR-ST1
                GO TO OPEN-000.
        OPEN-005.
          OPEN I-O INCR1-REGISTER.
          IF WS-INCR1-ST1 NOT = 0
                MOVE "SLREGTEMP FILE BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-INCR1-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                MOVE 0 TO WS-INCR1-ST1
                GO TO OPEN-005.
          OPEN-010.
             MOVE Ws-Forms-Name   TO F-FILENAME
             MOVE Ws-cbForms-name TO F-CBFILENAME.
             CALL "OPENFILE" USING  F-ERROR1
                                    F-FH
                                    F-FILENAME
                                    F-CBFILENAME
                                    F-FILENAME
                                    F-INTEGERZERO
                                    F-OPENMODE.
            IF F-ERROR1 NOT = 0
                DISPLAY "OPENFILE ERROR"
                DISPLAY F-ERROR1
                DISPLAY F-FILENAME
                PERFORM ERROR-010
                STOP RUN.
       OPEN-020.
            MOVE 8 TO F-CBFORMNAME.
            CALL "OPENFORM" USING  F-ERROR2
                                   F-FH
                                   F-FORMNAME
                                   F-CBFORMNAME
                                   F-FORM
                                   F-CBMAX.
            IF F-ERROR2 NOT = 0
                DISPLAY "OPENFORM ERROR"
                DISPLAY F-ERROR2
                DISPLAY F-FORMNAME
                PERFORM ERROR-010
                STOP RUN.
            CALL "CLOSEFILE" USING F-ERROR1
                                   F-FH.
            IF F-ERROR1 NOT = 0
                DISPLAY "CLOSEFILE ERROR"
                DISPLAY F-ERROR1
                PERFORM ERROR-010
                STOP RUN.
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
            CLOSE INCR-REGISTER.
            EXIT PROGRAM.
      *      STOP RUN.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAccount".
       Copy "ReadFieldAlpha".
       Copy "WriteField99Mil".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAccount".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldAnalysis".
       Copy "WriteFieldIndex1".
       Copy "WriteFieldInv".
       Copy "WriteFieldLine".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldPost".
       Copy "WriteFieldPrice".
       Copy "GetSystemY2KDate".
       Copy "WriteFieldGroup".
       Copy "WriteFieldAmount".
      ******************
      *Mandatory Copies*
      ******************
       Copy "DisplayForm".
       Copy "UserFillField".
       Copy "DecimaliseRate".
       Copy "ConvertDateFormat".
       Copy "TimeChecking".
       Copy "ClearScreen".
       Copy "ErrorMessage".
       Copy "Error1Message".
      *
      * END-OF-JOB
