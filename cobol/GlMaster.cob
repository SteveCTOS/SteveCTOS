        IDENTIFICATION DIVISION.
        PROGRAM-ID. GlMaster.
        AUTHOR. CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        Copy "SelectGlMaster".
        Copy "SelectGlParameter".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY ChlfdGlMast.
           COPY ChlfdGlParam.

       WORKING-STORAGE SECTION.
       77  WS-TYPE            PIC X VALUE " ".
       77  WS-BUDGET          PIC S9(8)V99 VALUE 0.
       77  WS-YEAR-BU         PIC S9(8)V99 VALUE 0.
       77  WS-MARGIN          PIC S9(8)V99 VALUE 0.
       77  WS-TOTALMV         PIC S9(8)V99 VALUE 0.
       77  WS-TOTALBU         PIC S9(8)V99 VALUE 0.
       77  WS-TOTALVA         PIC S9(8)V99 VALUE 0.
       77  WS-TOTALLY         PIC S9(8)V99 VALUE 0.
       77  WS-INQUIRY         PIC X(8) VALUE "GlNameIq".
       77  NEW-GLNO           PIC X VALUE " ".
       77  WS-HEAD-VALID      PIC X VALUE " ".
       77  WS-SUB-VALID       PIC X VALUE " ".
       77  WS-END             PIC X VALUE " ".
       77  WS-NUMBER          PIC X(12) VALUE " ".
       77  SUB-1-SAVE         PIC S9(5) VALUE 0.
       01  WS-GLACC.
           03  WS-SUBHEADER.
               05  WS-GLHEADER     PIC X(2).
               05  WS-GLSUBHEADER  PIC X(4).
           03  WS-REST             PIC X(6).
       01  WS-MONTH-BUDGET.
           03  WS-MONTH-BU   PIC S9(8)V99 OCCURS 13.
       01  WS-GLMAST-STATUS.
           03  WS-GLMAST-ST1   PIC 99.
      *     03  WS-GLMAST-ST2   PIC X.
       01  WS-GLPARAMETER-STATUS.
           03  WS-GLPARAMETER-ST1   PIC 99.
      *     03  WS-GLPARAMETER-ST2   PIC X.
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
       CONTROL-010.
           PERFORM DISPLAY-FORM.
           PERFORM GET-DATA.
           PERFORM FILL-DATA.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
            MOVE " " TO GL-RECORD.
            MOVE "N" TO NEW-GLNO
                        WS-END.
            MOVE 0 TO WS-TOTALMV WS-TOTALBU WS-TOTALVA.

            MOVE "                        " TO F-NAMEFIELD ALPHA-RATE.
       GET-001.
            MOVE "                        " TO F-NAMEFIELD.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
            IF ALPHA-RATE > SPACES
               PERFORM NUMBER-CHECK
               MOVE WS-GLNO-CHECK TO GL-NUMBER
            IF SIGN-FOUND = 9
               MOVE 0 TO SIGN-FOUND
               GO TO GET-001.
            IF F-EXIT-CH = X"0C"
                MOVE WS-NUMBER TO GL-NUMBER
                PERFORM START-GL
                PERFORM READ-GL-NEXT
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-EXIT-CH = X"05"
                PERFORM READ-GL-PREVIOUS
             IF WS-END NOT = "Y"
               GO TO GET-003
             ELSE
               PERFORM CLEAR-FORM
               GO TO GET-000.
            IF F-NAMEFIELD = " "
               CLOSE GL-MASTER
               CALL WS-INQUIRY USING WS-LINKAGE
               PERFORM OPEN-000
               PERFORM CLEAR-SCREEN
               PERFORM DISPLAY-FORM
               GO TO GET-000.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO GET-001.

            PERFORM READ-GL.
            IF NEW-GLNO = "Y"
                PERFORM CLSC-000 THRU CLSC-010
                MOVE WS-GLNO-CHECK TO GL-NUMBER
                MOVE WS-TYPE   TO GL-P-B
                MOVE "CRDATE"  TO F-FIELDNAME
                MOVE 6         TO F-CBFIELDNAME
                MOVE WS-DATE   TO GL-DATE SPLIT-DATE
                PERFORM CONVERT-DATE-FORMAT
                MOVE DISPLAY-DATE TO F-NAMEFIELD
                MOVE 10        TO F-CBFIELDLENGTH
                PERFORM WRITE-FIELD-ALPHA
                PERFORM GET-003 THRU GET-005
                GO TO GET-999.
        GET-003.
            MOVE "ACCNUM" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE GL-NUMBER TO F-NAMEFIELD.
            MOVE 13 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.
        GET-005.
            MOVE "DESCRIPTION" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE GL-DESCRIPTION TO F-NAMEFIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "PB" TO F-FIELDNAME.
            MOVE 2 TO F-CBFIELDNAME.
            MOVE GL-P-B TO F-NAMEFIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "CRDATE"     TO F-FIELDNAME.
            MOVE 6            TO F-CBFIELDNAME.
            MOVE GL-DATE      TO SPLIT-DATE.
            PERFORM CONVERT-DATE-FORMAT.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            MOVE 10           TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GL-BALANCE TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENPERBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            MOVE GL-OPEN-PER-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "OPENYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE GL-OPEN-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "LASTYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            MOVE GL-LAST-YEAR-BAL TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "YEARBU" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            MOVE "            " TO F-NAMEFIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-ALPHA.

            MOVE 1 TO SUB-1 F-INDEX.
       GET-006.
            MOVE "PER" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            MOVE GL-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-006.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-020.
            MOVE "PERBU" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            MOVE GL-PER-BU (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-020.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-025.
            IF SUB-1 = 1
               MOVE 0 TO WS-TOTALVA.

            MOVE "VAR" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            IF SUB-1 NOT > GLPA-CURRENT-GLPER
               COMPUTE WS-MARGIN = GL-PER (SUB-1) - GL-PER-BU (SUB-1)
               MOVE WS-MARGIN TO F-EDNAMEFIELDREC
               ADD WS-MARGIN TO WS-TOTALVA
            ELSE
               MOVE 0 TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-025.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-030.
            MOVE "LASTPER" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE GL-LAST-PER (SUB-1) TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
                GO TO GET-030.
            MOVE 1 TO SUB-1 F-INDEX.
       GET-050.
            PERFORM ADD-TOTALS.

            MOVE "TOTALMV" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALMV TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALBU" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALBU TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALVA" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALVA TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.

            MOVE "TOTALLY" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            MOVE WS-TOTALLY TO F-EDNAMEFIELDREC.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM WRITE-FIELD-REC.
       GET-999.
            EXIT.
      *
       FILL-DATA SECTION.
       FILL-001.
            IF WS-HEAD-VALID NOT = "Y"
                  GO TO FILL-999.
            IF WS-SUB-VALID NOT = "Y"
                  GO TO FILL-999.
            MOVE "DESCRIPTION" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 40 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GL-DESCRIPTION.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GL-DESCRIPTION = " "
               MOVE "This Field May Not Be Blank, Enter A Character"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-001.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-001.
        FILL-010.
            MOVE "PB" TO F-FIELDNAME.
            MOVE 2 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 1 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO GL-P-B.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF GL-P-B NOT = "P" AND NOT = "B"
               MOVE "THIS FIELD MUST BE EITHER 'P' OR 'B', RE-ENTER."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-001.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-010.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-010.
        FILL-012.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "YEARBU" TO F-FIELDNAME.
            MOVE 6 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            IF F-CBFIRSTLINE NOT = 0
               MOVE F-NAMEFIELD TO ALPHA-RATE
               PERFORM DECIMALISE-RATE
               MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                    WS-YEAR-BU
               PERFORM WRITE-FIELD-REC
               PERFORM CALCULATE-YEAR-BU.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-010.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-012.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-012.
        FILL-015.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "CRDATE" TO F-FIELDNAME.
            MOVE 6        TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 10       TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            PERFORM DATE-CHECKING.
            IF SIGN-FOUND = 9
               GO TO FILL-015.
            MOVE WS-NEW-DATE TO WS-CH-DATE CONVERT-DATE.
            MOVE WS-CONVERT-DATE TO DISPLAY-DATE.
            MOVE DISPLAY-DATE TO F-NAMEFIELD.
            PERFORM WRITE-FIELD-ALPHA.
            PERFORM CONVERT-SPLIT-FORMAT.
            MOVE SPLIT-DATE TO GL-DATE.
            PERFORM CHECK-DATE-VALID.
            IF SIGN-FOUND = 9
               GO TO FILL-015.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-012.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-015.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-015.
        FILL-020.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "BALANCE" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 GL-BALANCE.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-015.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-020.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-020.
        FILL-025.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OPENPERBAL" TO F-FIELDNAME.
            MOVE 10 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 GL-OPEN-PER-BAL.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-020.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-025.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-025.
        FILL-030.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "OPENYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 GL-OPEN-YEAR-BAL.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-025.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-030.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-030.
        FILL-035.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTYEARBAL" TO F-FIELDNAME.
            MOVE 11 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 GL-LAST-YEAR-BAL.
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
               GO TO FILL-030.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-035.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-035.
        FILL-040.
            MOVE 1 TO SUB-1 F-INDEX.
        FILL-045.
            MOVE "                          " TO F-NAMEFIELD.
            MOVE "PER" TO F-FIELDNAME.
            MOVE 3 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 GL-PER (SUB-1).
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               GO TO FILL-035
             ELSE
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-045.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-040.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-045.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
             IF F-EXIT-CH = X"1D"
               MOVE GL-PER (SUB-2) TO GL-PER (SUB-1)
                                      F-EDNAMEFIELDREC
               PERFORM WRITE-FIELD-REC
               GO TO FILL-045
              ELSE
               GO TO FILL-045.

            PERFORM GET-050.
            MOVE 1 TO SUB-1 F-INDEX.
        FILL-100.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "PERBU" TO F-FIELDNAME.
            MOVE 5 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 GL-PER-BU (SUB-1).
            PERFORM WRITE-FIELD-REC.

            MOVE SUB-1 TO SUB-1-SAVE.
            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM GET-025.
            PERFORM GET-050.
            MOVE SUB-1-SAVE TO SUB-1 F-INDEX.
            
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               MOVE 12 TO SUB-1 F-INDEX
               GO TO FILL-045
             ELSE
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-100.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-100.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B" AND NOT = X"1D"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-100.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
             IF F-EXIT-CH = X"1D"
               MOVE GL-PER-BU (SUB-2) TO GL-PER-BU (SUB-1)
                                         F-EDNAMEFIELDREC
               PERFORM WRITE-FIELD-REC
               GO TO FILL-100
              ELSE
               GO TO FILL-100.

            PERFORM GET-050.
        FILL-160.
            MOVE 1 TO SUB-1 F-INDEX.
        FILL-165.
            MOVE "                    " TO F-NAMEFIELD.
            MOVE "LASTPER" TO F-FIELDNAME.
            MOVE 7 TO F-CBFIELDNAME.
            PERFORM USER-FILL-FIELD.
            MOVE 12 TO F-CBFIELDLENGTH.
            PERFORM READ-FIELD-ALPHA.
            MOVE F-NAMEFIELD TO ALPHA-RATE.
            PERFORM DECIMALISE-RATE.
            MOVE NUMERIC-RATE TO F-EDNAMEFIELDREC
                                 GL-LAST-PER (SUB-1).
            PERFORM WRITE-FIELD-REC.
            IF F-EXIT-CH = X"07"
               PERFORM RELEASE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
            IF F-EXIT-CH = X"0C"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-NEXT
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"05"
               PERFORM REWRITE-GL-RECORD
               PERFORM READ-GL-PREVIOUS
             IF WS-END = "Y"
               PERFORM CLEAR-FORM
               GO TO FILL-999
             ELSE
               PERFORM GET-003 THRU GET-050
               GO TO FILL-001.
            IF F-EXIT-CH = X"01"
             IF SUB-1 = 1
               MOVE 12 TO SUB-1 F-INDEX
               GO TO FILL-100
             ELSE
               SUBTRACT 1 FROM SUB-1 F-INDEX
               GO TO FILL-165.
            IF F-EXIT-CH = X"1B"
               PERFORM REWRITE-GL-RECORD
               PERFORM CLEAR-FORM
              GO TO FILL-999.
            IF F-EXIT-CH = X"1F"
               PERFORM DELETE-GL-RECORD
               PERFORM CLEAR-FORM
               GO TO FILL-999.
           IF F-EXIT-CH = X"04"
               MOVE
           "Press 'ESC' To Clear The Current Input Before 'END'"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO FILL-160.
            IF F-EXIT-CH NOT = X"0A" AND NOT = X"0B"
               DISPLAY " " AT 3079 WITH BELL
               GO TO FILL-160.
            ADD 1 TO SUB-1 F-INDEX.
            IF SUB-1 < 13
             IF F-EXIT-CH = X"1D"
               MOVE GL-PER-BU (SUB-2) TO GL-PER-BU (SUB-1)
                                         F-EDNAMEFIELDREC
               PERFORM WRITE-FIELD-REC
               GO TO FILL-165
              ELSE
               GO TO FILL-165.
            MOVE 1 TO SUB-1 F-INDEX.

            PERFORM GET-050.
            GO TO FILL-001.
       FILL-999.
             EXIT.
      *
       ADD-TOTALS SECTION.
       ATS-001.
            MOVE 0 TO WS-TOTALMV WS-TOTALBU WS-TOTALLY.
            MOVE 1 TO SUB-1.
       ATS-005.
            COMPUTE WS-TOTALMV = WS-TOTALMV + GL-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-005.
            MOVE 1 TO SUB-1.
       ATS-010.
            COMPUTE WS-TOTALBU = WS-TOTALBU + GL-PER-BU (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-010.
            MOVE 1 TO SUB-1.
       ATS-030.
            COMPUTE WS-TOTALLY = WS-TOTALLY + GL-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                GO TO ATS-030.
            MOVE 1 TO SUB-1.
       ATS-999.
            EXIT.
      *
       CALCULATE-YEAR-BU SECTION.
       CYB-001.
            MOVE 0 TO SUB-1.
       CYB-005.
            COMPUTE WS-BUDGET ROUNDED = WS-YEAR-BU / 12.
       CYB-010.
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
               MOVE WS-BUDGET TO WS-MONTH-BU (SUB-1)
               GO TO CYB-010.
            COMPUTE WS-MARGIN = WS-YEAR-BU - (WS-BUDGET * 12).
            MOVE 12 TO SUB-1.
            ADD WS-MARGIN TO WS-MONTH-BU (SUB-1).
            MOVE 1 TO SUB-1.
       CYB-020.
            MOVE WS-MONTH-BU (SUB-1) TO GL-PER-BU (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
               GO TO CYB-020.

            MOVE 1 TO SUB-1 F-INDEX.
            PERFORM GET-020 THRU GET-025.
            PERFORM GET-050.
       CYB-999.
            EXIT.
      *
       DELETE-GL-RECORD SECTION.
       DSR-000.
            IF NEW-GLNO = "Y"
               GO TO DSR-999.
       DSR-010.
            DELETE GL-MASTER
               INVALID KEY NEXT SENTENCE.
            IF WS-GLMAST-ST1 NOT = 0
               MOVE "GLMASTER BUSY ON DELETE, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               MOVE 0 TO WS-GLMAST-ST1
               GO TO DSR-010. 
       DSR-999.
            EXIT. 
      *
       RELEASE-GL-RECORD SECTION.
       REL-000.
          IF WS-GLMAST-ST1 = 51
              UNLOCK GL-MASTER.
       REL-999.
           EXIT.
      *
       REWRITE-GL-RECORD SECTION.
       RSR-010.
          IF NEW-GLNO = "Y"
              GO TO RSR-020.
          REWRITE GL-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-GLMAST-ST1 NOT = 0
              MOVE 0 TO WS-GLMAST-ST1
              MOVE "GL RECORD BUSY ON REWRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-010.
          GO TO RSR-999.
       RSR-020.
          WRITE GL-RECORD
              INVALID KEY NEXT SENTENCE.
          IF WS-GLMAST-ST1 NOT = 0
              MOVE 0 TO WS-GLMAST-ST1
              MOVE "GL RECORD BUSY ON WRITE, 'ESC' TO RETRY."
              TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              GO TO RSR-020.
       RSR-999.
          EXIT.
      *
       READ-PARAMETER-RECORD SECTION.
       RPR-000.
           MOVE 1 TO GLPA-KEY.
           START GLPARAMETER-FILE KEY NOT < GLPA-KEY.
       RPR-010.
           READ GLPARAMETER-FILE
                 INVALID KEY NEXT SENTENCE.
           IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
                MOVE "PARAMETER RECORD NOT THERE, 'ESC' TO EXIT"
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RPR-999.
           IF WS-GLPARAMETER-ST1 NOT = 0
                MOVE 0 TO WS-GLPARAMETER-ST1
                MOVE "PARAMETER Record Busy, Press 'ESC' To Retry"
                  TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RPR-010.
       RPR-999.
            EXIT.
      *
       READ-GL SECTION.
       R-GL-000.
             MOVE GL-NUMBER TO WS-NUMBER.
             START GL-MASTER KEY NOT < GL-KEY.
       R-GL-010.
             READ GL-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-GLNO
                MOVE WS-NUMBER TO GL-NUMBER
                GO TO R-GL-900.
             IF WS-GLMAST-ST1 NOT = 0
                MOVE 0 TO WS-GLMAST-ST1
                MOVE "GL RECORD BUSY, BE PATIENT!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-GL-010.
             MOVE "N" TO NEW-GLNO.
       R-GL-900.
             MOVE "N" TO WS-HEAD-VALID
                         WS-SUB-VALID.
             MOVE GL-NUMBER TO WS-GLACC.
             IF WS-GLSUBHEADER = "   "
                 MOVE "Y" TO WS-HEAD-VALID
                             WS-SUB-VALID
                 GO TO R-GL-940.
             MOVE " " TO GL-NUMBER.
             MOVE WS-GLHEADER TO GL-NUMBER.
             PERFORM READ-HEADER.
             IF WS-REST = "      "
                 MOVE "Y" TO WS-SUB-VALID
                 GO TO R-GL-940.
             MOVE " " TO GL-NUMBER.
             MOVE WS-SUBHEADER TO GL-NUMBER.
             PERFORM READ-SUBHEADER.
       R-GL-940.
             MOVE WS-GLACC TO GL-NUMBER WS-NUMBER.
             START GL-MASTER KEY NOT < GL-KEY.
       R-GL-950.
             IF NEW-GLNO = "Y"
                 PERFORM CLEAR-FORM
                 MOVE WS-NUMBER TO GL-NUMBER
                 MOVE WS-TYPE TO GL-P-B
                 GO TO R-GL-999.
             READ GL-MASTER WITH LOCK
                 INVALID KEY NEXT SENTENCE.
             IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                PERFORM CLEAR-FORM
                MOVE "Y" TO NEW-GLNO
                MOVE WS-NUMBER TO GL-NUMBER
                MOVE WS-TYPE TO GL-P-B.
                GO TO R-GL-999.
             IF WS-GLMAST-ST1 NOT = 0
                MOVE 0 TO WS-GLMAST-ST1
                MOVE "GL RECORD BUSY, BE PATIENT!" TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-GL-950.
       R-GL-999.
             EXIT.
      *
       READ-HEADER SECTION.
       R-HEAD-000.
             START GL-MASTER KEY NOT < GL-KEY.
       R-HEAD-010.
             READ GL-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-GLMAST-ST1
                MOVE "ENTER A HEADER ACC BEFORE ENTERING A DETAIL ACC."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-HEAD-999.
             IF WS-GLMAST-ST1 NOT = 0
                MOVE 0 TO WS-GLMAST-ST1
                MOVE "GL HEADER RECORD BUSY ON READ, 'ESC' TO RETRY."
                 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-HEAD-010.
             MOVE GL-P-B TO WS-TYPE.
             MOVE "Y" TO WS-HEAD-VALID.
       R-HEAD-999.
             EXIT.
      *
       READ-SUBHEADER SECTION.
       R-SUB-000.
             START GL-MASTER KEY NOT < GL-KEY.
       R-SUB-010.
             READ GL-MASTER
                 INVALID KEY NEXT SENTENCE.
             IF WS-GLMAST-ST1 = 23 OR 35 OR 49
                MOVE 0 TO WS-GLMAST-ST1
                MOVE
                "ENTER A SUBHEADER ACC BEFORE ENTERING A DETAIL ACC."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-SUB-999.
             IF WS-GLMAST-ST1 NOT = 0
                MOVE 0 TO WS-GLMAST-ST1
                MOVE
                 "GL SUB-HEADER RECORD BUSY ON READ, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO R-SUB-010.
             MOVE "Y" TO WS-SUB-VALID.
       R-SUB-999.
             EXIT.
      *
       START-GL SECTION.
       GL-GL-000.
              MOVE WS-NUMBER TO GL-NUMBER.
              START GL-MASTER KEY NOT LESS GL-NUMBER.
       GL-GL-999.
             EXIT.
      *
       READ-GL-NEXT SECTION.
       RSN-005. 
           READ GL-MASTER NEXT WITH LOCK
             AT END 
               MOVE " " TO GL-NUMBER
                           WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
               MOVE "END OF NEXT PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-999.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GL-MASTER FILE BUSY ON READ-NEXT, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RSN-005.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               PERFORM START-GL
               GO TO RSN-005.
           MOVE GL-NUMBER TO WS-NUMBER.
           MOVE "N" TO NEW-GLNO.
           MOVE "Y" TO WS-HEAD-VALID
                       WS-SUB-VALID.
       RSN-999.
             EXIT.
      *
       READ-GL-PREVIOUS SECTION.
       RDPR-005. 
           READ GL-MASTER PREVIOUS WITH LOCK
             AT END 
               MOVE " " TO GL-NUMBER
                           WS-NUMBER
               MOVE "Y" TO WS-END
               PERFORM CLEAR-FORM
            MOVE "END OF PREVIOUS PAGE SEQUENCE, ENTER A NEW IDENTIFIER"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPR-999.
           IF WS-GLMAST-ST1 = 23 OR 35 OR 49 OR 51
               MOVE 0 TO WS-GLMAST-ST1
           MOVE "GL-MASTER BUSY, ON READ-PREVIOUS-LOCK, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO RDPR-005.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               PERFORM START-GL
               GO TO RDPR-005.
           MOVE GL-NUMBER TO WS-NUMBER.
           MOVE "N" TO NEW-GLNO.
           MOVE "Y" TO WS-HEAD-VALID
                       WS-SUB-VALID.
       RDPR-999.
             EXIT.
      *
       CLEAR-FORM SECTION.
       CLSC-000.
             MOVE " " TO GL-NUMBER
                         GL-DESCRIPTION
                         GL-P-B.
             MOVE 1 TO SUB-1.
       CLSC-005.
             MOVE 0   TO GL-PER (SUB-1)
                         GL-PER-BU (SUB-1)
                         GL-LAST-PER (SUB-1).
            ADD 1 TO SUB-1.
            IF SUB-1 < 13
                 GO TO CLSC-005.
             MOVE 1 TO SUB-1.
       CLSC-010.
           MOVE 0 TO GL-DATE
                     GL-BALANCE
                     GL-OPEN-PER-BAL
                     GL-OPEN-YEAR-BAL
                     GL-LAST-YEAR-BAL.
       CLSC-500.
           PERFORM RELEASE-GL-RECORD.
       CLSC-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
           OPEN I-O GL-MASTER.
           IF WS-GLMAST-ST1 NOT = 0
               MOVE 0 TO WS-GLMAST-ST1
               MOVE "GLMASTER FILE BUSY ON OPEN, 'ESC' TO RE-TRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-000.
        OPEN-001.
            OPEN I-O GLPARAMETER-FILE.
            IF WS-GLPARAMETER-ST1 NOT = 0
               MOVE 0 TO WS-GLPARAMETER-ST1
               MOVE "GLPARAMETER FILE BUSY ON OPEN, 'ESC' TO RETRY"
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-001.
            PERFORM READ-PARAMETER-RECORD.
            IF WS-GLPARAMETER-ST1 = 23 OR 35 OR 49
               CLOSE GLPARAMETER-FILE
               PERFORM END-OFF.
            CLOSE GLPARAMETER-FILE.
       OPEN-005.
           PERFORM GET-SYSTEM-Y2K-DATE.
      *     ACCEPT WS-DATE FROM DATE.
       OPEN-010.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "GlMastIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
           EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE GL-MASTER.
           EXIT PROGRAM.
      *      STOP RUN.
       END-999.
          EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "ReadFieldNumeric".
       Copy "WriteFieldAlpha".
       Copy "WriteFieldDate".
       Copy "WriteFieldNumeric".
       Copy "WriteFieldRec".
       Copy "EnterPeriodDates".
       Copy "ComputeDatePeriod".
       Copy "NumberCheck".
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
      * END-OF-JOB
