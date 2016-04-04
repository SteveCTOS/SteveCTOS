        IDENTIFICATION DIVISION.
        PROGRAM-ID. StDescIq.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          Copy "SelectStMaster".
          Copy "SelectStSpecPr".
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdStock.
           COPY ChlfdStPrice.

       WORKING-STORAGE SECTION.
       77  WS-SHORTDESC         PIC X(10) VALUE " ".
       77  WS-WORK              PIC X(20) VALUE " ".
       77  WS-1ST               PIC X VALUE " ".
       77  WS-ONHAND-ONLY       PIC X VALUE " ".
       77  WS-SUB1-DIS          PIC 9(4) VALUE 0.
       77  WS-QTY               PIC Z(4)9.
       77  WS-PRICE             PIC Z(5)9.99 BLANK WHEN ZERO.
       77  WS-SP-PRICE          PIC Z(5)9.99 BLANK WHEN ZERO.
       77  WS-MIDDLE            PIC X(79) VALUE " ".
       01  WS-STOCK-STATUS.
           03  WS-STOCK-ST1     PIC 99.
       01  WS-STPR-STATUS.
           03  WS-STPR-ST1      PIC 99.
       01  WS-SPLIT-DESC.
           03  WS-SP-1          PIC X(5) VALUE " ".
           03  WS-SP-REST       PIC X(15) VALUE " ".
       01  WS-SPLIT-INPUT-DESC.
           03  WS-SP-I-1        PIC X(5) VALUE " ".
           03  WS-SP-I-REST     PIC X(15) VALUE " ".
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
      * <f5> & <Code-F5>
           IF F-EXIT-CH = X"19" OR = X"99"
               PERFORM READ-SPECIAL-DISPLAY
           ELSE
               PERFORM READ-MASTER-DISPLAY.
           GO TO CONTROL-010.
      *
       GET-DATA SECTION.
       GET-000.
           MOVE " " TO WS-SPLIT-INPUT-DESC
                       WS-SPLIT-DESC.
           MOVE "Y" TO WS-1ST.
           
           MOVE 1505 TO POS
           DISPLAY 
          "Hold The <ALT> Key While Pressing <F5> OR <F8> OR <Return>"
             AT POS
           MOVE 1605 TO POS
           DISPLAY
          "To Get The System To Display ONLY Items With On-Hand > Zero."
             AT POS.

           MOVE "SHORTDESC" TO F-FIELDNAME.
           MOVE 9           TO F-CBFIELDNAME.
           PERFORM USER-FILL-FIELD.
           IF F-EXIT-CH = X"04"
               PERFORM END-OFF.
           PERFORM ERROR-020.
           MOVE 10          TO F-CBFIELDLENGTH.
           PERFORM READ-FIELD-ALPHA.
           MOVE F-NAMEFIELD TO WS-SHORTDESC.
      *     MOVE 0           TO F-EXIT-CH.
           MOVE SPACES TO WS-MESSAGE
           MOVE 1505 TO POS
           DISPLAY WS-MESSAGE AT POS
           MOVE 1605 TO POS
           DISPLAY WS-MESSAGE AT POS.
      * <CODE-F5> & <CODE-RETURN> & <CODE-F8>
      *     IF F-EXIT-CH = X"99" OR = X"8A" OR = X"9D"
      * <CODE-F5> & <CODE-RETURN> & <CODE-F8>
      * <CODE-RETURN> CHANGED TO <GO> = X"1B"
           IF F-EXIT-CH = X"99" OR = X"1B" OR = X"9D"
              MOVE "Y" TO WS-ONHAND-ONLY
           ELSE
              MOVE "N" TO WS-ONHAND-ONLY.
       GET-999.
            EXIT.
      *
       READ-MASTER-DISPLAY SECTION.
       READ-005.
            IF F-EXIT-CH = X"0A" OR = X"1B"
               MOVE WS-SHORTDESC TO ST-DESCRIPTION1
                                    WS-SPLIT-INPUT-DESC
            START STOCK-MASTER KEY NOT < ST-ALT-KEY
               INVALID KEY NEXT SENTENCE.
            IF F-EXIT-CH = X"1D" OR = X"9D"
               MOVE WS-SHORTDESC TO ST-STOCKNUMBER
                                    WS-SPLIT-INPUT-DESC
            START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
            MOVE 0 TO F-EXIT-CH.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON START, 'ESC' TO EXIT."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO READ-999.
            MOVE 0 TO SUB-2 SUB-3.
            MOVE 800 TO WS-SUB1-DIS.
       READ-010.
            IF F-EXIT-CH = 0
              READ STOCK-MASTER NEXT
                AT END
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                MOVE "END OF NEXT-FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-999.
            IF F-EXIT-CH = 1
              READ STOCK-MASTER PREVIOUS
                AT END
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                MOVE "END OF PREV-FILE, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO READ-999.
            IF WS-STOCK-ST1 = 91
                CLOSE STOCK-MASTER
                MOVE "STOCK BUSY SYSTEM ERROR=91, CALL YOUR SUPERVISOR."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM OPEN-000
                GO TO READ-999.
            IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO READ-010.
            IF WS-ONHAND-ONLY = "Y"
             IF ST-QTYONHAND NOT > 0
                MOVE 2701 TO POS
                DISPLAY "NEXT" AT POS
                GO TO READ-010
             ELSE
                MOVE 2701 TO POS
                DISPLAY "    " AT POS.

            IF WS-1ST = "Y"
              IF ST-DESCRIPTION1 NOT = " "
                MOVE ST-DESCRIPTION1 TO WS-SPLIT-INPUT-DESC
                MOVE "N" TO WS-1ST.
            MOVE ST-DESCRIPTION1 TO WS-SPLIT-DESC.
      *      IF WS-SP-1 NOT = WS-SP-I-1
      *          CLOSE STOCK-MASTER
      *          MOVE 2710 TO POS
      *          DISPLAY "No More Items With That Description." AT POS
      *          MOVE 2816 TO POS
      *          DISPLAY "Press 'ESC' To Clear The Screen !" AT POS
      *          CALL "&LOCKKBD" USING F-FIELDNAME
      *          MOVE 2710 TO POS
      *          DISPLAY "                                    " AT POS
      *          MOVE 2816 TO POS
      *          DISPLAY "                                    " AT POS
      *          GO TO READ-999.
       READ-020.
            PERFORM READ-SPECIAL-PRICES.
            ADD 1 TO SUB-3.
       READ-025.
            IF SUB-3 > 16
                MOVE 2701 TO POS
                DISPLAY "Press 'PgDn' For More, 'PgUp' for Previous,"
                 AT POS
                MOVE 2806 TO POS
                DISPLAY "Or 'ESC' To Clear The Screen !" AT POS

                MOVE 2655 TO POS
                DISPLAY "Analysis Codes:" AT POS
                MOVE 2748 TO POS
                DISPLAY "D=DELETE, N=NON RE-ORDERABLE, " AT POS
                MOVE 2848 TO POS
                DISPLAY "T=TOOLKIT ITEM, S=SPECIAL ORDER" AT POS
                MOVE "SHORTDESC" TO F-FIELDNAME
                MOVE 9           TO F-CBFIELDNAME
                MOVE 10          TO F-CBFIELDLENGTH
                PERFORM USER-FILL-FIELD.
            MOVE 2705 TO POS
            DISPLAY "                                        " AT POS
            MOVE 2806 TO POS
            DISPLAY "                                        " AT POS.
            
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTDESC
                MOVE 0   TO WS-STOCK-STATUS
                PERFORM CLEAR-MIDDLE
                CLOSE STOCK-MASTER
                PERFORM OPEN-000
                GO TO READ-999.
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                GO TO READ-025.
            IF ST-STOCKNUMBER = " "
                GO TO READ-010.

            ADD 100 TO WS-SUB1-DIS.
      *       DISPLAY "ÿAF" AT WS-SUB1-DIS.
            ADD 1 TO WS-SUB1-DIS.
            MOVE ST-STOCKNUMBER TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS
              WITH reverse-video FOREGROUND-COLOR IS 2.
      *      ADD 13 TO WS-SUB1-DIS.
      *       DISPLAY "ÿAA" AT WS-SUB1-DIS.
            MOVE " " TO WS-WORK.
            ADD 15 TO WS-SUB1-DIS.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
      *      ADD 1 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION1 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION2 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-PRICE TO WS-PRICE.
            DISPLAY WS-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE STPR-PRICE TO WS-SP-PRICE.
            DISPLAY WS-SP-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE ST-ANALYSIS TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
             ADD 1 TO WS-SUB1-DIS.
            MOVE ST-QTYONHAND TO WS-QTY.
            DISPLAY WS-QTY AT WS-SUB1-DIS.
            
            SUBTRACT 75 FROM WS-SUB1-DIS.
            GO TO READ-010.
        READ-999.
            EXIT.
      *
       READ-SPECIAL-DISPLAY SECTION.
       RDSP-005.
            IF F-EXIT-CH = X"19" OR = X"99"
               MOVE WS-SHORTDESC TO STPR-STOCKNUMBER
                                      WS-SPLIT-INPUT-DESC
            START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
            MOVE 0 TO F-EXIT-CH.
            IF WS-STPR-ST1 NOT = 0
                MOVE 
           "THERE ARE NO SPECIALS ON START TO READ, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STPR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO RDSP-999.
            MOVE 0   TO SUB-2 SUB-3.
            MOVE 800 TO WS-SUB1-DIS.
       RDSP-010.
            IF F-EXIT-CH = 0
              READ STPR-MASTER NEXT
                 AT END
                CLOSE STPR-MASTER
                PERFORM OPEN-005
                MOVE 
                "END OF SPECIALS NEXT-READ, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDSP-999.
            IF F-EXIT-CH = 1
              READ STPR-MASTER PREVIOUS
                 AT END
                CLOSE STPR-MASTER
                PERFORM OPEN-005
                MOVE 
                "END OF SPECIALS PREV-READ, 'ESC' TO THE CLEAR SCREEN."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                GO TO RDSP-999.
            IF WS-STPR-ST1 = 91
                MOVE 0 TO WS-STPR-STATUS
                CLOSE STPR-MASTER
                MOVE "SPECIALS SYSTEM ERROR=9, CALL YOUR SUPERVISOR."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM OPEN-005
                GO TO RDSP-010.
            IF WS-STPR-ST1 NOT = 0
                MOVE "STOCK SPECIALS BUSY ON READ-NEXT, 'ESC' TO EXIT."
                TO WS-MESSAGE
                PERFORM ERROR1-000
                MOVE WS-STPR-ST1 TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                PERFORM ERROR1-020
                GO TO RDSP-010.
                
            PERFORM READ-STOCK.
            IF WS-ONHAND-ONLY = "Y"
             IF ST-QTYONHAND NOT > 0
                MOVE 2701 TO POS
                DISPLAY "NEXT" AT POS
                GO TO RDSP-010
             ELSE
                MOVE 2701 TO POS
                DISPLAY "    " AT POS.
                
            IF WS-1ST = "Y"
              IF ST-DESCRIPTION1 NOT = " "
                MOVE ST-DESCRIPTION1 TO WS-SPLIT-INPUT-DESC
                MOVE "N" TO WS-1ST.
            MOVE ST-DESCRIPTION1 TO WS-SPLIT-DESC.
      *      IF WS-SP-1 NOT = WS-SP-I-1
      *          CLOSE STOCK-MASTER
      *          MOVE 2710 TO POS
      *          DISPLAY "No More Items With That Description." AT POS
      *          MOVE 2816 TO POS
      *          DISPLAY "Press 'ESC' To Clear The Screen !" AT POS
      *          CALL "&LOCKKBD" USING F-FIELDNAME
      *          MOVE 2710 TO POS
      *          DISPLAY "                                    " AT POS
      *          MOVE 2816 TO POS
      *          DISPLAY "                                    " AT POS
      *          GO TO RDSP-999.
       RDSP-020.
            ADD 1 TO SUB-3.
       RDSP-025.
            IF SUB-3 > 16
                MOVE 2701 TO POS
                DISPLAY "Press 'PgDn' For More, 'PdUp' for Previous,"
                 AT POS
                MOVE 2806 TO POS
                DISPLAY "Or 'ESC' To Clear The Screen !" AT POS
                MOVE 2655 TO POS
                DISPLAY "Analysis Codes:" AT POS
                MOVE 2748 TO POS
                DISPLAY "D=DELETE, N=NON RE-ORDERABLE, " AT POS
                MOVE 2848 TO POS
                DISPLAY "T=TOOLKIT ITEM, S=SPECIAL ORDER" AT POS
                PERFORM USER-FILL-FIELD.
            MOVE 2705 TO POS
            DISPLAY "                              " AT POS
            MOVE 2806 TO POS
            DISPLAY "                              " AT POS.
            IF F-EXIT-CH = X"04"
                PERFORM END-OFF.
            IF F-EXIT-CH = X"0C"
                PERFORM CLEAR-MIDDLE
                MOVE 0 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"05"
                PERFORM CLEAR-MIDDLE
                MOVE 1 TO F-EXIT-CH
                MOVE 0 TO SUB-2
                MOVE 1 TO SUB-3
                MOVE 800 TO WS-SUB1-DIS.
            IF F-EXIT-CH = X"07"
                MOVE " " TO WS-SHORTDESC
                MOVE " " TO WS-STOCK-STATUS
                PERFORM CLEAR-MIDDLE
      *          CLOSE STPR-MASTER
      *          PERFORM OPEN-005
                GO TO RDSP-999.
            IF F-EXIT-CH NOT = X"04" AND NOT = X"0C" AND NOT = X"05"
                     AND NOT = X"07" AND NOT = 0     AND NOT = 1
                GO TO RDSP-025.
            IF ST-STOCKNUMBER = " "
                GO TO RDSP-010.

            ADD 100 TO WS-SUB1-DIS.
      *      DISPLAY "ÿAF" AT WS-SUB1-DIS.
            ADD 1 TO WS-SUB1-DIS.
            MOVE ST-STOCKNUMBER TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS
              WITH reverse-video FOREGROUND-COLOR IS 2.

      *      ADD 13 TO WS-SUB1-DIS.
      *      DISPLAY "ÿAA" AT WS-SUB1-DIS.
            MOVE " " TO WS-WORK.
            ADD 15 TO WS-SUB1-DIS.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
      *      ADD 1 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION1 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-DESCRIPTION2 TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
            ADD 20 TO WS-SUB1-DIS.
            MOVE ST-PRICE TO WS-PRICE.
            DISPLAY WS-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE STPR-PRICE TO WS-SP-PRICE.
            DISPLAY WS-SP-PRICE AT WS-SUB1-DIS.
            
            ADD 9 TO WS-SUB1-DIS.
            MOVE ST-ANALYSIS TO WS-WORK.
            DISPLAY WS-WORK AT WS-SUB1-DIS.
            
             ADD 1 TO WS-SUB1-DIS.
            MOVE ST-QTYONHAND TO WS-QTY.
            DISPLAY WS-QTY AT WS-SUB1-DIS.
            
            SUBTRACT 75 FROM WS-SUB1-DIS.
            GO TO RDSP-010.
        RDSP-999.
            EXIT.
      *
       READ-SPECIAL-PRICES SECTION.
       SPR-000.
           MOVE ST-STOCKNUMBER TO STPR-STOCKNUMBER.
           START STPR-MASTER KEY NOT < STPR-KEY
               INVALID KEY NEXT SENTENCE.
      *     IF WS-STPR-ST1 NOT = "0"
      *        Move "BAD START ON SPECIAL'S OPEN, 'ESC' to RETRY"
      *        TO WS-MESSAGE
      *        PERFORM ERROR-MESSAGE
      *        GO TO SPR-000.
       SPR-005.
           READ STPR-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STPR-ST1 = 23 OR 35 OR 49
               MOVE 0 TO STPR-PRICE
               GO TO SPR-900.
           IF WS-STPR-ST1 NOT = 0
              Move "SPECIAL PRICES BUSY ON READ, 'ESC' to RETRY"
              TO WS-MESSAGE
              PERFORM ERROR1-000
              MOVE WS-STPR-ST1 TO WS-MESSAGE
              PERFORM ERROR-MESSAGE
              PERFORM ERROR1-020
              MOVE 0 TO WS-STPR-ST1
              GO TO SPR-005.
       SPR-900.
      *     MOVE STPR-STOCKNUMBER TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE
      *     MOVE STPR-PRICE TO WS-MESSAGE
      *     PERFORM ERROR-MESSAGE.
       SPR-999.
           EXIT.
      *
       READ-STOCK SECTION.
       R-STOCK-000.
           MOVE STPR-STOCKNUMBER TO ST-STOCKNUMBER.
           START STOCK-MASTER KEY NOT < ST-KEY
               INVALID KEY NEXT SENTENCE.
       R-STOCK-005.
           READ STOCK-MASTER
               INVALID KEY NEXT SENTENCE.
           IF WS-STOCK-ST1 = 23 OR 35 OR 49
               MOVE 0 TO ST-PRICE
               GO TO R-STOCK-999.
           IF WS-STOCK-ST1 NOT = 0
               MOVE "STOCK FILE BUSY ON READ, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR1-000
               MOVE WS-STOCK-ST1 TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               PERFORM ERROR1-020
               MOVE 0 TO WS-STOCK-ST1
               GO TO R-STOCK-005.
       R-STOCK-999.
           EXIT.
      *
       CLEAR-MIDDLE SECTION.
       CM-010.
           MOVE " " TO WS-MIDDLE
           MOVE 801 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 901 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1001 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1101 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1201 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1301 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1401 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1501 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1601 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1701 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1801 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 1901 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2001 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2101 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2201 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2301 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2401 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2501 TO POS
           DISPLAY WS-MIDDLE AT POS
           MOVE 2601 TO POS
           DISPLAY WS-MIDDLE AT POS.
       CM-999.
           EXIT.
      *
       OPEN-FILES SECTION.
       OPEN-000.
            OPEN I-O STOCK-MASTER.
            IF WS-STOCK-ST1 NOT = 0
                MOVE "STOCK-MASTER BUSY ON OPEN, 'ESC' TO RETRY."
                TO WS-MESSAGE
                PERFORM ERROR-MESSAGE
                MOVE 0 TO WS-STOCK-STATUS
                GO TO OPEN-000.
       OPEN-005.
      *       GO TO OPEN-008.
      *
            OPEN I-O STPR-MASTER.
            IF WS-STPR-ST1 NOT = 0
               MOVE 0 TO WS-STPR-ST1
               MOVE "STOCK PRICE FILE BUSY ON OPEN, 'ESC' TO RETRY."
               TO WS-MESSAGE
               PERFORM ERROR-MESSAGE
               GO TO OPEN-005.
       OPEN-008.
           MOVE Ws-Forms-Name   TO F-FILENAME
           MOVE Ws-cbForms-name TO F-CBFILENAME.
           MOVE "StDescIq"      TO F-FORMNAME
           MOVE 8               TO F-CBFORMNAME.
       Copy "OpenForms".
       OPEN-999.
            EXIT.
      *
       END-OFF SECTION.
       END-000.
           CLOSE STOCK-MASTER
                 STPR-MASTER.
           PERFORM CLEAR-SCREEN.
      *     STOP RUN.
           EXIT PROGRAM.
       END-999.
            EXIT.
      *
       Copy "ReadFieldAlpha".
       Copy "WriteFieldAlpha".
       Copy "DisplayForm".
       Copy "UserFillField".
      *      
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
