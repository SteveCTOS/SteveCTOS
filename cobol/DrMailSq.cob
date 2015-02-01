        IDENTIFICATION DIVISION.
        PROGRAM-ID. MailList.
        AUTHOR.     CHRISTENSEN.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. B20.
        OBJECT-COMPUTER. B20.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT MAILSEQ-MASTER ASSIGN TO "/main/data01/DrMailSequ"
               FILE STATUS IS WS-DEBTOR-STATUS.
           SELECT MAIL-MASTER ASSIGN TO "/main/data01/MailMaster"
               ORGANIZATION IS INDEXED
               LOCK MANUAL
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ML-KEY
               FILE STATUS IS WS-DEBTOR-STATUS.
      *
        DATA DIVISION.
        FILE SECTION.
           COPY ChlfdMailSequential.
           COPY ChlfdMailList.
      *
       WORKING-STORAGE SECTION.
           77  WS-EOF        PIC X(3) VALUE "   ".
           01  WS-DEBTOR-STATUS.
               03  WS-STAT1  PIC 99.
      *         03  WS-STAT2  PIC X.     
      *
       PROCEDURE DIVISION.
       CONTROL-PARAGRAPH SECTION.
           PERFORM A-INIT.
           PERFORM B-MAIN.
           PERFORM C-END.
           STOP RUN.
       CONTROL-999.
           EXIT. 
      *
       A-INIT SECTION.
       A-000.
           OPEN INPUT MAILSEQ-MASTER.
           IF WS-STAT1 NOT = 0
               DISPLAY "ERROR IN OPENING MailSequential FILE"
               STOP RUN.
           OPEN I-O MAIL-MASTER.
           IF WS-STAT1 NOT = 0
               DISPLAY "ERROR IN OPENING MailList FILE"
               STOP RUN.
       A-EXIT.
           EXIT.
      *
       B-MAIN SECTION.
       B-005.
           READ MAILSEQ-MASTER
               AT END 
                 MOVE "EOF" TO WS-EOF
               GO TO B-EXIT.
              
           DISPLAY MS-CO-NAME.
           DISPLAY ML-CO-NAME.

           ADD 1                     TO ML-NUMBER.
           IF MS-CO-NAME NOT = " "
              MOVE MS-CO-NAME        TO ML-CO-NAME
              MOVE MS-ADDRESS1       TO ML-ADDRESS1
              MOVE MS-ADDRESS2       TO ML-ADDRESS2
              MOVE MS-POST-CODE      TO ML-POST-CODE
              MOVE MS-TELE-CODE      TO ML-TELE-CODE
              MOVE MS-TELEPHONE      TO ML-TELEPHONE.
           MOVE MS-INITIALS          TO ML-INITIALS
           MOVE MS-NAME              TO ML-NAME.
  
           WRITE MAIL-RECORD
                 INVALID KEY
                 DISPLAY "INVALID WRITE"
                 STOP RUN.
           GO TO B-005.
       B-EXIT.
           EXIT.
      *
       C-END SECTION.
       C-000.
           CLOSE MAILSEQ-MASTER
                MAIL-MASTER.
       C-EXIT.
          EXIT.
      *END-OF-JOB.
