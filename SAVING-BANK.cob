      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "ACTIN.TXT".
           SELECT OUTFILE ASSIGN TO "OUTACT.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 33 CHARACTERS
           DATA RECORD IS REC-IN.
       01  REC-IN.
           05 ACCNO-IN PIC X(3).
           05 ACCNAME-IN PIC X(22).
           05 TC-IN PIC X.
           05 AMOUNT-IN PIC 9(5)V99.
       FD  OUTFILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD OUTREC.
       01  OUTREC.
       05  FILLER PIC X(80).

       WORKING-STORAGE SECTION.
       01  HD1-REC.
           05 FILLER PIC X(32) VALUE SPACES.
           05 FILLER PIC X(17) VALUE "MJRC SAVING BANK".
           05 FILLER PIC X(31) VALUE SPACES.
       01  HD2-REC.
           05 FILLER PIC X(28) VALUE SPACES.
           05 FILLER PIC X(24) VALUE "Maypajo, Caloocan Branch".
           05 FILLER PIC X(28) VALUE SPACES.
       01  HD3-REC.
           05 FILLER PIC X(31) VALUE SPACES.
           05 FILLER PIC X(18) VALUE "DEPOSITOR'S REPORT".
           05 FILLER PIC X(31) VALUE SPACES.
       01  COLHD1.
           05 FILLER PIC X(15) VALUE SPACES.
           05 FILLER PIC X(7) VALUE "ACCOUNT".
           05 FILLER PIC X(15) VALUE SPACES.
           05 FILLER PIC X(7) VALUE "ACCOUNT".
           05 FILLER PIC X(15) VALUE SPACES.
           05 FILLER PIC X(7) VALUE "BALANCE".
           05 FILLER PIC X(14) VALUE SPACES.
       01  COLHD2.
           05 FILLER PIC X(16) VALUE SPACES.
           05 FILLER PIC X(6) VALUE "NUMBER".
           05 FILLER PIC X(16) VALUE SPACES.
           05 FILLER PIC X(4) VALUE "NAME".
           05 FILLER PIC X(17) VALUE SPACES.
           05 FILLER PIC X(6) VALUE "AMOUNT".
           05 FILLER PIC X(15) VALUE SPACES.
       01  REC-OUT.
           05 FILLER PIC X(17) VALUE SPACES.
           05 ACCNO-OUT PIC X(3).
           05 FILLER PIC X(10) VALUE SPACES.
           05 ACCNAME-OUT PIC X(22).
           05 FILLER PIC X(3) VALUE SPACES.
           05 BAL-OUT PIC Z,ZZZ,ZZ9.99.
           05 FILLER PIC X(13) VALUE SPACES.
       01  TOTDREC.
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(21) VALUE "TOTAL DEPOSITORS:   ".
           05 FILLER PIC X(19) VALUE SPACES.
           05 DCTR-OUT PIC Z9.
           05 FILLER PIC X(20) VALUE SPACES.
       01  TOTABREC.
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(30) VALUE "TOTAL ACCUMULATED BALANCES:   ".
           05 ABCTR-OUT PIC Z,ZZZ,ZZ9.99.
           05 FILLER PIC X(10) VALUE SPACES.
           05 FILLER PIC X(27) VALUE SPACES.
       01  TEMP-VARIABLES.
           05 DCTR PIC 99 VALUE 0.
           05 ABCTR PIC 9(7)V99 VALUE 0.
           05 BALANCE PIC 9(7)V99 VALUE 0.
           05 EOFSW PIC X(3) VALUE 'NO '.
           05 TACCNO PIC X(3).
           05 TACCNAME PIC X(24) VALUE ' '.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN THRU INIT-END.
           PERFORM PROCESS-RTN THRU PROCESS-END UNTIL EOFSW = 'YES'.
           PERFORM FINISH-RTN THRU FINISH-END.
           STOP RUN.

       INIT-RTN.
           OPEN INPUT INFILE OUTPUT OUTFILE.
           READ INFILE
               AT END MOVE 'YES' TO EOFSW
               NOT END MOVE ACCNO-IN TO TACCNO,
                       MOVE ACCNAME-IN TO TACCNAME.
           WRITE OUTREC FROM HD1-REC.
           WRITE OUTREC FROM HD2-REC.
           WRITE OUTREC FROM HD3-REC AFTER ADVANCING 2 LINES.
           WRITE OUTREC FROM COLHD1 AFTER ADVANCING 2 LINES.
           WRITE OUTREC FROM COLHD2.
       INIT-END.

       PROCESS-RTN.
           IF ACCNO-IN NOT EQUAL TO TACCNO
               PERFORM ACCNT-BREAK-RTN THRU ACCNT-BREAK-END.
           IF TC-IN NOT EQUAL TO 'D'
               SUBTRACT AMOUNT-IN FROM BALANCE
           ELSE
               ADD AMOUNT-IN TO BALANCE
           END-IF.
           READ INFILE
               AT END MOVE 'YES' TO EOFSW
               PERFORM ACCNT-BREAK-RTN THRU ACCNT-BREAK-END.
       PROCESS-END.

       ACCNT-BREAK-RTN.
           MOVE TACCNO TO ACCNO-OUT.
           MOVE TACCNAME TO ACCNAME-OUT.
           MOVE BALANCE TO BAL-OUT.
           WRITE OUTREC FROM REC-OUT.
           ADD 1 TO DCTR.
           ADD BALANCE TO ABCTR.
           MOVE 0 TO BALANCE.
           MOVE ACCNO-IN TO TACCNO.
           MOVE ACCNAME-IN TO TACCNAME.
       ACCNT-BREAK-END.

       FINISH-RTN.
           MOVE DCTR TO DCTR-OUT.
           MOVE ABCTR TO ABCTR-OUT.
           WRITE OUTREC FROM TOTDREC.
           WRITE OUTREC FROM TOTABREC.
           CLOSE INFILE, OUTFILE.
       FINISH-END.
       END PROGRAM YOUR-PROGRAM-NAME.
