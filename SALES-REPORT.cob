       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVITY.
       AUTHOR. BSIT 2-4.
       INSTALLATION. OWN COMPUTER.
       DATE-WRITTEN. FEBRUARY 6, 2023.
       DATE-COMPILED.
       SECURITY. ACCESSIBLE TO BSIT 2-4 ONLY.
       REMARKS. ACTIVITY_ACC PROGRAM SA COBOL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LAPTOP.
       OBJECT-COMPUTER. LAPTOP.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "INFILE.TXT".
           SELECT OUTFILE ASSIGN TO "OUTFILE.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 36 CHARACTERS
           DATA RECORD IS REC-IN.
       01  REC-IN.
           05 AC-IN PIC A.
           05 SNO-IN PIC X(3).
           05 SNAME-IN PIC X(25).
           05 SALES-IN PIC 9(5)V99.
       FD  OUTFILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS OUTREC.
       01  OUTREC.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01  REC-OUT.
           05 FILLER PIC X(13).
           05 AC-OUT PIC A.
           05 FILLER PIC X(14).
           05 SNO-OUT PIC X(3).
           05 FILLER PIC X(7).
           05 SNAME-OUT PIC X(25).
           05 FILLER PIC X(5).
           05 SALES-OUT PIC ZZ,ZZ9.99.
           05 FILLER PIC X(4).
       01  TOTSREC.
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(29) VALUE 'TOTAL NUMBER OF SALESMEN:   '.
           05 SCTR-OUT PIC Z9.
           05 FILLER PIC X(38) VALUE SPACES.
       01  TOTASREC.
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(29) VALUE 'TOTAL ACCUMULATED SALES:    '.
           05 ASCTR-OUT PIC Z,ZZZ,ZZ9.99.
           05 FILLER PIC X(31) VALUE SPACES.
       01  TEMP-VARIABLES.
           05 SCTR PIC 99 VALUE 0.
           05 ASCTR PIC 9(7)V99 VALUE 0.
           05 TAC PIC A.
           05 EOFSW PIC X(3) VALUE 'NO '.
       01  HD1.
           05 FILLER PIC X(32) VALUE SPACES.
           05 FILLER PIC X(16) VALUE "CHIKA LANG CORP.".
           05 FILLER PIC X(32) VALUE SPACES.
       01  HD2.
           05 FILLER PIC X(32) VALUE SPACES.
           05 FILLER PIC X(17) VALUE "STA. MESA, MANILA".
           05 FILLER PIC X(31) VALUE SPACES.
       01  HD3.
           05 FILLER PIC X(34) VALUE SPACES.
           05 FILLER PIC X(12) VALUE "SALES REPORT".
           05 FILLER PIC X(34) VALUE SPACES.
       01  COLHD1.
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(4) VALUE "AREA".
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(8) VALUE "SALESMAN".
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(8) VALUE "SALESMAN".
           05 FILLER PIC X(18) VALUE SPACES.
           05 FILLER PIC X(5) VALUE "SALES".
           05 FILLER PIC X(4) VALUE SPACES.
       01  COLHD2.
           05 FILLER PIC X(11) VALUE SPACES.
           05 FILLER PIC X(4) VALUE "CODE".
           05 FILLER PIC X(12) VALUE SPACES.
           05 FILLER PIC X(6) VALUE "NUMBER".
           05 FILLER PIC X(14) VALUE SPACES.
           05 FILLER PIC X(4) VALUE "NAME".
           05 FILLER PIC X(19) VALUE SPACES.
           05 FILLER PIC X(6) VALUE "AMOUNT".
           05 FILLER PIC X(3) VALUE SPACES.
       01  BLNK-HD.
           05 FILLER PIC X(80).

       PROCEDURE DIVISION.
       MAIN-RTN.
           perform initial-rtn thru initial-end.
           perform process-rtn thru process-end until eofsw = 'yes'.
           perform finish-rtn thru finish-end.
           stop run.

       initial-rtn.
           open input INFILE
                output OUTFILE.
           read infile
                at end move 'yes' to eofsw
                not at end move ac-in to tac.
           write OUTREC from HD1.
           write OUTREC from HD2.
           write OUTREC from BLNK-HD.
           write OUTREC from HD3.
           write OUTREC from BLNK-HD.
           write OUTREC from COLHD1.
           write OUTREC from COLHD2.
       initial-end.

       process-rtn.
           if ac-in not equal to tac
               perform ac-break-rtn through ac-break-end.
           MOVE AC-IN TO AC-OUT.
           MOVE SNO-IN TO SNO-OUT.
           MOVE SNAME-IN TO SNAME-OUT.
           MOVE SALES-IN TO SALES-OUT.
           WRITE OUTREC FROM REC-OUT.
           ADD 1 TO SCTR.
           ADD SALES-IN TO ASCTR.
           read infile at end move 'yes' to eofsw
               perform ac-break-rtn thru ac-break-end.
       process-end.
       ac-break-rtn.
           MOVE SCTR TO SCTR-OUT.
           MOVE ASCTR TO ASCTR-OUT.
           WRITE OUTREC FROM TOTSREC.
           WRITE OUTREC FROM TOTASREC.
           MOVE 0 TO SCTR.
           MOVE 0 TO ASCTR.
           MOVE AC-IN TO TAC.
       ac-break-end.
       finish-rtn.
           close INFILE, OUTFILE.
       finish-end.
