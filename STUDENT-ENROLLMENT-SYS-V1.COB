       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINAL-PROJECT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT1 ASSIGN TO 'SUBJECT.TXT'.
           SELECT INPUT2 ASSIGN TO 'FACULTY.TXT'.
           SELECT INPUT3 ASSIGN TO 'STUDENT.TXT'.
           SELECT INPUT4 ASSIGN TO 'GRADE.TXT'.
           SELECT OUTPUT1 ASSIGN TO 'ENROLL.TXT'.
           SELECT OUTPUT2 ASSIGN TO 'GRADES.TXT'.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT1
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 40 CHARACTERS
           DATA RECORD IS SUBJ-INFO.
       01 SUBJ-INFO.
           05 SUBJ-CODE PIC X(10).
           05 SUBJ-DESC PIC X(30).
       FD INPUT2
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 47 CHARACTERS
           DATA RECORD IS FACULTY-INFO.
       01 FACULTY-INFO.
           05 SUBJ-CODE2 PIC X(10).
           05 FACULTY-ID PIC X(12).
           05 FACULTY-NAME PIC X(25).
       FD INPUT3
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 59 CHARACTERS
           DATA RECORD IS STUD-INFO.
       01 STUD-INFO.
           05 SUBJ-CODE3 PIC X(10).
           05 FACULTY-ID2 PIC X(12).
           05 STUD-NUM PIC X(12).
           05 STUD-NAME PIC X(25).
       FD INPUT4
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 40 CHARACTERS
           DATA RECORD IS GRADE-INFO.
       01 GRADE-INFO.
           05 SUBJ-CODE4 PIC X(10).
           05 FACULTY-ID3 PIC X(12).
           05 STUD-NUM2 PIC X(12).
           05 MID-GRADE PIC 9V99.
           05 FINAL-GRADE PIC 9V99.
       FD OUTPUT1.
       01 ENROLL.
           05 FILLER PIC X(80).
       FD OUTPUT2.
       01 GRADES.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01 HDNG1.
           05 FILLER PIC X(32).
           05 FILLER PIC X(16) VALUE 'TRI-STAR ACADEMY'.
           05 FILLER PIC X(32).
       01 HDNG2.
           05 FILLER PIC X(28).
           05 FILLER PIC X(25) VALUE 'GREENHILLS, SAN JUAN CITY'.
           05 FILLER PIC X(27).
       01 HDNG3.
           05 FILLER PIC X(28).
           05 FILLER PIC X(25) VALUE 'STUDENT ENROLLMENT REPORT'.
           05 FILLER PIC X(27).
       01 BLANK-HEADING.
           05 FILLER PIC X(80).
       01 HDR1.
           05 FILLER PIC X(26) VALUE 'SUBJECT CODE          :   '.
           05 SUB-CD PIC X(10).
           05 FILLER PIC X(44).
       01 HDR2.
           05 FILLER PIC X(26) VALUE 'SUBJECT DESCRIPTION   :   '.
           05 SUB-DESC PIC X(30).
           05 FILLER PIC X(24).
       01 HDR3.
           05 FILLER PIC X(14) VALUE 'STUDENT NUMBER'.
           05 FILLER PIC X(14).
           05 FILLER PIC X(12) VALUE 'STUDENT NAME'.
           05 FILLER PIC X(40).
       01 E-INFO.
           05 FILLER PIC X(6).
           05 STUD-NO PIC X(12).
           05 FILLER PIC X(8).
           05 STUD-NA PIC X(25).
           05 FILLER PIC X(29).
       01 PROG1-NUM-E.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(11) VALUE ' ENROLLED: '.
           05 PROG1-NO-ENRL PIC Z99.
           05 FILLER PIC X(42).
       01 PHIL-NUM-E.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 PHIL-NO-ENRL PIC Z99.
           05 FILLER PIC X(40).
       01 BIO-NUM-E.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 BIO-NO-ENRL PIC Z99.
           05 FILLER PIC X(40).
       01 PHYS-NUM-E.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 PHYS-NO-ENRL PIC Z99.
           05 FILLER PIC X(40).
       01 DMS-NUM-E.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 DMS-NO-ENRL PIC Z99.
           05 FILLER PIC X(40).
       01 PROG3-NUM-E.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 PROG3-NO-ENRL PIC Z99.
           05 FILLER PIC X(40).
       01 DISC-NUM-E.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 DISC-NO-ENRL PIC Z99.
           05 FILLER PIC X(40).
       01 GT-NUM-E.
           05 FILLER PIC X(30) VALUE 'GRAND TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 GT-NO-ENRL PIC Z,Z99.
           05 FILLER PIC X(32).
       01 OTHERS.
           05 EOF1 PIC X(3) VALUE 'NO'.
           05 EOF2 PIC X(3) VALUE 'NO'.
           05 EOF3 PIC X(3) VALUE 'NO'.
           05 EOF4 PIC X(3) VALUE 'NO'.
           05 EOF5 PIC X(3) VALUE 'NO'.
           05 EOF6 PIC X(3) VALUE 'NO'.
           05 EOF7 PIC X(3) VALUE 'NO'.
           05 TOT-PROG1 PIC 999 VALUE 0.
           05 TOT-PHIL PIC 999 VALUE 0.
           05 TOT-BIO PIC 999 VALUE 0.
           05 TOT-PHYS PIC 999 VALUE 0.
           05 TOT-DMS PIC 999 VALUE 0.
           05 TOT-PROG3 PIC 999 VALUE 0.
           05 TOT-DISC PIC 999 VALUE 0.
           05 GT-NUM-ENRL PIC 9999 VALUE 0.
       01 G-HDR1.
           05 FILLER PIC X(19) VALUE 'SUBJECT CODE   :   '.
           05 SUBJ-C PIC X(10).
           05 FILLER PIC X(51).
       01 G-HDR2.
           05 FILLER PIC X(19) VALUE 'FACULTY ID     :   '.
           05 FACUL-ID PIC X(12).
           05 FILLER PIC X(49).
       01 G-HDR3.
           05 FILLER PIC X(19) VALUE 'FACULTY NAME   :   '.
           05 FACUL-NAME PIC X(25).
           05 FILLER PIC X(36).
       01 G-HDR4.
           05 FILLER PIC X(14) VALUE 'STUDENT NUMBER'.
           05 FILLER PIC X(11).
           05 FILLER PIC X(12) VALUE 'STUDENT NAME'.
           05 FILLER PIC X(16).
           05 FILLER PIC X(13) VALUE 'AVERAGE GRADE'.
           05 FILLER PIC X(7).
           05 FILLER PIC X(7) VALUE 'REMARKS'.
       01 G-INFO.
           05 STUD-NO2 PIC X(12).
           05 FILLER PIC X(13).
           05 STUD-NA2 PIC X(25).
           05 FILLER PIC X(3).
           05 AVG-GRD PIC 9.99.
           05 FILLER PIC X(16).
           05 REMARK PIC X(7).
       01 FTR-TOT-PASS.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS2.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS2 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL2.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL2 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS3.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS3 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL3.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL3 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS4.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS4 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL4.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL4 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS5.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS5 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL5.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL5 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS6.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS6 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL6.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL6 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS7.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS7 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL7.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL7 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS8.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS8 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL8.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL8 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS9.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS9 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL9.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL9 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS10.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS10 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL10.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL10 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-PASS11.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 S-TOT-PASS11 PIC Z9.
           05 FILLER PIC X(41).
       01 FTR-TOT-FAIL11.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 S-TOT-FAIL11 PIC Z9.
           05 FILLER PIC X(41).
       01 OTHERS2.
           05 EOF8 PIC X(3) VALUE 'NO'.
           05 EOF9 PIC X(3) VALUE 'NO'.
           05 EOF10 PIC X(3) VALUE 'NO'.
           05 EOF11 PIC X(3) VALUE 'NO'.
           05 EOF12 PIC X(3) VALUE 'NO'.
           05 EOF13 PIC X(3) VALUE 'NO'.
           05 EOF14 PIC X(3) VALUE 'NO'.
           05 EOF15 PIC X(3) VALUE 'NO'.
           05 EOF16 PIC X(3) VALUE 'NO'.
           05 EOF17 PIC X(3) VALUE 'NO'.
           05 EOF18 PIC X(3) VALUE 'NO'.
           05 EOF19 PIC X(3) VALUE 'NO'.
           05 EOF20 PIC X(3) VALUE 'NO'.
           05 EOF21 PIC X(3) VALUE 'NO'.
           05 EOF22 PIC X(3) VALUE 'NO'.
           05 EOF23 PIC X(3) VALUE 'NO'.
           05 EOF24 PIC X(3) VALUE 'NO'.
           05 EOF25 PIC X(3) VALUE 'NO'.
           05 EOF26 PIC X(3) VALUE 'NO'.
           05 EOF27 PIC X(3) VALUE 'NO'.
           05 EOF28 PIC X(3) VALUE 'NO'.
           05 EOF29 PIC X(3) VALUE 'NO'.
           05 AVE-GRADE PIC 9V99 VALUE 0.
           05 S-PASS PIC 9 VALUE 0.
           05 S-FAIL PIC 9 VALUE 0.
           05 S-PASS2 PIC 9 VALUE 0.
           05 S-FAIL2 PIC 9 VALUE 0.
           05 S-PASS3 PIC 9 VALUE 0.
           05 S-FAIL3 PIC 9 VALUE 0.
           05 S-PASS4 PIC 9 VALUE 0.
           05 S-FAIL4 PIC 9 VALUE 0.
           05 S-PASS5 PIC 9 VALUE 0.
           05 S-FAIL5 PIC 9 VALUE 0.
           05 S-PASS6 PIC 9 VALUE 0.
           05 S-FAIL6 PIC 9 VALUE 0.
           05 S-PASS7 PIC 9 VALUE 0.
           05 S-FAIL7 PIC 9 VALUE 0.
           05 S-PASS8 PIC 9 VALUE 0.
           05 S-FAIL8 PIC 9 VALUE 0.
           05 S-PASS9 PIC 9 VALUE 0.
           05 S-FAIL9 PIC 9 VALUE 0.
           05 S-PASS10 PIC 9 VALUE 0.
           05 S-FAIL10 PIC 9 VALUE 0.
           05 S-PASS11 PIC 9 VALUE 0.
           05 S-FAIL11 PIC 9 VALUE 0.
       SCREEN SECTION.
       01 SCRN.
           05 BLANK SCREEN.
       PROCEDURE DIVISION.
           OPEN INPUT INPUT1
                INPUT INPUT3
               OUTPUT OUTPUT1.
           DISPLAY SCRN.
           PERFORM WRITE-HDNG.
           PERFORM PROG1-PRCS-RTN.
           CLOSE INPUT3.
           OPEN INPUT INPUT3.
           PERFORM PHILHIST-PRCS-RTN.
           CLOSE INPUT3.
           OPEN INPUT INPUT3.
           PERFORM BIO-PRCS-RTN.
           CLOSE INPUT3.
           OPEN INPUT INPUT3.
           PERFORM PHYS-PRCS-RTN.
           CLOSE INPUT3.
           OPEN INPUT INPUT3.
           PERFORM DMS-PRCS-RTN.
           CLOSE INPUT3.
           OPEN INPUT INPUT3.
           PERFORM PROG3-PRCS-RTN.
           CLOSE INPUT3.
           OPEN INPUT INPUT3.
           PERFORM DISC-PRCS-RTN.
           PERFORM FINISH-RTN.
           CLOSE INPUT1, INPUT3, OUTPUT1.
           OPEN INPUT INPUT2
                INPUT INPUT3
                INPUT INPUT4
                OUTPUT OUTPUT2.
           PERFORM WRITE-HDNG2.
           PERFORM PRCS-RTN.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN2.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN3.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN4.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN5.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN6.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN7.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN8.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN9.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN10.
           CLOSE INPUT3, INPUT4.
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN11.
           CLOSE INPUT2, INPUT3, INPUT4, OUTPUT2.
           DISPLAY 'END OF MERGING PROGRAM!'.
           STOP RUN.
       WRITE-HDNG.
           WRITE ENROLL FROM HDNG1.
           WRITE ENROLL FROM HDNG2.
           WRITE ENROLL FROM BLANK-HEADING AFTER 2.
           WRITE ENROLL FROM HDNG3.
           WRITE ENROLL FROM BLANK-HEADING AFTER 2.
       PROG1-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'IT 2001   '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF1.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM PROG1-COMPUTE-RTN UNTIL EOF1 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM PROG1-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
       PROG1-COMPUTE-RTN.
           IF SUBJ-CODE3 = 'IT 2001   '
               MOVE STUD-NUM TO STUD-NO
               MOVE STUD-NAME TO STUD-NA
               ADD 1 TO TOT-PROG1
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-PROG1 TO PROG1-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL FROM E-INFO.
           READ INPUT3 AT END MOVE 'YES' TO EOF1.
       PHILHIST-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'HIST 1000 '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF2.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM PHILHIST-COMPUTE-RTN UNTIL EOF2 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM PHIL-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
       PHILHIST-COMPUTE-RTN.
           IF SUBJ-CODE3 = 'HIST 1000 '
               MOVE STUD-NUM TO STUD-NO
               MOVE STUD-NAME TO STUD-NA
               ADD 1 TO TOT-PHIL
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-PHIL TO PHIL-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL FROM E-INFO.
           READ INPUT3 AT END MOVE 'YES' TO EOF2.
       BIO-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'NATSCI 100'
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF3.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM BIO-COMPUTE-RTN UNTIL EOF3 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM BIO-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
       BIO-COMPUTE-RTN.
           IF SUBJ-CODE3 = 'NATSCI 100'
               MOVE STUD-NUM TO STUD-NO
               MOVE STUD-NAME TO STUD-NA
               ADD 1 TO TOT-BIO
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-BIO TO BIO-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL FROM E-INFO.
           READ INPUT3 AT END MOVE 'YES' TO EOF3.
       PHYS-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'NATSCI 105'
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF4.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM PHYS-COMPUTE-RTN UNTIL EOF4 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM PHYS-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
       PHYS-COMPUTE-RTN.
           IF SUBJ-CODE3 = 'NATSCI 105'
               MOVE STUD-NUM TO STUD-NO
               MOVE STUD-NAME TO STUD-NA
               ADD 1 TO TOT-PHYS
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-PHYS TO PHYS-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL FROM E-INFO.
           READ INPUT3 AT END MOVE 'YES' TO EOF4.
       DMS-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'COMP 2000 '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF5.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM DMS-COMPUTE-RTN UNTIL EOF5 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM DMS-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
       DMS-COMPUTE-RTN.
           IF SUBJ-CODE3 = 'COMP 2000 '
               MOVE STUD-NUM TO STUD-NO
               MOVE STUD-NAME TO STUD-NA
               ADD 1 TO TOT-DMS
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-DMS TO DMS-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL FROM E-INFO.
           READ INPUT3 AT END MOVE 'YES' TO EOF5.
       PROG3-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'IT 2003      '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF6.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM PROG3-COMPUTE-RTN UNTIL EOF6 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM PROG3-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
       PROG3-COMPUTE-RTN.
           IF SUBJ-CODE3 = 'IT 2003      '
               MOVE STUD-NUM TO STUD-NO
               MOVE STUD-NAME TO STUD-NA
               ADD 1 TO TOT-PROG3
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-PROG3 TO PROG3-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL FROM E-INFO.
           READ INPUT3 AT END MOVE 'YES' TO EOF6.
       DISC-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'MATH 1000 '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF7.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM DISC-COMPUTE-RTN UNTIL EOF7 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM DISC-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
       DISC-COMPUTE-RTN.
           IF SUBJ-CODE3 = 'MATH 1000 '
               MOVE STUD-NUM TO STUD-NO
               MOVE STUD-NAME TO STUD-NA
               ADD 1 TO TOT-DISC
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-DISC TO DISC-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL FROM E-INFO.
           READ INPUT3 AT END MOVE 'YES' TO EOF7.
       FINISH-RTN.
           WRITE ENROLL FROM BLANK-HEADING.
           WRITE ENROLL FROM GT-NUM-E.
       WRITE-HDNG2.
           WRITE GRADES FROM HDNG1.
           WRITE GRADES FROM HDNG2.
           WRITE GRADES FROM BLANK-HEADING AFTER 2.
           WRITE GRADES FROM HDNG3.
           WRITE GRADES FROM BLANK-HEADING AFTER 2.
       PRCS-RTN.
           READ INPUT2.
           IF SUBJ-CODE2 = 'COMP 2000 ' AND
               FACULTY-ID = '67890          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF8.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL EOF8 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN UNTIL EOF9 = 'YES'
               PERFORM BREAK-RTN
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF8.
       FIN-COMP-RTN.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS = S-PASS + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL = S-FAIL + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF9.
       BREAK-RTN.
           MOVE S-PASS TO S-TOT-PASS
           MOVE S-FAIL TO S-TOT-FAIL.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF9.
       PRCS-RTN2.
           READ INPUT2.
           IF SUBJ-CODE2 = 'NATSCI 105' AND
               FACULTY-ID = '12345          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF10.
               READ INPUT4.
               PERFORM COMP-RTN2 UNTIL EOF10 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS2.
               WRITE GRADES FROM FTR-TOT-FAIL2.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN2.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN2 UNTIL EOF11 = 'YES'
               PERFORM BREAK-RTN2
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF10.
       FIN-COMP-RTN2.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS2 = S-PASS2 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL2 = S-FAIL2 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF11.
       BREAK-RTN2.
           MOVE S-PASS2 TO S-TOT-PASS2
           MOVE S-FAIL2 TO S-TOT-FAIL2.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF11.
       PRCS-RTN3.
           READ INPUT2.
           IF SUBJ-CODE2 = 'IT 2001      ' AND
               FACULTY-ID = '34567          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF12.
               READ INPUT4.
               PERFORM COMP-RTN3 UNTIL EOF12 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS3.
               WRITE GRADES FROM FTR-TOT-FAIL3.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN3.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN3 UNTIL EOF13 = 'YES'
               PERFORM BREAK-RTN3
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF12.
       FIN-COMP-RTN3.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS3 = S-PASS3 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL3 = S-FAIL3 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF13.
       BREAK-RTN3.
           MOVE S-PASS3 TO S-TOT-PASS3
           MOVE S-FAIL3 TO S-TOT-FAIL3.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF13.
       PRCS-RTN4.
           READ INPUT2.
           IF SUBJ-CODE2 = 'COMP 2000 ' AND
               FACULTY-ID = '23456          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF14.
               READ INPUT4.
               PERFORM COMP-RTN4 UNTIL EOF14 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS4.
               WRITE GRADES FROM FTR-TOT-FAIL4.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN4.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN4 UNTIL EOF15 = 'YES'
               PERFORM BREAK-RTN4
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF14.
       FIN-COMP-RTN4.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS4 = S-PASS4 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL4 = S-FAIL4 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF15.
       BREAK-RTN4.
           MOVE S-PASS4 TO S-TOT-PASS4
           MOVE S-FAIL4 TO S-TOT-FAIL4.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF15.
       PRCS-RTN5.
           READ INPUT2.
           IF SUBJ-CODE2 = 'HIST 1000 ' AND
               FACULTY-ID = '45678          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF16.
               READ INPUT4.
               PERFORM COMP-RTN5 UNTIL EOF16 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS5.
               WRITE GRADES FROM FTR-TOT-FAIL5.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN5.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN5 UNTIL EOF17 = 'YES'
               PERFORM BREAK-RTN5
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF16.
       FIN-COMP-RTN5.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS5 = S-PASS5 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL5 = S-FAIL5 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF17.
       BREAK-RTN5.
           MOVE S-PASS5 TO S-TOT-PASS5
           MOVE S-FAIL5 TO S-TOT-FAIL5.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF17.
       PRCS-RTN6.
           READ INPUT2.
           IF SUBJ-CODE2 = 'IT 2003      ' AND
               FACULTY-ID = '56789          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF18.
               READ INPUT4.
               PERFORM COMP-RTN6 UNTIL EOF18 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS6.
               WRITE GRADES FROM FTR-TOT-FAIL6.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN6.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN6 UNTIL EOF19 = 'YES'
               PERFORM BREAK-RTN6
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF18.
       FIN-COMP-RTN6.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS6 = S-PASS6 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL6 = S-FAIL6 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF19.
       BREAK-RTN6.
           MOVE S-PASS6 TO S-TOT-PASS6.
           MOVE S-FAIL6 TO S-TOT-FAIL6.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF19.
       PRCS-RTN7.
           READ INPUT2.
           IF SUBJ-CODE2 = 'MATH 1000 ' AND
               FACULTY-ID = '89012          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF20.
               READ INPUT4.
               PERFORM COMP-RTN7 UNTIL EOF20 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS7.
               WRITE GRADES FROM FTR-TOT-FAIL7.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN7.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN7 UNTIL EOF21 = 'YES'
               PERFORM BREAK-RTN7
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF20.
       FIN-COMP-RTN7.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS7 = S-PASS7 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL7 = S-FAIL7 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF21.
       BREAK-RTN7.
           MOVE S-PASS7 TO S-TOT-PASS7
           MOVE S-FAIL7 TO S-TOT-FAIL7.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF21.
       PRCS-RTN8.
           READ INPUT2.
           IF SUBJ-CODE2 = 'NATSCI 100' AND
               FACULTY-ID = '12345          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF22.
               READ INPUT4.
               PERFORM COMP-RTN8 UNTIL EOF22 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS8.
               WRITE GRADES FROM FTR-TOT-FAIL8.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN8.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN8 UNTIL EOF23 = 'YES'
               PERFORM BREAK-RTN8
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF22.
       FIN-COMP-RTN8.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS8 = S-PASS8 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL8 = S-FAIL8 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF23.
       BREAK-RTN8.
           MOVE S-PASS8 TO S-TOT-PASS8
           MOVE S-FAIL8 TO S-TOT-FAIL8.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF23.
       PRCS-RTN9.
           READ INPUT2.
           IF SUBJ-CODE2 = 'COMP 2000 ' AND
               FACULTY-ID = '34567          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF24.
               READ INPUT4.
               PERFORM COMP-RTN9 UNTIL EOF24 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS9.
               WRITE GRADES FROM FTR-TOT-FAIL9.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN9.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN9 UNTIL EOF25 = 'YES'
               PERFORM BREAK-RTN9
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF24.
       FIN-COMP-RTN9.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS9 = S-PASS9 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL9 = S-FAIL9 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF25.
       BREAK-RTN9.
           MOVE S-PASS9 TO S-TOT-PASS9
           MOVE S-FAIL9 TO S-TOT-FAIL9.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF25.
       PRCS-RTN10.
           READ INPUT2.
           IF SUBJ-CODE2 = 'IT 2001      ' AND
               FACULTY-ID = '56789          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF26.
               READ INPUT4.
               PERFORM COMP-RTN10 UNTIL EOF26 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS10.
               WRITE GRADES FROM FTR-TOT-FAIL10.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN10.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN10 UNTIL EOF27 = 'YES'
               PERFORM BREAK-RTN10
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF26.
       FIN-COMP-RTN10.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS10 = S-PASS10 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL10 = S-FAIL10 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF27.
       BREAK-RTN10.
           MOVE S-PASS10 TO S-TOT-PASS10
           MOVE S-FAIL10 TO S-TOT-FAIL10.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF27.
       PRCS-RTN11.
           READ INPUT2.
           IF SUBJ-CODE2 = 'MATH 1000 ' AND
               FACULTY-ID = '78901          '
               MOVE SUBJ-CODE2 TO SUBJ-C.
               MOVE FACULTY-ID TO FACUL-ID.
               MOVE FACULTY-NAME TO FACUL-NAME.
               WRITE GRADES FROM G-HDR1.
               WRITE GRADES FROM G-HDR2.
               WRITE GRADES FROM G-HDR3.
               WRITE GRADES FROM G-HDR4.
               READ INPUT3 AT END MOVE 'YES' TO EOF28.
               READ INPUT4.
               PERFORM COMP-RTN11 UNTIL EOF28 = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS11.
               WRITE GRADES FROM FTR-TOT-FAIL11.
               WRITE GRADES FROM BLANK-HEADING.
       COMP-RTN11.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN11 UNTIL EOF29 = 'YES'
               PERFORM BREAK-RTN11
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO EOF28.
       FIN-COMP-RTN11.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD
               IF AVE-GRADE <= 3.12
                   MOVE 'PASSED ' TO REMARK
                   COMPUTE S-PASS11 = S-PASS11 + 1
               ELSE
                   MOVE 'FAILED ' TO REMARK
                   COMPUTE S-FAIL11 = S-FAIL11 + 1.
           READ INPUT4 AT END MOVE 'YES' TO EOF29.
       BREAK-RTN11.
           MOVE S-PASS11 TO S-TOT-PASS11
           MOVE S-FAIL11 TO S-TOT-FAIL11.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO EOF29.
