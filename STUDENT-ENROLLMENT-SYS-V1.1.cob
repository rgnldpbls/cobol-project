      *=================================================================
      *------------------------- COBOL PROGRAM -------------------------
      *------------- STUDENT ENROLLMENT AND GRADING SYSTEM -------------
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINAL-PROJECT.
       AUTHOR. JASPE, FAYE
               LOZADA, LUEN
               PUEBLOS, REGIENALD
               PORLARES, ALEXANDER
               RONCALES, JALLANE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT1 ASSIGN TO 'SUBJECT.TXT'.
           SELECT INPUT2 ASSIGN TO 'FACULTY.TXT'.
           SELECT INPUT3 ASSIGN TO 'STUDENT.TXT'.
           SELECT INPUT4 ASSIGN TO 'GRADE.TXT'.
           SELECT OUTPUT1 ASSIGN TO 'ENROLL.TXT'.
           SELECT OUTPUT2 ASSIGN TO 'GRADES.TXT'.

      *VARIABLES DECLARATION    
       DATA DIVISION.
       FILE SECTION.
      *FOR INPUT FILES
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
      *FOR OUTPUT FILES
       FD OUTPUT1.
       01 ENROLL.
           05 FILLER PIC X(80).
       FD OUTPUT2.
       01 GRADES.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
      *ENROLLMENT SYSTEM: HEADERS
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

      *ENROLLMENT SYSTEM: FOOTER
       01 FTR-STUD-NUM-E.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(11) VALUE ' ENROLLED: '.
           05 TOT-NUM-ENRL PIC Z99.
           05 FILLER PIC X(42).
       01 GT-NUM-E.
           05 FILLER PIC X(30) VALUE 'GRAND TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 GT-NO-ENRL PIC Z,Z99.
           05 FILLER PIC X(32).

       01 ENROLL-OTHERS.
           05 EOF1 PIC X(3) VALUE 'NO'.
           05 TOT-ENRL PIC 999 VALUE 0.
           05 GT-NUM-ENRL PIC 9999 VALUE 0.

      *GRADING SYSTEM: HEADERS
       01 HDNG4.
           05 FILLER PIC X(30).
           05 FILLER PIC X(20) VALUE 'STUDENT GRADE REPORT'.
           05 FILLER PIC X(30).
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

      *OUTPUT VARIABLES FOR GRADING SYSTEM
       01 G-INFO.
           05 STUD-NO2 PIC X(12).
           05 FILLER PIC X(13).
           05 STUD-NA2 PIC X(25).
           05 FILLER PIC X(3).
           05 AVG-GRD PIC 9.99.
           05 FILLER PIC X(16).
           05 REMARK PIC X(7).

      *GRADING SYSTEM: FOOTERS
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

       01 OTHER-GRADING.
           05 G-EOF PIC X(3) VALUE 'NO'.
           05 G-EOF2 PIC X(3) VALUE 'NO'.
      *COUNTERS FOR STUDENT PASSED AND FAILED
           05 S-PASS PIC 9 VALUE 0.
           05 S-FAIL PIC 9 VALUE 0.
           05 AVE-GRADE PIC 9V99 VALUE 0.

       SCREEN SECTION.
       01 SCRN.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
      *======================= ENROLLMENT SYSTEM =======================
           OPEN INPUT INPUT1
                INPUT INPUT3
               OUTPUT OUTPUT1.
           DISPLAY SCRN.
      *DISPLAY HEADERS
           PERFORM WRITE-HDNG.
      *PROGRAMMING 1
           PERFORM PROG1-PRCS-RTN.
           CLOSE INPUT3.
      *PHIL. HISTORY
           OPEN INPUT INPUT3.
           PERFORM PHILHIST-PRCS-RTN.
           CLOSE INPUT3.
      *BIOLOGY
           OPEN INPUT INPUT3.
           PERFORM BIO-PRCS-RTN.
           CLOSE INPUT3.
      *PHYSICS
           OPEN INPUT INPUT3.
           PERFORM PHYS-PRCS-RTN.
           CLOSE INPUT3.
      *DATABASE MANAGEMENT SYSTEMS
           OPEN INPUT INPUT3.
           PERFORM DMS-PRCS-RTN.
           CLOSE INPUT3.
      *PROGRAMMING 3
           OPEN INPUT INPUT3.
           PERFORM PROG3-PRCS-RTN.
           CLOSE INPUT3.
      *DISCRETE STRUCTURE
           OPEN INPUT INPUT3.
           PERFORM DISC-PRCS-RTN.
           PERFORM FINISH-RTN.
           CLOSE INPUT1, INPUT3, OUTPUT1.

      *======================== GRADING SYSTEM =========================
           OPEN INPUT INPUT2
                INPUT INPUT3
                INPUT INPUT4
                OUTPUT OUTPUT2.
      *DISPLAY HEADERS
           PERFORM WRITE-HDNG2.
      *COMP 2000 DATABASE MANAGEMENT SYSTEMS WITH FACULTY ID: 67890
           PERFORM PRCS-RTN.
           CLOSE INPUT3, INPUT4.
      *NATSCI 105 PHYSICS WITH FACULTY ID: 12345
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN2.
           CLOSE INPUT3, INPUT4.
      *IT 2001 PROGRAMMING 1 WITH FACULTY ID: 34567
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN3.
           CLOSE INPUT3, INPUT4.
      *COMP 2000 DATABASE MANAGEMENT SYSTEMS WITH FACULTY ID: 23456
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN4.
           CLOSE INPUT3, INPUT4.
      *HIST 1000 PHIL. HISTORY WITH FACULTY ID: 45678
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN5.
           CLOSE INPUT3, INPUT4.
      *IT 2003 PROGRAMMING 3 WITH FACULTY ID: 56789
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN6.
           CLOSE INPUT3, INPUT4.
      *MATH 1000 DISCRETE STRUCTURE WITH FACULTY ID: 89012
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN7.
           CLOSE INPUT3, INPUT4.
      *NATSCI 100 BIOLOGY WITH FACULTY ID: 12345
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN8.
           CLOSE INPUT3, INPUT4.
      *COMP 2000 DATABASE MANAGEMENT SYSTEMS WITH FACULTY ID: 34567
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN9.
           CLOSE INPUT3, INPUT4.
      *IT 2001 PROGRAMMING 1 WITH FACULTY ID: 56789
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN10.
           CLOSE INPUT3, INPUT4.
      *MATH 1000 DISCRETE STRUCTURE WITH FACULTY ID: 78901
           OPEN INPUT INPUT3
                INPUT INPUT4.
           PERFORM PRCS-RTN11.
           CLOSE INPUT2, INPUT3, INPUT4, OUTPUT2.
      *DISPLAY TO CONSOLE
           DISPLAY '--------------------------- END OF MERGING PROGRAM'
           ' ----------------------------'.
           DISPLAY '                       PLEASE CHECK THE '
           'ENROLL.TXT FILE                        ' AT LINE 2.
           DISPLAY '                       PLEASE CHECK THE '
           'GRADES.TXT FILE                        ' AT LINE 3.
           STOP RUN.

      *=================================================================
      *===================== ENROLLMENT SYSTEM RTNs ====================
      *=================================================================
      *HEADER FOR ENROLLMENT
       WRITE-HDNG.
           WRITE ENROLL FROM HDNG1.
           WRITE ENROLL FROM HDNG2.
           WRITE ENROLL FROM BLANK-HEADING AFTER 2.
           WRITE ENROLL FROM HDNG3.
           WRITE ENROLL FROM BLANK-HEADING AFTER 2.

      *PROGRAMMING 1
       PROG1-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'IT 2001   '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF1.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM ENRL-COMPUTE-RTN UNTIL EOF1 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM FTR-STUD-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
               PERFORM ENRL-BREAK-RTN.

      *PHIL. HISTORY
       PHILHIST-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'HIST 1000 '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF1.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM ENRL-COMPUTE-RTN UNTIL EOF1 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM FTR-STUD-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
               PERFORM ENRL-BREAK-RTN.

      *BIOLOGY
       BIO-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'NATSCI 100'
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF1.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM ENRL-COMPUTE-RTN UNTIL EOF1 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM FTR-STUD-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
               PERFORM ENRL-BREAK-RTN.

      *PHYSICS
       PHYS-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'NATSCI 105'
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF1.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM ENRL-COMPUTE-RTN UNTIL EOF1 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM FTR-STUD-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
               PERFORM ENRL-BREAK-RTN.

      *DATABASE MANAGEMENT SYSTEMS
       DMS-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'COMP 2000 '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF1.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM ENRL-COMPUTE-RTN UNTIL EOF1 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM FTR-STUD-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
               PERFORM ENRL-BREAK-RTN.

      *PROGRAMMING 3
       PROG3-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'IT 2003      '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF1.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM ENRL-COMPUTE-RTN UNTIL EOF1 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM FTR-STUD-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
               PERFORM ENRL-BREAK-RTN.

      *DISCRETE STRUCTURE
       DISC-PRCS-RTN.
           READ INPUT1.
           IF SUBJ-CODE = 'MATH 1000 '
               MOVE SUBJ-CODE TO SUB-CD.
               MOVE SUBJ-DESC TO SUB-DESC.
               READ INPUT3 AT END MOVE 'YES' TO EOF1.
               WRITE ENROLL FROM HDR1.
               WRITE ENROLL FROM HDR2.
               WRITE ENROLL FROM HDR3.
               PERFORM ENRL-COMPUTE-RTN UNTIL EOF1 = 'YES'.
               WRITE ENROLL FROM BLANK-HEADING.
               WRITE ENROLL FROM FTR-STUD-NUM-E.
               WRITE ENROLL FROM BLANK-HEADING.
               PERFORM ENRL-BREAK-RTN.

      *COMPUTE RTN FOR EACH SUBJECT
       ENRL-COMPUTE-RTN.
           IF SUBJ-CODE3 = SUB-CD
               MOVE STUD-NUM TO STUD-NO
               MOVE STUD-NAME TO STUD-NA
               ADD 1 TO TOT-ENRL
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-ENRL TO TOT-NUM-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL FROM E-INFO.
           READ INPUT3 AT END MOVE 'YES' TO EOF1.

       ENRL-BREAK-RTN.
           MOVE 0 TO TOT-ENRL.
           MOVE 0 TO TOT-NUM-ENRL.
           MOVE 'NO' TO EOF1.

       FINISH-RTN.
           WRITE ENROLL FROM BLANK-HEADING.
           WRITE ENROLL FROM GT-NUM-E.

      *=================================================================
      *====================== GRADING SYSTEM RTNs ======================
      *=================================================================
      *HEADERS FOR GRADING SYSTEM
       WRITE-HDNG2.
           WRITE GRADES FROM HDNG1.
           WRITE GRADES FROM HDNG2.
           WRITE GRADES FROM BLANK-HEADING AFTER 2.
           WRITE GRADES FROM HDNG4.
           WRITE GRADES FROM BLANK-HEADING AFTER 2.

      *COMP 2000 DATABASE MANAGEMENT SYSTEMS WITH FACULTY ID: 67890
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *NATSCI 105 PHYSICS WITH FACULTY ID: 12345
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *IT 2001 PROGRAMMING 1 WITH FACULTY ID: 34567
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *COMP 2000 DATABASE MANAGEMENT SYSTEMS WITH FACULTY ID: 23456
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *HIST 1000 PHIL. HISTORY WITH FACULTY ID: 45678
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *IT 2003 PROGRAMMING 3 WITH FACULTY ID: 56789
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *MATH 1000 DISCRETE STRUCTURE WITH FACULTY ID: 89012
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *NATSCI 100 BIOLOGY WITH FACULTY ID: 12345
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *COMP 2000 DATABASE MANAGEMENT SYSTEMS WITH FACULTY ID: 34567
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *IT 2001 PROGRAMMING 1 WITH FACULTY ID: 56789
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.



      *MATH 1000 DISCRETE STRUCTURE WITH FACULTY ID: 78901
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
               READ INPUT3 AT END MOVE 'YES' TO G-EOF.
               READ INPUT4.
               PERFORM COMP-RTN UNTIL G-EOF = 'YES'.
               WRITE GRADES FROM BLANK-HEADING.
               WRITE GRADES FROM FTR-TOT-PASS.
               WRITE GRADES FROM FTR-TOT-FAIL.
               WRITE GRADES FROM BLANK-HEADING.
               PERFORM ULTRA-BREAK-TRN.

      *COMPUTE RTN AND FINAL COMP RTN FOR EACH PROCESS
       COMP-RTN.
           IF SUBJ-CODE3 = SUBJ-C AND FACULTY-ID2 = FACUL-ID
               MOVE STUD-NUM TO STUD-NO2
               MOVE STUD-NAME TO STUD-NA2
               PERFORM FIN-COMP-RTN UNTIL G-EOF2 = 'YES'
               PERFORM GRD-BREAK-RTN
               OPEN INPUT INPUT4.
           READ INPUT3 AT END MOVE 'YES' TO G-EOF.
       FIN-COMP-RTN.
           IF SUBJ-CODE4 = SUBJ-C AND FACULTY-ID3 = FACUL-ID AND
               STUD-NUM2 = STUD-NO2
               COMPUTE AVE-GRADE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE AVE-GRADE TO AVG-GRD.
           READ INPUT4 AT END MOVE 'YES' TO G-EOF2.

       GRD-BREAK-RTN.
           IF AVE-GRADE <= 3.12
               MOVE 'PASSED ' TO REMARK
               ADD 1 TO S-PASS
           ELSE
               MOVE 'FAILED ' TO REMARK
               ADD 1 TO S-FAIL.
           MOVE S-PASS TO S-TOT-PASS.
           MOVE S-FAIL TO S-TOT-FAIL.
           WRITE GRADES FROM G-INFO.
           CLOSE INPUT4.
           MOVE 'NO' TO G-EOF2.

       ULTRA-BREAK-TRN.
           MOVE 0 TO S-TOT-PASS.
           MOVE 0 TO S-TOT-FAIL.
           MOVE 0 TO S-PASS.
           MOVE 0 TO S-FAIL.
           MOVE 0 TO AVE-GRADE.
           MOVE 'NO' TO G-EOF.
           MOVE 'NO' TO G-EOF2.
