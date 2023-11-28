
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERGING-FILES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUD1 ASSIGN TO 'STUDENT.TXT'.
           SELECT STUD2 ASSIGN TO 'SUBJECT.TXT'.
           SELECT STUD3 ASSIGN TO 'FACULTY.TXT'.
           SELECT STUD4 ASSIGN TO 'GRADE.TXT'.
           SELECT STUD5 ASSIGN TO 'ENROLL.TXT'.
           SELECT STUD6 ASSIGN TO 'GRADES.TXT'.
       DATA DIVISION.
       FILE SECTION.
       FD  STUD1
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 59 CHARACTERS
           DATA RECORD IS STUD-INFO.
       01  STUD-INFO.
           05 SUBJ-CODE PIC X(10).
           05 FACULTY-ID PIC X(12).
           05 STUD-NUM PIC X(12).
           05 STUD-NAME PIC X(25).
       FD  STUD2
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 45 CHARACTERS
           DATA RECORD IS SUB-INFO.
       01  SUB-INFO.
           05 SUBJECT-CODE PIC X(10).
           05 FILLER PIC X(5).
           05 SUBJ-DES PIC X(30).
       FD  STUD3
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 55 CHARACTERS
           DATA RECORD IS FAC-INFO.
       01 FAC-INFO.
           05 SUB-CODE PIC X(10).
           05 FILLER PIC X(4).
           05 FAC-ID PIC X(12).
           05 FILLER PIC X(4).
           05 FAC-NAME PIC X(25).
       FD  STUD4
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 48 CHARACTERS
           DATA RECORD IS GRADE-INFO.
       01 GRADE-INFO.
           05 S-CODE PIC X(10).
           05 FILLER PIC X(2).
           05 F-ID PIC X(12).
           05 FILLER PIC X(2).
           05 ST-NUM PIC X(12).
           05 FILLER PIC X(2).
           05 MID-GRADE PIC 9V99.
           05 FILLER PIC X(2).
           05 FINAL-GRADE PIC 9V99.
       FD  STUD5.
       01  MERGING.
           05 FILLER PIC X(80).
       FD  STUD6.
       01  GR-MERGING.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01  PROG-OUT.
           05 PROGNO PIC X(10).
           05 FILLER PIC X(15).
           05 PROGNA PIC X(20).
       01  PHIL-OUT.
           05 PHILNO PIC X(10).
           05 FILLER PIC X(15).
           05 PHILNA PIC X(15).
       01  BIO-OUT.
           05 BIONO PIC X(10).
           05 FILLER PIC X(15).
           05 BIONA PIC X(20).
       01  PHY-OUT.
           05 PHYNO PIC X(10).
           05 FILLER PIC X(15).
           05 PHYNA PIC X(20).
       01  DATA-OUT.
           05 DATANO PIC X(10).
           05 FILLER PIC X(15).
           05 DATANA PIC X(20).
       01  PROGRA-OUT.
           05 PROGRANO PIC X(10).
           05 FILLER PIC X(15).
           05 PROGRANA PIC X(20).
       01  AVG-OUT.
           05 FILLER PIC X(15).
           05 AVERAGE PIC 9.99.
           05 FILLER PIC X(15).
       01  DISC-OUT.
           05 DISCNO PIC X(10).
           05 FILLER PIC X(15).
           05 DISCNA PIC X(20).
       01  PROG-GRADE-OUT.
           05 PROG-NO PIC X(10).
           05 FILLER PIC X(15).
           05 PROG-NA PIC X(20).
           05 FILLER PIC X(8).
           05 PROG-AVG PIC 9.99.
           05 FILLER PIC X(8).
           05 PROG-RMRKS PIC X(6).
       01  PHIL-GRADE-OUT.
           05 PHIL-NO PIC X(10).
           05 FILLER PIC X(15).
           05 PHIL-NA PIC X(20).
           05 FILLER PIC X(8).
           05 PHIL-AVG PIC 9.99.
           05 FILLER PIC X(8).
           05 PHIL-RMRKS PIC X(6).
       01  BIO-GRADE-OUT.
           05 BIO-NO PIC X(10).
           05 FILLER PIC X(15).
           05 BIO-NA PIC X(20).
           05 FILLER PIC X(8).
           05 BIO-AVG PIC 9.99.
           05 FILLER PIC X(8).
           05 BIO-RMRKS PIC X(6).
       01  PHY-GRADE-OUT.
           05 PHY-NO PIC X(10).
           05 FILLER PIC X(15).
           05 PHY-NA PIC X(20).
           05 FILLER PIC X(8).
           05 PHY-AVG PIC 9.99.
           05 FILLER PIC X(8).
           05 PHY-RMRKS PIC X(6).
       01  DATA-GRADE-OUT.
           05 DATA-NO PIC X(10).
           05 FILLER PIC X(15).
           05 DATA-NA PIC X(20).
           05 FILLER PIC X(8).
           05 DATA-AVG PIC 9.99.
           05 FILLER PIC X(8).
           05 DATA-RMRKS PIC X(6).
       01  PROGRA-GRADE-OUT.
           05 PROGRA-NO PIC X(10).
           05 FILLER PIC X(15).
           05 PROGRA-NA PIC X(20).
           05 FILLER PIC X(8).
           05 PROGRA-AVG PIC 9.99.
           05 FILLER PIC X(8).
           05 PROGRA-RMRKS PIC X(6).
       01  DISC-GRADE-OUT.
           05 DISC-NO PIC X(10).
           05 FILLER PIC X(15).
           05 DISC-NA PIC X(20).
           05 FILLER PIC X(8).
           05 DISC-AVG PIC 9.99.
           05 FILLER PIC X(8).
           05 DISC-RMRKS PIC X(6).
       01  ACADEMY.
           05 FILLER PIC X(29).
           05 FILLER PIC X(22) VALUE 'TRI-STAR ACADEMY'.
           05 FILLER PIC X(29).
       01  ADDRESS-HDR.
           05 FILLER PIC X(26).
           05 FILLER PIC X(25) VALUE 'GREENHILLS, SAN JUAN'.
           05 FILLER PIC X(26).
       01  HEADER1.
           05 FILLER PIC X(25).
           05 FILLER PIC X(30) VALUE 'STUDENT ENROLMENT REPORT'.
           05 FILLER PIC X(25).
       01  PROG-HDR.
           05 FILLER PIC X(15) VALUE 'SUBJECT CODE:  '.
           05 PROG-CODE PIC X(22).
           05 FILLER PIC X(30).
       01  SUBJECT-PROG.
           05 FILLER PIC X(24) VALUE 'SUBJECT DESCRIPTION:   '.
           05 PROG-DES PIC X(14).
           05 FILLER PIC X(32).
       01  TOT-PROG-ENR.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS '.
           05 FILLER PIC X(11) VALUE 'ENROLLED: '.
           05 TOT-PROG PIC Z99.
       01  PHIL-HDR.
           05 FILLER PIC X(15) VALUE 'SUBJECT CODE:  '.
           05 PHIL-CODE PIC X(25).
           05 FILLER PIC X(32).
       01  SUBJECT-PHIL.
           05 FILLER PIC X(24) VALUE 'SUBJECT DESCRIPTION:   '.
           05 PHIL-DES PIC X(16).
           05 FILLER PIC X(35).
       01  TOT-PHIL-ENR.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS '.
           05 FILLER PIC X(11) VALUE 'ENROLLED: '.
           05 TOT-PHIL PIC Z99.
       01  BIO-HDR.
           05 FILLER PIC X(15) VALUE 'SUBJECT CODE:  '.
           05 BIO-CODE PIC X(20).
           05 FILLER PIC X(34).
       01  SUBJECT-BIO.
           05 FILLER PIC X(24) VALUE 'SUBJECT DESCRIPTION:   '.
           05 BIO-DES PIC X(18).
           05 FILLER PIC X(35).
       01  TOT-BIO-ENR.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS '.
           05 FILLER PIC X(11) VALUE 'ENROLLED: '.
           05 TOT-BIO PIC Z99.
       01  PHY-HDR.
           05 FILLER PIC X(15) VALUE 'SUBJECT CODE:  '.
           05 PHY-CODE PIC X(26).
           05 FILLER PIC X(29).
       01  SUBJECT-PHY.
           05 FILLER PIC X(24) VALUE 'SUBJECT DESCRIPTION:   '.
           05 PHY-DES PIC X(20).
           05 FILLER PIC X(38).
       01  TOT-PHY-ENR.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS '.
           05 FILLER PIC X(11) VALUE 'ENROLLED: '.
           05 TOT-PHY PIC Z99.
       01  DATA-HDR.
           05 FILLER PIC X(15) VALUE 'SUBJECT CODE:  '.
           05 DATA-CODE PIC X(40).
           05 FILLER PIC X(35).
       01  SUBJECT-DATA.
           05 FILLER PIC X(24) VALUE 'SUBJECT DESCRIPTION:   '.
           05 DATA-DES PIC X(30).
           05 FILLER PIC X(40).
       01  TOT-DATA-ENR.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS '.
           05 FILLER PIC X(11) VALUE 'ENROLLED: '.
           05 TOT-DATA PIC Z99.
       01  PROGRA-HDR.
           05 FILLER PIC X(15) VALUE 'SUBJECT CODE:  '.
           05 PROGRA-CODE PIC X(23).
           05 FILLER PIC X(26).
       01  SUBJECT-PROGRA.
           05 FILLER PIC X(24) VALUE 'SUBJECT DESCRIPTION:   '.
           05 PROGRA-DES PIC X(15).
           05 FILLER PIC X(35).
       01  TOT-PROGRA-ENR.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS '.
           05 FILLER PIC X(11) VALUE 'ENROLLED: '.
           05 TOT-PROGRA PIC Z99.
       01  DISC-HDR.
           05 FILLER PIC X(15) VALUE 'SUBJECT CODE:  '.
           05 DISC-CODE PIC X(33).
           05 FILLER PIC X(18).
       01  SUBJECT-DISC.
           05 FILLER PIC X(24) VALUE 'SUBJECT DESCRIPTION:   '.
           05 DISC-DES PIC X(20).
           05 FILLER PIC X(35).
       01  TOT-DISC-ENR.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS '.
           05 FILLER PIC X(11) VALUE 'ENROLLED: '.
           05 TOT-DISC PIC Z99.
       01  STUDENTINFO.
           05 FILLER PIC X(9).
           05 FILLER PIC X(25) VALUE 'STUDENT NUMBER:         '.
           05 FILLER PIC X(9).
           05 FILLER PIC X(25) VALUE 'STUDENT NAME:        '.
           05 FILLER PIC X(9).
       01  PROG-FACULTY-ID.
           05 FILLER PIC X(20) VALUE "FACULTY ID :".
           05 PROG-ID PIC X(12).
           05 FILLER PIC X(5).
       01  PHIL-FACULTY-ID.
           05 FILLER PIC X(20) VALUE "FACULTY ID :".
           05 PHIL-ID PIC X(12).
           05 FILLER PIC X(5).
       01  BIO-FACULTY-ID.
           05 FILLER PIC X(20) VALUE "FACULTY ID :".
           05 BIO-ID PIC X(12).
           05 FILLER PIC X(5).
       01  PHY-FACULTY-ID.
           05 FILLER PIC X(20) VALUE "FACULTY ID :".
           05 PHY-ID PIC X(12).
           05 FILLER PIC X(5).
       01  DATA-FACULTY-ID.
           05 FILLER PIC X(20) VALUE "FACULTY ID :".
           05 DATA-ID PIC X(12).
           05 FILLER PIC X(5).
       01  PROGRA-FACULTY-ID.
           05 FILLER PIC X(20) VALUE "FACULTY ID :".
           05 PROGRA-ID PIC X(12).
           05 FILLER PIC X(5).
       01  DISC-FACULTY-ID.
           05 FILLER PIC X(20) VALUE "FACULTY ID :".
           05 DISC-ID PIC X(12).
           05 FILLER PIC X(5).
       01  PROG-FACULTY-NAME.
           05 FILLER PIC X(20) VALUE "FACULTY NAME :".
           05 PROG-FCLTY-NA PIC X(25).
           05 FILLER PIC X(5).
       01  PHIL-FACULTY-NAME.
           05 FILLER PIC X(20) VALUE "FACULTY NAME :".
           05 PHIL-FCLTY-NA PIC X(25).
           05 FILLER PIC X(5).
       01  BIO-FACULTY-NAME.
           05 FILLER PIC X(20) VALUE "FACULTY NAME :".
           05 BIO-FCLTY-NA PIC X(25).
           05 FILLER PIC X(5).
       01  PHY-FACULTY-NAME.
           05 FILLER PIC X(20) VALUE "FACULTY NAME :".
           05 PHY-FCLTY-NA PIC X(25).
           05 FILLER PIC X(5).
       01  DATA-FACULTY-NAME.
           05 FILLER PIC X(20) VALUE "FACULTY NAME :".
           05 DATA-FCLTY-NA PIC X(25).
           05 FILLER PIC X(5).
       01  PROGRA-FACULTY-NAME.
           05 FILLER PIC X(20) VALUE "FACULTY NAME :".
           05 PROGRA-FCLTY-NA PIC X(25).
           05 FILLER PIC X(5).
       01  DISC-FACULTY-NAME.
           05 FILLER PIC X(20) VALUE "FACULTY NAME :".
           05 DISC-FCLTY-NA PIC X(25).
           05 FILLER PIC X(5).
       01  GRD-HEADER.
           05 FILLER PIC X(15) VALUE 'STUDENT NUMBER'.
           05 FILLER PIC X(9).
           05 FILLER PIC X(15) VALUE 'STUDENT NAME'.
           05 FILLER PIC X(9).
           05 FILLER PIC X(15) VALUE 'AVERAGE GRADE'.
           05 FILLER PIC X(9).
           05 FILLER PIC X(15) VALUE 'REMARKS'.
       01  PROG-TOTAL-PASS.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO PASSED:'.
           05 PROG-T-PASS PIC Z9.
       01  PROG-TOTAL-FAIL.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO FAILED:'.
           05 PROG-T-FAIL PIC Z9.
       01  PHIL-TOTAL-PASS.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO PASSED:'.
           05 PHIL-T-PASS PIC Z9.
       01  PHIL-TOTAL-FAIL.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO FAILED:'.
           05 PHIL-T-FAIL PIC Z9.
       01  BIO-TOTAL-PASS.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO PASSED:'.
           05 BIO-T-PASS PIC Z9.
       01  BIO-TOTAL-FAIL.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO FAILED:'.
           05 BIO-T-FAIL PIC Z9.
       01  PHY-TOTAL-PASS.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO PASSED:'.
           05 PHY-T-PASS PIC Z9.
       01  PHY-TOTAL-FAIL.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO FAILED:'.
           05 PHY-T-FAIL PIC Z9.
       01  DATA-TOTAL-PASS.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO PASSED:'.
           05 DATA-T-PASS PIC Z9.
       01  DATA-TOTAL-FAIL.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO FAILED:'.
           05 DATA-T-FAIL PIC Z9.
       01  PROGRA-TOTAL-PASS.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO PASSED:'.
           05 PROGRA-T-PASS PIC Z9.
       01  PROGRA-TOTAL-FAIL.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO FAILED:'.
           05 PROGRA-T-FAIL PIC Z9.
       01  DISC-TOTAL-PASS.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO PASSED:'.
           05 DISC-T-PASS PIC Z9.
       01  DISC-TOTAL-FAIL.
           05 FILLER PIC X(25) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(15) VALUE 'WHO FAILED:'.
           05 DISC-T-FAIL PIC Z9.
       01  GRAND-TOTAL.
           05 FILLER PIC X(31) VALUE 'GRAND TOTAL NUMBER OF STUDENTS '.
           05 FILLER PIC X(11) VALUE 'ENROLLED:  '.
           05 FILLER PIC X(5).
           05 GRAND-TOT PIC Z999.
           05 FILLER PIC X(30).
       01  BLANK-HEADER.
           05 FILLER PIC X(80).
       01  EXTENDED-INFO.
           05 EOF1 PIC X(3) VALUE 'NO'.
           05 EOF2 PIC X(3) VALUE 'NO'.
           05 EOF3 PIC X(3) VALUE 'NO'.
           05 EOF4 PIC X(3) VALUE 'NO'.
           05 EOF5 PIC X(3) VALUE 'NO'.
           05 EOF6 PIC X(3) VALUE 'NO'.
           05 EOF7 PIC X(3) VALUE 'NO'.
           05 EOF8 PIC X(3) VALUE 'NO'.
           05 EOF9 PIC X(3) VALUE 'NO'.
           05 EOF10 PIC X(3) VALUE 'NO'.
           05 EOF11 PIC X(3) VALUE 'NO'.
           05 EOF12 PIC X(3) VALUE 'NO'.
           05 EOF13 PIC X(3) VALUE 'NO'.
           05 EOF14 PIC X(3) VALUE 'NO'.
           05 TOTAL-PROG PIC 999 VALUE 0.
           05 TOTAL-PHIL PIC 999 VALUE 0.
           05 TOTAL-BIO PIC 999 VALUE 0.
           05 TOTAL-PHY PIC 999 VALUE 0.
           05 TOTAL-DATA PIC 999 VALUE 0.
           05 TOTAL-PROGRA PIC 999 VALUE 0.
           05 TOTAL-DISC PIC 999 VALUE 0.
           05 TOTAL-PROG-PASS PIC 99 VALUE 0.
           05 TOTAL-PROG-FAIL PIC 99 VALUE 0.
           05 TOTAL-PHIL-PASS PIC 99 VALUE 0.
           05 TOTAL-PHIL-FAIL PIC 99 VALUE 0.
           05 TOTAL-BIO-PASS PIC 99 VALUE 0.
           05 TOTAL-BIO-FAIL PIC 99 VALUE 0.
           05 TOTAL-PHY-PASS PIC 99 VALUE 0.
           05 TOTAL-PHY-FAIL PIC 99 VALUE 0.
           05 TOTAL-DATA-PASS PIC 99 VALUE 0.
           05 TOTAL-DATA-FAIL PIC 99 VALUE 0.
           05 TOTAL-PROGRA-PASS PIC 99 VALUE 0.
           05 TOTAL-PROGRA-FAIL PIC 99 VALUE 0.
           05 TOTAL-DISC-PASS PIC 99 VALUE 0.
           05 TOTAL-DISC-FAIL PIC 99 VALUE 0.
           05 PROG-AVERAGE PIC 9V99 VALUE 0.
           05 PHIL-AVERAGE PIC 9V99 VALUE 0.
           05 BIO-AVERAGE PIC 9V99 VALUE 0.
           05 PHY-AVERAGE PIC 9V99 VALUE 0.
           05 DATA-AVERAGE PIC 9V99 VALUE 0.
           05 PROGRA-AVERAGE PIC 9V99 VALUE 0.
           05 DISC-AVERAGE PIC 9V99 VALUE 0.
           05 GRAND-ENR PIC 9999 VALUE 0.
       SCREEN SECTION.
       01  SCRN.
           05 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN INPUT STUD1
                INPUT STUD2
                OUTPUT STUD5.
           DISPLAY SCRN.
           WRITE MERGING FROM ACADEMY.
           WRITE MERGING FROM ADDRESS-HDR.
           WRITE MERGING FROM BLANK-HEADER.
           WRITE MERGING FROM BLANK-HEADER.
           WRITE MERGING FROM HEADER1.
           WRITE MERGING FROM BLANK-HEADER.
           WRITE MERGING FROM BLANK-HEADER.
           READ STUD1 AT END MOVE 'YES' TO EOF1.
           READ STUD2 AT END MOVE 'YES' TO EOF1.
           MOVE 'IT 2001' TO PROG-CODE.
           MOVE 'PROGRAMMING 1' TO PROG-DES.
           WRITE MERGING FROM PROG-HDR.
           WRITE MERGING FROM SUBJECT-PROG.
           WRITE MERGING FROM STUDENTINFO.
           PERFORM PROG-COMPUTE UNTIL EOF1 IS EQUAL TO 'YES'.
           WRITE MERGING FROM TOT-PROG-ENR.
           WRITE MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           OPEN INPUT STUD1
                INPUT STUD2.
           READ STUD1 AT END MOVE 'YES' TO EOF2.
           READ STUD2 AT END MOVE 'YES' TO EOF2.
           MOVE 'HIST 1000' TO PHIL-CODE.
           MOVE 'PHIL. HISTORY' TO PHIL-DES.
           WRITE MERGING FROM PHIL-HDR.
           WRITE MERGING FROM SUBJECT-PHIL.
           WRITE MERGING FROM STUDENTINFO.
           PERFORM PHIL-COMPUTE UNTIL EOF2 IS EQUAL TO 'YES'.
           WRITE MERGING FROM TOT-PHIL-ENR.
           WRITE MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           OPEN INPUT STUD1
                INPUT STUD2.
           READ STUD1 AT END MOVE 'YES' TO EOF3.
           READ STUD2 AT END MOVE 'YES' TO EOF3.
           MOVE 'NATSCI 100' TO BIO-CODE.
           MOVE 'BIOLOGY' TO BIO-DES.
           WRITE MERGING FROM BIO-HDR.
           WRITE MERGING FROM SUBJECT-BIO.
           WRITE MERGING FROM STUDENTINFO.
           PERFORM BIO-COMPUTE UNTIL EOF3 IS EQUAL TO 'YES'.
           WRITE MERGING FROM TOT-BIO-ENR.
           WRITE MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           OPEN INPUT STUD1
                INPUT STUD2.
           READ STUD1 AT END MOVE 'YES' TO EOF4.
           READ STUD2 AT END MOVE 'YES' TO EOF4.
           MOVE 'NATSCI 105' TO PHY-CODE.
           MOVE 'PHYSICS' TO PHY-DES.
           WRITE MERGING FROM PHY-HDR.
           WRITE MERGING FROM SUBJECT-PHY.
           WRITE MERGING FROM STUDENTINFO.
           PERFORM PHY-COMPUTE UNTIL EOF4 IS EQUAL TO 'YES'.
           WRITE MERGING FROM TOT-PHY-ENR.
           WRITE MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           OPEN INPUT STUD1
                INPUT STUD2.
           READ STUD1 AT END MOVE 'YES' TO EOF5.
           READ STUD2 AT END MOVE 'YES' TO EOF5.
           MOVE 'COMP 2000' TO DATA-CODE.
           MOVE 'DATABASE MANAGEMENT SYSTEMS' TO DATA-DES.
           WRITE MERGING FROM DATA-HDR.
           WRITE MERGING FROM SUBJECT-DATA.
           WRITE MERGING FROM STUDENTINFO.
           PERFORM DATA-COMPUTE UNTIL EOF5 IS EQUAL TO 'YES'.
           WRITE MERGING FROM TOT-DATA-ENR.
           WRITE MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           OPEN INPUT STUD1
                INPUT STUD2.
           READ STUD1 AT END MOVE 'YES' TO EOF6.
           READ STUD2 AT END MOVE 'YES' TO EOF6.
           MOVE 'IT 2003' TO PROGRA-CODE.
           MOVE 'PROGRAMMING 3' TO PROGRA-DES.
           WRITE MERGING FROM PROGRA-HDR.
           WRITE MERGING FROM SUBJECT-PROGRA.
           WRITE MERGING FROM STUDENTINFO.
           PERFORM PROGRA-COMPUTE UNTIL EOF6 IS EQUAL TO 'YES'.
           WRITE MERGING FROM TOT-PROGRA-ENR.
           WRITE MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           OPEN INPUT STUD1
                INPUT STUD2.
           READ STUD1 AT END MOVE 'YES' TO EOF7.
           READ STUD2 AT END MOVE 'YES' TO EOF7.
           MOVE 'MATH 1000' TO DISC-CODE.
           MOVE 'DISCRETE STRUCTURE' TO DISC-DES.
           WRITE MERGING FROM DISC-HDR.
           WRITE MERGING FROM SUBJECT-DISC.
           WRITE MERGING FROM STUDENTINFO.
           PERFORM DISC-COMPUTE UNTIL EOF7 IS EQUAL TO 'YES'.
           WRITE MERGING FROM TOT-DISC-ENR.
           WRITE MERGING FROM BLANK-HEADER.
           PERFORM FINAL-RTN.
           CLOSE STUD1.
           CLOSE STUD2.
           PERFORM PROG-GRADE-RTN.
           PERFORM PHIL-GRADE-RTN.
           PERFORM BIO-GRADE-RTN.
           PERFORM PHY-GRADE-RTN.
           PERFORM DATA-GRADE-RTN.
           PERFORM PROGRA-GRADE-RTN.
           PERFORM DISC-GRADE-RTN.
           DISPLAY 'END OF MERGING!!!!!'.
           CLOSE STUD1, STUD2, STUD3, STUD4, STUD5, STUD6.
           STOP RUN.

       PROG-GRADE-RTN.
           OPEN INPUT STUD1
               INPUT STUD2
               INPUT STUD3
               INPUT STUD4
               OUTPUT STUD6.
           WRITE GR-MERGING FROM ACADEMY.
           WRITE GR-MERGING FROM ADDRESS-HDR.
           WRITE GR-MERGING FROM BLANK-HEADER.
           WRITE GR-MERGING FROM BLANK-HEADER.
           WRITE GR-MERGING FROM HEADER1.
           WRITE GR-MERGING FROM BLANK-HEADER.
           WRITE GR-MERGING FROM BLANK-HEADER.
           READ STUD1 AT END MOVE 'YES' TO EOF8.
           READ STUD2 AT END MOVE 'YES' TO EOF8.
           READ STUD3 AT END MOVE 'YES' TO EOF8.
           READ STUD4 AT END MOVE 'YES' TO EOF8.
           MOVE 'IT 2001' TO PROG-CODE.
           MOVE FAC-ID TO PROG-ID.
           MOVE FAC-NAME TO PROG-FCLTY-NA.
           WRITE GR-MERGING FROM PROG-HDR.
           WRITE GR-MERGING FROM PROG-FACULTY-ID.
           WRITE GR-MERGING FROM PROG-FACULTY-NAME.
           WRITE GR-MERGING FROM GRD-HEADER.
           PERFORM PROG-GRADE-COMP UNTIL EOF8 IS EQUAL TO 'YES'.
           WRITE GR-MERGING FROM PROG-TOTAL-PASS.
           WRITE GR-MERGING FROM PROG-TOTAL-FAIL.
           WRITE GR-MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           CLOSE STUD3.
           CLOSE STUD4.

       PHIL-GRADE-RTN.
           OPEN INPUT STUD1
               INPUT STUD2
               INPUT STUD3
               INPUT STUD4.
           READ STUD1 AT END MOVE 'YES' TO EOF9.
           READ STUD2 AT END MOVE 'YES' TO EOF9.
           READ STUD3 AT END MOVE 'YES' TO EOF9.
           READ STUD4 AT END MOVE 'YES' TO EOF9.
           MOVE 'HIST 1000' TO PHIL-CODE.
           MOVE FAC-ID TO PHIL-ID.
           MOVE FAC-NAME TO PHIL-FCLTY-NA.
           WRITE GR-MERGING FROM PHIL-HDR.
           WRITE GR-MERGING FROM PHIL-FACULTY-ID.
           WRITE GR-MERGING FROM PHIL-FACULTY-NAME.
           WRITE GR-MERGING FROM GRD-HEADER.
           PERFORM PHIL-GRADE-COMP UNTIL EOF9 IS EQUAL TO 'YES'.
           WRITE GR-MERGING FROM PHIL-TOTAL-PASS.
           WRITE GR-MERGING FROM PHIL-TOTAL-FAIL.
           WRITE GR-MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           CLOSE STUD3.
           CLOSE STUD4.

       BIO-GRADE-RTN.
           OPEN INPUT STUD1
               INPUT STUD2
               INPUT STUD3
               INPUT STUD4.
           READ STUD1 AT END MOVE 'YES' TO EOF10.
           READ STUD2 AT END MOVE 'YES' TO EOF10.
           READ STUD3 AT END MOVE 'YES' TO EOF10.
           READ STUD4 AT END MOVE 'YES' TO EOF10.
           MOVE 'NATSCI 100' TO BIO-CODE.
           MOVE FAC-ID TO BIO-ID.
           MOVE FAC-NAME TO BIO-FCLTY-NA.
           WRITE GR-MERGING FROM BIO-HDR.
           WRITE GR-MERGING FROM BIO-FACULTY-ID.
           WRITE GR-MERGING FROM BIO-FACULTY-NAME.
           WRITE GR-MERGING FROM GRD-HEADER.
           PERFORM BIO-GRADE-COMP UNTIL EOF10 IS EQUAL TO 'YES'.
           WRITE GR-MERGING FROM BIO-TOTAL-PASS.
           WRITE GR-MERGING FROM BIO-TOTAL-FAIL.
           WRITE GR-MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           CLOSE STUD3.
           CLOSE STUD4.

       PHY-GRADE-RTN.
           OPEN INPUT STUD1
               INPUT STUD2
               INPUT STUD3
               INPUT STUD4.
           READ STUD1 AT END MOVE 'YES' TO EOF11.
           READ STUD2 AT END MOVE 'YES' TO EOF11.
           READ STUD3 AT END MOVE 'YES' TO EOF11.
           READ STUD4 AT END MOVE 'YES' TO EOF11.
           MOVE 'NATSCI 105' TO PHY-CODE.
           MOVE FAC-ID TO PHY-ID.
           MOVE FAC-NAME TO PHY-FCLTY-NA.
           WRITE GR-MERGING FROM PHY-HDR.
           WRITE GR-MERGING FROM PHY-FACULTY-ID.
           WRITE GR-MERGING FROM PHY-FACULTY-NAME.
           WRITE GR-MERGING FROM GRD-HEADER.
           PERFORM PHY-GRADE-COMP UNTIL EOF11 IS EQUAL TO 'YES'.
           WRITE GR-MERGING FROM PHY-TOTAL-PASS.
           WRITE GR-MERGING FROM PHY-TOTAL-FAIL.
           WRITE GR-MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           CLOSE STUD3.
           CLOSE STUD4.

       DATA-GRADE-RTN.
           OPEN INPUT STUD1
               INPUT STUD2
               INPUT STUD3
               INPUT STUD4.
           READ STUD1 AT END MOVE 'YES' TO EOF12.
           READ STUD2 AT END MOVE 'YES' TO EOF12.
           READ STUD3 AT END MOVE 'YES' TO EOF12.
           READ STUD4 AT END MOVE 'YES' TO EOF12.
           MOVE 'COMP 2000' TO DATA-CODE.
           MOVE FAC-ID TO DATA-ID.
           MOVE FAC-NAME TO DATA-FCLTY-NA.
           WRITE GR-MERGING FROM DATA-HDR.
           WRITE GR-MERGING FROM DATA-FACULTY-ID.
           WRITE GR-MERGING FROM DATA-FACULTY-NAME.
           WRITE GR-MERGING FROM GRD-HEADER.
           PERFORM DATA-GRADE-COMP UNTIL EOF12 IS EQUAL TO 'YES'.
           WRITE GR-MERGING FROM DATA-TOTAL-PASS.
           WRITE GR-MERGING FROM DATA-TOTAL-FAIL.
           WRITE GR-MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           CLOSE STUD3.
           CLOSE STUD4.

       PROGRA-GRADE-RTN.
           OPEN INPUT STUD1
               INPUT STUD2
               INPUT STUD3
               INPUT STUD4.
           READ STUD1 AT END MOVE 'YES' TO EOF13.
           READ STUD2 AT END MOVE 'YES' TO EOF13.
           READ STUD3 AT END MOVE 'YES' TO EOF13.
           READ STUD4 AT END MOVE 'YES' TO EOF13.
           MOVE 'IT 2003' TO PROGRA-CODE.
           MOVE FAC-ID TO PROGRA-ID.
           MOVE FAC-NAME TO PROGRA-FCLTY-NA.
           WRITE GR-MERGING FROM PROGRA-HDR.
           WRITE GR-MERGING FROM PROGRA-FACULTY-ID.
           WRITE GR-MERGING FROM PROGRA-FACULTY-NAME.
           WRITE GR-MERGING FROM GRD-HEADER.
           PERFORM PROGRA-GRADE-COMP UNTIL EOF13 IS EQUAL TO 'YES'.
           WRITE GR-MERGING FROM PROGRA-TOTAL-PASS.
           WRITE GR-MERGING FROM PROGRA-TOTAL-FAIL.
           WRITE GR-MERGING FROM BLANK-HEADER.
           CLOSE STUD1.
           CLOSE STUD2.
           CLOSE STUD3.
           CLOSE STUD4.

       DISC-GRADE-RTN.
           OPEN INPUT STUD1
               INPUT STUD2
               INPUT STUD3
               INPUT STUD4.
           READ STUD1 AT END MOVE 'YES' TO EOF14.
           READ STUD2 AT END MOVE 'YES' TO EOF14.
           READ STUD3 AT END MOVE 'YES' TO EOF14.
           READ STUD4 AT END MOVE 'YES' TO EOF14.
           MOVE 'MATH 1000' TO DISC-CODE.
           MOVE FAC-ID TO DISC-ID.
           MOVE FAC-NAME TO DISC-FCLTY-NA.
           WRITE GR-MERGING FROM DATA-HDR.
           WRITE GR-MERGING FROM DISC-FACULTY-ID.
           WRITE GR-MERGING FROM DISC-FACULTY-NAME.
           WRITE GR-MERGING FROM GRD-HEADER.
           PERFORM PHIL-GRADE-COMP UNTIL EOF14 IS EQUAL TO 'YES'.
           WRITE GR-MERGING FROM DISC-TOTAL-PASS.
           WRITE GR-MERGING FROM DISC-TOTAL-FAIL.
           WRITE GR-MERGING FROM BLANK-HEADER.

       PROG-GRADE-COMP.
           IF SUBJECT-CODE = 'IT 2001'
               MOVE STUD-NUM TO PROG-NO
               MOVE STUD-NAME TO PROG-NA
               COMPUTE PROG-AVERAGE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE PROG-AVERAGE TO PROG-AVG
               IF PROG-AVERAGE <= 3.12
                   MOVE 'PASSED' TO PROG-RMRKS
                   ADD 1 TO TOTAL-PROG-PASS
                   MOVE TOTAL-PROG-PASS TO PROG-T-PASS
               ELSE
                   MOVE 'FAILED' TO PROG-RMRKS
                   ADD 1 TO TOTAL-PROG-FAIL
                   MOVE TOTAL-PROG-FAIL TO PROG-T-FAIL
           WRITE GR-MERGING FROM PROG-GRADE-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF8.
           READ STUD2 AT END MOVE 'YES' TO EOF8.
           READ STUD3 AT END MOVE 'YES' TO EOF8.
           READ STUD4 AT END MOVE 'YES' TO EOF8.

       PHIL-GRADE-COMP.
           IF SUBJECT-CODE = 'HIST 1000'
               MOVE STUD-NUM TO PHIL-NO
               MOVE STUD-NAME TO PHIL-NA
               COMPUTE PHIL-AVERAGE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE PHIL-AVERAGE TO PHIL-AVG
               IF PHIL-AVERAGE <= 3.12
                   MOVE 'PASSED' TO PHIL-RMRKS
                   ADD 1 TO TOTAL-PHIL-PASS
                   MOVE TOTAL-PHIL-PASS TO PHIL-T-PASS
               ELSE
                   MOVE 'FAILED' TO PHIL-RMRKS
                   ADD 1 TO TOTAL-PHIL-FAIL
                   MOVE TOTAL-PHIL-FAIL TO PHIL-T-FAIL
           WRITE GR-MERGING FROM PHIL-GRADE-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF9.
           READ STUD2 AT END MOVE 'YES' TO EOF9.
           READ STUD3 AT END MOVE 'YES' TO EOF9.
           READ STUD4 AT END MOVE 'YES' TO EOF9.

       BIO-GRADE-COMP.
           IF SUBJECT-CODE = 'NATSCI 100'
               MOVE STUD-NUM TO BIO-NO
               MOVE STUD-NAME TO BIO-NA
               COMPUTE BIO-AVERAGE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE BIO-AVERAGE TO BIO-AVG
               IF BIO-AVERAGE <= 3.12
                   MOVE 'PASSED' TO BIO-RMRKS
                   ADD 1 TO TOTAL-BIO-PASS
                   MOVE TOTAL-BIO-PASS TO BIO-T-PASS
               ELSE
                   MOVE 'FAILED' TO BIO-RMRKS
                   ADD 1 TO TOTAL-BIO-FAIL
                   MOVE TOTAL-BIO-FAIL TO BIO-T-FAIL
           WRITE GR-MERGING FROM BIO-GRADE-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF10.
           READ STUD2 AT END MOVE 'YES' TO EOF10.
           READ STUD3 AT END MOVE 'YES' TO EOF10.
           READ STUD4 AT END MOVE 'YES' TO EOF10.

       PHY-GRADE-COMP.
           IF SUBJECT-CODE = 'NATSCI 105'
               MOVE STUD-NUM TO PHYNO
               MOVE STUD-NAME TO PHYNA
               COMPUTE PHY-AVERAGE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE PHY-AVERAGE TO PHY-AVG
               IF PHY-AVERAGE <= 3.12
                   MOVE 'PASSED' TO PHY-RMRKS
                   ADD 1 TO TOTAL-PHY-PASS
                   MOVE TOTAL-PHY-PASS TO PHY-T-PASS
               ELSE
                   MOVE 'FAILED' TO PHY-RMRKS
                   ADD 1 TO TOTAL-PHY-FAIL
                   MOVE TOTAL-PHY-FAIL TO PHY-T-FAIL
           WRITE GR-MERGING FROM PHY-GRADE-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF11.
           READ STUD2 AT END MOVE 'YES' TO EOF11.
           READ STUD3 AT END MOVE 'YES' TO EOF11.
           READ STUD4 AT END MOVE 'YES' TO EOF11.

       DATA-GRADE-COMP.
           IF SUBJECT-CODE = 'COMP 2000'
               MOVE STUD-NUM TO DATA-NO
               MOVE STUD-NAME TO DATA-NA
               COMPUTE DATA-AVERAGE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE DATA-AVERAGE TO DATA-AVG
               IF DATA-AVERAGE <= 3.12
                   MOVE 'PASSED' TO DATA-RMRKS
                   ADD 1 TO TOTAL-DATA-PASS
                   MOVE TOTAL-DATA-PASS TO DATA-T-PASS
               ELSE
                   MOVE 'FAILED' TO DATA-RMRKS
                   ADD 1 TO TOTAL-DATA-FAIL
                   MOVE TOTAL-DATA-FAIL TO DATA-T-FAIL
           WRITE GR-MERGING FROM DATA-GRADE-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF12.
           READ STUD2 AT END MOVE 'YES' TO EOF12.
           READ STUD3 AT END MOVE 'YES' TO EOF12.
           READ STUD4 AT END MOVE 'YES' TO EOF12.


       PROGRA-GRADE-COMP.
           IF SUBJECT-CODE = 'IT 2003'
               MOVE STUD-NUM TO PROGRA-NO
               MOVE STUD-NAME TO PROGRA-NA
               COMPUTE PROGRA-AVERAGE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE PROGRA-AVERAGE TO PROGRA-AVG
               IF PROGRA-AVERAGE <= 3.12
                   MOVE 'PASSED' TO PROGRA-RMRKS
                   ADD 1 TO TOTAL-PROGRA-PASS
                   MOVE TOTAL-PROGRA-PASS TO PROGRA-T-PASS
               ELSE
                   MOVE 'FAILED' TO PROGRA-RMRKS
                   ADD 1 TO TOTAL-PROGRA-FAIL
                   MOVE TOTAL-PROGRA-FAIL TO PROGRA-T-FAIL
           WRITE GR-MERGING FROM PROGRA-GRADE-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF13.
           READ STUD2 AT END MOVE 'YES' TO EOF13.
           READ STUD3 AT END MOVE 'YES' TO EOF13.
           READ STUD4 AT END MOVE 'YES' TO EOF13.

       DISC-GRADE-COMP.
           IF SUBJECT-CODE = 'MATH 1000'
               MOVE STUD-NUM TO DISC-NO
               MOVE STUD-NAME TO DISC-NA
               COMPUTE DISC-AVERAGE = (MID-GRADE + FINAL-GRADE) / 2
               MOVE DISC-AVERAGE TO DISC-AVG
               IF DISC-AVERAGE <= 3.12
                   MOVE 'PASSED' TO DISC-RMRKS
                   ADD 1 TO TOTAL-DISC-PASS
                   MOVE TOTAL-DISC-PASS TO DISC-T-PASS
               ELSE
                   MOVE 'FAILED' TO DISC-RMRKS
                   ADD 1 TO TOTAL-DISC-FAIL
                   MOVE TOTAL-DISC-FAIL TO DISC-T-FAIL
                   WRITE GR-MERGING FROM DISC-GRADE-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF14.
           READ STUD2 AT END MOVE 'YES' TO EOF14.
           READ STUD3 AT END MOVE 'YES' TO EOF14.
           READ STUD4 AT END MOVE 'YES' TO EOF14.


       PROG-COMPUTE.
           IF SUBJECT-CODE = 'IT 2001'
               IF SUBJ-DES = 'PROGRAMMING 1'
                   MOVE STUD-NUM TO PROGNO
                   MOVE STUD-NAME TO PROGNA
                   ADD 1 TO TOTAL-PROG
                   ADD 1 TO GRAND-ENR
                   MOVE TOTAL-PROG TO TOT-PROG
                   MOVE GRAND-ENR TO GRAND-TOT
                   WRITE MERGING FROM PROG-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF1.
           READ STUD2 AT END MOVE 'YES' TO EOF1.

       PHIL-COMPUTE.
           IF SUBJECT-CODE = 'HIST 1000'
               IF SUBJ-DES = 'PHIL. HISTORY'
                   MOVE STUD-NUM TO PHILNO
                   MOVE STUD-NAME TO PHILNA
                   ADD 1 TO TOTAL-PHIL
                   ADD 1 TO GRAND-ENR
                   MOVE TOTAL-PHIL TO TOT-PHIL
                   MOVE GRAND-ENR TO GRAND-TOT
                   WRITE MERGING FROM PHIL-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF2.
           READ STUD2 AT END MOVE 'YES' TO EOF2.

       BIO-COMPUTE.
           IF SUBJECT-CODE = 'NATSCI 100'
               IF SUBJ-DES = 'BIOLOGY'
                   MOVE STUD-NUM TO BIONO
                   MOVE STUD-NAME TO BIONA
                   ADD 1 TO TOTAL-BIO
                   ADD 1 TO GRAND-ENR
                   MOVE TOTAL-BIO TO TOT-BIO
                   MOVE GRAND-ENR TO GRAND-TOT
                   WRITE MERGING FROM BIO-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF3.
           READ STUD2 AT END MOVE 'YES' TO EOF3.

       PHY-COMPUTE.
           IF SUBJECT-CODE = 'NATSCI 105'
               IF SUBJ-DES = 'PHYSICS'
                   MOVE STUD-NUM TO PHYNO
                   MOVE STUD-NAME TO PHYNA
                   ADD 1 TO TOTAL-PHY
                   ADD 1 TO GRAND-ENR
                   MOVE TOTAL-PHY TO TOT-PHY
                   MOVE GRAND-ENR TO GRAND-TOT
                   WRITE MERGING FROM PHY-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF4.
           READ STUD2 AT END MOVE 'YES' TO EOF4.

       DATA-COMPUTE.
           IF SUBJECT-CODE = 'COMP 2000'
               IF SUBJ-DES = 'DATABASE MANAGEMENT SYSTEMS'
                   MOVE STUD-NUM TO DATANO
                   MOVE STUD-NAME TO DATANA
                   ADD 1 TO TOTAL-DATA
                   ADD 1 TO GRAND-ENR
                   MOVE TOTAL-DATA TO TOT-DATA
                   MOVE GRAND-ENR TO GRAND-TOT
                   WRITE MERGING FROM DATA-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF5.
           READ STUD2 AT END MOVE 'YES' TO EOF5.

       PROGRA-COMPUTE.
           IF SUBJECT-CODE = 'IT 2003'
               IF SUBJ-DES = 'PROGRAMMING 3'
                   MOVE STUD-NUM TO PROGRANO
                   MOVE STUD-NAME TO PROGRANA
                   ADD 1 TO TOTAL-PROGRA
                   ADD 1 TO GRAND-ENR
                   MOVE TOTAL-PROGRA TO TOT-PROGRA
                   MOVE GRAND-ENR TO GRAND-TOT
                   WRITE MERGING FROM PROGRA-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF6.
           READ STUD2 AT END MOVE 'YES' TO EOF6.

       DISC-COMPUTE.
           IF SUBJECT-CODE = 'MATH 1000'
               IF SUBJ-DES = 'DISCRETE STRUCTURE'
                   MOVE STUD-NUM TO DISCNO
                   MOVE STUD-NAME TO DISCNA
                   ADD 1 TO TOTAL-DISC
                   ADD 1 TO GRAND-ENR
                   MOVE TOTAL-DISC TO TOT-DISC
                   MOVE GRAND-ENR TO GRAND-TOT
                   WRITE MERGING FROM DISC-OUT
           END-IF.
           READ STUD1 AT END MOVE 'YES' TO EOF7.
           READ STUD2 AT END MOVE 'YES' TO EOF7.
       FINAL-RTN.
           WRITE MERGING FROM BLANK-HEADER.
           WRITE MERGING FROM BLANK-HEADER.
           WRITE MERGING FROM GRAND-TOTAL.