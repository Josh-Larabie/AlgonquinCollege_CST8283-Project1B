       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT-1-B.
       AUTHOR. Josh Larabie, Design by Mel Sanschagrin.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLE-FILE
               ASSIGN TO "C:\PAYROLE-FILE.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
       DATA DIVISION.
       FILE SECTION.
       FD PAYROLE-FILE.
       01  PAYRECORD-OUT   PIC X(33).
       
       WORKING-STORAGE SECTION.
       01  PAYRECORD-IN.
           05 EMPLOYEE-NUM         PIC 9(9).
           05 EMPLOYEE-LNAME       PIC X(13).
           05 EMPLOYEE-INITIALS    PIC X(2).
           05 EMPLOYEE-HOURLY-PAY  PIC 9(4).
           05 HOURS-WORKED         PIC 9(4).
           05 UNION-MEMBER         PIC X(1).
           
       01  FLAGS-AND-CONTROLS.
           05  USER-RESPONSE       PIC X(1)    VALUE "Y".
           
       01  COUNTERS.
           05  FILLER      PIC X(14) VALUE "RECORDS READ  ".
           05  RECORDS-IN  PIC 9(3).
           05  FILLER      PIC X(19)   VALUE "  RECORDS WRITTEN  ".
           05  RECORDS-OUT PIC 9(3).
      
      *Defines output variables for user input prompts.
       01  PROMPTS.
           05  RECORD-PROMPT       PIC X(25)   VALUE "RECORD TO ENTER Y  or N".
           05  EMP-NUM-PROMPT      PIC X(21)   VALUE "ENTER EMPLOYEE NUMBER".
           05  EMP-LNAME-PROMPT    PIC X(15)   VALUE "ENTER LAST NAME".
           05  EMP-INITIALS-PROMPT PIC X(14)   VALUE "ENTER INITIALS".
           05  EMP-HRPAY-PROMPT    PIC X(10)   VALUE "ENTER RATE".
           05  EMP-HRWORKED-PROMPT PIC X(11)   VALUE "ENTER HOURS".
           05  EMP-UNION-PROMPT    PIC X(16)   VALUE "ENTER UNION CODE".
           
       PROCEDURE DIVISION.
       
      *Main Loop, iterates control functionality until provided input of "N"/"n".
       100-CREATE-PAYROLE-FILE.
           PERFORM 200-INIT-CREATE-PAYROLE-FILE.
           PERFORM 200-CREATE-PAYROLE-RECORD
               UNTIL USER-RESPONSE = "N" OR "n".
           PERFORM 200-TERM-CREATE-PAYROLE-FILE.
           STOP RUN.

      *Opens File & prompts user to see if the user desires to create a record.
       200-INIT-CREATE-PAYROLE-FILE.
           PERFORM  700-OPEN-PAYROLE-FILE
           PERFORM  700-PROMPT-FOR-RECORD.
           PERFORM  700-INIT-READ-WRITE-CTRS.

      *Prompts user to enter data for the payrole record, writes record to file.
       200-CREATE-PAYROLE-RECORD.
           PERFORM  700-ENTER-PAYROLE-DATA.
           PERFORM  700-WRITE-PAYROLE-RECORD.
           PERFORM  700-PROMPT-FOR-RECORD.
      
      *Closes file & outputs record count.
       200-TERM-CREATE-PAYROLE-FILE.
           PERFORM  700-CLOSE-PAYROLE-FILE.
           PERFORM  700-DISPLAY-AUDIT-COUNTERS.

      *Opens the payrole file, in preparation for writing.
       700-OPEN-PAYROLE-FILE.
           OPEN OUTPUT PAYROLE-FILE.

      *Outputs the prompt to create a record. For use within the main control loop (100-CREATE-PAYROLE-FILE.).
       700-PROMPT-FOR-RECORD.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY RECORD-PROMPT       LINE 2   COLUMN 4.
           ACCEPT  USER-RESPONSE       LINE 2   COLUMN 40.
      
      *Initialization of Records
       700-INIT-READ-WRITE-CTRS.
           INITIALIZE  RECORDS-IN
                       RECORDS-OUT.

       700-ENTER-PAYROLE-DATA.

           DISPLAY EMP-NUM-PROMPT      LINE 4   COLUMN 4.
           ACCEPT  EMPLOYEE-NUM        LINE 5   COLUMN 4.
           
           DISPLAY EMP-LNAME-PROMPT    LINE 6  COLUMN 4.
           ACCEPT  EMPLOYEE-LNAME      LINE 7  COLUMN 4.
           
           DISPLAY EMP-INITIALS-PROMPT LINE 8  COLUMN 4.
           ACCEPT  EMPLOYEE-INITIALS   LINE 9  COLUMN 4.
           
           DISPLAY EMP-HRPAY-PROMPT    LINE 10  COLUMN 4.
           ACCEPT  EMPLOYEE-HOURLY-PAY LINE 11  COLUMN 4.
           
           DISPLAY EMP-HRWORKED-PROMPT LINE 12  COLUMN 4.
           ACCEPT  HOURS-WORKED        LINE 13  COLUMN 4.
           
           DISPLAY EMP-UNION-PROMPT    LINE 14  COLUMN 4.
           ACCEPT  UNION-MEMBER        LINE 14  COLUMN 22.
           
           ADD  1  TO RECORDS-IN.

       700-WRITE-PAYROLE-RECORD.
           MOVE   PAYRECORD-IN  TO  PAYRECORD-OUT.
           WRITE  PAYRECORD-OUT.
           ADD  1  TO RECORDS-OUT.
           INITIALIZE PAYRECORD-IN.

       700-CLOSE-PAYROLE-FILE.
           CLOSE PAYROLE-FILE.

       700-DISPLAY-AUDIT-COUNTERS.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY COUNTERS.
       