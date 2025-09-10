*> This is free-form
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.
ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT InputFile ASSIGN TO "/workspace/src/InCollege-Input.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT OutputFile ASSIGN TO "/workspace/src/Keep-Output.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT AccountsFile ASSIGN TO "/workspace/src/Accounts.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
       FILE SECTION.
           FD InputFile.
               01 User-Input PIC X(100).
           FD OutputFile.
               01 Output-Line PIC X(100).
           FD AccountsFile.
               01 Account-Record.
                   05 Account-Username PIC X(50).
                   05 Account-Password PIC X(50).

       WORKING-STORAGE SECTION.
           01 Message-Text PIC X(100).
           01 Account-Username-Input PIC X(50).
           01 Account-Password-Input PIC X(50).
           01 User-Choice PIC X(100).
           01 Username-Exists PIC X VALUE 'N'.
           01 Password-Valid PIC X VALUE 'N'.
           01 PW-Length PIC 9(2).
           01 IDX PIC 9(2).
           01 CHAR-ORD PIC 9(3).
           01 Upper-Flag PIC 9 VALUE 0.
           01 Digit-Flag PIC 9 VALUE 0.
           01 Special-Flag PIC 9 VALUE 0.
           01 Is-Logged-In PIC X VALUE 'N'.

PROCEDURE DIVISION.
       OPEN OUTPUT OutputFile
       CLOSE OutputFile
       OPEN INPUT InputFile

       PERFORM Show-Login-Menu

       READ InputFile INTO User-Input
           AT END
               MOVE "No input found." TO Message-Text
               PERFORM Write-And-Display
               CLOSE InputFile
               STOP RUN
       END-READ

       PERFORM Handle-Auth

       IF Is-Logged-In = 'N'
           CLOSE InputFile
           STOP RUN
       END-IF

       STRING "Welcome, " Account-Username INTO Message-Text
       PERFORM Write-And-Display

       PERFORM Show-Main-Menu

       CLOSE InputFile
       STOP RUN.

Write-And-Display SECTION.
       OPEN EXTEND OutputFile
       MOVE Message-Text TO Output-Line.
       DISPLAY Output-Line
       WRITE Output-Line
       CLOSE OutputFile
       EXIT.

Show-Login-Menu SECTION.
       MOVE "Welcome to InCollege!" TO Message-Text
       PERFORM Write-And-Display
       MOVE "1. Log In" TO Message-Text
       PERFORM Write-And-Display
       MOVE "2. Create New Account" TO Message-Text
       PERFORM Write-And-Display
       MOVE "Enter your choice (1 or 2): " TO Message-Text
       PERFORM Write-And-Display
       EXIT.

Show-Main-Menu SECTION.
       MOVE "1. Search for a job" TO Message-Text
       PERFORM Write-And-Display
       MOVE "2. Find someone you know" TO Message-Text
       PERFORM Write-And-Display
       MOVE "3. Learn a new skill" TO Message-Text
       PERFORM Write-And-Display
       MOVE "Enter your choice (1-3): " TO Message-Text
       PERFORM Write-And-Display

       READ InputFile INTO User-Input
           AT END
               MOVE "No input found." TO Message-Text
               PERFORM Write-And-Display
               CLOSE InputFile
               STOP RUN
       END-READ

       EVALUATE User-Input
           WHEN "1"
               MOVE "Job search/internship is under construction." TO Message-Text
               PERFORM Write-And-Display
               PERFORM Show-Main-Menu
           WHEN "2"
               MOVE "Find someone you know is under construction." TO Message-Text
               PERFORM Write-And-Display
               PERFORM Show-Main-Menu
           WHEN "3"
               PERFORM Learn-Skill-Menu
           WHEN OTHER
               MOVE "Invalid choice." TO Message-Text
               PERFORM Write-And-Display
       END-EVALUATE.
       EXIT.

Handle-Auth SECTION.
       EVALUATE User-Input
           WHEN "1"
               PERFORM Log-In-Workflow
           WHEN "2"
               PERFORM Create-Account-Workflow
           WHEN OTHER
               MOVE "Invalid choice." TO Message-Text
               PERFORM Write-And-Display
       END-EVALUATE.

Learn-Skill-Menu SECTION.
       MOVE "1. AWS" TO Message-Text
       PERFORM Write-And-Display
       MOVE "2. Docker" TO Message-Text
       PERFORM Write-And-Display
       MOVE "3. COBOL" TO Message-Text
       PERFORM Write-And-Display
       MOVE "4. Azure" TO Message-Text
       PERFORM Write-And-Display
       MOVE "5. GCP" TO Message-Text
       PERFORM Write-And-Display
       MOVE "6. Return to main menu" TO Message-Text
       PERFORM Write-And-Display
       MOVE "Enter your choice (1-6): " TO Message-Text
       PERFORM Write-And-Display

       READ InputFile INTO User-Input
           AT END
               MOVE "No skill input found." TO Message-Text
               PERFORM Write-And-Display
               EXIT SECTION
       END-READ

       EVALUATE User-Input
           WHEN "1"
               MOVE "AWS is under construction." TO Message-Text
               PERFORM Write-And-Display
           WHEN "2"
               MOVE "Docker is under construction." TO Message-Text
               PERFORM Write-And-Display
           WHEN "3"
               MOVE "COBOL is under construction." TO Message-Text
               PERFORM Write-And-Display
           WHEN "4"
               MOVE "Azure is under construction." TO Message-Text
               PERFORM Write-And-Display
           WHEN "5"
               MOVE "GCP is under construction." TO Message-Text
               PERFORM Write-And-Display
           WHEN "6"
               PERFORM Show-Main-Menu
               READ InputFile INTO User-Input
                   AT END
                       MOVE "No input found." TO Message-Text
                       PERFORM Write-And-Display
                       EXIT SECTION
               END-READ
               PERFORM Show-Main-Menu
           WHEN OTHER
               MOVE "Invalid skill choice." TO Message-Text
               PERFORM Write-And-Display
       END-EVALUATE.
       EXIT.


Ask-For-Login SECTION.
       MOVE "Please enter your username: " TO Message-Text
       PERFORM Write-And-Display
       READ InputFile INTO Account-Username-Input
           AT END
               MOVE "No username input found." TO Message-Text
               PERFORM Write-And-Display
               CLOSE InputFile
               STOP RUN
       END-READ

       MOVE "Please enter your password: " TO Message-Text
       PERFORM Write-And-Display
       READ InputFile INTO Account-Password-Input
           AT END
               MOVE "No password input found." TO Message-Text
               PERFORM Write-And-Display
               CLOSE InputFile
               STOP RUN
       END-READ
       EXIT.

Create-Account-Workflow SECTION.
       OPEN INPUT AccountsFile
       MOVE 0 TO IDX
       PERFORM UNTIL IDX >= 5
           READ AccountsFile
               AT END
                   EXIT PERFORM
               NOT AT END
                   ADD 1 TO IDX
           END-READ
       END-PERFORM
       CLOSE AccountsFile

       IF IDX >= 5
           MOVE "All permitted accounts have been created, please come back later." TO Message-Text
           PERFORM Write-And-Display
           EXIT SECTION
       END-IF

       MOVE 'N' TO Is-Logged-In
       PERFORM UNTIL Is-Logged-In = 'Y'
           PERFORM Ask-For-Login
           PERFORM Create-Account
       END-PERFORM
       EXIT.

Log-In-Workflow SECTION.
       MOVE 'N' TO Is-Logged-In
       PERFORM UNTIL Is-Logged-In = 'Y'
           PERFORM Ask-For-Login
           PERFORM Log-In
       END-PERFORM
       EXIT.

Log-In SECTION.
       OPEN INPUT AccountsFile
       MOVE 'N' TO Username-Exists
       PERFORM UNTIL Is-Logged-In = 'Y'
           READ AccountsFile
                AT END
                   MOVE "Invalid username or password." TO Message-Text
                   PERFORM Write-And-Display
                   CLOSE AccountsFile
                   EXIT PERFORM

               NOT AT END
                   IF Account-Username = Account-Username-Input
                       IF Account-Password = Account-Password-Input
                           MOVE 'Y' TO Is-Logged-In
                           MOVE "You have successfully logged in!" TO Message-Text
                           PERFORM Write-And-Display
                           CLOSE AccountsFile
                           EXIT PERFORM
                       END-IF
                   END-IF
           END-READ
       END-PERFORM.
       EXIT.

Create-Account SECTION.
       OPEN INPUT AccountsFile
       MOVE 'N' TO Username-Exists
       PERFORM UNTIL Username-Exists = 'Y'
           READ AccountsFile
               AT END
                   CLOSE AccountsFile
                   IF FUNCTION LENGTH(FUNCTION TRIM(Account-Username-Input)) > 0 AND FUNCTION LENGTH(FUNCTION TRIM(Account-Username-Input)) < 50
                       PERFORM Verify-Password
                       IF Password-Valid = 'Y'
                           PERFORM Write-Account
                           MOVE 'Y' TO Is-Logged-In
                           MOVE "Account created successfully!" TO Message-Text
                           PERFORM Write-And-Display
                           EXIT PERFORM
                       ELSE
                           EXIT SECTION
                       END-IF
                   ELSE
                       MOVE "Invalid username length. Try again." TO Message-Text
                       PERFORM Write-And-Display
                       EXIT SECTION
                   END-IF

               NOT AT END
                   IF Account-Username = Account-Username-Input
                        MOVE "Username already exists. Try again." TO Message-Text
                        PERFORM Write-And-Display
                        MOVE 'Y' TO Username-Exists
                        CLOSE AccountsFile
                   END-IF
           END-READ
       END-PERFORM.
       EXIT.

Verify-Password SECTION.
       MOVE FUNCTION LENGTH(FUNCTION TRIM(Account-Password-Input)) TO PW-Length
       MOVE 0 TO Upper-Flag Digit-Flag Special-Flag

       IF PW-Length < 8 OR PW-Length > 12
           MOVE "Password must be 8-12 characters." TO Message-Text
           PERFORM Write-And-Display
           MOVE 'N' TO Password-Valid
           EXIT SECTION
       END-IF

       *> check for character types
       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > PW-Length
           MOVE FUNCTION ORD(Account-Password-Input(IDX:1)) TO CHAR-ORD
           IF CHAR-ORD >= 66 AND CHAR-ORD <= 91
               MOVE 1 TO Upper-Flag
           ELSE IF CHAR-ORD >= 49 AND CHAR-ORD <= 58
               MOVE 1 TO Digit-Flag
           ELSE IF (CHAR-ORD >= 34 AND CHAR-ORD <= 48) OR
                  (CHAR-ORD >= 59 AND CHAR-ORD <= 65) OR
                  (CHAR-ORD >= 92 AND CHAR-ORD <= 97) OR
                  (CHAR-ORD >= 124 AND CHAR-ORD <= 127)
               MOVE 1 TO Special-Flag
           END-IF
       END-PERFORM.

       IF Upper-Flag = 1 AND Digit-Flag = 1 AND Special-Flag = 1
           MOVE 'Y' TO Password-Valid
           EXIT SECTION
       ELSE
           MOVE "Password must include at least one uppercase letter, one digit, and one special character." TO Message-Text
           PERFORM Write-And-Display
           MOVE 'N' TO Password-Valid
           EXIT SECTION
       END-IF

       MOVE 'Y' TO Password-Valid
       EXIT.

Write-Account SECTION.
       OPEN EXTEND AccountsFile
       MOVE Account-Username-Input TO Account-Username
       MOVE Account-Password-Input TO Account-Password
       WRITE Account-Record
       CLOSE AccountsFile
       EXIT.

