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

       PERFORM MAIN-AUTHENTICATE

       IF Is-Logged-In = 'N'
           CLOSE InputFile
           STOP RUN
       END-IF

       STRING "Welcome, " Account-Username INTO Message-Text
       PERFORM WRITE-AND-DISPLAY

       PERFORM SHOW-MAIN-MENU

       CLOSE InputFile
       STOP RUN.


*> AUTHENTICATION
MAIN-AUTHENTICATE SECTION.
       MOVE "Welcome to InCollege!" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "1. Log In" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "2. Create New Account" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "Enter your choice (1 or 2): " TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       EXIT.

       READ InputFile INTO User-Input
           AT END
               MOVE "No input found." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               CLOSE InputFile
      *>         CLOSE OutputFile
               STOP RUN
       END-READ

       EVALUATE User-Input
           WHEN "1"
               PERFORM LOG-IN
           WHEN "2"
               PERFORM CREATE-ACCOUNT
           WHEN OTHER
               MOVE "Invalid choice. Please choose 1-2." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM MAIN-AUTHENTICATE
       END-EVALUATE.
       EXIT SECTION.


TAKE-ACCOUNT-INPUT SECTION.
       MOVE "Please enter your username: " TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       READ InputFile INTO Account-Username-Input
           AT END
               MOVE "No username input found." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               CLOSE InputFile
               STOP RUN
       END-READ

       MOVE "Please enter your password: " TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       READ InputFile INTO Account-Password-Input
           AT END
               MOVE "No password input found." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               CLOSE InputFile
               STOP RUN
       END-READ
       EXIT SECTION.

CREATE-ACCOUNT SECTION.
       *> Count how many accounts exist
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

         *> If already 5 accounts, show message and return to login menu
       IF IDX >= 5
           MOVE "All permitted accounts have been created, please come back later."
               TO Message-Text
           PERFORM WRITE-AND-DISPLAY

           PERFORM MAIN-AUTHENTICATE
           EXIT SECTION
       END-IF

       *> Otherwise, proceed to ask for new username/password
       MOVE 'N' TO Is-Logged-In
       PERFORM UNTIL Is-Logged-In = 'Y'
           PERFORM TAKE-ACCOUNT-INPUT     *> (better: Ask-For-New-Account)
           PERFORM CREATE-ACCOUNT-AUTHENTICATE
       END-PERFORM
       EXIT SECTION.

CREATE-ACCOUNT-AUTHENTICATE SECTION.
      *> MOVE 'RUNNING CREATE-ACCOUNT-AUTHENTICATE' TO Message-Text
      *> PERFORM WRITE-AND-DISPLAY

       MOVE 'N' TO Is-Logged-In
       MOVE 'N' TO Username-Exists

       OPEN INPUT AccountsFile

       PERFORM UNTIL Username-Exists = 'Y' OR Is-Logged-In = 'Y'
           READ AccountsFile
               AT END
                    *> No accounts exist yet, so username is unique
                   CLOSE AccountsFile
                   IF FUNCTION LENGTH(FUNCTION TRIM(Account-Username-Input)) > 0 AND FUNCTION LENGTH(FUNCTION TRIM(Account-Username-Input)) < 50
                       PERFORM VERIFY-PASSWORD
                       IF Password-Valid = 'Y'
                           PERFORM WRITE-ACCOUNT
                           MOVE 'Y' TO Is-Logged-In
                           MOVE "Account created successfully!" TO Message-Text
                           PERFORM WRITE-AND-DISPLAY
                       ELSE
                           EXIT SECTION
                       END-IF
                   ELSE
                       MOVE "Invalid username length. Try again." TO Message-Text
                       PERFORM WRITE-AND-DISPLAY
                       EXIT SECTION
                   END-IF

               NOT AT END
                   IF Account-Username = Account-Username-Input
                        MOVE "Username already exists. Try again." TO Message-Text
                        PERFORM WRITE-AND-DISPLAY
                        MOVE 'Y' TO Username-Exists
                        CLOSE AccountsFile
                        EXIT SECTION
                   END-IF
           END-READ
       END-PERFORM
       EXIT SECTION.

LOG-IN SECTION.
       MOVE 'N' TO Is-Logged-In
       PERFORM UNTIL Is-Logged-In = 'Y'
           PERFORM TAKE-ACCOUNT-INPUT
           PERFORM LOG-IN-AUTHENTICATE
       END-PERFORM
       EXIT SECTION.

LOG-IN-AUTHENTICATE SECTION.
       MOVE 'N' TO Is-Logged-In
       MOVE 'N' TO Username-Exists
       OPEN INPUT AccountsFile

       PERFORM UNTIL Username-Exists = 'Y'
           READ AccountsFile
               AT END
                   MOVE "Invalid username. Please try again." TO Message-Text
                   PERFORM WRITE-AND-DISPLAY
                   CLOSE AccountsFile
                   PERFORM LOG-IN
               NOT AT END
                   IF Account-Username = Account-Username-Input
                       MOVE 'Y' TO Username-Exists
                       IF Account-Password = Account-Password-Input
                           MOVE 'Y' TO Is-Logged-In
                           MOVE "You have successfully logged in!" TO Message-Text
                           PERFORM WRITE-AND-DISPLAY
                           CLOSE AccountsFile
                       ELSE
                            MOVE "Invalid password. Please try again." TO Message-Text
                            PERFORM WRITE-AND-DISPLAY
                            CLOSE AccountsFile
                            PERFORM LOG-IN
                          END-IF
                   END-IF
           END-READ
       END-PERFORM
       EXIT SECTION.



VERIFY-PASSWORD SECTION.
       MOVE 'N' TO Password-Valid
       MOVE FUNCTION LENGTH(FUNCTION TRIM(Account-Password-Input)) TO PW-Length
       MOVE 0 TO Upper-Flag Digit-Flag Special-Flag


       IF PW-Length < 8 OR PW-Length > 12
           MOVE "Password must be 8-12 characters." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
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
           PERFORM WRITE-AND-DISPLAY
           MOVE 'N' TO Password-Valid
           EXIT SECTION
       END-IF

       MOVE 'Y' TO Password-Valid
       EXIT SECTION.

*> MAIN MENU
SHOW-MAIN-MENU SECTION.
       MOVE "1. Create/Edit My Profile" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "2. View My Profile" TO Message-Text
        PERFORM WRITE-AND-DISPLAY
       MOVE "3. Learn a new skill" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "4. Search for a user" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "Enter your choice (1-4): " TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       READ InputFile INTO User-Input
           AT END
               MOVE "No input found." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               CLOSE InputFile
               STOP RUN
       END-READ

       EVALUATE User-Input
           WHEN "1"
               MOVE "CREATE/EDIT PROFILE IS CHOSEN" TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM SHOW-MAIN-MENU
           WHEN "2"
               MOVE "FIND SOMEONE YOU KNOW IS CHOSEN" TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM SHOW-MAIN-MENU
           WHEN "3"
               PERFORM LEARN-SKILL-MENU
            WHEN "4"
                MOVE "SEARCH FOR A USER IS UNDER CONSTRUCTION" TO Message-Text
                PERFORM WRITE-AND-DISPLAY
                PERFORM SHOW-MAIN-MENU
           WHEN OTHER
               MOVE "Invalid choice. Please choose from 1-4." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM SHOW-MAIN-MENU
       END-EVALUATE.
       EXIT SECTION.


LEARN-SKILL-MENU SECTION.
       MOVE "1. AWS" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "2. Docker" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "3. COBOL" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "4. Azure" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "5. GCP" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "6. Return to main menu" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "Enter your choice (1-6): " TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       READ InputFile INTO User-Input
           AT END
               MOVE "No skill input found." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               CLOSE InputFile
               STOP RUN
       END-READ

       EVALUATE User-Input
           WHEN "1"
               MOVE "AWS is under construction." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM LEARN-SKILL-MENU
           WHEN "2"
               MOVE "Docker is under construction." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
                PERFORM LEARN-SKILL-MENU
           WHEN "3"
               MOVE "COBOL is under construction." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM LEARN-SKILL-MENU
           WHEN "4"
               MOVE "Azure is under construction." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM LEARN-SKILL-MENU
           WHEN "5"
               MOVE "GCP is under construction." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM LEARN-SKILL-MENU
           WHEN "6"
               PERFORM SHOW-MAIN-MENU
               EXIT SECTION
           WHEN OTHER
               MOVE "Invalid skill choice. Please choose from 1-6." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM LEARN-SKILL-MENU
       END-EVALUATE.
       EXIT SECTION.

WRITE-AND-DISPLAY SECTION.
       OPEN EXTEND OutputFile
       MOVE Message-Text TO Output-Line.
       DISPLAY Output-Line
       WRITE Output-Line
       CLOSE OutputFile
       EXIT SECTION.

WRITE-ACCOUNT SECTION.
       OPEN EXTEND AccountsFile
       MOVE Account-Username-Input TO Account-Username
       MOVE Account-Password-Input TO Account-Password
       WRITE Account-Record
       CLOSE AccountsFile
       EXIT SECTION.

