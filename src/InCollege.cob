*> This is free-form
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.
ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT InputFile ASSIGN TO "/workspace/src/InCollege-Input.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT OutputFile ASSIGN TO "/workspace/src/InCollege-Output.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT AccountsFile ASSIGN TO "/workspace/src/Accounts.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT ProfilesFile ASSIGN TO "/workspace/src/Profiles.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT TempProfilesFile ASSIGN TO "/workspace/src/ProfilesTmp.txt"
                    ORGANIZATION IS LINE SEQUENTIAL.

               SELECT ConnectionsFile ASSIGN TO "/workspace/src/Connections.txt"
                    ORGANIZATION IS LINE SEQUENTIAL.
               SELECT EstablishedFile ASSIGN TO "/workspace/src/Established.txt"
                    ORGANIZATION IS LINE SEQUENTIAL.
               SELECT TempConnectionsFile ASSIGN TO "/workspace/src/Connect.txt"
                    ORGANIZATION IS LINE SEQUENTIAL.

               SELECT JobsFile ASSIGN TO "/workspace/src/Jobs.txt"
                    ORGANIZATION IS LINE SEQUENTIAL.



DATA DIVISION.
       FILE SECTION.
           FD InputFile.
               01 User-Input PIC X(300).
           FD OutputFile.
               01 Output-Line PIC X(300).
           FD AccountsFile.
               01 Account-Record.
                   05 Account-Username PIC X(50).
                   05 Account-Password PIC X(50).

           FD ProfilesFile.
               01 Profile-Record.
                  05 Prof-Username                 PIC X(50).
                  05 Prof-FirstName                PIC X(30).
                  05 Prof-LastName                 PIC X(30).
                  05 Prof-University               PIC X(60).
                  05 Prof-Major                    PIC X(40).
                  05 Prof-GradYear                 PIC 9(4).
                  05 Prof-About                    PIC X(200).
                  05 Prof-Exp-Count                PIC 9.

                  05 Prof-Exp OCCURS 3 TIMES.
                      10 Prof-Exp-Title    PIC X(40).
                      10 Prof-Exp-Company  PIC X(40).
                      10 Prof-Exp-Dates    PIC X(30).
                      10 Prof-Exp-Desc     PIC X(100).

                  05 Prof-Edu-Count                PIC 9.
                  05 Prof-Edu OCCURS 3 TIMES.
                     10 Prof-Edu-Degree     PIC X(40).
                     10 Prof-Edu-University PIC X(60).
                     10 Prof-Edu-Years      PIC X(20).

           FD TempProfilesFile.
                01 Temp-Profile-Record.
                   05 T-Prof-Username       PIC X(50).
                   05 T-Prof-FirstName      PIC X(30).
                   05 T-Prof-LastName       PIC X(30).
                   05 T-Prof-University     PIC X(60).
                   05 T-Prof-Major          PIC X(40).
                   05 T-Prof-GradYear       PIC 9(4).
                   05 T-Prof-About          PIC X(200).
                   05 T-Prof-Exp-Count      PIC 9.
                   05 T-Prof-Exp OCCURS 3 TIMES.
                      10 T-Prof-Exp-Title    PIC X(40).
                      10 T-Prof-Exp-Company  PIC X(40).
                      10 T-Prof-Exp-Dates    PIC X(30).
                      10 T-Prof-Exp-Desc     PIC X(100).
                   05 T-Prof-Edu-Count      PIC 9.
                   05 T-Prof-Edu OCCURS 3 TIMES.
                      10 T-Prof-Edu-Degree     PIC X(40).
                      10 T-Prof-Edu-University PIC X(60).
                      10 T-Prof-Edu-Years      PIC X(20).

           FD ConnectionsFile.
                   01 Connection-Record-Line PIC X(200).
           FD EstablishedFile.
                   01 Established-Record-Line PIC X(200).

           FD TempConnectionsFile.
                   01 Temp-Connection-Record-Line PIC X(200).

           FD JobsFile.
                   01 Job-Record PIC X(550).

       WORKING-STORAGE SECTION.
           01 Message-Text PIC X(300).
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

           01 Validation-Failed PIC X VALUE 'N'.

           01 Found-Username PIC X(50) VALUE SPACES.

           *> Connection
           01 Conn-Sender-WS      PIC X(50) VALUE SPACES.
           01 Conn-Recipient-WS   PIC X(50) VALUE SPACES.

           01 Current-Username            PIC X(50).  *> set after successful login
           01 Temp-Input                   PIC X(100).

            *>     * Required profile fields
           01 WS-FirstName                 PIC X(30).
           01 WS-LastName                  PIC X(30).
           01 WS-University                PIC X(60).
           01 WS-Major                     PIC X(40).
           01 WS-GradYear-Text             PIC X(4).
           01 WS-GradYear                  PIC 9(4).

           *>     * Optional fields
           01 WS-About                     PIC X(200).

           *>     * Experience arrays
           01 WS-Exp-Count                 PIC 9 VALUE 0.
           01 WS-Exp-Titles     OCCURS 3 TIMES PIC X(40).
           01 WS-Exp-Companies  OCCURS 3 TIMES PIC X(40).
           01 WS-Exp-Dates      OCCURS 3 TIMES PIC X(30).
           01 WS-Exp-Descs      OCCURS 3 TIMES PIC X(100).

           *>     * Education arrays
           01 WS-Edu-Count                 PIC 9 VALUE 0.
           01 WS-Edu-Degrees    OCCURS 3 TIMES PIC X(40).
           01 WS-Edu-Univers    OCCURS 3 TIMES PIC X(60).
           01 WS-Edu-Years      OCCURS 3 TIMES PIC X(20).

           *>     * Loop helpers
           01 I                          PIC 9 VALUE 0.
           01 Found-Flag                  PIC X VALUE 'N'.
           01 All-Digits                 PIC X VALUE 'Y'.

           *>Helper for string operations
           01  Ptr          PIC 9(4).

           *> Search variables
           01 Search-Name                 PIC X(100).
           01 Full-Name                   PIC X(100).

           *> Request Variables
           01 Request-Choice          PIC X(1).
           01 Already-Exists          PIC X VALUE 'N'.
           01 Request-Sender          PIC X(50).
           01 Request-Recipient       PIC X(50).

           *> Jobs Variables
           01 WS-Job-Title       PIC X(100).
           01 WS-Job-Description PIC X(200).
           01 WS-Job-Employer    PIC X(100).
           01 WS-Job-Location    PIC X(100).
           01 WS-Job-Salary      PIC X(50).


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


       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO User-Input

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
                           MOVE Account-Username-Input TO Current-Username
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
       Move "------- Main Menu --------" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "1. Create/Edit My Profile" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "2. View My Profile" TO Message-Text
        PERFORM WRITE-AND-DISPLAY
       MOVE "3. Learn a new skill" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "4. Find someone you know" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "5. View My Pending Connection Requests" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "6. View My Connections" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "7. Job /Internship Search" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "Enter your choice (1-7): " TO Message-Text
       PERFORM WRITE-AND-DISPLAY


       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO User-Input

       EVALUATE User-Input
           WHEN "1"
               PERFORM CREATE-EDIT-PROFILE
               PERFORM SHOW-MAIN-MENU
           WHEN "2"
               PERFORM VIEW-PROFILE
               PERFORM SHOW-MAIN-MENU
           WHEN "3"
               PERFORM LEARN-SKILL-MENU
           WHEN "4"
               PERFORM SEARCH-USER
           WHEN "5"
               PERFORM VIEW-PENDING-REQUESTS
               PERFORM SHOW-MAIN-MENU
           WHEN "6"
               PERFORM VIEW-CONNECTIONS
               PERFORM SHOW-MAIN-MENU
           WHEN "7"
                PERFORM JOB-INTERNSHIP-SEARCH
           WHEN OTHER
               MOVE "Invalid choice. Please choose from 1-7." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM SHOW-MAIN-MENU
       END-EVALUATE.

       EXIT SECTION.
*> PROFILE
CREATE-EDIT-PROFILE SECTION.
       MOVE "--- Create/Edit Profile ---" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       *> First Name (required)
       MOVE "Enter First Name:" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-FirstName


       *> Last Name (required)
       MOVE "Enter Last Name:" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-LastName


       *> University (required)
       MOVE "Enter University/College Attended:" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-University

       *> Major (required)
       MOVE "Enter Major:" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-Major


       *> Graduation Year (required, 4 digits, reasonable range)
       MOVE "Enter Graduation Year (YYYY):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-GradYear-Text

       *> About Me (optional)
       MOVE "Enter About Me (optional, max 200 chars, blank to skip):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-About

       *> Experience loop (up to 3)
       MOVE 0 TO WS-Exp-Count
       MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           MOVE "Experience # " TO Message-Text
           STRING Message-Text DELIMITED BY SIZE
                  FUNCTION NUMVAL(I) DELIMITED BY SIZE
                  " - Title (or DONE):" DELIMITED BY SIZE
                  INTO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM READ-NEXT-INPUT
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(User-Input)) = "DONE"
               EXIT PERFORM
           END-IF
           ADD 1 TO WS-Exp-Count
           MOVE FUNCTION TRIM(User-Input)         TO WS-Exp-Titles(I)

           MOVE "Company/Organization:" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(User-Input)         TO WS-Exp-Companies(I)

           MOVE "Dates (e.g., Summer 2024):" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(User-Input)         TO WS-Exp-Dates(I)

           MOVE "Description (optional, blank to skip):" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(User-Input)         TO WS-Exp-Descs(I)
       END-PERFORM

       *> Education loop (up to 3)
       MOVE 0 TO WS-Edu-Count
       MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           MOVE "Education # " TO Message-Text
           STRING Message-Text DELIMITED BY SIZE
                  FUNCTION NUMVAL(I) DELIMITED BY SIZE
                  " - Degree (or DONE):" DELIMITED BY SIZE
                  INTO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM READ-NEXT-INPUT
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(User-Input)) = "DONE"
               EXIT PERFORM
           END-IF
           ADD 1 TO WS-Edu-Count
           MOVE FUNCTION TRIM(User-Input)         TO WS-Edu-Degrees(I)

           MOVE "University/College:" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(User-Input)         TO WS-Edu-Univers(I)

           MOVE "Years Attended (e.g., 2023-2025):" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(User-Input)         TO WS-Edu-Years(I)
       END-PERFORM

       PERFORM VALIDATE-AND-SAVE
       EXIT SECTION.

VALIDATE-AND-SAVE SECTION.
       MOVE 'N' TO Validation-Failed

       *> --- Validation checks ---
       IF WS-FirstName = SPACES
           MOVE "First Name is required." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE 'Y' TO Validation-Failed
       END-IF

       IF WS-LastName = SPACES
           MOVE "Last Name is required." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE 'Y' TO Validation-Failed
       END-IF

       IF WS-University = SPACES
           MOVE "University is required." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE 'Y' TO Validation-Failed
       END-IF

       IF WS-Major = SPACES
           MOVE "Major is required." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE 'Y' TO Validation-Failed
       END-IF

       IF FUNCTION LENGTH(WS-GradYear-Text) NOT = 4
           MOVE "Graduation Year must be 4 digits." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE 'Y' TO Validation-Failed
       ELSE
           MOVE 'Y' TO All-Digits
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               IF WS-GradYear-Text(I:1) < "0" OR WS-GradYear-Text(I:1) > "9"
                   MOVE 'N' TO All-Digits
               END-IF
           END-PERFORM
           IF All-Digits NOT = 'Y'
               MOVE "Graduation Year must be numeric." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               MOVE 'Y' TO Validation-Failed
           ELSE
               MOVE WS-GradYear-Text TO WS-GradYear
               IF WS-GradYear < 1900 OR WS-GradYear > 2099
                   MOVE "Graduation Year out of range (1900–2099)." TO Message-Text
                   PERFORM WRITE-AND-DISPLAY
                   MOVE 'Y' TO Validation-Failed
               END-IF
           END-IF
       END-IF

       IF FUNCTION LENGTH(WS-About) > 200
           MOVE "About Me must be at most 200 characters." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE 'Y' TO Validation-Failed
       END-IF

       *> --- Stop if any validation failed ---
       IF Validation-Failed = 'Y'
           MOVE "Profile not saved due to errors." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           EXIT SECTION
       END-IF

       *> --- Save profile if all validations passed ---
       PERFORM SAVE-PROFILE
       MOVE "Profile saved successfully!" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       EXIT SECTION.

SAVE-PROFILE SECTION.
       *> Step 1: Copy all profiles except current user into TempProfilesFile
       OPEN INPUT ProfilesFile
       OPEN OUTPUT TempProfilesFile

       PERFORM UNTIL 1 = 0
           READ ProfilesFile
               AT END EXIT PERFORM
               NOT AT END
                   IF Prof-Username NOT = Current-Username
                       MOVE Profile-Record TO Temp-Profile-Record
                       WRITE Temp-Profile-Record
                   END-IF
           END-READ
       END-PERFORM

       CLOSE ProfilesFile

       *> Step 2: Build the new profile for current user
       MOVE Current-Username  TO Prof-Username
       MOVE WS-FirstName      TO Prof-FirstName
       MOVE WS-LastName       TO Prof-LastName
       MOVE WS-University     TO Prof-University
       MOVE WS-Major          TO Prof-Major
       MOVE WS-GradYear       TO Prof-GradYear
       MOVE WS-About          TO Prof-About

       MOVE WS-Exp-Count      TO Prof-Exp-Count
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           IF I <= WS-Exp-Count
               MOVE WS-Exp-Titles(I)    TO Prof-Exp-Title(I)
               MOVE WS-Exp-Companies(I) TO Prof-Exp-Company(I)
               MOVE WS-Exp-Dates(I)     TO Prof-Exp-Dates(I)
               MOVE WS-Exp-Descs(I)     TO Prof-Exp-Desc(I)
           ELSE
               MOVE SPACES TO Prof-Exp-Title(I)
               MOVE SPACES TO Prof-Exp-Company(I)
               MOVE SPACES TO Prof-Exp-Dates(I)
               MOVE SPACES TO Prof-Exp-Desc(I)
           END-IF
       END-PERFORM

       MOVE WS-Edu-Count TO Prof-Edu-Count
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
           IF I <= WS-Edu-Count
               MOVE WS-Edu-Degrees(I) TO Prof-Edu-Degree(I)
               MOVE WS-Edu-Univers(I) TO Prof-Edu-University(I)
               MOVE WS-Edu-Years(I)   TO Prof-Edu-Years(I)
           ELSE
               MOVE SPACES TO Prof-Edu-Degree(I)
               MOVE SPACES TO Prof-Edu-University(I)
               MOVE SPACES TO Prof-Edu-Years(I)
           END-IF
       END-PERFORM

       *> Step 3: Append new/edited profile into TempProfilesFile
       MOVE Profile-Record TO Temp-Profile-Record
       WRITE Temp-Profile-Record

       CLOSE TempProfilesFile

       *> Step 4: Copy TempProfilesFile back to ProfilesFile
       OPEN INPUT TempProfilesFile
       OPEN OUTPUT ProfilesFile

       PERFORM UNTIL 1 = 0
           READ TempProfilesFile
               AT END EXIT PERFORM
               NOT AT END
                   MOVE Temp-Profile-Record TO Profile-Record
                   WRITE Profile-Record
           END-READ
       END-PERFORM

       CLOSE TempProfilesFile
       CLOSE ProfilesFile
       EXIT SECTION.

VIEW-PROFILE SECTION.
       MOVE "--- Your Profile ---" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       OPEN INPUT ProfilesFile
       MOVE 'N' TO Found-Flag

       *> Scan entire file to find current user's profile
       PERFORM UNTIL 1 = 0
           READ ProfilesFile
               AT END
                   EXIT PERFORM
               NOT AT END
                   IF Prof-Username = Current-Username
                       MOVE 'Y' TO Found-Flag
                       *> Display profile information
                       PERFORM DISPLAY-PROFILE-INFO
                   END-IF
           END-READ
       END-PERFORM
       CLOSE ProfilesFile

       IF Found-Flag NOT = 'Y'
           MOVE "No profile found for current user." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           EXIT SECTION
       END-IF

       EXIT SECTION.

DISPLAY-PROFILE-INFO SECTION.
       *> Name
       MOVE SPACES TO Message-Text
       MOVE 1 TO Ptr
       STRING "Name: " DELIMITED BY SIZE
              FUNCTION TRIM(Prof-FirstName) DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              FUNCTION TRIM(Prof-LastName) DELIMITED BY SIZE
              INTO Message-Text
              WITH POINTER Ptr
       PERFORM WRITE-AND-DISPLAY

       *> University
       MOVE SPACES TO Message-Text
       MOVE 1 TO Ptr
       STRING "University: " DELIMITED BY SIZE
              FUNCTION TRIM(Prof-University) DELIMITED BY SIZE
              INTO Message-Text
              WITH POINTER Ptr
       PERFORM WRITE-AND-DISPLAY

       *> Major
       MOVE SPACES TO Message-Text
       MOVE 1 TO Ptr
       STRING "Major: " DELIMITED BY SIZE
              FUNCTION TRIM(Prof-Major) DELIMITED BY SIZE
              INTO Message-Text
              WITH POINTER Ptr
       PERFORM WRITE-AND-DISPLAY

       *> Graduation Year
       MOVE SPACES TO Message-Text
       MOVE 1 TO Ptr
       STRING "Graduation Year: " DELIMITED BY SIZE
              Prof-GradYear DELIMITED BY SIZE
              INTO Message-Text
              WITH POINTER Ptr
       PERFORM WRITE-AND-DISPLAY

       *> About Me
       IF FUNCTION TRIM(Prof-About) NOT = SPACES
           MOVE SPACES TO Message-Text
           MOVE 1 TO Ptr
           STRING "About Me: " DELIMITED BY SIZE
                  FUNCTION TRIM(Prof-About) DELIMITED BY SIZE
                  INTO Message-Text
                  WITH POINTER Ptr
           PERFORM WRITE-AND-DISPLAY
       ELSE
           MOVE "About Me: " TO Message-Text
           PERFORM WRITE-AND-DISPLAY
       END-IF

       *> Experience
       MOVE "Experience:" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       IF Prof-Exp-Count = 0
           MOVE "  None" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
       ELSE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > Prof-Exp-Count
               MOVE SPACES TO Message-Text
               MOVE 1 TO Ptr
               STRING "  Title: " DELIMITED BY SIZE
                      FUNCTION TRIM(Prof-Exp-Title(I)) DELIMITED BY SIZE
                      INTO Message-Text
                      WITH POINTER Ptr
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO Message-Text
               MOVE 1 TO Ptr
               STRING "  Company: " DELIMITED BY SIZE
                      FUNCTION TRIM(Prof-Exp-Company(I)) DELIMITED BY SIZE
                      INTO Message-Text
                      WITH POINTER Ptr
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO Message-Text
               MOVE 1 TO Ptr
               STRING "  Dates: " DELIMITED BY SIZE
                      FUNCTION TRIM(Prof-Exp-Dates(I)) DELIMITED BY SIZE
                      INTO Message-Text
                      WITH POINTER Ptr
               PERFORM WRITE-AND-DISPLAY

               IF FUNCTION TRIM(Prof-Exp-Desc(I)) NOT = SPACES
                   MOVE SPACES TO Message-Text
                   MOVE 1 TO Ptr
                   STRING "  Description: " DELIMITED BY SIZE
                          FUNCTION TRIM(Prof-Exp-Desc(I)) DELIMITED BY SIZE
                          INTO Message-Text
                          WITH POINTER Ptr
                   PERFORM WRITE-AND-DISPLAY
               ELSE
                   MOVE "  Description: " TO Message-Text
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-PERFORM
       END-IF

       *> Education
       MOVE "Education:" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       IF Prof-Edu-Count = 0
           MOVE "  None" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
       ELSE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > Prof-Edu-Count
               MOVE SPACES TO Message-Text
               MOVE 1 TO Ptr
               STRING "  Degree: " DELIMITED BY SIZE
                      FUNCTION TRIM(Prof-Edu-Degree(I)) DELIMITED BY SIZE
                      INTO Message-Text
                      WITH POINTER Ptr
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO Message-Text
               MOVE 1 TO Ptr
               STRING "  University: " DELIMITED BY SIZE
                      FUNCTION TRIM(Prof-Edu-University(I)) DELIMITED BY SIZE
                      INTO Message-Text
                      WITH POINTER Ptr
               PERFORM WRITE-AND-DISPLAY

               MOVE SPACES TO Message-Text
               MOVE 1 TO Ptr
               STRING "  Years: " DELIMITED BY SIZE
                      FUNCTION TRIM(Prof-Edu-Years(I)) DELIMITED BY SIZE
                      INTO Message-Text
                      WITH POINTER Ptr
               PERFORM WRITE-AND-DISPLAY
           END-PERFORM
       END-IF
*>
      *> MOVE "--------------------" TO Message-Text
      *> PERFORM WRITE-AND-DISPLAY
       EXIT SECTION.

*> USER SEARCH FUNCTIONALITY
SEARCH-USER SECTION.
       MOVE "Enter the full name of the person you are looking for:" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO Search-Name

       *> Search through profiles file
       OPEN INPUT ProfilesFile
       MOVE 'N' TO Found-Flag
       MOVE SPACES TO Found-Username

       PERFORM UNTIL 1 = 0
           READ ProfilesFile
               AT END
                   EXIT PERFORM
               NOT AT END
                   *> Build full name from profile
                   MOVE SPACES TO Full-Name
                   MOVE 1 TO Ptr
                   STRING FUNCTION TRIM(Prof-FirstName) DELIMITED BY SIZE
                          " " DELIMITED BY SIZE
                          FUNCTION TRIM(Prof-LastName) DELIMITED BY SIZE
                          INTO Full-Name
                          WITH POINTER Ptr

                   *> Case-insensitive compare
                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(Full-Name)) =
                      FUNCTION UPPER-CASE(FUNCTION TRIM(Search-Name))
                       MOVE 'Y' TO Found-Flag
                       MOVE Prof-Username TO Found-Username
                       EXIT PERFORM
                   END-IF
           END-READ
       END-PERFORM
       CLOSE ProfilesFile

       IF Found-Flag = 'Y'
           MOVE "--- Found User Profile ---" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM DISPLAY-PROFILE-INFO

           *> Connection request menu
           MOVE "1. Send Connection Request" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE "2. Back to Main Menu" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE "Enter your choice: " TO Message-Text
           PERFORM WRITE-AND-DISPLAY

           *> Use helper to avoid READ-after-EOF errors
           PERFORM READ-NEXT-INPUT
           MOVE User-Input(1:1) TO Request-Choice

           EVALUATE Request-Choice
               WHEN "1"
                   PERFORM SEND-CONNECTION-REQUEST
                   PERFORM SHOW-MAIN-MENU
               WHEN "2"
                   PERFORM SHOW-MAIN-MENU
               WHEN OTHER
                   MOVE "Invalid choice. Returning to main menu." TO Message-Text
                   PERFORM WRITE-AND-DISPLAY
                   PERFORM SHOW-MAIN-MENU
           END-EVALUATE

       ELSE
           MOVE "No one by that name could be found." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM SHOW-MAIN-MENU
           EXIT SECTION
       END-IF

       EXIT SECTION.


*> LEARN A NEW SKILL
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

       PERFORM READ-NEXT-INPUT

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


*> SEND CONNECTION REQUEST SECTIONS
SEND-CONNECTION-REQUEST SECTION.
       MOVE FUNCTION TRIM(Current-Username)  TO Request-Sender
       MOVE FUNCTION TRIM(Found-Username)    TO Request-Recipient
       MOVE 'N' TO Already-Exists

       IF Request-Recipient = SPACES
           MOVE "Unable to send request: target username not found." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           EXIT SECTION
       END-IF

       IF FUNCTION UPPER-CASE(Request-Sender) =
          FUNCTION UPPER-CASE(Request-Recipient)
           MOVE "You cannot send a connection request to yourself." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           EXIT SECTION
       END-IF

       *> NEW: Check Established.txt first
       OPEN INPUT EstablishedFile
       PERFORM UNTIL 1 = 0
           READ EstablishedFile INTO Established-Record-Line
               AT END EXIT PERFORM
               NOT AT END
                   MOVE SPACES TO Conn-Sender-WS Conn-Recipient-WS
                   UNSTRING Established-Record-Line
                       DELIMITED BY "|"
                       INTO Conn-Sender-WS
                            Conn-Recipient-WS

                   IF (FUNCTION TRIM(Conn-Sender-WS)    = Request-Sender AND
                       FUNCTION TRIM(Conn-Recipient-WS) = Request-Recipient)
                      OR
                      (FUNCTION TRIM(Conn-Sender-WS)    = Request-Recipient AND
                       FUNCTION TRIM(Conn-Recipient-WS) = Request-Sender)
                       MOVE "You are already connected with this user." TO Message-Text
                       PERFORM WRITE-AND-DISPLAY
                       MOVE 'Y' TO Already-Exists
                       EXIT PERFORM
                   END-IF
           END-READ
       END-PERFORM
       CLOSE EstablishedFile

       IF Already-Exists = 'Y'
           EXIT SECTION
       END-IF
       *> END NEW

       OPEN INPUT ConnectionsFile
       PERFORM UNTIL 1 = 0
           READ ConnectionsFile INTO Connection-Record-Line
               AT END EXIT PERFORM
               NOT AT END
                   MOVE SPACES TO Conn-Sender-WS Conn-Recipient-WS
                   UNSTRING Connection-Record-Line
                       DELIMITED BY "|"
                       INTO Conn-Sender-WS
                            Conn-Recipient-WS

                   IF FUNCTION TRIM(Conn-Sender-WS)    = Request-Sender AND
                      FUNCTION TRIM(Conn-Recipient-WS) = Request-Recipient
                       MOVE "You already sent a connection request to this user." TO Message-Text
                       PERFORM WRITE-AND-DISPLAY
                       MOVE 'Y' TO Already-Exists
                       EXIT PERFORM
                   END-IF

                   IF FUNCTION TRIM(Conn-Sender-WS)    = Request-Recipient AND
                      FUNCTION TRIM(Conn-Recipient-WS) = Request-Sender
                       MOVE "This user has already sent you a connection request." TO Message-Text
                       PERFORM WRITE-AND-DISPLAY
                       MOVE 'Y' TO Already-Exists
                       EXIT PERFORM
                   END-IF
           END-READ
       END-PERFORM
       CLOSE ConnectionsFile

       IF Already-Exists = 'Y'
           EXIT SECTION
       END-IF

       OPEN EXTEND ConnectionsFile
       MOVE SPACES TO Connection-Record-Line
       STRING FUNCTION TRIM(Request-Sender)    DELIMITED BY SIZE
              "|"                                DELIMITED BY SIZE
              FUNCTION TRIM(Request-Recipient)   DELIMITED BY SIZE
              INTO Connection-Record-Line
       WRITE Connection-Record-Line
       CLOSE ConnectionsFile

       MOVE SPACES TO Message-Text
       MOVE 1 TO Ptr
       STRING "You have successfully sent a connection request to " DELIMITED BY SIZE
              FUNCTION TRIM(Prof-FirstName) DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              FUNCTION TRIM(Prof-LastName) DELIMITED BY SIZE
              "!" DELIMITED BY SIZE
              INTO Message-Text
              WITH POINTER Ptr
       PERFORM WRITE-AND-DISPLAY

       EXIT SECTION.



*> VIEW PENDING REQUEST SECTIONS
VIEW-PENDING-REQUESTS SECTION.
       MOVE "--- Pending Connection Requests ---" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE 'N' TO Found-Flag

       *> Step 1: Display all pending requests first
       OPEN INPUT ConnectionsFile
       PERFORM UNTIL 1 = 0
           READ ConnectionsFile INTO Connection-Record-Line
               AT END EXIT PERFORM
               NOT AT END
                   UNSTRING Connection-Record-Line
                       DELIMITED BY "|"
                       INTO Conn-Sender-WS
                            Conn-Recipient-WS
                   IF FUNCTION TRIM(Conn-Recipient-WS) = FUNCTION TRIM(Current-Username)
                       MOVE 'Y' TO Found-Flag
                       PERFORM DISPLAY-REQUEST-FROM
                   END-IF
           END-READ
       END-PERFORM
       CLOSE ConnectionsFile

       IF Found-Flag = 'N'
           MOVE "You have no pending connection requests at this time." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           EXIT SECTION
       END-IF

       MOVE "-----------------------------------" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       *> Step 2: Loop through each request for accept/reject
       PERFORM UNTIL 1 = 0
           MOVE 'N' TO Found-Flag

           *> Find the first request for current user
           OPEN INPUT ConnectionsFile
           MOVE SPACES TO Conn-Sender-WS Conn-Recipient-WS
           PERFORM UNTIL 1 = 0
               READ ConnectionsFile INTO Connection-Record-Line
                   AT END EXIT PERFORM
                   NOT AT END
                       UNSTRING Connection-Record-Line
                           DELIMITED BY "|"
                           INTO Conn-Sender-WS
                                Conn-Recipient-WS
                       IF FUNCTION TRIM(Conn-Recipient-WS) = FUNCTION TRIM(Current-Username)
                           MOVE 'Y' TO Found-Flag
                           *> Save original sender/recipient for later removal
                           MOVE FUNCTION TRIM(Conn-Sender-WS) TO Request-Sender
                           MOVE FUNCTION TRIM(Conn-Recipient-WS) TO Request-Recipient
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ConnectionsFile

           *> If no more requests, exit the loop
           IF Found-Flag = 'N'
               EXIT PERFORM
           END-IF

           *> Ask Accept/Reject for this request
           PERFORM DISPLAY-REQUEST-FROM

           MOVE "1. Accept" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE "2. Reject" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           MOVE "Enter your choice (1 or 2): " TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(User-Input)(1:1) TO Request-Choice

           IF Request-Choice = "1"
               *> Accept: add to Established
               OPEN EXTEND EstablishedFile
               MOVE SPACES TO Established-Record-Line
               STRING Request-Sender DELIMITED BY SIZE
                      "|" DELIMITED BY SIZE
                      Request-Recipient DELIMITED BY SIZE
                      INTO Established-Record-Line
               WRITE Established-Record-Line
               CLOSE EstablishedFile
               MOVE "Connection accepted!" TO Message-Text
               PERFORM WRITE-AND-DISPLAY
           ELSE
               IF Request-Choice = "2"
                   MOVE "Connection rejected." TO Message-Text
                   PERFORM WRITE-AND-DISPLAY
               ELSE
                   MOVE "Invalid choice. Request kept pending." TO Message-Text
                   PERFORM WRITE-AND-DISPLAY
               END-IF
           END-IF

           *> Remove the processed request from Connections.txt
           IF Request-Choice = "1" OR Request-Choice = "2"
               OPEN INPUT ConnectionsFile
               OPEN OUTPUT TempConnectionsFile
               PERFORM UNTIL 1 = 0
                   READ ConnectionsFile INTO Connection-Record-Line
                       AT END EXIT PERFORM
                       NOT AT END
                           UNSTRING Connection-Record-Line
                               DELIMITED BY "|"
                               INTO Conn-Sender-WS
                                    Conn-Recipient-WS
                           *> Keep all unless this is the processed request
                           IF NOT ( FUNCTION TRIM(Conn-Sender-WS) = Request-Sender
                                    AND FUNCTION TRIM(Conn-Recipient-WS) = Request-Recipient )
                               MOVE Connection-Record-Line TO Temp-Connection-Record-Line
                               WRITE Temp-Connection-Record-Line
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE ConnectionsFile
               CLOSE TempConnectionsFile

               *> Copy back to ConnectionsFile
               OPEN INPUT TempConnectionsFile
               OPEN OUTPUT ConnectionsFile
               PERFORM UNTIL 1 = 0
                   READ TempConnectionsFile INTO Temp-Connection-Record-Line
                       AT END EXIT PERFORM
                       NOT AT END
                           MOVE Temp-Connection-Record-Line TO Connection-Record-Line
                           WRITE Connection-Record-Line
                   END-READ
               END-PERFORM
               CLOSE TempConnectionsFile
               CLOSE ConnectionsFile
           END-IF

           MOVE "-----------------------------------" TO Message-Text
           PERFORM WRITE-AND-DISPLAY
       END-PERFORM

       MOVE "All pending requests have been processed." TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       EXIT SECTION.


VIEW-CONNECTIONS SECTION.
       MOVE "---- Your Connections ----" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE 'N' TO Found-Flag

       *> Search EstablishedFile for connections involving current user
       OPEN INPUT EstablishedFile
       PERFORM UNTIL 1 = 0
           READ EstablishedFile INTO Established-Record-Line
               AT END EXIT PERFORM
               NOT AT END
                   UNSTRING Established-Record-Line
                       DELIMITED BY "|"
                       INTO Conn-Sender-WS
                            Conn-Recipient-WS

                   *> Check if current user is either sender or recipient
                   IF FUNCTION TRIM(Conn-Sender-WS) = FUNCTION TRIM(Current-Username)
                       MOVE 'Y' TO Found-Flag
                       MOVE Conn-Recipient-WS TO Conn-Sender-WS
                       PERFORM DISPLAY-CONNECTED-USER
                   ELSE IF FUNCTION TRIM(Conn-Recipient-WS) = FUNCTION TRIM(Current-Username)
                       MOVE 'Y' TO Found-Flag
                       *> Conn-Sender-WS already contains the connected user
                       PERFORM DISPLAY-CONNECTED-USER
                   END-IF
           END-READ
       END-PERFORM
       CLOSE EstablishedFile

       IF Found-Flag = 'N'
           MOVE "You have no connections yet." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
       END-IF
       EXIT SECTION.

DISPLAY-CONNECTED-USER SECTION.
       *> Look up the connected user's profile to show name/university
       OPEN INPUT ProfilesFile
       MOVE 'N' TO Found-Flag
       PERFORM UNTIL 1 = 0
           READ ProfilesFile
               AT END EXIT PERFORM
               NOT AT END
                   IF FUNCTION TRIM(Prof-Username) = FUNCTION TRIM(Conn-Sender-WS)
                       MOVE 'Y' TO Found-Flag
                       MOVE SPACES TO Message-Text
                       MOVE 1 TO Ptr
                       STRING "• " DELIMITED BY SIZE
                              FUNCTION TRIM(Prof-FirstName) DELIMITED BY SIZE
                              " " DELIMITED BY SIZE
                              FUNCTION TRIM(Prof-LastName) DELIMITED BY SIZE
                              " (" DELIMITED BY SIZE
                              FUNCTION TRIM(Prof-University) DELIMITED BY SIZE
                              ", " DELIMITED BY SIZE
                              FUNCTION TRIM(Prof-Major) DELIMITED BY SIZE
                              ")" DELIMITED BY SIZE
                              INTO Message-Text
                              WITH POINTER Ptr
                       PERFORM WRITE-AND-DISPLAY
                       EXIT PERFORM
                   END-IF
           END-READ
       END-PERFORM
       CLOSE ProfilesFile

       *> If no profile found, display username only
       IF Found-Flag = 'N'
           MOVE SPACES TO Message-Text
           MOVE 1 TO Ptr
           STRING "• " DELIMITED BY SIZE
                  FUNCTION TRIM(Conn-Sender-WS) DELIMITED BY SIZE
                  INTO Message-Text
                  WITH POINTER Ptr
           PERFORM WRITE-AND-DISPLAY
       END-IF

       EXIT SECTION.

DISPLAY-REQUEST-FROM SECTION.
       *> Look up the sender's profile to show name if available
       *> Use Request-Sender if available, otherwise use Conn-Sender-WS
       OPEN INPUT ProfilesFile
       MOVE 'N' TO Found-Flag
       PERFORM UNTIL 1 = 0
           READ ProfilesFile
               AT END EXIT PERFORM
               NOT AT END
                   IF (Request-Sender NOT = SPACES AND
                       FUNCTION TRIM(Prof-Username) = FUNCTION TRIM(Request-Sender))
                       OR
                      (Request-Sender = SPACES AND
                       FUNCTION TRIM(Prof-Username) = FUNCTION TRIM(Conn-Sender-WS))
                       MOVE 'Y' TO Found-Flag
                       MOVE SPACES TO Message-Text
                       MOVE 1 TO Ptr
                       STRING "Request from: " DELIMITED BY SIZE
                              FUNCTION TRIM(Prof-FirstName) DELIMITED BY SIZE
                              " " DELIMITED BY SIZE
                              FUNCTION TRIM(Prof-LastName) DELIMITED BY SIZE
                              INTO Message-Text
                              WITH POINTER Ptr
                       PERFORM WRITE-AND-DISPLAY
                       EXIT PERFORM
                   END-IF
           END-READ
       END-PERFORM
       CLOSE ProfilesFile

       *> If no profile found, display username only
       IF Found-Flag = 'N'
           MOVE SPACES TO Message-Text
           MOVE 1 TO Ptr
           IF Request-Sender NOT = SPACES
               STRING "Request from: " DELIMITED BY SIZE
                      FUNCTION TRIM(Request-Sender) DELIMITED BY SIZE
                      INTO Message-Text
                      WITH POINTER Ptr
           ELSE
               STRING "Request from: " DELIMITED BY SIZE
                      FUNCTION TRIM(Conn-Sender-WS) DELIMITED BY SIZE
                      INTO Message-Text
                      WITH POINTER Ptr
           END-IF
           PERFORM WRITE-AND-DISPLAY
       END-IF

       EXIT SECTION.

JOB-INTERNSHIP-SEARCH SECTION.
       MOVE "1. Post a Job/Internship" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "2. Browse Jobs/Internships (under construction)" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "3. Return to main menu" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       MOVE "Enter your choice (1-3): " TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO User-Input

       EVALUATE User-Input
           WHEN "1"
               PERFORM POST-JOB
           WHEN "2"
               MOVE "Browse Jobs/Internships is under construction." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
           WHEN "3"
                PERFORM SHOW-MAIN-MENU
           WHEN OTHER
               MOVE "Invalid choice. Returning to main menu." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               PERFORM SHOW-MAIN-MENU
       END-EVALUATE.
       EXIT SECTION.

POST-JOB SECTION.
       MOVE "--- Post a Job/Internship ---" TO Message-Text
       PERFORM WRITE-AND-DISPLAY

       *> Job Title
       MOVE "Enter Job Title (required):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-Job-Title
       IF WS-Job-Title = SPACES
           MOVE "Error: Job Title is required. Returning to main menu." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM SHOW-MAIN-MENU
           EXIT SECTION
       END-IF

       *> Description
       MOVE "Enter Job Description (required):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-Job-Description
       IF WS-Job-Description = SPACES
           MOVE "Error: Job Description is required. Returning to main menu." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM JOB-INTERNSHIP-SEARCH
           EXIT SECTION
       END-IF
       IF FUNCTION LENGTH(WS-Job-Description) > 200
           MOVE "Error: Job Description exceeds 200 characters. Returning to Job Search menu." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM JOB-INTERNSHIP-SEARCH
           EXIT SECTION
       END-IF

       *> Employer
       MOVE "Enter Employer Name (required):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-Job-Employer
       IF WS-Job-Employer = SPACES
           MOVE "Error: Employer Name is required. Returning to main menu." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM SHOW-MAIN-MENU
           EXIT SECTION
       END-IF

       *> Location
       MOVE "Enter Job Location (required):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-Job-Location
       IF WS-Job-Location = SPACES
           MOVE "Error: Job Location is required. Returning to main menu." TO Message-Text
           PERFORM WRITE-AND-DISPLAY
           PERFORM SHOW-MAIN-MENU
           EXIT SECTION
       END-IF

       *> Salary
       MOVE "Enter Salary (optional, enter NONE to skip):" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM READ-NEXT-INPUT
       MOVE FUNCTION TRIM(User-Input) TO WS-Job-Salary
       IF FUNCTION UPPER-CASE(WS-Job-Salary) = "NONE"
           MOVE SPACES TO WS-Job-Salary
       END-IF

       *> Save the job posting
       OPEN EXTEND JobsFile
       MOVE SPACES TO Job-Record
       STRING WS-Job-Title DELIMITED BY SIZE
              "|" DELIMITED BY SIZE
              WS-Job-Description DELIMITED BY SIZE
              "|" DELIMITED BY SIZE
              WS-Job-Employer DELIMITED BY SIZE
              "|" DELIMITED BY SIZE
              WS-Job-Location DELIMITED BY SIZE
              "|" DELIMITED BY SIZE
              WS-Job-Salary DELIMITED BY SIZE
              INTO Job-Record
       WRITE Job-Record
       CLOSE JobsFile

       MOVE "Job/Internship posted successfully!" TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       PERFORM JOB-INTERNSHIP-SEARCH

       EXIT SECTION.


*> HELPER SECTIONS
READ-NEXT-INPUT SECTION.
       READ InputFile INTO User-Input
           AT END
               MOVE "No input found." TO Message-Text
               PERFORM WRITE-AND-DISPLAY
               CLOSE InputFile
               STOP RUN
       END-READ

       MOVE FUNCTION TRIM(User-Input) TO Message-Text
       PERFORM WRITE-AND-DISPLAY
       EXIT SECTION.

WRITE-AND-DISPLAY SECTION.
       OPEN EXTEND OutputFile
       MOVE FUNCTION TRIM(Message-Text) TO Output-Line.
       DISPLAY FUNCTION TRIM(Message-Text)
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
