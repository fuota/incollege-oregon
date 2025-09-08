TEST FILE (.txt)                            PURPOSE

InCollege-Test-CreateUser-CreatePass8       Check username constraints, then create user with password exactly 8 chars long
InCollege-Test-CreatePass12                 Create user with password exactly 12 chars long
InCollege-Test-CreateUser3                  Create 3rd user
InCollege-Test-CreateUser4                  Create 4th user
InCollege-Test-CreateUser5                  Create 5th user
InCollege-Test-UsePass12-UseApp             Login into user with 12-char password, test all possible selections
InCollege-Test-ValidateLogin                Ensure cannot log into account with unknown username or incorrect password
InCollege-Test-ValidateUserLimit            Create 6th user, which is expected to fail if all other accounts created

NOTE: tests should be ran IN THIS ORDER!