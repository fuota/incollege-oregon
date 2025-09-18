# InCollege Project

## How to Test in batch
This project includes a set of test cases located in the `src/tests/input` directory.
For **Tester**: All the cases were prepared in advance. Please ensure the test input files are based on the requirements.
- Notes:
  - There're already 5 accounts created in the `Accounts.txt` file. `alice` and `user4` already have profiles created. Now add profiles for `bob`, `tammy` with variations depending on the test case. Leave `phineas` without a profile to test the "Find someone you know" feature.
  - For optional fields such as `About Me`, leave a blank line if you don't want to fill them. Don't skip the line.
  - For verification, we need to choose View Profile so that the profile details can be displayed in the output files.

To run all test cases and generate corresponding output files, execute the `test.py` script from the `src` directory:
Get Python if you don't have it installed already in this environment.
```bash
apt-get install python3
```
Then run the test script. This will read each input file from the `src/tests/input` directory, execute the `InCollege` program with that input, and save the output to the `src/tests/output` directory.
```bash
cd src
python3 test.py
```
## Project Overview

This is a **console-based COBOL application** that simulates the InCollege platform.
It supports:

- **Account creation** (with password validation and a maximum of 5 accounts).
- **Login** with unlimited attempts.
- **Initial navigation** options:
  - Create/Edit user profile.
  - View user profile.
  - Learn a new skill (with a submenu).
  - Find someone you know (search by full name).
- The program will terminate if the input file ends.


## ⚙️ How to Compile and Run

From the `src/InCollege.cob` file, Press Ctrl Shift B, then open the terminal.
For  running the program : ./bin/InCollege



