import os
import shutil
import subprocess

base_dir = os.path.dirname(os.path.abspath(__file__))
input_dir = os.path.join(base_dir, "tests", "input")
output_dir = os.path.join(base_dir, "tests", "output")
in_college_input = os.path.join(base_dir, "InCollege-Input.txt")
exe_path = os.path.join(base_dir, ".." ,"bin", "InCollege")  # ./bin/InCollege

# Ensure output directory exists and is clean
if not os.path.exists(output_dir):
    os.makedirs(output_dir)
else:
    for file in os.listdir(output_dir):
        file_path = os.path.join(output_dir, file)
        if os.path.isfile(file_path):
            os.remove(file_path)

# Run tests
input_files = os.listdir(input_dir)

for file in input_files:
    test_input_path = os.path.join(input_dir, file)
    test_output_path = os.path.join(output_dir, file)

    # Replace InCollege-Input.txt with test input
    shutil.copyfile(test_input_path, in_college_input)

    # Run the executable and capture output
    try:
        result = subprocess.run(
            [exe_path],
            text=True,
            capture_output=True,
            check=True
        )
        with open(test_output_path, "w", encoding="utf-8") as f:
            f.write(result.stdout)
            if result.stderr:
                f.write("\n[stderr]\n" + result.stderr)
    except subprocess.CalledProcessError as e:
        print(f"Error running InCollege for {file}")
        with open(test_output_path, "w", encoding="utf-8") as f:
            f.write(e.stdout or "")
            if e.stderr:
                f.write("\n[stderr]\n" + e.stderr)

    print(f"Tested {file}")