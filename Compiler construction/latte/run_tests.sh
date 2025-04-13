#!/bin/bash

TARGET_BINARY="latc_llvm"

# Function to print colored text
print_color() {
    color="$1"
    text="$2"
    echo -e "\e[${color}m${text}\e[0m"
}
# Function to test "good" programs
test_good_program() {
    path="$1"
    expected_output_file="${path%.lat}.output"
    input_file="${path%.lat}.input"
    ll_file="${path%.lat}.ll"

    # Compile the .lat file
    print_color "34" "Compiling program: $path"
    print_color "34" "===================================================================="
    print_color "28" "$(cat "$path")"
    print_color "34" "===================================================================="

    ./$TARGET_BINARY "$path" > /dev/null 2>&1
    compile_status=$?

    # Check if compilation succeeded
    if [ $compile_status -ne 0 ]; then
        print_color "31" "Compilation failed with status code $compile_status."
        failed_tests+=("$path (compilation failed)")
        return
    fi

    # Ensure the .ll file was generated
    if [ ! -f "$ll_file" ]; then
        print_color "31" "Error: Expected LLVM file $ll_file not found after compilation."
        failed_tests+=("$path (missing .ll file)")
        return
    fi

    # Execute the .ll file using lli
    print_color "34" "\nExecuting generated .ll file: $ll_file"

    if [ -f "$input_file" ]; then
        print_color "33" "Using input file: $input_file"
        actual_output=$(lli "$ll_file" < "$input_file" 2>&1)
    else
        actual_output=$(lli "$ll_file" 2>&1)
    fi
    actual_status=$?

    # Print the output of the program
    print_color "34" "===================================================================="
    echo "$actual_output"
    print_color "34" "===================================================================="

    # Compare output
    if [ -f "$expected_output_file" ]; then
        expected_output=$(cat "$expected_output_file")
        if [ "$actual_output" == "$expected_output" ]; then
            print_color "32" "Output matches expected output."
            passed_tests+=("$path")
        else
            print_color "31" "Output does not match expected output."
            print_color "31" "Expected Output:"
            print_color "28" "$expected_output"
            print_color "31" "Actual Output:"
            print_color "28" "$actual_output"
            failed_tests+=("$path (output mismatch)")
        fi
    else
        print_color "33" "Warning: Expected output file $expected_output_file not found. Skipping output check."
        passed_tests+=("$path")
    fi

    echo -e "\n"
}


# Function to test "bad" programs
test_bad_program() {
    path="$1"

    # Compile the .lat file
    print_color "34" "Testing bad program: $path"
    print_color "34" "===================================================================="
    print_color "28" "$(cat "$path")"
    print_color "34" "===================================================================="

    ./$TARGET_BINARY "$path" > /dev/null 2>&1
    compile_status=$?

    # Check if the compiler fails as expected
    if [ $compile_status -ne 0 ]; then
        print_color "32" "Compiler failed as expected with status code $compile_status."
        passed_tests+=("$path")
    else
        print_color "31" "Compiler succeeded unexpectedly."
        failed_tests+=("$path (unexpected compilation success)")
    fi

    echo -e "\n"
}

# Main script
if [ $# -ne 1 ]; then
    echo "Usage: $0 <test_directory>"
    exit 1
fi

TEST_DIR="$1"

if [ ! -d "$TEST_DIR" ]; then
    echo "Error: $TEST_DIR is not a valid directory."
    exit 1
fi

make || { echo 'make failed'; exit 1; }
passed_tests=()
failed_tests=()

# Execute "good" tests
for file in "$TEST_DIR"/good/*.lat; do
    if [ -f "$file" ]; then
        test_good_program "$file"
    fi
done

# Execute "bad" tests
for file in "$TEST_DIR"/bad/*.lat; do
    if [ -f "$file" ]; then
        test_bad_program "$file"
    fi
done

# Print summary
echo -e "Passed Tests:"
for test in "${passed_tests[@]}"; do
    print_color "32" "- $test"
done

echo -e "\nFailed Tests:"
for test in "${failed_tests[@]}"; do
    print_color "31" "- $test"
done

# Print counts
num_passed=${#passed_tests[@]}
num_failed=${#failed_tests[@]}

print_color "32" "\n$num_passed tests passed."
print_color "31" "$num_failed tests failed."