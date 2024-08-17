#!/bin/bash

# Function to print colored text
print_color() {
    color="$1"
    text="$2"
    echo -e "\e[${color}m${text}\e[0m"
}

# Function to execute program with filename argument
execute_program() {
    path="$1"
    expected_status="$2"

    # Execute program
    print_color "34" "Executing program: $path"
    print_color "34" "===================================================================="
    print_color "28" "$(cat "$path")"
    print_color "34" "===================================================================="
    print_color "34" "\nResult:"
    print_color "34" "===================================================================="
    ./emilia-lang-exe "$path"
    actual_status=$?
    print_color "34" "===================================================================="

    # Check status and print output accordingly
    if [ "$actual_status" -eq "$expected_status" ]; then
        print_color "32" "Status code: $actual_status (Expected)"
        passed_tests+=("$file")
    else
        print_color "31" "Status code: $actual_status (Unexpected)"
        failed_tests+=("$file")
    fi

    echo -e "\n"
}

# Main script
make || { echo 'make failed' ; exit 1; }
passed_tests=()
failed_tests=()

for file in codes/good/*; do
    if [ -f "$file" ]; then
        execute_program $file 0
    fi
done

for file in codes/bad/*; do
    if [ -f "$file" ]; then
        execute_program $file 1
    fi
done

echo -e "Passed Tests:"
for test in "${passed_tests[@]}"; do
    print_color "32" "- $test"
done
echo -e "\nFailed Tests:"
for test in "${failed_tests[@]}"; do
    print_color "31" "- $test"
done