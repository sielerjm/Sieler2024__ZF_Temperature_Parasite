#!/bin/bash

# Function to clean and validate directory paths
clean_path() {
    local path="$1"
    # Remove surrounding quotes while preserving internal spaces
    path=$(echo "$path" | sed -e 's/^"//' -e 's/"$//' -e "s/^'//" -e "s/'$//")
    echo "$path"
}

# Check if input and output directories are provided
if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Usage: $0 <input_directory> <output_directory>"
    echo "Or run without arguments to be prompted for directories"
    read -p "Enter the input directory containing the files: " input_dir
    read -p "Enter the output directory to copy files to: " output_dir
else
    input_dir="$1"
    output_dir="$2"
fi

# Clean the directory paths
input_dir=$(clean_path "$input_dir")
output_dir=$(clean_path "$output_dir")

# Verify the directories exist
if [ ! -d "$input_dir" ]; then
    echo "Error: Input directory does not exist: $input_dir"
    exit 1
fi

if [ ! -d "$output_dir" ]; then
    echo "Creating output directory: $output_dir"
    mkdir -p "$output_dir"
fi

# Get the path to the CSV file (assuming it's in the same directory as this script)
csv_file="$(dirname "$0")/sample_filenames.csv"

# Verify the CSV file exists
if [ ! -f "$csv_file" ]; then
    echo "Error: CSV file not found: $csv_file"
    exit 1
fi

# Read the CSV file and store filenames in a temporary file
temp_file=$(mktemp)
# Process CSV file, trim whitespace, and ensure proper line endings
tail -n +2 "$csv_file" | awk -F, '{print $2; print $3}' | sed 's/^[ \t]*//;s/[ \t]*$//' | tr -d '\r' > "$temp_file"

# Find and copy matching files
copied_count=0
find "$input_dir" -type f | while read -r filepath; do
    filename=$(basename "$filepath")
    
    # Check if filename exists in our list
    if grep -Fxq "$(echo "$filename" | tr -d '\r')" "$temp_file"; then
        # Copy the file to the output directory
        cp "$filepath" "$output_dir/"
        copied_count=$((copied_count + 1))
        echo "Copied: $filename"
    fi
done

# Clean up temporary file
rm "$temp_file"

echo "Copy complete. Total files copied: $copied_count"