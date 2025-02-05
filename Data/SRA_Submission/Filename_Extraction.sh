#!/bin/bash

# Check if directory path is provided as first argument
if [ -z "$1" ]; then
    # Ask user for directory path, handling spaces and quotes
    read -p "Enter the directory path containing the files: " input_dir
else
    input_dir="$1"
fi

# Remove any surrounding quotes from the input while preserving internal spaces
input_dir=$(echo "$input_dir" | sed -e 's/^"//' -e 's/"$//' -e "s/^'//" -e "s/'$//")

# Verify the directory exists
if [ ! -d "$input_dir" ]; then
    echo "Error: Directory does not exist: $input_dir"
    exit 1
fi

# Initialize CSV file in the script's directory
output_csv="$(dirname "$0")/sample_filenames.csv"
echo "sample_name,filename,filename2" > "$output_csv"

# Find all files in the directory and process them
find "$input_dir" -type f | while read -r filepath; do
    # Extract filename from path
    filename=$(basename "$filepath")
    
    # Extract sample name using more portable regex
    if [[ "$filename" =~ _RTF[0-9]{3}_ ]]; then
        sample_name=$(echo "$filename" | grep -oE '_RTF[0-9]{3}_' | tr -d '_')
        
        # Check if this is R1 or R2 file
        if [[ "$filename" =~ _R1_ ]]; then
            r1_file="$filename"
            # Look for corresponding R2 file
            r2_file=$(echo "$filename" | sed 's/_R1_/_R2_/')
            r2_path="$input_dir/$r2_file"
            
            # If R2 file exists, write to CSV
            if [ -f "$r2_path" ]; then
                echo "$sample_name,$r1_file,$r2_file" >> "$output_csv"
                # Print the processed sample to screen
                echo "Processed sample: $sample_name with files: $r1_file and $r2_file"
            fi
        fi
    fi
done

echo "Processing complete. CSV file created at: $output_csv"
