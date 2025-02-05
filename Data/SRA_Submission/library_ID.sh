#!/bin/bash

# Input and output file paths
input_file="rownames.txt"
output_file="extracted_names.txt"

# Clear the output file if it exists
> "$output_file"

# Read the input file line by line
while IFS= read -r line; do
    # Debug: Print the current line being processed
    echo "Processing line: $line"
    
    # Extract the text between the last '-' and 'R1'
    # Modified pattern to be more inclusive
    extracted=$(echo "$line" | grep -oE '.*-([^-]+)_R1' | sed 's/.*-\([^-]*\)_R1.*/\1/')
    
    # Debug: Print the extracted value
    echo "Extracted: $extracted"
    
    # Append the extracted text to the output file if not empty
    if [ -n "$extracted" ]; then
        echo "$extracted" >> "$output_file"
    else
        echo "Warning: No match found in line: $line" >&2
    fi
done < "$input_file"

echo "Extraction complete. Results saved to $output_file"