# SRA Submission Documentation

This folder contains scripts and files related to the submission of FASTQ files to the NCBI Sequence Read Archive (SRA). Below is a description of each file and a step-by-step guide for the submission process.

## File Descriptions

- **sample_filenames.csv**: Contains the list of FASTQ filenames to be submitted
- **Copy_Files_From_CSV.sh**: Shell script to copy FASTQ files listed in sample_filenames.csv to a new directory
- **rownames.txt**: Contains extracted sample names from FASTQ filenames
- **extracted_names.txt**: Contains processed sample names for metadata preparation
- **Filename_Extraction.sh**: Shell script to extract sample names from FASTQ filenames
- **library_ID.sh**: Shell script to extract library IDs from FASTQ filenames

## Submission Workflow

1. **Locate Raw Data**
   - Identify the directory containing raw FASTQ files
   - Ensure metadata matches FASTQ file names

2. **Extract Filenames**
   - Run `Filename_Extraction.sh` to extract sample names from FASTQ files
   - Output will be saved in `rownames.txt`

3. **Process Sample Names**
   - Process `rownames.txt` to create `extracted_names.txt`
   - Use these names to create `sample_filenames.csv`

4. **Extract Library IDs**
   - Run `library_ID.sh` to extract library IDs from FASTQ filenames
   - These will be used in the metadata preparation

5. **Prepare Metadata**
   - Create Biosample template sheet (e.g., MIMARKS)
   - Create SRA metadata sheet
   - Ensure sample names match across all documents
   - If non-unique attributes exist, create a new column with unique identifiers

6. **Copy Files**
   - Run `Copy_Files_From_CSV.sh` to copy listed FASTQ files to a new directory
   - This directory will be used for SRA submission

7. **Submit to SRA**
   - Follow NCBI SRA submission process
   - Upload prepared FASTQ files and metadata
   - Complete submission through the SRA web interface

## Notes
- Always verify file paths in scripts before execution
- Double-check metadata consistency before submission
- The SRA submission process may require additional steps depending on the specific requirements of your project
