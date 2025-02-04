# Temperature and Parasitic Infection Shape the Zebrafish Gut Microbiome

This repository contains [figures and tables](https://sielerjm.github.io/Sieler2025__ZF_Temperature_Parasite/Results_Overview.html), and the analysis code and data processing pipelines for:

Sieler et al. (2025) "Title of paper" *Journal Name*. DOI: [pending]

## Overview


![Experimental Design Overview](ExperimentalDesignSchematic.png)


## Repository Structure

    ├── Code/
    │   ├── Analysis/        # Statistical analysis scripts
    │   ├── Dada2/          # 16S rRNA sequence processing
    │   └── Plots/          # Figure generation scripts
    ├── Data/               # Raw and processed data files
    └── Results/           # Output files and figures

## Running the Analysis

To reproduce the analysis and generate the manuscript figures and tables, you have two options:

**Note:** The complete analysis pipeline is documented in `starting_doc.Rmd`. This R Markdown file contains all the code needed to either run the full analysis from scratch or load pre-computed results. You can open this file in RStudio to interactively run the analysis.

### Option 1: Run Full Analysis from Scratch
This will run all analyses post-DADA2 processing (may take several minutes). In R:

```r
# Set seed for reproducibility
set.seed(42)

# Run complete analysis pipeline
source("MicrobiomeProcessing_2024-08-12.R")
```

### Option 2: Load Pre-computed Results
This loads saved R objects for quick access to results. Use this if you only need to generate figures and tables. In R:

```r
# Set seed for reproducibility
set.seed(42)

# Load most recent saved environment
latest_env <- fs::dir_info("Data/R_objects/Environment", regexp = "\\.RData$") %>% 
  dplyr::arrange(dplyr::desc(modification_time)) %>% 
  dplyr::slice(1) %>% 
  dplyr::pull(path)

load(latest_env)
```

After running either option, generate all figures and tables by running:

```r
# Generate manuscript figures and tables
rmarkdown::render("Results_Overview.Rmd")
```

## Analysis Pipeline

1. **Sequence Processing**:
   - Raw sequence processing using DADA2 (v2.0)
   - Quality filtering and chimera removal
   - ASV table generation

2. **Taxonomic Analysis**:
   - Taxonomic assignment using SILVA database (v138)
   - Abundance calculations and normalization

3. **Statistical Analysis**:
   - Temperature effect analysis using linear mixed models
   - Parasite infection impact assessment
   - Differential abundance testing (ANCOM, DESeq2)

4. **Visualization**:
   - Principal Coordinates Analysis (PCoA)
   - Heatmaps and taxonomic composition plots
   - Statistical result visualizations

## Data Availability

Raw sequencing data is available at [NCBI BioProject PRJNAXXXXXX](https://www.ncbi.nlm.nih.gov/bioproject/PRJNAXXXXXX). Processed data files are included in the `Data/` directory.

## Results

The complete analysis results, including all main figures, supplementary plots, and tables can be accessed through our [Results Overview Page](https://sielerjm.github.io/Sieler2024__ZF_Temperature_Parasite/Results_Overview.html).

## Requirements

- R version 4.3.3 or higher
- Required R packages listed in `Code/Functions/StartFunctions/libraries.R`
- For full analysis: Machine with at least 16GB RAM recommended

## Troubleshooting

* **Library Installation**: Ensure all required libraries are installed
* **Project Structure**: Verify `.Rproj` file is located in the top level of your project repository
* **Memory Issues**: The full analysis may require significant memory
* **Reporting Problems**: If you identify any issues with the code or analysis, please [open an issue](https://github.com/sielerjm/Sieler2024__ZF_Temperature_Parasite/issues) on GitHub

## Citation

```bibtex
@article{sieler2024temperature,
  title={Temperature and Parasitic Infection Shape the Zebrafish Gut Microbiome},
  author={Sieler Jr., Michael James et. al.},
  journal={Journal Name},
  volume={XX},
  number={XX},
  pages={XX--XX},
  year={2024},
  doi={XX.XXXX/XXXXXX}
}
```

## License

This work is licensed under the Oregon State University License. See the [LICENSE](LICENSE.md) file for details.

## Contact

For questions about the code or analysis, please [open an issue](https://github.com/sielerjm/Sieler2024__ZF_Temperature_Parasite/issues) or contact the corresponding author listed in the manuscript.
