# Temperature and Parasitic Infection Shape the Zebrafish Gut Microbiome

This repository contains [figures and tables](https://sielerjm.github.io/Sieler2024__ZF_Temperature_Parasite/Results_Overview.html), and the analysis code and data processing pipelines for:

Sieler et al. (2024) "Title of paper" *Journal Name*. DOI: [pending]

## Overview


![Experimental Design Overview](ExperimentalDesignSchematic.png)


## Repository Structure

    ├── Code/
    │   ├── Analysis/        # Statistical analysis scripts
    │   ├── Dada2/          # 16S rRNA sequence processing
    │   └── Plots/          # Figure generation scripts
    ├── Data/               # Raw and processed data files
    └── Results/           # Output files and figures

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
