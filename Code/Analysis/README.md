# Analysis Directory Overview

This directory contains statistical analyses for the zebrafish temperature and parasite exposure study. The analyses are organized into subdirectories by experimental condition and analysis type.

## Directory Structure

```
Analysis/
├── 01__Overview/                # Initial data exploration and summary statistics
│   └── Overview.Rmd             # R Markdown file for initial data exploration
├── 02__Temp_Unexposed/          # Temperature effects on unexposed fish
│   ├── 02__Temp_Unexposed__Methods.md  # Statistical methods documentation
│   └── 02__Temp_Unexposed__Stats.R     # R script for statistical analysis
├── 03__Infection/               # Infection analysis in exposed fish
│   ├── 03__Infection__Methods.md
│   └── 03__Infection__Stats.R
├── 04__Temp_Exposed/            # Temperature effects on exposed fish
│   ├── 04__Temp_Exposed__Methods.md
│   └── 04__Temp_Exposed__Stats.R
├── 05__TempPathInf_Exposed/     # Temperature and pathogen infection effects
│   ├── 05__TempPathInf_Exposed__Methods.md
│   └── 05__TempPathInf_Exposed__Stats.R
├── 06__Temp_Unexp_Exp/          # Temperature effects comparison (unexposed vs exposed)
│   ├── 06-1__Temp_PreExp__Methods.md
│   ├── 06-1__Temp_PreExp__Stats.R
│   ├── 06-2__Temp_PostExp__Methods.md
│   └── 06-2__Temp_PostExp__Stats.R
└── 07__TaxonAbund_Unexp_Exp/    # Taxon abundance analysis (unexposed vs exposed)
    ├── 07__TaxonAbund_Unexp_Exp__Methods.md
    └── 07__TaxonAbund_Unexp_Exp__Stats.R
```

## File Types

- **Methods.md**: Markdown files documenting the statistical methods used for each analysis
- **Stats.R**: R scripts containing the statistical analysis code
- **Overview.Rmd**: R Markdown file for initial data exploration

## Analysis Types

1. **Temperature Effects**: 
   - On unexposed fish (02__Temp_Unexposed)
   - On exposed fish (04__Temp_Exposed)
   - Comparison between unexposed and exposed (06__Temp_Unexp_Exp)

2. **Infection Analysis**:
   - Worm count and pathology effects (03__Infection)
   - Combined temperature and infection effects (05__TempPathInf_Exposed)

3. **Taxon Abundance**:
   - Differential abundance analysis between unexposed and exposed fish (07__TaxonAbund_Unexp_Exp)

## Statistical Approaches

- **Alpha Diversity**: GLM, ANOVA, Tukey's HSD
- **Beta Diversity**: CAPSCALE, PERMANOVA, Homogeneity of Dispersion
- **Infection Analysis**: Negative Binomial GLM
- **Taxon Abundance**: MaAsLin2 differential abundance analysis

## Key Packages Used

- `phyloseq`: Microbiome data handling
- `vegan`: Multivariate analyses
- `Maaslin2`: Differential abundance analysis
- `gt`: Table generation
- `tidyverse`: Data manipulation

