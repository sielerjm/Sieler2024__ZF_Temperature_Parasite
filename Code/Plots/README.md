# Plots Directory Overview

This directory contains code for generating all figures used in the manuscript and supplemental materials. The plots are organized into subdirectories by experimental condition and analysis type.

## Directory Structure

```
Plots/
├── 02__Temp_Unexposed/          # Temperature effects on unexposed fish
│   └── 02__Temp_Unexposed__Plots.R
├── 03__Infection/               # Infection analysis in exposed fish
│   └── 03__Infection__Plots.R
├── 04__Temp_Exposed/            # Temperature effects on exposed fish
│   └── 04__Temp_Exposed__Plots.R
├── 05__TempPathInf_Exposed/     # Temperature and pathogen infection effects
│   └── 05__TempPathInf_Exposed__Plots.R
├── 06__Temp_Unexp_Exp/          # Temperature effects comparison (unexposed vs exposed)
│   ├── 06-1__Temp_PreExp__Plots.R
│   └── 06-2__Temp_PostExp__Plots.R
└── 07__TaxonAbund_Unexp_Exp/    # Taxon abundance analysis (unexposed vs exposed)
    └── 07__TaxonAbund_Unexp_Exp__Plots.R
```

## File Types

- **Plots.R**: R scripts containing the plotting code for each analysis
- **README.md**: This overview document

## Plot Types

1. **Alpha Diversity Plots**:
   - Violin plots
   - Line plots over time
   - Statistical annotations

2. **Beta Diversity Plots**:
   - CAP (Canonical Analysis of Principal Coordinates) plots
   - Ellipse visualizations
   - Multiple distance metrics (Bray-Curtis, Canberra, Unifrac)

3. **Infection Plots**:
   - Worm count visualizations
   - Pathology results tables
   - Method comparison plots

4. **Taxon Abundance Plots**:
   - Heatmaps of significant taxa
   - Association tables
   - Taxonomic summary plots

## Key Packages Used

- `ggplot2`: Primary plotting system
- `gt`: Table generation
- `microViz`: Microbiome-specific visualizations
- `ggnewscale`: Multiple color scales
- `cowplot`: Plot arrangement and composition
- `ggpubr`: Statistical annotations

## Plotting Conventions

- Consistent color schemes for temperature groups
- Standardized axis labels and scales
- Uniform figure sizing for publication
- Statistical annotations using Tukey HSD or GLM results
- Multiple distance metrics for beta diversity analyses

## Output Formats

- High-resolution PNG files for publication
- PDF versions for vector graphics
- Interactive HTML versions for exploration
- GT tables for supplementary materials

## Version Control

- Plot scripts are version controlled with Git
- Each plot script includes a timestamp and execution time
- Output files are named consistently with analysis type and date