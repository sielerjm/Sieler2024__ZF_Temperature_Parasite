# Statistical Methods for Taxon Abundance Analysis in Exposed vs Unexposed Fish

## Differential Abundance Analysis

### MaAsLin2 Analysis
- **Model**: Taxon abundance ~ Temperature + DPE + Treatment + Cluster + Pathology + Worm Count
- **Implementation**:
  ```r
  # Create cluster variable based on alpha diversity
  tmp.psOBJ <- tmp.psOBJ %>%
    ps_mutate(Cluster = if_else(
      Treatment == "Exposed" & Total.Worm.Count > 0,
      case_when(
        Simpson__Genus_norm <= 0.5 ~ "Low",
        Simpson__Genus_norm > 0.5 ~ "High",
        TRUE ~ "Other"
      ),
      "Other"
    ))
  
  # Run MaAsLin2 analysis
  diffAbnd.stats[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]]
  ```

### Results Processing
- **Significance Threshold**: q-value < 0.05
- **Coefficient Direction**:
  - Positive: coef > 0
  - Negative: coef < 0
- **Implementation**:
  ```r
  diffAbnd.stats[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["output"]] %>%
    dplyr::filter(qval < 0.05) %>%
    dplyr::mutate(Significance = ifelse(coef > 0, "Positive", "Negative"))
  ```

## Visualization Methods

### Heatmap Generation
- **Color Scale**: Blue (-8.1) to White (0) to Red (+8.1)
- **Implementation**:
  ```r
  gradient_palette <- scales::col_numeric(
    palette = c("blue", "white", "red"), 
    domain = c(-8.1, 8.1)
  )
  
  apply_GT_tabStyles(gt_table, columns, coef_columns)
  ```

### Table Formatting
- **GT Table Features**:
  - Color-coded cells based on coefficient values
  - Hidden coefficient columns
  - Bold headers and row groups
  - Vertical lines between columns
  - Compact row spacing
- **Implementation**:
  ```r
  gt::gt() %>%
    apply_GT_tabStyles(., columns, coef_columns) %>%
    gt::cols_hide(columns = starts_with("coef_")) %>%
    gt::tab_options(data_row.padding = px(1.5))
  ```

## Supplementary Analyses

### Significant Associations Count
- **Per Variable**:
  ```r
  diffAbnd.plots[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["SUPP__SigCounts"]]
  ```
- **Per Taxonomic Level**:
  ```r
  diffAbnd.plots[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["SUPP__SigTaxonomy"]]
  ```

## Statistical Software and Packages
- All analyses were performed in R using the following packages:
  ```r
  library(phyloseq)  # Microbiome data handling
  library(Maaslin2)  # Differential abundance analysis
  library(gt)        # Table generation
  library(tidyverse) # Data manipulation
  library(scales)    # Color scaling
  ```

## Significance Testing
- All q-values were calculated using MaAsLin2's default method
- Significance levels were set at q < 0.05
- Seed was set to 42 prior to all randomization procedures
  ```r
  set.seed(42)  # Before any randomization procedure
  ``` 