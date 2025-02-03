# Statistical Methods for Temperature Effects on Unexposed Fish

## Alpha Diversity Analysis

### General Linear Models (GLM)
- **Model**: `Alpha.Score ~ Temperature`
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
    cutCellNames(col = "Alpha.Metric", sep = "__") %>%
    run_glm_models(formula_str = "Alpha.Score ~ Temperature")
  ```

### ANOVA
- **Model**: ANOVA of GLM results
- **Implementation**:
  ```r
  run_glm_anova(alpha.stats[[tmp.resSubSection]][["TEMP"]][["GLM"]]) %>%
    set_GT(var = "p.value", group.by = "Alpha.Metric")
  ```

### Tukey's HSD Test
- **Model**: Pairwise comparisons of temperature effects
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
    cutCellNames(col = "Alpha.Metric", sep = "__") %>%
    run_tukey_glm(., "Alpha.Score", "Alpha.Metric", c("Temperature"))
  ```

## Beta Diversity Analysis

### CAPSCALE (Constrained Analysis of Principal Coordinates)
- **Model**: `dist ~ Temperature`
- **Implementation**:
  ```r
  run_capscale(tmp.psOBJ, 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature")
  ```

### PERMANOVA (ADONIS2)
- **Model**: `dist ~ Temperature`
- **Implementation**:
  ```r
  run_cap_adonis(tmp.psOBJ,
                 dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                 formula_str = "dist ~ Temperature",
                 by.method = "terms")
  ```

### Homogeneity of Dispersion (HoD)
- **Model**: Beta dispersion ~ Temperature
- **Implementation**:
  ```r
  run_BetaDispersion(dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                     var = c("Temperature"))
  ```

## Temperature x DPE Interaction Analysis

### Alpha Diversity
- **Model**: `Alpha.Score ~ Temperature*DPE`
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
    cutCellNames(col = "Alpha.Metric", sep = "__") %>%
    run_glm_models(formula_str = "Alpha.Score ~ Temperature*DPE")
  ```

### Beta Diversity
- **Model**: `dist ~ Temperature*DPE`
- **Implementation**:
  ```r
  run_capscale(tmp.psOBJ, 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*DPE")
  ```

## Statistical Software and Packages
- All analyses were performed in R using the following packages:
  ```r
  library(phyloseq)  # Microbiome data handling
  library(vegan)     # Multivariate analyses
  library(gt)        # Table generation
  library(tidyverse) # Data manipulation
  library(broom)     # Model tidying
  ```

## Significance Testing
- All p-values were adjusted for multiple comparisons using Dunnett's method
- Significance levels were set at α = 0.05
- Seed was set to 42 prior to all randomization procedures
  ```r
  set.seed(42)  # Before any randomization procedure
  ``` 