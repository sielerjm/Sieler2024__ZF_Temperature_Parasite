# Temperature Effects on Pre- and Post-Exposed Fish - Statistical Methods

This directory contains analysis scripts and results for examining temperature effects on both pre-exposed and post-exposed fish. The statistical methods are consistent across both conditions.

## Analysis Files
- `06-1__Temp_PreExp__Stats.R`: Statistical analysis for pre-exposed fish
- `06-2__Temp_PostExp__Stats.R`: Statistical analysis for post-exposed fish
- `06-1__Temp_PreExp__Methods.md`: Methods documentation for pre-exposed analysis
- `06-2__Temp_PostExp__Methods.md`: Methods documentation for post-exposed analysis

## Statistical Methods

### Alpha Diversity Analysis

#### General Linear Models (GLM)
- **Model**: `Alpha.Score ~ Temperature*Treatment`
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
    cutCellNames(col = "Alpha.Metric", sep = "__") %>%
    run_glm_models(formula_str = "Alpha.Score ~ Temperature*Treatment")
  ```

#### ANOVA
- **Model**: ANOVA of GLM results
- **Implementation**:
  ```r
  run_glm_anova(alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["GLM"]]) %>%
    set_GT(var = "p.value", group.by = "Alpha.Metric")
  ```

#### Tukey's HSD Test
- **Model**: Pairwise comparisons of temperature and treatment effects
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
    cutCellNames(col = "Alpha.Metric", sep = "__") %>%
    run_tukey_glm(., "Alpha.Score", "Alpha.Metric", c("Temperature", "Treatment"), 
                  group_by_var = "Temperature")
  ```

### Beta Diversity Analysis

#### CAPSCALE (Constrained Analysis of Principal Coordinates)
- **Model**: `dist ~ Temperature*Treatment`
- **Implementation**:
  ```r
  run_capscale(tmp.psOBJ, 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*Treatment")
  ```

#### PERMANOVA (ADONIS2)
- **Model**: `dist ~ Temperature*Treatment`
- **Implementation**:
  ```r
  run_cap_adonis(tmp.psOBJ,
                 dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                 formula_str = "dist ~ Temperature*Treatment",
                 by.method = "terms")
  ```

#### Homogeneity of Dispersion (HoD)
- **Model**: Beta dispersion ~ Temperature*Treatment
- **Implementation**:
  ```r
  run_BetaDispersion(dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                     var = c("Temp.Treat"))
  ```

## Statistical Software and Packages
All analyses were performed in R using the following packages:
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
