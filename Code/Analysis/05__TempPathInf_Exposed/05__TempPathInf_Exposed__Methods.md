# Statistical Methods for Temperature and Pathogen Infection Effects on Exposed Fish

## Alpha Diversity Analysis

### General Linear Models (GLM)
- **Model**: `Alpha.Score ~ Temperature*Pathology.Results`
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
    cutCellNames(col = "Alpha.Metric", sep = "__") %>%
    run_glm_models(formula_str = "Alpha.Score ~ Temperature*Pathology.Results")
  ```

### ANOVA
- **Model**: ANOVA of GLM results
- **Implementation**:
  ```r
  run_glm_anova(alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["GLM"]]) %>%
    set_GT(var = "p.value", group.by = "Alpha.Metric")
  ```

### Tukey's HSD Test
- **Model**: Pairwise comparisons of temperature and pathology effects
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.MMetric") %>%
    cutCellNames(col = "Alpha.Metric", sep = "__") %>%
    run_tukey_glm(., "Alpha.Score", "Alpha.Metric", c("Temperature", "Pathology.Results"), 
                  group_by_var = "Temperature")
  ```

## Beta Diversity Analysis

### CAPSCALE (Constrained Analysis of Principal Coordinates)
- **Model**: `dist ~ Temperature*Pathology.Results`
- **Implementation**:
  ```r
  run_capscale(tmp.psOBJ, 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*Pathology.Results")
  ```

### PERMANOVA (ADONIS2)
- **Model**: `dist ~ Temperature*Pathology.Results`
- **Implementation**:
  ```r
  run_cap_adonis(tmp.psOBJ,
                 dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                 formula_str = "dist ~ Temperature*Pathology.Results",
                 by.method = "terms")
  ```

### Homogeneity of Dispersion (HoD)
- **Model**: Beta dispersion ~ Temperature*Pathology.Results
- **Implementation**:
  ```r
  run_BetaDispersion(dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                     var = c("Temp.Path"))
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