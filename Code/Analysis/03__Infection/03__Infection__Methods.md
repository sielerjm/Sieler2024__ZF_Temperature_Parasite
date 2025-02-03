# Statistical Methods for Infection Analysis in Exposed Fish

## Worm Count Analysis

### Negative Binomial Generalized Linear Models (GLM.NB)

#### Temperature Effects
- **Model**: `Total.Worm.Count ~ Temperature`
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%
    MASS::glm.nb(formula = Total.Worm.Count ~ Temperature)
  ```

#### Temperature x DPE Interaction
- **Model**: `Total.Worm.Count ~ Temperature*DPE`
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%
    MASS::glm.nb(formula = Total.Worm.Count ~ Temperature*DPE)
  ```

### ANOVA
- **Model**: Type II ANOVA of GLM.NB results
- **Implementation**:
  ```r
  Anova(worm.stats[[tmp.resSubSection]][["TEMP"]][["GLM.NB"]], type = 2)
  ```

### Tukey's HSD Test
- **Model**: Pairwise comparisons of temperature effects
- **Implementation**:
  ```r
  worm.stats[[tmp.resSubSection]][["TEMP"]][["GLM.NB"]] %>% 
    emmeans::emmeans(~ Temperature) %>%
    emmeans::contrast(method = "pairwise", adjust = "tukey")
  ```

#### Temperature x DPE Interaction
- **Model**: Pairwise comparisons within each DPE
- **Implementation**:
  ```r
  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%
    dplyr::mutate(DPE = as.factor(DPE)) %>%
    group_by(DPE) %>%
    nest(data = -DPE) %>%
    dplyr::mutate(test = map(.x=data, 
                           ~ MASS::glm.nb(formula = Total.Worm.Count ~ -1 + Temperature, data = .x) %>% 
                             emmeans::emmeans(~ Temperature) %>%
                             emmeans::contrast(method = "pairwise", adjust = "tukey")))
  ```

## Table Generation

### GLM Results Tables
- **Implementation**:
  ```r
  worm.stats[[tmp.resSubSection]][["TEMP"]][["GLM.NB"]] %>%
    tidy() %>%
    SigStars() %>%
    set_GT(var = "p.value", group.by = "")
  ```

### ANOVA Tables
- **Implementation**:
  ```r
  worm.stats[[tmp.resSubSection]][["TEMP"]][["ANOVA"]] %>%
    tidy() %>%
    set_GT(var = "p.value", group.by = "")
  ```

### Tukey Tables
- **Implementation**:
  ```r
  worm.stats[[tmp.resSubSection]][["TEMP"]][["TUKEY_GLM.NB"]] %>% 
    set_GT(var = "adj.p.value", group.by = "")
  ```

## Statistical Software and Packages
- All analyses were performed in R using the following packages:
  ```r
  library(phyloseq)    # Microbiome data handling
  library(microViz)    # Phyloseq data manipulation
  library(MASS)        # Negative binomial GLM
  library(emmeans)     # Post-hoc comparisons
  library(gt)          # Table generation
  library(tidyverse)   # Data manipulation
  library(broom)       # Model tidying
  ```

## Significance Testing
- All p-values were adjusted for multiple comparisons using Tukey's method
- Significance levels were set at α = 0.05
- Seed was set to 42 prior to all randomization procedures
  ```r
  set.seed(42)  # Before any randomization procedure
  ```

