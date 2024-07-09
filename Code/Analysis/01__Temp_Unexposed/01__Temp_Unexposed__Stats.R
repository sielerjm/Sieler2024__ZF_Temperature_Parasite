
# 01__Temp_Unexposed__Stats_Plots --------------------------------------------------

tmp.psOBJ <- ps.list[["Unexposed"]]
tmp.resSubSection <- "Unexposed"

## TEMP -------------------------------------------------------------

### Alpha -------------------------------------------------------------------

#### GLM ---------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["Temperature"]][["GLM"]] <- 
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey Test
  run_glm_models(formula_str = "Alpha.Score ~ Temperature")

### Table

alpha.stats[[tmp.resSubSection]][["Temperature"]][["GLM.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["Temperature"]][["GLM"]] %>%
  
  # Create a column called "Alpha.Metric" for each metric's GLM model
  purrr::imap(., ~tidy(.x) %>% mutate(Alpha.Metric = .y)) %>%
  
  # Combine the metric dataframes
  dplyr::bind_rows() %>%
  
  # Add significance indicators in a new column
  SigStars() %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "Alpha.Metric") 


#### ANOVA -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["Temperature"]][["ANOVA"]] <-
  run_glm_anova(alpha.stats[[tmp.resSubSection]][["Temperature"]][["GLM"]])

### Table

alpha.stats[[tmp.resSubSection]][["Temperature"]][["ANOVA.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["Temperature"]][["ANOVA"]] %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "Alpha.Metric") 


#### TUKEY -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["Temperature"]][["Tukey"]] <-
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey test on GLM models
  run_tukey_glm(., "Alpha.Score", "Alpha.Metric", c("Temperature"), 
                group_by_var = NULL) 

### Table

alpha.stats[[tmp.resSubSection]][["Temperature"]][["Tukey.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["Temperature"]][["Tukey"]] %>%
  
  # Combine the different alpha diversity metrics into one dataframe
  dplyr::bind_rows() %>%
  
  # Create the table
  dplyr::group_by(Alpha.Metric, term) %>%
  set_GT(var = "adj.p.value", group.by = "Alpha.Metric")


### Beta --------------------------------------------------------------------


#### Capscale


##### ANOVA -------------------------------------------------------------------



##### ADONIS ------------------------------------------------------------------




#### Dispersion --------------------------------------------------------------




### Taxon Abundance ---------------------------------------------------------


#### MaAsLin2 ----------------------------------------------------------------











