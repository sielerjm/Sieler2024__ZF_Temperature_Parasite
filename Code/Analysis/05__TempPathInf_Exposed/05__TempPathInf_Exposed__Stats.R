
# 05__Temp_Exposed__Stats --------------------------------------------------

tmp.psOBJ <- ps.list[["Exposed"]]
tmp.resSubSection <- "Exposed"






## TEMP:PATH ---------------------------------------------------------------

### Alpha -------------------------------------------------------------------

#### GLM ---------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["GLM"]] <- 
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey Test
  run_glm_models(formula_str = "Alpha.Score ~ Temperature*Pathology.Results")

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["GLM.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["GLM"]] %>%
  
  # Create a column called "Alpha.Metric" for each metric's GLM model
  purrr::imap(., ~tidy(.x) %>% mutate(Alpha.Metric = .y)) %>%
  
  # Combine the metric dataframes
  dplyr::bind_rows() %>%
  
  # Add significance indicators in a new column
  SigStars() %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "Alpha.Metric")%>%
  
  # Title/caption
  gt::tab_header(
    title = "GLM Results",
    subtitle = "glm(Alpha.Score ~ Temperature*Pathology.Results); Exposed fish"
  )


#### ANOVA -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["ANOVA"]] <-
  run_glm_anova(alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["GLM"]])

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["ANOVA.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["ANOVA"]] %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "Alpha.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA of GLM",
    subtitle = "ANOVA(GLM(Alpha.Score ~ Temperature*Time), type = 2); Exposed fish"
  )


#### TUKEY -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["Tukey"]] <-
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey test on GLM models
  run_tukey_glm(., "Alpha.Score", "Alpha.Metric", c("Temperature", "Pathology.Results"), 
                group_by_var = "Pathology.Results") 

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["Tukey.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["Tukey"]] %>%
  
  # Combine the different alpha diversity metrics into one dataframe
  dplyr::bind_rows() %>%
  
  # Create the table
  dplyr::group_by(Alpha.Metric, term) %>%
  set_GT(var = "adj.p.value", group.by = "Alpha.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "Pairwise Tukey's HSD, p.adj: Dunnett",
    subtitle = "Tukey(Alpha.Score ~ Temperature*Time); Exposed fish"
  )

### Beta --------------------------------------------------------------------



#### Capscale ----------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["CAP.mod"]] <-
  run_capscale(ps.list[[tmp.resSubSection]], 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*Pathology.Results")



##### ADONIS ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["CAP.ADONIS"]] <-
  run_cap_adonis(ps.list[[tmp.resSubSection]],
                 dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                 formula_str = "dist ~ Temperature*Pathology.Results",
                 by.method = "terms") 

### Table

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["CAP.ADONIS.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["CAP.ADONIS"]] %>%
  set_GT(var = "p.value", group.by = "Beta.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ADONIS2",
    subtitle = "adonis2(Beta Distance ~ Temperature*Pathology.Results); Exposed fish"
  )


#### Dispersion --------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.model"]] <-
  run_BetaDispersion(dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                     var = c("Temp.Path"))

##### ANOVA ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.ANOVA"]] <-
  get_HoD_anova(betaDisper = beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.model"]],
                var = c("Temp.Path"))

### Table

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.ANOVA.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.ANOVA"]] %>%
  
  # Create the table
  dplyr::group_by(Beta.Metric) %>%
  set_GT(var = "p.value", group.by = "Beta.Metric")  %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA: Homogeneity of Dispersion",
    subtitle = "ANOVA(Beta Disperson ~ Temperature*Pathology.Results); Exposed fish"
  )

##### Tukey ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.Tukey"]] <-
  get_HoD_tukey(betaDisper = beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.model"]],
                var = c("Temp.Path")) 

### Table

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.Tukey.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["HoD.Tukey"]] %>%
  
  # Create the table
  dplyr::group_by(Beta.Metric) %>%
  set_GT(var = "adj.p.value", group.by = "Beta.Metric")  %>%
  
  # Title/caption
  gt::tab_header(
    title = "Tukey: Homogeneity of Dispersion",
    subtitle = "Tukey(Beta Disperson ~ Temperature*Pathology.Results); Exposed fish"
  )




## TEMP:WORM ---------------------------------------------------------------

### Alpha -------------------------------------------------------------------

#### GLM ---------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:WORM"]][["GLM"]] <- 
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey Test
  run_glm_models(formula_str = "Alpha.Score ~ Temperature*Total.Worm.Count")

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:WORM"]][["GLM.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:WORM"]][["GLM"]] %>%
  
  # Create a column called "Alpha.Metric" for each metric's GLM model
  purrr::imap(., ~tidy(.x) %>% mutate(Alpha.Metric = .y)) %>%
  
  # Combine the metric dataframes
  dplyr::bind_rows() %>%
  
  # Add significance indicators in a new column
  SigStars() %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "Alpha.Metric")%>%
  
  # Title/caption
  gt::tab_header(
    title = "GLM Results",
    subtitle = "glm(Alpha.Score ~ Temperature*Total.Worm.Count); Exposed fish"
  )


#### ANOVA -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:WORM"]][["ANOVA"]] <-
  run_glm_anova(alpha.stats[[tmp.resSubSection]][["TEMP:WORM"]][["GLM"]])

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:WORM"]][["ANOVA.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:WORM"]][["ANOVA"]] %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "Alpha.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA of GLM",
    subtitle = "ANOVA(GLM(Alpha.Score ~ Temperature*Time), type = 2); Exposed fish"
  )


#### TUKEY -------------------------------------------------------------------

# Cannot do tukey with worm counts because it is numeric variable

### Beta --------------------------------------------------------------------



#### Capscale ----------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:WORM"]][["CAP.mod"]] <-
  run_capscale(ps.list[[tmp.resSubSection]], 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*Total.Worm.Count")



##### ADONIS ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:WORM"]][["CAP.ADONIS"]] <-
  run_cap_adonis(ps.list[[tmp.resSubSection]],
                 dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                 formula_str = "dist ~ Temperature*Total.Worm.Count",
                 by.method = "terms") 

### Table

beta.stats[[tmp.resSubSection]][["TEMP:WORM"]][["CAP.ADONIS.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:WORM"]][["CAP.ADONIS"]] %>%
  set_GT(var = "p.value", group.by = "Beta.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ADONIS2",
    subtitle = "adonis2(Beta Distance ~ Temperature*Total.Worm.Count); Exposed fish"
  )


#### Dispersion --------------------------------------------------------------

# Cannot do tukey with worm counts because it is numeric variable

##### ANOVA ------------------------------------------------------------------

# Cannot do tukey with worm counts because it is numeric variable

##### Tukey ------------------------------------------------------------------

# Cannot do tukey with worm counts because it is numeric variable












