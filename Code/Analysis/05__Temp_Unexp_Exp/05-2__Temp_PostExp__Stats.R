
# 05-2__Temp_PreExp__Stats --------------------------------------------------

tmp.psOBJ <- ps.list[["PostExposed"]]
tmp.resSubSection <- "PostExposed"


## TEMP:TREAT ---------------------------------------------------------------

### Alpha -------------------------------------------------------------------

#### GLM ---------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["GLM"]] <- 
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey Test
  run_glm_models(formula_str = "Alpha.Score ~ Temperature*Treatment")

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["GLM.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["GLM"]] %>%
  
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
    subtitle = "glm(Alpha.Score ~ Temperature*Treatment); Post-Exposed fish"
  )


#### ANOVA -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["ANOVA"]] <-
  run_glm_anova(alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["GLM"]])

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["ANOVA.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["ANOVA"]] %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "Alpha.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA of GLM",
    subtitle = "ANOVA(GLM(Alpha.Score ~ Temperature*Treatment), type = 2); Post-Exposed fish"
  )


#### TUKEY -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["Tukey"]] <-
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey test on GLM models
  run_tukey_glm(., "Alpha.Score", "Alpha.Metric", c("Temperature", "Treatment"), 
                group_by_var = "Temperature") 

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["Tukey.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["Tukey"]] %>%
  
  # Combine the different alpha diversity metrics into one dataframe
  dplyr::bind_rows() %>%
  
  # Create the table
  dplyr::group_by(Alpha.Metric, term) %>%
  set_GT(var = "adj.p.value", group.by = "Alpha.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "Pairwise Tukey's HSD, p.adj: Dunnett",
    subtitle = "Tukey(Alpha.Score ~ Temperature*Treatment); Post-Exposed fish"
  )


### Beta --------------------------------------------------------------------



#### Capscale ----------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["CAP.mod"]] <-
  run_capscale(tmp.psOBJ, 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*Treatment")



##### ADONIS ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["CAP.ADONIS"]] <-
  run_cap_adonis(tmp.psOBJ,
                 dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                 formula_str = "dist ~ Temperature*Treatment",
                 by.method = "terms") 

### Table

beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["CAP.ADONIS.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["CAP.ADONIS"]] %>%
  set_GT(var = "p.value", group.by = "Beta.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ADONIS2",
    subtitle = "adonis2(Beta Distance ~ Temperature*Treatment); Post-Exposed fish"
  )


#### Dispersion --------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.model"]] <-
  run_BetaDispersion(dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                     var = c("Temp.Treat"))

##### ANOVA ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.ANOVA"]] <-
  get_HoD_anova(betaDisper = beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.model"]],
                var = c("Temp.Treat"))

### Table

beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.ANOVA.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.ANOVA"]] %>%
  
  # Create the table
  dplyr::group_by(Beta.Metric) %>%
  set_GT(var = "p.value", group.by = "Beta.Metric")  %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA: Homogeneity of Dispersion",
    subtitle = "ANOVA(Beta Disperson ~ Temperature); Post-Exposed fish"
  )

##### Tukey ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.Tukey"]] <-
  get_HoD_tukey(betaDisper = beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.model"]],
                var = c("Temp.Treat")) 

### Table

beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.Tukey.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["HoD.Tukey"]] %>%
  
  # Create the table
  dplyr::group_by(Beta.Metric) %>%
  set_GT(var = "adj.p.value", group.by = "Beta.Metric")  %>%
  
  # Title/caption
  gt::tab_header(
    title = "Tukey: Homogeneity of Dispersion",
    subtitle = "Tukey(Beta Disperson ~ Temperature*Treatment); Post-Exposed fish"
  )




