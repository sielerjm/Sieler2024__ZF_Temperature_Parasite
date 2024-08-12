
# 06__Temp_Exposed__Stats --------------------------------------------------

tmp.psOBJ <- ps.list[["Exposed"]]
tmp.resSubSection <- "Exposed"


# Add Cluster Columns
tmp.psOBJ <- 
  tmp.psOBJ %>%
  
  # Group samples by Alpha Score
    ps_mutate(Cluster = if_else(
      Treatment == "Exposed" & Total.Worm.Count > 0,
      case_when(
        Simpson__Genus_norm <= 0.5 ~ "Low",
        Simpson__Genus_norm > 0.5 ~ "High",
        TRUE ~ "Other"
      ),
      "Other"
    ), .after = Treatment) %>%
    ps_mutate(Cluster = fct_relevel(factor(Cluster, levels = c("Other", "Low", "High")))) %>%
    ps_mutate(Cluster. = as.numeric(Cluster))




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
                group_by_var = "Temperature") 

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
  run_capscale(tmp.psOBJ, 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*Pathology.Results")



##### ADONIS ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:PATH"]][["CAP.ADONIS"]] <-
  run_cap_adonis(tmp.psOBJ,
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
  run_capscale(tmp.psOBJ, 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*Total.Worm.Count")



##### ADONIS ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:WORM"]][["CAP.ADONIS"]] <-
  run_cap_adonis(tmp.psOBJ,
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



## TEMP:CLUSTER ------------------------------------------------------------


### Counts -----------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:WORM"]][["CLUSTER"]][["TABLE"]] <-
  tmp.psOBJ %>%
  ps_mutate(Cluster = if_else(
    Treatment == "Exposed" & Total.Worm.Count > 0,
    case_when(
      Simpson__Genus_norm < .5 ~ "Low", # Samples below .5 alpha score are assigned "Low"
      Simpson__Genus_norm >= .5 ~ "High", # Samples above .5 alpha score are assigned "High"
      TRUE ~ "Other"
    ),
    "Other"
  )) %>%
  ps_mutate(Cluster = fct_relevel(factor(Cluster, levels = c("Other", "Low", "High")))) %>%
  samdat_tbl() %>%
  group_by(Cluster, Temperature) %>%
  count() %>%
  ungroup() %>%
  group_by(Cluster) %>%
  mutate(Percentage = round(n / sum(n) * 100, digits = 2)) %>%
  ungroup() %>%
  gt::gt()

### Alpha -------------------------------------------------------------------

#### GLM ---------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM"]] <- 
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey Test
  run_glm_models(formula_str = "Total.Worm.Count ~ Alpha.Score*Cluster", family_str = "gaussian")

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM"]] %>%
  
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
    subtitle = "glm(Total.Worm.Count ~ Alpha.Score*Cluster); Exposed fish"
  )


#### ANOVA -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["ANOVA"]] <-
  run_glm_anova(alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM"]])

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["ANOVA.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["ANOVA"]] %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "Alpha.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA of GLM",
    subtitle = "ANOVA(GLM(Total.Worm.Count ~ Alpha.Score*Cluster), type = 2); Exposed fish"
  )


#### TUKEY -------------------------------------------------------------------

alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["Tukey"]] <-
  tmp.psOBJ %>%
  
  # Convert phyloseq object into a dataframe and pivot longer by Alpha Metric and Score
  psObjToDfLong(div.score = "Alpha.Score", div.metric = "Alpha.Metric") %>%
  
  # Clean up cell value names by removing any strings including and after "__"
  cutCellNames(col = "Alpha.Metric", sep = "__") %>%
  
  # Run Tukey test on GLM models
  run_tukey_glm(., "Alpha.Score", "Alpha.Metric", c("Temperature", "Cluster"), 
                group_by_var = "Temperature") 

### Table

alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["Tukey.Table"]] <-
  alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["Tukey"]] %>%
  
  # Combine the different alpha diversity metrics into one dataframe
  dplyr::bind_rows() %>%
  
  # Create the table
  dplyr::group_by(Alpha.Metric, term) %>%
  set_GT(var = "adj.p.value", group.by = "Alpha.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "Pairwise Tukey's HSD, p.adj: Dunnett",
    subtitle = "Tukey(Total.Worm.Count ~ Alpha.Score*Cluster); Exposed fish"
  )


### Beta --------------------------------------------------------------------

#### Dist Matrix -------------------------------------------------------------
# Rerun dist Matrix to include Cluster

beta.dist.mat[["Exposed"]] <- # saves the results of this loop to a list
  furrr::future_map(diversity.method[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(diversity.method[["beta"]]) # assigns names of the beta methods to the list 

#### Capscale ----------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP.mod"]] <-
  run_capscale(tmp.psOBJ, 
               dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
               formula_str = "dist ~ Temperature*Cluster")



##### ADONIS ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP.ADONIS"]] <-
  run_cap_adonis(tmp.psOBJ,
                 dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                 formula_str = "dist ~ Temperature*Cluster",
                 by.method = "terms") 

### Table

beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP.ADONIS.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP.ADONIS"]] %>%
  set_GT(var = "p.value", group.by = "Beta.Metric") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ADONIS2",
    subtitle = "adonis2(Beta Distance ~ Temperature*Cluster); Exposed fish"
  )

#### Dispersion --------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.model"]] <-
  run_BetaDispersion(dist.matrix = beta.dist.mat[[tmp.resSubSection]], 
                     var = c("Cluster"))

##### ANOVA ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.ANOVA"]] <-
  get_HoD_anova(betaDisper = beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.model"]],
                var = c("Cluster"))

### Table

beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.ANOVA.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.ANOVA"]] %>%
  
  # Create the table
  dplyr::group_by(Beta.Metric) %>%
  set_GT(var = "p.value", group.by = "Beta.Metric")  %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA: Homogeneity of Dispersion",
    subtitle = "ANOVA(Beta Disperson ~ Temperature*Cluster); Exposed fish"
  )

##### Tukey ------------------------------------------------------------------

beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.Tukey"]] <-
  get_HoD_tukey(betaDisper = beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.model"]],
                var = c("Cluster")) 

### Table

beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.Tukey.Table"]] <-
  beta.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["HoD.Tukey"]] %>%
  
  # Create the table
  dplyr::group_by(Beta.Metric) %>%
  set_GT(var = "adj.p.value", group.by = "Beta.Metric")  %>%
  
  # Title/caption
  gt::tab_header(
    title = "Tukey: Homogeneity of Dispersion",
    subtitle = "Tukey(Beta Disperson ~ Temperature*Cluster); Exposed fish"
  )





