
# 03__Infection_Stats -----------------------------------------------------------

tmp.psOBJ <- ps.list[["Exposed"]] 
tmp.resSubSection <- "Exposed"


## INFECTION ---------------------------------------------------------------


### TEMP --------------------------------------------------------------------



#### GLM ---------------------------------------------------------------------

worm.stats[[tmp.resSubSection]][["TEMP"]][["GLM.NB"]] <-
  tmp.psOBJ %>%
  # Convert phyloseq object into a tibble
  microViz::samdat_tbl() %>%
  # Run negative binomial GLM
  MASS::glm.nb(formula = Total.Worm.Count ~ Temperature)

### Table

worm.stats[[tmp.resSubSection]][["TEMP"]][["GLM.NB.Table"]] <-
  worm.stats[[tmp.resSubSection]][["TEMP"]][["GLM.NB"]] %>%
  
  # Create tidy dataframe
  tidy() %>%
  
  # Add significance indicators in a new column
  SigStars() %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "") %>%
  
  # Title/caption
  gt::tab_header(
    title = "GLM Results",
    subtitle = "glm.nb(Total.Worm.Count ~ Temperature); Exposed fish"
  )

#### ANOVA -------------------------------------------------------------------

worm.stats[[tmp.resSubSection]][["TEMP"]][["ANOVA"]] <-
  Anova(worm.stats[[tmp.resSubSection]][["TEMP"]][["GLM.NB"]], type = 2)

### Table

worm.stats[[tmp.resSubSection]][["TEMP"]][["ANOVA.Table"]] <-
  worm.stats[[tmp.resSubSection]][["TEMP"]][["ANOVA"]] %>%
  
  # Tidy
  tidy() %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA of GLM",
    subtitle = "ANOVA(GLM.NB(Total.Worm.Count ~ Temperature), type = 2); Exposed fish"
  )

#### Tukey -------------------------------------------------------------------

worm.stats[[tmp.resSubSection]][["TEMP"]][["TUKEY_GLM.NB"]] <-
  worm.stats[[tmp.resSubSection]][["TEMP"]][["GLM.NB"]] %>% 
  # Estimated marginal means (Least-squares means)
    emmeans::emmeans( ~ Temperature ) %>%
  # Contrasts and linear functions of EMMs, Tukey test
    emmeans::contrast(method = "pairwise", adjust = "tukey") %>% 
  # Clean up code
    tidy() %>%
  # Remove these columns if they exist in dataframe
    dplyr::select(-any_of(c("null.value", "data"))) %>%
  # Separate out the contrast column for later significance bar plotting
    tidyr::separate(contrast, c('group1', 'group2'), sep = " - ") %>%
  # Add a column ".y." for plotting worm counts
    dplyr::mutate(`.y.` = "Total.Worm.Count", .after = 1) %>%
  # Clean up cell values under the group1 and 2 columns to get rid of the prefix
    dplyr::mutate(group1 = (str_remove(group1, term)),
                  group2 = (str_remove(group2, term))) %>%
  # Ungroup
    dplyr::ungroup()


### Table 

worm.stats[[tmp.resSubSection]][["TEMP"]][["TUKEY_GLM.NB.Table"]] <-
  worm.stats[[tmp.resSubSection]][["TEMP"]][["TUKEY_GLM.NB"]] %>% 
    set_GT(var = "adj.p.value", group.by = "") %>%
    
    # Title/caption
    gt::tab_header(
      title = "Pairwise Tukey's HSD, p.adj: Dunnett",
      subtitle = "Tukey(Total.Worm.Count ~ Temperature); Exposed fish"
    )



### TEMP:DPE ----------------------------------------------------------------

#### GLM ---------------------------------------------------------------------

worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["GLM.NB"]] <-
  tmp.psOBJ %>%
  # Convert phyloseq object into a tibble
  microViz::samdat_tbl() %>%
  # Run negative binomial GLM
  MASS::glm.nb(formula = Total.Worm.Count ~ Temperature*DPE)

### Table

worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["GLM.NB.Table"]] <-
  worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["GLM.NB"]] %>%
  
  # Create tidy dataframe
  tidy() %>%
  
  # Add significance indicators in a new column
  SigStars() %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "") %>%
  
  # Title/caption
  gt::tab_header(
    title = "GLM Results",
    subtitle = "glm.nb(Total.Worm.Count ~ Temperature*DPE); Exposed fish"
  )

#### ANOVA -------------------------------------------------------------------

worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["ANOVA"]] <-
  Anova(worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["GLM.NB"]], type = 2)

### Table

worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["ANOVA.Table"]] <-
  worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["ANOVA"]] %>%
  
  # Tidy
  tidy() %>%
  
  # Create GT Table
  set_GT(var = "p.value", group.by = "") %>%
  
  # Title/caption
  gt::tab_header(
    title = "ANOVA of GLM",
    subtitle = "ANOVA(GLM.NB(Total.Worm.Count ~ Temperature*DPE), type = 2); Exposed fish"
  )

#### Tukey -------------------------------------------------------------------

worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]] <-
  tmp.psOBJ %>%
  # Convert phyloseq object into a tibble
  microViz::samdat_tbl() %>%
  # Ignore 0 DPE since all have no detectable worms since parasites were not exposed yet
  # dplyr::filter(DPE != 0) %>%
  # Convert DPE (time) into a factor for pairwise comparison
  dplyr::mutate(DPE = as.factor(DPE)) %>%
  # Group by DPE
  group_by(DPE) %>%
  # Nest the data for looping with map()
  nest(data = -DPE) %>%
  # Create a column called test to store the results of the GLM, emmeans, contrasts, and tidy
  dplyr::mutate(test = map(.x=data, 
                           ~ MASS::glm.nb(formula = Total.Worm.Count ~ -1 + Temperature, data = .x) %>% 
                             emmeans::emmeans( ~ Temperature ) %>%
                             emmeans::contrast(method = "pairwise", adjust = "tukey") %>% 
                             tidy() )) %>%
  # Unnest the test column into a tibble
  unnest(test) %>%
  # Remove any of the following columns if they exist
  dplyr::select(-any_of(c("null.value", "data"))) %>%
  # Separate out the contrast column for later significance bar plotting
  tidyr::separate(contrast, c('group1', 'group2'), sep = " - ") %>%
  # Add a column ".y." for plotting worm counts
  dplyr::mutate(`.y.` = "Total.Worm.Count", .after = 1) %>%
  # Clean up cell values under the group1 and 2 columns to get rid of the prefix
  dplyr::mutate(group1 = (str_remove(group1, term)),
                group2 = (str_remove(group2, term))) %>%
  # Ungroup
  dplyr::ungroup()


### Table 

worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB.Table"]] <-
  worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]] %>% 
  set_GT(var = "adj.p.value", group.by = "") %>%
  
  # Title/caption
  gt::tab_header(
    title = "Pairwise Tukey's HSD, p.adj: Dunnett",
    subtitle = "Tukey(Total.Worm.Count ~ Temperature*DPE); Exposed fish"
  )