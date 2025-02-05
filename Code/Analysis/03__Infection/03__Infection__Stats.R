# Replace start message
start_time <- Sys.time()
cat("Starting 03__Infection__Stats.R at", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

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


### INF DETECT ----------------------------------------------------------------

tmp.inf.data <- readxl::read_excel(file.path(path.data, "Raw/Connor Hot Fish RTF Primary No Controls Ver 2.xlsx")) %>%
  dplyr::rename(
    Path.Res.Comb = "Infect combine",
    Histo = "histo",
    Histo.Res = "histo result"
  ) %>%
  dplyr::mutate(
    
    Histo = case_when(
      Histo == "pos" ~ "positive",
      Histo == "neg" ~ "negative",
      .default = NA
    ),
    
    # Convert character columns to factors
    Histo = factor(Histo, levels = c("negative", "positive")),
    Temperature = factor(Temperature, levels = c(28, 32, 35)),
    DPE = factor(DPE, levels = c(0, 14, 21, 28, 42)),
    
    # Ensure numeric columns are properly typed
    Wet = as.numeric(Wet),
    
    # Create logical detection columns
    Wet.Detect = Wet > 0,
    Histo.Detect = Histo == "positive"
  ) %>%
  # Select and order relevant columns
  dplyr::select(Sample, Temperature, DPE, Wet, Histo, Histo.Res,
                Wet.Detect, Histo.Detect)



#### COUNTS ------------------------------------------------------------------

# Function to summarize detection counts by DPE
summarize_detection_counts <- function(data) {
  data %>%
    dplyr::group_by(Temperature, DPE) %>%
    dplyr::summarize(
      
      Histo_Positive = sum(Histo.Detect == TRUE, na.rm = TRUE),
      Wet_Positive = sum(Wet.Detect == TRUE, na.rm = TRUE),
      
      Histo_Negative = sum(Histo.Detect == FALSE, na.rm = TRUE),
      Wet_Negative = sum(Wet.Detect == FALSE, na.rm = TRUE),
      .groups = 'drop'
    )
}

# Create detection count summary table
detection_counts <- tmp.inf.data %>%
  summarize_detection_counts() %>%
  dplyr::group_by(Temperature) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Detection Method Comparison by DPE",
    subtitle = "Number of positive and negative detections for Wet and Histo methods"
  ) %>%
  gt::cols_label(
    DPE = "Days Post Exposure",
    Wet_Positive = "Wet Positive",
    Histo_Positive = "Histo Positive",
    Wet_Negative = "Wet Negative",
    Histo_Negative = "Histo Negative"
  )

# Store and display results
worm.stats[[tmp.resSubSection]][["Method.Counts"]][["Detection_Counts"]] <- detection_counts



#### MCNEMARS ----------------------------------------------------------------

# Function to perform McNemar's test for each Temperature and DPE combination

compare_methods_by_temp_dpe <- function(data) {
  results <- data %>%
    dplyr::group_by(Temperature, DPE) %>%
    dplyr::summarize(
      # Create 2x2 contingency table
      Histo_Only = sum(!Wet.Detect & Histo.Detect, na.rm = TRUE),
      Wet_Only = sum(Wet.Detect & !Histo.Detect, na.rm = TRUE),
      Both_Positive = sum(Wet.Detect & Histo.Detect, na.rm = TRUE),
      Both_Negative = sum(!Wet.Detect & !Histo.Detect, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Perform McNemar's test
      mcnemar_test = list(stats::mcnemar.test(matrix(c(Both_Positive, Wet_Only, Histo_Only, Both_Negative), nrow = 2))),
      p.value = mcnemar_test$p.value,
      statistic = mcnemar_test$statistic
    ) %>%
    dplyr::select(-mcnemar_test)
  
  return(results)
}

# Perform comparisons and store results
set.seed(42)  # Set seed for reproducibility
method_comparison_results <- compare_methods_by_temp_dpe(tmp.inf.data)

# Create formatted table
method_comparison_table <- method_comparison_results %>%
  dplyr::group_by(Temperature) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Statistical Comparison of Detection Methods",
    subtitle = "McNemar's test comparing Wet and Histo methods by Temperature and DPE"
  ) %>%
  gt::cols_label(
    Temperature = "Temperature (°C)",
    DPE = "Days Post Exposure",
    Histo_Only = "Histo Only",
    Wet_Only = "Wet Only",
    Both_Positive = "Both Positive",
    Both_Negative = "Both Negative",
    p.value = "P-value",
    statistic = "Chi-squared"
  ) %>%
  gt::fmt_number(columns = c(p.value), decimals = 4) %>%
  gt::tab_footnote(
    footnote = "McNemar's test compares the discordant pairs (Wet Only vs Histo Only)",
    locations = cells_column_labels(columns = p.value)
  )

# Store and display results
worm.stats[[tmp.resSubSection]][["Method.Counts"]][["Method_Comparison_Stats"]] <- method_comparison_table



# Replace end message
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "secs")
cat("Completed 03__Infection__Stats.R at", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total execution time:", round(duration, 2), "seconds\n")



