
# 03__Infection_Stats -----------------------------------------------------------

tmp.psOBJ <- ps.list[["All"]] %>% microViz::ps_filter(Treatment == "Exposed")
tmp.resSubSection <- "Exposed"


## INFECTION ---------------------------------------------------------------


### TEMP --------------------------------------------------------------------

formula_str.glmNB = as.formula("Total.Worm.Count ~ Temperature")
formula_str.tukey = as.formula("~ Temperature")

worm.stats[["TEMP"]][["TUKEY_GLM.NB"]] <-
  tmp.psOBJ %>%
  # Convert phyloseq object into a tibble
    microViz::samdat_tbl() %>%
  # Run negative binomial GLM
    MASS::glm.nb(formula = Total.Worm.Count ~ -1 + Temperature) %>% 
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


### TEMP:DPE ----------------------------------------------------------------

formula_str.glmNB = as.formula("Total.Worm.Count ~ -1 + DPE")
formula_str.tukey = as.formula("~ Temperature | DPE")

worm.stats[["TEMP:DPE"]][["TUKEY_GLM.NB"]] <-
  tmp.psOBJ %>%
  # Convert phyloseq object into a tibble
    microViz::samdat_tbl() %>%
  # Convert DPE (time) into a factor for pairwise comparison
    dplyr::mutate(DPE = as.factor(DPE)) %>%
  # Group by DPE
    group_by(DPE) %>%
  # Nest the data for looping with map()
    nest(data = -DPE) %>%
  # Create a column called test to store the results of the GLM, emmeans, contrasts, and tidy
    dplyr::mutate(test = map(.x=data, 
                             ~ MASS::glm.nb(formula = Total.Worm.Count ~ -1 + Temperature, data = .x, na.action = na.omit) %>% 
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
