
# Alpha Diversity Stats ---------------------------------------------------
#   - Calculate Distances
#   - Stats
#     - Tukey
#     - GLM
#     - ANOVA

## Global variables, lists, etc.
# alpha.stats <- list() # save all stat results here
# alpha.plots <- list() # save plots here


## Run purrr::map() in parallel
future::plan(future::multisession, workers = (detectCores()-1) )


# All ---------------------------------------------------------------------



## Calculate Distance ----------------------------------------------------------------


ps.list[["All"]] <- # Saves to the phyloseq object
  ps.list[["All"]] %>% # phyloseq object we'll be using
  ps_calc_diversity( # calculates various alpha diversity metrics
    rank = "Genus", # What taxonomic rank do you want to calculate diversity metrics at
    index = "shannon", # What diversity metric to use
    exp = T # exponentiate the result or not
  ) %>% ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson"
  ) %>% ps_calc_richness( # related to ps_calc_diversity, this calculates richness or "observed" values
    rank = "Genus",
  ) %>% ps_calc_diversity.phy() %>% # Note: This is a helper function I made. You need to adjust function manually to change taxon rank (See: microViz_Helper.R)
  ps_mutate(across(contains("_Genus"), norm_alpha_score.2, .names = "{.col}_norm")) # Runs normalization func over each diversity metric and creates new columns for each

# Unexposed ---------------------------------------------------------------------



## Calculate Distance ----------------------------------------------------------------


ps.list[["Unexposed"]] <- # Saves to the phyloseq object
  ps.list[["Unexposed"]] %>% # phyloseq object we'll be using
  ps_calc_diversity( # calculates various alpha diversity metrics
    rank = "Genus", # What taxonomic rank do you want to calculate diversity metrics at
    index = "shannon", # What diversity metric to use
    exp = T # exponentiate the result or not
  ) %>% ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson"
  ) %>% ps_calc_richness( # related to ps_calc_diversity, this calculates richness or "observed" values
    rank = "Genus",
  ) %>% ps_calc_diversity.phy() %>% # Note: This is a helper function I made. You need to adjust function manually to change taxon rank (See: microViz_Helper.R)
  ps_mutate(across(contains("_Genus"), norm_alpha_score.2, .names = "{.col}_norm")) # Runs normalization func over each diversity metric and creates new columns for each

# Exposed ---------------------------------------------------------------------



## Calculate Distance ----------------------------------------------------------------


ps.list[["Exposed"]] <- # Saves to the phyloseq object
  ps.list[["Exposed"]] %>% # phyloseq object we'll be using
  ps_calc_diversity( # calculates various alpha diversity metrics
    rank = "Genus", # What taxonomic rank do you want to calculate diversity metrics at
    index = "shannon", # What diversity metric to use
    exp = T # exponentiate the result or not
  ) %>% ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson"
  ) %>% ps_calc_richness( # related to ps_calc_diversity, this calculates richness or "observed" values
    rank = "Genus",
  ) %>% ps_calc_diversity.phy() %>% # Note: This is a helper function I made. You need to adjust function manually to change taxon rank (See: microViz_Helper.R)
  ps_mutate(across(contains("_Genus"), norm_alpha_score.2, .names = "{.col}_norm")) # Runs normalization func over each diversity metric and creates new columns for each

