
# Alpha Diversity Scores ---------------------------------------------------


## All ---------------------------------------------------------------------


ps.list[["All"]] <- # Saves to the phyloseq object
  ps.list[["All"]] %>% # phyloseq object we'll be using
  ps_calc_diversity( # calculates various alpha diversity metrics
    rank = "Genus", # What taxonomic rank do you want to calculate diversity metrics at
    index = "shannon", # What diversity metric to use
    varname = "Shannon__Genus", # Column name in sample data
    exp = T # exponentiate the result or not
  ) %>%
  ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus" # Column name in sample data
  ) %>%
  ps_calc_richness( # related to ps_calc_diversity, this calculates richness or "observed" values
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>% # Note: This is a helper function I made. You need to adjust function manually to change taxon rank (See: microViz_Helper.R)
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm")) # Runs normalization func over each diversity metric and creates new columns for each


## Unexposed ---------------------------------------------------------------------

ps.list[["Unexposed"]] <- # Saves to the phyloseq object
  ps.list[["Unexposed"]] %>% # phyloseq object we'll be using
  ps_calc_diversity( # calculates various alpha diversity metrics
    rank = "Genus", # What taxonomic rank do you want to calculate diversity metrics at
    index = "shannon", # What diversity metric to use
    varname = "Shannon__Genus", # Column name in sample data
    exp = T # exponentiate the result or not
  ) %>%
  ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus" # Column name in sample data
  ) %>%
  ps_calc_richness( # related to ps_calc_diversity, this calculates richness or "observed" values
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>% # Note: This is a helper function I made. You need to adjust function manually to change taxon rank (See: microViz_Helper.R)
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm")) # Runs normalization func over each diversity metric and creates new columns for each


## Exposed ---------------------------------------------------------------------

ps.list[["Exposed"]] <- # Saves to the phyloseq object
  ps.list[["Exposed"]] %>% # phyloseq object we'll be using
  ps_calc_diversity( # calculates various alpha diversity metrics
    rank = "Genus", # What taxonomic rank do you want to calculate diversity metrics at
    index = "shannon", # What diversity metric to use
    varname = "Shannon__Genus", # Column name in sample data
    exp = T # exponentiate the result or not
  ) %>%
  ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus" # Column name in sample data
  ) %>%
  ps_calc_richness( # related to ps_calc_diversity, this calculates richness or "observed" values
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>% # Note: This is a helper function I made. You need to adjust function manually to change taxon rank (See: microViz_Helper.R)
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm")) # Runs normalization func over each diversity metric and creates new columns for each


## Pre-Exposed ---------------------------------------------------------------------

ps.list[["PreExposed"]] <- # Saves to the phyloseq object
  ps.list[["PreExposed"]] %>% # phyloseq object we'll be using
  ps_calc_diversity( # calculates various alpha diversity metrics
    rank = "Genus", # What taxonomic rank do you want to calculate diversity metrics at
    index = "shannon", # What diversity metric to use
    varname = "Shannon__Genus", # Column name in sample data
    exp = T # exponentiate the result or not
  ) %>%
  ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus" # Column name in sample data
  ) %>%
  ps_calc_richness( # related to ps_calc_diversity, this calculates richness or "observed" values
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>% # Note: This is a helper function I made. You need to adjust function manually to change taxon rank (See: microViz_Helper.R)
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm")) # Runs normalization func over each diversity metric and creates new columns for each



## Post-Exposed ---------------------------------------------------------------------

ps.list[["PostExposed"]] <- # Saves to the phyloseq object
  ps.list[["PostExposed"]] %>% # phyloseq object we'll be using
  ps_calc_diversity( # calculates various alpha diversity metrics
    rank = "Genus", # What taxonomic rank do you want to calculate diversity metrics at
    index = "shannon", # What diversity metric to use
    varname = "Shannon__Genus", # Column name in sample data
    exp = T # exponentiate the result or not
  ) %>%
  ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus" # Column name in sample data
  ) %>%
  ps_calc_richness( # related to ps_calc_diversity, this calculates richness or "observed" values
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>% # Note: This is a helper function I made. You need to adjust function manually to change taxon rank (See: microViz_Helper.R)
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm")) # Runs normalization func over each diversity metric and creates new columns for each


