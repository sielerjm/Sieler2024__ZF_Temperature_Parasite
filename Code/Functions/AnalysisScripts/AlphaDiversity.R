# =============================================================================
# Alpha Diversity Calculations
# =============================================================================

# Add start message with timestamp
start_time <- Sys.time()
message("Starting AlphaDiversity.R at ", format(start_time, "%Y-%m-%d %H:%M:%S"))

# All Samples -----------------------------------------------------------------

cat("\n[INFO] Calculating alpha diversity scores: all samples\n")

# Calculate Shannon, Simpson, Richness, and Phylogenetic diversity metrics
# at Genus level, then normalize scores
ps.list[["All"]] <- 
  ps.list[["All"]] %>%
  microViz::ps_calc_diversity(
    rank = "Genus",        # Taxonomic rank
    index = "shannon",     # Diversity metric
    varname = "Shannon__Genus",  # Output column name
    exp = T                # Exponentiate the result
  ) %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus"
  ) %>%
  microViz::ps_calc_richness(
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>%
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm"))


# Unexposed Samples ----------------------------------------------------------

cat("\n[INFO] Calculating alpha diversity scores: unexposed samples\n")

ps.list[["Unexposed"]] <- 
  ps.list[["Unexposed"]] %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "shannon",
    varname = "Shannon__Genus",
    exp = T
  ) %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus"
  ) %>%
  microViz::ps_calc_richness(
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>%
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm"))


# Exposed Samples -----------------------------------------------------------

cat("\n[INFO] Calculating alpha diversity scores: exposed samples\n")

ps.list[["Exposed"]] <- 
  ps.list[["Exposed"]] %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "shannon",
    varname = "Shannon__Genus",
    exp = T
  ) %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus"
  ) %>%
  microViz::ps_calc_richness(
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>%
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm"))


# Pre-Exposed Samples -------------------------------------------------------

cat("\n[INFO] Calculating alpha diversity scores: pre-exposed samples\n")

ps.list[["PreExposed"]] <- 
  ps.list[["PreExposed"]] %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "shannon",
    varname = "Shannon__Genus",
    exp = T
  ) %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus"
  ) %>%
  microViz::ps_calc_richness(
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>%
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm"))


# Post-Exposed Samples -----------------------------------------------------

cat("\n[INFO] Calculating alpha diversity scores: post-exposed samples\n")

ps.list[["PostExposed"]] <- 
  ps.list[["PostExposed"]] %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "shannon",
    varname = "Shannon__Genus",
    exp = T
  ) %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus"
  ) %>%
  microViz::ps_calc_richness(
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>%
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm"))


# Final Timepoint ----------------------------------------------------------

cat("\n[INFO] Calculating alpha diversity scores: final timepoint\n")

ps.list[["TimeFinal"]] <- 
  ps.list[["TimeFinal"]] %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "shannon",
    varname = "Shannon__Genus",
    exp = T
  ) %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus"
  ) %>%
  microViz::ps_calc_richness(
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>%
  ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm"))


# Initial vs Final Timepoint -----------------------------------------------

cat("\n[INFO] Calculating alpha diversity scores: initial vs final timepoint\n")

ps.list[["InitialFinal"]] <- 
  ps.list[["InitialFinal"]] %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "shannon",
    varname = "Shannon__Genus",
    exp = T
  ) %>%
  microViz::ps_calc_diversity(
    rank = "Genus",
    index = "inverse_simpson",
    varname = "Simpson__Genus"
  ) %>%
  microViz::ps_calc_richness(
    rank = "Genus",
    varname = "Richness__Genus"
  ) %>%
  ps_calc_diversity.phy(
    varname = "Phylogenetic__Genus"
  ) %>%
  microViz::ps_mutate(across(contains("__Genus"), norm_scores, .names = "{.col}_norm"))


# Add end message with timestamp and duration
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "secs")
message("Completed AlphaDiversity.R at ", format(end_time, "%Y-%m-%d %H:%M:%S"))
message("Total execution time: ", round(duration, 2), " seconds")
