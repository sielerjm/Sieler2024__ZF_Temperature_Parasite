# =============================================================================
# Beta Diversity Calculations
# =============================================================================
#   - Calculate Distance Matrices
#   - Permanova
#   - Beta Dispersion


# Add start message with timestamp
start_time <- Sys.time()
message("Starting BetaDiversity.R at ", format(start_time, "%Y-%m-%d %H:%M:%S"))


## Variables, lists, etc.
beta.dist.mat <- list() # Saves distance matrix values here

## Set up parallel processing using all available cores minus one
future::plan(future::multisession, workers = (detectCores()-1))

# All Samples -----------------------------------------------------------------
cat("\n[INFO] Calculating beta diversity scores: all samples\n")

tmp.psOBJ <- ps.list[["All"]]
set.seed(42)
beta.dist.mat[["All"]] <- 
  furrr::future_map(diversity.method[["beta"]], function(x){
    set.seed(42)
    tmp.psOBJ %>%
      microViz::tax_transform(trans = "identity",
                    rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>%
      microViz::dist_calc(x, gunifrac_alpha = 0.5)
  }, .options = furrr::furrr_options(seed = TRUE)) %>%
  setNames(diversity.method[["beta"]])

# Unexposed Samples -----------------------------------------------------------
cat("\n[INFO] Calculating beta diversity scores: unexposed samples\n")

tmp.psOBJ <- ps.list[["Unexposed"]]
set.seed(42)
beta.dist.mat[["Unexposed"]] <- 
  furrr::future_map(diversity.method[["beta"]], function(x){
    set.seed(42)
    tmp.psOBJ %>%
      microViz::tax_transform(trans = "identity",
                    rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>%
      microViz::dist_calc(x, gunifrac_alpha = 0.5)
  }, .options = furrr::furrr_options(seed = TRUE)) %>%
  setNames(diversity.method[["beta"]])

# Exposed Samples -------------------------------------------------------------
cat("\n[INFO] Calculating beta diversity scores: exposed samples\n")

tmp.psOBJ <- ps.list[["Exposed"]]
set.seed(42)
beta.dist.mat[["Exposed"]] <- 
  furrr::future_map(diversity.method[["beta"]], function(x){
    set.seed(42)
    tmp.psOBJ %>%
      microViz::tax_transform(trans = "identity",
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>%
      microViz::dist_calc(x, gunifrac_alpha = 0.5)
  }, .options = furrr::furrr_options(seed = TRUE)) %>%
  setNames(diversity.method[["beta"]])

# Pre-Exposed Samples ---------------------------------------------------------
cat("\n[INFO] Calculating beta diversity scores: pre-exposed samples\n")

tmp.psOBJ <- ps.list[["PreExposed"]]
set.seed(42)
beta.dist.mat[["PreExposed"]] <- 
  furrr::future_map(diversity.method[["beta"]], function(x){
    set.seed(42)
    tmp.psOBJ %>%
      microViz::tax_transform(trans = "identity",
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>%
      microViz::dist_calc(x, gunifrac_alpha = 0.5)
  }, .options = furrr::furrr_options(seed = TRUE)) %>%
  setNames(diversity.method[["beta"]])

# Post-Exposed Samples --------------------------------------------------------
cat("\n[INFO] Calculating beta diversity scores: post-exposed samples\n")

tmp.psOBJ <- ps.list[["PostExposed"]]
set.seed(42)
beta.dist.mat[["PostExposed"]] <- 
  furrr::future_map(diversity.method[["beta"]], function(x){
    set.seed(42)
    tmp.psOBJ %>%
      microViz::tax_transform(trans = "identity",
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>%
      microViz::dist_calc(x, gunifrac_alpha = 0.5)
  }, .options = furrr::furrr_options(seed = TRUE)) %>%
  setNames(diversity.method[["beta"]])

# Final Timepoint -------------------------------------------------------------
cat("\n[INFO] Calculating beta diversity scores: final timepoint\n")

tmp.psOBJ <- ps.list[["TimeFinal"]]
set.seed(42)
beta.dist.mat[["TimeFinal"]] <- 
  furrr::future_map(diversity.method[["beta"]], function(x){
    set.seed(42)
    tmp.psOBJ %>%
      microViz::tax_transform(trans = "identity",
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>%
      microViz::dist_calc(x, gunifrac_alpha = 0.5)
  }, .options = furrr::furrr_options(seed = TRUE)) %>%
  setNames(diversity.method[["beta"]])

# Initial vs Final Timepoint --------------------------------------------------
cat("\n[INFO] Calculating beta diversity scores: initial vs final timepoint\n")

tmp.psOBJ <- ps.list[["InitialFinal"]]
set.seed(42)
beta.dist.mat[["InitialFinal"]] <- 
  furrr::future_map(diversity.method[["beta"]], function(x){
    set.seed(42)
    tmp.psOBJ %>%
      microViz::tax_transform(trans = "identity",
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>%
      microViz::dist_calc(x, gunifrac_alpha = 0.5)
  }, .options = furrr::furrr_options(seed = TRUE)) %>%
  setNames(diversity.method[["beta"]])


# Add end message with timestamp and duration
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "secs")
message("Completed BetaDiversity.R at ", format(end_time, "%Y-%m-%d %H:%M:%S"))
message("Total execution time: ", round(duration, 2), " seconds")
