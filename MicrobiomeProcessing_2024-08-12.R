# Microbiome Data Analysis Processing -------------------------------------


## Overview ----------------------------------------------------------------

# Description:
#   - This script calculates alpha, beta, and differential abundance scores
#   - This script should be ran prior to any statistical tests to generate objects used to store test results and plots
# 
# Input: 
#   - Phyloseq object 
# 
# Output:
#   - Creates a list of phyloseq objects for each analysis

# Add start message with timestamp
start_time <- Sys.time()
message("Starting MicrobiomeProcessing_2024-08-12.R at ", format(start_time, "%Y-%m-%d %H:%M:%S"))

## Set Environmental Variables ---------------------------------------------

cat("INITIALIZING ENVIRONMENT \n")

## Analysis ID
analysis.ID <- paste0(
  "Sieler2024__ZF_Temperature_Parasite__",  
  Sys.Date()  # Date of last processing
)

## Diversity Methods

diversity.method <- list()
diversity.method[["alpha"]] <- c("shannon", 
                                 "inverse_simpson", 
                                 "observed", 
                                 "phylogenetic")
diversity.method[["beta"]] <- c("bray", "canberra", "gunifrac")

## Stats/Plotting Variables

alpha.stats <- list() # save all stat results here
alpha.plots <- list() # save plots here

beta.stats <- list() # save all stat results here
beta.plots <- list() # save plots here

diffAbnd.stats <- list() # save all stat results here
diffAbnd.plots <- list() # save plots here

worm.stats <- list() # save all stat results here
worm.plots <- list() # save plots here

# Import Phyloseq Object --------------------------------------------------

## Create an empty list object to store ps objects
ps.list <- list()


### All ---------------------------------------------------------------------

cat("CREATING PHYLOSEQ OBJECTS \n")

ps.list[["All"]] <- readRDS(file.path(path.data, 
                                      "R_objects", 
                                      paste0("phyloseq__PostDADA2Cleaning_2024-07-08.rds"))) 



# Subset Phyloseq Object --------------------------------------------------


### Unexposed -----------------------------------------------------------------

ps.list[["Unexposed"]] <-
  ps.list[["All"]] %>%
  # If sample is a control sample, or is assigned exposed but sampled on 0 dpe
  ps_filter((Treatment == "Control") | (Treatment == "Exposed" & DPE == 0))


### Exposed -----------------------------------------------------------------

ps.list[["Exposed"]] <-
  ps.list[["All"]] %>%
  # If sample is exposed and not sampled on 0 dpe
  #   - All samples at 0 dpe were technically unexposed, so this removes any 
  #     potential confounding of non-exposure of "exposed" samples at 0 dpe
  ps_filter((Treatment == "Exposed") & DPE != 0)


### Pre-Exposed -----------------------------------------------------------------

ps.list[["PreExposed"]] <-
  # Any sample from 0 dpe is technically "unexposed" because "exposed" fish were 
  #   not exposed to parasites until after 0 dpe fecal sampling
  ps.list[["All"]] %>%
  ps_filter(DPE == 0)


### Post-Exposed -----------------------------------------------------------------

ps.list[["PostExposed"]] <-
  # Any sample after 0 dpe is post-exposed
  ps.list[["All"]] %>%
  ps_filter(DPE != 0)


### TimeFinal ---------------------------------------------------------------

ps.list[["TimeFinal"]] <-
  # Any sample after 0 dpe is post-exposed
  ps.list[["All"]] %>%
  ps_filter(DPE == 42)

### InitialFinal ---------------------------------------------------------------

ps.list[["InitialFinal"]] <-
  # Any sample after 0 dpe is post-exposed
  ps.list[["All"]] %>%
  ps_filter(DPE == 0 | DPE == 42)

# Diversity Scores --------------------------------------------------------


## Alpha -------------------------------------------------------------------

cat("CALCULATING ALPHA SCORES \n")

### Calculate Scores --------------------------------------------------------

source(paste0(proj.path,"/Code/Functions/AnalysisScripts/AlphaDiversity.R"))


## Beta -------------------------------------------------------------------

cat("CALCULATING BETA SCORES \n")

### Calculate Scores --------------------------------------------------------

source(paste0(proj.path,"/Code/Functions/AnalysisScripts/BetaDiversity.R"))



# Stats & Plots -----------------------------------------------------------


## Run stats ---------------------------------------------------------------

cat("RUNNING STATS SCRIPTS \n")

sourceFolder(paste0(proj.path,"/Code/Analysis"), T)

## Make plots ---------------------------------------------------------------

cat("RUNNING PLOT SCRIPTS \n")

sourceFolder(paste0(proj.path,"/Code/Plots"), T)

# Save Environment --------------------------------------------------------

## Save Stats Objects ------------------------------------------------------

cat("SAVING STATS OBJECTS \n")

saveRDS(alpha.stats, file = file.path(path.results, "Stats_Plots", "alpha_stats.rds"))
saveRDS(beta.stats, file = file.path(path.results, "Stats_Plots", "beta_stats.rds"))
saveRDS(worm.stats, file = file.path(path.results, "Stats_Plots", "worm_stats.rds"))
saveRDS(diffAbnd.stats, file = file.path(path.results, "Stats_Plots", "diffAbnd_stats.rds"))

## Save Plot Objects -------------------------------------------------------

cat("SAVING PLOT OBJECTS \n")

saveRDS(alpha.plots, file = file.path(path.results, "Stats_Plots", "alpha_plots.rds"))
saveRDS(beta.plots, file = file.path(path.results, "Stats_Plots", "beta_plots.rds"))
saveRDS(worm.plots, file = file.path(path.results, "Stats_Plots", "worm_plots.rds"))
saveRDS(diffAbnd.plots, file = file.path(path.results, "Stats_Plots", "diffAbnd_plots.rds"))


# Save ENV ----------------------------------------------------------------

cat("SAVING ENVIRONMENT \n")

sesh.info <- sessionInfo()

# Save a snapshot of the environment and session info
save_env(path.objects, ID = analysis.ID, extra_info = "microbiomeProcessing")

# Add end message with timestamp and duration
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "secs")
message("Completed MicrobiomeProcessing_2024-08-12.R at ", format(end_time, "%Y-%m-%d %H:%M:%S"))
message("Total execution time: ", round(duration, 2), " seconds")

