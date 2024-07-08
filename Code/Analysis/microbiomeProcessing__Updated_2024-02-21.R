
# Microbiome Processing ---------------------------------------------------
# Description: Runs microbiome analyses and saves results to list objects
#   - Uses tidyverse functions where possible
#   - Uses microViz functions where possible


# Naming Conventions ------------------------------------------------------
# Description: format for naming variables/functions consistently
#   - If multiple ords per `<>`, Capitalize subsequent words (eg. <moreThanOneWord>)

# VARIABLES: 
#   <highLevelVarType>.<subtype>.<subset>.<variables>.<Modifications>
#
#   dt.<subtype>.<subset>.<modifications>  # datatables
#   df.<subtype>.<subset>.<modifications> # dataframes
#   plot.<subset>.<y-var>.<x-vars...>  # plots/figures
#   table.<subset>.<y-var>.<x-vars...>  # tables
#   mod.<subtype>.<subset>.<y>.<x-vars...>  # models (lm, glm, etc.)
#   ps.<subtype>.<subset>  # phyloseq objects

# FUNCTIONS:
#   Should be descriptive, but short enough to know the main task
#   Should be as dynamic as possible, requiring minimal manual adjustment


# Set Environmental Variables ---------------------------------------------

## Analysis ID
analysis.ID <- paste0(
  "RoL-HeaterTrial_",  # Data subsetted? If so, how? "<name>_"
  Sys.Date(),  # Date of analysis
  "_MicroVizFilt"
)

## Diversity Methods

div.mtd <- list()
div.mtd[["alpha"]] <- c("shannon", "inverse_simpson", "observed", "phylogenetic")
div.mtd[["beta"]] <- c("bray", "canberra", "gunifrac")

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

ps.list[["All"]] <- readRDS(file.path(path.objects, "phyloseq__MicroViz_2024-01-24.rds"))



## Pre-process PS All ------------------------------------------------------

ps.list[["All"]] <- 
  ps.list[["All"]] %>%
  # For color scheme
    microViz::ps_mutate(
      Treatment = as.factor(Treatment),
      Temperature = as.factor(Temperature),
      DPE = as.factor(DPE),
      Temp.DPE = paste0(Temperature,"°C_", DPE, "DPE"), 
      Temp.Treat = paste0(Temperature,"°C_", Treatment),
      Treat.Temp.DPE = paste0(Treatment, "_", Temperature,"°C_", DPE, "DPE"),
    # Factorize
      Treat.Temp.DPE = as.factor(Treat.Temp.DPE),
      Treatment. = as.numeric(Treatment),
      Temperature. = as.numeric(Temperature),
      DPE. = as.numeric(DPE),
      Temp.DPE. = as.numeric(as.factor(Temp.DPE)),
      Temp.Treat. = as.numeric(as.factor(Temp.Treat)),
      Treat.Temp.DPE. = as.numeric(Treat.Temp.DPE)
    ) %>% 
    microViz::ps_mutate(Pathology.Results = ifelse(is.na(Pathology.Results), "negative", Pathology.Results)) %>%
    microViz::ps_mutate(Path.Res. = as.numeric(as.factor(Pathology.Results)),
                        Temp.Path = paste0(Temperature,"°C_", ifelse(Pathology.Results == "positive", "pos", "neg")),
                        Temp.Path. = as.numeric(as.factor(Temp.Path))) %>%
    microViz::ps_mutate(Total.Worm.Count = ifelse(is.na(Total.Worm.Count), 0, Total.Worm.Count)) %>% 
    microViz::tax_names2rank(colname = "unique") # Unique ASVs listed under "unique" column



# Extract sample data frames and table


# # Subset PS ----------------------------------------------------------------
# 
# 
# ## Initial -----------------------------------------------------------------

ps.list[["T0"]] <-
  ps.list[["All"]] %>%
  ps_filter(DPE == 0)


# ## Unexposed -----------------------------------------------------------------

ps.list[["Unexposed"]] <-
  ps.list[["All"]] %>%
  ps_filter((Treatment == "Control") | (Treatment == "Exposed" & DPE == 0))


# ## Final -----------------------------------------------------------------

ps.list[["TF"]] <-
  ps.list[["All"]] %>%
  ps_filter(DPE == 42)


# ## Exposed -----------------------------------------------------------------

ps.list[["Exposed"]] <-
  ps.list[["All"]] %>%
  ps_filter((Treatment == "Exposed") | (Treatment == "Control" & DPE == 0))


# Results, Stats, etc. ---------------------------------------------------

## Alpha Diversity ---------------------------------------------------------

source(paste0(proj.path,"/Code/Analysis/AlphaDiversity_v2.R"))


## Beta Diversity ----------------------------------------------------------

source(paste0(proj.path,"/Code/Analysis/BetaDiversity_v2.R"))



## Dynamics Metrics ----------------------------------------------------------

source(paste0(proj.path,"/Code/Analysis/DynamicsMetrics_v2.R")) # Calculates volatility, velocity, and acceleration based on displacement


## Diff Abundance ----------------------------------------------------------

# source(paste0(proj.path,"/Code/Analysis/DiffAbund.R"))


## Infection ----------------------------------------------------------

# source(paste0(proj.path,"/Code/Analysis/Infection.R"))



# Session Info ------------------------------------------------------------

sessionInfo <- sessionInfo()

# Save ENV ----------------------------------------------------------------

# Save a snapshot of the environment
save_env(path.objects, ID = analysis.ID, extra_info = "microbiomeProcessing")
