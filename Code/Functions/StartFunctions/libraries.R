# Libraries ---------------------------------------------------------------
#   Organized by category and alphabet


# Style Guide -------------------------------------------------------------
#   library(<package_name>)  # Brief description


# General -----------------------------------------------------------------
# library(data.table)  # Handle data tables
# library(flextable)  # Making pretty tables
library(gt)  # Making pretty tables, better than flextable, imo
# library(forcats)  # suite of useful tools that solve common problems with factors
library(knitr)  # For knitting documents to HTML or PDF formats
# library(lubridate)  # Help with time series data wrangling
# library(parallel)  # Parallel processing functions
# library(rcompanion)  # Allows us to run transformTukey()
# # library(readxl)  # Import Excel docs
# # library(RedoControl)  # Used to determine whether or not to rerun a code chunk
# library(tinytex)  # Knitting and using LaTeX formatting
# library(DT)  # microViz rec 
# library(scales)  # Normalizing values across a given scale
library(purrr)
library(furrr)


# Plotting ----------------------------------------------------------------
# library(cowplot)  # Plotting customization
library(ggplot2)  # For plotting pretty graphs
library(ggbeeswarm)  # organizes the jitters better on boxplots
# library(ggExtra)  # Additional ggplot settings
# library(ggrepel)  # Adds label buffering to labels so they don't overlap, geom_label_repel()
library(ggpubr) # Statistical plotting 
# # library(gridExtra)  # Miscellaneous Functions for "Grid" Graphics
# library(RColorBrewer)  # Making and using pretty color palettes
# library(rstatix) 
library(ComplexHeatmap)  # microViz rec
library(ggnewscale)  # Allows you to add an additional color/fill scale "new_scale_color()" https://github.com/eliocamp/ggnewscale


# Microbiome --------------------------------------------------------------
# library(Maaslin2)  # Differential abundance analysis
library(phyloseq)  # Microbiome analysis package
library(phyloseqCompanion)  # Helper functions for phyloseq
library(vegan)  # Ecological analysis
library(ANCOMBC)  # Differntial abundance, composition
library(microbiome) # microbiome analysis
library(mia) # microbiome analysis
# library(corncob)   # microViz rec 
# library(ggraph)  # microViz rec 
library(microViz) # Microbiome analysis 


# Statistics --------------------------------------------------------------
library(broom)  # Convert Statistical Objects into Tidy Tibbles
library(car)  # Allows us to run Anova()
# # library(caret)
# library(CoDaSeq)  # For CLR transformations
# library(compositions)  # Ancom dependency
library(easystats)  # Compilation of statistical packages
library(lme4)  # Linear mixed models
# # library(glmmTMB)  # Generalized Linear Mixed Models
# library(MASS)  # polr() - ordinal regression
# library(nortest)  # Allows us to run ad.test()
# library(recipes)  # Allows us to use tidy()
# library(zCompositions)  # For CLR transformations
library(emmeans)  # Post-hoc model comparisons
library(multcomp)  # Multiple comparison tests
library(brms)


# Ecological Modeling -----------------------------------------------------
# library(rstan)
# library(bridgesampling)
# library(broman)




# KEEP AS LAST LIBRARY TO LOAD --------------------------------------------
# Load last because of potential conflicting function names in previous libs
library(tidyverse)  # Making your code look pretty and tidy

cacheing=TRUE


# Install packages (placeholder)

# BiocManager::install(c("tidyverse"))


