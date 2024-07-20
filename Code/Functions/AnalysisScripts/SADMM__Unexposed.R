
# Dynamics metrics -----------------------------------------------------------
#   - Displacement
#   - Volatility
#   - Velocity
#   - Acceleration
# Notes:
#   - Calculates Alpha and Beta metrics
#   - Version 2 calculates velocity and acceleration from displacement scores 


# Variables ---------------------------------------------------------------

cat("Instantiating variables")

# Instantiate Variables
metrics.dynamics <- c("Displacement", "Volatility", "Velocity", "Acceleration")

data.dynamics_Unexp <- list()

data.dynamics_Unexp[["Alpha"]] <- list()

data.dynamics_Unexp[["Alpha"]][["Metadata"]] <- list()
data.dynamics_Unexp[["Alpha"]][["Displacement"]] <- list()
data.dynamics_Unexp[["Alpha"]][["Volatility"]] <- list()
data.dynamics_Unexp[["Alpha"]][["Velocity"]] <- list()
data.dynamics_Unexp[["Alpha"]][["Acceleration"]] <- list()

data.dynamics_Unexp[["Beta"]] <- list()

data.dynamics_Unexp[["Beta"]][["Metadata"]] <- list()
data.dynamics_Unexp[["Beta"]][["Displacement"]] <- list()
data.dynamics_Unexp[["Beta"]][["Volatility"]] <- list()
data.dynamics_Unexp[["Beta"]][["Velocity"]] <- list()
data.dynamics_Unexp[["Beta"]][["Acceleration"]] <- list()



# Data --------------------------------------------------------------------

## Alpha -------------------------------------------------------------------

data.dynamics_Unexp[["Alpha"]][["Metadata"]] <-
  ps.list$Unexposed %>%
  samdat_tbl() %>%
  # select(!contains("_norm")) %>%
  # pivot_longer(cols = contains("_Genus"), names_to = "Alpha.Metric", values_to = "Alpha.Score") %>%
  pivot_longer(cols = contains("_norm"), names_to = "Alpha.Metric", values_to = "Alpha.Score") %>%
  ungroup()

## Beta -------------------------------------------------------------------

data.dynamics_Unexp[["Beta"]][["Metadata"]] <-
  data.beta.list[["Unexposed"]] %>%
  ungroup()

# Displacement ------------------------------------------------------------
# Notes:
#   - Calculates difference in position between two time points

cat("Calculating Displacement scores: Initial")

## Initial  ----------------------------------------------------------------
# Notes:
#   Here, we calculate displacement by subtracting the median diversity found at the 
#   initial time point of all 28°C control samples by the current sample's diversity score. 
#   This assumes that the reference point for gut microbiome normality is at the initial time point.

### Alpha -------------------------------------------------------------------

cat("Calculating Displacement scores: Alpha")

# Step 1: Filter initial samples at 28°C and calculate their median diversity score
data.dynamics_Unexp[["Alpha"]][["Displacement"]]$median_diversity_initial_28C <-
  data.dynamics_Unexp[["Alpha"]][["Metadata"]] %>%
  
  # Initial samples at 28°C are all technically "controls" because they are unexposed
  dplyr::filter(DPE == 0, Temperature == 28) %>%  
  
  # Group by alpha diversity metric 
  dplyr::group_by(Alpha.Metric) %>%
  
  # Calculate median diversity score
  dplyr::summarize(Ref.Score = median(Alpha.Score))


# Step 2: Calculate displacement for each sample
data.dynamics_Unexp[["Alpha"]][["Displacement"]]$init28 <-
  data.dynamics_Unexp[["Alpha"]][["Metadata"]] %>%
  
  # Merge dataframe with previously calculated median scores to data frame with diversity scores and metadata
  dplyr::left_join(data.dynamics_Unexp[["Alpha"]][["Displacement"]]$median_diversity_initial_28C, by = "Alpha.Metric") %>%
  
  # Calculate displacement (`Displacement = Xn - Xi`)
  dplyr::mutate(Displacement = Alpha.Score - Ref.Score) %>%
  
  # Simplify dataframe to only contain relevant columns for analysis and plotting
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Alpha.Metric, Alpha.Score,Ref.Score, Displacement) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) 

### Beta -------------------------------------------------------------------

cat("Calculating Displacement scores: Beta")

# Step 1: Filter initial samples at 28°C and calculate their median diversity score
data.dynamics_Unexp[["Beta"]][["Displacement"]]$median_diversity_initial_28C <-
  data.dynamics_Unexp[["Beta"]][["Metadata"]] %>%
  dplyr::ungroup() %>%
  dplyr::filter(DPE == 0, Temperature == 28) %>%
  dplyr::group_by(Beta.Metric) %>%
  dplyr::summarize(Ref.Score = median(Beta.Score_norm))# 

# Step 2: Calculate displacement for each sample
data.dynamics_Unexp[["Beta"]][["Displacement"]]$init28 <-
  data.dynamics_Unexp[["Beta"]][["Metadata"]] %>%
  dplyr::left_join(data.dynamics_Unexp[["Beta"]][["Displacement"]]$median_diversity_initial_28C, by = "Beta.Metric") %>% ## Multiple metrics
  dplyr::group_by(Beta.Metric) %>%
  dplyr::mutate(Displacement = Beta.Score_norm - Ref.Score) %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Beta.Score, Beta.Score_norm, Ref.Score, Displacement) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)

## Relative  ----------------------------------------------------------------
# Notes: 
#   Here, we calculate displacement by subtracting the median diversity found at the 
#   initial time point of all 28°C control samples by the current sample's diversity score. 
#   This assumes that the reference point for gut microbiome normality is at the initial time point. 

cat("Calculating Displacement scores: Relative Control")

### Alpha -------------------------------------------------------------------

cat("Calculating Displacement scores: Alpha")

# Step 1: Filter initial samples at 28°C and calculate their median diversity score
data.dynamics_Unexp[["Alpha"]][["Displacement"]]$median_diversity_relTime_28C <-
  data.dynamics_Unexp[["Alpha"]][["Metadata"]] %>%
  
  # Filter for only control samples at 28°C
  dplyr::filter(Treatment == "Control", Temperature == 28) %>%
  
  # Group by time (DPE) and alpha diversity metric
  dplyr::group_by(DPE, Alpha.Metric) %>%
  
  # Calculate median alpha scores at each time point in controls
  dplyr::summarize(Ref.Score = median(Alpha.Score)) %>%
  dplyr::ungroup()

# Step 2: Calculate displacement for each sample
data.dynamics_Unexp[["Alpha"]][["Displacement"]]$relTime28 <-
  data.dynamics_Unexp[["Alpha"]][["Metadata"]] %>%
  
  # Merge data frame from previous step to data frame
  dplyr::left_join(data.dynamics_Unexp[["Alpha"]][["Displacement"]]$median_diversity_relTime_28C, by = c("Alpha.Metric", "DPE")) %>%
  
  # Calculate displacement
  dplyr::mutate(Displacement = Alpha.Score - Ref.Score) %>%
  
  # Select relevant columns for analysis and plotting
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Alpha.Metric, Alpha.Score,Ref.Score, Displacement) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)

### Beta -------------------------------------------------------------------

cat("Calculating Displacement scores: Beta")

# Step 1: Filter initial samples at 28°C and calculate their median diversity score
data.dynamics_Unexp[["Beta"]][["Displacement"]]$median_diversity_relTime_28C <-
  data.dynamics_Unexp[["Beta"]][["Metadata"]] %>%
  dplyr::filter(Treatment == "Control", Temperature == 28) %>%
  dplyr::group_by(DPE, Beta.Metric) %>%
  dplyr::summarize(Ref.Score = median(Beta.Score_norm)) %>%
  dplyr::ungroup()

# Step 2: Calculate displacement for each sample
data.dynamics_Unexp[["Beta"]][["Displacement"]]$relTime28 <-
  data.dynamics_Unexp[["Beta"]][["Metadata"]] %>%
  dplyr::left_join(data.dynamics_Unexp[["Beta"]][["Displacement"]]$median_diversity_relTime_28C, by = c("Beta.Metric", "DPE")) %>%
  dplyr::mutate(Displacement = Beta.Score_norm - Ref.Score) %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Beta.Metric, Beta.Score_norm,Ref.Score, Displacement) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)

## Relative Group ----------------------------------------------------------------
# Notes: 
#   Here, we calculate displacement by subtracting the median diversity found at the 
#   initial time point of all 28°C control samples by the current sample's diversity score. 
#   This assumes that the reference point for gut microbiome normality is at the initial time point. 

cat("Calculating Displacement scores: Relative Control")

### Alpha -------------------------------------------------------------------

cat("Calculating Displacement scores: Alpha")

# Step 1: Filter initial samples at 28°C and calculate their median diversity score
data.dynamics_Unexp[["Alpha"]][["Displacement"]]$median_diversity_relTimeTemp <-
  data.dynamics_Unexp[["Alpha"]][["Metadata"]] %>%
  
  # Filter for only control samples at 28°C
  dplyr::filter(Treatment == "Control") %>%
  
  # Group by time (DPE) and alpha diversity metric
  dplyr::group_by(DPE, Temperature, Alpha.Metric) %>%
  
  # Calculate median alpha scores at each time point in controls
  dplyr::summarize(Ref.Score = median(Alpha.Score)) %>%
  dplyr::ungroup()

# Step 2: Calculate displacement for each sample
data.dynamics_Unexp[["Alpha"]][["Displacement"]]$relTimeTemp <-
  data.dynamics_Unexp[["Alpha"]][["Metadata"]] %>%
  
  # Merge data frame from previous step to data frame
  dplyr::left_join(data.dynamics_Unexp[["Alpha"]][["Displacement"]]$median_diversity_relTimeTemp, by = c("Alpha.Metric", "DPE", "Temperature")) %>%
  
  # Calculate displacement
  dplyr::mutate(Displacement = Alpha.Score - Ref.Score) %>%
  
  # Select relevant columns for analysis and plotting
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Alpha.Metric, Alpha.Score,Ref.Score, Displacement) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)

### Beta -------------------------------------------------------------------

cat("Calculating Displacement scores: Beta")

# Step 1: Filter initial samples at 28°C and calculate their median diversity score
data.dynamics_Unexp[["Beta"]][["Displacement"]]$median_diversity_relTimeTemp <-
  data.dynamics_Unexp[["Beta"]][["Metadata"]] %>%
  dplyr::filter(Treatment == "Control") %>%
  dplyr::group_by(DPE, Temperature, Beta.Metric) %>%
  dplyr::summarize(Ref.Score = median(Beta.Score_norm)) %>%
  dplyr::ungroup()

# Step 2: Calculate displacement for each sample
data.dynamics_Unexp[["Beta"]][["Displacement"]]$relTimeTemp <-
  data.dynamics_Unexp[["Beta"]][["Metadata"]] %>%
  dplyr::left_join(data.dynamics_Unexp[["Beta"]][["Displacement"]]$median_diversity_relTimeTemp, by = c("Beta.Metric", "DPE", "Temperature")) %>%
  dplyr::mutate(Displacement = Beta.Score_norm - Ref.Score) %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Beta.Metric, Beta.Score_norm,Ref.Score, Displacement) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)



# Velocity & Acceleration -------------------------------------------------- 

cat("Calculating Velocity scores: Alpha")

## Alpha -------------------------------------------------------------------

### Calculate Velocity --------------------------------------------------

# Step 1: Calculate median diversity score for each Treatment, Temperature, and DPE
data.dynamics_Unexp[["Alpha"]][["Velocity"]]$median_displacement_by_group <- data.dynamics_Unexp[["Alpha"]][["Displacement"]]$relTime28 %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::group_by(Alpha.Metric, Treatment, Temperature, DPE) %>%
  dplyr::summarize(median_displacement = median(Displacement), .groups = 'drop') %>%
  dplyr::ungroup()

# Step 2: Create a shifted version of median diversity to align with the next DPE
data.dynamics_Unexp[["Alpha"]][["Velocity"]]$shifted_median_displacement <- data.dynamics_Unexp[["Alpha"]][["Velocity"]]$median_displacement_by_group %>%
  dplyr::arrange(Treatment, Temperature, DPE) %>%
  dplyr::group_by(Alpha.Metric, Treatment, Temperature) %>%
  dplyr::mutate(previous_median_displacement = dplyr::lag(median_displacement),
                previous_timepoint = dplyr::lag(DPE)) %>%
  dplyr::filter(!is.na(previous_median_displacement)) %>% # Remove the first row of each group where lag creates NA
  dplyr::ungroup()

# Step 3: Calculate the change in diversity score for each sample
data.dynamics_Unexp[["Alpha"]][["Velocity"]]$data.vel <- data.dynamics_Unexp[["Alpha"]][["Displacement"]]$relTime28 %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::full_join(data.dynamics_Unexp[["Alpha"]][["Velocity"]]$shifted_median_displacement, by = c("Alpha.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Alpha.Metric", "Alpha.Score", "median_displacement", "previous_median_displacement", "previous_timepoint"), .before = 1) %>%
  dplyr::ungroup() %>%
  # group_by(Alpha.Metric) %>%
  dplyr:: mutate(Velocity = (Displacement - previous_median_displacement) / (DPE - previous_timepoint), .after = "previous_median_displacement") %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Alpha.Metric, Alpha.Score, Displacement, Velocity) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)


### Calculate Acceleration --------------------------------------------------

cat("Calculating Acceleration scores: Alpha")

# Step 4: Calculate median diversity score for each Treatment, Temperature, and DPE
data.dynamics_Unexp[["Alpha"]][["Velocity"]]$median_velocity_by_group <- data.dynamics_Unexp[["Alpha"]][["Velocity"]]$data.vel %>%
  dplyr::filter(!is.na(Velocity)) %>%
  dplyr::group_by(Alpha.Metric, Treatment, Temperature, DPE) %>%
  dplyr::summarize(median_velocity = median(Velocity), .groups = 'drop')

# Step 5: Create a shifted version of median diversity to align with the next DPE
data.dynamics_Unexp[["Alpha"]][["Acceleration"]]$shifted_median_velocity <- data.dynamics_Unexp[["Alpha"]][["Velocity"]]$median_velocity_by_group %>%
  dplyr::arrange(Treatment, Temperature, DPE) %>%
  dplyr::group_by(Alpha.Metric, Treatment, Temperature) %>%
  dplyr::mutate(previous_median_velocity = dplyr::lag(median_velocity),
                previous_timepoint = dplyr::lag(DPE)) %>%
  dplyr::filter(!is.na(previous_median_velocity)) %>% # Remove the first row of each group where lag creates NA
  dplyr::ungroup()

# Step 6: Calculate the change in diversity score for each sample
data.dynamics_Unexp[["Alpha"]][["Acceleration"]]$data.acc <- data.dynamics_Unexp[["Alpha"]][["Velocity"]]$data.vel %>%
  dplyr::full_join(data.dynamics_Unexp[["Alpha"]][["Velocity"]]$shifted_median_displacement, by = c("Alpha.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::select(-previous_timepoint) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Alpha.Metric", "Alpha.Score", "previous_median_displacement"), .before = 1) %>%
  dplyr::full_join(data.dynamics_Unexp[["Alpha"]][["Acceleration"]]$shifted_median_velocity, by = c("Alpha.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Alpha.Metric", "Alpha.Score", "previous_median_velocity", "previous_timepoint", "median_displacement", "previous_median_displacement"), .before = 1) %>%
  dplyr::ungroup() %>%
  # group_by(Alpha.Metric) %>%
  dplyr::mutate(Acceleration = (Velocity - previous_median_velocity) / (DPE - previous_timepoint)) %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Alpha.Metric, Acceleration) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)


## Beta -------------------------------------------------------------------

### Calculate Velocity ------------------------------------------------------

cat("Calculating Velocity scores: Beta")

# Step 1: Calculate median diversity score for each Treatment, Temperature, and DPE
data.dynamics_Unexp[["Beta"]][["Velocity"]]$median_displacement_by_group <- data.dynamics_Unexp[["Beta"]][["Displacement"]]$relTime28 %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature, DPE) %>%
  dplyr::summarize(median_displacement = median(Displacement), .groups = 'drop') %>%
  dplyr::ungroup()

# Step 2: Create a shifted version of median diversity to align with the next DPE
data.dynamics_Unexp[["Beta"]][["Velocity"]]$shifted_median_displacement <- data.dynamics_Unexp[["Beta"]][["Velocity"]]$median_displacement_by_group %>%
  dplyr::arrange(Treatment, Temperature, DPE) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature) %>%
  dplyr::mutate(previous_median_displacement = dplyr::lag(median_displacement),
                previous_timepoint = dplyr::lag(DPE)) %>%
  dplyr::filter(!is.na(previous_median_displacement)) %>% # Remove the first row of each group where lag creates NA
  dplyr::ungroup()

# Step 3: Calculate the change in diversity score for each sample
data.dynamics_Unexp[["Beta"]][["Velocity"]]$data.vel <- data.dynamics_Unexp[["Beta"]][["Displacement"]]$relTime28 %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Velocity"]]$shifted_median_displacement, by = c("Beta.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Beta.Metric", "Beta.Score_norm", "median_displacement", "previous_median_displacement", "previous_timepoint"), .before = 1) %>%
  dplyr::ungroup() %>%
  # group_by(Beta.Metric) %>%
  dplyr::mutate(Velocity = (Displacement - previous_median_displacement) / (DPE - previous_timepoint), .after = "previous_median_displacement") %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Beta.Metric, Beta.Score_norm, Displacement, Velocity) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)

### Calculate Acceleration ------------------------------------------------------

cat("Calculating Acceleration scores: Beta")

# Step 4: Calculate median diversity score for each Treatment, Temperature, and DPE
data.dynamics_Unexp[["Beta"]][["Velocity"]]$median_velocity_by_group <- data.dynamics_Unexp[["Beta"]][["Velocity"]]$data.vel %>%
  dplyr::filter(!is.na(Velocity)) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature, DPE) %>%
  dplyr::summarize(median_velocity = median(Velocity), .groups = 'drop')

# Step 5: Create a shifted version of median diversity to align with the next DPE
data.dynamics_Unexp[["Beta"]][["Acceleration"]]$shifted_median_velocity <- data.dynamics_Unexp[["Beta"]][["Velocity"]]$median_velocity_by_group %>%
  dplyr::arrange(Treatment, Temperature, DPE) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature) %>%
  dplyr::mutate(previous_median_velocity = lag(median_velocity),
                previous_timepoint = lag(DPE)) %>%
  dplyr::filter(!is.na(previous_median_velocity)) %>% # Remove the first row of each group where lag creates NA
  dplyr::ungroup()

# Step 6: Calculate the change in diversity score for each sample
data.dynamics_Unexp[["Beta"]][["Acceleration"]]$data.acc <- data.dynamics_Unexp[["Beta"]][["Velocity"]]$data.vel %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Velocity"]]$shifted_median_displacement, by = c("Beta.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::select(-previous_timepoint) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Beta.Metric", "Beta.Score_norm", "previous_median_displacement"), .before = 1) %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Acceleration"]]$shifted_median_velocity, by = c("Beta.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Beta.Metric", "Beta.Score_norm", "previous_median_velocity", "previous_timepoint", "median_displacement", "previous_median_displacement"), .before = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Acceleration = (Velocity - previous_median_velocity) / (DPE - previous_timepoint)) %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Beta.Metric, Acceleration) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)


#### By Group ----------------------------------------------------------------

##### Calculate Velocity ------------------------------------------------------

cat("Calculating Velocity scores: Beta")

# Step 1: Calculate median diversity score for each Treatment, Temperature, and DPE
data.dynamics_Unexp[["Beta"]][["Velocity"]]$median_displacement_by_group <- data.dynamics_Unexp[["Beta"]][["Displacement"]]$relTimeTemp %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature, DPE) %>%
  dplyr::summarize(median_displacement = median(Displacement), .groups = 'drop') %>%
  dplyr::ungroup()

# Step 2: Create a shifted version of median diversity to align with the next DPE
data.dynamics_Unexp[["Beta"]][["Velocity"]]$shifted_median_displacement <- data.dynamics_Unexp[["Beta"]][["Velocity"]]$median_displacement_by_group %>%
  dplyr::arrange(Treatment, Temperature, DPE) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature) %>%
  dplyr::mutate(previous_median_displacement = dplyr::lag(median_displacement),
                previous_timepoint = dplyr::lag(DPE)) %>%
  dplyr::filter(!is.na(previous_median_displacement)) %>% # Remove the first row of each group where lag creates NA
  dplyr::ungroup()

# Step 3: Calculate the change in diversity score for each sample
data.dynamics_Unexp[["Beta"]][["Velocity"]]$data.vel <- data.dynamics_Unexp[["Beta"]][["Displacement"]]$relTimeTemp %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Velocity"]]$shifted_median_displacement, by = c("Beta.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Beta.Metric", "Beta.Score_norm", "median_displacement", "previous_median_displacement", "previous_timepoint"), .before = 1) %>%
  dplyr::ungroup() %>%
  # group_by(Beta.Metric) %>%
  dplyr::mutate(Velocity = (Displacement - previous_median_displacement) / (DPE - previous_timepoint), .after = "previous_median_displacement") %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Beta.Metric, Beta.Score_norm, Displacement, Velocity) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)

##### Calculate Acceleration ------------------------------------------------------

cat("Calculating Acceleration scores: Beta")

# Step 4: Calculate median diversity score for each Treatment, Temperature, and DPE
data.dynamics_Unexp[["Beta"]][["Velocity"]]$median_velocity_by_group <- data.dynamics_Unexp[["Beta"]][["Velocity"]]$data.vel %>%
  dplyr::filter(!is.na(Velocity)) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature, DPE) %>%
  dplyr::summarize(median_velocity = median(Velocity), .groups = 'drop')

# Step 5: Create a shifted version of median diversity to align with the next DPE
data.dynamics_Unexp[["Beta"]][["Acceleration"]]$shifted_median_velocity <- data.dynamics_Unexp[["Beta"]][["Velocity"]]$median_velocity_by_group %>%
  dplyr::arrange(Treatment, Temperature, DPE) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature) %>%
  dplyr::mutate(previous_median_velocity = lag(median_velocity),
                previous_timepoint = lag(DPE)) %>%
  dplyr::filter(!is.na(previous_median_velocity)) %>% # Remove the first row of each group where lag creates NA
  dplyr::ungroup()

# Step 6: Calculate the change in diversity score for each sample
data.dynamics_Unexp[["Beta"]][["Acceleration"]]$data.acc <- data.dynamics_Unexp[["Beta"]][["Velocity"]]$data.vel %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Velocity"]]$shifted_median_displacement, by = c("Beta.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::select(-previous_timepoint) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Beta.Metric", "Beta.Score_norm", "previous_median_displacement"), .before = 1) %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Acceleration"]]$shifted_median_velocity, by = c("Beta.Metric", "Treatment", "Temperature", "DPE")) %>%
  dplyr::relocate(c("Sample", "Treatment", "Temperature", "DPE", "Tank.ID", "Beta.Metric", "Beta.Score_norm", "previous_median_velocity", "previous_timepoint", "median_displacement", "previous_median_displacement"), .before = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Acceleration = (Velocity - previous_median_velocity) / (DPE - previous_timepoint)) %>%
  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID, Beta.Metric, Acceleration) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID)


# Volatility ------------------------------------------------------------

cat("Calculating Volatility scores: Alpha")

## Alpha -------------------------------------------------------------------

data.dynamics_Unexp[["Alpha"]][["Volatility"]]$All <- 
  data.dynamics_Unexp[["Alpha"]][["Displacement"]]$relTime28 %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::full_join(data.dynamics_Unexp[["Alpha"]][["Velocity"]]$data.vel) %>%
  dplyr::full_join(data.dynamics_Unexp[["Alpha"]][["Acceleration"]]$data.acc) %>%
  dplyr::group_by(Alpha.Metric, Treatment, Temperature, DPE) %>%
  dplyr::mutate(Volatility = sd(Displacement), .after = "Displacement") %>%
  dplyr::ungroup() 

## Beta -------------------------------------------------------------------

cat("Calculating Volatility scores: Beta")

data.dynamics_Unexp[["Beta"]][["Volatility"]]$All <- 
  data.dynamics_Unexp[["Beta"]][["Displacement"]]$relTime28 %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Velocity"]]$data.vel) %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Acceleration"]]$data.acc) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature, DPE) %>%
  dplyr::mutate(Volatility = sd(Displacement), .after = "Displacement") %>%
  dplyr::ungroup() 

cat("Completed calculating Dynamic Scores")


### By Group ----------------------------------------------------------------

cat("Calculating Volatility scores: Beta")

data.dynamics_Unexp[["Beta"]][["Volatility"]]$All <- 
  data.dynamics_Unexp[["Beta"]][["Displacement"]]$relTimeTemp %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Velocity"]]$data.vel) %>%
  dplyr::full_join(data.dynamics_Unexp[["Beta"]][["Acceleration"]]$data.acc) %>%
  dplyr::group_by(Beta.Metric, Treatment, Temperature, DPE) %>%
  dplyr::mutate(Volatility = sd(Displacement), .after = "Displacement") %>%
  dplyr::ungroup() 

cat("Completed calculating Dynamic Scores")
