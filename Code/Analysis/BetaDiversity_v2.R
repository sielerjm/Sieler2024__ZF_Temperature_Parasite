
# Beta Diversity ----------------------------------------------------------
#   - Calculate Distance Matrices
#   - Permanova
#   - Beta Dispersion

## Variables, lists, etc.
beta.dist.mat <- list() # Saves distnace matrix values here
beta.perm <- list() # Saves PERMANOVA results here
beta.disp <- list() # Saves Beta Dispersion results here
vars.list <- list() # Saves a list of variables
data.beta.list <- list() # Saves a list of beta data for different subsetted ps.objects

## Run purrr::map() in parallel
future::plan(future::multisession, workers = (detectCores()-1) )



# All ---------------------------------------------------------------------


##  Calculate Personalization Scores ----------------------------------------------

# Save a temporary 
tmp.psOBJ <- ps.list[["All"]]

# Save dataframe
data.beta.list[["All"]] <-
  
  # Grab Phyloseq Object
  tmp.psOBJ %>% 
  
  # Calculate Distance matrix
  microViz::dist_calc("bray") %>% 
  
  # Get distance matric
  microViz::dist_get() %>%
  # Pat Schloss methods to view beta diversity matrix data
  # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
  
  # Convert into a matrix object
  as.matrix() %>%
  
  # Convert matrix into a tibble
  tibble::as_tibble(rownames = "Sample_a") %>%
  
  # Convert dataframe into "long" format
  tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
  
  # Remove any rows where names equal eachother
  dplyr::filter(Sample_a != Sample_b) %>%
  
  # Merge sample dataframe
  dplyr::right_join((tmp.psOBJ %>% 
                samdat_tbl() %>% ungroup() %>%  arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                select(!contains("_Genus")) %>%
                select(Sample, DPE, Treatment, Temperature, Tank.ID)
  ), 
  by = c("Sample_a" = "Sample")) %>% 
  dplyr::right_join((tmp.psOBJ %>% 
                microViz::samdat_tbl() %>% 
                  dplyr::ungroup() %>% 
                  dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                  dplyr::select(!contains("_Genus")) %>%
                  dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
  ), 
  by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>% # add suffix to differentiate between sample metadata
  # Move columns around
  dplyr::select(order(colnames(.))) %>%
  dplyr::relocate(dplyr::any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
  dplyr::relocate(dplyr::contains(c("Tank")), .after = dplyr::last_col()) %>%
  dplyr::ungroup() %>%
  
  # Calculate the median distance of samples to all other samples (Measure of "personalization")
  dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
  dplyr::summarize(Beta.Score = median(Dist)*100, # multiply by 100 to ease normalization calculations (probably unneeded)
                   Beta.Score.Mean = mean(Dist)*100, # multiply by 100 to ease normalization calculations
                   count = dplyr::n()) %>% 
  dplyr::ungroup() %>%
  
  # Rename Columns
  dplyr::rename(Sample = Sample_a,
         Treatment = Treatment_a,
         Temperature = Temperature_a,
         DPE = DPE_a,
         Tank.ID = Tank.ID_a) %>%
  dplyr::right_join(tmp.psOBJ %>%
               microViz::samdat_tbl() %>% 
                 dplyr::select(!contains("_Genus")),
             by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
  dplyr::ungroup() %>%
  
  # Normalize diversity scores
  dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
  
  # Create a column to save what type of beta metric was calculated
  dplyr::mutate(Beta.Metric = "Bray-Curtis", .before = "Beta.Score")

data.beta.list[["All"]] <- data.beta.list[["All"]] %>%
  dplyr::full_join(., 
            tmp.psOBJ %>% 
              # tax_agg("Genus") %>%
              microViz::dist_calc("canberra") %>% 
              
              # Get distance matric
              microViz::dist_get() %>%
              # Pat Schloss methods to view beta diversity matrix data
              # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
              
              # Convert into a matrix object
              as.matrix() %>%
              
              # Convert matrix into a tibble
              tibble::as_tibble(rownames = "Sample_a") %>%
              
              # Convert dataframe into "long" format
              tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
              
              # Remove any rows where names equal eachother
              dplyr::filter(Sample_a != Sample_b) %>%
              
              # Merge sample dataframe
              dplyr::right_join((tmp.psOBJ %>% 
                            microViz::samdat_tbl() %>% 
                              dplyr::ungroup() %>%  
                              dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                              dplyr::select(!contains("_Genus")) %>%
                              dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
              ), 
              by = c("Sample_a" = "Sample")) %>%
              dplyr::right_join((tmp.psOBJ %>% 
                            microViz::samdat_tbl() %>% 
                              dplyr::ungroup() %>%  
                              dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                              dplyr::select(!contains("_Genus")) %>%
                              dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
              ), 
              by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>%
              # Move columns around
              dplyr::select(order(colnames(.))) %>%
              dplyr::relocate(any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
              dplyr::relocate(contains(c("Tank")), .after = dplyr::last_col()) %>%
              dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
              
              # Calculate the median distance of samples to all other samples (Measure of "personalization")
              dplyr::summarize(Beta.Score = median(Dist)*100, 
                               Beta.Score.Mean = mean(Dist)*100, 
                               count = dplyr::n()) %>% # multiply by 100 to ease normalization calculations
              dplyr::ungroup() %>%
              dplyr::rename(Sample = Sample_a,
                     Treatment = Treatment_a,
                     Temperature = Temperature_a,
                     DPE = DPE_a,
                     Tank.ID = Tank.ID_a) %>%
              dplyr::right_join(tmp.psOBJ %>%
                                  microViz::samdat_tbl() %>% dplyr::select(!contains("_Genus")),
                         by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
              dplyr::ungroup() %>%
              
              # Normalize diversity scores
              dplyr::mutate(dplyr::across(contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
              
              # Create a column to save what type of beta metric was calculated
              dplyr::mutate(Beta.Metric = "Canberra", .before = "Beta.Score")
  )

data.beta.list[["All"]] <- data.beta.list[["All"]] %>%
  dplyr::full_join(., 
            tmp.psOBJ %>% 
              # tax_agg("Genus") %>%
              microViz::dist_calc("gunifrac") %>% 
              
              # Get distance matric
              microViz::dist_get() %>%
              # Pat Schloss methods to view beta diversity matrix data
              # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
              
              # Convert into a matrix object
              as.matrix() %>%
              
              # Convert matrix into a tibble
              tibble::as_tibble(rownames = "Sample_a") %>%
              
              # Convert dataframe into "long" format
              tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
              
              # Remove any rows where names equal eachother
              dplyr::filter(Sample_a != Sample_b) %>%
              
              # Merge sample dataframe
              dplyr::right_join((tmp.psOBJ %>% 
                            microViz::samdat_tbl() %>% 
                              dplyr::ungroup() %>%  
                              dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                              dplyr::select(!contains("_Genus")) %>%
                              dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
              ), 
              by = c("Sample_a" = "Sample")) %>%
              dplyr::right_join((tmp.psOBJ %>% 
                            microViz::samdat_tbl() %>% ungroup() %>%  arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                            select(!contains("_Genus")) %>%
                            select(Sample, DPE, Treatment, Temperature, Tank.ID)
              ), 
              by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>%
              # Move columns around
              dplyr::select(order(colnames(.))) %>%
              dplyr::relocate(dplyr::any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
              dplyr::relocate(dplyr::contains(c("Tank")), .after = dplyr::last_col()) %>%
              dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
              
              # Calculate the median distance of samples to all other samples (Measure of "personalization")
              dplyr::summarize(Beta.Score = median(Dist)*100, 
                               Beta.Score.Mean = mean(Dist)*100, 
                               count = dplyr::n()) %>% # multiply by 100 to ease normalization calculations
              dplyr::ungroup() %>%
              dplyr::rename(Sample = Sample_a,
                     Treatment = Treatment_a,
                     Temperature = Temperature_a,
                     DPE = DPE_a,
                     Tank.ID = Tank.ID_a) %>%
              dplyr::right_join(tmp.psOBJ %>%
                           microViz::samdat_tbl() %>% dplyr::select(!contains("_Genus")),
                         by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
              dplyr::ungroup() %>%
              
              # Normalize diversity scores
              dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
              
              # Create a column to save what type of beta metric was calculated
              dplyr::mutate(Beta.Metric = "G-Unifrac", .before = "Beta.Score")
  )



## Calc Distance Matrix ----------------------------------------------------

beta.dist.mat[["All"]] <- # saves the results of this loop to a list
  furrr::future_map(div.mtd[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # ps.list[["All"]] is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                    rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(div.mtd[["beta"]]) # assigns names of the beta methods to the list 


## Permanova ---------------------------------------------------------------

# 
# 
# ### Run PERMANOVA
# beta.perm[["All"]][["Temp + DPE"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["All"]][[x]] %>% # list of distance matrices for each beta method
#       dist_permanova(variables = c("Temperature", "DPE", "Treatment"), n_perms = 999, seed = 123) %>% # runs PERMANOVA test
#       perm_get() # Returns the permanova results
#   }) %>%
#   setNames(div.mtd[["beta"]])
# 
# beta.perm[["All"]][["Temp + DPE"]][["Table"]] <- # Save table to a list
#   map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.perm[["All"]][["Temp + DPE"]][["Stats"]][[x]] %>% tidy() %>% # tidy() converts stat results into dataframe
#       as_tibble() %>% # Make a tibble
#       mutate(metric = x, .before = term) %>% # Make a new column called "metric" and fill in which beta metric
#       mutate(sig = ifelse(p.value <= 0.05, "*", "")) # Add starts to a col called "sig" if significant
#   }) %>% bind_rows() %>% # Merge each beta dataframe into one dataframe
#   group_by(metric) 
# 
# 
# ## Hom Disp ----------------------------------------------------------------
# 
# 
# ### Temp + DPE + Treat ------------------------------------------------------
# 
# vars.list[["All"]][["Temp + DPE + Treat"]] <- c("Temperature", "DPE", "Treatment")
# 
# beta.disp[["All"]][["Temp + DPE + Treat"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["All"]][[x]] %>% # list of distance matrices for each beta method
#       dist_bdisp(variables = c("Temperature", "DPE", "Treatment")) %>% # Runs Homogeneity of multivariate dispersions (HoMD)
#       bdisp_get() # Returns the HoMD results
#   }) %>%
#   setNames(div.mtd[["beta"]]) # Assigns names for each beta method to the list
# 
# 
# beta.disp[["All"]][["Temp + DPE + Treat"]][["Table"]]["Anova"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["All"]][["Temp + DPE + Treat"]], function(y){
#       
#       beta.disp[["All"]][["Temp + DPE + Treat"]][["Stats"]][[x]][[y]][["anova"]] %>% 
#         tidy() %>%
#         mutate(metric = x, 
#                .before = 1) %>%
#         mutate(term = case_when(
#           term == "Groups" ~ y,
#           .default = term
#         )) %>%
#         mutate(Sig = ifelse(`p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows()
#   }) %>% bind_rows() %>% nest()
# 
# 
# #### Tukey
# 
# beta.disp[["All"]][["Temp + DPE + Treat"]][["Table"]]["Tukey"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["All"]][["Temp + DPE + Treat"]], function(y){
#       
#       beta.disp[["All"]][["Temp + DPE + Treat"]][["Stats"]][[x]][[y]][["tukeyHSD"]] %>% 
#         tidy() %>%
#         mutate(metric = x, .before = term) %>%
#         mutate(term = y) %>%
#         mutate(Sig = ifelse(`adj.p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows() 
#   }) %>% bind_rows() %>% nest()
# 
# 
# 
# ### Treat.Temp.DPE ----------------------------------------------------------
# 
# vars.list[["All"]][["Treat.Temp.DPE"]] <- c("Treat.Temp.DPE")
# 
# beta.disp[["All"]][["Treat.Temp.DPE"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["All"]][[x]] %>% # list of distance matrices for each beta method
#       dist_bdisp(variables = c("Treat.Temp.DPE")) %>% # Runs Homogeneity of multivariate dispersions (HoMD)
#       bdisp_get() # Returns the HoMD results
#   }) %>%
#   setNames(div.mtd[["beta"]]) # Assigns names for each beta method to the list
# 
# 
# beta.disp[["All"]][["Treat.Temp.DPE"]][["Table"]]["Anova"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["All"]][["Treat.Temp.DPE"]], function(y){
#       
#       beta.disp[["All"]][["Treat.Temp.DPE"]][["Stats"]][[x]][[y]][["anova"]] %>% 
#         tidy() %>%
#         mutate(metric = x, 
#                .before = 1) %>%
#         mutate(term = case_when(
#           term == "Groups" ~ y,
#           .default = term
#         )) %>%
#         mutate(Sig = ifelse(`p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows()
#   }) %>% bind_rows() %>% nest()
# 
# 
# #### Tukey
# 
# beta.disp[["All"]][["Treat.Temp.DPE"]][["Table"]]["Tukey"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["All"]][["Treat.Temp.DPE"]], function(y){
#       
#       beta.disp[["All"]][["Treat.Temp.DPE"]][["Stats"]][[x]][[y]][["tukeyHSD"]] %>% 
#         tidy() %>%
#         mutate(metric = x, .before = term) %>%
#         mutate(term = y) %>%
#         mutate(Sig = ifelse(`adj.p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows() 
#   }) %>% bind_rows() %>% nest()


# Unexposed ---------------------------------------------------------------



##  Calculate Median Distance ----------------------------------------------

# Save a temporary 
tmp.psOBJ <- ps.list[["Unexposed"]]

# Save dataframe
data.beta.list[["Unexposed"]] <-
  
  # Grab Phyloseq Object
  tmp.psOBJ %>% 
  
  # Calculate Distance matrix
  microViz::dist_calc("bray") %>% 
  
  # Get distance matric
  microViz::dist_get() %>%
  # Pat Schloss methods to view beta diversity matrix data
  # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
  
  # Convert into a matrix object
  as.matrix() %>%
  
  # Convert matrix into a tibble
  tibble::as_tibble(rownames = "Sample_a") %>%
  
  # Convert dataframe into "long" format
  tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
  
  # Remove any rows where names equal eachother
  dplyr::filter(Sample_a != Sample_b) %>%
  
  # Merge sample dataframe
  dplyr::right_join((tmp.psOBJ %>% 
                       samdat_tbl() %>% ungroup() %>%  arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                       select(!contains("_Genus")) %>%
                       select(Sample, DPE, Treatment, Temperature, Tank.ID)
  ), 
  by = c("Sample_a" = "Sample")) %>% 
  dplyr::right_join((tmp.psOBJ %>% 
                       microViz::samdat_tbl() %>% 
                       dplyr::ungroup() %>% 
                       dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                       dplyr::select(!contains("_Genus")) %>%
                       dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
  ), 
  by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>% # add suffix to differentiate between sample metadata
  # Move columns around
  dplyr::select(order(colnames(.))) %>%
  dplyr::relocate(dplyr::any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
  dplyr::relocate(dplyr::contains(c("Tank")), .after = dplyr::last_col()) %>%
  dplyr::ungroup() %>%
  
  # Calculate the median distance of samples to all other samples (Measure of "personalization")
  dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
  dplyr::summarize(Beta.Score = median(Dist)*100, # multiply by 100 to ease normalization calculations (probably unneeded)
                   Beta.Score.Mean = mean(Dist)*100, # multiply by 100 to ease normalization calculations
                   count = dplyr::n()) %>% 
  dplyr::ungroup() %>%
  
  # Rename Columns
  dplyr::rename(Sample = Sample_a,
                Treatment = Treatment_a,
                Temperature = Temperature_a,
                DPE = DPE_a,
                Tank.ID = Tank.ID_a) %>%
  dplyr::right_join(tmp.psOBJ %>%
                      microViz::samdat_tbl() %>% 
                      dplyr::select(!contains("_Genus")),
                    by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
  dplyr::ungroup() %>%
  
  # Normalize diversity scores
  dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
  
  # Create a column to save what type of beta metric was calculated
  dplyr::mutate(Beta.Metric = "Bray-Curtis", .before = "Beta.Score")

data.beta.list[["Unexposed"]] <- data.beta.list[["Unexposed"]] %>%
  dplyr::full_join(., 
                   tmp.psOBJ %>% 
                     # tax_agg("Genus") %>%
                     microViz::dist_calc("canberra") %>% 
                     
                     # Get distance matric
                     microViz::dist_get() %>%
                     # Pat Schloss methods to view beta diversity matrix data
                     # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
                     
                     # Convert into a matrix object
                     as.matrix() %>%
                     
                     # Convert matrix into a tibble
                     tibble::as_tibble(rownames = "Sample_a") %>%
                     
                     # Convert dataframe into "long" format
                     tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
                     
                     # Remove any rows where names equal eachother
                     dplyr::filter(Sample_a != Sample_b) %>%
                     
                     # Merge sample dataframe
                     dplyr::right_join((tmp.psOBJ %>% 
                                          microViz::samdat_tbl() %>% 
                                          dplyr::ungroup() %>%  
                                          dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                                          dplyr::select(!contains("_Genus")) %>%
                                          dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
                     ), 
                     by = c("Sample_a" = "Sample")) %>%
                     dplyr::right_join((tmp.psOBJ %>% 
                                          microViz::samdat_tbl() %>% 
                                          dplyr::ungroup() %>%  
                                          dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                                          dplyr::select(!contains("_Genus")) %>%
                                          dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
                     ), 
                     by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>%
                     # Move columns around
                     dplyr::select(order(colnames(.))) %>%
                     dplyr::relocate(any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
                     dplyr::relocate(contains(c("Tank")), .after = dplyr::last_col()) %>%
                     dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
                     
                     # Calculate the median distance of samples to all other samples (Measure of "personalization")
                     dplyr::summarize(Beta.Score = median(Dist)*100, 
                                      Beta.Score.Mean = mean(Dist)*100, 
                                      count = dplyr::n()) %>% # multiply by 100 to ease normalization calculations
                     dplyr::ungroup() %>%
                     dplyr::rename(Sample = Sample_a,
                                   Treatment = Treatment_a,
                                   Temperature = Temperature_a,
                                   DPE = DPE_a,
                                   Tank.ID = Tank.ID_a) %>%
                     dplyr::right_join(tmp.psOBJ %>%
                                         microViz::samdat_tbl() %>% dplyr::select(!contains("_Genus")),
                                       by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
                     dplyr::ungroup() %>%
                     
                     # Normalize diversity scores
                     dplyr::mutate(dplyr::across(contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "Canberra", .before = "Beta.Score")
  )

data.beta.list[["Unexposed"]] <- data.beta.list[["Unexposed"]] %>%
  dplyr::full_join(., 
                   tmp.psOBJ %>% 
                     # tax_agg("Genus") %>%
                     microViz::dist_calc("gunifrac") %>% 
                     
                     # Get distance matric
                     microViz::dist_get() %>%
                     # Pat Schloss methods to view beta diversity matrix data
                     # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
                     
                     # Convert into a matrix object
                     as.matrix() %>%
                     
                     # Convert matrix into a tibble
                     tibble::as_tibble(rownames = "Sample_a") %>%
                     
                     # Convert dataframe into "long" format
                     tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
                     
                     # Remove any rows where names equal eachother
                     dplyr::filter(Sample_a != Sample_b) %>%
                     
                     # Merge sample dataframe
                     dplyr::right_join((tmp.psOBJ %>% 
                                          microViz::samdat_tbl() %>% 
                                          dplyr::ungroup() %>%  
                                          dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                                          dplyr::select(!contains("_Genus")) %>%
                                          dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
                     ), 
                     by = c("Sample_a" = "Sample")) %>%
                     dplyr::right_join((tmp.psOBJ %>% 
                                          microViz::samdat_tbl() %>% ungroup() %>%  arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                                          select(!contains("_Genus")) %>%
                                          select(Sample, DPE, Treatment, Temperature, Tank.ID)
                     ), 
                     by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>%
                     # Move columns around
                     dplyr::select(order(colnames(.))) %>%
                     dplyr::relocate(dplyr::any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
                     dplyr::relocate(dplyr::contains(c("Tank")), .after = dplyr::last_col()) %>%
                     dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
                     
                     # Calculate the median distance of samples to all other samples (Measure of "personalization")
                     dplyr::summarize(Beta.Score = median(Dist)*100, 
                                      Beta.Score.Mean = mean(Dist)*100, 
                                      count = dplyr::n()) %>% # multiply by 100 to ease normalization calculations
                     dplyr::ungroup() %>%
                     dplyr::rename(Sample = Sample_a,
                                   Treatment = Treatment_a,
                                   Temperature = Temperature_a,
                                   DPE = DPE_a,
                                   Tank.ID = Tank.ID_a) %>%
                     dplyr::right_join(tmp.psOBJ %>%
                                         microViz::samdat_tbl() %>% dplyr::select(!contains("_Genus")),
                                       by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
                     dplyr::ungroup() %>%
                     
                     # Normalize diversity scores
                     dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "G-Unifrac", .before = "Beta.Score")
  )



## Calc Distance Matrix ----------------------------------------------------

beta.dist.mat[["Unexposed"]] <- # saves the results of this loop to a list
  furrr::future_map(div.mtd[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # ps.list[["All"]] is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                    rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(div.mtd[["beta"]]) # assigns names of the beta methods to the list 


# ## Permanova ---------------------------------------------------------------
# 
# 
# 
# ### Run PERMANOVA
# beta.perm[["Unexposed"]][["Temp + DPE + Treat"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["Unexposed"]][[x]] %>% # list of distance matrices for each beta method
#       dist_permanova(variables = c("Temperature", "DPE", "Treatment"), n_perms = 999, seed = 123) %>% # runs PERMANOVA test
#       perm_get() # Returns the permanova results
#   }) %>%
#   setNames(div.mtd[["beta"]])
# 
# beta.perm[["Unexposed"]][["Temp + DPE + Treat"]][["Table"]] <- # Save table to a list
#   map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.perm[["Unexposed"]][["Temp + DPE + Treat"]][["Stats"]][[x]] %>% tidy() %>% # tidy() converts stat results into dataframe
#       as_tibble() %>% # Make a tibble
#       mutate(metric = x, .before = term) %>% # Make a new column called "metric" and fill in which beta metric
#       mutate(sig = ifelse(p.value <= 0.05, "*", "")) # Add starts to a col called "sig" if significant
#   }) %>% bind_rows() %>% # Merge each beta dataframe into one dataframe
#   group_by(metric) 
# 
# 
# ## Hom Disp ----------------------------------------------------------------
# 
# 
# ### Temp + DPE ------------------------------------------------------
# 
# vars.list[["Unexposed"]][["Temp + DPE"]] <- c("Temperature", "DPE")
# 
# beta.disp[["Unexposed"]][["Temp + DPE"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["Unexposed"]][[x]] %>% # list of distance matrices for each beta method
#       dist_bdisp(variables = c("Temperature", "DPE")) %>% # Runs Homogeneity of multivariate dispersions (HoMD)
#       bdisp_get() # Returns the HoMD results
#   }) %>%
#   setNames(div.mtd[["beta"]]) # Assigns names for each beta method to the list
# 
# 
# beta.disp[["Unexposed"]][["Temp + DPE"]][["Table"]]["Anova"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["Unexposed"]][["Temp + DPE"]], function(y){
#       
#       beta.disp[["Unexposed"]][["Temp + DPE"]][["Stats"]][[x]][[y]][["anova"]] %>% 
#         tidy() %>%
#         mutate(metric = x, 
#                .before = 1) %>%
#         mutate(term = case_when(
#           term == "Groups" ~ y,
#           .default = term
#         )) %>%
#         mutate(Sig = ifelse(`p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows()
#   }) %>% bind_rows() %>% nest()
# 
# 
# #### Tukey
# 
# beta.disp[["Unexposed"]][["Temp + DPE"]][["Table"]]["Tukey"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["Unexposed"]][["Temp + DPE"]], function(y){
#       
#       beta.disp[["Unexposed"]][["Temp + DPE"]][["Stats"]][[x]][[y]][["tukeyHSD"]] %>% 
#         tidy() %>%
#         mutate(metric = x, .before = term) %>%
#         mutate(term = y) %>%
#         mutate(Sig = ifelse(`adj.p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows() 
#   }) %>% bind_rows() %>% nest()
# 
# 
# 
# ### Treat.Temp.DPE ----------------------------------------------------------
# 
# vars.list[["Unexposed"]][["Temp.DPE"]] <- c("Temp.DPE")
# 
# beta.disp[["Unexposed"]][["Temp.DPE"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["Unexposed"]][[x]] %>% # list of distance matrices for each beta method
#       dist_bdisp(variables = c("Temp.DPE")) %>% # Runs Homogeneity of multivariate dispersions (HoMD)
#       bdisp_get() # Returns the HoMD results
#   }) %>%
#   setNames(div.mtd[["beta"]]) # Assigns names for each beta method to the list
# 
# 
# beta.disp[["Unexposed"]][["Temp.DPE"]][["Table"]]["Anova"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["Unexposed"]][["Temp.DPE"]], function(y){
#       
#       beta.disp[["Unexposed"]][["Temp.DPE"]][["Stats"]][[x]][[y]][["anova"]] %>% 
#         tidy() %>%
#         mutate(metric = x, 
#                .before = 1) %>%
#         mutate(term = case_when(
#           term == "Groups" ~ y,
#           .default = term
#         )) %>%
#         mutate(Sig = ifelse(`p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows()
#   }) %>% bind_rows() %>% nest()
# 
# 
# #### Tukey
# 
# beta.disp[["Unexposed"]][["Temp.DPE"]][["Table"]]["Tukey"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["Unexposed"]][["Temp.DPE"]], function(y){
#       
#       beta.disp[["Unexposed"]][["Temp.DPE"]][["Stats"]][[x]][[y]][["tukeyHSD"]] %>% 
#         tidy() %>%
#         mutate(metric = x, .before = term) %>%
#         mutate(term = y) %>%
#         mutate(Sig = ifelse(`adj.p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows() 
#   }) %>% bind_rows() %>% nest()
# 



# Exposed -----------------------------------------------------------------




##  Calculate Median Distance ----------------------------------------------

# Save a temporary 
tmp.psOBJ <- ps.list[["Exposed"]]

# Save dataframe
data.beta.list[["Exposed"]] <-
  
  # Grab Phyloseq Object
  tmp.psOBJ %>% 
  
  # Calculate Distance matrix
  microViz::dist_calc("bray") %>% 
  
  # Get distance matric
  microViz::dist_get() %>%
  # Pat Schloss methods to view beta diversity matrix data
  # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
  
  # Convert into a matrix object
  as.matrix() %>%
  
  # Convert matrix into a tibble
  tibble::as_tibble(rownames = "Sample_a") %>%
  
  # Convert dataframe into "long" format
  tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
  
  # Remove any rows where names equal eachother
  dplyr::filter(Sample_a != Sample_b) %>%
  
  # Merge sample dataframe
  dplyr::right_join((tmp.psOBJ %>% 
                       samdat_tbl() %>% ungroup() %>%  arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                       select(!contains("_Genus")) %>%
                       select(Sample, DPE, Treatment, Temperature, Tank.ID)
  ), 
  by = c("Sample_a" = "Sample")) %>% 
  dplyr::right_join((tmp.psOBJ %>% 
                       microViz::samdat_tbl() %>% 
                       dplyr::ungroup() %>% 
                       dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                       dplyr::select(!contains("_Genus")) %>%
                       dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
  ), 
  by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>% # add suffix to differentiate between sample metadata
  # Move columns around
  dplyr::select(order(colnames(.))) %>%
  dplyr::relocate(dplyr::any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
  dplyr::relocate(dplyr::contains(c("Tank")), .after = dplyr::last_col()) %>%
  dplyr::ungroup() %>%
  
  # Calculate the median distance of samples to all other samples (Measure of "personalization")
  dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
  dplyr::summarize(Beta.Score = median(Dist)*100, # multiply by 100 to ease normalization calculations (probably unneeded)
                   Beta.Score.Mean = mean(Dist)*100, # multiply by 100 to ease normalization calculations
                   count = dplyr::n()) %>% 
  dplyr::ungroup() %>%
  
  # Rename Columns
  dplyr::rename(Sample = Sample_a,
                Treatment = Treatment_a,
                Temperature = Temperature_a,
                DPE = DPE_a,
                Tank.ID = Tank.ID_a) %>%
  dplyr::right_join(tmp.psOBJ %>%
                      microViz::samdat_tbl() %>% 
                      dplyr::select(!contains("_Genus")),
                    by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
  dplyr::ungroup() %>%
  
  # Normalize diversity scores
  dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
  
  # Create a column to save what type of beta metric was calculated
  dplyr::mutate(Beta.Metric = "Bray-Curtis", .before = "Beta.Score")

data.beta.list[["Exposed"]] <- data.beta.list[["Exposed"]] %>%
  dplyr::full_join(., 
                   tmp.psOBJ %>% 
                     # tax_agg("Genus") %>%
                     microViz::dist_calc("canberra") %>% 
                     
                     # Get distance matric
                     microViz::dist_get() %>%
                     # Pat Schloss methods to view beta diversity matrix data
                     # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
                     
                     # Convert into a matrix object
                     as.matrix() %>%
                     
                     # Convert matrix into a tibble
                     tibble::as_tibble(rownames = "Sample_a") %>%
                     
                     # Convert dataframe into "long" format
                     tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
                     
                     # Remove any rows where names equal eachother
                     dplyr::filter(Sample_a != Sample_b) %>%
                     
                     # Merge sample dataframe
                     dplyr::right_join((tmp.psOBJ %>% 
                                          microViz::samdat_tbl() %>% 
                                          dplyr::ungroup() %>%  
                                          dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                                          dplyr::select(!contains("_Genus")) %>%
                                          dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
                     ), 
                     by = c("Sample_a" = "Sample")) %>%
                     dplyr::right_join((tmp.psOBJ %>% 
                                          microViz::samdat_tbl() %>% 
                                          dplyr::ungroup() %>%  
                                          dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                                          dplyr::select(!contains("_Genus")) %>%
                                          dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
                     ), 
                     by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>%
                     # Move columns around
                     dplyr::select(order(colnames(.))) %>%
                     dplyr::relocate(any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
                     dplyr::relocate(contains(c("Tank")), .after = dplyr::last_col()) %>%
                     dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
                     
                     # Calculate the median distance of samples to all other samples (Measure of "personalization")
                     dplyr::summarize(Beta.Score = median(Dist)*100, 
                                      Beta.Score.Mean = mean(Dist)*100, 
                                      count = dplyr::n()) %>% # multiply by 100 to ease normalization calculations
                     dplyr::ungroup() %>%
                     dplyr::rename(Sample = Sample_a,
                                   Treatment = Treatment_a,
                                   Temperature = Temperature_a,
                                   DPE = DPE_a,
                                   Tank.ID = Tank.ID_a) %>%
                     dplyr::right_join(tmp.psOBJ %>%
                                         microViz::samdat_tbl() %>% dplyr::select(!contains("_Genus")),
                                       by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
                     dplyr::ungroup() %>%
                     
                     # Normalize diversity scores
                     dplyr::mutate(dplyr::across(contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "Canberra", .before = "Beta.Score")
  )

data.beta.list[["Exposed"]] <- data.beta.list[["Exposed"]] %>%
  dplyr::full_join(., 
                   tmp.psOBJ %>% 
                     # tax_agg("Genus") %>%
                     microViz::dist_calc("gunifrac") %>% 
                     
                     # Get distance matric
                     microViz::dist_get() %>%
                     # Pat Schloss methods to view beta diversity matrix data
                     # Source: https://youtu.be/Gm-kg3TuML4?si=nYLLAxm7uv8aYG27&t=250
                     
                     # Convert into a matrix object
                     as.matrix() %>%
                     
                     # Convert matrix into a tibble
                     tibble::as_tibble(rownames = "Sample_a") %>%
                     
                     # Convert dataframe into "long" format
                     tidyr::pivot_longer(-Sample_a, names_to = "Sample_b", values_to = "Dist") %>%
                     
                     # Remove any rows where names equal eachother
                     dplyr::filter(Sample_a != Sample_b) %>%
                     
                     # Merge sample dataframe
                     dplyr::right_join((tmp.psOBJ %>% 
                                          microViz::samdat_tbl() %>% 
                                          dplyr::ungroup() %>%  
                                          dplyr::arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                                          dplyr::select(!contains("_Genus")) %>%
                                          dplyr::select(Sample, DPE, Treatment, Temperature, Tank.ID)
                     ), 
                     by = c("Sample_a" = "Sample")) %>%
                     dplyr::right_join((tmp.psOBJ %>% 
                                          microViz::samdat_tbl() %>% ungroup() %>%  arrange(DPE, Treatment, Temperature, Tank.ID) %>% 
                                          select(!contains("_Genus")) %>%
                                          select(Sample, DPE, Treatment, Temperature, Tank.ID)
                     ), 
                     by = c("Sample_b" = "Sample"), suffix = c("_a", "_b")) %>%
                     # Move columns around
                     dplyr::select(order(colnames(.))) %>%
                     dplyr::relocate(dplyr::any_of(c("Sample_a", "Sample_b", "Dist")), .before = 1) %>%
                     dplyr::relocate(dplyr::contains(c("Tank")), .after = dplyr::last_col()) %>%
                     dplyr::group_by(Sample_a, Treatment_a, Temperature_a, DPE_a, Tank.ID_a) %>%
                     
                     # Calculate the median distance of samples to all other samples (Measure of "personalization")
                     dplyr::summarize(Beta.Score = median(Dist)*100, 
                                      Beta.Score.Mean = mean(Dist)*100, 
                                      count = dplyr::n()) %>% # multiply by 100 to ease normalization calculations
                     dplyr::ungroup() %>%
                     dplyr::rename(Sample = Sample_a,
                                   Treatment = Treatment_a,
                                   Temperature = Temperature_a,
                                   DPE = DPE_a,
                                   Tank.ID = Tank.ID_a) %>%
                     dplyr::right_join(tmp.psOBJ %>%
                                         microViz::samdat_tbl() %>% dplyr::select(!contains("_Genus")),
                                       by = c("Sample", "DPE", "Treatment", "Temperature", "Tank.ID")) %>%
                     dplyr::ungroup() %>%
                     
                     # Normalize diversity scores
                     dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_alpha_score.2, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "G-Unifrac", .before = "Beta.Score")
  )



## Calc Distance Matrix ----------------------------------------------------

beta.dist.mat[["Exposed"]] <- # saves the results of this loop to a list
  furrr::future_map(div.mtd[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # ps.list[["All"]] is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                    rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(div.mtd[["beta"]]) # assigns names of the beta methods to the list 


# ## Permanova ---------------------------------------------------------------
# 
# 
# 
# ### Run PERMANOVA
# beta.perm[["Exposed"]][["Temp + DPE"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["Exposed"]][[x]] %>% # list of distance matrices for each beta method
#       dist_permanova(variables = c("Temperature", "DPE"), n_perms = 999, seed = 123) %>% # runs PERMANOVA test
#       perm_get() # Returns the permanova results
#   }) %>%
#   setNames(div.mtd[["beta"]])
# 
# beta.perm[["Exposed"]][["Temp + DPE"]][["Table"]] <- # Save table to a list
#   map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.perm[["Exposed"]][["Temp + DPE"]][["Stats"]][[x]] %>% tidy() %>% # tidy() converts stat results into dataframe
#       as_tibble() %>% # Make a tibble
#       mutate(metric = x, .before = term) %>% # Make a new column called "metric" and fill in which beta metric
#       mutate(sig = ifelse(p.value <= 0.05, "*", "")) # Add starts to a col called "sig" if significant
#   }) %>% bind_rows() %>% # Merge each beta dataframe into one dataframe
#   group_by(metric) 
# 
# 
# ## Hom Disp ----------------------------------------------------------------
# 
# 
# ### Temp + DPE ------------------------------------------------------
# 
# vars.list[["Exposed"]][["Temp + DPE"]] <- c("Temperature", "DPE")
# 
# beta.disp[["Exposed"]][["Temp + DPE"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["Exposed"]][[x]] %>% # list of distance matrices for each beta method
#       dist_bdisp(variables = c("Temperature", "DPE")) %>% # Runs Homogeneity of multivariate dispersions (HoMD)
#       bdisp_get() # Returns the HoMD results
#   }) %>%
#   setNames(div.mtd[["beta"]]) # Assigns names for each beta method to the list
# 
# 
# beta.disp[["Exposed"]][["Temp + DPE"]][["Table"]]["Anova"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["Exposed"]][["Temp + DPE"]], function(y){
#       
#       beta.disp[["Exposed"]][["Temp + DPE"]][["Stats"]][[x]][[y]][["anova"]] %>% 
#         tidy() %>%
#         mutate(metric = x, 
#                .before = 1) %>%
#         mutate(term = case_when(
#           term == "Groups" ~ y,
#           .default = term
#         )) %>%
#         mutate(Sig = ifelse(`p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows()
#   }) %>% bind_rows() %>% nest()
# 
# 
# #### Tukey
# 
# beta.disp[["Exposed"]][["Temp + DPE"]][["Table"]]["Tukey"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["Exposed"]][["Temp + DPE"]], function(y){
#       
#       beta.disp[["Exposed"]][["Temp + DPE"]][["Stats"]][[x]][[y]][["tukeyHSD"]] %>% 
#         tidy() %>%
#         mutate(metric = x, .before = term) %>%
#         mutate(term = y) %>%
#         mutate(Sig = ifelse(`adj.p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows() 
#   }) %>% bind_rows() %>% nest()
# 
# 
# 
# ### Treat.Temp.DPE ----------------------------------------------------------
# 
# vars.list[["Exposed"]][["Temp.DPE"]] <- c("Temp.DPE")
# 
# beta.disp[["Exposed"]][["Temp.DPE"]][["Stats"]] <- # Save results to a list
#   furrr::future_map(div.mtd[["beta"]], function(x){ # Loops over the diff beta methods in parallel
#     beta.dist.mat[["Exposed"]][[x]] %>% # list of distance matrices for each beta method
#       dist_bdisp(variables = c("Temp.DPE")) %>% # Runs Homogeneity of multivariate dispersions (HoMD)
#       bdisp_get() # Returns the HoMD results
#   }) %>%
#   setNames(div.mtd[["beta"]]) # Assigns names for each beta method to the list
# 
# 
# beta.disp[["Exposed"]][["Temp.DPE"]][["Table"]]["Anova"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["Exposed"]][["Temp.DPE"]], function(y){
#       
#       beta.disp[["Exposed"]][["Temp.DPE"]][["Stats"]][[x]][[y]][["anova"]] %>% 
#         tidy() %>%
#         mutate(metric = x, 
#                .before = 1) %>%
#         mutate(term = case_when(
#           term == "Groups" ~ y,
#           .default = term
#         )) %>%
#         mutate(Sig = ifelse(`p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows()
#   }) %>% bind_rows() %>% nest()
# 
# 
# #### Tukey
# 
# beta.disp[["Exposed"]][["Temp.DPE"]][["Table"]]["Tukey"] <- # Save results to a list
#   map(div.mtd[["beta"]], function(x){
#     map(vars.list[["Exposed"]][["Temp.DPE"]], function(y){
#       
#       beta.disp[["Exposed"]][["Temp.DPE"]][["Stats"]][[x]][[y]][["tukeyHSD"]] %>% 
#         tidy() %>%
#         mutate(metric = x, .before = term) %>%
#         mutate(term = y) %>%
#         mutate(Sig = ifelse(`adj.p.value` < 0.05, "*", "")) 
#       
#     }) %>% bind_rows() 
#   }) %>% bind_rows() %>% nest()
