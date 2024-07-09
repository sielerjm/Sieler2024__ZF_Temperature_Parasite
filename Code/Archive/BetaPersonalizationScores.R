
# Beta Personalization Score ----------------------------------------------


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
  dplyr::summarize(Beta.Score = median(Dist)*100,  
                   Beta.Score.Mean = mean(Dist)*100, 
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
  dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
  
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "G-Unifrac", .before = "Beta.Score")
  )


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
  dplyr::summarize(Beta.Score = median(Dist)*100,  
                   Beta.Score.Mean = mean(Dist)*100, 
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
  dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
  
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "G-Unifrac", .before = "Beta.Score")
  )




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
  dplyr::summarize(Beta.Score = median(Dist)*100,  
                   Beta.Score.Mean = mean(Dist)*100, 
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
  dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
  
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "G-Unifrac", .before = "Beta.Score")
  )


# Pre-Exposed --------------------------------------------------------------


## Calc Distance Matrix ----------------------------------------------------

tmp.psOBJ <- ps.list[["Exposed"]]



beta.dist.mat[["Exposed"]] <- # saves the results of this loop to a list
  furrr::future_map(div.mtd[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # ps.list[["All"]] is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(div.mtd[["beta"]]) # assigns names of the beta methods to the list 




# Pre-Exposed ---------------------------------------------------------------------


##  Calculate Personalization Scores ----------------------------------------------

# Save a temporary 
tmp.psOBJ <- ps.list[["PreExposed"]]

# Save dataframe
data.beta.list[["PreExposed"]] <-
  
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
  dplyr::summarize(Beta.Score = median(Dist)*100,  
                   Beta.Score.Mean = mean(Dist)*100, 
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
  dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
  
  # Create a column to save what type of beta metric was calculated
  dplyr::mutate(Beta.Metric = "Bray-Curtis", .before = "Beta.Score")

data.beta.list[["PreExposed"]] <- data.beta.list[["PreExposed"]] %>%
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "Canberra", .before = "Beta.Score")
  )

data.beta.list[["PreExposed"]] <- data.beta.list[["PreExposed"]] %>%
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "G-Unifrac", .before = "Beta.Score")
  )



# Post-Exposed ------------------------------------------------------------


##  Calculate Personalization Scores ----------------------------------------------

# Save a temporary 
tmp.psOBJ <- ps.list[["PostExposed"]]

# Save dataframe
data.beta.list[["PostExposed"]] <-
  
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
  dplyr::summarize(Beta.Score = median(Dist)*100,  
                   Beta.Score.Mean = mean(Dist)*100, 
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
  dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
  
  # Create a column to save what type of beta metric was calculated
  dplyr::mutate(Beta.Metric = "Bray-Curtis", .before = "Beta.Score")

data.beta.list[["PostExposed"]] <- data.beta.list[["PostExposed"]] %>%
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "Canberra", .before = "Beta.Score")
  )

data.beta.list[["PostExposed"]] <- data.beta.list[["PostExposed"]] %>%
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
                                      count = dplyr::n()) %>% 
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
                     dplyr::mutate(dplyr::across(dplyr::contains("Beta.Score"), norm_scores, .names = "{.col}_norm"), .after = "Beta.Score") %>%
                     
                     # Create a column to save what type of beta metric was calculated
                     dplyr::mutate(Beta.Metric = "G-Unifrac", .before = "Beta.Score")
  )





