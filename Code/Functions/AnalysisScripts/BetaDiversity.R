
# Beta Diversity ----------------------------------------------------------
#   - Calculate Distance Matrices
#   - Permanova
#   - Beta Dispersion

## Variables, lists, etc.
beta.dist.mat <- list() # Saves distnace matrix values here


## Run purrr::map() in parallel
future::plan(future::multisession, workers = (detectCores()-1) )



# All ---------------------------------------------------------------------

cat("Calculating beta diversity scores: all")

## Calc Distance Matrix ----------------------------------------------------

tmp.psOBJ <- ps.list[["All"]]

beta.dist.mat[["All"]] <- # saves the results of this loop to a list
  furrr::future_map(diversity.method[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                    rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(diversity.method[["beta"]]) # assigns names of the beta methods to the list 



# Unexposed ---------------------------------------------------------------

cat("Calculating beta diversity scores: unexposed")

## Calc Distance Matrix ----------------------------------------------------

tmp.psOBJ <- ps.list[["Unexposed"]]

beta.dist.mat[["Unexposed"]] <- # saves the results of this loop to a list
  furrr::future_map(diversity.method[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                    rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(diversity.method[["beta"]]) # assigns names of the beta methods to the list 


# Exposed -----------------------------------------------------------------

cat("Calculating beta diversity scores: exposed")

## Calc Distance Matrix ----------------------------------------------------

tmp.psOBJ <- ps.list[["Exposed"]]

beta.dist.mat[["Exposed"]] <- # saves the results of this loop to a list
  furrr::future_map(diversity.method[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(diversity.method[["beta"]]) # assigns names of the beta methods to the list 

# Pre-Exposed -----------------------------------------------------------------

cat("Calculating beta diversity scores: pre-exposed")

## Calc Distance Matrix ----------------------------------------------------

tmp.psOBJ <- ps.list[["PreExposed"]]

beta.dist.mat[["PreExposed"]] <- # saves the results of this loop to a list
  furrr::future_map(diversity.method[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(diversity.method[["beta"]]) # assigns names of the beta methods to the list 

# Post-Exposed ---------------------------------------------------------------------

cat("Calculating beta diversity scores: post-exposed")

## Calc Distance Matrix ----------------------------------------------------

tmp.psOBJ <- ps.list[["PostExposed"]]

beta.dist.mat[["PostExposed"]] <- # saves the results of this loop to a list
  furrr::future_map(diversity.method[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # ps.list[["PostExposed"]] is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(diversity.method[["beta"]]) # assigns names of the beta methods to the list 


# TimeFinal ---------------------------------------------------------------

cat("Calculating beta diversity scores: final timepoint")

## Calc Distance Matrix ----------------------------------------------------

tmp.psOBJ <- ps.list[["TimeFinal"]]

beta.dist.mat[["TimeFinal"]] <- # saves the results of this loop to a list
  furrr::future_map(diversity.method[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(diversity.method[["beta"]]) # assigns names of the beta methods to the list 


# InitialFinal ------------------------------------------------------------


cat("Calculating beta diversity scores: initial vs final timepoint")

## Calc Distance Matrix ----------------------------------------------------

tmp.psOBJ <- ps.list[["InitialFinal"]]

beta.dist.mat[["InitialFinal"]] <- # saves the results of this loop to a list
  furrr::future_map(diversity.method[["beta"]], function(x){ # Future_map runs the function in parallel
    tmp.psOBJ %>% # is the phyloseq object we are looping over
      microViz::tax_transform(trans = "identity", # tax transform transforms taxa counts
                              rank = ifelse(x == "gunifrac" || x == "wunifrac" || x == "unifrac", "unique", "Genus")) %>% # phylogenetic methods need "unique" ASV counts
      microViz::dist_calc(x, gunifrac_alpha = 0.5) # calculates the distance between samples. gunifrac_alpha = 0.5 weights abundance into its calculation
  }) %>%
  setNames(diversity.method[["beta"]]) # assigns names of the beta methods to the list 

