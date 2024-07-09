# Alpha Diversity Functions



# Norm Scores ----------------------------------------------------
#   Description: normalizes alpha diversity scores using MicroViz functionality
#   Input: dataframe of alpha diversity scores, metadata table
#   Output: normalized datatable of alpha scores (0 to 1)

norm_scores <- function(x) {
  
  ### Exampl of how to run function in your code ###
  # 
  # PS.obj %>%
  #   microViz::ps_mutate(across(contains("_Taxon"),  ## Grab columns with this substring
  #                    norm_alpha_score, ## THIS is the function
  #                    .names = "{.col}_norm"))  ## add columns and rename appending '_norm" 
  # 
  #######
  
  if (nortest::ad.test(x)$p.value <= 0.05) {
    # If the alpha scores do not follow a normal distribution, then you transform it using Tukey's (not Turkey's) power of ladders
    #   - This will transform your data as closely to a normal distribution
    
    # Sub-function to transform data that isn't normally distributed using Tukey's (not Turkey's) power of ladders
    #   - Check this out for more info: ?transformTukey()
    x.trans <- rcompanion::transformTukey(x, plotit = F, quiet = F, statistic = 2)
    
    x.trans.norm <- (x.trans-min(x.trans, na.rm = TRUE))/(max(x.trans, na.rm = TRUE)-min(x.trans, na.rm = TRUE))   # Fixes normalization 0 to 1
    
    # Runs ad.test again to see if data returns higher than 0.05 p-value, if true then it transforms your data along tukey's power of ladders
    
    if (nortest::ad.test(x.trans.norm)$p.value < 0.05) {
      
      x.trans.norm <- (x.trans.norm - min(x.trans.norm, na.rm = TRUE ))/(max(x.trans.norm, na.rm = TRUE )-min(x.trans.norm, na.rm = TRUE )) 
      
      # Sends your data back normalized from 0 to 1
      return(x.trans.norm)
      
      # If your data is now normally distributed it will return < 0.05 p.val, and then it uses max/min values to distribute the scores along a 0 and 1 scale.
    } else {
      
      # Sends your data back normalized from 0 to 1
      return(x.trans.norm)
    }
    
    # If your data is already normally distributed, then it uses max/min values to distribute the scores along a 0 and 1 scale.
  } else {
    
    x.norm <- (x - min(x, na.rm = TRUE ))/(max(x, na.rm = TRUE )-min(x, na.rm = TRUE ))  # Fixes normalization 0 to 1
    # print(paste0("Finished: ", alpha))  # Letting you know what it's working on
    return(x.norm)
  }
  
}






# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


ps_calc_diversity.phy <- function(ps,
                              index = "phylogenetic",
                              varname = index
                              ) {
  
  df <- picante::pd(samp = otu.matrix(ps), tree = phyloseq::phy_tree(ps)) %>% 
    dplyr::rename(!!varname := PD) %>%
    dplyr::select(-SR)  # Remove richness column
  
  
  
  # rename diversity variable
  # colnames(df)[colnames(df) == index] <- varname
  # add rownames column to join by (as it is the sample names)
  df[[".rownames."]] <- rownames(df)
  
  # check if varname is already in the phyloseq sample data
  if (varname %in% phyloseq::sample_variables(ps)) {
    warning(
      varname, " is already a variable in phyloseq sample data -> OVERWRITING"
    )
    phyloseq::sample_data(ps)[[varname]] <- NULL
  }
  
  # join diversity index variable to (original & unaggregated) phyloseq
  ps <- microViz::ps_join(
    x = ps, y = df, type = "left", .keep_all_taxa = TRUE,
    match_sample_names = ".rownames.", keep_sample_name_col = FALSE
  )
  return(ps)
}


# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


