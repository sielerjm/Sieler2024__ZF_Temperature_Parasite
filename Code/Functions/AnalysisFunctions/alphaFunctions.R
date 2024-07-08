# Alpha Diversity Functions




# Calculate Alpha Scores --------------------------------------------------
#   Description: Generates alpha diversity scores from a list of alpha methods
#   Input: phyloseq object, list of alpha div. methods, 
#   Output: dataframe of alpha-diversity scores

alpha_base <- function(
    physeq,  # Phyloseq object
    methods,  # List of alpha methods (e.g., c(Shannon, Simpson, Observed) )
    smpl.col.name = "Sample",  # Default is "Sample" but you can change it to whatever when you call the function
    phylo.div = T  # Only set to false if you don't have phylogenetic information attached to your phyloseq object
){
  
  # Calculates alpha scores
  tmp.dt <- phyloseq::estimate_richness(
    physeq = physeq,  # Physeq object
    measures = methods
  ) %>% as.data.table(keep.rownames = smpl.col.name) %>% setkeyv(smpl.col.name)  # Sets sample column name to whatever you've called it (e.g., "Sample" or "Sample.ID")
  tmp.dt[, se.chao1 := NULL] # No idea what this does, but it's from Keatons code and I think it's important if you calculate se.chao1 scores
  
  # If you have phylogenetic information in your phyloseq object you'll want to set phylo.div to true
  if(isTRUE(phylo.div)){
    
    print("Calculating phylogenetic diversity, takes a while...")
    
    # Calculate the sum of the total phylogenetic branch length for one or multiple samples. See ?picante::pd() for more info
    #   - Returns a dataframe of the PD and species richness (SR) values for all samples
    phy.dt <- picante::pd(samp = otu.matrix(physeq), tree = phyloseq::phy_tree(physeq)) %>%
      select(-SR) %>%  # Deselects "SR" (Species Richness) since we already included it.
      rename(Phylogenetic = PD) %>%  # renames "PD" to "Phylogenetic"
      as.data.table(keep.rownames = smpl.col.name) %>% setkeyv(smpl.col.name)  # set col name for samples column
    
    tmp.dt <- tmp.dt %>% 
      inner_join(., phy.dt, by = smpl.col.name)
    
    # return to sender
    return (tmp.dt)
  }
  
  # Returns alpha scores datatable
  return (tmp.dt)
}


# Normalize Alpha Scores --------------------------------------------------
#   Description: normalizes alpha diversity scores based on their distributions
#   Input: dataframe of alpha diversity scores, metadata table
#   Output: normalized datatable of alpha scores (0 to 1)

norm_alpha_score <- function(
    alpha.base, 
    sample.df, 
    methods,
    smpl.col.name = "Sample"
){
  
  # Makes a copy of the dataframe you input and adds a column for your sample IDs
  model.data.base <- copy(alpha.base[alpha.base[[smpl.col.name]] %in% 
                                       row.names(sample.df)])
  
  # Loops through the different alpha methods
  for (alpha in methods) {
    
    # ad.test(): Performs the Anderson-Darling test for the composite hypothesis of normality
    #   - Basically checking to see if the alpha score distribution that was calculated previously follows a normal distribution or not
    #   - Check this out for more info: ?ad.test()
    if (nortest::ad.test(model.data.base[[alpha]])$p.value <= 0.05) {
      
      # If the alpha scores do not follow a normal distribution, then you transform it using Tukey's (not Turkey's) power of ladders
      #   - This will transform your data as closely to a normal distribution
      
      # Sub-function to transform data that isn't normally distributed using Tukey's (not Turkey's) power of ladders
      #   - Check this out for more info: ?transformTukey()
      trans <- rcompanion::transformTukey(model.data.base[[alpha]], plotit = F, quiet = F, statistic = 2)
      trans <- (trans-min(trans))/(max(trans)-min(trans))   # Fixes normalization 0 to 1
      
      # Runs ad.test again to see if data returns higher than 0.05 p-value, if true then it transforms your data along tukey's power of ladders
      if (nortest::ad.test(trans)$p.value > 0.05) {
        model.data.base[[alpha]] <- trans  # Transorm data with transformTukey() above
        print(paste0("Finished: ", alpha))  # Letting you know what it's working on
        
        # If your data is now normally distributed it will return < 0.05 p.val, and then it uses max/min values to distribute the scores along a 0 and 1 scale.
      } else {
        model.data.base[[alpha]] <- (model.data.base[[alpha]] - min(model.data.base[[alpha]] ))/(max(model.data.base[[alpha]] )-min(model.data.base[[alpha]] ))  # Fixes normalization 0 to 1
        print(paste0("Finished: ", alpha))  # Letting you know what it's working on
      } 
      
      # If your data is already normally distributed, then it uses max/min values to distribute the scores along a 0 and 1 scale.
    } else {
      model.data.base[[alpha]] <- (model.data.base[[alpha]] - min(model.data.base[[alpha]] ))/(max(model.data.base[[alpha]] )-min(model.data.base[[alpha]] ))  # Fixes normalization 0 to 1
      print(paste0("Finished: ", alpha))  # Letting you know what it's working on
    }
  }
  
  # Sends your data back normalized from 0 to 1
  return(model.data.base)
}



# Norm Alpha Score 2.0 ----------------------------------------------------
#   Description: normalizes alpha diversity scores using MicroViz functionality
#   Input: dataframe of alpha diversity scores, metadata table
#   Output: normalized datatable of alpha scores (0 to 1)

norm_alpha_score.2 <- function(x) {
  
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




# Alpha DataTable -------------------------------------------------------------------------
# Description: Merges alpha diversity scores with metadata table
# Input: Metadata table and normalized alpha diversity scores
# Output: Merged dataframe with your metadata and alpha diversity scores (alpha measures are in separate columns)

alpha_dataTable <- function (tmp.dataTable,
                             tmp.normScores,
                             smpl.col.name = "Sample"){
  return(
    tmp.dataTable %>%
      arrange(smpl.col.name) %>%
      inner_join(., tmp.normScores, by = smpl.col.name)
  )
  
}


# Pivot longer sample data table -------------------------------------------------------------------------
# Description: Creates a "long" version of your datatable with alpha diversity scores and measures
# Input: Metadata table and normalized alpha diversity scores (as separate columns)
# Output: Metadata table with all alpha diversity scores in a single long column

melt_alphaDataTable <- function(tmp.dataTable, tmp.methods = methods.alpha) {


  return(
    tmp.dataTable.melt <- tmp.dataTable %>%
        tidyr::pivot_longer(cols = names(tmp.methods), # List of column names containing alpha metrics and scores e.g. c("Observed", "Shannon", "Simpson") or methods.alpha
                 names_to = "Alpha.Metric", # Column name for alpha metrics
                 values_to = "Alpha.Score" # Column name for alpha scores
    ) %>%
    arrange(Alpha.Metric) # Sort datatable by alpha metric
  )
}

# melt_alphaDataTable <- function(tmp.dataTable) {
# 
#   
#   return(
#     tmp.dataTable.melt <- tmp.dataTable %>%
#     pivot_longer(cols = methods.alpha,    # List of column names containing alpha metrics and scores e.g. c("Observed", "Shannon", "Simpson") or methods.alpha
#                  names_to = "Alpha.Metric",   # Column name for alpha metrics
#                  values_to = "Alpha.Score"  # Column name for alpha scores
#     ) %>% 
#     arrange(Alpha.Metric)  # Sort datatable by alpha metric
#   )
# }



# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


ps_calc_diversity.phy <- function(ps,
                              index = "phylogenetic",
                              varname = index
                              ) {
  
  df <- picante::pd(samp = otu.matrix(ps), tree = phyloseq::phy_tree(ps)) %>% 
    dplyr::rename(phylogenetic_Genus = PD) %>%
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


# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 



# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

