# Misc Functions


# Save Environment File ---------------------------------------------------
#   Description: Finds the latest R environment file
#   Input: path name to Robjects directory
#   Output: saves environment file with current date

save_env <- function(
  obj.path, 
  ID = Sys.Date(), 
  extra_info = NA
  ){
  
  if(is.na(extra_info)){
    save.image(paste0(obj.path, "/Environment/environment_", ID, "_ENV.RData"))
  } else{
    save.image(paste0(obj.path, "/Environment/environment__", extra_info, ID, "_ENV.RData"))
  }
  
  # Save Session Info
  session_info <- capture.output(sessionInfo())
  
  # Write the captured output to a text file
  writeLines(session_info, paste0(obj.path, "/Environment/session_info__", extra_info, ID, ".txt") )
}



# P-value Format ----------------------------------------------------------
#   Description: Formats P-values for summary statistic tables
#     * P-values below a certain threshold will appear as "<0.001"
#   Input: 
#   Output: 

p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}




# GT Table Setting -------------------------------------------------------------------------
# Description: sets default settings for a gt table
# Input: dataframe, significant figures, variables to set threshold (single or multiple variables)
# Output: 

set_GT <- function(x, digits = 3, var, group.by = NULL){
  x %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!sym(group.by)) %>%
    gt::gt() %>%
      gt::fmt_number(
        decimals = digits, # round to 3 decimal places
        use_seps = FALSE
      ) %>%
      gt::sub_large_vals( # values above 0.25 will be simplified
        columns = !!var,
        threshold = 0.25) %>%
      gt::sub_small_vals( # values below 0.001 will be simplified
        columns = !!var,
        threshold = 0.001)
  }

# SigStars -------------------------------------------------------------------------
# Description: Adds a column with significance indicators
# Input: Tidy statistical dataframe
# Output: same dataframe with an extra column indicating level of significance with star indicators

SigStars <- function(x, # Tidy statistical dataframe
                     pval.var = "p.value" # May differ depending on statistical test output
                     ){
  x %>%
    dplyr::rename(p.value = !!pval.var) %>% # Dataframe clean up
    dplyr::mutate(p.adj.sig = case_when(
      p.value < 0.0001 ~ "****",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*", 
      p.value >= 0.05 ~ "ns")) # pvalues above 0.05 are not significant (ns)
}



# Cut Column Names -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

cutColNames <- function(df,
                        cols =,
                        sep = "__"
) {
  
  df %>%
    dplyr::rename_with(~ sub("__.*", "", .x), .col = cols)
  
}


# Cut Cell Names -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

cutCellNames <- function(df,
                        col = c(),
                        sep = "__"
) {
  
  df %>%
    dplyr::mutate(across(all_of(col), ~ sub(paste0(sep, ".*"), "", .)))
  
}


# Phyloseq Object to Dataframe Tests Pivot Long -------------------------------------------------------------------------
# Description: Converts a PS obj to a dataframe and pivots longer by alpha scores and metrics for statistical tests
# Input: Phyloseq Object
# Output: Dataframe

psObjToDfLong <- function(ps.obj, 
                          div.score, # Column name for where alpha scores are contained (e.g., Alpha.Score, Beta.Score)
                          div.metric, # column name for where alpha metric labels are contained (e.g., Alpha.Metric, Beta.Metric)
                          pivot.long_col = "_norm" # Common suffix in column names by which to pivot longer (e.g., "_Genus", or "_norm")
) {
  
  ps.obj %>%
    microViz::samdat_tbl() %>%
    # Merge the 
    tidyr::pivot_longer(cols = contains(pivot.long_col), 
                        names_to = div.metric, 
                        values_to = div.score) %>%
    dplyr::ungroup()
  
  
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




# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 




