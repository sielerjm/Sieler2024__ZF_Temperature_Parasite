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
                        cols,
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


# Print Tables -------------------------------------------------------------------------
# Description: 
# Input: list of GT tables
# Output: Prints GT tables to screen

print_tables <- function(x, parent_name = "") {
  if (is.list(x)) {
    for (name in names(x)) {
      new_parent_name <- paste0(parent_name, if (parent_name != "") "::", name)
      if (grepl("Table", name)) {
        print(paste0("Table found: ", new_parent_name))
        print(x[[name]])
      } else {
        print_tables(x[[name]], new_parent_name)
      }
    }
  }
}


# Save Tables PDF -------------------------------------------------------------------------
# Description: 
# Input: list of GT tables
# Output: Saves pdfs to desired location

save_tables_as_files <- function(x, parent_name = "", output_dir = "output", counter = 1, suffix = "", format = "pdf") {
  pdf_dir <- file.path(output_dir, "PDF")
  png_dir <- file.path(output_dir, "PNG")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  if (format == "pdf" && !dir.exists(pdf_dir)) {
    dir.create(pdf_dir, recursive = TRUE)
  }
  
  if (format == "png" && !dir.exists(png_dir)) {
    dir.create(png_dir, recursive = TRUE)
  }
  
  
  if (is.list(x)) {
    for (name in names(x)) {
      new_parent_name <- paste0(parent_name, if (parent_name != "") "::", name)
      if (grepl("Table", name)) {
        file_name <- gsub("[::\\.]", "-", new_parent_name)  # Replace "::" and "." with "-"
        sub_dir <- if (format == "pdf") pdf_dir else png_dir
        file_path <- file.path(sub_dir, paste0(suffix, "_", counter, "_", file_name, ".", format))
        cat("Saving Table:", file_path, "\n")
        
        # Save the gt table as PDF or PNG
        gt_table <- x[[name]]
        if (format == "pdf") {
          gtsave(data = gt_table, filename = file_path)
        } else if (format == "png") {
          gtsave(data = gt_table, filename = file_path, vwidth = 800, vheight = 600, expand = 0)
        } else {
          stop("Unsupported format. Use 'pdf' or 'png'.")
        }
        
        counter <- counter + 1  # Increment the counter for the next table
      } else {
        counter <- save_tables_as_files(x[[name]], new_parent_name, output_dir, counter, suffix, format)
      }
    }
  }
  
  return(counter)
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




