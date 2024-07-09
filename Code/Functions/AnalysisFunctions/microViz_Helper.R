
# MicroViz Helper Functions -----------------------------------------------

samdatAsDataframe <- function(ps) {
  samdat <- phyloseq::sample_data(ps)
  df <- data.frame(samdat, check.names = FALSE, stringsAsFactors = FALSE)
  return(df)
}



# PS Pivot Longer ----------------------------------------------------------


ps_pivot_longer <- function(ps, cols = c(), names_to = "", values_to = "", ...) {
  # microViz::check_is_phyloseq(ps, argName = "ps")
  
  df <- samdatAsDataframe(ps)
  # saved_rownames <- rownames(df)
  df <- tidyr::pivot_longer(df, cols = cols, names_to = names_to, values_to = values_to, ...)
  # rownames(df) <- saved_rownames
  ps@sam_data <- phyloseq::sample_data(df) # should work for psExtra and phyloseq
  
  return(ps)
}



# PS Calc Diversity (Phylogenetic) -------------------------------------------------------------------------
# Description: 
# Improve: 
#   - Dynamic rank input vis-a-vis: https://github.com/david-barnett/microViz/blob/11cdd447c4e7d59a75f7194b249f4ddb1b942867/R/ps_calc_diversity.R#L55

ps_calc_diversity.phy <- function(ps,
                                  index = "phylogenetic",
                                  varname = tolower(index),
                                  use.root = T
) {
  
  df <- picante::pd(samp = otu.matrix(ps), tree = phyloseq::phy_tree(ps), include.root = use.root) %>% 
    rename(phylogenetic_Genus = PD) %>%
    select(-SR)  # Remove richness column
  
  
  
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
  ps <- ps_join(
    x = ps, y = df, type = "left", .keep_all_taxa = TRUE,
    match_sample_names = ".rownames.", keep_sample_name_col = FALSE
  )
  return(ps)
}


# Rowname Fixer -----------------------------------------------------------


ps_rowname_fix <- function(ps, tmp.sampCol = "Sample"){
  
  df <- samdatAsDataframe(ps) %>% remove_rownames() %>% column_to_rownames(var = tmp.sampCol)
  
  # rownames(df) <- saved_rownames 
  ps@sam_data <- phyloseq::sample_data(df) # should work for psExtra and phyloseq
  return(ps)
  
}



# ps_mutate_grp -----------------------------------------------------------

ps_mutate_grp <- function(ps, tmp.group, ...) {
  # check_is_phyloseq(ps, argName = "ps")
  
  tmp.group_var <- enquo(tmp.group)
  
  df <- samdatAsDataframe(ps)
  # saved_rownames <- rownames(df)
  df <- dplyr::group_by(df, !!tmp.group_var) %>% dplyr::mutate(., ...)
  # rownames(df) <- saved_rownames
  ps@sam_data <- phyloseq::sample_data(df) # should work for psExtra and phyloseq
  
  return(ps)
}
