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






# Calculate Phylogenetic Diversity -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


ps_calc_diversity.phy <- function(ps,
                              index = "Phylogenetic",
                              varname = index
                              ) {
  
  # Note, not possible to aggregate by Genus
  #   - See this GitHub issue: https://github.com/david-barnett/microViz/issues/143#issuecomment-1999688881
  
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



# Run GLM (Alpha) -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

run_glm_models <- function(data, alpha_metric_col = "Alpha.Metric", alpha_score_col = "Alpha.Score", formula_str, family_str = "quasibinomial") {
  # Extract unique alpha metrics
  unique_metrics <- data %>% select(all_of(alpha_metric_col)) %>% distinct() %>% pull()
  
  # Run GLM models for each unique alpha metric
  glm_models <- purrr::map(unique_metrics, function(x){
    glm( formula = as.formula(formula_str),
         data = subset(data, Alpha.Metric == x),
         family = family_str)
  }) %>% setNames(unique_metrics) 
  
  return(glm_models)
}

run_glm.nb_models <- function(data, alpha_metric_col = "Alpha.Metric", alpha_score_col = "Alpha.Score", formula_str) {
  # Extract unique alpha metrics
  unique_metrics <- data %>% select(all_of(alpha_metric_col)) %>% distinct() %>% pull()
  
  # Run GLM models for each unique alpha metric
  glm.nb_models <- purrr::map(unique_metrics, function(x){
    # glm( formula = as.formula(formula_str),
    #      data = subset(data, Alpha.Metric == x),
    #      family = family_str)
    MASS::glm.nb(formula = as.formula(formula_str), data = subset(data, Alpha.Metric == x))
  }) %>% setNames(unique_metrics) 
  
  return(glm.nb_models)
}



# Run GLM ANOVA (Alpha) -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

run_glm_anova <- function(models, alpha_metric_col = "Alpha.Metric") {
  # Extract unique alpha metrics
  unique_metrics <- names(models)
  
  # Run GLM models for each unique alpha metric
  anova.table <- purrr::map(unique_metrics, function(x){
    models[[x]] %>% Anova(type = 2) %>%
      broom::tidy() %>%
      dplyr::mutate(Alpha.Metric = x, .before = 1)  %>%
      dplyr::mutate(sig = case_when(
        p.value <= 0.0001 ~ "****",
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value < 0.05 ~ "*", 
        p.value >= 0.05 ~ "ns"))
  }) %>% setNames(unique_metrics) %>%
    dplyr::bind_rows()
  
  return(anova.table)
}


# Run Tukey GLM (Alpha) -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


run_tukey_glm <- function(data, alpha_score_col, alpha_metric_col, variables, group_by_var = NULL) {
  
  # Helper function to run GLM and Tukey test
  run_glm_tukey <- function(metric_data, var, alpha_score_col, group_var = NULL) {
    if (!is.null(group_var)) {
      results <- metric_data %>%
        group_by(.data[[group_var]]) %>%
        group_modify(~ {
          .x[[var]] <- as.factor(.x[[var]])  # Convert to factor if not already
          formula <- as.formula(paste(alpha_score_col, "~", var))
          glm_fit <- glm(formula, data = .x, family = quasibinomial)
          mcp_arg <- eval(parse(text = paste0("mcp(", var, " = \"Tukey\")")))
          tukey_test <- glht(glm_fit, linfct = mcp_arg)
          tidy(tukey_test) %>%
            mutate(Variable = var, Group = .y[[group_var]])
        }) %>%
        ungroup() 
    } else {
      metric_data[[var]] <- as.factor(metric_data[[var]])  # Convert to factor if not already
      formula <- as.formula(paste(alpha_score_col, "~", var))
      glm_fit <- glm(formula, data = metric_data, family = quasibinomial)
      mcp_arg <- eval(parse(text = paste0("mcp(", var, " = \"Tukey\")")))
      tukey_test <- glht(glm_fit, linfct = mcp_arg)
      results <- tidy(tukey_test) %>%
        mutate(Variable = var)
    }
    return(results) 
  }
  
  # Get unique alpha metrics
  unique_metrics <- unique(data[[alpha_metric_col]])
  
  results <- unique_metrics %>%
    set_names() %>%
    map_df(~ {
      metric_data <- filter(data, .data[[alpha_metric_col]] == .x)
      other_vars <- setdiff(variables, group_by_var)
      other_vars %>%
        set_names() %>%
        map_df(~ run_glm_tukey(metric_data, .x, alpha_score_col, group_by_var)) %>%
        mutate(Alpha.Metric = .x) %>%
        dplyr::select(-any_of("null.value")) %>%
        tidyr::separate(contrast, c('group1', 'group2'), sep = " - ") %>% # Dataframe clean up
        dplyr::mutate(`.y.` = "Alpha.Score", .after = 1) 
    })
  
  return(results) 
}




# Levene's test (Alpha) -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

run_leveneTest <- function(data, alpha_metric_col = "Alpha.Metric", alpha_score_col = "Alpha.Score", formula_str) {
  # Extract unique alpha metrics
  unique_metrics <- data %>% select(all_of(alpha_metric_col)) %>% distinct() %>% pull()
  
  # Run GLM models for each unique alpha metric
  test.res <- purrr::map(unique_metrics, function(x){
    car::leveneTest( formula = as.formula(formula_str),
                     data = subset(data, Alpha.Metric == x)
         )
  }) %>% setNames(unique_metrics) 
  
  return(test.res)
}

run_tukey_glm <- function(data, alpha_score_col, alpha_metric_col, variables, group_by_var = NULL) {
  
  # Helper function to run GLM and Tukey test
  run_glm_tukey <- function(metric_data, var, alpha_score_col, group_var = NULL) {
    if (!is.null(group_var)) {
      results <- metric_data %>%
        group_by(.data[[group_var]]) %>%
        group_modify(~ {
          .x[[var]] <- as.factor(.x[[var]])  # Convert to factor if not already
          formula <- as.formula(paste(alpha_score_col, "~", var))
          glm_fit <- glm(formula, data = .x, family = quasibinomial)
          mcp_arg <- eval(parse(text = paste0("mcp(", var, " = \"Tukey\")")))
          tukey_test <- glht(glm_fit, linfct = mcp_arg)
          tidy(tukey_test) %>%
            mutate(Variable = var, Group = .y[[group_var]])
        }) %>%
        ungroup() 
    } else {
      metric_data[[var]] <- as.factor(metric_data[[var]])  # Convert to factor if not already
      formula <- as.formula(paste(alpha_score_col, "~", var))
      glm_fit <- glm(formula, data = metric_data, family = quasibinomial)
      mcp_arg <- eval(parse(text = paste0("mcp(", var, " = \"Tukey\")")))
      tukey_test <- glht(glm_fit, linfct = mcp_arg)
      results <- tidy(tukey_test) %>%
        mutate(Variable = var)
    }
    return(results) 
  }
  
  # Get unique alpha metrics
  unique_metrics <- unique(data[[alpha_metric_col]])
  
  results <- unique_metrics %>%
    set_names() %>%
    map_df(~ {
      metric_data <- filter(data, .data[[alpha_metric_col]] == .x)
      other_vars <- setdiff(variables, group_by_var)
      other_vars %>%
        set_names() %>%
        map_df(~ run_glm_tukey(metric_data, .x, alpha_score_col, group_by_var)) %>%
        mutate(Alpha.Metric = .x) %>%
        dplyr::select(-any_of("null.value")) %>%
        tidyr::separate(contrast, c('group1', 'group2'), sep = " - ") %>% # Dataframe clean up
        dplyr::mutate(`.y.` = "Alpha.Score", .after = 1) 
    })
  
  return(results) 
}

# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 



# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


