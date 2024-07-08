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
    save.image(paste0(obj.path, "/environment", ID, "_ENV.RData"))
  } else{
    save.image(paste0(obj.path, "/environment_", extra_info, ID, "_ENV.RData"))
  }
}



# -------------------------------------------------------------------------
# Description: Performs wilcoxon tests
# Input: 
# Output: 


pair.wilcox.test_gen <- function(
    data,
    respVar,
    expVar1,
    expVar2 = NULL,
    p.adj.method = "BH"
){
  
  if(is.null(expVar2)){
    
    test.res <-  pairwise.wilcox.test(x=eval(parse(text = paste0("data$", respVar))),
                                      g=eval(parse(text = paste0("data$", expVar1))),
                                      p.adjust.method = p.adj.method) %>% tidy() %>%
      mutate(Response = respVar,
             Explanatory = expVar1, 
             .before = 1) %>%
      mutate(sig = ifelse(p.value <= 0.05, "*", "")) %>% arrange(Explanatory) %>%
      flextable() %>%
      # merge_v(j = c(1)) %>%
      set_caption(paste0("Pairwise wilcoxon, p.adj: BH. ", respVar, " ~ ", expVar1)) %>%
      set_formatter(values = list("p.value" = p_val_format) )
    
  } else {
    test.res <- lapply(unique(data[[expVar1]]), function(y){
      data <- subset(data, eval(parse(text = expVar1)) == y)
      pairwise.wilcox.test(x=eval(parse(text = paste0("data$", respVar))),
                           g=eval(parse(text = paste0("data$", expVar2))),
                           p.adjust.method = p.adj.method) %>% tidy() %>% 
        mutate(Response = respVar,
               Explanatory = y, 
               .before = 1) %>%
        mutate(sig = ifelse(p.value <= 0.05, "*", ""))
    }) %>% bind_rows() %>% arrange(Explanatory) %>%
      flextable() %>%
      # merge_v(j = c(1)) %>% 
      set_caption(paste0("Pairwise wilcoxon, p.adj: (", p.adj.method,") ", respVar, " ~ ", expVar1, ":", expVar2)) %>%
      set_formatter(values = list("p.value" = p_val_format) )
  }
  
  return(test.res)
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



# Sub-char in variable ----------------------------------------------------
# Description: 
# Input: 
# Output: 

sub_char_var <- function(var, old.char, new.char){
  
  tmp.var <- var
  if(old.char == "."){
    
    print(paste0("Replacing period with ", new.char))  # TEST
    tmp.var.new <- gsub("\\.", new.char, # replaces periods 9.) with space
                        tmp.var
    )
  } else{
    
    print(paste0("Replacing ", old.var," with ", new.char))  # TEST
    tmp.var.new <- gsub(old.char, new.char, 
                        tmp.var
    )
  }
  
  return(tmp.var.new) 
}



# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


full_dbrda_model  <- function(vars, distance, methods, names, physeq, data, terms = 1, num.cores = num.cores){  
  
  # Starts parallel computing
  cl <- makeCluster(num.cores, type = "FORK", outfile = "")
  registerDoParallel(cl, num.cores)
  
  # Build full model
  beta.model.full <- foreach(
    beta = methods,
    .final = names,
    .verbose = TRUE
  ) %dopar% {
    # progress_Bar_Start(which(methods == beta), length(methods))  # Progress bar
    dist.mat <- distance[[beta]]
    form <- if(length(vars) == 1){
      paste0("dist.mat ~ ", vars)
    } else{paste0("dist.mat ~ (", paste0(vars, collapse = "+") ,")^", terms)}
    print(form)  #  TEST
    capscale(as.formula(form),
             data = data,
             na.action = na.omit # na.omit only non-missing site scores are shown
             #comm = otu.matrix(physeq)  # Error might originate here, I think
    )
  }
  
  stopCluster(cl)
  return(beta.model.full)
  
}


# Stats Table -------------------------------------------------------
#   Description: produces a statistical table from anova stats
#   Input: anova results, variables, terms
#   Output: statistical table of anova results 

stats_table <- function(dataframe, terms = NA, hline.num = NA, formula = NA, stat.desc = F){
  
  # Arrange dataframe by most to least significant
  if(!"sig" %in% colnames(dataframe)){
    dataframe<- dataframe %>%
      tidy() %>%
      mutate(sig = ifelse(p.value <= 0.05, "*", "")) %>%
      # arrange(desc(statistic))  # highest to lowest effect size
      arrange(if(!isTRUE(stat.desc)) TRUE else desc(statistic)) 
  }
  
  if(is.na(hline.num)){
    hline.num = seq(1, nrow(dataframe) -1)
  } 
  
  
  # caption <- paste0("beta score ~ (", paste0(var, collapse = "+"), ")^", terms)
  return (dataframe %>%
            flextable() %>%
            set_caption(caption = ifelse(is.na(formula), "", formula)) %>%
            #set_caption(caption = table.caption(caption)) %>%  # Uses autoNumCaptions
            # align(j = c(1, ncol(dataframe)), align = "left") %>%
            align(j = 2:5, align = "right") %>%
            colformat_double(j = 3, digits = 2) %>%
            colformat_double(j = 5, digits = 3) %>%
            merge_v(j = 1) %>%
            hline(i = hline.num, j = NULL, border = NULL, part = "body") %>%
            set_formatter(values = list("p.value" = p_val_format) ) %>%
            autofit()
  )
}

# Get variable names ------------------------------------------------------
# Description: Extracts a list of variables you want from a dataframe
# Input: dataframe 
# Output: list of variables from column 1 if subset.type condition met

# Extracts variables with more broadness

get_Variables <- function(data.vars, subset.type = NULL, col.1 = 1, col.2 = 2){
  # Expects a two column df/dt, but can handle any size if col's specified
  
  # returns a list of variables
  list.vars <- data.vars %>%
    
    # If subset.type is not specified, returns all variables in col.1
    filter(if (is.null(subset.type)) TRUE else across(col.2) == subset.type) %>% 
    pull(col.1)
  return(list.vars)
}


# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

gen_glm_anova <- function(tmp.mod, tmp.metric, filt.pval = 1){
  return(Anova(tmp.mod, type = 2) %>%
           tidy() %>%
           mutate(sig = ifelse(p.value <= 0.05, "*", "")) %>%
           mutate(metric = tmp.metric, .before = 1) %>%
           filter((p.value < filt.pval | is.na(p.value)) & df > 0) %>%
           arrange(desc(statistic))  # highest to lowest effect size
  )
}


# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

flextable_helperFuncs <- function(flextbl.obj,
                                  col.round){
  flextbl.obj %>%
    # align(j = 3:6, align = "right") %>%
    colformat_double(j = col.round, digits = 3) %>%
    # merge_v(j = 1) %>%
    set_formatter(values = list("p" = p_val_format,
                                "p.adj" = p_val_format,
                                "p.value" = p_val_format)
                  ) %>%
    # set_caption(paste0("Wilcoxon Test. p. adj: BH. Weight ~ Sex")) %>%
    autofit()
}



# print glm formula -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

glm_formula <- function(mod){
  
  return(
    paste0("glm(",
           mod[[1]][["formula"]],
           ", family=",
           mod[[1]][["family"]][["family"]],
           ")")
           )
}

# print capscale formula -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

capscale_formula <- function(mod){
  
  return(
    paste0("capscale(",
           mod[[1]][["formula"]],
           ", family=",
           mod[[1]][["family"]][["family"]],
           ")")
  )
}



# GT Table Setting -------------------------------------------------------------------------
# Description: sets default settings for a gt table
# Input: dataframe, significant figures, variables to set threshold (single or multiple variables)
# Output: 

set_GT <- function(x, digits = 3, var){
  x %>%
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

# Find columns -------------------------------------------------------------------------
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




