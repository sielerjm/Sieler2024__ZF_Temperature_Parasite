# Beta Diversity Functions



# Run Capscale -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


run_capscale <- function( ps.OBJ, dist.matrix, beta_metric_col = diversity.method[["beta"]], formula_str) {
  
  # Get sample data
  data <- ps.OBJ %>% microViz::samdat_tbl()
  
  # Extract unique alpha metrics
  unique_metrics <- beta_metric_col
    
  # Run Cap models for each unique alpha metric
  cap_models <- purrr::map(unique_metrics, function(x){
    
    # Get distance matrix
    dist <- dist.matrix[[x]] %>% microViz::dist_get()
    
    # Run capscale
    vegan::capscale(as.formula(formula_str), data = data,
                    na.action = na.omit,
                    sqrt.dist = FALSE)
  }) %>% setNames(unique_metrics) 
  
  return(cap_models)
}


# Run Capscale ADONIS (Beta) -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

run_cap_adonis <- function(ps.OBJ, dist.matrix, beta_metric_col = diversity.method[["beta"]], formula_str, by.method = "margin") {

    # Get sample data
  data <- ps.OBJ %>% microViz::samdat_tbl()
  
  # Extract unique alpha metrics
  unique_metrics <- beta_metric_col
  
  # Run Cap models for each unique alpha metric
  anova.table <- purrr::map(unique_metrics, function(x){
    
    # Get distance matrix
    dist <- dist.matrix[[x]] %>% microViz::dist_get()
    
    # Run Adonis
    adonis2(formula = as.formula(formula_str), data, by = by.method, parallel = 8) %>%
      broom::tidy() %>%
      dplyr::mutate(Beta.Metric = x, .before = 1)  %>%
      dplyr::mutate(sig = case_when(
        p.value <= 0.0001 ~ "****",
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value >= 0.05 ~ "ns"))
  }) %>% setNames(unique_metrics) %>% dplyr::bind_rows()
  
  return(anova.table)
}


# Run Capscale -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


run_BetaDispersion <- function(dist.matrix, beta_metric_col = diversity.method[["beta"]], var) {
  
  # Extract unique alpha metrics
  unique_metrics <- beta_metric_col
  
  # Run HoD models for each unique alpha metric
  HoD_models <- purrr::map(unique_metrics, function(x){
    
    dist.matrix[[x]] %>% 
      microViz::dist_bdisp(variables = var) %>% 
      microViz::bdisp_get()

  }) %>% setNames(unique_metrics) 
  
  return(HoD_models)
}


# Get HOD ADONIS (Beta) -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

get_HoD_anova <- function(betaDisper, beta_metric_col = diversity.method[["beta"]], var) {
  
  # Extract unique alpha metrics
  unique_metrics <- beta_metric_col
  
  anova.table <-
    purrr::map(unique_metrics, function(x){
      purrr::map(c(var), function(y){
        
        betaDisper[[x]][[y]][["anova"]] %>% 
          broom::tidy() %>%
          dplyr::mutate(Beta.Metric = x, 
                        .before = 1) %>%
          dplyr::mutate(term = dplyr::case_when(
            term == "Groups" ~ y,
            .default =  "Residual"
          )) %>%
          dplyr::mutate(sig = case_when(
            p.value <= 0.0001 ~ "****",
            p.value <= 0.001 ~ "***",
            p.value <= 0.01 ~ "**",
            p.value < 0.05 ~ "*", 
            p.value >= 0.05 ~ "ns")) 
        
      }) %>% bind_rows()
    }) %>% bind_rows() 
  
  return(anova.table)
}


# Get HOD ADONIS (Beta) -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

get_HoD_tukey <- function(betaDisper, beta_metric_col = diversity.method[["beta"]], var) {
  
  # Extract unique alpha metrics
  unique_metrics <- beta_metric_col
  
  tukey.table <-
    purrr::map(unique_metrics, function(x){
      purrr::map(c(var), function(y){
        
        betaDisper[[x]][[y]][["tukeyHSD"]] %>% 
          broom::tidy() %>%
          dplyr::mutate(Beta.Metric = x, 
                        .before = 1) %>%
          dplyr::mutate(term = y) %>%
          # dplyr::mutate(term = dplyr::case_when(
          #   term == "Groups" ~ y,
          #   .default =  "Residual"
          # )) %>%
          dplyr::mutate(sig = case_when(
            adj.p.value <= 0.0001 ~ "****",
            adj.p.value <= 0.001 ~ "***",
            adj.p.value <= 0.01 ~ "**",
            adj.p.value < 0.05 ~ "*", 
            adj.p.value >= 0.05 ~ "ns")) 
        
      }) %>% bind_rows()
    }) %>% bind_rows()  %>%
    select(-c(null.value)) %>%
    tidyr::separate(contrast, c('group1', 'group2'), sep = "-") %>% # Dataframe clean up
    dplyr::mutate(`.y.` = "Distance", .after = 1) 
  
  return(tukey.table)
}




















# Set Names Beta ----------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 

set_names_beta <- function(x) {
  setNames(x, div.mtd[["beta"]])
}



# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


# Beta Anova --------------------------------------------------------------

beta_anova <- function(beta.model, methods, names, num.cores = num.cores){
  
  # Assigns number of cores/threads for parallel computing (aka %dopar%) 
  # num.cores = 4
  cl <- parallel::makeCluster(num.cores, type = "FORK", outfile = "")
  doParallel::registerDoParallel(cl, num.cores)
  
  # Anova
  res <- foreach(
    beta = methods,
    .final = names,
    .verbose = TRUE
  ) %dopar% {
    mod <- beta.model[[beta]]
    car::Anova(mod, type = 2) %>% 
      # tidy() %>% 
      tidyr::as_tibble(rownames = "term") %>%
      dplyr::rename("p.value" = "Pr(>Chisq)" ,
             "statistic" = "F") %>%
      dplyr::mutate(sig = ifelse(p.value <= 0.05, "*", "")) %>%
      dplyr::mutate(metric = beta, .before = 1) %>%
      dplyr::arrange(desc(statistic))  # highest to lowest effect size 
  } %>%
    dplyr::bind_rows()
  
  parallel::stopCluster(cl)
  return(res)
}




#Ordination data table list ---------------------------------------------
#   Description: 
#   Input: 
#   Output: 

ord_dt_list <- function(model, physeq){
  return(
    lapply(model, function(model) {
      get.biplot.data(smpls = physeq, ord = model)})
  )
  
}


# Sample Coordinate Datatable ---------------------------------------------
#   Description: 
#   Input: 
#   Output: 

samp_coord_dt <- function(ord.datatable.list, names){
  
  print(paste0("names: ", names))  # TEST
  
  sample.coord.dt <- lapply(names(ord.datatable.list), function(beta) {  # CHANGE
    ord.datatable.list[[beta]]$sample.coords[, Dist := beta]
    return(ord.datatable.list[[beta]]$sample.coords)
  }) %>% rbindlist()
  
  sample.coord.dt[
    , Dist := factor(Dist, levels = names(names)[c(1:length(names))]) ]
  
  return(sample.coord.dt)
}


# Axis Labels -------------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 

axis_labels <- function(ord.datatable.list){
  return(axis.labels <- lapply(names(ord.datatable.list), function(beta) {
    data.table(
      Dist = beta,
      X.lab = ord.datatable.list[[beta]]$axes.labs[1],
      Y.lab = ord.datatable.list[[beta]]$axes.labs[2]
    )
  })  %>% rbindlist())
}



# Homogeneity of Dispersion  -------------------------------------------------
#   Description: 
#   Input: 
#   Output: Statistical results

beta_hom_disper <- function(data, physeq, vars, methods = "bray", plot = F, factor = F){
  
  tmp.list <- list()
  
  # Temp workaround in the case that data needs to be "factorized"
  if(isTRUE(factor)){
    data$vars <- factor(data[[vars]])
  }
  
  for (m in methods) {
    
    set.seed(1)
    # print(m)
    tmp.dist <- phyloseq::distance(physeq, method = m)
    # print(tmp.dist)
    tmp.mod <- betadisper(tmp.dist, data[[vars]])
    tmp.anova <- anova(tmp.mod)
    tmp.permTest <- permutest(tmp.mod, pairwise = TRUE, permutations = 999) 
    tmp.mod.HSD <- TukeyHSD(tmp.mod)
    
    cat("\n")
    print(paste0("Method: ", m))
    # print(tmp.anova)
    print(tmp.permTest) 
    # tmp.permTest %>% pander()
    # tmp.list[m] <- c(tmp.permTest)
    
    if(isTRUE(plot)){
      # plot(tmp.mod)
      boxplot(tmp.mod)
      # plot(tmp.mod.HSD)
    }
    
  }
  # tmp.dist <- phyloseq::distance(physeq, method = methods)
  # tmp.mod <- betadisper(tmp.dist, data[[vars]])
  # tmp.permTest <- permutest(tmp.mod, pairwise = TRUE, permutations = 99)
  
  # return()
}



# Beta Full dbRDA  --------------------------------------------------------------
#   Description: Distance based redundancy analysis beta diversity metrics
#   Input: variables, distance list, beta methods, method names, phyloseq obj
#   Output: statistical model


full_dbrda_model  <- function(vars, distance, methods, names = set_names_beta, physeq, terms = 1, num.cores = 8){  
  
  tmp.ps.obj <- physeq # temporary phyloseq object
  
  # Starts parallel computing
  cl <- parallel::makeCluster(num.cores, type = "FORK", outfile = "")
  doParallel::registerDoParallel(cl, num.cores)
  
  tmp.data <-
    tmp.ps.obj %>%
    microViz::samdat_tbl()
  
  # Build full model
  beta.model.full <- foreach::foreach(
    beta = methods,
    .final = names,
    .verbose = TRUE
  ) %dopar% {
    # progress_Bar_Start(which(methods == beta), length(methods))  # Progress bar
    dist.mat <- distance[[beta]] %>% microViz::dist_get()
    form <- if(terms == 1){
      paste0("dist.mat ~ ", paste0(vars, collapse = "+"))
    } else if (terms == 2) {
      paste0("dist.mat ~ ", paste0(vars, collapse = ":"))
    } else{paste0("dist.mat ~ (", paste0(vars, collapse = "+") ,")^", terms)}
    cat(form)  #  TEST
    vegan::capscale(as.formula(form),
             data = tmp.data,
             na.action = na.omit, # na.omit only non-missing site scores are shown
             sqrt.dist = F # square roots of dissimilarities to avoid negative eigenvalues
             #comm = otu.matrix(physeq)  # Error might originate here, I think
    )
  }
  
  parallel::stopCluster(cl)
  return(beta.model.full)
  
}

# Beta Capscale  --------------------------------------------------------------
#   Description: Distance based redundancy analysis beta diversity metrics
#   Input: variables, distance list, beta methods, method names, phyloseq obj
#   Output: statistical model


capscale_model  <- function(vars, distance, methods, names = set_names_beta, physeq, num.cores = 8){  
  
  tmp.ps.obj <- physeq # temporary phyloseq object
  
  # Starts parallel computing
  cl <- parallel::makeCluster(num.cores, type = "FORK", outfile = "")
  doParallel::registerDoParallel(cl, num.cores)
  
  tmp.data <-
    tmp.ps.obj %>%
    microViz::samdat_tbl()
  
  # Build full model
  beta.model.full <- foreach::foreach(
    beta = methods,
    .final = names,
    .verbose = TRUE
  ) %dopar% {
    # progress_Bar_Start(which(methods == beta), length(methods))  # Progress bar
    dist.mat <- distance[[beta]] %>% microViz::dist_get()
    form <-
      paste0("dist.mat ~ (", vars,")")
      
    cat(form)  #  TEST
    vegan::capscale(as.formula(form),
                    data = tmp.data,
                    na.action = na.omit, # na.omit only non-missing site scores are shown
                    sqrt.dist = F # square roots of dissimilarities to avoid negative eigenvalues
                    #comm = otu.matrix(physeq)  # Error might originate here, I think
    )
  }
  
  parallel::stopCluster(cl)
  return(beta.model.full)
  
}



# Parallell dbRDAs-------------------------------------------------------------------------
# Description: Generate dbRDA ordinations with all Spatial Learning covariates
# Input: 
# Output: 
# Source: https://doi.org/10.1038/s41598-021-83851-4
#   Methods: https://static-content.springer.com/esm/art%3A10.1038%2Fs41598-021-83851-4/MediaObjects/41598_2021_83851_MOESM1_ESM.pdf


par.dbrdas <- function(dist.mats, dbrda.frm, sample.data, nCores, verbose = TRUE) {
  cl <- makeCluster(nCores, type = "FORK", outfile = "")
  registerDoParallel(cl, nCores)
  dbrda.list <- foreach(
    n = names(dist.mats),
    .final = function(x) setNames(x, names(dist.mats)),
    .verbose = verbose
  ) %dopar% {
    dist <- dist.mats[[n]] %>% microViz::dist_get()
    dbrda.obj <- vegan::capscale(as.formula(dbrda.frm), data = sample.data,
                                 na.action = na.omit,
                                 sqrt.dist = FALSE)
    return(dbrda.obj)
  }
  stopCluster(cl)
  return(dbrda.list)
}



# Ordistep -------------------------------------------------------------------------
# Description: Use ordistep to select Spatial Learning covariates that explain the most variance in beta-diversity
# Input: 
# Output: 
# Source: https://doi.org/10.1038/s41598-021-83851-4
#   Methods: https://static-content.springer.com/esm/art%3A10.1038%2Fs41598-021-83851-4/MediaObjects/41598_2021_83851_MOESM1_ESM.pdf

par.ordistep <- function(full.dbrdas, selectDirection, nCores, seed = 42, verbose = TRUE) {
  cl <- makeCluster(nCores, type = "FORK")
  registerDoParallel(cl, nCores)
  dbrda.select.list <- foreach(
    n = names(full.dbrdas),
    .final = function(x) setNames(x, names(full.dbrdas)),
    .verbose = verbose
  ) %dopar% {
    possibleDirections <- c("both", "forward", "reverse")
    dbrda0 <- full.dbrdas[[n]]
    set.seed(seed)
    dbrda.select <- try(ordistep(dbrda0, direction = selectDirection), silent = T)
    if ("try-error" %in% class(dbrda.select)) {
      cat(
        paste0(
          "# ", Sys.time(), "\n",
          "\tOrdistep on full model for ", n, " distance with `direction = ", 
          selectDirection, "` failed."
        ), 
        file = ordi.log, 
        sep = "\n", 
        append = TRUE
      )
      for (newDirection in possibleDirections[possibleDirections != selectDirection]) {
        cat(
          paste0("\tTrying `direction = ", newDirection, "`..."), 
          file = ordi.log, 
          sep = " ", 
          append = TRUE
        )
        set.seed(seed)
        dbrda.select <- try(ordistep(dbrda0, direction = newDirection), silent = T)
        if ("try-error" %in% class(dbrda.select)) {
          cat(paste0("Failed."), file = ordi.log, sep = "\n", append = TRUE)
        } else {
          cat(paste0("Success"), file = ordi.log, sep = "\n", append = TRUE)
          return(dbrda.select)
        }
      }
    } else {
      return(dbrda.select)
    }
  }
  stopCluster(cl)
  return(dbrda.select.list)
}



# ANOVA RDA -------------------------------------------------------------------------
# Description: Significance assessment on ordistep-selected dbRDAs with permanova
# Input: 
# Output: 
# Source: https://doi.org/10.1038/s41598-021-83851-4
#   Methods: https://static-content.springer.com/esm/art%3A10.1038%2Fs41598-021-83851-4/MediaObjects/41598_2021_83851_MOESM1_ESM.pdf

par.anova.rda <- function(
    dbrdas, 
    by.what = c("term", "margin", "axis"), 
    perm.model = c("reduced", "direct", "full"),
    nCores, 
    seed = 42,
    verbose = TRUE
) {
  cl <- makeCluster(nCores, type = "FORK", outfile = "")
  registerDoParallel(cl, nCores)
  dbrda.anova.list <- foreach(
    n = names(dbrdas),
    .final = function(x) setNames(x, names(dbrdas)),
    .verbose = verbose
  ) %dopar% {
    dbrda.obj <- dbrdas[[n]]
    set.seed(seed)
    permanova <- 
      vegan::anova.cca(dbrda.obj, by = by.what, model = perm.model) %>% 
      # tidy() %>% 
      tidyr::as_tibble(rownames = "term") %>%
      dplyr::rename("p.value" = "Pr(>F)" ,
                    "statistic" = "F") %>%
      dplyr::mutate(sig = case_when(
        p.value <= 0.0001 ~ "****",
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value < 0.05 ~ "*", 
        p.value >= 0.05 ~ "ns")) %>%
      dplyr::mutate(metric = n, .before = 1) %>%
      dplyr::arrange(desc(statistic))  # highest to lowest effect size 
    
    return(permanova)
  } %>%
    dplyr::bind_rows()
  stopCluster(cl)
  return(dbrda.anova.list)
}

par.anova.rda_v2 <- function(
    dbrdas, 
    nCores, 
    seed = 42,
    verbose = TRUE
) {
  cl <- makeCluster(nCores, type = "FORK", outfile = "")
  registerDoParallel(cl, nCores)
  dbrda.anova.list <- foreach(
    n = names(dbrdas),
    .final = function(x) setNames(x, names(dbrdas)),
    .verbose = verbose
  ) %dopar% {
    dbrda.obj <- dbrdas[[n]]
    set.seed(seed)
    permanova <- 
      vegan::anova.cca(dbrda.obj) %>% 
      # tidy() %>% 
      tidyr::as_tibble(rownames = "term") %>%
      dplyr::rename("p.value" = "Pr(>F)" ,
                    "statistic" = "F") %>%
      dplyr::mutate(sig = case_when(
        p.value <= 0.0001 ~ "****",
        p.value <= 0.001 ~ "***",
        p.value <= 0.01 ~ "**",
        p.value < 0.05 ~ "*", 
        p.value >= 0.05 ~ "ns")) %>%
      dplyr::mutate(metric = n, .before = 1) %>%
      dplyr::arrange(desc(statistic))  # highest to lowest effect size 
    
    return(permanova)
  } %>%
    dplyr::bind_rows()
  stopCluster(cl)
  return(dbrda.anova.list)
}




# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

par.adonis.rda <- function(dist.mats, 
                           dbrda.frm, 
                           sample.data, 
                           by.what = "margin",
                           nCores, 
                           verbose = TRUE) {
  cl <- makeCluster(nCores, type = "FORK", outfile = "")
  registerDoParallel(cl, nCores)
  adonis.res.list <- foreach(
    n = names(dist.mats),
    .final = function(x) setNames(x, names(dist.mats)),
    .verbose = verbose
  ) %dopar% {
    dist <- dist.mats[[n]] %>% microViz::dist_get()
    adonis.res <- vegan::adonis2(as.formula(dbrda.frm), 
                                 data = sample.data,
                                 by = by.what,
                                 na.action = na.omit,
                                 sqrt.dist = FALSE)
    
    return(adonis.res)
  }
  stopCluster(cl)
  return(adonis.res.list)
}
