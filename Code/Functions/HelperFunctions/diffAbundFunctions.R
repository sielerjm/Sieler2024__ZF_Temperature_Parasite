# Differential Abundance Functions



# ANCOM-BC  -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

run_ANCOMBC_funcs <- function(
    physeq,
    tmp.datatable,
    tmp.params
    
    ){
  
  # Genus level data
  Genus.data = aggregate_taxa(physeq, "Genus")
  
  
  # Family level data
  Family.data = aggregate_taxa(physeq, "Family")
  
  
  # Phylum level data
  Phylum.data = aggregate_taxa(physeq, "Phylum")
  
  
  
}






# Maaslin2 Helper -------------------------------------------------------------------------
# Description: 
# Input: Phyloseq object
# Output: Diff abundance results to /Results directory

# Methods derived from:
#   Elementary methods provide more replicable results in microbial differential abundance analysis
#   Juho Pelto, Kari Auranen, Janne Kujala, Leo Lahti
#   https://arxiv.org/abs/2404.02691

run_maaslin2 <- function(tmp.PS, 
                         tmp.sample.names = "Sample",
                         tmp.rank = "Genus", # What rank to aggregate at ("unique", "Genus", etc.)
                         tmp.fixed,
                         tmp.reference,
                         tmp.output = paste0("__", Sys.Date()),
                         tmp.plot.scatter = F,
                         tmp.plot.heatmap = T,
                         tr = 'LOG', 
                         norm = 'TSS'){
  
  tmp.input.otu = tmp.PS %>% microViz::tax_agg(rank = "Genus") %>% otu_get() %>% as.data.frame()
  tmp.input.meta = microViz::samdat_tbl(tmp.PS) %>% as.data.frame()
  row.names(tmp.input.meta) <- tmp.input.meta[[tmp.sample.names]]
  
  obj <- Maaslin2::Maaslin2(input_data = tmp.input.otu,
                            input_metadata = tmp.input.meta,
                            output = file.path(path.results, "Tables/MaAsLin2", tmp.output), 
                            min_prevalence = 0,
                            normalization = norm, 
                            transform = tr,
                            fixed_effects = tmp.fixed, 
                            reference = tmp.reference,
                            standardize = F,
                            plot_heatmap = tmp.plot.heatmap, 
                            plot_scatter = tmp.plot.scatter,
                            cores = 8)
  
  res <- obj$results %>%
    filter(metadata == 'group' & value == 'case') %>%
    mutate(df = N - length(tmp.fixed) - 1,
           method = paste0('MaAsLin2 (', tr, ', ', norm, ')')) %>%
    select(taxon = feature, est = coef, se = stderr, df, p = pval, method)
  
  return(res)
}


# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

run_ancombc2 <- function(tmp.pseq, 
                         tmp.taxLevel = "Genus",
                         tmp.fixFormula = NULL, 
                         tmp.randFormula = NULL,
                         tmp.group = NULL,
                         tmp.pairwise = ifelse(!is.null(tmp.group), T, F),
                         tmp.numcores = num.cores,
                         tmp.mdfdr = ifelse(!is.null(tmp.group), list(fwer_ctrl_method = "BH", B = 100), NULL)
){
  
  set.seed(42)
  
  return(`ancombc2`(data = tmp.pseq, tax_level = tmp.taxLevel,
                  fix_formula = tmp.fixFormula,
                  rand_formula = tmp.randFormula,
                  p_adj_method = "BH", pseudo = 0, pseudo_sens = TRUE,
                  prv_cut = 0.10, lib_cut = 1000, s0_perc = 0.05,
                  group = tmp.group, #struc_zero = TRUE, neg_lb = TRUE,
                  alpha = 0.05, n_cl = tmp.numcores, verbose = TRUE,
                  global = ifelse(!is.null(tmp.group), T, F), pairwise = tmp.pairwise, 
                  dunnet = F, trend = F,
                  iter_control = list(tol = 1e-2, max_iter = 20, 
                                      verbose = TRUE),
                  em_control = list(tol = 1e-5, max_iter = 100),
                  mdfdr_control = tmp.mdfdr
  )
  ) 
  
}




# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

