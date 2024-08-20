# 07__TaxonAbund_Unexp_Exp__Stats --------------------------------------------------

tmp.psOBJ <- ps.list[["All"]]
tmp.resSubSection <- "All"

# Add Cluster Columns
tmp.psOBJ <- 
  tmp.psOBJ %>%
  
  # Group samples by Alpha Score
  ps_mutate(Cluster = if_else(
    Treatment == "Exposed" & Total.Worm.Count > 0,
    case_when(
      Simpson__Genus_norm <= 0.5 ~ "Low",
      Simpson__Genus_norm > 0.5 ~ "High",
      TRUE ~ "Other"
    ),
    "Other"
  ), .after = Treatment) %>%
  ps_mutate(Cluster = fct_relevel(factor(Cluster, levels = c("Other", "Low", "High")))) 




## TEMP_DPE_TREAT_PATH_WORM_CLUSTER ----------------------------------------


### MaAsLin2 ----------------------------------------------------------------

#### Run Maaslin2

# Methods derived from:
#   Elementary methods provide more replicable results in microbial differential abundance analysis
#   Juho Pelto, Kari Auranen, Janne Kujala, Leo Lahti
#   https://arxiv.org/abs/2404.02691

tmp.output.path.diffAbund <- paste0("All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER")
tmp.file.path.diffAbund <- file.path(path.results, "Tables/MaAsLin2", tmp.output.path.diffAbund, "significant_results.tsv")

run_maaslin2(tmp.PS = tmp.psOBJ,
             tmp.fixed = c("Temperature", "DPE", "Treatment", "Pathology.Results", "Total.Worm.Count", "Cluster"),
             tmp.reference = c("Temperature,28", "Treatment,Control", "Pathology.Results,negative", "Cluster,Other"),
             tmp.output = tmp.output.path.diffAbund
)

#### Table

diffAbnd.stats[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["output"]] <- 
  readr::read_delim(tmp.file.path.diffAbund, 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE) %>%
  dplyr::left_join(tmp.psOBJ %>% 
                     microViz::ps_melt() %>%
                     dplyr::mutate(Genus = stringr::str_replace_all(Genus, " ", ".")) %>%
                     dplyr::mutate(Genus = stringr::str_replace_all(Genus, "-", ".")) %>%
                     dplyr::select(Kingdom:Genus) %>%
                     dplyr::distinct(Genus, .keep_all = T), by = c("feature" = "Genus")) %>%
  dplyr::rename(Taxon = "feature")  %>%
  dplyr::mutate(Taxon = stringr::str_replace_all(Taxon, "\\.", " ")) %>%
  dplyr::arrange(qval) %>%
  dplyr::ungroup()



