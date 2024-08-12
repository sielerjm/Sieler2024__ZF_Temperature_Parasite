# 06-2__Temp_Unexp_Exp__Plots -----------------------------------------------------
## Final Timepoint

tmp.psOBJ <- ps.list[["TimeFinal"]]
tmp.resSubSection <- "TimeFinal"


## Alpha ---------------------------------------------------------------


### TEMP:TREAT (Pre-Exposed) ----------------------------------------------------------------

#### Sig Labels --------------------------------------------------------------

alpha.plots[[tmp.resSubSection]][["TEMP:TREAT"]][["Tukey"]][["Sig.Labels"]] <- 
  alpha.stats[[tmp.resSubSection]][["TEMP:TREAT"]][["Tukey"]] %>%
  SigStars(pval.var = "adj.p.value") %>%
  dplyr::filter(p.value < 0.05 ) %>%
  dplyr::filter(Alpha.Metric == "Simpson") 


#### Plot --------------------------------------------------------------------

alpha.plots[[tmp.resSubSection]][["TEMP:TREAT"]][["TUKEY"]][["Plot"]] <-
  tmp.psOBJ %>%
  microViz::samdat_tbl() %>%
  tidyr::pivot_longer(cols = contains("_norm"), names_to = "Alpha.Metric", values_to = "Alpha.Score") %>%
  cutCellNames(col = "Alpha.Metric") %>%
  filter(Alpha.Metric == "Simpson") %>%
  # filter(Treatment == "Control") %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  ggplot(aes(x = Treatment, y = Alpha.Score, group = interaction(Temperature, Treatment))) +
  
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  
  geom_violin(aes(color = Temperature,
                  fill = Temperature,
                  shape = Treatment,
                  alpha = ifelse(Treatment == "Control", Treatment, "white")),
              size = 1.25,
              # alpha = .05,
              scale = "width",
              show.legend = F,
              na.rm = T) +
  
  scale_alpha_manual(values = c(.5, .15)) +
  scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)", guide = "none", label = c("Control", "Pre-Exposure")) +
  scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none", label = c("Control", "Pre-Exposure")) +
  
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  
  geom_violin(aes(color = Temperature,
                  fill = Temperature),
              # color = "white",
              draw_quantiles = c(.5),
              size = 3,
              alpha = 0,
              scale = "width",
              show.legend = F,
              na.rm = T) +
  
  scale_color_manual(values = c("white","white","white")) +
  scale_fill_manual(values = c("white","white","white")) +
  
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  
  geom_violin(aes(color = Temperature,
                  fill = Temperature),
              draw_quantiles = c(.5),
              size = 1.25,
              alpha = 0,
              scale = "width",
              show.legend = F,
              na.rm = T) +
  
  scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)", guide = "none") +
  scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
  
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  
  ggbeeswarm::geom_quasirandom(aes(color = ifelse(Treatment != "Control", Temperature, "white"),
                                   fill = ifelse(Treatment == "Control", Temperature, "white"),
                                   shape = Treatment),
                               size = 2,
                               # fill = "white",
                               # shape = 23,
                               stroke = 1.25,
                               show.legend = T,
                               # dodge.width = 6,
                               # varwidth = T,
                               na.rm = T) +
  
  scale_fill_manual(values = c(col.Temp, "white"), name = "Treatment", guide = "none", label = c("Control", "Pre-Exposure")) +
  scale_color_manual(values = c(col.Temp, "white"), name = "Temperature", guide = "none", label = c("Control", "Pre-Exposure")) +
  scale_shape_manual(values = c(21, 23)) +
  
  # Scaling
  scale_x_discrete(labels = c("Control" = "Control", "Exposed" = "Pre-Exposed")) +
  
  # Labels
  labs(#title = "Alpha Diversity Scores across Temperature and Time", #caption = "Alpha; All",
    x = "Treatment",
    y = "Alpha Score (Normalized)"
  ) +
  
  # Facet
  facet_grid(.~ Temperature, labeller = labeller(Temperature = c("28" = "28°C", "32" = "32°C", "35" = "35°C"))) +
  
  # Theme Settings
  theme(legend.position = "none",
        legend.justification = "center",
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 12)
  )  #+
# 
# # Statistical Notation
# - No Significant results, so no notation present
# ggpubr::stat_pvalue_manual(alpha.plots[[tmp.resSubSection]][["TEMP:PATH"]][["Tukey"]][["Sig.Labels"]] %>%
#                              dplyr::mutate(group1 = as.numeric(group1),
#                                            group2 = as.numeric(group2)),
#                            label = "p.adj.sig",
#                            y.position = NULL, #c(1, 1.075, 1.15, 1, 1, 1.15, 1.075),
#                            step.group.by = "term",
#                            size = 6,
#                            label.size = 7,
#                            bracket.size = 1,
#                            hide.ns = T)




