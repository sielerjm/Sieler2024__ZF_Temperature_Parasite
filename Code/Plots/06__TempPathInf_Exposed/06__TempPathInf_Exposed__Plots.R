
# 06__TempPathInf_Exposed__Plots -----------------------------------------------------

cat("06__TempPathInf_Exposed__Plots \n")

tmp.psOBJ <- ps.list[["Exposed"]]
tmp.resSubSection <- "Exposed"

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
  ps_mutate(Cluster = fct_relevel(factor(Cluster, levels = c("Other", "Low", "High")))) %>%
  ps_mutate(Cluster. = as.numeric(Cluster))



## Alpha ---------------------------------------------------------------


### TEMP:PATH ----------------------------------------------------------------

#### Sig Labels --------------------------------------------------------------

alpha.plots[[tmp.resSubSection]][["TEMP:PATH"]][["Tukey"]][["Sig.Labels"]] <- 
  alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["Tukey"]] %>%
  SigStars(pval.var = "adj.p.value") %>%
  dplyr::filter(p.value < 0.05 ) %>%
  dplyr::filter(Alpha.Metric == "Simpson") 


#### Plot --------------------------------------------------------------------

alpha.plots[[tmp.resSubSection]][["TEMP:PATH"]][["TUKEY"]][["Plot"]] <-
  tmp.psOBJ %>%
  microViz::samdat_tbl() %>%
  tidyr::pivot_longer(cols = contains("_norm"), names_to = "Alpha.Metric", values_to = "Alpha.Score") %>%
  cutCellNames(col = "Alpha.Metric") %>%
  filter(Alpha.Metric == "Simpson") %>%
  # filter(Treatment == "Control") %>%
  # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
  ggplot(aes(x = Pathology.Results, y = Alpha.Score, group = interaction(Temperature, Pathology.Results))) +
  
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  
  geom_violin(aes(color = Temperature,
                  fill = Temperature),
              size = 1.25,
              alpha = .05,
              scale = "width",
              show.legend = F,
              na.rm = T) +
  
  scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)", guide = "none") +
  scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
  
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
  
  ggbeeswarm::geom_quasirandom(aes(color = Temperature,
                                   fill = Pathology.Results),
                               size = 2,
                               # fill = "white",
                               shape = 23,
                               stroke = 1.25,
                               show.legend = T,
                               # dodge.width = 6,
                               # varwidth = T,
                               na.rm = T) +
  
  scale_fill_manual(values = c("white", "black"), name = "Pathology Results", guide = "none") +
  scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
  
  # Scaling
  scale_x_discrete(labels = c("negative" = "Negative", "positive" = "Positive")) +
  
  # Labels
  labs(#title = "Alpha Diversity Scores across Temperature and Time", #caption = "Alpha; All",
    x = "Pathology Results",
    y = "Alpha Score (Normalized)"
  ) +
  
  # Facet
  facet_grid(.~ Temperature, labeller = labeller(Temperature = c("28" = "28°C", "32" = "32°C", "35" = "35°C"))) +
  # facet_grid(.~ Pathology.Results) +
  
  # Theme Settings
  theme(legend.position = "none",
        legend.justification = "center",
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 12)
  ) #+
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



### TEMP:CLUSTER ------------------------------------------------------------

#### Sig Labels --------------------------------------------------------------

alpha.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["Tukey"]][["Sig.Labels"]] <- 
  alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["Tukey"]] %>%
  SigStars(pval.var = "adj.p.value") %>%
  dplyr::filter(p.value < 0.05 ) %>%
  dplyr::filter(Alpha.Metric == "Simpson") 

#### Plot --------------------------------------------------------------------

alpha.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["Cluster"]][["Plot"]] <-
  tmp.psOBJ %>%
  
  microViz::samdat_tbl() %>% {
    
    ggplot(data = ., aes(x = Simpson__Genus_norm, y = Total.Worm.Count)) +
      
      
      geom_ribbon(data = modelbased::estimate_expectation(alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM"]][["Simpson"]], data = "grid"),
                  aes(x = Alpha.Score, y = Predicted, color = Cluster, fill = Cluster, 
                      ymin = CI_low, ymax = CI_high), 
                  alpha = 0.2,
                  inherit.aes = F,
                  show.legend = F) + 
      
      scale_fill_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      scale_color_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      new_scale_color() +
      new_scale_fill() +
      
      geom_smooth(data = modelbased::estimate_expectation(alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM"]][["Simpson"]], data = "grid"),
                  aes(x = Alpha.Score, y = Predicted, color = Cluster, fill = Cluster),
                  size = 3,
                  method = "glm", se = F,
                  inherit.aes = F) +
      
      scale_color_manual(values = c("white", "white", "white")) +
      # scale_fill_manual(values = c(col.Temp, "white")) +
      scale_fill_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      new_scale_color() +
      new_scale_fill() +
      
      geom_smooth(data = modelbased::estimate_expectation(alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM"]][["Simpson"]], data = "grid"),
                  aes(x = Alpha.Score, y = Predicted, color = Cluster),
                  linetype = "dashed",
                  size = 1,
                  method = "glm", se = F,
                  inherit.aes = F) +
      
      scale_color_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      new_scale_color() +
      
      geom_point(aes(color = Temperature, 
                     fill = Cluster,
                     # fill = Total.Worm.Count,
                     alpha = ifelse(Total.Worm.Count > 0, 1, .1)
      ),
      # fill = "white",
      size = 4,
      stroke = 2,
      shape = 23) +
      
      
      scale_color_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C")) +
      scale_fill_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      
      scale_alpha(guide = "none") +
      scale_linetype(guide = "none") +
      
      new_scale_color() +
      new_scale_fill() +
      # scale_fill_gradient2(low = "white", mid = "grey", high = "black", midpoint = 8, guide = "none") +
      
      # scale_color_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C")) +
      scale_color_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      
      
      # scale_y_continuous(limits = c(-5,18), breaks = seq(0,16,2)) +
      labs(#title = "Diversity (ALPHA (Exposed); ref = Relative 28°C)", #caption = "Alpha; Exposed",
        x = "Alpha Score (Normalized)",
        y = "Total worm counts"
      ) +
      guides(color = guide_legend(override.aes = list(size = 5, stroke = 2, linetype = NA))) +
      theme(
        #   legend.position = "bottom",
        #   legend.direction = "horizontal",
        #   legend.title.position = "top",
        panel.grid.minor.y = element_blank())
  }




## BETA --------------------------------------------------------------------

### TEMP:PATH --------------------------------------------------------------------

#### Plot --------------------------------------------------------------------

beta.plots[[tmp.resSubSection]][["TEMP:PATH"]][["CAP"]][["Plot"]] <-
  tmp.psOBJ %>%
  # ps_filter(Temperature == temp) %>%
  # ps_filter(DPE == 0) %>%
  ps_mutate(Cluster = if_else(
    Treatment == "Exposed" & Total.Worm.Count > 0,
    case_when(
      Simpson__Genus_norm %in% head(sort(Simpson__Genus_norm), 16) ~ "Low",
      Simpson__Genus_norm %in% tail(sort(Simpson__Genus_norm), 16) ~ "High",
      TRUE ~ "Other"
    ),
    "Other"
  )) %>%
  # ungroup()  %>%
  ps_mutate(Cluster = fct_relevel(factor(Cluster, levels = c("Other", "Low", "High")))) %>%
  # samdat_tbl()
  tax_agg("Genus") %>%
  dist_calc("canberra") %>%
  dist_permanova(
    seed = 1,
    variables = c("Temperature", "Path.Res.", "Temp.Path"),
    n_processes = 8,
    n_perms = 999 # only 99 perms used in examples for speed (use 9999+!)
  ) %>% 
  ord_calc(
    method = "CAP",
    constraints = c("Temperature.", "Path.Res."),
  ) %>%
  # ord_plot(color = "Temperature", #shape = "Treatment",
  #          fill = "Temperature", #ifelse("Treatment" == "Control", "Temperature", "white"),
  #          size = 3) +
  microViz::ord_plot(
    axes = c(1, 2),
    color = "Temperature",
    fill = "Pathology.Results",
    size = 3,
    stroke = 1.5,
    shape = 23,
    constraint_vec_style = vec_constraint(colour = "black",
                                          size = 2,
                                          alpha = 1,
                                          arrow = grid::arrow(length = grid::unit(0.05, units = "npc"))),
    constraint_lab_style = constraint_lab_style(
      type = "label",
      justify = "side",
      colour = "black",
      # max_angle = 90, 
      # perpendicular = TRUE, 
      size = 3.5,
      check_overlap = TRUE
    ), 
    auto_caption = NA
  ) +
  
  scale_fill_manual(values = c("white", "black"), name = "Pathology Results", labels = c("Negative", "Positive")) +
  scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
  
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  
  stat_ellipse(aes(color = Temperature,
                   fill = Temperature),
               alpha = .05,
               linetype = "dashed",
               geom = "polygon"
  ) +
  
  # facet_grid(.~Temperature, labeller = labeller(Temperature = c("28" = "28°C", "32" = "32°C", "35" = "35°C"))) +
  
  scale_shape_manual(values = c(16, 23)) +
  scale_color_manual(values = col.Temp, labels = c("28°C", "32°C", "35°C")) +
  scale_fill_manual(values = c(col.Temp, "white"), labels = c("28°C", "32°C", "35°C"))  +
  
  # scale_x_continuous(limits = c(-3.5,2.5)) +
  
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title.position = "top",
        strip.text = element_text(size = 14)) 


# Reshuffle layers so ellipse is in background
beta.plots[[tmp.resSubSection]][["TEMP:PATH"]][["CAP"]][["Plot"]] <- 
  rearrange_layers(beta.plots[[tmp.resSubSection]][["TEMP:PATH"]][["CAP"]][["Plot"]])



### TEMP:CLUSTER ---------------------------------------------------------------


#### Plot --------------------------------------------------------------------

beta.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP"]][["Plot"]] <-
  tmp.psOBJ %>%
    # ungroup()  %>%
    tax_agg("Genus") %>%
    dist_calc("bray") %>%
    dist_permanova(
      seed = 1,
      variables = c("Temperature", "Total.Worm.Count", "Cluster"),
      n_processes = 8,
      n_perms = 999 # only 99 perms used in examples for speed (use 9999+!)
    ) %>% 
    ord_calc(
      method = "CAP",
      constraints = c("Temperature.", "Total.Worm.Count"),
    ) %>%
    microViz::ord_plot(fill = NA, 
                       color = NA,
                       alpha = 0,
                       shape = 23, size = 3.5, stroke = 1,
                       constraint_vec_style = vec_constraint(colour = "black",
                                                             size = 2,
                                                             alpha = 1,
                                                             arrow = grid::arrow(length = grid::unit(0.05, units = "npc"))),
                       constraint_lab_style = constraint_lab_style(
                         type = "label", 
                         justify = "side",
                         colour = "black",
                         # max_angle = 90, 
                         # perpendicular = TRUE, 
                         size = 3.5,
                         check_overlap = TRUE
                       ), 
                       show.legend = F, 
                       auto_caption = NA
    ) +
    
    scale_color_manual(values = c("white", "orange", "purple"), name = "Cluster") +
    ggnewscale::new_scale_color() +
    
    stat_ellipse(aes(color = Cluster, 
                     fill = Cluster,
                     alpha = ifelse(Cluster == "Other",0,.15)),
                 # alpha = .5,
                 linetype = "dashed",
                 geom = "polygon",
                 show.legend = F
    ) +
    
    scale_alpha_manual(values = c(0,.15), guide = "none") +
    scale_color_manual(values = c("white", "orange", "purple"), name = "Cluster", guide = "none") +
    ggnewscale::new_scale_color() +
    
    geom_point(aes(color = Temperature,
                   fill = Cluster,
                   # alpha = ifelse(Cluster != "Other", "Other", "H v L"),
                   alpha = ifelse(Cluster != "Other", 1, .1)),
               shape = 23,
               size = 3.5,
               stroke = 1) +
    
    scale_fill_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
    scale_shape_manual(values = c(23)) +
    scale_color_manual(values = col.Temp, labels = c("28°C", "32°C", "35°C"), name = "Temperature") +
    scale_alpha(guide = "none") +
    
    # scale_x_continuous(limits = c(-.75, 1.75)) +
    # scale_y_continuous(limits = c(-2, 1.75)) +
    
    guides(color = guide_legend(override.aes = list(size = 4, stroke = 1.5, 
                                                    linetype = NA,
                                                    shape = c(23, 23, 23), 
                                                    fill = "white" 
    )),
    fill = guide_legend(override.aes = list(size = 4, stroke = 1.5, 
                                            linetype = NA,
                                            shape = c(23, 23, 23), 
                                            fill = c("white", "orange", "purple")
    ))
    ) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title.position = "top",
          strip.text = element_text(size = 14))

# Move "Temperature" label to the right to fit within print window
beta.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP"]][["Plot"]][["layers"]][[3]][["aes_params"]][["hjust"]][1] <- -0.5

# Reshuffle layers so ellipse is in background and labels are on top
beta.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP"]][["Plot"]] <- 
  rearrange_layers(beta.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP"]][["Plot"]])

beta.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP"]][["Plot"]] <- 
  move_label_layer_to_top(beta.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["CAP"]][["Plot"]])


### SUPP --------------------------------------------------------------------

#### S6A: TEMP:PATH ---------------------------------------------------------------------

alpha.plots[[tmp.resSubSection]][["TEMP:PATH"]][["TUKEY"]][["Plot_SUPP"]] <-
  {
    
    tmp.sig.bars <-
      alpha.stats[[tmp.resSubSection]][["TEMP:PATH"]][["Tukey"]] %>%
      SigStars(pval.var = "adj.p.value") %>%
      dplyr::filter(p.value < 0.05 ) 
    
    tmp.psOBJ %>%
      microViz::samdat_tbl() %>%
      tidyr::pivot_longer(cols = contains("_norm"), names_to = "Alpha.Metric", values_to = "Alpha.Score") %>%
      cutCellNames(col = "Alpha.Metric") %>%
      # filter(Alpha.Metric == "Simpson") %>%
      # filter(Treatment == "Control") %>%
      # dplyr::mutate(DPE = as.numeric(levels(DPE)[DPE])) %>%
      ggplot(aes(x = Pathology.Results, y = Alpha.Score, group = interaction(Temperature, Pathology.Results))) +
      
      ggnewscale::new_scale_color() +
      ggnewscale::new_scale_fill() +
      
      geom_violin(aes(color = Temperature,
                      fill = Temperature),
                  size = 1.25,
                  alpha = .05,
                  scale = "width",
                  show.legend = F,
                  na.rm = T) +
      
      scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)", guide = "none") +
      scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
      
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
      
      ggbeeswarm::geom_quasirandom(aes(color = Temperature,
                                       fill = Pathology.Results),
                                   size = 2,
                                   # fill = "white",
                                   shape = 23,
                                   stroke = 1.25,
                                   show.legend = T,
                                   # dodge.width = 6,
                                   # varwidth = T,
                                   na.rm = T) +
      
      scale_fill_manual(values = c("white", "black"), name = "Pathology Results", guide = "none") +
      scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
      
      # Scaling
      scale_x_discrete(labels = c("negative" = "Negative", "positive" = "Positive")) +
      
      # Labels
      labs(#title = "Alpha Diversity Scores across Temperature and Time", #caption = "Alpha; All",
        x = "Pathology Results",
        y = "Alpha Score (Normalized)"
      ) +
      
      # Facet
      facet_grid(Alpha.Metric ~ Temperature, labeller = labeller(Temperature = c("28" = "28°C", "32" = "32°C", "35" = "35°C"))) +
      
      # Theme Settings
      theme(legend.position = "none",
            legend.justification = "center",
            legend.direction = "horizontal",
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(size = 12),
            strip.text.y = element_text(size = 12)
      ) #+
    
    # Statistical Notation
    # ggpubr::stat_pvalue_manual(tmp.sig.bars %>%
    #                              dplyr::mutate(group1 = as.numeric(group1),
    #                                            group2 = as.numeric(group2)),
    #                            label = "p.adj.sig",
    #                            y.position = c(1,1.1),
    #                            step.group.by = "term",
    #                            size = 3,
    #                            label.size = 5,
    #                            bracket.size = .75,
    #                            hide.ns = T)   
  }

#### S6C: TEMP:CLUSTER ---------------------------------------------------------------------

alpha.plots[[tmp.resSubSection]][["TEMP:CLUSTER"]][["TUKEY"]][["Plot_SUPP"]] <-
  {
    tmp.psOBJ <- ps.list[["Exposed"]]
    tmp.resSubSection <- "Exposed"
    
    # Add Cluster Columns
    tmp.data <- 
      tmp.psOBJ %>%
      
      microViz::samdat_tbl() %>%
      pivot_longer(cols = contains("_norm"), names_to = "Alpha.Metric", values_to = "Alpha.Score") %>%
      cutCellNames(col = "Alpha.Metric", sep = "__") %>%
      
      # Group samples by Alpha Score
      group_by(Alpha.Metric) %>%
      mutate(Cluster = if_else(
        Treatment == "Exposed" & Total.Worm.Count > 0,
        case_when(
          Alpha.Score <= 0.5 ~ "Low",
          Alpha.Score > 0.5 ~ "High",
          TRUE ~ "Other"
        ),
        "Other"
      ), .after = Treatment) %>%
      mutate(Cluster = fct_relevel(factor(Cluster, levels = c("Other", "Low", "High")))) %>%
      mutate(Cluster. = as.numeric(Cluster))
    
    tmp.data %>%
      
      ggplot(aes(x = Alpha.Score, y = Total.Worm.Count)) +
      
      
      # geom_ribbon(data = modelbased::estimate_expectation(alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM"]][["Shannon"]], data = "grid"),
      #             aes(x = Alpha.Score, y = Predicted, color = Cluster, fill = Cluster, 
      #                 ymin = CI_low, ymax = CI_high), 
      #             alpha = 0.2,
      #             inherit.aes = F,
      #             show.legend = F) + 
      
      scale_fill_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      scale_color_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      new_scale_color() +
      new_scale_fill() +
      
      geom_smooth(data = modelbased::estimate_expectation(alpha.stats[[tmp.resSubSection]][["TEMP:CLUSTER"]][["GLM"]][["Shannon"]], data = "grid"),
                  aes(x = Alpha.Score, y = Predicted, color = Cluster, fill = Cluster),
                  size = 3,
                  method = "glm", se = F,
                  inherit.aes = F) +
      
      scale_color_manual(values = c("white", "white", "white")) +
      # scale_fill_manual(values = c(col.Temp, "white")) +
      scale_fill_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      
      ggnewscale::new_scale_color() +
      ggnewscale::new_scale_fill() +
      
      geom_smooth(#data = modelbased::estimate_expectation(alpha.stats[[tmp.resSubSection]][["TEMP:DPE"]][["GLM"]][["Simpson"]], data = "grid"),
        aes(x = Alpha.Score, y = Total.Worm.Count, color = Temperature, fill = Temperature, group = Temperature),
        size = 3,
        method = "glm", se = F,
        inherit.aes = T) +
      
      scale_color_manual(values = c("white","white","white")) +
      scale_fill_manual(values = c("white","white","white")) +
      ggnewscale::new_scale_color() +
      ggnewscale::new_scale_fill() +
      
      geom_smooth(#data = modelbased::estimate_expectation(alpha.stats[[tmp.resSubSection]][["TEMP:DPE"]][["GLM"]][["Simpson"]], data = "grid"),
        aes(x = Alpha.Score, y = Total.Worm.Count, color = Cluster),
        size = 1.5,
        method = "glm", se = F,
        inherit.aes = T) +
      
      scale_fill_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      scale_color_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      
      new_scale_color() +
      new_scale_fill() +
      
      geom_point(aes(color = Temperature, 
                     fill = Cluster,
                     # fill = Total.Worm.Count,
                     alpha = ifelse(Total.Worm.Count > 0, 1, .1)
      ),
      # fill = "white",
      size = 4,
      stroke = 2,
      shape = 23) +
      
      
      scale_color_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C")) +
      scale_fill_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      
      scale_alpha(guide = "none") +
      scale_linetype(guide = "none") +
      
      new_scale_color() +
      new_scale_fill() +
      # scale_fill_gradient2(low = "white", mid = "grey", high = "black", midpoint = 8, guide = "none") +
      
      # scale_color_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C")) +
      scale_color_manual(values = c("Other" = "white", "Low" = "orange", "High" = "purple"), name = "Cluster") +
      facet_grid(Alpha.Metric~.) +
      
      scale_y_continuous(limits = c(0,16), breaks = seq(0,16,4)) +
      
      labs(#title = "Diversity (ALPHA (Exposed); ref = Relative 28°C)", #caption = "Alpha; Exposed",
        x = "Alpha Score (Normalized)",
        y = "Total worm counts"
      ) +
      guides(color = guide_legend(override.aes = list(size = 5, stroke = 2, linetype = NA))) +
      theme(
        legend.position = "none",
        #   legend.direction = "horizontal",
        #   legend.title.position = "top",
        panel.grid.minor.y = element_blank(),
        strip.text.y = element_text(size = 12))
  }
