
# 03__Infection__Plots -----------------------------------------------------

cat("03__Infection__Plots \n")

tmp.psOBJ <- ps.list[["Exposed"]]
tmp.resSubSection <- "Exposed"


## INFECTION ---------------------------------------------------------------


### TEMP --------------------------------------------------------------------

#### Sig Labels --------------------------------------------------------------

worm.plots[[tmp.resSubSection]][["TEMP"]][["TUKEY_GLM.NB"]][["Sig.Labels"]] <- 
  worm.stats[[tmp.resSubSection]][["TEMP"]][["TUKEY_GLM.NB"]] %>%
    SigStars(pval.var = "adj.p.value") %>%
    dplyr::filter(p.value < 0.05 )


#### Plot --------------------------------------------------------------------

worm.plots[[tmp.resSubSection]][["TEMP"]][["TUKEY_GLM.NB"]][["Plot"]] <-
  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%
  
    ggplot(aes(x = Temperature, y = Total.Worm.Count)) +
  
  ##
  geom_violin(aes(#color = Temperature,
    fill = Temperature),
    color = "white",
    # position = position_dodge(width = 5),
    # width = 4,
    draw_quantiles = c(.5),
    size = 3,
    alpha = .05,
    scale = "width",
    show.legend = F,
    na.rm = T) +
  
  geom_violin(aes(color = Temperature,
                  fill = Temperature),
              draw_quantiles = c(0.5),
              alpha = 0,
              size = 1.25,
              show.legend = F,
              scale = "width") +
  ##
  
  geom_violin(aes(#color = Temperature,
    fill = Temperature),
    color = "white",
    # position = position_dodge(width = 5),
    # width = 4,
    draw_quantiles = c(.5),
    size = 2,
    alpha = 0,
    scale = "width",
    show.legend = F,
    na.rm = T) +
  
  geom_violin(aes(color = Temperature,
                  fill = Temperature
  ),
  draw_quantiles = c(0.5),
  alpha = 0,
  size = .75,
  show.legend = F,
  scale = "width") +
  geom_quasirandom(aes(color = Temperature),
                   fill = "white",
                   shape = 23,
                   stroke = 2,
                   size = 2) +
  
    geom_quasirandom(aes(color = Temperature,group = interaction(Temperature)),
                     fill = "white",
                     shape = 23,
                     # dodge.width = 6,
                     stroke = 2,
                     size = 2) +
  
    scale_color_manual(values = c(col.Temp, "white")) +
    scale_fill_manual(values = c(col.Temp, "white")) +
  
    scale_x_discrete(
      labels = function(x) paste0(x, "°C"), # Append '°C' to x-axis labels
      breaks = c('28', '32', '35')
    ) +
    scale_y_continuous(limits = c(0, 17.5), breaks = seq(0, 16, by = 2)) +
  
    labs(title = "Infection Burden by Temperature",
         x = "Temperature",
         y = "Total worm counts")  +
  
    theme(legend.position = "right",
          legend.direction = "vertical",
          panel.grid.major.x = element_blank())  +
  
    ggpubr::stat_pvalue_manual(worm.plots[[tmp.resSubSection]][["TEMP"]][["TUKEY_GLM.NB"]][["Sig.Labels"]],
                               label = "p.adj.sig",
                               y.position = c(17.25,16.25),
                               size = 6,
                               label.size = 10,
                               bracket.size = 2,
                               hide.ns = T) 


# ### TEMP:DPE ----------------------------------------------------------------
# 
# #### Sig Labels --------------------------------------------------------------
# 
# worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Sig.Labels"]] <-
#   worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]] %>%
#   SigStars(pval.var = "adj.p.value") %>%
#   dplyr::filter(p.value < 0.05 ) 
# 
# 
# #### Plot --------------------------------------------------------------------
# 
# worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Plot"]] <-
#   tmp.psOBJ %>%
#     microViz::samdat_tbl() %>%
#     
#     ggplot(aes(x = Temperature, y = Total.Worm.Count)) +
#     
#     geom_violin(aes(#color = Temperature,
#       fill = Temperature),
#       color = "white",
#       # position = position_dodge(width = 5),
#       # width = 4,
#       draw_quantiles = c(.5),
#       size = 2,
#       alpha = .2,
#       scale = "width",
#       show.legend = F,
#       na.rm = T) +
#     
#     geom_violin(aes(color = Temperature,
#                     fill = Temperature),
#     draw_quantiles = c(0.5),
#     alpha = 0,
#     size = .75,
#     show.legend = F,
#     scale = "width") +
#     
#     geom_quasirandom(aes(color = Temperature),
#                      fill = "white",
#                      shape = 23,
#                      stroke = 2,
#                      size = 2) +
#     
#     scale_color_manual(values = c(col.Temp, "white"), name = "Temp (°C)") +
#     scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)") +
#     
#     scale_x_discrete(
#       labels = function(x) paste0(x, "°C"), # Append '°C' to x-axis labels
#       breaks = c('28', '32', '35')
#     ) +
#     scale_y_continuous(
#       sec.axis = sec_axis(~ ., name = "", 
#                           breaks = seq(0, 16, by = 2)),
#       limits = c(0, 16), breaks = seq(0, 16, by = 2)
#     ) +
#     
#     facet_grid(.~DPE, labeller = labeller(DPE = c("0" = "0 dpe", "14" = "14 dpe", "21" = "21 dpe", "28" = "28 dpe", "42" = "42 dpe"))) +
#     # facet_grid(.~DPE) +
#     
#     labs(title = "Infection Burden by Temperature and Time",
#          x = "Temperature",
#          y = "Total worm counts")  +
#     
#     theme(legend.position = "right",
#           legend.direction = "vertical",
#           panel.grid.major.x = element_blank()) +
#     
#     ggpubr::stat_pvalue_manual(worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Sig.Labels"]],
#                                label = "p.adj.sig",
#                                y.position = c(14,13),
#                                size = 6,
#                                label.size = 10,
#                                bracket.size = 2,
#                                hide.ns = T)
#     
# 
# #### Plot v2 - Line ----------------------------------------------------------
# 
# test.mod <-
#   tmp.psOBJ %>%
#   # Convert phyloseq object into a tibble
#   microViz::samdat_tbl() %>%
#   # Run negative binomial GLM
#   MASS::glm.nb(formula = Total.Worm.Count ~ Temperature*DPE) 
# 
# worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Plot_v2"]] <-
#   tmp.psOBJ %>%
#     microViz::samdat_tbl() %>%
#     
#     ggplot(aes(x = DPE, y = Total.Worm.Count, group = interaction(Temperature, DPE))) +
#     
#     geom_ribbon(data = modelbased::estimate_expectation(test.mod, data = "grid"),
#                 aes(x = DPE, y = Predicted, color = Temperature, fill = Temperature, 
#                     ymin = CI_low, ymax = CI_high), alpha = 0.15, size = .75,
#                 inherit.aes = F) + 
#   
#     scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)", guide = "none") +
#     scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
#     
#     ggnewscale::new_scale_color() +
#     ggnewscale::new_scale_fill() +
#     
#     ggbeeswarm::geom_quasirandom(aes(color = Temperature,
#                                      #fill = Temperature
#     ),
#     size = 2,
#     fill = "white",
#     shape = 23,
#     stroke = 1,
#     show.legend = T,
#     # dodge.width = 6,
#     # varwidth = T,
#     na.rm = T) +
#     
#     scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)", guide = "none") +
#     scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
#     ggnewscale::new_scale_color() +
#     ggnewscale::new_scale_fill() +
#     
#     scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)", guide = "none") +
#     scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
#     ggnewscale::new_scale_color() +
#     ggnewscale::new_scale_fill() +
#     
#     geom_smooth(data = modelbased::estimate_expectation(test.mod, data = "grid"),
#                 aes(x = DPE, y = Predicted, color = Temperature, fill = Temperature),
#                 size = 4,
#                 method = "glm", se = T,
#                 inherit.aes = F) +
#     
#     scale_color_manual(values = c("white","white","white")) +
#     scale_fill_manual(values = c("white","white","white")) +
#     ggnewscale::new_scale_color() +
#     ggnewscale::new_scale_fill() +
#     
#     geom_smooth(data = modelbased::estimate_expectation(test.mod, data = "grid"),
#                 aes(x = DPE, y = Predicted, color = Temperature),
#                 size = 2,
#                 method = "glm", se = F,
#                 inherit.aes = F) +
#     
#     scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)", guide = "none") +
#     scale_color_manual(values = col.Temp, name = "Temp (°C)", guide = "none") +
#     
#     # Scaling
#     scale_x_continuous(limits = c(10, 45), breaks = seq(14, 42, by = 7)) +
#     # scale_y_continuous(limits = c(-0.05, 1.15), breaks = seq(0, 1, by = .25)) +
#     
#     # Labels
#     labs(#title = "Alpha Diversity Scores across Temperature and Time", #caption = "Alpha; All",
#       x = "Days post exposure (dpe)",
#       y = "Total worm counts"
#     ) +
#     
#     # Facet
#     # facet_grid(.~ Temperature, labeller = labeller(Temperature = c("28" = "28°C", "32" = "32°C", "35" = "35°C"))) +
#     
#     # Theme Settings
#     theme(legend.position = "none",
#           legend.justification = "center",
#           legend.direction = "horizontal",
#           panel.grid.major.x = element_blank()
#     )
#   
