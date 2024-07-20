
# 03__Infection_Plots -----------------------------------------------------


tmp.psOBJ <- ps.list[["Exposed"]]
tmp.resSubSection <- "Exposed"


## INFECTION ---------------------------------------------------------------


### TEMP --------------------------------------------------------------------

#### Sig Labels --------------------------------------------------------------

worm.plots[["TEMP"]][["TUKEY_GLM.NB"]][["Sig.Labels"]] <- 
  worm.stats[["TEMP"]][["TUKEY_GLM.NB"]] %>%
    SigStars(pval.var = "adj.p.value") %>%
    dplyr::filter(p.value < 0.05 )


#### Plot --------------------------------------------------------------------

worm.plots[["TEMP"]][["TUKEY_GLM.NB"]][["Plot"]] <-
  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%
  
    ggplot(aes(x = Temperature, y = Total.Worm.Count)) +
  
    geom_violin(aes(color = Temperature,
                    fill = Temperature,
                    group = interaction(Temperature)),
                # fill = "white",
                draw_quantiles = c(0.5),
                # dodge.width = 1, varwidth = TRUE,
                alpha = .25,
                size = .75,
                # adjust = .75,
                show.legend = F,
                scale = "width") +
  
    geom_quasirandom(aes(color = Temperature,group = interaction(Temperature)),
                     fill = "white",
                     shape = 23,
                     # dodge.width = 6,
                     stroke = 2,
                     size = 2) +
  
    scale_color_manual(values = c(col.Temp, "white")) +
    scale_fill_manual(values = c(col.Temp, "white")) +
    scale_y_continuous(limits = c(0, 17.5), breaks = seq(0, 16, by = 2)) +
  
    labs(title = "Infection Burden by Temperature",
         x = "Temperature (°C)",
         y = "Total worm counts per sample")  +
  
    theme(legend.position = "right",
          legend.direction = "vertical",
          panel.grid.major.x = element_blank())  +
  
    ggpubr::stat_pvalue_manual(worm.plots[["TEMP"]][["TUKEY_GLM.NB"]][["Sig.Labels"]],
                               label = "p.adj.sig",
                               y.position = c(17.25,16.25),
                               size = 6,
                               bracket.size = 1,
                               hide.ns = T) 


### TEMP:DPE ----------------------------------------------------------------

#### Sig Labels --------------------------------------------------------------

worm.plots[["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Sig.Labels"]] <- 
  worm.stats[["TEMP:DPE"]][["TUKEY_GLM.NB"]] %>%
  SigStars(pval.var = "adj.p.value") %>%
  dplyr::filter(p.value < 0.05 )


#### Plot --------------------------------------------------------------------

worm.plots[["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Plot"]] <-
  ps.list[["All"]] %>%
  microViz::ps_filter(Treatment == "Exposed") %>%
  microViz::samdat_tbl() %>%
  
  ggplot(aes(x = Temperature, y = Total.Worm.Count)) +
  
  geom_violin(aes(color = Temperature,
                  fill = Temperature
                  ),
              draw_quantiles = c(0.5),
              alpha = .25,
              size = .75,
              show.legend = F,
              scale = "width") +
  geom_quasirandom(aes(color = Temperature),
                   fill = "white",
                   shape = 23,
                   stroke = 2,
                   size = 2) +
  
  scale_color_manual(values = c(col.Temp, "white"), name = "Temp (°C)") +
  scale_fill_manual(values = c(col.Temp, "white"), name = "Temp (°C)") +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "", 
                        breaks = seq(0, 16, by = 2)),
    limits = c(0, 16), breaks = seq(0, 16, by = 2)
  ) +
  
  # facet_grid(.~DPE, labeller = c(14 = "14 dpe", 21 = "21 dpe", 28 = "28 dpe", 42 = "42 dpe")) +
  facet_grid(.~DPE) +
  
  labs(title = "Infection Burden by Temperature and Time",
       x = "Temperature (°C)",
       y = "Total worm counts per sample")  +
  
  theme(legend.position = "right",
        legend.direction = "vertical",
        panel.grid.major.x = element_blank()) +
  
  ggpubr::stat_pvalue_manual(worm.plots[["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Sig.Labels"]],
                             label = "p.adj.sig",
                             y.position = c(14,13),
                             size = 6,
                             bracket.size = 1,
                             hide.ns = T) 
    
