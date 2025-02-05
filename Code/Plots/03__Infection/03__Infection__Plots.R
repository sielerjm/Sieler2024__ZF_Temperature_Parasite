
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


#### Table -------------------------------------------------------------------


##### Pathology Results -------------------------------------------------------

worm.plots[[tmp.resSubSection]][["TEMP"]][["PathologyResults"]][["Table"]] <-
  
  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%
    
    dplyr::group_by(Temperature, Pathology.Results) %>%
    count(name = "Count") %>%
    ungroup() %>%
    dplyr::group_by(Temperature) %>%
    
    # Make table
    gt::gt() %>%
    gt::tab_header(
      title = "Summary of Infection Outcomes",
      subtitle = "(Pathology Results)"
    ) %>%
    gt::fmt_number(
      columns = everything(),
      decimals = 0
    )

##### Total Worm Counts -------------------------------------------------------

worm.plots[[tmp.resSubSection]][["TEMP"]][["TotalWormCount"]][["Table"]] <-

  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%
    group_by(Temperature) %>%
    dplyr::select(Temperature, Total.Worm.Count) %>%
    report::report_table() %>%
    summary() %>%
    
    # Make table
    gt::gt() %>%
    gt::tab_header(
      title = "Summary of Infection Outcomes",
      subtitle = "(Total Worm Count)"
    ) %>%
    gt::fmt_number(
      columns = everything(),
      decimals = 0
    )


### TEMP:DPE ----------------------------------------------------------------

#### Sig Labels --------------------------------------------------------------

worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Sig.Labels"]] <-
  worm.stats[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]] %>%
  SigStars(pval.var = "adj.p.value") %>%
  dplyr::filter(p.value < 0.05 )


#### Plot --------------------------------------------------------------------

worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Plot"]] <-
  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%

    ggplot(aes(x = Temperature, y = Total.Worm.Count)) +

    geom_violin(aes(#color = Temperature,
      fill = Temperature),
      color = "white",
      # position = position_dodge(width = 5),
      # width = 4,
      draw_quantiles = c(.5),
      size = 2,
      alpha = .2,
      scale = "width",
      show.legend = F,
      na.rm = T) +

    geom_violin(aes(color = Temperature,
                    fill = Temperature),
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

    scale_color_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C")) +
    scale_fill_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C")) +

    scale_x_discrete(
      labels = function(x) paste0(x, "°C"), # Append '°C' to x-axis labels
      breaks = c('28', '32', '35')
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ., name = "",
                          breaks = seq(0, 16, by = 2)),
      limits = c(0, 16), breaks = seq(0, 16, by = 2)
    ) +

    facet_grid(.~DPE, labeller = labeller(DPE = c("0" = "0 dpe", "14" = "14 dpe", "21" = "21 dpe", "28" = "28 dpe", "42" = "42 dpe"))) +
    # facet_grid(.~DPE) +

    labs(#title = "Infection Burden by Temperature and Time",
         x = "Temperature",
         y = "Total worm counts")  +

    theme(legend.position = "right",
          legend.direction = "vertical",
          panel.grid.major.x = element_blank()) +

    ggpubr::stat_pvalue_manual(worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Sig.Labels"]],
                               label = "p.adj.sig",
                               y.position = c(14,13),
                               size = 6,
                               label.size = 10,
                               bracket.size = 2,
                               hide.ns = T)


#### Plot v2 - Line ----------------------------------------------------------


worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["TUKEY_GLM.NB"]][["Plot_v2"]] <-
  {
  
  test.mod <-
    tmp.psOBJ %>%
    # Convert phyloseq object into a tibble
    microViz::samdat_tbl() %>%
    # Run negative binomial GLM
    MASS::glm.nb(formula = Total.Worm.Count ~ Temperature*DPE)
  
  tmp.psOBJ %>%
    microViz::samdat_tbl() %>%
    
    ggplot(aes(x = DPE, y = Total.Worm.Count, group = interaction(Temperature, DPE))) +
    
    geom_ribbon(data = modelbased::estimate_expectation(test.mod, data = "grid"),
                aes(x = DPE, y = Predicted, color = Temperature, fill = Temperature,
                    ymin = CI_low, ymax = CI_high), alpha = 0.15, size = .75,
                inherit.aes = F,
                show.legend = F) +
    
    scale_fill_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C"), guide = "none") +
    scale_color_manual(values = col.Temp, name = "Temperature", labels = c("28°C", "32°C", "35°C"), guide = "none") +
    
    ggnewscale::new_scale_color() +
    ggnewscale::new_scale_fill() +
    
    ggbeeswarm::geom_quasirandom(aes(color = Temperature,
                                     #fill = Temperature
    ),
    size = 2,
    fill = "white",
    shape = 23,
    stroke = 1,
    show.legend = T,
    # dodge.width = 6,
    # varwidth = T,
    na.rm = T) +
    
    scale_fill_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C")) +
    scale_color_manual(values = col.Temp, name = "Temperature", labels = c("28°C", "32°C", "35°C")) +

    ggnewscale::new_scale_color() +
    ggnewscale::new_scale_fill() +
    
    geom_smooth(data = modelbased::estimate_expectation(test.mod, data = "grid"),
                aes(x = DPE, y = Predicted, color = Temperature, fill = Temperature),
                size = 4,
                method = "glm", se = T,
                inherit.aes = F,
                show.legend = F) +
    
    scale_color_manual(values = c("white","white","white"), guide = "none") +
    scale_fill_manual(values = c("white","white","white"), guide = "none") +
    ggnewscale::new_scale_color() +
    ggnewscale::new_scale_fill() +
    
    geom_smooth(data = modelbased::estimate_expectation(test.mod, data = "grid"),
                aes(x = DPE, y = Predicted, color = Temperature),
                size = 2,
                method = "glm", se = F,
                inherit.aes = F) +
    
    scale_fill_manual(values = c(col.Temp, "white"), name = "Temperature", labels = c("28°C", "32°C", "35°C")) +
    scale_color_manual(values = col.Temp, name = "Temperature", labels = c("28°C", "32°C", "35°C")) +
    
    # Scaling
    scale_x_continuous(limits = c(10, 45), breaks = seq(14, 42, by = 7)) +
    # scale_y_continuous(limits = c(-0.05, 1.15), breaks = seq(0, 1, by = .25)) +
    
    # Labels
    labs(#title = "Alpha Diversity Scores across Temperature and Time", #caption = "Alpha; All",
      x = "Days post exposure (dpe)",
      y = "Total worm counts"
    ) +
    
    # Facet
    # facet_grid(.~ Temperature, labeller = labeller(Temperature = c("28" = "28°C", "32" = "32°C", "35" = "35°C"))) +
    
    # Theme Settings
    theme(legend.position = "right",
          legend.justification = "center",
          legend.direction = "vertical",
          panel.grid.major.x = element_blank()
    )
}



## SUPP --------------------------------------------------------------------


### INF Method --------------------------------------------------------------

# Import Data

tmp.inf.data <- readxl::read_excel(file.path(path.data, "Raw/Connor Hot Fish RTF Primary No Controls Ver 2.xlsx"))

# Plot

worm.plots[[tmp.resSubSection]][["TEMP:DPE"]][["Method.Counts"]][["Plot"]] <-
  tmp.inf.data %>%
    dplyr::filter(!is.na(DPE)) %>%
    dplyr::mutate(wet.res = case_when(
      Wet == "0" ~ "neg",
      Wet != "0" & Wet != "NA" ~ "pos",
      Wet == "NA" ~ NA
    ),
    histo.res = case_when(
      histo == "pos" ~ "pos",
      histo == "neg" ~ "neg",
      histo == "NA" ~ NA
    ),
    .after = "Sample") %>%
    dplyr::relocate(Wet, histo, .after = histo.res) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DPE. = factor(DPE),
                  Temperature. = factor(Temperature)) %>%
    
    tidyr::pivot_longer(cols = c(wet.res, histo.res), names_to = "Method", values_to = "Method.Res") %>%
    dplyr::relocate(Method, Method.Res, .after = "Sample") %>%
    dplyr::group_by(DPE, Temperature, Method) %>%
    
    # # Prevalence
    # dplyr::summarise(Total = n(),
    #                  pos = sum(Method.Res == "pos", na.rm = T)/Total,
    #                  neg = sum(Method.Res == "neg", na.rm = T)/Total,
    #                  na  = sum(is.na(Method.Res))/Total) %>%
    
    # Counts  
    dplyr::summarise(Total = n(),
                     pos = sum(Method.Res == "pos", na.rm = T),
                     neg = sum(Method.Res == "neg", na.rm = T),
                     na  = sum(is.na(Method.Res)),
                     Total.noNA = Total - na) %>%
    
    tidyr::pivot_longer(cols = c(pos, neg, na), names_to = "Results", values_to = "Counts") %>%
    dplyr::mutate(Results = factor(Results, levels = c("pos", "neg", "na"))) %>%
    
    dplyr::mutate(DPE. = factor(DPE),
                  Temperature. = factor(Temperature)) %>%
    dplyr::mutate(DPE. = paste0(DPE.," DPE"),
                  Temperature. = paste0(Temperature.,"°C")) %>%
    
    dplyr::filter(Results == "pos") %>%
    
    ggplot(aes(x = Method, y = Counts, 
               fill = interaction(Results, Method, Temperature),
               color = Temperature.,
               group = interaction(DPE.,Method))) +
    
    geom_bar(stat = "identity", position = position_dodge2(), aes(group = Method)) +
    
    geom_text(aes(label = paste0("n = ",Total.noNA),
                  y = 5
    ),
    color = "black",
    position = position_stack(vjust = .5),
    fontface = "bold",
    size = 5, 
    show.legend = F) +
    
    facet_grid(Temperature.~DPE., scales = "free_x") +
    
    scale_x_discrete(labels = c("Histo", "Wet")) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
    
    scale_fill_manual(values = c(col.Temp[1], pal.Paired[1],
                                 col.Temp[2], pal.Paired[3],
                                 col.Temp[3], pal.Paired[5]),
                      name = "Method",
                      labels = c("28°C Histo",
                                 "28°C Wet mount",
                                 "28°C Histo",
                                 "32°C Wet mount",
                                 "32°C Histo",
                                 "32°C Wet mount")) +
    scale_color_manual(values = col.Temp, guide = F) +
    
    labs(title = "Positive Identification of Infection by Method",
         x = NULL,
         y = "Counts of positive identification",
         caption = "Number of fish analyzed (n = #)") +
    
    guides(fill = guide_legend(ncol = 3,
                               override.aes = list(color = c(col.Temp[1], col.Temp[1],
                                                             col.Temp[2], col.Temp[2],
                                                             col.Temp[3], col.Temp[3]))
    )) # Specify the number of columns in the legend

# "#3B65DB" "#7AB84C" "#A03022"
# "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"