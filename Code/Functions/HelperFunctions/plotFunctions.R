

# Generic Box Plots -----------------------------------------
#   Description: Generates box plot 
#   Input: data, significant explanatory variables, resp variable, facet conditions
#   Output: box plot

gen_box_plot <- function(data, x.var, y.var, facet.var.x = ".", facet.var.y = ".", 
                           y.lab = y.var, colors = pal.Dark2, title = "", caption = "",
                           comparisons = ""){
  
  p <- ggplot(data,
              aes_string(x = x.var, y = y.var)) +
    geom_boxplot(aes(fill = eval(parse(text = x.var)) ) # converts string to var name
    ) +
    geom_quasirandom() + 
    facet_grid(as.formula(paste0(facet.var.y, " ~ ", facet.var.x)), scales = "free_y") + 
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    guides(fill=guide_legend(title=x.var)) +
    # theme(legend.position = "bottom") + 
    labs(
      title = ifelse(title != "", sub_char_var(title, ".", " "), ""),
      caption = ifelse(caption != "", caption, ""),
      y = sub_char_var(y.lab, ".", " "),
      x = sub_char_var(x.var, ".", " ")
    ) #+
  # ggpubr::stat_compare_means(comparisons = comparisons, 
  #                            label = "p.signif" ) + 
  # scale_y_continuous(breaks = scales::breaks_pretty(n = 8), limits = c(0, 1.1)) 
  
  return(p)
}


gen_scatter_plot <- function(data, x.var, y.var, facet.var.x = ".", facet.var.y = ".", 
                               y.lab = y.var, colors = NA, title = "", caption = ""){
  
  p <- ggplot(data,
              aes_string(x = x.var, y = y.var)) +
    geom_point(aes(fill = x.var ) # converts string to var name
    ) +
    geom_smooth(color = "blue",
                method="glm", 
                size = 1, 
                alpha = .20) + 
    facet_grid(paste0(facet.var.y, " ~ ", facet.var.x), scales = "free_y") + 
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    guides(fill=guide_legend(title=x.var)) +
    # theme(legend.position = "non") +
    labs(
      title = ifelse(title != "", sub_char_var(title, ".", " "), ""),
      caption = ifelse(caption != "", caption, ""),
      y = sub_char_var(y.lab, ".", " "),
      x = sub_char_var(x.var, ".", " ")
    )
  
  return(p)
}


# PCoA Plots -----------------------------------------
#   Description: Generates box plot 
#   Input: data, significant explanatory variables, resp variable, facet conditions
#   Output: box plot


gen_PCoA_Cat_Plot_vector <- function(
    data,
    mod,
    x.var, 
    y.var, 
    axis, 
    stats, 
    x.var.2 = "", 
    shape.var = "",
    size.var = 3.5,
    elps.var = x.var, 
    title = "", 
    caption = "", 
    disp.stats = F,
    colors = pal.Dark2
){
  
  print("Inside: gen_PCoA_Cat_Plot_vector()")  # TEST
  
  vars <- c(x.var, x.var.2)
  
  # centroid <- calc_Centroid(data, x.var)
  
  p <- {lapply(names(y.var), function(beta){
    
    # If disp.stats == T:
    if(isTRUE(disp.stats)){
      # Extract statistic
      statistic = round(stats[which(stats[2] == x.var &
                                      stats[1] == beta),]$statistic, 3)
      # Extract p.value
      p.val = stats[which(stats[2] == x.var &
                            stats[1] == beta),]$p.value
      
      if(length(p.val != 0)){
        p.val <- p.val
        statistic <- statistic
      } else {
        p.val <- ">> 0.05"
        statistic <- NULL
      }
    }
    
    # VECTORS
    smpl.ord <- data.table()
    arws.ord <- data.table()
    cntr.ord <- data.table()
    
    permanova <- subset(stats, metric == beta)
    
    # Coords, Vectors, Centriods
    dbrda.data <- phyloseqCompanion::get.biplot.data(
      smpls = ps.obj,
      ord = mod[[beta]]
    )
    
    dbrda.data$sample.coords[, Dist := beta]
    dbrda.data$vector.coords[, Dist := beta]
    dbrda.data$centroid.coords[, Dist := beta]
    
    smpl.ord <- rbind(smpl.ord, dbrda.data$sample.coords)[Dist == beta]
    arws.ord <- rbind(arws.ord, dbrda.data$vector.coords)[Dist == beta]
    cntr.ord <- rbind(cntr.ord, dbrda.data$centroid.coords)[Dist == beta]
    
    
    # Extract Significant Variables
    perm.dt <- as.data.table(permanova)
    sig.intrxn <- perm.dt[str_detect(term, ":") & p.value <= 0.05]$term
    sig.intrxn.patterns <- sapply(sig.intrxn, function(term) { {str_split(term, ":")[[1]]} %>% paste(collapse = ".+:") })
    sig.main <- perm.dt[!str_detect(term, ":") & p.value <= 0.05]$term
    sig.vector.coords <- if(length(sig.intrxn.patterns) == 0){
      arws.ord[!str_detect(Variable, ":") & str_detect(Variable, paste(sig.main, collapse = "|"))]
    } else {
      rbind(
        arws.ord[!str_detect(Variable, ":") & str_detect(Variable, paste(sig.main, collapse = "|"))],
        arws.ord[str_detect(Variable, paste(sig.intrxn.patterns, collapse = "|"))]
      )
    }
    
    ## Remove variable prefixes
    lapply(vars, function(tmp.var){
      sig.vector.coords[, Variable := sub(tmp.var, "", Variable)]
      cntr.ord[, Variable := sub(tmp.var, "", Variable)]
    })
    
    ## Relevel factors
    levels(cntr.ord$Variable) <- factor(levels(cntr.ord$Variable), levels = cntr.ord$Variable)
    
    
    # Temp list for storing ggplots as they are created
    tmp.p <- list()
    
    tmp.p[[beta]] <- ggplot(smpl.ord[Dist == beta], aes(x = data[Dist == beta][[2]], y = data[Dist == beta][[3]])) + 
      geom_point(aes(color = eval(parse(text = x.var)), 
                     shape = eval(parse(text = ifelse(shape.var == "", x.var.2, shape.var))),
                     size = eval(parse(text = size.var))
      ),
      alpha = 0.75) +
      geom_point(data = cntr.ord, aes(color = Variable), stroke = 1.5, shape = 13, size = 4, alpha = .95, show.legend = F) +
      geom_segment(
        data = sig.vector.coords, 
        x = 0, y = 0,
        aes(xend = CAP1, yend = CAP2),
        arrow = arrow(length = unit(0.05, "npc")),
      ) +
      stat_ellipse(aes(color = eval(parse(text = x.var)) )) +
      geom_label_repel(
        data = sig.vector.coords, 
        aes(x = CAP1, y = CAP2, label = Variable),
        alpha = .75,
        size = 3
      ) +
      scale_color_manual(name = "Variable", values = colors) +
      theme(
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.just = "center"
      ) +
      guides(
        color = guide_legend(order = 1),
        shape = guide_legend(title=x.var.2),
        size = F
      ) + 
      labs(
        title = ifelse(title == "", paste0("Beta Score ~ ", ifelse(x.var.2 == "", sub_char_var(x.var, ".", " "), paste0(c(sub_char_var(x.var, ".", " "), sub_char_var(x.var.2, ".", " ")), collapse = " + ")) ), sub_char_var(title, ".", " ")),
        caption = ifelse(isTRUE(disp.stats), paste0("Statistic = ", statistic ,", P-value = ", p.val, ", capscale(), ", beta), caption),
        x = axis[Dist == beta]$X.lab,
        y = axis[Dist == beta]$Y.lab
      )
    
  })
  }
  
  return(p)
}


# Unconstrained Ordination Plot -----------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 

betaUncon_plot <- function(data, p.method = "PCoA", physeq, dist = c("bray", "canberra"),  #, "sor"
                           facet.var.x = ".", facet.var.y = ".", tmp.ellipse = NULL,
                           x.var = NULL, x.var.2 = "", title = "", colors = pal.dark2, tmp.size = 1){
  
  # tmp.ellipse <- ifelse(x.var.2 == "", x.var, x.var.2)
  # if(!is.null(tmp.ellipse)){
  #   tmp.ellipse <- tmp.ellipse
  # } else{
  #   tmp.ellipse <- ifelse(x.var.2 == "", x.var, x.var.2)
  # }
  tmp.ellipse <- x.var
  
  # Set Seed
  set.seed(42)
  
  # If no x.var.2, temp workaround for an endless loop
  if(is.null(x.var.2)){
    x.var.2 <- x.var
  }
  
  p <- {lapply(dist, function(beta){
    
    # Prints which beta metric is being ran
    print(beta)
    
    tmp.p <- list() # temp list to store plots
    
    # Temp list to store ordination data
    tmp.ord <- ordinate(
      physeq = physeq, 
      method = p.method,  # c("DCA", "CCA", "RDA", "CAP", "DPCoA", "NMDS", "MDS", "PCoA")
      distance = beta  # c("bray", "canberra", "sor")
    )
    
    # Temp list for storing ggplots as they are created
    tmp.p <- list()
    
    # Plot function
    tmp.p[[beta]] <- plot_ordination(
      physeq = physeq,
      ordination = tmp.ord,
      # color = x.var,12333
      justDF = T
      
    ) #+
    
    ggplot(data = tmp.p[[beta]], aes(x=Axis.1, y=Axis.2)) +
      geom_point(aes(fill = eval(parse(text = x.var)),
                     color = eval(parse(text = x.var)),
                     shape = eval(parse(text = x.var.2))
                     ),
                 alpha = 0.9,
                 size = 3*eval(parse(text = tmp.size))) +
      stat_ellipse(aes(color = eval(parse(text = tmp.ellipse)) )) +
      # geom_label_repel(data = calc_Centroid_2(tmp.ord, x.var = x.var),
      #                  aes(label = eval(parse(text = x.var)) ),
      #                  alpha = 0.80) +
      facet_grid(paste0(facet.var.y, " ~ ", facet.var.x), scales = "free_y") +
      # scale_color_brewer(palette = "Dark2") +
      # scale_fill_brewer(palette = "Dark2") +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      labs(
        # title = ifelse(title != "", title, paste0(c(p.method, beta), collapse = ", "))
        title = ifelse(title == "", paste0("Beta Score ~ ", ifelse(x.var.2 == "", sub_char_var(x.var, ".", " "), paste0(c(sub_char_var(x.var, ".", " "), sub_char_var(x.var.2, ".", " ")), collapse = " + ")) ), sub_char_var(title, ".", " ")),
        caption = paste0(c(p.method, beta), collapse = ", "),
        x = "Axis 1", # x = axis[Dist == beta]$X.lab,
        y = "Axis 2" # y = axis[Dist == beta]$Y.lab
      ) +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title=x.var),
             color = guide_legend(title=x.var),#, override.aes = list(size=3)),
             shape = guide_legend(title=x.var.2),
             #override.aes = list(alpha = 1, size = 3) ),
             # linetype=guide_legend(title="Ellipses"),
             size = F,
             alpha = F)
    # 
  })}
  
  # return(p)
}


# -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 


betaUncon_plot_num <- function(data, p.method = "PCoA", physeq, dist = c("bray", "canberra", "sor"), 
                               facet.var.x = ".", facet.var.y = ".", tmp.ellipse = NULL,
                               x.var = NULL, x.var.2 = "", title = "", colors = "RdYlBu"){
  
  # tmp.ellipse <- ifelse(x.var.2 == "", x.var, x.var.2)
  # if(!is.null(tmp.ellipse)){
  #   tmp.ellipse <- tmp.ellipse
  # } else{
  #   tmp.ellipse <- ifelse(x.var.2 == "", x.var, x.var.2)
  # }
  tmp.ellipse <- x.var
  
  # Set Seed
  set.seed(42)
  
  # If no x.var.2, temp workaround for an endless loop
  if(is.null(x.var.2)){
    x.var.2 <- x.var
  }
  
  p <- {lapply(dist, function(beta){
    
    # Prints which beta metric is being ran
    print(beta)
    
    tmp.p <- list() # temp list to store plots
    
    # Temp list to store ordination data
    tmp.ord <- ordinate(
      physeq = physeq, 
      method = p.method,  # c("DCA", "CCA", "RDA", "CAP", "DPCoA", "NMDS", "MDS", "PCoA")
      distance = beta  # c("bray", "canberra", "sor")
    )
    
    # Temp list for storing ggplots as they are created
    tmp.p <- list()
    
    # Plot function
    tmp.p[[beta]] <- plot_ordination(
      physeq = physeq,
      ordination = tmp.ord,
      # color = x.var,
      justDF = T
      
    ) #+
    
    ggplot(data = tmp.p[[beta]], aes(x=Axis.1, y=Axis.2)) +
      # geom_point(aes(fill = eval(parse(text = x.var)),
      #                color = eval(parse(text = x.var)),
      #                shape = eval(parse(text = x.var.2))),
      #            alpha = 0.7, size = 4) +
      geom_point(aes(color = eval(parse(text = x.var.2)),
                     # fill = eval(parse(text = x.var)),
                     shape = eval(parse(text = x.var)),
                     size = eval(parse(text = x.var.2)) ),
                 alpha = .9, size = 2) +
      scale_size_area() +
      stat_ellipse(aes(linetype = eval(parse(text = tmp.ellipse)) )) +
      # geom_label_repel(data = calc_Centroid_2(tmp.ord, x.var = x.var),
      #                  aes(label = eval(parse(text = x.var)) ),
      #                  alpha = 0.80) +
      facet_grid(paste0(facet.var.y, " ~ ", facet.var.x), scales = "free_y") +
      # scale_color_brewer(palette = "Dark2") +
      # scale_fill_brewer(palette = "Dark2") +
      scale_colour_gradient(
        low = "red",
        high = "blue",
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "colour"
      ) +
      labs(
        # title = ifelse(title != "", title, paste0(c(p.method, beta), collapse = ", "))
        title = ifelse(title == "", paste0("Beta Score ~ ", ifelse(x.var.2 == "", sub_char_var(x.var, ".", " "), paste0(c(sub_char_var(x.var, ".", " "), sub_char_var(x.var.2, ".", " ")), collapse = " + ")) ), sub_char_var(title, ".", " ")),
        caption = paste0(c(p.method, beta), collapse = ", ")
      ) + 
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title=x.var.2),
             color = guide_legend(title=x.var.2, override.aes = list(size=3)),
             shape = guide_legend(title=x.var, override.aes = list(size=3)),
             #override.aes = list(alpha = 1, size = 3) ),
             linetype=guide_legend(title="Ellipses"),
             size = F,
             alpha = F)
    # 
  })}
  
  # return(p)
}


# Calculate Centroid -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

calc_Centroid <- function(data, vars){
  
  return(
    data %>%
      group_by(vars) %>%
      summarize_at(c("Axis.1", "Axis.2"), mean, na.rm = T)
  )
  
}



# Euclidian Distance -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

euc_Dist <- function(data, x, y, cen.x, cen.y) {
  return(
    sqrt( ((data$cen.x - data$x)^2) + ((data$cen.y - data$y)^2)  )
    )
}

# Num Stat Sig Comp  -------------------------------------------------------------------------
# Description: Returns a number of statistically significant comparisons 
# Input: 
# Output: Returns number of significant comparisons (e.g., 6)

numStatSigComp <- function(tmp.data){
  return(
    length(which(tmp.data$p.adj.signif != "ns")) - 1 # Returns number of significant comparisons (e.g., 6)
  )
}

# Seq Stat Sig Comp  -------------------------------------------------------------------------
# Description: Returns a number of statistically significant comparisons 
# Input: 
# Output: 

seqStatSigComp <- function(tmp.data){
  return(
    seq.int(1,  # Sequences starts at 1
            ((
              numStatSigComp(tmp.data)  # Returns number of significant comparisons (e.g., 6)
              * 0.075) # Multiply that number by 0.075, which is the distance between significance lines
             + 1), # Add one to get your "max" value or where the sequences will end
            by = 0.075)  # The seq function will go from start to end integars by this value
  )
}


# Rearrange layers -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

rearrange_layers <- function(plot) {
  # Identify the index of the layer with stat_ellipse
  stat_ellipse_index <- which(sapply(plot$layers, function(layer) inherits(layer$stat, "StatEllipse")))
  
  # If stat_ellipse layer is found, move it to the first position
  if (length(stat_ellipse_index) > 0) {
    stat_ellipse_layer <- plot$layers[[stat_ellipse_index]]
    plot$layers <- plot$layers[-stat_ellipse_index]
    plot$layers <- c(stat_ellipse_layer, plot$layers)
  }
  
  return(plot)
}

# Move layers to top -------------------------------------------------------------------------
# Description: 
# Input: 
# Output: 

move_label_layer_to_top <- function(plot) {
  # Identify the index of the layer with new_new_geom_label
  label_layer_index <- which(sapply(plot$layers, function(layer) inherits(layer$geom, "GeomLabel")))
  
  # If new_new_geom_label layer is found, move it to the last position
  if (length(label_layer_index) > 0) {
    label_layer <- plot$layers[[label_layer_index]]
    plot$layers <- plot$layers[-label_layer_index]
    plot$layers <- c(plot$layers, label_layer)
  }
  
  return(plot)
}


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

