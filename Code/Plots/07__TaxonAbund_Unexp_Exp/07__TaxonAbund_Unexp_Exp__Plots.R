# 07__TaxonAbund_Unexp_Exp__Stats --------------------------------------------------

cat("07__TaxonAbund_Unexp_Exp__Stats \n")

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



### Heatmap setup -----------------------------------------------------------

# Create column names
columns <- c("Temp: 32°C", 
             "Temp: 35°C", 
             "Time (DPE)", 
             "Cluster: Low", 
             # "Cluster: High",
             "Parasite exposed", 
             "Infection present", 
             "Infection burden")

coef_columns <- c("coef_Temp: 32°C", 
                  "coef_Temp: 35°C", 
                  "coef_Time (DPE)",
                  "coef_Cluster: Low", 
                  # "coef_Cluster: High",
                  "coef_Parasite exposed", 
                  "coef_Infection present", 
                  "coef_Infection burden")


# Derive minimum and maximum coef scores for color scale
global_min <- min(diffAbnd.stats[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["output"]][["coef"]], na.rm = TRUE)

global_max <- max(diffAbnd.stats[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["output"]][["coef"]], na.rm = TRUE)

gradient_palette <- scales::col_numeric(palette = c("blue", "white", "red"), domain = c(-8.1, 8.1)) # (GLobal Min and max, but I just set this manually.


# Function for applying color formatting and other GT table styles
apply_GT_tabStyles <- function(gt_table, columns, coef_columns) {
  for (i in seq_along(columns)) {
    column <- columns[i]
    coef_column <- coef_columns[i]
    
    coef_vals <- gt_table$`_data`[[coef_column]]
    colors <- gradient_palette(coef_vals)
    
    for (j in seq_along(colors)) {
      cell_color <- ifelse(is.na(coef_vals[j]), "grey95", colors[j])
      cell_text <- "black" # ifelse(is.na(coef_vals[j]), "black", "white")
      cell_size <- "medium"
      bold_text <- ifelse(gt_table$`_data`[[column]][j] %in% c("+", "-"), "bold", "normal")
      
      gt_table <- gt_table %>%
        tab_style(
          style = list(cell_fill(color = cell_color),
                       cell_text(color = cell_text,
                                 weight = bold_text,
                                 align = "center",
                                 size = cell_size
                       )
          ),
          locations = cells_body(
            columns = !!sym(column),
            rows = j
          )
        )
    }
  }
  gt_table
}


# Function to create color legend with fixed width
create_color_legend <- function(min_val, max_val, height = "400px") {
  htmltools::div(
    style = paste("display: flex; flex-direction: column; align-items: center; height:", height, ";"),
    htmltools::span(max_val, style = "margin-bottom: 5px;"),
    htmltools::div(
      style = paste("width: 30px; height: 100%; background: linear-gradient(to top, blue, white, red); margin: 0 20px;")
    ),
    htmltools::span(min_val, style = "margin-top: 5px;")
  )
}


### Plots -------------------------------------------------------------------

#### Top 50 ------------------------------------------------------------------

diffAbnd.plots[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["Top50"]] <-
  diffAbnd.stats[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["output"]] %>%
  
  # Rename Columns
  dplyr::mutate(names = case_when(
    metadata == "Temperature" ~ paste0("Temp: ", value, "°C"),
    metadata == "DPE" ~ paste0("Time (DPE)"),
    metadata == "Treatment" ~ paste0("Parasite exposed"),
    metadata == "Cluster" ~ paste0("Cluster: ", value),
    metadata == "Total.Worm.Count" ~ paste0("Infection burden"),
    metadata == "Pathology.Results" ~ paste0("Infection present"),
    .default = value
  ),
  .before = metadata) %>%
  
  # Rearrange and Remove some Columns/Rows
  dplyr::relocate(coef, Phylum, .after = names) %>%
  dplyr::filter(!is.na(Phylum)) %>%
  dplyr::filter(qval < 0.05) %>%
  dplyr::select(-c(metadata:pval, Kingdom:Family)) %>%
  dplyr::arrange(qval) %>%
  
  # Expand columns
  tidyr::pivot_wider(id_cols = c(Taxon, Phylum), names_from = names, values_from = c(qval, coef)) %>%
  
  # Export spreadsheet:
  
  # readr::write_csv(file = file.path("/Users/michaelsieler/Dropbox/Mac (2)/Documents/Sharpton_Lab/Projects_Repository/Rules_of_Life/RoL_HeaterTrial2021/Results/Maaslin/All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER/figures/Maaslin2__ALL_Table.csv"))
  
  dplyr::rename_with(~ stringr::str_replace(., "^qval_", ""), starts_with("qval_")) %>%
  
  mutate(across(all_of(columns),
                ~ case_when(
                  !is.na(.) & get(paste0("coef_", cur_column())) > 0 ~ "+",
                  !is.na(.) & get(paste0("coef_", cur_column())) < 0 ~ "-",
                  TRUE ~ as.character(.)
                ))) %>%
  
  # Rearrange Columns
  dplyr::relocate(c("Taxon","Phylum", "Temp: 32°C", "Temp: 35°C", "Time (DPE)", "Cluster: Low", 
                    # "Cluster: High", 
                    "Parasite exposed", "Infection present", "Infection burden",
                    "coef_Temp: 32°C", "coef_Temp: 35°C", "coef_Time (DPE)",
                    "coef_Cluster: Low", 
                    # "coef_Cluster: High",
                    "coef_Parasite exposed", "coef_Infection present",
                    "coef_Infection burden"), .before = 1) %>%
  
  # Get Top 50
  dplyr::slice_head(n = 50) %>%
  dplyr::group_by(Phylum) %>%
  dplyr::arrange(Phylum, Taxon) %>%
  
  # readr::write_csv(file = file.path("/Users/michaelsieler/Dropbox/Mac (2)/Documents/Sharpton_Lab/Projects_Repository/Rules_of_Life/RoL_HeaterTrial2021/Results/Maaslin/All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER/figures/Maaslin2__Top50_Table.csv"))
  
  gt::gt() %>%
  
  # Apply Tab Styles iteratively
  apply_GT_tabStyles(., columns, coef_columns) %>%
  
  # Bold the row and column names
  tab_style(
    style = cell_text(weight = "bold",
                      align = "center"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  # Hide columns containing "coef_" from final table
  cols_hide(columns = starts_with("coef_")) %>%
  
  # Convert NA's to blanks
  fmt_missing(
    columns = everything(),
    missing_text = ""
  ) %>%
  
  # Ensure text in the first column does not wrap
  tab_style(
    style = cell_text(whitespace = "nowrap"),
    locations = cells_body(
      columns = c(Taxon)
    )
  ) %>%
  
  # Add vertical lines to columns
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "grey",
      weight = px(1)
    ),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  
  # Verticle lines to column headers
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Reduce cell padding to decrease row height
  tab_options(data_row.padding = px(1.5),
              row_group.padding.horizontal = px(.5)) %>%
  
  # Number formatting
  fmt_number(decimals = 3, use_seps = FALSE) %>%
  sub_large_vals(columns = c("Temp: 32°C":"Infection burden"), threshold = 0.25) %>%
  sub_small_vals(columns = c("Temp: 32°C":"Infection burden"), threshold = 0.001) #%>%
  # 
  # # Title/caption
  # tab_header(
  #   title = "Top 50 taxa with significant associations",
  #   subtitle = "MaAsLin2(log(qval)*sign(coef)); All fish"
  # )

legend.html <- htmltools::HTML('
  <div style="display: flex; align-items: center;">
    <div style="display: flex; flex-direction: column; margin-left: 10px; align-items: center;">
    <span style="color: black; font-weight: bold;">Coefficient</span>
    <span style="color: black; font-weight: bold;">8</span>
      <div style="background: linear-gradient(red, white, blue); width: 20px; height: 100px; border: 1px solid black;"></div>
      <div style="display: flex; justify-content: space-between; font-size: 12px;">
      </div>
      <span style="color: black; font-weight: bold;">-8</span>
    </div>
  </div>
')

html_output <- htmltools::div(
  style = "display: flex; align-items: center;",
  as_raw_html(diffAbnd.plots[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["Top50"]]),
  legend.html
)

# Display the combined HTML
# htmltools::browsable(html_output)


#### All Sig -----------------------------------------------------------------

diffAbnd.plots[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["All_sig"]] <-
  diffAbnd.stats[["All"]][["All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER"]][["Maaslin2"]][["output"]] %>%
  
  # Rename Columns
  dplyr::mutate(names = case_when(
    metadata == "Temperature" ~ paste0("Temp: ", value, "°C"),
    metadata == "DPE" ~ paste0("Time (DPE)"),
    metadata == "Treatment" ~ paste0("Parasite exposed"),
    metadata == "Cluster" ~ paste0("Cluster: ", value),
    metadata == "Total.Worm.Count" ~ paste0("Infection burden"),
    metadata == "Pathology.Results" ~ paste0("Infection present"),
    .default = value
  ),
  .before = metadata) %>%
  
  # Rearrange and Remove some Columns/Rows
  dplyr::relocate(coef, Phylum, .after = names) %>%
  dplyr::filter(!is.na(Phylum)) %>%
  dplyr::filter(qval < 0.05) %>%
  dplyr::select(-c(metadata:pval, Kingdom:Family)) %>%
  dplyr::arrange(qval) %>%
  
  # Expand columns
  tidyr::pivot_wider(id_cols = c(Taxon, Phylum), names_from = names, values_from = c(qval, coef)) %>%
  
  # Export spreadsheet:
  
  # readr::write_csv(file = file.path("/Users/michaelsieler/Dropbox/Mac (2)/Documents/Sharpton_Lab/Projects_Repository/Rules_of_Life/RoL_HeaterTrial2021/Results/Maaslin/All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER/figures/Maaslin2__ALL_Table.csv"))
  
  dplyr::rename_with(~ stringr::str_replace(., "^qval_", ""), starts_with("qval_")) %>%
  
  mutate(across(all_of(columns),
                ~ case_when(
                  !is.na(.) & get(paste0("coef_", cur_column())) > 0 ~ "+",
                  !is.na(.) & get(paste0("coef_", cur_column())) < 0 ~ "-",
                  TRUE ~ as.character(.)
                ))) %>%
  
  # Rearrange Columns
  dplyr::relocate(c("Taxon","Phylum", "Temp: 32°C", "Temp: 35°C", "Time (DPE)", "Cluster: Low", 
                    # "Cluster: High", 
                    "Parasite exposed", "Infection present", "Infection burden",
                    "coef_Temp: 32°C", "coef_Temp: 35°C", "coef_Time (DPE)",
                    "coef_Cluster: Low", 
                    # "coef_Cluster: High",
                    "coef_Parasite exposed", "coef_Infection present",
                    "coef_Infection burden"), .before = 1) %>%
  
  # # Get Top 50
  # dplyr::slice_head(n = 50) %>%
  dplyr::group_by(Phylum) %>%
  dplyr::arrange(Phylum, Taxon) %>%
  
  # readr::write_csv(file = file.path("/Users/michaelsieler/Dropbox/Mac (2)/Documents/Sharpton_Lab/Projects_Repository/Rules_of_Life/RoL_HeaterTrial2021/Results/Maaslin/All__TEMP_DPE_TREAT_PATH_WORM_CLUSTER/figures/Maaslin2__Top50_Table.csv"))
  
  gt::gt() %>%
  
  # Apply Tab Styles iteratively
  apply_GT_tabStyles(., columns, coef_columns) %>%
  
  # Bold the row and column names
  tab_style(
    style = cell_text(weight = "bold",
                      align = "center"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  # Hide columns containing "coef_" from final table
  cols_hide(columns = starts_with("coef_")) %>%
  
  # Convert NA's to blanks
  fmt_missing(
    columns = everything(),
    missing_text = ""
  ) %>%
  
  # Ensure text in the first column does not wrap
  tab_style(
    style = cell_text(whitespace = "nowrap"),
    locations = cells_body(
      columns = c(Taxon)
    )
  ) %>%
  
  # Add vertical lines to columns
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "grey",
      weight = px(1)
    ),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  
  # Verticle lines to column headers
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Reduce cell padding to decrease row height
  tab_options(data_row.padding = px(1.5),
              row_group.padding.horizontal = px(.5)) %>%
  
  # Number formatting
  fmt_number(decimals = 3, use_seps = FALSE) %>%
  sub_large_vals(columns = c("Temp: 32°C":"Infection burden"), threshold = 0.25) %>%
  sub_small_vals(columns = c("Temp: 32°C":"Infection burden"), threshold = 0.001) %>%
  
  # Title/caption
  tab_header(
    title = "All taxa with significant associations",
    subtitle = "MaAsLin2(log(qval)*sign(coef)); All fish"
  )
