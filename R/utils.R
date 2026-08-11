PrepareLoadDonutData <- function(data){

  # #--for trouble shooting
   #data <- data_total_load_ex
  
    # Define compartment colors
    compartment_colors <- c(
      "Ecotoxicity, aquatic" = "#08519c",
      "Ecotoxicity, terrestrial" = "#fd8d3c",
      "Environmental fate" =  "#31a354",
      "Human health" = "#7a0177"
    )
    
    # Define compartment names
    compartment_names <- c(
      "Ecotoxicity, aquatic",
      "Ecotoxicity, terrestrial",
      "Environmental fate",
      "Human health"
    )
    
    # Calculate load percentages
    d0 <- 
      data |>
      mutate(
        load_pct = round(Total_Load / sum(Total_Load) * 100, 0),
        load_pct2 = ifelse(load_pct < 1, "(<1%)", paste0("(", load_pct, "%)")),
        Substance2 = paste(Substance, load_pct2)
      )
    
    # Pivot and adjust values
    d1 <- 
      d0 |> 
      select(Substance, EnvPers_Load, EcoAqu_Load, EcoTerr_Load, HumHea_Load) |> 
      pivot_longer(2:5) |> 
      mutate(value = ifelse(name == "EcoAqu_Load" | name == "EcoTerr_Load", value / 6, value / 3))
    
    # Join and categorize by compartment
    d2 <- 
      d0 |>
      select(Substance, Substance2, Total_Load, load_pct) |> 
      left_join(d1, by = "Substance") |>
      mutate(compartment = case_when(
        name == "EcoAqu_Load" ~ compartment_names[1],
        name == "EcoTerr_Load" ~ compartment_names[2],
        name == "EnvPers_Load" ~ compartment_names[3],
        name == "HumHea_Load" ~ compartment_names[4]
      ))
    
    # Calculate compartment percentages
    d3 <- 
      d2 |> 
      select(compartment, value) |> 
      group_by(compartment) |> 
      summarise(value = sum(value)) |> 
      ungroup() |> 
      mutate(
        value_pct = round(value / sum(value) * 100, 0),
        value_pct2 = ifelse(value_pct < 1, "(<1%)", paste0("(", value_pct, "%)")),
        compartment2 = paste(compartment, value_pct2)
      ) |> 
      select(compartment, compartment2)
    
    # Create final dataset
    d_final <- 
      d2 |>
      left_join(d3, by = "compartment") |> 
      arrange(load_pct) |> 
      mutate_if(is.character, as.factor) |> 
      mutate(Substance = fct_inorder(Substance),
             Substance2 = fct_inorder(Substance2))
    
    # Generate substance colors
    n_substances <- length(unique(d_final$Substance))
    
    clrs_substances <- colorRampPalette(
      brewer.pal(9, "YlOrBr")
    )(n_substances)
    
    # Return all four outputs as a list
    return(list(
      compartment_colors = compartment_colors,
      compartment_names = compartment_names,
      d_final = d_final,
      clrs_substances = clrs_substances
    ))
    }



fxn_Make_LoadDonut_Compartment_Emphasis <- function(data){

  # #--for trouble shooting
  # data <- data_total_load_ex
  
  result <- PrepareLoadDonutData(data)
  
  compartment_colors <- result$compartment_colors
  compartment_names <- result$compartment_names
  dfinal <- result$d_final
  clrs_substances <- result$clrs_substances

  ggplot() +
    geom_col_interactive(data = dfinal |> group_by(compartment2) |> summarise(value = sum(value)),
                         aes(x = 2,
                             y = value,
                             fill = compartment2,
                             group = compartment2,
                             tooltip = paste0(compartment2, ", ", round(value, 2))),
                         color = "black",
                         linewidth = 2) +
    scale_fill_manual(values = unname(compartment_colors),
                      guide = guide_legend(reverse = TRUE, order = 1),
                      name = "Compartment") +
    ggnewscale::new_scale_fill() +
    geom_col(data = dfinal,
             alpha = 0.4,
                         aes(x = 3,
                             y = value,
                             fill = Substance2,
                             group = compartment2,
                             ),
                         color = "gray") +
    scale_fill_manual(values = clrs_substances,
                      guide = guide_legend(reverse = TRUE, order = 2),
                      labels = dfinal |> pull(Substance) |> levels(),
                      name = "Substance") +
     geom_text(data = dfinal |>
                select(Substance2, Total_Load) |>
                distinct() |>
                summarise(Total_Load = round(sum(Total_Load), 2)),
              aes(x = 0.2, y = 0, label = paste0(Total_Load, "/ha")),
              size = 8) +
    coord_polar(theta = "y") +
    theme_void() 
  
}

fxn_Make_LoadDonut_Substance_Emphasis <- function(data){
  
  
  # #--for trouble shooting
  # data <- data_total_load_ex
  
  result <- PrepareLoadDonutData(data = data)
  
  compartment_colors <- result$compartment_colors
  compartment_names <- result$compartment_names
  dfinal <- result$d_final
  clrs_substances <- result$clrs_substances
  
  
  ggplot() +
    geom_col_interactive(data = dfinal |> select(Substance2, Total_Load) |> distinct(),
                         aes(x = 2, y = Total_Load, fill = Substance2, group = Substance2,
                             tooltip = paste0(Substance2, ", ", round(Total_Load, 2))),
                         color = "black",
                         linewidth = 2) +
    scale_fill_manual(values = clrs_substances, 
                      guide = guide_legend(reverse = TRUE, order = 1),
                      name = "Substance")  +
    ggnewscale::new_scale_fill() +
    geom_col(data = dfinal,
             alpha = 0.4,
             aes(x = 3, y = value, fill = compartment, group = Substance2),
             color = "gray") +
    scale_fill_manual(values = compartment_colors,
                      guide = guide_legend(reverse = T, order = 2),
                      name = "Compartment") +
    geom_text(data = dfinal |> 
                select(Substance2, Total_Load) |> 
                distinct() |> 
                summarise(Total_Load = round(sum(Total_Load), 2)),
              aes(x = 0.2, y = 0, label = paste0(Total_Load, "/ha")),
              size = 8) +
    coord_polar(theta = "y") +
    theme_void() 
  
}

PrepareCostDonutData <- function(data){
  
  # #--for trouble shooting
  #data <- data_total_load_ex
  
  # Define compartment colors
  compartment_colors <- c(
    "Ecotoxicity, aquatic" = "#08519c",
    "Ecotoxicity, terrestrial" = "#fd8d3c",
    "Environmental fate" =  "#31a354",
    "Human health" = "#7a0177"
  )
  
  # Define compartment names
  compartment_names <- c(
    "Ecotoxicity, aquatic",
    "Ecotoxicity, terrestrial",
    "Environmental fate",
    "Human health"
  )
  
  # Calculate cost percentages
  d0 <- 
    data |>
    ungroup() |> 
    mutate(
      cost_pct = round(Total_SocietalCost / sum(Total_SocietalCost) * 100, 0),
      cost_pct2 = ifelse(cost_pct < 1, "(<1%)", paste0("(", cost_pct, "%)")),
      Substance2 = paste(Substance, cost_pct2)
    )
  
  # Pivot and adjust values
  d1 <- 
    d0 |> 
    select(Substance, EnvPers_Cost, EcoAqu_Cost, EcoTerr_Cost, HumHea_Cost) |> 
    pivot_longer(2:5) 
  
  # Join and categorize by compartment
  d2 <- 
    d0 |>
    select(Substance, Substance2, Total_SocietalCost, cost_pct) |> 
    left_join(d1, by = "Substance") |>
    mutate(compartment = case_when(
      name == "EcoAqu_Cost" ~ compartment_names[1],
      name == "EcoTerr_Cost" ~ compartment_names[2],
      name == "EnvPers_Cost" ~ compartment_names[3],
      name == "HumHea_Cost" ~ compartment_names[4]
    ))
  
  # Calculate compartment percentages
  d3 <- 
    d2 |> 
    select(compartment, value) |> 
    group_by(compartment) |> 
    summarise(value = sum(value)) |> 
    ungroup() |> 
    mutate(
      value_pct = round(value / sum(value) * 100, 0),
      value_pct2 = ifelse(value_pct < 1, "(<1%)", paste0("(", value_pct, "%)")),
      compartment2 = paste(compartment, value_pct2)
    ) |> 
    select(compartment, compartment2)
  
  # Create final dataset
  d_final <- 
    d2 |>
    left_join(d3, by = "compartment") |> 
    arrange(cost_pct) |> 
    mutate_if(is.character, as.factor) |> 
    mutate(Substance = fct_inorder(Substance),
           Substance2 = fct_inorder(Substance2))
  
  # Generate substance colors
  n_substances <- length(unique(d_final$Substance))
  
  clrs_substances <- colorRampPalette(
    brewer.pal(9, "YlOrBr")
  )(n_substances)
  
  # Return all four outputs as a list
  return(list(
    compartment_colors = compartment_colors,
    compartment_names = compartment_names,
    d_final = d_final,
    clrs_substances = clrs_substances
  ))
}


fxn_Make_CostDonut_Compartment_Emphasis <- function(data){
  
  # #--for trouble shooting
  # data <- data_total_load_ex
  
  result <- PrepareCostDonutData(data)
  
  compartment_colors <- result$compartment_colors
  compartment_names <- result$compartment_names
  dfinal <- result$d_final
  clrs_substances <- result$clrs_substances
  
  ggplot() +
    geom_col_interactive(data = dfinal |> group_by(compartment2) |> summarise(value = sum(value)),
                         aes(x = 2,
                             y = value,
                             fill = compartment2,
                             group = compartment2,
                             tooltip = paste0(compartment2, ", ", round(value, 2))),
                         color = "black",
                         linewidth = 2) +
    scale_fill_manual(values = unname(compartment_colors),
                      guide = guide_legend(reverse = TRUE, order = 1),
                      name = "Compartment") +
    ggnewscale::new_scale_fill() +
    geom_col(data = dfinal,
             alpha = 0.4,
             aes(x = 3,
                 y = value,
                 fill = Substance2,
                 group = compartment2,
             ),
             color = "gray") +
    scale_fill_manual(values = clrs_substances,
                      guide = guide_legend(reverse = TRUE, order = 2),
                      labels = dfinal |> pull(Substance) |> levels(),
                      name = "Substance") +
    geom_text(data = dfinal |>
                select(Substance2, Total_SocietalCost) |>
                distinct() |>
                summarise(Total_SocietalCost = round(sum(Total_SocietalCost), 0)),
              aes(x = 0.2, y = 0, label = paste0("€", Total_SocietalCost, "/ha")),
              size = 8) +
    coord_polar(theta = "y") +
    theme_void() 
  
}


fxn_Make_CostDonut_Substance_Emphasis <- function(data){

  # #--for trouble shooting
  # data <- data_total_load_ex

  result <- PrepareCostDonutData(data = data)

  compartment_colors <- result$compartment_colors
  compartment_names <- result$compartment_names
  dfinal <- result$d_final
  clrs_substances <- result$clrs_substances


  ggplot() +
    geom_col_interactive(data = dfinal |> select(Substance2, Total_SocietalCost) |> distinct(),
                         aes(x = 2, y = Total_SocietalCost, fill = Substance2, group = Substance2,
                             tooltip = paste0(Substance2, ", ", round(Total_SocietalCost, 2))),
                         color = "black",
                         linewidth = 2) +
    scale_fill_manual(values = clrs_substances,
                      guide = guide_legend(reverse = TRUE, order = 1),
                      name = "Substance") +
    ggnewscale::new_scale_fill() +
    geom_col(data = dfinal,
             alpha = 0.4,
             aes(x = 3, y = value, fill = compartment, group = Substance2),
             color = "gray") +
    scale_fill_manual(values = compartment_colors,
                      guide = guide_legend(reverse = T, order = 2),
                      name = "Compartment") +
    geom_text(data = dfinal |>
                select(Substance2, Total_SocietalCost) |>
                distinct() |>
                summarise(Total_SocietalCost = round(sum(Total_SocietalCost), 0)),
              aes(x = 0.2, y = 0, label = paste0("€", Total_SocietalCost, "/ha")),
              size = 8) +
    coord_polar(theta = "y") +
    theme_void()

}


#--make vertical stacked bar cost plot
fxn_Make_Costs_Plot_Vertical <- function(compound_name = "diquat",
                                data = data_compartments,
                                data2 = data_peacou,
                                country_adjuster = "EU") {
  compartment_colors <- c(
    "Ecotoxicity, aquatic" = "#08519c",
    "Ecotoxicity, terrestrial" = "#fd8d3c",
    "Environmental fate" =  "#31a354",
    "Human health" = "#7a0177"
  )
  
  compartment_names <-
    c(
      "Ecotoxicity, aquatic",
      "Ecotoxicity, terrestrial",
      "Environmental fate",
      "Human health"
    )
  
  #--multiply by this number to fix that is was calculated in the UK  
  adjuster <-
    data2 |>
    filter(country == country_adjuster) |>
    pull(GDP_percapita_multiplier)
  
  #--cost_euros_kg is the thing  
  data_new <-
    data |>
    mutate(cost_euros_kg2 = loadweightedcost_euros_kg_ref * adjuster) |> 
    select(-load_score, -load_score2, -loadweightedcost_euros_kg_ref)
  
  
  # #--are there duplicate compounds?
  # #--no
  # data_new |>
  #   group_by(compound) |>
  #   summarise(n = n()) |>
  #   arrange(-n)
  
  
  # #--the max is 22.3 euros
  # max_cost_poss <-
  #   data_new |>
  #   group_by(compound) |>
  #   summarise(cost_euros_kg2 = sum(cost_euros_kg2)) |>
  #   filter(cost_euros_kg2 == max(cost_euros_kg2)) |>
  #   pull(cost_euros_kg2) |>
  #   unique()
  
  
  plot1_data <-
    data_new |>
    filter(compound == compound_name) |>
    select(compound, compartment, cost_euros_kg2) |>
    dplyr::mutate(
      compartment_label = paste0(compartment, " (", round(cost_euros_kg2, 2), ")"),
      compartmentF = factor(compartment, levels = compartment_names),
      compartment_num = as.numeric(compartmentF)
    )
  
  suppressMessages(
    
    plot1_totcost <-
      plot1_data |>
      group_by(compound) |>
      summarise(total_costs = sum(cost_euros_kg2)) |>
      pull(total_costs) |>
      round(2)
    
  )
  
  
  
  plot1 <-
    plot1_data |>
    ggplot(aes(compound, cost_euros_kg2)) +
    geom_col(aes(fill = compartment), 
             color = "black",
             width = 0.5
    ) +
    geom_text(aes(x = compound, y = plot1_totcost + 3,
                  label = paste0("€ ", plot1_totcost, "/kg"),
                  fontface = "bold"),
              check_overlap = T, 
              size = 8) +
    geom_hline(data = plot1_data |> summarise(cost_euros_kg2 = sum(cost_euros_kg2)),
               aes(yintercept = cost_euros_kg2), 
               color = "black", 
               linetype = "dotted") +
    # geom_text(aes(x = compound, y = max_cost_poss + 0.5,
    #               label = paste("Highest cost compound, ",
    #                             round(max_cost_poss, 2),
    #                             "€/kg"),
    #               fontface = "italic"),
    #           color = "darkred",
    #           check_overlap = T) +
    scale_fill_manual(
      values = compartment_colors,
      guide = guide_legend(
        nrow = 2,
        reverse = T,
        order = 1,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    scale_y_continuous(labels = label_currency(prefix = "€"), 
                       limits = c(-0.5, 24),
                       breaks = c(0, 6, 12, 18, 24)) +
    labs(
      #caption = paste0("*Adjusted to per captita GDP of ", country_adjuster),
      x = NULL,
      y = "Societal costs (€/kg)",
      y = NULL,
      #fill = "Compartments"
      fill = NULL
    ) +
    #--theme
    theme_minimal() +
    theme(
      plot.caption = element_text(face = "italic"),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      #panel.grid.major.x = element_blank(),
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      #axis.text.x = element_blank(),
      axis.text.x = element_text(size = rel(1.5)),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5))
  
  plot2_data <-
    data_new |>
    group_by(compound) |> 
    summarise(tot_cost = sum(cost_euros_kg2)) 
  
  plot2_data |> 
    arrange(-tot_cost)
  
  plot2 <-
    ggplot() +
    geom_histogram(
      data = plot2_data,
      aes(x = tot_cost),
      fill = "gray",
      bins = 30
    ) +
    geom_vline(
      data = plot2_data |> filter(compound == compound_name),
      aes(xintercept = tot_cost),
      color = "black",
      linetype = "dotted"
    ) +
    scale_x_continuous(labels = label_currency(prefix = "€"), 
                       limits = c(-0.5, 24),
                       breaks = c(0, 6, 12, 18, 24)) +
    labs(
      y = "Frequency of\nsubstances\nwith a given\nsocietal cost",
      x = "Societal costs*\n(€/kg)"
    ) +
    coord_flip() +
    #--theme
    theme_minimal() +
    theme(
      plot.caption = element_text(face = "italic"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(angle = 0, vjust = 0.5, face = "italic", color = "gray"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) 
  
  
  final_plot <- plot1 + plot2 + plot_layout(widths = c(3, 1))

    return(final_plot)
  
  
}

#--horizontal costs plot
fxn_Make_Costs_Plot <- function(compound_name = "diquat",
                                data = data_compartments,
                                data2 = data_peacou,
                                country_adjuster = "EU") {
  compartment_colors <- c(
    "Ecotoxicity, aquatic" = "#08519c",
    "Ecotoxicity, terrestrial" = "#fd8d3c",
    "Environmental fate" =  "#31a354",
    "Human health" = "#7a0177"
  )
  
  compartment_names <-
    c(
      "Ecotoxicity, aquatic",
      "Ecotoxicity, terrestrial",
      "Environmental fate",
      "Human health"
    )
  
  adjuster <-
    data2 |>
    filter(country == country_adjuster) |>
    pull(GDP_percapita_multiplier)
  
  data_new <-
    data |>
    mutate(cost_euros_kg = loadweightedcost_euros_kg_ref * adjuster)
  
  
  plot1_data <-
    data_new |>
    filter(compound == compound_name) |>
    select(compound, compartment, cost_euros_kg) |>
    dplyr::mutate(
      compartment_label = paste0(compartment, " (", round(cost_euros_kg, 2), ")"),
      compartmentF = factor(compartment, levels = compartment_names),
      compartment_num = as.numeric(compartmentF)
    )
  
  suppressMessages(
    plot1_totcost <-
      plot1_data |>
      group_by(compound) |>
      summarise(total_costs = sum(cost_euros_kg)) |>
      pull(total_costs) |>
      round(2)
  )
  
  
  suppressMessages(
    
    plot2_data <-
      data_new |>
      group_by(compound) |>
      summarise(tot_cost = sum(cost_euros_kg))  
    
  )
  
  
  plot1 <-
    plot1_data |>
    ggplot(aes(compound, cost_euros_kg)) +
    geom_col(aes(fill = compartment), color = "black") +
    # geom_text(aes(x = compound, y = plot1_totcost + 1,
    #               label = paste(plot1_totcost, "€/kg"),
    #               fontface = "italic"),
    #           check_overlap = T) +
    scale_fill_manual(
      values = compartment_colors,
      guide = guide_legend(
        nrow = 1,
        reverse = T,
        order = 1,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    scale_y_continuous(labels = label_currency(prefix = "€"), 
                       limits = c(0, 24),
                       breaks = c(0, 6, 12, 18, 24)) +
    labs(
      #caption = paste0("*Adjusted to per captita GDP of: ", country_adjuster),
      x = NULL,
      y = "Societal costs* (€/kg)",
      #y = NULL,
      caption = paste0("*Adjusted to per captita GDP of ", country_adjuster),
      #fill = "Compartments"
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 2, reverse = TRUE, byrow = TRUE)) +
    coord_flip() +
    #--theme
    theme_minimal() +
    theme(
      plot.caption = element_text(face = "italic"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = rel(1.5)),
      #panel.grid.major.x = element_blank(),
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      plot.margin = margin(10, 50, 10, 10),
      
      axis.text.x = element_text(size = rel(1.5)),
      axis.text.y = element_text(size = rel(2)),
      axis.title.x = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  plot2 <-
    ggplot() +
    geom_histogram(
      data = plot2_data,
      aes(x = tot_cost),
      fill = "gray",
      bins = 50
    ) +
    geom_point(
      data = plot2_data |> filter(compound == compound_name),
      aes(x = tot_cost, y = 0),
      color = "black",
      size = 5,
      pch = 17
    ) +
    geom_text(
      data = plot2_data |> filter(compound == compound_name),
      aes(
        x = tot_cost,
        y = 6,
        label = paste0("€", plot1_totcost, "/kg")
      ),
      size = 10,
      fontface = "italic",
      check_overlap = T
    ) +
    scale_x_continuous(labels = label_currency(prefix = "€"), 
                       limits = c(0, 24),
                       breaks = c(0, 6, 12, 18, 24)) +
    labs(
      #caption = paste0("*Adjusted to per captita GDP of ", country_adjuster),
      y = "Frequency\nof substances\nwith a given\nsocietal cost",
      #x = "Societal costs*\n(€/kg)"
      x = NULL
    ) +
    #--theme
    theme_minimal() +
    theme(
      plot.caption = element_text(face = "italic"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = rel(1.5)),
      #panel.grid.major.x = element_blank(),
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      
      plot.margin = margin(10, 50, 10, 10),
      
      axis.text.x = element_blank(),
      #axis.text.x = element_text(face = "italic", color = "gray"),
      axis.title.y = element_text(angle = 0, vjust = 0.5, face = "italic", color = "gray", size = rel(1.5)),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # plot1 / plot2  +
  #   plot_layout(heights = c(1, 2))
  free(plot2, "label") / plot1  +
    plot_layout(heights = c(2, 1)) & 
    theme(plot.margin = margin(10, 30, 10, 10)) # margin for all plots in composition
  
}




fxn_Make_Girafe_Beeswarm_Plot <- function(compound_name = c("glyphosate"),
                                                data = data_details) {
 
  #--distribution of data for all compounds, column for highlighting selected one
  suppressMessages(
    plot_data <-
      data |>
      mutate(main_compound_type2 = ifelse(main_compound_type %in% c("Herbicide",
                                                                    "Insecticide", 
                                                                    "Fungicide"), main_compound_type, "Other")) |> 
      group_by(main_compound_type2, main_compound_type, compound) |>
      summarise(load_score = sum(index_value * weight)) |>
      dplyr::arrange(load_score) |>
      dplyr::mutate(
        load_score = round(load_score, 2),
        highlight = ifelse(compound %in% compound_name, "Y", "N"),
        main_compound_type2 = factor(main_compound_type2, 
                                     levels = c("Herbicide",
                                                "Insecticide", 
                                                "Fungicide", 
                                                "Other")),
        main_compound_type2num = as.numeric(main_compound_type2))  
  )
  
  
  
  #--top five and bottom five for plotting as points for interacting
  #--rest will be plotted as beeswarm
  suppressMessages(
    
    plot_top5 <- 
      bind_rows(
        plot_data |> group_by(main_compound_type2) |> slice_max(order_by = load_score, n = 5, with_ties = FALSE),
        plot_data |> group_by(main_compound_type2) |> slice_min(order_by = load_score, n = 5, with_ties = FALSE)
      )
    
  )
  
  suppressMessages(
    number_of_compounds_cats <- plot_data |> group_by(main_compound_type2, main_compound_type2num) |> summarise(n = n())
  )
  
  number_of_compounds <- nrow(plot_data)
  
  ggplot() +
    #--rectangles of load division
    #--low load
    geom_rect(
        aes(xmin = 0,
        xmax = 5,
        ymin = 0,
        ymax = 0.5),
      fill = "gray95"
    ) +
    #--moderate load
    geom_rect(
      aes(
          xmin = 0,
        xmax = 5,
        ymin = 0.5,
        ymax = 1),
      fill = "gray80"
    ) +
    #--high load
    geom_rect(
      aes(
        xmin = 0,
        xmax = 5,
        ymin = 1,
        ymax = 1.5),
      fill = "gray65"
    ) +
    geom_point_interactive(data = plot_top5,
               aes(x = main_compound_type2num,
                   y = load_score, 
                   tooltip = paste0(compound, " (", load_score, ")", ", ", main_compound_type)
                   )
               ) +
    geom_beeswarm(data = plot_data,
                   aes(x = main_compound_type2num,
                       y = load_score),
                  color = "black",
                  size = 2,
                  shape = 16) +
    geom_point_interactive(data = plot_data |> filter(compound %in% compound_name),
                  aes(x = main_compound_type2num,
                      y = load_score, 
                      tooltip = paste0(compound, " (", load_score, ")", ", ", main_compound_type)),
                      color = "red",
                      size = 4,
                      shape = 17) +
    geom_text(data = number_of_compounds_cats,
              aes(x = main_compound_type2num,
                  y = 0.1,
                  label = paste0("(n=", n, ")")),
              fontface = "italic") +
    scale_x_continuous(
      breaks = c(1, 2, 3, 4),
      labels = c("Herbicide",
                 "Insecticide", 
                 "Fungicide", 
                 "Other")) +
    labs(
      title = paste0(compound_name),
      subtitle = NULL,
      caption = paste(
        "Database currently includes",
        number_of_compounds,
        "substances"
      ),
      x = NULL,
      y = "Load\nscore"
    ) +
    # Theme
    theme_minimal() +
    theme(
      # legend.position = "bottom",
      # legend.direction = "horizontal",
      # legend.title = element_text(face = "bold"),
      plot.caption = element_text(face = "italic"),
      #panel.grid.major.x = element_blank(),
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
      axis.text.x = element_text(size = rel(1.5)),
      axis.text.y = element_text(size = rel(1.5)),
      plot.title = element_text(hjust = 0.5, face = "bold", size = rel(2)),
      plot.subtitle = element_text(hjust = 0.5)
      # plot.margin = margin(t = 0,  # Top margin
      #                      r = 0,  # Right margin
      #                      b = 0,  # Bottom margin
      #                      l = 0)
    )
  
  
  
}



fxn_Make_Girafe_Rose_Plot <- function(compound_name = "diquat",
                               data = data_compartments) {
  compartment_colors <- c(
    "Ecotoxicity, aquatic" = "#08519c",
    "Ecotoxicity, terrestrial" = "#fd8d3c",
    "Environmental fate" =  "#31a354",
    "Human health" = "#7a0177"
  )
  
  compartment_names <-
    c(
      "Ecotoxicity, aquatic",
      "Ecotoxicity, terrestrial",
      "Environmental fate",
      "Human health"
    )
  
  # Data to plot
  suppressMessages(
    plot_data <-
      data |>
      dplyr::filter(compound == compound_name) |>
      dplyr::mutate(
        compartment_label = paste0(compartment, " (", round(load_score2, 2), ")"),
        compartmentF = factor(compartment, levels = compartment_names),
        compartment_num = as.numeric(compartmentF)
      ) |>
      dplyr::mutate(
        xmin = case_when(
          compartment == "Environmental fate" ~ 0,
          compartment == "Ecotoxicity, terrestrial" ~ 120 / 360,
          compartment == "Ecotoxicity, aquatic" ~ 180 / 360,
          compartment == "Human health" ~ 240 / 360
        ),
        xmid = case_when(
          compartment == "Environmental fate" ~ 60 / 360,
          compartment == "Ecotoxicity, terrestrial" ~ 150 / 360,
          compartment == "Ecotoxicity, aquatic" ~ 210 / 360,
          compartment == "Human health" ~ 300 / 360
        ),
        xmax = case_when(
          compartment == "Environmental fate" ~ 120 / 360,
          compartment == "Ecotoxicity, terrestrial" ~ 180 / 360,
          compartment == "Ecotoxicity, aquatic" ~ 240 / 360,
          compartment == "Human health" ~ 360 / 360
        )
      )
  )
  
  # Dummy data for background concentric circles
  background <- data.frame(
    xmin = 0,
    xmax = 1,
    ymin = c(0, 0.5, 1.0),
    ymax = c(0.5, 1.0, 1.5),
    band = factor(
      c(
        "0-0.5 Low to moderate load",
        "0.5-1 Moderate to high load",
        "1-1.5 High to very high load"
      ),
      levels = c(
        "0-0.5 Low to moderate load",
        "0.5-1 Moderate to high load",
        "1-1.5 High to very high load"
      )
    )
  )
  
  suppressMessages(
    data_total_load_score <-
      plot_data |>
      group_by(compound) |>
      summarise(tot_load_score = sum(load_score))  
  )
  
  
  total_load_score <- round(data_total_load_score |> pull(tot_load_score) |> unique(), 2)
  
  # Plot
  ggplot(plot_data, aes(
    x = 0,
    #compartment,
    y = load_score2,
    fill = compartment
  )) +
    # Concentric circles
    geom_rect(
      data = background,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = band
      ),
      #show.legend = F,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      name = " ",
      # breaks = c(0, 0.5, 1.0, 1.5),
      values = c(
        "0-0.5 Low to moderate load" = "gray95",
        "0.5-1 Moderate to high load" = "gray80",
        "1-1.5 High to very high load" = "gray65"
      ),
      # give the boxes a gray70 outline of size 0.5
      guide = guide_legend(
        override.aes = list(color = "gray70", size  = 0.5)
        ,
        ncol = 1
      )
    ) +
    # Compartment divisions and labels
    geom_segment(
      data = data.frame(x = c(0, 1 / 3, 1 / 2, 2 / 3)),
      aes(
        x = x,
        xend = x,
        y = 0,
        yend = 1.5
      ),
      colour = "gray65",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    # New fill layer for the compartments
    ggnewscale::new_scale_fill() +
    geom_rect_interactive(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = load_score2,
        fill = compartment,
        tooltip = paste0(compartment, "\n(load score = ", round(load_score2, 2), ")")),
      color = "black",
      inherit.aes = FALSE
    ) +
    # Legend
    scale_fill_manual(values = compartment_colors,
                      guide = guide_legend(
                        ncol = 1,
                        reverse = T,
                        order = 1
                      )) +
    labs(
      #caption = paste0("Substance: ", compound_name),
      title = paste0(compound_name),
      subtitle = paste0("Total load score: ", round(total_load_score, 2)),
      x = NULL,
      y = NULL,
      fill = NULL
      #fill = "Compartments"
    ) +
    # Theme
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = rel(1.5)),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      
      plot.title = element_text(hjust = 0.5, face = "bold", size = rel(2)),
      plot.title.position = "plot", #--centers within the entire plot, not just the panel
      plot.subtitle = element_text(hjust = 0.5, size = rel(1.5))
    ) +
    # Turn the barplot into a roseplot
    coord_polar(start = 0, clip = "off")
}


fxn_Make_Girafe_Detailed_Rose_Plot <- function(compound_name = "diquat",
                                        data = data_details) {
  # get things in the desired order 
  
  # Environmental fate attributes
  attribute_envfate <- c(
    "Soil persistence (DT50 soil)"   = "soil_dt50_completed",
    "Water persistence (DT50 water)" = "water_dt50",
    "Surface water transfer (Kfoc)"  = "kfoc_completed",
    "Groundwater transfer (GUS)"     = "gus",
    "Aquatic biome transfer (BCF)"   = "bcf_completed"
    ## terrestrial bioaccumulation
  )
  
  # Ecotoxicity (terrestrial) attribute
  attribute_ecoterr <- c(
    "Birds (acute oral)"                   = "bird_ld50"
    # ,"Birds (chronic oral)"                 = "bird_noel"
    ## bumble bees acute
    ,
    "Earthworms (acute soil)"             = "earthworm_lc50"
    # ,"Earthworms (chronic soil)"            = "earthworm_noec"
    ,
    "Honeybees (acute oral/contact/other)" = "honeybees_ld50_all"
    ## honeybees chronic
    ## lacewings acute
    ## ladybird chronic
    ,
    "Mammals (acute oral)"                 = "mammals_ld50_oral"
    # ,"Mammals (chronic oral)"               = "mammals_noael"
    ## mason bee acute
    # ,"Parasitic wasps (acute contact)"      = "parasiticwasps"
    # ,"Predatory mites (acute contact)"      = "predatorymites"
    ## soil microorganisms
    ## springtales acute
    ## springtales chronic
    ## terrestrial plants
  )
  
  # Ecotoxicity (aquatic) attribute
  attribute_ecoaqua <- c(
    "Algae (acute aqueous)"               = "algae_ec50"
    # ,"Aquatic plants (acute aqueous)"      = "algae_noec"
    # ,"aquaticplants_ec50"
    ,
    "Aquatic invertebrates (acute aq.)"   = "aquaticinvertebrates_ec50"
    ,
    "Aquatic invertebrates (chronic aq.)" = "aquaticinvertebrates_noec"
    ,
    "Fish (acute aqueous)"                = "fish_lc50"
    ,
    "Fish (chronic aqueous)"              = "fish_noec"
    ## sediment dwelling organisms acute
    ## sediment dwelling organisms chronic
  )
  
  # Human health attribute
  attribute_humheal <- c(
    "Mammals (acute dermal)"             = "mammals_ld50_dermal"
    ,
    "Mammals (acute inhalation)"         = "mammals_lc50_inhalation"
    ,
    "Humans (carcinogenicity)"           = "carcinogenicity"
    ,
    "Humans (cholinesterase inhibition)" = "cholinesteraseinhibition"
    # ,"endocrinedisruption"
    # ,"Humans (genotoxicity)"              = "genotoxicity_worst"
    ,
    "Humans (neurotoxicity)"             = "neurotoxicity"
    ,
    "Humans (reprotoxicity)"             = "reprotoxicity"
  )
  
  attribute <- c(attribute_envfate,
                 attribute_ecoterr,
                 attribute_ecoaqua,
                 attribute_humheal)
  
  attribute_names <- names(attribute)
  
  # compartment names 
  
  compartment_names <-
    c(
      "Environmental fate",
      "Ecotoxicity, terrestrial",
      "Ecotoxicity, aquatic",
      "Human health"
    )
  
  # colors 
  
  
  attribute_colors <- c(
    # Environmental fate
    "Soil persistence (DT50 soil)"         = "#ffffcc",
    "Water persistence (DT50 water)"       = "#c2e699",
    "Surface water transfer (Kfoc)"        = "#78c679",
    "Groundwater transfer (GUS)"           = "#31a354",
    "Aquatic biome transfer (BCF)"         = "#006837",
    # Ecotoxicity (terrestrial)
    "Birds (acute oral)"                   = "#feedde",
    "Earthworms (acute soil)"              = "#fdbe85",
    "Honeybees (acute oral/contact/other)" = "#fd8d3c",
    "Mammals (acute oral)"                 = "#d94701",
    # Ecotoxicity (aquatic)
    "Algae (acute aqueous)"                = "#eff3ff",
    "Aquatic invertebrates (acute aq.)"    = "#bdd7e7",
    "Aquatic invertebrates (chronic aq.)"  = "#6baed6",
    "Fish (acute aqueous)"                 = "#3182bd",
    "Fish (chronic aqueous)"               = "#08519c",
    # Human health
    "Mammals (acute dermal)"               = "#feebe2",
    "Mammals (acute inhalation)"           = "#fcc5c0",
    "Humans (carcinogenicity)"             = "#fa9fb5",
    "Humans (cholinesterase inhibition)"   = "#f768a1",
    "Humans (neurotoxicity)"               = "#c51b8a",
    "Humans (reprotoxicity)"               = "#7a0177"
  )
  
  
  # data to plot 
  
  #--attributes
  plot_data <-
    data |>
    filter(compound == compound_name) |>
    mutate(
      attribute = factor(attribute, levels = attribute_names),
      attribute_num = as.numeric(factor(attribute, levels = attribute_names))
    )
  
  #--compartment labels
  plot_data2 <-
    plot_data |>
    ungroup() |>
    select(compartment) |>
    distinct() |>
    mutate(
      compartmentF = factor(compartment, levels = compartment_names),
      compartment_num = as.numeric(compartmentF)
    ) |>
    mutate(
      xmin = case_when(
        compartment == "Environmental fate" ~ 0,
        compartment == "Ecotoxicity, terrestrial" ~ 120 / 360,
        compartment == "Ecotoxicity, aquatic" ~ 180 / 360,
        compartment == "Human health" ~ 240 / 360
      ),
      xmid = case_when(
        compartment == "Environmental fate" ~ 60 / 360,
        compartment == "Ecotoxicity, terrestrial" ~ 150 / 360,
        compartment == "Ecotoxicity, aquatic" ~ 210 / 360,
        compartment == "Human health" ~ 300 / 360
      ),
      xmax = case_when(
        compartment == "Environmental fate" ~ 120 / 360,
        compartment == "Ecotoxicity, terrestrial" ~ 180 / 360,
        compartment == "Ecotoxicity, aquatic" ~ 240 / 360,
        compartment == "Human health" ~ 360 / 360
      )
    )
  
  #--Dummy data for background concentric circles
  background <- data.frame(
    xmin = 0,
    xmax = 1,
    ymin = c(0, 0.5, 1.0),
    ymax = c(0.5, 1.0, 1.5),
    band = factor(
      c("Low to moderate", "Moderate to high", "High to very high"),
      levels = c("Low to moderate", "Moderate to high", "High to very high")
    )
  )
  
  total_load_score <- round(plot_data |> pull(tot_load_score) |> unique(), 2)
  
  #--plots
  ggplot() +
    #--concentric circles
    geom_rect(
      data = background,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = band
      ),
      alpha = 0.5,
      inherit.aes = FALSE,
      show.legend = F
    ) +
    scale_fill_manual(
      name = "Load",
      # breaks = c(0, 0.5, 1.0, 1.5),
      values = c(
        "Low to moderate" = "gray95",
        "Moderate to high" = "gray80",
        "High to very high" = "gray65"
      ),
      guide = guide_legend(override.aes = list(
        color = "gray70", size  = 0.5
      ))
    ) +
    #--compartment divisions and labels
    geom_segment(
      data = data.frame(x = c(0, 1 / 3, 1 / 2, 2 / 3)),
      aes(
        x = x,
        xend = x,
        y = 0,
        yend = 1.5
      ),
      colour = "gray65",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    #--attribute data
    ggnewscale::new_scale_fill() +
    geom_rect_interactive(
      data = plot_data,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = trunk,
        fill = attribute,
        tooltip = paste0(attribute, "\n(load score: ", round(trunk, 2), ")", 
                         "\nData quality score: ", quality_verbose)),
      color = "black",
      inherit.aes = FALSE
    ) +
    #--attribute (missing data)
    ggpattern::geom_rect_pattern(
      data = plot_data |>
        filter(missing == "*"),
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = trunk
      ),
      fill = "transparent",
      color = NA,
      pattern_fill = "black",
      pattern_density = 0.025,
      pattern_spacing = 0.02,
      pattern_angle = 60,
      pattern = "stripe",
      inherit.aes = FALSE
    ) +
    # #--data quality (1-5, NR, X)
    # geom_text(
    #   data = plot_data,
    #   aes(x = xmid, y = trunk + 0.2, label = quality2),
    #   size = 3,
    #   color = "black"
    # ) +
    #--legend
    scale_fill_manual(values = attribute_colors, guide = guide_legend(ncol = 1, order = 1)) +
    labs(
      title = NULL,
      caption = paste0(
        "Substance: ",
        compound_name,
        "\nTotal load score: ",
        total_load_score
      ),
      x = NULL,
      y = NULL,
      fill = NULL
      #fill = "Attributes"
    ) +
    #--theme
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot", #--centers within the entire plot, not just the panel
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      #legend.text = element_text(size = rel(1.5)),
      
      #legend.key.height = unit(0.1, "cm"),   # Increase vertical space between keys
      #legend.key.spacing.y = unit(0.1, "cm"),# Extra space between legend rows
      plot.margin = margin(10, 0, 0, 0), #--it cuts off the tall legend
      
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    #--turn barplot into roseplot
    coord_polar(start = 0)
}



