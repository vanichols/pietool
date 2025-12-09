#' Create money plot
#'
#' @param compound_names Vector of desired compounds, length of one or more.
#' @param data The dataset.
#' @param data2 The dataset with adjustments to the pea value for each EU country.
#' @param country_adjuster The desired country adjustment, default is EU-wide.
#' @returns A bar graph showing the compartment costs stacked together

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
  
  plot2_data <- 
    data_new |> 
    group_by(compound) |> 
    summarise(tot_cost = sum(cost_euros_kg))
  
  plot1 <-
    plot1_data |> 
    ggplot(aes(compound, cost_euros_kg)) +
    geom_col(aes(fill = compartment), color = "black") +
    scale_fill_manual(values = compartment_colors, 
                      guide = guide_legend(
                        nrow = 1, 
                        reverse = T, 
                        order = 1, 
                        title.position = "top",
                        title.hjust = 0.5)) +
    scale_y_continuous(labels = label_currency(prefix = "€"),
                       limits = c(0, max(plot2_data$tot_cost))) +
  labs(
    #caption = paste0("*Adjusted to per captita GDP of: ", country_adjuster),
    x = NULL,
    y = "Societal costs*\n(€/kg)",
    fill = "Compartments"
  ) +
    coord_flip() +
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
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) 
  
  
  
  plot2 <- 
    ggplot() +
    geom_histogram(data = plot2_data, aes(x = tot_cost), fill = "gray", bins = 30) +
      geom_point(data = plot2_data |> filter(compound == compound_name), 
                 aes(x = tot_cost, y = 0),
                 color = "black", size = 6, pch = 18) +
      scale_x_continuous(labels = label_currency(prefix = "€")) +
      labs(
        caption = paste0("*Adjusted to per captita GDP of: ", country_adjuster),
        y = "Number of\ncompounds",
        x = "Societal costs*\n(€/kg)"
      ) +
      #--theme
      theme_minimal() +
      theme(
        plot.caption = element_text(face = "italic"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #axis.text.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      ) 

  
    plot1 / plot2  +
      plot_layout(heights = c(1, 2))
  
      

}

#' Create a rose plot of the four categories for a given compound
#'
#' @param compound_name Name of desired compound.
#' @param data The dataset.
#' @returns A rose plot


# #--for testing
#  compound_name <- "diquat"
# data <- data_details

fxn_Make_Rose_Plot <- function(compound_name = "diquat",
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
    filter(compound == compound_name) |> 
    dplyr::mutate(
      compartment_label = paste0(compartment, " (", round(load_score2, 2), ")"),
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
  )
  
  # Dummy data for background concentric circles
  background <- data.frame(
    xmin = 0,
    xmax = 1,
    ymin = c(0, 0.5, 1.0),
    ymax = c(0.5, 1.0, 1.5),
    band = factor(
      c(
        "Low to moderate load",
        "Moderate to high load",
        "High to very high load"
      ),
      levels = c(
        "Low to moderate load",
        "Moderate to high load",
        "High to very high load"
      )
    )
  )
  
  data_total_load_score <- 
    plot_data |> 
    group_by(compound) |> 
    summarise(tot_load_score = sum(load_score))
  
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
        "Low to moderate load" = "white",
        "Moderate to high load" = "gray85",
        "High to very high load" = "gray70"
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
      data = data.frame(x = c(0, 1/3, 1/2, 2/3)),
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
    geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = load_score2,
        fill = compartment
      ),
      color = "black",
      inherit.aes = FALSE
    ) +
    # Labelling compartments
    geom_text(
      aes(
        x = xmid,
        y = 2,
        label = stringr::str_wrap(compartment_label, 8)
      ),
      show.legend = F,
      size = 4.5,
      color = "black",
      #color = "#8B0000",
      fontface = "italic"
    ) +
    # Legend
    scale_fill_manual(values = compartment_colors, guide = guide_legend(ncol = 1, reverse = T, order = 1)) +
    labs(
      caption = paste0("Substance: ", compound_name),# "\nTotal load score: ", total_load_score),
      x = NULL,
      y = NULL,
      fill = "Compartments"
    ) +
    # Theme
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    # Turn the barplot into a roseplot
    coord_polar(start = 0, clip = "off")
}


#' Create a detailed rose plot of the four categories for a given compound
#'
#' @param compound_name Name of desired compound.
#' @param data The dataset.
#' @returns A detailed rose plot

# #--for testing
# compound_name <- "diquat"
# data <- data_details


fxn_Make_Detailed_Rose_Plot <- function(compound_name = "diquat",
                                        data = data_details) {
  
  # get things in the desired order --------------------------------------
  
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
  
  # compartment names -------------------------------------------------------
  
  compartment_names <-
    c(
      "Environmental fate",
      "Ecotoxicity, terrestrial",
      "Ecotoxicity, aquatic",
      "Human health"
    )
  
  # colors ------------------------------------------------------------------
  
  
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
  
  
  # data to plot ------------------------------------------------------------
  
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
  
  # plot --------------------------------------------------------------------
  
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
        "Low to moderate" = "white",
        "Moderate to high" = "gray85",
        "High to very high" = "gray70"
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
    geom_text(
      data = plot_data2,
      aes(
        x = xmid,
        y = 2,
        label = stringr::str_wrap(compartment, 8),
      ),
      show.legend = F,
      size = 4.5,
      fontface = "italic"
    ) +
    #--attribute data
    ggnewscale::new_scale_fill() +
    geom_rect(
      data = plot_data,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = trunk,
        fill = attribute
      ),
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
    #--data quality (1-5, NR, X)
    geom_text(
      data = plot_data,
      aes(x = xmid, y = trunk + 0.2, label = quality2),
      size = 3,
      color = "black"
    ) +
    #--legend
    scale_fill_manual(values = attribute_colors, guide = guide_legend(ncol = 1, order = 1)) +
    labs(
      title = NULL,
      caption = paste0("Compound: ", compound_name, "\nTotal load score: ", total_load_score),
      x = NULL,
      y = NULL,
      fill = "Attributes"
    ) +
    #--theme
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    #--turn barplot into roseplot
    coord_polar(start = 0)
}


#' Create distribution plot
#'
#' @param compound_names Vector of desired compounds, length of one or more.
#' @param data The dataset.
#' @returns A distribution of all compounds with the selected one(s) highlighted

fxn_Make_Distribution_Plot <- function(compound_names = c("diquat", "glyphosate"),
                                       data = data_details) {
 
  plot_compounds <- compound_names
  
  #--distribution of data for all compounds
  plot_data <-
    data |>
    group_by(compound) |> 
    summarise(load_score = sum(index_value*weight)) |> 
    dplyr::arrange(load_score) |>
    dplyr::mutate(
      n = 1:dplyr::n(),
      n = n / max(n),
      load_score = round(load_score, 2)
    )
  
  number_of_compounds <- nrow(plot_data)
  
  #--get just the desired compounds
  data_compounds <-
    plot_data |>
    dplyr::filter(compound %in% plot_compounds) |>
    dplyr::select(compound, n, load_score) |>
    dplyr::mutate(load_score = round(load_score, 2))
  
  
  #--rectangle for load levels
  background <- data.frame(
    xmin = 0,
    xmax = 1,
    ymin = c(0, 0.5, 1.0),
    ymax = c(0.5, 1.0, 1.5),
    band = factor(
      c(
        "Low to moderate load",
        "Moderate to high load",
        "High to very high load"
      ),
      levels = c(
        "Low to moderate load",
        "Moderate to high load",
        "High to very high load"
      )
    )
  )
  
  
  ggplot() +
    #--rectangles of load division
    geom_rect(
      data = background,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = band
      ),
      #show.legend = F
    ) +
    scale_fill_manual(
      name = " ",
      # breaks = c(0, 0.5, 1.0, 1.5),
      values = c(
        "Low to moderate load" = "white",
        "Moderate to high load" = "gray85",
        "High to very high load" = "gray70"
      ),
      guide = guide_legend(
        override.aes = list(color = "gray70", size  = 0.5),
        nrow = 1
      )
    ) +
    #--line of all compounds
    geom_line(data = plot_data,
                       aes(n, load_score),
                       color = "black") +
    #--reference points
    geom_point(
      data = plot_data |>
        dplyr::filter(load_score == max(load_score) |
                        load_score == min(load_score)),
      aes(n, load_score),
      fill = "black",
      pch = 22,
      size = 3
    ) +
    ggrepel::geom_label_repel(
      data = plot_data |>
        dplyr::filter(load_score == max(load_score) |
                        load_score == min(load_score)),
      aes(n, load_score, label = paste0(compound, " (", load_score, ")")),
      size = 5,
      color = "gray70",
      #point.padding = 5,
      #label.padding = 0.5,
      #min.segment.length = 0.01
    ) +
    #--substance 1
    geom_point(
      data = data_compounds |>
        dplyr::filter(compound %in% plot_compounds),
      aes(n, load_score),
      fill = "red",
      pch = 21,
      size = 5
    ) +
    # ggrepel::geom_label_repel(
    #   data = data_compounds |>
    #     dplyr::filter(compound %in% plot_compounds),
    #   aes(n, load_score, label = paste0(compound, " (", load_score, ")")),
    #   size = 5,
    #   #color = "gray70",
    #   point.padding = 5,
    #   label.padding = 0.5,
    #   min.segment.length = 0.1
    # ) +
    scale_x_continuous(
      breaks = c(0, 0.5, 1),
      labels = c(
        "Lowest\nhazard compound",
        "Median\nhazard compound",
        "Highest\nhazard compound"
      )
    ) +
    labs(
      title = NULL,
      subtitle = NULL,
      caption = paste("Database currently includes", number_of_compounds, "substances"),
      x = NULL,
      y = "Load\nscore"
    ) +
    # Theme
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      plot.caption = element_text(face = "italic"),
      #panel.grid.major.x = element_blank(),
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      #axis.text.y = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
      # plot.margin = margin(t = 0,  # Top margin
      #                      r = 0,  # Right margin
      #                      b = 0,  # Bottom margin
      #                      l = 0)
    )
  
  
  
}
