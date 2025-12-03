#' Create a rose plot of the four categories for a given compound
#'
#' @param compound_name Name of desired compound.
#' @param data The adopt_hpli dataset.
#' @returns A rose plot


# #--for testing
# compound_name <- "diquat"
# data <- data_hpli

fxn_Make_Rose_Plot <- function(compound_name = "diquat",
                               data = data_hpli) {
  metric_colors2 <- c(
    "Environmental fate" =  "#31a354",
    "Ecotoxicity (terrestrial)" = "#fd8d3c",
    "Ecotoxicity (aquatic)" = "#08519c",
    "Human health" = "#7a0177"
  )
  
  metric_names <-
    c(
      "Environmental fate",
      "Ecotoxicity (terrestrial)",
      "Ecotoxicity (aquatic)",
      "Human health"
    )
  
  # Data to plot
  plot_data <-
    data |>
    dplyr::filter(compound == compound_name) |>
    dplyr::select(compound, env_raw, eco.terr_raw, eco.aqua_raw, hum_raw) |>
    tidyr::pivot_longer(env_raw:hum_raw) |>
    dplyr::mutate(
      attribute = metric_names,
      attributeF = factor(attribute, levels = metric_names),
      attribute_num = as.numeric(attributeF)
    ) |>
    #--make dummy x values
    dplyr::mutate(
      xmin = c(0, 120, 180, 240),
      xmid = c(60, 150, 210, 300),
      xmax = c(120, 180, 240, 360)
    )
  
  # Dummy data for background concentric circles
  background <- data.frame(
    xmin = 0,
    xmax = 360,
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
  
  #--Load for title
  plot_title_load <-
    data |>
    dplyr::filter(compound == compound_name) |>
    dplyr::pull(load_score) |>
    round(2)
  
  # Plot
  ggplot2::ggplot(plot_data, ggplot2::aes(
    x = 0,
    #attribute,
    y = value,
    fill = attribute
  )) +
    # Concentric circles
    ggplot2::geom_rect(
      data = background,
      ggplot2::aes(
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
    ggplot2::scale_fill_manual(
      name = " ",
      # breaks = c(0, 0.5, 1.0, 1.5),
      values = c(
        "Low to moderate load" = "white",
        "Moderate to high load" = "gray85",
        "High to very high load" = "gray70"
      ),
      # give the boxes a gray70 outline of size 0.5
      guide = ggplot2::guide_legend(
        override.aes = list(color = "gray70", size  = 0.5)
        ,
        ncol = 1
      )
    ) +
    # Compartment divisions
    ggplot2::geom_segment(
      data = data.frame(x = c(0, 120, 180, 240)),
      ggplot2::aes(
        x = x,
        xend = x,
        y = 0,
        yend = 1.5
      ),
      colour = "gray65",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    # # New fill layer for the metrics
    ggnewscale::new_scale_fill() +
    # Metrics (existing values under 1.5)
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = value,
        fill = attribute
      ),
      show.legend = F,
      color = "black",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = xmid,
        y = 2,
        label = stringr::str_wrap(attribute, 8),
        color = attribute
      ),
      show.legend = F,
      size = 4.5,
      #color = "#8B0000",
      fontface = "italic"
    ) +
    # Legend
    ggplot2::scale_fill_manual(values = metric_colors2, guide = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::scale_color_manual(values = metric_colors2, guide = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(
      title = paste("Compound:", compound_name),
      subtitle = paste("Overall load:", plot_title_load),
      x = NULL,
      y = NULL,
      fill = "Metrics",
      color = "Metrics"
    ) +
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      #legend.box = "vertical",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    ) +
    # axis.ticks.y = element_line(color = "gray33")) +
    # Turn the barplot into a roseplot
    ggplot2::coord_polar(start = 0, clip = "off")
}


#' Create a detailed rose plot of the four categories for a given compound
#'
#' @param compound_name Name of desired compound.
#' @param data The adopt_hpli dataset.
#' @returns A detailed rose plot

# #--for testing
# compound_name <- "diquat"
# data <- data_noe


fxn_Make_Detailed_Rose_Plot <- function(compound_names = c("diquat"),
                                        data = data_noe) {
  # to get things in the desired order --------------------------------------
  
  # Environmental fate metrics
  metrics_envfate <- c(
    "Soil persistence (DT50 soil)"   = "soil_dt50_completed"
    ,
    "Water persistence (DT50 water)" = "water_dt50"
    ,
    "Surface water transfer (Kfoc)"  = "kfoc_completed"
    ,
    "Groundwater transfer (GUS)"     = "gus"
    ,
    "Aquatic biome transfer (BCF)"   = "bcf_completed"
    ## terrestrial bioaccumulation
  )
  
  # Ecotoxicity (terrestrial) metrics
  metrics_ecoterr <- c(
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
  
  # Ecotoxicity (aquatic) metrics
  metrics_ecoaqua <- c(
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
  
  # Human health metrics
  metrics_humheal <- c(
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
  
  metrics <- c(metrics_envfate,
               metrics_ecoterr,
               metrics_ecoaqua,
               metrics_humheal)
  
  metric_names <- names(metrics)
  
  
  
  # compartment names -------------------------------------------------------
  
  compartment_names <-
    c(
      "Environmental fate",
      "Ecotoxicity (terrestrial)",
      "Ecotoxicity (aquatic)",
      "Human health"
    )
  
  # colors ------------------------------------------------------------------
  
  
  metric_colors <- c(
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
  
  plot_data <-
    data |>
    filter(compound == compound_name) |>
    mutate(
      attribute = factor(attribute, levels = metric_names),
      attribute_num = as.numeric(factor(attribute, levels = metric_names))
    )
  
  plot_data2 <-
    plot_data |>
    select(sub_compartment) |>
    distinct() |>
    mutate(
      compartment = case_when(
        sub_compartment == "env" ~ "Environmental fate",
        sub_compartment == "hum" ~ "Human health",
        sub_compartment == "eco.aqua" ~ "Ecotoxicity (aquatic)",
        sub_compartment == "eco.terr" ~ "Ecotoxicity (terrestrial)",
        TRUE ~ NA
      ),
      compartmentF = factor(compartment, levels = compartment_names),
      compartment_num = as.numeric(compartmentF)
    ) |>
    mutate(
      xmin = case_when(
        compartment == "Environmental fate" ~ 0,
        compartment == "Ecotoxicity (terrestrial)" ~ 120 / 360,
        compartment == "Ecotoxicity (aquatic)" ~ 180 / 360,
        compartment == "Human health" ~ 240 / 360
      ),
      xmid = case_when(
        compartment == "Environmental fate" ~ 60 / 360,
        compartment == "Ecotoxicity (terrestrial)" ~ 150 / 360,
        compartment == "Ecotoxicity (aquatic)" ~ 210 / 360,
        compartment == "Human health" ~ 300 / 360
      ),
      xmax = case_when(
        compartment == "Environmental fate" ~ 120 / 360,
        compartment == "Ecotoxicity (terrestrial)" ~ 180 / 360,
        compartment == "Ecotoxicity (aquatic)" ~ 240 / 360,
        compartment == "Human health" ~ 360 / 360
      )
    )
  
  # Dummy data for background concentric circles
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
  
  
  
  # plot --------------------------------------------------------------------
  
  ggplot() +
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
      alpha = 0.5,
      inherit.aes = FALSE
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
    geom_text(
      data = plot_data2,
      aes(
        x = xmid,
        y = 2,
        label = stringr::str_wrap(compartment, 8),
        #color = attribute
      ),
      show.legend = F,
      size = 4.5,
      #color = "#8B0000",
      fontface = "italic"
    ) +
    # Data
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
    # Metrics (missing data)
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
    # Data quality (1-5, NR, X)
    geom_text(
      data = plot_data,
      aes(x = xmid, y = trunk + 0.2, label = quality2),
      size = 3,
      color = "black"
    ) +
    # Legend
    scale_fill_manual(values = metric_colors, guide = guide_legend(ncol = 1)) +
    labs(
      title = NULL,
      x = NULL,
      y = NULL,
      fill = "Metrics"
    ) +
    # Theme
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    # axis.ticks.y = element_line(color = "gray33")) +
    # Turn the barplot into a roseplot
    coord_polar(start = 0)
}


#' Create distribution plot
#'
#' @param compound_names Vector of desired compounds, length of one or more.
#' @param data The adopt_hpli dataset.
#' @returns A distribution of all compounds with the selected one(s) highlighted

fxn_Make_Distribution_Plot <- function(compound_names = c("diquat", "glyphosate"),
                                       data = adopt_hpli) {
  metric_colors2 <- c(
    "Environmental fate" =  "#31a354",
    "Ecotoxicity (terrestrial)" = "#fd8d3c",
    "Ecotoxicity (aquatic)" = "#08519c",
    "Human health" = "#7a0177"
  )
  
  metric_names <-
    c(
      "Environmental fate",
      "Ecotoxicity (terrestrial)",
      "Ecotoxicity (aquatic)",
      "Human health"
    )
  
  plot_compounds <- compound_names
  
  #--distribution of data for all compounds
  plot_data <-
    data |>
    #dplyr::group_by(compound_category) |>
    dplyr::arrange(load_score) |>
    dplyr::mutate(
      n = 1:dplyr::n(),
      n = n / max(n),
      load_score = round(load_score, 2)
    )
  
  #--get
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
  
  
  ggplot2::ggplot() +
    #--rectangles of load division
    ggplot2::geom_rect(
      data = background,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = band
      ),
      #show.legend = F
    ) +
    ggplot2::scale_fill_manual(
      name = " ",
      # breaks = c(0, 0.5, 1.0, 1.5),
      values = c(
        "Low to moderate load" = "white",
        "Moderate to high load" = "gray85",
        "High to very high load" = "gray70"
      ),
      guide = ggplot2::guide_legend(
        override.aes = list(color = "gray70", size  = 0.5),
        ncol = 1
      )
    ) +
    #--line of all compounds
    ggplot2::geom_line(data = plot_data,
                       ggplot2::aes(n, load_score),
                       color = "black") +
    #--reference points
    ggplot2::geom_point(
      data = plot_data |>
        dplyr::filter(load_score == max(load_score) |
                        load_score == min(load_score)),
      ggplot2::aes(n, load_score),
      fill = "black",
      pch = 22,
      size = 3
    ) +
    ggrepel::geom_label_repel(
      data = plot_data |>
        dplyr::filter(load_score == max(load_score) |
                        load_score == min(load_score)),
      ggplot2::aes(n, load_score, label = paste0(compound, " (", load_score, ")")),
      size = 5,
      color = "gray70",
      #point.padding = 5,
      #label.padding = 0.5,
      #min.segment.length = 0.01
    ) +
    #--substance 1
    ggplot2::geom_point(
      data = data_compounds |>
        dplyr::filter(compound %in% plot_compounds),
      ggplot2::aes(n, load_score),
      fill = "red",
      pch = 21,
      size = 5
    ) +
    ggrepel::geom_label_repel(
      data = data_compounds |>
        dplyr::filter(compound %in% plot_compounds),
      ggplot2::aes(n, load_score, label = paste0(compound, " (", load_score, ")")),
      size = 5,
      #color = "gray70",
      point.padding = 5,
      label.padding = 0.5,
      min.segment.length = 0.1
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.5, 1),
      labels = c(
        "Lowest\ntoxicity load",
        "Median\ntoxicity load",
        "Highest\ntoxicity load"
      )
    ) +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = NULL,
      y = "Load\nscore"
    ) +
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      #panel.grid.major.x = element_blank(),
      #panel.grid.major = element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5),
      #axis.text.y = element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
      # plot.margin = ggplot2::margin(t = 0,  # Top margin
      #                      r = 0,  # Right margin
      #                      b = 0,  # Bottom margin
      #                      l = 0)
    )
  
  
  
}
