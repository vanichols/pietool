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
      guide = ggplot2::guide_legend(override.aes = list(
        color = "gray70", size  = 0.5)
        , ncol = 1)
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
    dplyr::mutate(n = 1:dplyr::n(), n = n / max(n),
                  load_score = round(load_score, 2))
  
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
        dplyr::filter(load_score == max(load_score)| load_score == min(load_score)),
      ggplot2::aes(n, load_score),
      fill = "black",
      pch = 22,
      size = 3
    ) +
    ggrepel::geom_label_repel(
      data = plot_data |>
        dplyr::filter(load_score == max(load_score)| load_score == min(load_score)),
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

