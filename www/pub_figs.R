# #--make figs for pub / comment out when done
# 
# library(tidyverse)
# library(scales)
# library(patchwork)
# library(cowplot)
# library(ggrepel)
# 
# rm(list = ls())
# 
# 
# # data --------------------------------------------------------------------
# 
# data_details <- read_rds("data/processed/data_details.RDS")
# data_compartments <- read_rds("data/processed/data_compartments.RDS")
# data_totloads <- read_rds("data/processed/data_totloads.RDS")
# 
# data_peacou <- read_rds("data/processed/data_peacou.RDS")
# 
# # Source utility functions (rose plot, distribution plot)
# source("R/utils.R")
# source("R/palettes.R")
# 
# 
# # pinwheel of substance types ---------------------------------------------
# 
# data_details |>
#   select(compound, main_compound_type) |> 
#   distinct() |> 
#   group_by(main_compound_type) |> 
#   summarise(n = n()) |> 
#   arrange(-n) |> 
#   mutate(
#     cmp = paste0(main_compound_type, " (", n, ")"),
#     cmp = str_wrap(cmp, 50),
#     cmpF = factor(cmp, levels = cmp)) |> 
#   ggplot(aes(cmpF, n)) +
#   geom_col() +
#   coord_polar()
# 
# data_details |>
#   select(compound, compound_type) |> 
#   distinct() |> 
#   group_by(compound_type) |> 
#   summarise(n = n()) |> 
#   arrange(-n) |> 
#   mutate(
#     cmp = paste0(compound_type, " (", n, ")"),
#     cmp = str_wrap(cmp, 50),
#     cmpF = factor(cmp, levels = cmp)) |> 
#   ggplot(aes(cmpF, n)) +
#   geom_col() +
#   coord_polar()


# example of soil persistence load scaling --------------------------------

# data_exsoilpersis <- read_rds("data/processed/data_exsoilpersis.RDS")
# 
# data_details2 <-
#   data_details |>
#   ungroup() |>
#   filter(attribute == "Soil persistence (DT50 soil)")
# 
# ex1 <-
#   data_details2 |>
#   filter(compound %in% mysamples) |>
#   select(attribute, index_value = trunk, compound)
# 
# mysamples <- c("clopyralid-olamine",
#                "mesosulfuron",
#                "flupyradifurone")
# 
# ex2 <-
#   ppdb |>
#   filter(substance %in% ex.all$compound) |>
#   select(compound = substance,
#          raw_value = soil_degradation_dt50_field_days) |>
#   mutate(attribute = "Soil persistence (DT50 soil)") |>
#   left_join(ex1)
# 
# #--create reference points
# df_soil <- tibble(
#   attribute = "Soil persistence (DT50 soil)",
#   index_value = c(0, 0.25, 0.5, 1, 1.5),
#   raw_value = c(0, 20, 60, 180, 800),
#   compound = "Reference point"
# )
# 
# 
# d_ex <-
#   ex2 |>
#   bind_rows(df_soil)
# 
# p2 <-
#   data_details2 |>
#   ggplot(aes(trunk))+
#   geom_histogram(fill = "gray", bins = 40) +
#     coord_cartesian(xlim = c(0, 1.5)) +
#     labs(x = NULL,
#          y = "Frequency") +
#   coord_flip() +
#   #--theme
#   theme_minimal() +
#   theme(
#     plot.caption = element_text(face = "italic"),
#     legend.position = "right",
#     legend.title = element_text(face = "bold"),
#     legend.text = element_text(size = rel(1.5)),
#     panel.grid.major.x = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
# 
#     plot.margin = margin(10, 50, 10, 10),
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     #axis.text.x = element_text(face = "italic", color = "gray"),
#     # axis.title.x = element_text(angle = 0, vjust = 0.5,
#     #                             #face = "italic",
#     #                             color = "gray",
#     #                             size = rel(1.5)),
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# p2
# 
# p1 <- d_ex |>
#   ggplot(aes(raw_value, index_value)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(aes(color = compound, pch = compound, size = compound), stroke = 2, show.legend = F) +
#   geom_text(data = d_ex |> filter(compound != "Reference point"),
#                   aes(
#                     x = raw_value+25,
#                     y = index_value-0.12,
#                     label = paste(compound,
#                               ",\n",
#                               round(raw_value, 0),
#                               "days,\nload of",
#                               round(index_value, 2) ),
#                     color = compound),
#             #hjust = 0,
# 
#             #vjust = 0,
#             show.legend = F) +
#   scale_shape_manual(values = c(0, 3, 2,  16)) +
#   scale_size_manual(values = c(6, 6, 6,  6)) +
#   scale_linewidth_manual(values = c(2, 2, 2,  1)) +
#   scale_color_manual(values = c(adopt1, adopt2,  adopt4, "black")) +
#   labs(x = "\nHalf-life of substance in soil (days)",
#        y = "Load\nindex",
#        title = "Soil persistence") +
#   coord_cartesian(xlim = c(0, 200),
#                   ylim = c(0, 1.5)) +
#   #--theme
#   theme_minimal() +
#   theme(
#     plot.caption = element_text(face = "italic"),
#     legend.position = "top",
#     legend.direction = "horizontal",
#     legend.title = element_text(face = "bold"),
#     #panel.grid.major.x = element_blank(),
#     #panel.grid.major = element_blank(),
#     #panel.grid.minor = element_blank(),
#     #axis.text.x = element_blank(),
#     axis.text = element_text(size = rel(1.5)),
#     axis.title = element_text(size = rel(1.75)),
#     axis.title.y = element_text(size = rel(1.75), angle = 0, vjust = 0.5),
#     plot.title = element_text(hjust = 0.5, face = "bold", size = rel(2)),
#     plot.subtitle = element_text(hjust = 0.5))
# 
# 
# p1 + p2  +
#   plot_layout(widths = c(3, 1)) #&
#   #theme(plot.margin = margin(10, 30, 10, 10)) # margin for all plots in composition
# 
# ggsave("www/soil-persistence-ex.png",
#        width = 10, height = 7)

# # simple rose plot for fig 1/graphical abstract--------------------------------------------------------------------
# 
# 
# fxn_Make_Donut_Plot() +
#   theme(legend.position = "none")
# 
# ggsave("www/loads-ex-fig.png",
#        width = 6, height = 8)
# 
# 
# # costs plot --------------------------------------------------------------
# 
# fxn_Make_Costs_Plot()
# 
# #--flip for graphical abstract space wise
# fxn_Make_Costs_Plot2()
# ggsave("www/costs-ex-fig.png",
#        width = 4, height = 6)
# 
# 
# # side by side details ----------------------------------------------------
# 
# # simple rose plot --------------------------------------------------------------------
# 
# 
# p1 <-
#   fxn_Make_Donut_Plot(hole_text_size = 3) +
#   labs(title = NULL,
#        subtitle = NULL) +
#   theme(legend.position = "right",
#         legend.text = element_text(size = rel(1)))
# 
# p1$layers <- p1$layers[-5]
# 
# p1
# 
# p2 <-
#   fxn_Make_Detailed_Donut_Plot(hole_text_size = 8) +
#   guides(fill = guide_legend(ncol = 1)) +
#   labs(caption = NULL) +
#   theme(legend.position = "right",
#         legend.text = element_text(size = rel(1)))
# 
# 
# p2$layers <- p2$layers[c(-4, -7)]
# 
# p2
# 
# p2 + p1 +
#   plot_annotation(
#     #title = 'diquat',
#     #subtitle = 'Total load score: 0.78',
#     tag_levels = 'A'
#   ) &
#   theme(      plot.title = element_text(hjust = 0.5, face = "bold", size = rel(2)),
#               plot.subtitle = element_text(hjust = 0.5, size = rel(1.5)),
#               plot.tag = element_text(size = rel(2), face = "bold"))
# 
# ggsave("www/starbursts-ex-fig.png",
#        width = 12, height = 6)
# 
# 
# 
# # distribution view -------------------------------------------------------
# 
# fxn_Make_Reactive_Beeswarm_Plot(compound_name = "diquat")
# 
# ggsave("www/beeswarm-ex-fig.png",
#        width = 8, height = 6)
# 
# 
# 

# donut total load --------------------------------------------------------


