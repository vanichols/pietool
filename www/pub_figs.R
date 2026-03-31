# #--make figs for pub / comment out when done
# 
# library(tidyverse)
# library(scales)
# library(patchwork)
# library(cowplot)
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
# 
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



# experimenting -----------------------------------------------------------
compound_names = c("diquat")
data = data_details
compound_type = "Fungicide"

