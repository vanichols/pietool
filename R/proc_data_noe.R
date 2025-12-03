rm(list = ls())

library(tidyverse)
library(readxl)

# data_hpli ---------------------------------------------------------------

#--use data and code provided by Noe

data_noe <- 
  readxl::read_excel("data/raw/data-noe-shiny.xlsx", sheet = "HPLI_load",
                     col_types = c(rep("text", 8), "numeric", "text", rep("numeric", 4))) |>
  mutate(trunk = ifelse(index_value > 1.5, 1.5, index_value),
         quality2 = case_when(
           is.na(quality) & missing == "*" ~ "X",
           is.na(quality) ~ "NR",
           quality == "-Inf" ~ "NR",
           TRUE ~ quality
         ))

data_noe |>
  saveRDS("data/processed/data_noe.RDS")

