# Process raw data into processed data
#--raw data is from adrian's tool, included in raw data folder for reference
#--22 july 2026, changed column name to reflect that its the cost of the average ai

rm(list = ls())

library(tidyverse)
library(readxl)


# costs per median load score-------------------------------------------------------------------

data_pea <- 
  read_excel("data/raw/pea-tool.xlsx", sheet = "costs") |> 
  rename(cost_euros_avg_ai_kg = 2)

data_pea |>
  saveRDS("data/processed/data_pea.RDS")

