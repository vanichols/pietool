# Process raw data into processed data
#--raw data is from adrian's tool, included in raw data folder for reference
#--change default costs to EU level...euros?

rm(list = ls())

library(tidyverse)
library(readxl)

euros_per_englishpound <- 1.15


# multipliers -------------------------------------------------------------

d1 <- 
  read_excel("data/raw/pea-tool.xlsx", sheet = "GDP", col_types = c("text", "numeric", "numeric")) |> 
  mutate(across(where(is.numeric), ~ na_if(., 0))) |> 
  #filter(!is.na(GDP_percapita_multiplier)) |> 
  rename(country = Country)

#--should filter to only include countries in europe
d2 <- read_excel("data/raw/european-countries.xlsx")

data_peacou <- 
  d1 |> 
  filter(country %in% c(d2$country, "EU"))

data_peacou |>
  saveRDS("data/processed/data_peacou.RDS")

# costs -------------------------------------------------------------------

data_pea <- 
  read_excel("data/raw/pea-tool.xlsx", sheet = "costs") |> 
  mutate(cost_euros_kg_ref = cost_englishpounds_kg * euros_per_englishpound)

data_pea |>
  saveRDS("data/processed/data_pea.RDS")

