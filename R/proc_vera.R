library(tidyverse)
library(readxl)


d <- read_excel("data/raw/Vera_PL_Data - Zenodo download.xlsx") 


#--compare to data_details
data_details <- read_rds("data/processed/data_details.RDS")
data_compartments <- read_rds("data/processed/data_compartments.RDS")


#--make ex for vera
data_details_ex <- 
  data_details |>
  ungroup() |> 
  rename(active_ingredient = compound) |> 
  select(active_ingredient, tot_load_score, compartment, attribute,
         quality,
         PL_value_raw = index_value,
         PL_value = trunk) |> 
  filter(active_ingredient == "1,3-dichloropropene")

write_csv(data_details_ex, "data/raw/data_details_ex_for_vera.csv")

#--make ex for vera
data_compartments_ex <- 
  data_compartments |> 
  rename(active_ingredient = compound) |> 
  select(active_ingredient, compartment, 
         load_score_component = load_score, 
         load_score_absolute = load_score2) |> 
  filter(active_ingredient == "1,3-dichloropropene")

write_csv(data_compartments_ex, "data/raw/data_compartments_ex_for_vera.csv")


