# Process raw data into processed data
#--data used by util fxns

#--note - Noe's two datasets don't seem to match quite right (total loads...)
#--20 feb 2026, added 00_not listed as a compound with loads of 1 for everything
#--19 mar 2026, add 00_biopesticide as a compound with loads of 0 for everything
#--updated quality ratings to not be numbers
#--22 july 2026 separated data_pie for better tracking


rm(list = ls())

library(tidyverse)
library(readxl)

#--in case I decide to change them...
compartment_names <-
  c(
    "Ecotoxicity, aquatic",
    "Ecotoxicity, terrestrial",
    "Environmental fate",
    "Human health"
  )

data_details <- 
  readRDS("data/processed/data_details.RDS")

data_compartments <- readRDS("data/processed/data_compartments.RDS")

# data_totloads --------------------------------------------------

#--get total cost per compound
pea3 <-
  data_compartments |> 
  group_by(compound) |> 
  summarise(totcost_euros_kg_ref = sum(loadweightedcost_euros_kg_ref )) 

#--get the compartments in wide format
data_compartments_wide <- 
  data_compartments |> 
  select(compound, compartment, load_score2) |> 
  pivot_wider(names_from = compartment, values_from = load_score2) |> 
  janitor::clean_names() 

data_totloads <- 
  data_details |> 
  select(compound, tot_load_score) |> 
  distinct() |> 
  ungroup() |> 
  add_row(compound = "00_not listed", tot_load_score = 1) |> 
  add_row(compound = "00_biopesticide", tot_load_score = 0) |> 
  left_join(pea3) |> 
  left_join(data_compartments_wide) |> 
  arrange(compound)
 
#--test tot load score matching, they do!
data_totloads |> 
  mutate(tot_test = ecotoxicity_aquatic /6 + ecotoxicity_terrestrial/6 + environmental_fate /3 + human_health/3) |> 
  select(compound, tot_load_score, tot_test) |> 
  arrange(compound)

data_totloads |> 
  ggplot(aes(totcost_euros_kg_ref))+
  geom_histogram()

data_totloads |> 
  arrange(-totcost_euros_kg_ref)

data_totloads |>
  saveRDS("data/processed/data_totloads.RDS")

