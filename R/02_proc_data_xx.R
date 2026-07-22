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

# data_compartments -------------------------------------------------------
#--include the PEA costs here, need to scale them by median load in each compartment

#--load_score2 is the value on the 0-1.5 scale
pea1 <- 
  data_details |> 
  group_by(compound, compartment) |> 
  summarise(load_score = sum(index_value*weight)) |> 
  #--need to correct for the weighting of each compartment
  mutate(load_score2 = case_when(
    compartment == compartment_names[1] ~ load_score * 6,
    compartment == compartment_names[2] ~ load_score * 6,
    compartment == compartment_names[3] ~ load_score * 3,
    compartment == compartment_names[4] ~ load_score * 3
  ))

#--add a 00_not listed compound, etc.
pea2 <- 
  pea1 |> 
  ungroup() |> 
  #--add rows for 00_not listed
  add_row(
    compound = "00_not listed",
    compartment = compartment_names[1],
    load_score = 1/6,
    load_score2 = 1) |> 
  add_row(
    compound = "00_not listed",
    compartment = compartment_names[2],
    load_score = 1/6,
    load_score2 = 1) |> 
  add_row(
    compound = "00_not listed",
    compartment = compartment_names[3],
    load_score = 1/3,
    load_score2 = 1) |> 
  add_row(
    compound = "00_not listed",
    compartment = compartment_names[4],
    load_score = 1/3,
    load_score2 = 1) |> 
  #--add rows for 00_biopesticide
  add_row(
    compound = "00_biopesticide",
    compartment = compartment_names[1],
    load_score = 0,
    load_score2 = 0) |> 
  add_row(
    compound = "00_biopesticide",
    compartment = compartment_names[2],
    load_score = 0,
    load_score2 = 0) |> 
  add_row(
    compound = "00_biopesticide",
    compartment = compartment_names[3],
    load_score = 0,
    load_score2 = 0) |> 
  add_row(
    compound = "00_biopesticide",
    compartment = compartment_names[4],
    load_score = 0,
    load_score2 = 0) |> 
  arrange(compound)



#--get median values for each compartment
comp_med <- 
  pea1 |> 
  group_by(compartment) |> 
  summarise(med_load_score2 = median(load_score2))

#--how different are the means? MEan in eco-aqu is lower, otherwise higher
comp_mean <- 
  pea1 |> 
  group_by(compartment) |> 
  summarise(mean_load_score2 = mean(load_score2))

#--changed costs to per hpli per kg
pea <- 
  readRDS("data/processed/data_pea.RDS") |>
  mutate(
    compartment = dplyr::case_when(
      compartment == "eco.aqua" ~ compartment_names[1],
      compartment == "eco.terr" ~ compartment_names[2],
      compartment == "env" ~ compartment_names[3],
      compartment == "hum" ~ compartment_names[4],
      TRUE ~ "XX"
    )) |>  
  left_join(comp_med) |> 
    mutate(cost_euros_hpli_kg = cost_euros_avg_ai_kg / med_load_score2)


data_compartments <- 
  pea2 |> 
  left_join(pea) |> 
  mutate(loadweightedcost_euros_kg_ref = load_score2 * cost_euros_hpli_kg)


data_compartments |>
  saveRDS("data/processed/data_compartments.RDS")


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
  data_details2 |> 
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

