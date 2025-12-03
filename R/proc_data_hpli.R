# Process raw data into processed data


rm(list = ls())

library(tidyverse)
library(readxl)

# data_hpli ---------------------------------------------------------------

#--use supplemental material from Noe

d1 <- readxl::read_excel("data/raw/Supplementary material Table S2 (V2).xlsx",
                         sheet = "Pesticide Load (by substance)",
                         skip = 3) |>
  dplyr::rename(unknown = 8)

#--the unknown column is empty
d2 <-
  d1 |>
  janitor::remove_empty("cols")

#--in theory there should be no substances with <60% coverage, right?
#--ask Noe about this...
d2 |>
  dplyr::mutate(data_coverage = 1 - missing_share) |>
  ggplot2::ggplot(aes(data_coverage)) +
  ggplot2::geom_histogram()

#--the ind compartments add up to the total load_score
#--to get the 'original' scores for each compartment,
#  undo the weighting by multiplying by 3, 6, 6, and 3

d3 <-
  d2 |>
  dplyr::mutate(env_raw = env*3,
                eco.terr_raw = eco.terr * 6,
                eco.aqua_raw = eco.aqua * 6,
                hum_raw = hum * 3)

#--simplify the compound type column, how many are there?
d3 |>
  dplyr::select(compound_type) |>
  group_by(compound_type) |>
  summarise(n = n()) |>
  arrange(-n)

d4 <-
  d3 |>
  mutate(compound_category = case_when(
    (grepl("Herb", compound_type) == T) &
      (grepl("Insect", compound_type) == F) &
      (grepl("Fung", compound_type) == F)  ~ "Herbicide"
    ,(grepl("Herb", compound_type) == F) &
      (grepl("Insect", compound_type) == T) &
      (grepl("Fung", compound_type) == F)  ~ "Insecticide"
    ,(grepl("Herb", compound_type) == F) &
      (grepl("Insect", compound_type) == F) &
      (grepl("Fung", compound_type) == T)  ~ "Fungicide"
    ,TRUE~"Multiple/Other"
  ))


#--half herbicides, a third fungicides, the rest split between insect and other
d4 |>
  dplyr::select(compound_category) |>
  group_by(compound_category) |>
  summarise(n = n()) |>
  arrange(-n)

d5 <-
  d4 |>
  dplyr::select(compound,
                env_raw:hum_raw,
                env_sc = env,
                eco.terr_sc = eco.terr,
                eco.aqua_sc = eco.aqua,
                hum_sc = hum,
                load_score,
                missing_share,
                cas,
                compound_category,
                everything())

data_hpli <- d5

data_hpli |> 
  saveRDS("data/processed/data_hpli.RDS")
