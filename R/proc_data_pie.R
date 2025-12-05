# Process raw data into processed data
#--use combination of noe's data and his supplemental material data

rm(list = ls())

library(tidyverse)
library(readxl)


# data noe ----------------------------------------------------------------

#--use data from code provided by Noe

d0 <- 
  readxl::read_excel("data/raw/data-noe-shiny.xlsx", sheet = "HPLI_load",
                     col_types = c(rep("text", 8), "numeric", "text", rep("numeric", 4))) |>
  mutate(trunk = ifelse(index_value > 1.5, 1.5, index_value),
         trunk_ind = ifelse(trunk != index_value, "Y", "N"),
         quality2 = case_when(
           is.na(quality) & missing == "*" ~ "X",
           is.na(quality) ~ "NR",
           quality == "-Inf" ~ "NR",
           TRUE ~ quality
         ))

#--only five instances where value > 1.5
d0 |> 
  filter(trunk != index_value)

data_noe <- 
  d0 |> 
  dplyr::select(compound, attribute, sub_compartment, weight, xmax, xmin, xmid, trunk, index_value, quality2)


# data_hpli ---------------------------------------------------------------

#--use supplemental material from Noe, has less substances compared to data_noe

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



# combined ----------------------------------------------------------------

#--35 compounds are in data_noe but not data_hpli
data_noe |> 
  left_join(data_hpli) |> 
  filter(is.na(cas)) |> 
  select(compound) |> 
  distinct()

#--what if we calculate it directly from data_noe - this works, gets same answers

data_noe |> 
  group_by(compound) |> 
  summarise(load_score_noe = sum(index_value*weight)) |> 
  left_join(data_hpli |> select(compound, load_score)) |> 
  ggplot(aes(load_score_noe, load_score)) +
  geom_point()


data_noe |> 
  group_by(compound) |> 
  summarise(load_score_noe = sum(index_value*weight))


# use data_noe ------------------------------------------------------------


data_pie <- 
  data_noe

data_pie |>
  saveRDS("data/processed/data_pie.RDS")
