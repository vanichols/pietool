# Process raw data into processed data
#--data used by util fxns

#--note - Noe's two datasets don't seem to match quite right (total loads...)

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

# data noe ----------------------------------------------------------------

#--use combination of noe's data and his supplemental material data
#--use data from code provided by Noe

data_noe1 <- 
  readxl::read_excel("data/raw/data-noe-shiny.xlsx", sheet = "HPLI_load",
                     col_types = c(rep("text", 8), "numeric", "text", rep("numeric", 4))) |>
  mutate(trunk = ifelse(index_value > 1.5, 1.5, index_value),
         trunk_ind = ifelse(trunk != index_value, "Y", "N"),
         quality2 = case_when(
           is.na(quality) & missing == "*" ~ "X",
           is.na(quality) ~ " ",
           quality == "-Inf" ~ " ",
           TRUE ~ quality
         )) |> 
  select(-value, -qualifier, -value_chr, -interpolator)

#--only five instances where value > 1.5
data_noe1 |> 
  filter(trunk != index_value)

#--this has the total loads, they don't match data_noe1...
data_noe2 <- readxl::read_excel("data/raw/data-noe-shiny.xlsx", sheet = "HPLI_detail")

data_noe2 |> 
  select(compound, sum) |> 
  distinct() |> 
  arrange(-sum)

#--this has the detailed information about the compounds
data_noe3 <- 
  readxl::read_excel("data/raw/data-noe-shiny.xlsx", sheet = "HPLI_info")

#--35 appear in noe1 and not in the info sheet..fill them in with 'Missing'
data_noe3_new <- 
  data_noe1 |> 
  select(compound) |> 
  distinct() |> 
  left_join(data_noe3 |> select(compound, cas, compound_type, compound_group, compound_origin), relationship = "many-to-many") |> 
  mutate(across(everything(), ~replace_na(.x, "Missing")))

#--keep what noe did just in case (I don't understand it)
data_noe5 <- 
  data_noe1 |> 
  left_join(data_noe3_new, relationship = "many-to-many") |> 
  mutate(
    main_compound_type = str_replace(
      str_trim(compound_type), "^([^,]+),\\s*(.+)$", "\\1"),
    sub_compound_type = if_else(
      str_detect(compound_type, ","),                                           # only if comma exists
      str_replace(str_trim(compound_type), "^([^,]+),\\s*(.+)$", "\\2"),        # empty string if no comma
      ""),
    compound_origin = ifelse(
      compound_origin == "Natural; Mixture",
      "Natural (mixture)",
      compound_origin))

#--keep my simplification
data_noe <- 
  data_noe5 |> 
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

#--what about the compartments? Something is wrong there, tI think

data_tmp <- 
  data_hpli |> 
  select(compound, env_raw, eco.terr_raw, eco.aqua_raw, hum_raw) |> 
  pivot_longer(env_raw:hum_raw) |> 
  mutate(sub_compartment = str_remove_all(name, "_raw"))

#--they are raw, not adjusted
data_noe |> 
  group_by(compound, sub_compartment) |> 
  summarise(load_score_noe = sum(index_value*weight)) |> 
  left_join(data_tmp)

#--yes, this is the issue, add the reverse weighting factor
data_noe |> 
  group_by(compound, sub_compartment) |> 
  summarise(load_score_noe = sum(index_value*weight)) |> 
  left_join(data_tmp) |> 
  mutate(new_score = case_when(
    sub_compartment == "env" ~ load_score_noe * 3,
    TRUE ~ NA
  ))



# use data_noe ------------------------------------------------------------

data_details <- 
  data_noe |> 
  mutate(
  compartment = dplyr::case_when(
    sub_compartment == "eco.aqua" ~ compartment_names[1],
    sub_compartment == "eco.terr" ~ compartment_names[2],
    sub_compartment == "env" ~ compartment_names[3],
    sub_compartment == "hum" ~ compartment_names[4],
    TRUE ~ "XX"
  )) |> 
  group_by(compound) |> 
  mutate(tot_load_score = sum(weight * index_value)) |> 
  arrange(compound, compartment, attribute) |> 
  select(compound, compartment, tot_load_score, attribute, everything())



data_details |>
  saveRDS("data/processed/data_details.RDS")

# data_compartments -------------------------------------------------------
#--include the PEA costs here
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
  select(-cost_englishpounds_kg)

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

#--use the multiplier, depending on the load_score? not necessary, its already scaled via the load_score

data_compartments <- 
  pea1 |> 
  left_join(pea) |> 
  mutate(loadweightedcost_euros_kg_ref = load_score2 * cost_euros_kg_ref)

data_compartments |>
  saveRDS("data/processed/data_compartments.RDS")


# data_totloads --------------------------------------------------

#--get total cost per compound
pea3 <-
  data_compartments |> 
  group_by(compound) |> 
  summarise(totcost_euros_kg_ref = sum(loadweightedcost_euros_kg_ref ))

data_totloads <- 
  data_details |> 
  select(compound, tot_load_score) |> 
  distinct() |> 
  left_join(pea3)
 
data_totloads |> 
  ggplot(aes(totcost_euros_kg_ref))+
  geom_histogram()

data_totloads |>
  saveRDS("data/processed/data_totloads.RDS")

