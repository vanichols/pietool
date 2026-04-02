# library(readxl)
# library(tidyverse)
#
#--only run once, delete database from shiny app after running
# 
# #--need raw values for random 10 chosen
# ppdb <- readxl::read_excel("data/raw/PPDB_Aarhus_University_26-01-18.xlsx") |>
#   janitor::clean_names()
# 
# data_details2 <-
#   data_details |>
#   ungroup() |>
#   filter(attribute == "Soil persistence (DT50 soil)")
# 
# #--the chosen values...
# mysamples <- c("clopyralid-olamine",
#                "mesosulfuron",
#                #"aclonifen",
#                "flupyradifurone")
# 
# 
# data_exsoilpersis <-
#   ppdb |>
#   filter(substance %in% ex.all$compound) |>
#   select(compound = substance,
#          raw_value = soil_degradation_dt50_field_days) |>
#   mutate(attribute = "Soil persistence (DT50 soil)")
# 
# 
# data_exsoilpersis |> 
#   write_rds("data/processed/data_exsoilpersis.RDS")
