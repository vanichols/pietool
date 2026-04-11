# Process raw data into processed data

rm(list = ls())

library(tidyverse)
library(readxl)

data_total_load_ex <- readxl::read_excel("data/raw/raw-sunburst-fig-data-from-jon.xlsx",
                         sheet = "raw") 


data_total_load_ex |> 
  write_rds("data/processed/data_total_load_ex.RDS")

