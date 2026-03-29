#--make figs for pub

library(tidyverse)
library(scales)
library(patchwork)

rm(list = ls())

source("R/utils.R")

# data --------------------------------------------------------------------

fxn_Make_Costs_Plot2()
ggsave("www/costs-ex-fig.png", 
       width = 4, height = 6)  
