library(tidyverse)
library(ggExtra)
library(ggrepel)

d1 <- 
  readxl::read_excel("data/raw/EIQ_costs_per_ai_as_ref.xlsx") |> 
  janitor::clean_names() |> 
  mutate(eiq_cost_kgai = cost_kg_a_i/10000,
         compound = str_to_lower(active_ingredient)) |> 
  arrange(-eiq_cost_kgai) |> 
  mutate(eiq_rank = 1:n())


d2 <- 
  read_rds("data/processed/data_totloads.RDS") |> 
  select(compound, tot_load_score, pie_cost_kgai = totcost_euros_kg_ref)

d3 <- 
  d2 |> 
  filter(compound %in% d1$compound) |> 
  arrange(-pie_cost_kgai) |> 
  mutate(pie_rank = 1: n())

d <- 
  d1 |> 
  left_join(d3) |> 
  mutate(cmp_label = ifelse(eiq_cost_kgai > 10|pie_cost_kgai > 20, compound, NA))

# figs --------------------------------------------------------------------


d1 |> 
  left_join(d3) |> 
  ggplot(aes(eiq_rank, pie_rank)) +
  geom_point(aes(size = eiq_cost_kgai, color = pie_cost_kgai))


d |> 
  ggplot(aes(eiq_cost_kgai, pie_cost_kgai)) +
  geom_point() +
  geom_label_repel(aes(label = cmp_label))

p <- 
  d |> 
  ggplot(aes(eiq_cost_kgai, pie_cost_kgai)) +
  geom_point()

ggMarginal(
  d |> 
    ggplot(aes(eiq_cost_kgai, pie_cost_kgai)) +
    geom_point() +
    scale_x_continuous(limits = c(5, 15))+
    scale_y_continuous(limits = c(5, 30)) +
    geom_label_repel(aes(label = cmp_label))
  ,
  type = "histogram",
  bins = 20
  
)
