# load the packages
library(sf)
library(sp)
library(COINr6)
library(tidyverse)
library(tibble)

grid <- st_read("data/geographic/grids/grid_all-v-07.11.22_20.02.gpkg")
grid_df <- grid %>% 
  mutate(UnitName = as.character(1:nrow(grid))) %>%
  mutate(UnitCode = as.character(UnitName)) %>%
  st_drop_geometry()

summary(grid_df)
data <- as_tibble(grid_df)

IndName<-c("Median Income","Number of Households","Percentage Maori","Percentage Dampness", "no_intersections_in_100m",
           "station_dist", "bus_dist", "ev_dist", "conv_st_dist", "petrol_st_dist", 
           "biking_len_100m", "second_dist", "primary_dist","childcare_dist", 
           "cinemas_dist", "galleries_dist", "libraries_dist", "museum_dist",
           "theatre_dist","bigpark_dist","smallpark_dist","chemists_dist",
           "dentist_dist","supermarket_dist")
IndCode <- c("income","no_houses","maori","damp","intersections",
             "train", "bus","ev","con_sto", "petrol", 
             "bike","secondary","primary","childcare", 
             "cinema", "gallery", "library", "museum",
             "theatre","bigpark","smallpark","chemist",
             "dentist","supermarket")
Direction <- c(1,1,1,-1,1,
               -1,-1,-1,-1,-1,
               1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1)
IndUnit <- c("dollars","number","percentage","percentage","number within 100 m buffer",
             "m", "m","ev","m", "m", 
             "metres within 100 m buffer","m","m","m", 
             "m", "m", "m", "m",
             "m","m","m","m",
             "m","m")
IndWeight <- rep(1,24)
Agg1 <- c("None","None","None","None","Walkability",
          "Transportation", "Transportation","Transportation","Walkability", "Walkability", 
          "Bikeability","Social","Social","Social", 
          "Social", "Social", "Social", "Social",
          "Social","Greenery","Greenery","Medical",
          "Medical","Walkability")

metadata <- as_tibble(data.frame(IndName,IndCode,Direction,IndUnit,IndWeight,Agg1))

AgLevel <- rep(1,7)
Code <- c("None","Walkability","Transportation","Bikeability","Social","Greenery","Medical")
Name <- c("None","Walkability","Transportation","Bikeability","Social","Greenery","Medical")
Weight <- rep(1,7)

aggmeta <- as_tibble(data.frame(AgLevel,Code,Name,Weight))

# Assemble COIN
KULI <- assemble(IndData = data, IndMeta = metadata, AggMeta = aggmeta)





