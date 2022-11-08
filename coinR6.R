# load the packages
library(sf)
library(sp)
library(COINr6)
library(tidyverse)
library(tibble)

grid <- st_read("data/geographic/grids/grid_all-v-07.11.22_20.02.gpkg")
idx <- data.frame(1:nrow(grid)) %>%
  mutate(UnitName = as.character(1:nrow(grid))) %>% 
  mutate(UnitCode = UnitName)

grid_df <- idx %>% 
  cbind(grid) %>% 
  mutate(x_lon = lon) %>%
  mutate(x_lat = lat) %>%
  subset(select = c(-lat, -lon, -X1.nrow.grid., -geom))

summary(grid_df)
# Indicators Data
data <- grid_df

# Indicators Metadata
IndName<-c("UnitName","UnitCode","Median Income","Number of Households","Percentage Maori","Percentage Dampness", "no_intersections_in_100m",
           "station_dist", "bus_dist", "ev_dist", "conv_st_dist", "petrol_st_dist", 
           "biking_len_100m", "second_dist", "primary_dist","childcare_dist", 
           "cinemas_dist", "galleries_dist", "libraries_dist", "museum_dist",
           "theatre_dist","bigpark_dist","smallpark_dist","chemists_dist",
           "dentist_dist","supermarket_dist","longitude", "latitude")
IndCode <- c("UnitName","UnitCode","income","no_households","maori_pr","dampness","no_intersections_in_100m",
             "station_dist", "bus_dist","ev_dist","conv_st_dist", "petrol_st_dist", 
             "biking_len_100m","second_dist","primary_dist","childcare_dist", 
             "cinemas_dist", "galleries_dist", "libraries_dist", "museum_dist",
             "theatre_dist","bigpark_dist","smallpark_dist","chemists_dist",
             "dentist_dist","supermarket_dist", "x_lon", "x_lat")
Direction <- c(1,1,1,1,1,-1,1,
               -1,-1,-1,-1,-1,
               1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1,0,0)
IndUnit <- c("None","None","dollars","number","percentage","percentage","number within 100 m buffer",
             "m", "m","ev","m", "m", 
             "metres within 100 m buffer","m","m","m", 
             "m", "m", "m", "m",
             "m","m","m","m",
             "m","m",0,0)
IndWeight <- rep(1,28)
Agg1 <- c("None","None","None","None","None","None","Walkability",
          "Transportation", "Transportation","Transportation","Walkability", "Walkability", 
          "Bikeability","Social","Social","Social", 
          "Social", "Social", "Social", "Social",
          "Social","Greenery","Greenery","Medical",
          "Medical","Walkability","None","None")

metadata <- data.frame(IndName,IndCode,Direction,IndUnit,IndWeight,Agg1)

colnames(data)==metadata$IndCode

# Aggregation table
AgLevel <- rep(1,7)
Code <- c("None","Walkability","Transportation","Bikeability","Social","Greenery","Medical")
Name <- c("None","Walkability","Transportation","Bikeability","Social","Greenery","Medical")
Weight <- rep(1,7)

aggmeta <- data.frame(AgLevel,Code,Name,Weight)

# Assemble COIN
KULI <- assemble(IndData = data, IndMeta = metadata, AggMeta = aggmeta)

options(error = function() {traceback(2, max.lines=100); if(!interactive()) quit(save="no", status=1, runLast=T)})



