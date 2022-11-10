# load the packages
library(sf)
library(sp)
library(tmap)
library(COINr6)
library(tidyverse)
library(tibble)
library(dplyr)

##### Data Preparation #####
grid <- st_read("data/geographic/grids/grid_all-v-07.11.22_20.02.gpkg")
idx <- data.frame(1:nrow(grid))  |> 
  mutate(UnitName = as.character(1:nrow(grid))) |> 
  mutate(UnitCode = UnitName)

grid_df <- idx |> 
  cbind(grid) |> 
  mutate(x_lon = lon) |>
  mutate(x_lat = lat) |>
  mutate(x_income = income) |>
  mutate(x_maori_pr = maori_pr) |>
  subset(select = c(-lat, -lon, -X1.nrow.grid., -geom, -income, -maori_pr))


summary(grid_df)
# Indicators Data
data <- grid_df

# Indicators Metadata
IndName<-c("Number of Households","Percentage Dampness", "no_intersections_in_100m",
           "station_dist", "bus_dist", "ev_dist", "conv_st_dist", "petrol_st_dist", 
           "biking_len_100m", "second_dist", "primary_dist","childcare_dist", 
           "cinemas_dist", "galleries_dist", "libraries_dist", "museum_dist",
           "theatre_dist","bigpark_dist","smallpark_dist","chemists_dist",
           "dentist_dist","supermarket_dist")
IndCode <- c("no_households","dampness","no_intersections_in_100m",
             "station_dist", "bus_dist","ev_dist","conv_st_dist", "petrol_st_dist", 
             "biking_len_100m","second_dist","primary_dist","childcare_dist", 
             "cinemas_dist", "galleries_dist", "libraries_dist", "museum_dist",
             "theatre_dist","bigpark_dist","smallpark_dist","chemists_dist",
             "dentist_dist","supermarket_dist")
Direction <- c(1,-1,1,
               -1,-1,-1,-1,-1,
               1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1)
IndUnit <- c("","percentage","number within 100 m buffer",
             "m", "m","ev","m", "m", 
             "metres within 100 m buffer","m","m","m", 
             "m", "m", "m", "m",
             "m","m","m","m",
             "m","m")
IndWeight <- rep(1,22)
Agg1 <- c("Walkability","Social","Walkability",
          "Transportation", "Transportation","Transportation","Walkability", "Walkability", 
          "Bikeability","Social","Social","Social", 
          "Social", "Social", "Social", "Social",
          "Social","Greenery","Greenery","Medical",
          "Medical","Walkability")
Agg2 <- rep("Index", 22)

metadata <- data.frame(IndName,IndCode,Direction,IndUnit,IndWeight,Agg1, Agg2)

# Aggregation table
AgLevel <- c(rep(2,6),3)
Code <- c("Walkability","Transportation","Bikeability","Social","Greenery","Medical","Index")
Name <- c("Walkability","Transportation","Bikeability","Social","Greenery","Medical","Index")
Weight <- rep(1,7)

aggmeta <- data.frame(AgLevel,Code,Name,Weight)

# Assemble COIN
KULI <- assemble(IndData = data, IndMeta = metadata, AggMeta = aggmeta)
# error handling
#options(error = function() {traceback(3, max.lines=100); if(!interactive()) quit(save="no", status=1, runLast=T)})

##### Analysis ####
#plot components
plotframework(KULI)

# Violin Plots
plotIndDist(KULI, type = "Histogram", icodes = "Social")
plotIndDist(KULI, type = "Violindot", icodes = "Social")

#Summary Statistics
KULI <- getStats(KULI, dset = "Raw", out2 = "COIN")
KULI$Analysis$Raw$StatTable |> 
  roundDF() |> 
  reactable::reactable(resizable = TRUE, bordered = TRUE,
                       highlight = TRUE, defaultPageSize = 10)

# get statistics of raw data set
statlist <- getStats(KULI, dset = "Raw", out2 = "list")

# Correlations
statlist$StatTable[ c("Indicator", "Collinearity", "Neg.Correls")] |> 
  head(24) |> print(n=21)

# correlation plot
plotCorr(KULI, dset = "Raw", aglevs = 1, showvals = F)
plotCorr(KULI, dset = "Raw", aglevs = 1, showvals = F, grouplev = 3, box_level = 2)

# data reliability
getCronbach(KULI, dset = "Raw")
getCronbach(KULI, dset = "Raw", icodes = "Walkability", aglev = 1)

# PCA

##### Data Transformation ####
# Treatment - Winsorisation
df <- data.frame(GDP = grid_df$museum_dist,
                 LogGDP = log(grid_df$museum_dist))
plotIndDist(df, type = "Histogram")

KULI_treated <- treat(KULI, dset = "Raw", winmax = 20000)
KULI_treated$Analysis$Treated$TreatSummary |>
  filter(Treatment != "None")
iplotIndDist2(KULI_treated, dsets = c("Raw", "Treated"), icodes = "bigpark_dist", ptype = "Histogram") # Scatter

# Normalisation
# define nonlinear normalisation functions
busnorm <- function(x){
  x = ifelse(x<=400,
             x, #normal linear
             min(x*1.1,5000)) # penalty for not meeting the target capped at a limit
  minmaxed = ((x - min(x)) / (max(x) - min(x)))*(0-10)+10 #then scaling 0-10
  return (minmaxed)
}
stationnorm <- function(x){
  x = ifelse(x<=800,
             x, #normal linear
             min(x*1.1,10000)) # penalty for not meeting the target capped at a limit
  minmaxed = ((x - min(x)) / (max(x) - min(x)))*(0-10)+10 #then scaling 0-10
  return (minmaxed)
}
bigparknorm <- function(x){
  x = ifelse(x<=1000,
             x, #normal linear
             min(x*1.1,10000)) # penalty for not meeting the target capped at a limit
  minmaxed = ((x - min(x)) / (max(x) - min(x)))*(0-10)+10 #then scaling 0-10
  return (minmaxed)
}
smallparknorm <- function(x){
  x = ifelse(x<=600,
             x, #normal linear
             min(x*1.1,5000)) # penalty for not meeting the target capped at a limit
  minmaxed = ((x - min(x)) / (max(x) - min(x)))*(0-10)+10 #then scaling 0-10
  return (minmaxed)
}

indiv = list(
  bigpark_dist = list(ntype = "custom", npara = bigparknorm),
  smallpark_dist = list(ntype = "custom", npara = smallparknorm),
  station_dist = list(ntype = "custom", npara = stationnorm),
  bus_dist = list(ntype = "custom", npara = busnorm))

# Minmax in [0,10] for all indicators, except custom individual normalisation
KULI <- normalise(KULI, dset = "Raw", ntype = "minmax", npara = list(minmax = c(0,10)),
                  individual = indiv, indiv_only = FALSE)

# Aggregation
KULI <- aggregate(KULI_treated, dset = "Normalised", agtype = "geom_mean")


#plotting on map
result <- getResults(KULI, tab_type = "Full")

sdf <- SpatialPointsDataFrame(result[,c("x_lon", "x_lat")], result, proj4string=CRS("+init=epsg:3857"))
sdf <- st_as_sf(sdf)

tm_shape(sdf) +
  tm_dots(col="Index")

