# load the packages
library(sf)
library(sp)
library(tmap)
library(COINr6)
library(tidyverse)
library(tibble)
library(dplyr)

##### Data Preparation #####
df <- st_read("data/geographic/sa1_kuli_all.gpkg")
df <- df |> 
  st_drop_geometry() |> 
  mutate(x_SA12018_V1_00 = as.numeric(SA12018_V1_00))
idx <- data.frame(1:nrow(df))  |> 
  mutate(UnitName = as.character(1:nrow(df))) |> 
  mutate(UnitCode = UnitName)

df2 <- idx |> 
  cbind(df) |>
  subset(select = c(UnitCode, UnitName, x_SA12018_V1_00, househdens, dampness.x,
                    medianRent.x, shannon.x, crime_perarea.x,dist_crash.x, floodprone_prc.x,
                    alcoprohibited.x, str_connectivity.x, dist_stations.x, dist_busstops.x, dist_marae.x,
                    dist_cinema.x, dist_galleries.x, dist_libraries.x, dist_museums.x,
                    dist_theatre.x, dist_chemist.x, dist_dentist.x, dist_healthcentre.x, dist_hospital.x,
                    dist_childcare.x, dist_sport.x, dist_conveniencestore.x, dist_supermarket.x,
                    dist_secondary.x, dist_primary.x, dist_petrol.x, dist_bigpark.x, dist_smallpark.x,
                    dist_evs.x, dist_busstopsfreq.x, dist_cafe.x, dist_restaurants.x,
                    dist_pubs.x, dist_bbq.x, dist_gym.x, dist_beach.x, bikeperarea.x))

summary(df2)
# Indicators Data
data <- df2

# Indicators Metadata
IndName <- c("Housing Density","Dampness",
                     "Affordability", "Diversity", "Crime","Road Safety", "Floods",
                     "Alcohol Env", "Street Connectivity", "dist_stations.x", "dist_busstops.x", "dist_marae.x",
                     "dist_cinema.x", "dist_galleries.x", "dist_libraries.x", "dist_museums.x",
                     "dist_theatre.x", "dist_chemist.x", "dist_dentist.x", "dist_healthcentre.x", "dist_hospital.x",
                     "dist_childcare.x", "dist_sport.x", "dist_conveniencestore.x", "dist_supermarket.x",
                     "dist_secondary.x", "dist_primary.x", "dist_petrol.x", "dist_bigpark.x","dist_smallpark.x",
                     "dist_evs.x", "dist_busstopsfreq.x", "dist_cafe.x", "dist_restaurants.x",
                     "dist_pubs.x", "dist_bbq.x", "dist_gym.x", "dist_beach.x", "bikeperarea.x")
IndCode <- c("househdens","dampness.x",
             "medianRent.x", "shannon.x", "crime_perarea.x","dist_crash.x", "floodprone_prc.x",
             "alcoprohibited.x", "str_connectivity.x", "dist_stations.x", "dist_busstops.x", "dist_marae.x",
             "dist_cinema.x", "dist_galleries.x", "dist_libraries.x", "dist_museums.x",
             "dist_theatre.x", "dist_chemist.x", "dist_dentist.x", "dist_healthcentre.x", "dist_hospital.x",
             "dist_childcare.x", "dist_sport.x", "dist_conveniencestore.x", "dist_supermarket.x",
             "dist_secondary.x", "dist_primary.x", "dist_petrol.x", "dist_bigpark.x","dist_smallpark.x",
             "dist_evs.x", "dist_busstopsfreq.x", "dist_cafe.x", "dist_restaurants.x",
             "dist_pubs.x", "dist_bbq.x", "dist_gym.x", "dist_beach.x", "bikeperarea.x")
Direction <- c(1,-1,
               -1,1,-1,1,-1,
               1,1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1,-1,-1,-1,
               -1,-1,-1,-1,
               -1,-1,-1,-1,1)
IndUnit <- c("","",
             "NZD", "", "no","m", "",
             "yes", "", "m", "m", "m",
             "m", "m", "m", "m",
             "m", "m", "m", "m", "m",
             "m", "m", "m", "m",
             "m", "m", "m", "m","m",
             "m", "m", "m", "m",
             "m", "m", "m", "m", "")
IndWeight <- rep(1,39)
Agg1 <- c("Walkability","Housing",
          "Housing", "Social", "Safety","Safety", "Safety",
          "Safety", "Walkability", "Transport", "Transport", "Social",
          "Social", "Social", "Social", "Social",
          "Social", "Social", "Social", "Social", "Social",
          "Social", "Social", "Walkability", "Walkability",
          "Social", "Social", "Transport", "Green","Green",
          "Transport", "Transport", "Walkability", "Social",
          "Walkability", "Social", "Social", "Green", "Bikeability")
  
Agg2 <- rep("Index", 39)

metadata <- data.frame(IndName,IndCode,Direction,IndUnit,IndWeight,Agg1, Agg2)

# Aggregation table
AgLevel <- c(rep(2,7),3)
Code <- c("Walkability","Transport","Safety", "Bikeability","Social","Green","Housing","Index")
Name <- c("Walkability","Transport","Safety", "Bikeability","Social","Green","Housing","Index")
Weight <- rep(1,8)

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
  head(39) |> print(n=39)

# correlation plot
plotCorr(KULI, dset = "Raw", aglevs = 1, showvals = F)
plotCorr(KULI, dset = "Raw", aglevs = 1, showvals = F, grouplev = 3, box_level = 2)

# data reliability
getCronbach(KULI, dset = "Raw")
getCronbach(KULI, dset = "Raw", icodes = "Walkability", aglev = 1)

# PCA

##### Data Transformation ####
# Treatment - Winsorisation
df <- data.frame(GDP = df$museum_dist,
                 LogGDP = log(df$museum_dist))
plotIndDist(df, type = "Histogram")

KULI_treated <- treat(KULI, dset = "Raw", winmax = 20000)
KULI_treated$Analysis$Treated$TreatSummary |>
  filter(Treatment != "None")
iplotIndDist2(KULI_treated, dsets = c("Raw", "Treated"), icodes = "bigpark_dist", ptype = "Histogram") # Scatter

# Normalisation
# define nonlinear normalisation functions

indiv = list(
  bigpark_dist = list(ntype = "custom", npara = bigparknorm),
  smallpark_dist = list(ntype = "custom", npara = smallparknorm),
  station_dist = list(ntype = "custom", npara = stationnorm),
  bus_dist = list(ntype = "custom", npara = busnorm))

# Minmax in [0,10] for all indicators, except custom individual normalisation
KULI <- normalise(KULI, dset = "Raw", ntype = "minmax", npara = list(minmax = c(1,10)),indiv_only = FALSE)#individual = indiv,

# Aggregation
KULI <- aggregate(KULI, dset = "Normalised", agtype = "geom_mean")

#plotting on map
result <- getResults(KULI, tab_type = "Full")
result <- result |> 
  mutate(x_SA12018_V1_00 = as.character(x_SA12018_V1_00))
sa1_allg <- st_read("data/geographic/sa1_allvars.gpkg") |> st_transform(27291)
index_sa1g <- left_join(sa1_allg, result, by = c("SA12018_V1_00"="x_SA12018_V1_00"))

tmap_mode("plot")
tm_shape(index_sa1g) +
  tm_polygons(col = c("Index","Walkability","Transport","Safety","Bikeability","Social","Green","Housing"), palette = "Reds", style = "kmeans", lwd=0)

st_write(index_sa1g, "data/geographic/sa1_kuli_COIN6R.gpkg")


