# load the packages
library(sf)
library(sp)
library(tmap)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(geosphere)
library(spdep)
 
#### Data imports ####
# geographic data
grid <- st_read("data/geographic/grids/grid_all-v-04.11.22_19.52.gpkg")
#transforming to the same coordinate system
grid <- st_transform(grid, 27291)

# test plotting
tm_shape(grid) +
  tm_dots(col="cinemas_dist",
          style = "kmeans", palette = "Reds")

#### Index Construcion ####
