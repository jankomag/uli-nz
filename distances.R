library(sf)
library(sp)
library(tmap)
library(dplyr)
library(geosphere)
library(raster)

#### Data Imports ####
grid <- st_read("data/geographic/grids/sample_grid_all.gpkg")
grid <- st_transform(grid, 4326)
p1 <- grid[1,]
p1x <- st_coordinates(p1)[,1]
p1y <- st_coordinates(p1)[,2]
#27291

stations <- st_read("data/transport/public_transport/sample_stations.gpkg")
stations <- st_transform(stations, 4326)
p2 <- stations[1,]
p2x <- st_coordinates(p2)[,1]
p2y <- st_coordinates(p2)[,2]

distm(c(p1x, p1y), c(p2x, p2y), fun = distHaversine)

#### Distance analysis ####
i=0
for (point in 1:nrow(grid)){
  i
}


