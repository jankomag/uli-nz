# load the packages
library(sf)
library(sp)
library(tmap)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rgeos)
library(geosphere)

#### Data imports ####
sa1 <- st_read("data/geographic/auckland_urban_centroid_sa1.gpkg")
households <- read.csv("data/geographic/SA1 Census 2018-WellingtonRegion_updated_4-11-21/households_in_sa1s.csv")
dampness <- read.csv("data/geographic/SA1 Census 2018-WellingtonRegion_updated_4-11-21/dampness.csv")
stations <- st_read("data/transport/public_transport/trains_auckland.gpkg")

#transforming to the same coordinate system
stations <- st_transform(stations, 27291)
sa1 = st_transform(sa1, 27291)

tmap_mode("plot")
tm_shape(sa1) +
  tm_dots(col="black") +
  tm_shape(stations) +
  tm_dots(col="red")

#### Adding Census vars ####
sa1_nons <- sa1 %>% st_drop_geometry()
head(sa1)

dampness <- dampness %>%
  mutate(code = as.character(code)) %>%
  mutate(dampness = as.numeric(dampness))
household <- household %>%  
  mutate(code = as.character(code))

sa1_h <- left_join(sa1, household, by = c("SA12018_V1"="code"))
sa1_h_d <- left_join(sa1_h, dampness, by = c("SA12018_V1"="code"))

head(sa1_h_d)

tm_shape(sa1_h_d) +
  tm_fill(col="dampness",
          style = "kmeans", palette = "Reds") +
  tm_borders("transparent")
#tm_shape(pt) +
#tm_dots(col="Mode")
#### Station distances ####
sa1$nearest_station <- st_nearest_feature(sa1, stations)
sa1$dist_nearest_station <- st_distance(sa1, stations, by_element = TRUE)
summary(sa1)

tm_shape(sa1) +
  tm_dots(col="dist_nearest_station", style="kmeans") +
  tm_shape(stations) +
  tm_dots(col="black")


# bind results with original points
pts.wit.dist <- cbind(pts, dist.mat)
pts.wit.dist[1:3,]



##### Buffers ####
# union the stations to a single point file
pt_merge <- st_sf(st_union(pt))

# buffer this
buf_pt <- st_buffer(pt_merge, 800)
# and map
tmap_mode("plot")
tm_shape(buf_pt) + 
  tm_borders() +
  tm_shape(pt_merge) +
  tm_dots()
