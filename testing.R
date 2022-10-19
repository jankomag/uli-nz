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
# geographic data
sa1 <- st_read("data/geographic/wellington_urban_sa1s.gpkg")
#sa1_cent <- st_read("data/geographic/wellington_urban_polygons_sa1s.gpkg")
stations <- st_read("data/transport/public_transport/Wellington_Public_Transport.geojson")
# census variables
households <- read.csv("data/geographic/SA1 CensusWellingtonRegion/households_in_sa1s.csv")
dampness <- read.csv("data/geographic/SA1 CensusWellingtonRegion/dampness.csv")
population <- read.csv("data/geographic/SA1 CensusWellingtonRegion/population_welly.csv")


#transforming to the same coordinate system
stations <- st_transform(stations, 27291)
sa1 = st_transform(sa1, 27291)

tmap_mode("view")
tm_shape(sa1) +
  tm_borders(col="black") +
  tm_shape(stations) +
  tm_dots(col="red")

#### Adding Census vars ####
sa1_nons <- sa1 %>% st_drop_geometry()
head(sa1_nons)

dampness <- dampness %>%
  mutate(code = as.character(code)) %>%
  mutate(dampness_rate = as.numeric(as.factor(Total_damp))/as.numeric(as.factor(Total.stated)))

households <- households %>%  
  mutate(code = as.character(code)) %>%
  mutate(no_households = as.numeric(no_households))

population <- population %>%  
  mutate(code = as.character(code)) %>%
  mutate(population_usual = as.numeric(as.factor(population_usual))) %>%
  mutate(maori_descent = as.numeric(as.factor(maori_descent))) %>%
  mutate(median_income = as.numeric(as.factor(median_income))) %>%
  mutate(maori_pr = maori_descent/population_usual)

sa1_h <- left_join(sa1, households, by = c("SA12018_V1"="code"))
sa1_h_d <- left_join(sa1_h, dampness, by = c("SA12018_V1"="code"))
sa1_all <- left_join(sa1_h_d, population, by = c("SA12018_V1"="code"))


head(sa1_all)

tm_shape(sa1_all) +
  tm_fill(col="maori_pr",
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
