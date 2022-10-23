# load the packages
library(sf)
library(sp)
library(tmap)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rgeos)
library(geosphere)
library(gstat)
library(spdep)

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
sa1 <- st_transform(sa1, 27291)

tmap_mode("view")
tm_shape(sa1) +
  tm_borders(col="black") +
  tm_shape(stations) +
  tm_dots(col="red")

#### Adding Census vars ####
dampness[is.na(dampness)] <- 0
dampness <- dampness %>%
  mutate(code = as.character(code)) %>%
  mutate(dampness = as.numeric(as.character(dampness)))

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

sa1_all = subset(sa1_all, select = -c(X,Total_damp,Not_damp, Total.stated, maori_descent))

head(sa1_all)

tm_shape(sa1_all) +
  tm_fill(col="dampness",
          style = "kmeans", palette = "Reds") +
  tm_borders("transparent")
#tm_shape(pt) +
#tm_dots(col="Mode")

##### Impute missing values #####
sa1_all <- mutate(sa1_all, dampness = as.numeric(as.character(dampness)))
sa1 <- SpatialPoints(sa1)
knn5 <- knn2nb(knearneigh(sa1, k = 5))

sapply(1:length(knn5), function(N){mean(dampness[N])})

sa1s_na_damp <- sa1_all[which(is.na(sa1_all$dampness)),]
sa1s_na_damp$dampness
#### Spatial Interpolation ####
grid <- st_sample(sa1_all, 5000, type = "regular")
grid <- st_transform(grid, 27291)
grid <- st_as_sf(grid)

tmap_mode("view")
tm_shape(grid) +
  tm_dots(col="grey") +
  tm_shape(stations) +
  tm_dots("black")

# IDW for census variables
idw_income <- idw(formula = median_income~1, locations = sa1_all, 
               newdata = grid, idp = 1, nmax=10)
idw_hs <- idw(formula = no_households~1, locations = sa1_all, 
                  newdata = grid, idp = 1, nmax=10)
idw_damp <- idw(formula = dampness~1, locations = sa1_all, 
                      newdata = grid, idp = 1, nmax=10)
idw_maori <- idw(formula = maori_pr~1, locations = sa1_all, 
                newdata = grid, idp = 1, nmax=10)

tm_shape(idw_income) +
  tm_dots(col="var1.pred", style="kmeans") +
  tm_basemap("OpenStreetMap")

idw_joined <- cbind(idw_income, idw_hs, idw_damp, idw_maori)
colnames(idw_joined) <- c("income", "no_households", "dmapness", "maori_pr")
idw_joined
#### Distances to transport ####
train_stations <- stations %>% filter(Mode == "Railway Station")
bus_stops <- subset(stations, Mode == "Bus Stop")
grid <- st_as_sf(grid)
bus_stops <- st_as_sf(bus_stops)

sa1_all$nearest_station <- st_nearest_feature(sa1, stations)
g1 = st_geometry(grid)
g2 = st_geometry(stations) 

dlist = mapply(st_distance, g1, g2)

grid$dist_nearest_station <- st_distance(grid, stations, by_element = TRUE)

tm_shape(grid) +
  tm_dots() +
  tm_shape(bus_stops) +
  tm_dots(col="grey")

# bind results with original points
pts.wit.dist <- cbind(pts, dist.mat)
pts.wit.dist[1:3,]


#### Buffers ####
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
