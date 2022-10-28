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
library(VIM)
 
#### Data imports ####
# geographic data
sa1 <- st_read("data/geographic/auckland_urban_sa1s.gpkg")
#sa1_cent <- st_read("data/geographic/wellington_urban_polygons_sa1s.gpkg")
stations <- st_read("data/transport/public_transport/trains_auckland.gpkg")
stops <- st_read("data/transport/public_transport/bus_stops_auckland.geojson")
# census variables
census <- read.csv("data/geographic/SA1 CensusAucklandRegion/auckland_census.csv")
households <- read.csv("data/geographic/SA1 CensusAucklandRegion/households_in_sa1s.csv")
dampness <- read.csv("data/geographic/SA1 CensusAucklandRegion/dampness.csv")
population <- read.csv("data/geographic/SA1 CensusAucklandRegion/population_welly.csv")

#transforming to the same coordinate system
stations <- st_transform(stations, 27291)
stops <- st_transform(stops, 27291)
sa1 <- st_transform(sa1, 27291)

tmap_mode("view")
tm_shape(grid) +
  tm_dots(col="black")# +
  tm_shape(stations) +
  tm_dots(col="red")

#### Adding Census vars ####
census <- census %>%
  mutate(code = as.character(code)) %>%
  mutate(no_households = as.numeric(no_households)) %>%
  mutate(median_income = as.numeric(median_income)) %>%
  mutate(maori_desc = as.numeric(maori_desc)) %>%
  mutate(pop_usual = as.numeric(pop_usual)) %>%
  mutate(maori_pr = maori_desc/pop_usual) %>%
  mutate(dampness = as.numeric(as.factor(dampness)))

head(census)
sa1_all <- left_join(sa1, census, by = c("SA12018_V1"="code"))

#sa1_all = subset(sa1_all, select = -c(maori_desc))
head(sa1_all)

tm_shape(sa1_all) +
  tm_fill(col="maori_pr",
          style = "kmeans", palette = "Reds")

##### Impute missing values #####
summary(aggr(census))
sa1_all$maori_pr[is.na(sa1_all$maori_pr)] <- 0

#imputation by neighbouring values - not working yet
#sa1_all <- mutate(sa1_all, dampness = as.numeric(as.character(dampness)))
#sa1 <- SpatialPoints(sa1)
#knn5 <- knn2nb(knearneigh(sa1, k = 5))
#sapply(1:length(knn5), function(N){mean(dampness[N])})
#sa1s_na_damp <- sa1_all[which(is.na(sa1_all$dampness)),]
#sa1s_na_damp$dampness

#### Spatial Interpolation ####
grid <- st_sample(sa1, 50000, type = "regular")
grid <- st_transform(grid, 27291)
grid <- st_as_sf(grid)
#st_write(grid, "grid_10000.gpkg")

tmap_mode("view")
tm_shape(grid) +
  tm_dots(col="grey") +
  tm_shape(stations) +
  tm_dots("black")

# IDW for census variables
idw_income <- idw(formula = median_income~1, locations = sa1_all, 
               newdata = grid, idp = 1, nmax=10, na.action=na.pass)
idw_hs <- idw(formula = no_households~1, locations = sa1_all, 
                  newdata = grid, idp = 1, nmax=10, na.action=na.pass)
idw_maori <- idw(formula = maori_pr~1, locations = sa1_all, 
                newdata = grid, idp = 1, nmax=10, na.action=na.pass)
idw_damp <- idw(formula = dampness~1, locations = sa1_all, 
                 newdata = grid, idp = 1, nmax=10, na.action=na.pass)

tm_shape(idw_income) +
  tm_dots(col="var1.pred", style="kmeans") +
  tm_basemap("OpenStreetMap")

idw_joined <- cbind(idw_income, idw_hs, idw_maori, idw_damp)
idw_joined = subset(idw_joined, select = -c(var1.var, var1.var.1, var1.var.2,
                                            var1.var.3, geometry.1, geometry.2,geometry.3))

colnames(idw_joined) <- c("income", "no_households", "maori_pr", "dampness", "geometry")
st_write(idw_joined,"data/grid_auckaldn_census.gpkg")

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
