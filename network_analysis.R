library(sf)
library(sp)
library(tmap)
library(igraph)
library(sfnetworks)
library(dplyr)
library(tidygraph)
library(here)
library(magrittr)
library(osmdata)
library(dodgr)
library(expss)
library(stringr)
library(DescTools)
library(osmdata)

#### Data Imports ####
edges <- st_read("data/networks/auckland_waiheke_network_walk.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area

sa1 <- st_read("data/sa1_kuli_all.gpkg") |> 
  st_transform(4326) |> 
  subset(select = c(SA12018_V1_00))

#get network
network <- as_sfnetwork(edges, directed = FALSE) |> 
  st_transform(4326) |> 
  activate("edges")

get_distance <- function(supply) {
  supplyname <- deparse(substitute(supply))
  supplydist <- str_glue("dist_{supplyname}")
  distances <- data.frame(st_network_cost(network, from = sa1, to = supply, weights = "length"))
  distances[toString(supplydist)] <- do.call(pmin, distances)
  last_data <- distances[ , ncol(distances), drop = FALSE]
  cbind(sa1, last_data)
}

#### Loading point data ####
bbox_coords <- c(174.489515, -37.159816, 175.258558, -36.570920)
bbox <- matrix(c(bbox_coords[1], bbox_coords[2], bbox_coords[3], bbox_coords[4]), ncol = 2, byrow = TRUE)
get_osm_data <- function(osm_key, osm_value) {
  # Query OSM data
  osm_data <- opq(bbox = bbox, timeout = 100) %>%
    add_osm_feature(key = osm_key, value = osm_value) %>%
    osmdata_sf()
  points <- osm_data$osm_points[, c("osm_id")]
  st_crs(points) <- st_crs(4326)
  colnames(points) <- c('id', 'geometry')
  
  # Check for polygons and add centroids 
  if (!is.null(osm_data$osm_polygons)) {
    centroids <- st_centroid(osm_data$osm_polygons$geometry)
    centroids_sf <- st_sf(
      name = paste0((osm_value), "Centroids"),
      geometry = centroids
    )
    colnames(centroids_sf) <- c('id', 'geometry')
    result <- rbind(points, centroids_sf)
  } else {
    result <- points
  }
  return(result)
}

# Get point data
cafe <- get_osm_data("amenity", "cafe")
restaurant <- get_osm_data("amenity", "restaurant")
pub <- get_osm_data("amenity", "pub")
bbq <- get_osm_data("amenity", "bbq")
cinema <- get_osm_data("amenity", "cinema")
theatre <- get_osm_data("amenity", "theatre")
gallery <- get_osm_data("tourism", "gallery")
museum <- get_osm_data("tourism", "museum")
library <- get_osm_data("amenity", "library")
chemist <- get_osm_data("shop", "chemist")
dentist <- get_osm_data("amenity", "dentist")
hospital <- get_osm_data("amenity", "hospital")
healthcentre <- get_osm_data("amenity", "clinic")
childcare <- get_osm_data("amenity", "kindergarten")
conveniencestore <- get_osm_data("shop", "convenience")
supermarket <- get_osm_data("shop", "supermarket")
gym <- get_osm_data("leisure", "fitness_centre")
petrol <- get_osm_data("amenity", "fuel")
beach <- get_osm_data("natural", "beach")
# Load remaining data
stations <- st_read("data/trains_auckland.gpkg") |> st_transform(4326)
busstopsfreq <- st_read("data/frequentBusStoptsDone.gpkg") |> st_transform(4326)
marae <- st_read("data/auckland_marae_final.gpkg") |> st_transform(4326)
primary <- st_read("data/primary_schools.gpkg") |> st_transform(4326)
secondary <- st_read("data/secondary_schools.gpkg") |> st_transform(4326)
sport <- st_read("data/sport_facilities.gpkg") |> st_transform(4326)
bigpark <- st_read("data/final_interp_sinplif.gpkg") |> st_transform(4326)
  
#### Walking distance calculations ####
sa1 <- get_distance(cafe)
sa1 <- get_distance(restaurant)
sa1 <- get_distance(pub)
sa1 <- get_distance(bbq)
sa1 <- get_distance(cinema)
sa1 <- get_distance(theatre)
sa1 <- get_distance(gallery)
sa1 <- get_distance(museum)
sa1 <- get_distance(library)
sa1 <- get_distance(chemist)
sa1 <- get_distance(dentist)
sa1 <- get_distance(hospital)
sa1 <- get_distance(healthcentre)
sa1 <- get_distance(childcare)
sa1 <- get_distance(conveniencestore)
sa1 <- get_distance(supermarket)
sa1 <- get_distance(gym)
sa1 <- get_distance(beach)
sa1 <- get_distance(bigpark)
sa1 <- get_distance(stations)
sa1 <- get_distance(busstopsfreq)
sa1 <- get_distance(marae)
sa1 <- get_distance(primary)
sa1 <- get_distance(secondary)
sa1 <- get_distance(sport)

tm_shape(sa1) +
  tm_borders(lwd = 0) +
  tm_fill("dist_cinema", title = "Distance") +
  tm_layout(legend.position = c("left", "bottom"))

#### Driving distance calculations ####
edges <- st_read("data/networks/auckland_waiheke_network_drive.gpkg", layer='edges') |> 
  st_transform(27291) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area

head(edges)
#get network
network <- as_sfnetwork(edges, directed = FALSE) |> 
  st_transform(4326) |> 
  activate("edges")


emergency <- st_read("data/emergencies_auck.gpkg") |> st_transform(4326)
ev_charge <- st_read("data/EV_NZ_charging_stations.geojson") |> st_transform(4326)
#crash <- st_read("uli-nz/data/Crash_Analysis_System_(CAS)_data.geojson") |> st_transform(4326)

sa1 <- get_distance(emergency)
sa1 <- get_distance(ev_charge)
#sa1 <- get_distance(crash)
sa1 <- get_distance(petrol)

#### Road Crash Risk ####
# get polygon data
sa1_polys <- st_read("data/sa1_auckland_waiheke_urban.gpkg") |>
  subset(select = c(SA12018_V1_00)) |> 
  st_transform(27291) #transforming to the same coordinate system

# get population data
census <- read.csv('data/auckland_census_3.csv') |>
  subset(select = c(censusnightpop, code)) |> 
  mutate(code = as.character(code))
sa1_polys <- left_join(sa1_polys, census, by=c("SA12018_V1_00"="code"))

# Intersect roads with SA1 areas
sa1_roads <- st_intersection(sa1_polys, edges)
# calculate areas
sa1_roads <- sa1_roads %>%
  group_by(SA12018_V1_00) %>% # group by area
  summarize(total_road_length = sum(length)) |> # calculate road length 
  st_drop_geometry()

sa1_polys <- sa1_polys  |> 
  left_join(sa1_roads, by = "SA12018_V1_00")
sa1_polys[which(is.na(sa1_polys$total_raod_length),), "total_raod_length"] <- 1

# get crashes data
crash <- st_read("data/Crash_Analysis_System_(CAS)_data.geojson") |> 
  st_transform(27291) #transforming to the same coordinate system

sa1_crash <- st_join(sa1_polys, crash)
sa1_crash <- sa1_crash %>%
  group_by(SA12018_V1_00) %>%
  mutate(crash_count = n()) %>%
  ungroup() %>%
  select(SA12018_V1_00, crash_count, geom) |> 
  st_drop_geometry()

sa1_crash <- left_join(sa1_polys, sa1_crash)
sa1_crash$area <- as.numeric(st_area(sa1_crash))

sa1_crash <- sa1_crash |> 
  mutate(road_length_per_area = total_road_length.x/area) |> 
  mutate(crashes_per_roadlen = crash_count/total_road_length.x) |> 
  mutate(crasehs_per_roadlen_per_area = crash_count/road_length_per_area) |> 
  mutate(crash_risk = crashes_per_roadlen/(censusnightpop+0.1))
tm_shape(sa1_crash)+tm_fill("crash_risk", style="jenks")

min_max_scale <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  scaled <- 1 + 9 * ((x - min_val) / (max_val - min_val))
  return(scaled)
}
sa1_crash$crash_riskscaled <- min_max_scale(sa1_crash$crasehs_per_roadlen_per_population)
tm_shape(sa1_crash)+tm_fill("crash_riskscaled", style="jenks")

# finally join to the rest of the data
sa1_crash <- sa1_crash |> 
  select(SA12018_V1_00, crash_risk) |> 
  st_drop_geometry()
sa1 <- left_join(sa1, sa1_crash, by="SA12018_V1_00")

st_write(sa1, "data/sa1_crashrisk.gpkg")
