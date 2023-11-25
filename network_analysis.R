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
stations <- st_read("uli-nz/data/trains_auckland.gpkg") |> st_transform(4326)
busstopsfreq <- st_read("uli-nz/data/frequentBusStoptsDone.gpkg") |> st_transform(4326)
marae <- st_read("uli-nz/data/auckland_marae_final.gpkg") |> st_transform(4326)
primary <- st_read("uli-nz/data/primary_schools.gpkg") |> st_transform(4326)
secondary <- st_read("uli-nz/data/secondary_schools.gpkg") |> st_transform(4326)
sport <- st_read("uli-nz/data/sport_facilities.gpkg") |> st_transform(4326)
bigpark <- st_read("uli-nz/data/final_interp_sinplif.gpkg") |> st_transform(4326)
  
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
edges <- st_read("uli-nz/data/networks/auckland_waiheke_network_drive.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area
#get network
network <- as_sfnetwork(edges, directed = FALSE) |> 
  st_transform(4326) |> 
  activate("edges")

emergency <- st_read("uli-nz/data/emergencies_auck.gpkg") |> st_transform(4326)
ev_charge <- st_read("uli-nz/data/EV_NZ_charging_stations.geojson") |> st_transform(4326)
#crash <- st_read("uli-nz/data/Crash_Analysis_System_(CAS)_data.geojson") |> st_transform(4326)

sa1 <- get_distance(emergency)
sa1 <- get_distance(ev_charge)
#sa1 <- get_distance(crash)
sa1 <- get_distance(petrol)

st_write(sa1, "uli-nz/data/sa1_out_dist.gpkg")