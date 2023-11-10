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
edges <- st_read("data/network_analysis/auckland_waiheke_network_walk.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area
sa1 <- st_read("data/geographic/sa1_kuli_all.gpkg") |> 
  st_transform(4326) |> 
  subset(select = c(SA12018_V1_00)) #area

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

#### Querying OSM ####
bbox_coords <- c(174.489515, -37.159816, 175.258558, -36.570920)
bbox <- matrix(c(bbox_coords[1], bbox_coords[2], bbox_coords[3], bbox_coords[4]), ncol = 2, byrow = TRUE)
get_osm_data <- function(osm_key, osm_value) {
  # Query OSM data
  osm_data <- opq(bbox = bbox, timeout = 100) %>%
    add_osm_feature(key = osm_key, value = osm_value) %>%
    osmdata_sf()
  points <- osm_data$osm_points[, c("osm_id")]
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

cafe <- get_osm_data("amenity", "cafe")
restaurant <- get_osm_data("amenity", "restaurant")
cinema <- get_osm_data("amenity", "cinema")
theatre <- get_osm_data("amenity", "theatre")
gallerie <- get_osm_data("tourism", "gallery")
museum <- get_osm_data("tourism", "museum")
library <- get_osm_data("amenity", "library")
chemist <- get_osm_data("shop", "chemist")
dentist <- get_osm_data("amenity", "dentist")
conveniencestore <- get_osm_data("shop", "convenience")
supermarket <- get_osm_data("shop", "supermarket")
pub <- get_osm_data("amenity", "pub")
gym <- get_osm_data("leisure", "fitness_centre")
petrol <- get_osm_data("amenity", "fuel")

#### Walking distance calculations ####
stations <- st_read("data/transport/public_transport/trains_auckland.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(stations)

busstopsfreq <- st_read("data/transport/public_transport/frequentBusStoptsDone.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(busstopsfreq)

busstops <- st_read("data/transport/public_transport/bus_stops_auckland.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(busstops)

marae <- st_read("data/kiwi/auckland_marae_final.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(marae)

cinema <- st_read("data/social infrastructure/culture&leisure/cinemas_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(cinema)

galleries <- st_read("data/social infrastructure/culture&leisure/galleries_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(galleries)

libraries <- st_read("data/social infrastructure/culture&leisure/libraries_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(libraries)

museums <- st_read("data/social infrastructure/culture&leisure/museums_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(museums)

theatre <- st_read("data/social infrastructure/culture&leisure/theatres_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(theatre)

chemist <- st_read("data/social infrastructure/medical/chemist_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(chemist)

dentist <- st_read("data/social infrastructure/medical/dentists_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(dentist)

healthcentre <- st_read("data/social infrastructure/medical/healthcentres_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(healthcentre)

hospital <- st_read("data/social infrastructure/medical/newhospitals_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(hospital)

childcare <- st_read("data/social infrastructure/schools/childcare_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(childcare)

primary <- st_read("data/social infrastructure/schools/primary_schools.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(primary)

secondary <- st_read("data/social infrastructure/schools/secondary_schools.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(secondary)

sport <- st_read("data/social infrastructure/sport/sport_facilities.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(sport)

conveniencestore <- st_read("data/walkability/convst_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(conveniencestore)

supermarket <- st_read("data/walkability/supermarket_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(supermarket)

bigpark <- st_read("data/greeninfrastructure/final_interp_sinplif.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(bigpark)

smallpark2 <- st_read("data/greeninfrastructure/allsmallparknodes.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(smallpark2)

cafe <- st_read("data/social infrastructure/food/cafes_all.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(cafe)

restaurants <- st_read("data/social infrastructure/food/restaurants_all.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(restaurants)

pubs <- st_read("data/social infrastructure/food/pubs_all.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(pubs)

bbq <- st_read("data/social infrastructure/food/bbqs_all.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(bbq)

gym <- st_read("data/social infrastructure/sport/gyms_all.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(gym)

beach <- st_read("data/social infrastructure/sport/beaches_all.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(beach)

bigpark_vert <- st_read("data/greeninfrastructure/bigpark_vertex.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(bigpark_vert)

#### Driving distance calculations ####
edges <- st_read("data/network_analysis/auckland_waiheke_network_drive.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area
#get network
network <- as_sfnetwork(edges, directed = FALSE) |> 
  st_transform(4326) |> 
  activate("edges")

emergency <- st_read("data/safety/emergency/emergencies_auck.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(emergency)

petrol <- st_read("data/transport/petrol_st_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(petrol)

evs_updated <- st_read("data/transport/EV_NZ_charging_stations.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(evs_updated)

crash <- st_read("data/safety/crash/Crash_Analysis_System_(CAS)_data.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(crash)

st_write(sa1, "data/greeninfrastructure/bigpark_dist_new.gpkg")

sa1done <- st_read("data/geographic/sa1_alldist_final.gpkg")
head(sa1done)