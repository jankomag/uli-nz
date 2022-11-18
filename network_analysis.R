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

## RERUN WITH WAIHEKE ISLAND NETWORK - IT WAS EXCLUDED
#### Data Imports ####
edges <- st_read("data/network_analysis/full_auckland_network.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel, area))
sa1 <- st_read("data/geographic/sa1_centorids_clean.gpkg") |> 
  st_transform(4326)
grid_sample <- st_read("data/geographic/grids/sample_grid_all.gpkg") |> 
  st_transform(4326)

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

#calulate distances to train stations
stations <- st_read("data/transport/public_transport/trains_auckland.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(stations)

#calulate distances to bus stops
busstops <- st_read("data/transport/public_transport/bus_stops_auckland.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(busstops)

#calulate distances to marae
marae <- st_read("data/kiwi/marae.gpkg") |>
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
sa1 <- get_distance(childcare)

sport <- st_read("data/social infrastructure/sport/sport_facilities.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(sport)

conveniencestore <- st_read("data/walkability/convst_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(conveniencestore)

supermarket <- st_read("data/walkability/supermarket_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(supermarket)

#save final
st_write(sa1, "data/allsa1_dist.gpkg")
head(sa1)

# old way pre-function
dist_matrix = data.frame(st_network_cost(network, from = sa1, to = stations, weights = "length"))
dist_matrix$station_dist <- do.call(pmin, dist_matrix)
dist_matrix <- dist_matrix |> 
  subset(select = c(min_dist_station))

sa1_dist <- cbind(sa1, dist_matrix)


#### Distance analysis old ####
# Define our bbox coordinates, here our coordinates relate to Portsmouth
p_bbox <- c(174.207,-37.3268,175.3151,-36.0623)
# Pass our bounding box coordinates into the OverPassQuery (opq) function
osmdata <- opq(bbox = p_bbox, timeout = 100) |> 
  add_osm_feature(key = "highway", value = c("primary", "secondary", "tertiary", "residential", 
                                             "path", "footway", "unclassified", "living_street", "pedestrian")) |> 
  osmdata_sf()
# Extract our spatial data into variables of their own
# Extract the points, with their osm_id.
nodes <- osmdata$osm_points[, "osm_id"]

# Extract the lines, with their osm_id, name, type of highway, max speed
edges <- osmdata$osm_lines[, c("osm_id", "name", "highway", "maxspeed", 
                                           "oneway")]
# Create network graph using edge data, with the foot weighting profile
graph <- weight_streetnet(edges, wt_profile = "foot")

# Calculate distances between grid to stations
sch_to_ff_calc <- dodgr_distances(graph, from = st_coordinates(grid), to = st_coordinates(stations), 
                                  shortest = TRUE, pairwise = FALSE, quiet = FALSE)