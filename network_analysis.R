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

#### Data Imports ####
edges <- st_read("data/network_analysis/full_auckland_network.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel, area))
sa1 <- st_read("data/geographic/sa1_centorids_clean.gpkg") |> 
  st_transform(4326)

#get network
net <- as_sfnetwork(sample_edges, directed = FALSE) |> 
  st_transform(4326) |> 
  activate("edges")

get_distance <- function(supply) {
  dist_matrix <- data.frame(st_network_cost(net, from = sa1, to = supply, weights = "length"))
  dist_matrix$new_dist <- do.call(pmin, dist_matrix)
  dist_matrix <- colnames(dist_matrix)[-1] <- toString(c("dist",supply)) #rename the column to the thing being measured
  dist_matrix <- dist_matrix[ , ncol(dist_matrix), drop = FALSE] #keep only the last column
  sa1 <- cbind(sa1, dist_matrix)
}

#calulate distances to train stations
stations <- st_read("data/transport/public_transport/trains_auckland.gpkg") |>
  st_transform(4326)# 27291

dist_matrix = data.frame(st_network_cost(net, from = sa1, to = stations, weights = "length"))
dist_matrix$station_dist <- do.call(pmin, dist_matrix)
dist_matrix <- dist_matrix |> 
  subset(select = c(min_dist_station))

sa1_dist <- cbind(sa1, dist_matrix)

#calulate distances to bus stops
busstops <- st_read("data/transport/public_transport/bus_stops_auckland.geojson") |>
  st_transform(4326)# 27291

dist_matrix = data.frame(st_network_cost(net, from = sa1, to = busstops, weights = "length"))
dist_matrix$bus_dist <- do.call(pmin, dist_matrix)
dist_matrix <- dist_matrix |> 
  subset(select = c(bus_dist))

sa1_dist <- cbind(sa1, dist_matrix)


sa1c <- sa1

last_data <- sa1c[ , ncol(sa1c), drop = FALSE]    # Apply ncol & drop
last_data                   
#calulate distances to marae
marae <- st_read("data/kiwi/marae.gpkg") |>
  st_transform(4326)# 27291

get_distance(marae)
sa1
#save final
st_write(sa1, "allsa1_dist_marae.gpkg")


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