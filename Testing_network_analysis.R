#library(stplanr)
library(sf)
library(sp)
library(tmap)
library(igraph)
library(sfnetworks)
library(dplyr)
library(tidygraph)

#### Data Imports ####
gpkgnet <- st_read("data/network_analysis/sample_network.gpkg")
net <- as_sfnetwork(gpkgnet, directed = FALSE) %>%
  st_transform(27291) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

grid <- st_read("data/geographic/grids/sample_grid_all.gpkg")
grid <- st_transform(grid, 27291)

stations <- st_read("data/transport/public_transport/sample_stations.gpkg")
stations <- st_transform(stations, 27291)

#test plotting
tm_shape(grid) +
  tm_lines(col="black") +
  tm_shape(stations) +
  tm_dots(col="red") +
  tm_shape(grid) +
  tm_dots(col="grey")

# network distances
#p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
p1 = grid[1,]
st_crs(p1) = st_crs(net)
#p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
p2 = stations[1,]
st_crs(p2) = st_crs(net)

paths <- st_network_paths(net, from=p1, to=p2, type ="shortest", use_names=FALSE, weights = "weight")

node_path = paths %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()

oldpar = par(no.readonly = TRUE)
par(mar = c(1,1,1,1))
plot(net, col = "grey")
plot(c(p1, p2), col = "black", pch = 8, add = TRUE)
plot(slice(activate(net, "nodes"), node_path), col = "red", add = TRUE)

## Distance matrix
dist_matrix <- st_network_cost(net, from = grid, to = stations, weights = "weight")

#### Following analysis ####
new_net = net %>%
  activate("nodes") %>%
  filter(group_components() == 1) %>%
  st_network_blend(grid) %>%
  st_network_blend(stations)

# Calculate the cost matrix.
cost_matrix <- st_network_cost(new_net, from = grid, to = stations, weights = "weight")
# Find for each site which facility is closest.
closest <- stations[apply(cost_matrix, 1, function(x) which(x == min(x))[1])]

#### Dodgr ####
library(dodgr)

dnew_net = net %>%
  st_network_blend(grid) %>%
  st_network_blend(stations)

d <- dodgr_dists (graph = dnew_net, from = grid, to = stations)


#classic distances.R
library(sf)
library(sp)
library(tmap)
library(dplyr)
library(geosphere)
library(raster)

#### Data Imports ####
grid <- st_read("data/geographic/grids/sample_grid_all.gpkg")
grid <- st_transform(grid, 4326)
#27291

stations <- st_read("data/transport/public_transport/sample_stations.gpkg")
stations <- st_transform(stations, 4326)

p1 <- grid[1,]
p1x <- st_coordinates(p1)[,1]
p1y <- st_coordinates(p1)[,2]
p2 <- stations[1,]
p2x <- st_coordinates(p2)[,1]
p2y <- st_coordinates(p2)[,2]

distm(c(p1x, p1y), c(p2x, p2y), fun = distHaversine)

#### Distance analysis ####
i=0
for (point in 1:nrow(grid)){
  i
}





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
