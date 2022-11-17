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

#### Data Imports ####
gpkgnet <- st_read("data/network_analysis/sample_network.gpkg")
net <- as_sfnetwork(gpkgnet, crs = 27291)
net <- st_transform(net, 27291)

grid <- st_read("data/geographic/grids/sample_grid_all.gpkg")
grid <- st_transform(grid, 27291)

stations <- st_read("data/transport/public_transport/sample_stations.gpkg")
stations <- st_transform(stations, 27291)

#### Distance analysis old ####
new_net = net %>%
  activate(nodes) %>%
  filter(group_components() == 1) %>%
  st_network_blend(grid) %>%
  st_network_blend(stations)

# Calculate the cost matrix.
cost_matrix = st_network_cost(new_net, from = sites, to = facilities, weights = "weight")

# Find for each site which facility is closest.
closest = stations[apply(cost_matrix, 1, function(x) which(x == min(x))[1])]

distm (c(40.777250, -73.872610), c(40.6895, -74.1745), fun = distHaversine)


# test network distances
p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
st_crs(p1) = st_crs(net)
p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
st_crs(p2) = st_crs(net)

paths <- st_network_paths(net, from=grid[1,], to=stations[1,], type ="shortest", use_names=FALSE)

node_path = paths %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()
node_path
oldpar = par(no.readonly = TRUE)
par(mar = c(1,1,1,1))
plot(net, col = "grey")
plot(c(grid[1,], stations[1,]), col = "black", pch = 8, add = TRUE)
plot(slice(activate(net, "nodes"), node_path), col = "red", add = TRUE)

## Distance matrix
dist_matrix <- st_network_cost(net, from = grid, to = stations)

#### Distance analysis new ####

# Define our bbox coordinates, here our coordinates relate to Portsmouth
p_bbox <- c(174.207,-37.3268,175.3151,-36.0623)

# Pass our bounding box coordinates into the OverPassQuery (opq) function
osmdata <- opq(bbox = p_bbox) |> 
  add_osm_feature(key = "highway", value = c("primary", "secondary", "tertiary", "residential", 
                                             "path", "footway", "unclassified", "living_street", "pedestrian")) |> 
  osmdata_sf()
# Extract our spatial data into variables of their own

# Extract the points, with their osm_id.
ports_roads_nodes <- osmdata$osm_points[, "osm_id"]

# Extract the lines, with their osm_id, name, type of highway, max speed and
# oneway attributes
ports_roads_edges <- osmdata$osm_lines[, c("osm_id", "name", "highway", "maxspeed", 
                                           "oneway")]


# Create network graph using are edge data, with the foot weighting profile
bb <- osmdata::getbb("Auckland, NZ")
npts <- 1000
xy <- apply (bb, 1, function (i) min (i) + runif (npts) * diff (i))
bb; head (xy)

net <- dodgr_streetnet (bb)
net <- weight_streetnet (net, wt_profile = "foot")
system.time (
  d <- dodgr_dists (net, from = xy, to = xy)
)


# Calculate distances between schools to fast-food stores
sch_to_ff_calc <- dodgr_distances(graph, from = st_coordinates(ports_schools), to = st_coordinates(ports_ff), 
                                  shortest = TRUE, pairwise = FALSE, quiet = FALSE)