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


