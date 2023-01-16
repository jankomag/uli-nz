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
edges <- st_read("data/network_analysis/auckland_waiheke_network_walk.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area
sa1 <- st_read("data/geographic/urban_sa1_landvalid.gpkg") |> 
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
busstopsfreq <- st_read("data/transport/public_transport/busstops_frequent.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(busstopsfreq)

#calulate distances to bus stops with frequent service
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

bigpark2 <- st_read("data/green infrastructure/bigaprks_points.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(bigpark2)

smallpark2 <- st_read("data/green infrastructure/allsmallparknodes.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(smallpark2)

edges <- st_read("data/network_analysis/auckland_waiheke_network_drive.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area
#get network
network <- as_sfnetwork(edges, directed = FALSE) |> 
  st_transform(4326) |> 
  activate("edges")

petrol <- st_read("data/transport/petrol_st_all.gpkg") |>
  st_transform(4326)# 27291
sa1 <- get_distance(petrol)

evs_updated <- st_read("data/transport/EV_NZ_charging_stations.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(evs_updated)

crash <- st_read("data/safety/crash/Crash_Analysis_System_(CAS)_data.geojson") |>
  st_transform(4326)# 27291
sa1 <- get_distance(crash)

#save final
st_write(sa1, "data/safety/crash/sa1_crashdist.gpkg")
st_write(sa1, "data/geographic/sa1_alldist_final.gpkg")

sa1done <- st_read("data/geographic/sa1_alldist_final.gpkg")
head(sa1done)


# old way pre-function
dist_matrix = data.frame(st_network_cost(network, from = sa1, to = stations, weights = "length"))
dist_matrix$station_dist <- do.call(pmin, dist_matrix)
dist_matrix <- dist_matrix |> 
  subset(select = c(min_dist_station))

sa1_dist <- cbind(sa1, dist_matrix)
