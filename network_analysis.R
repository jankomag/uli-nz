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
library(ggplot2)
library(units)
library(geodist)

#### Data Imports ####
edges <- st_read("data/networks/auckland_waiheke_network_walk.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area

sa1 <- st_read("data/sa1_kuli_all.gpkg") |> 
  st_transform(4326) |> 
  subset(select = c(SA12018_V1_00))
smoledges <- head(edges, 1000)
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

smolpolys <- head(sa1_polys,10)
# Testing Density based measure
osmdata <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary", "tertiary", "residential", 
                                             "path", "footway", "unclassified", "living_street", "pedestrian")) %>%
  # And then pipe this into our osmdata_sf object
  osmdata_sf()
walkingosm_edges <- osmdata$osm_lines[, c("osm_id", "name", "highway", "maxspeed", 
                                           "oneway")]
graph <- weight_streetnet(walkingosm_edges, wt_profile = "foot", type_col="highway")
smolpolys_to_bigpark_calc <- dodgr_distances(graph, from = st_coordinates(smolpolys), to = st_coordinates(bigpark), 
                                  shortest = TRUE, pairwise = FALSE, quiet = FALSE)

smolpolys$park_within_400m <- count_row_if(lt(401), smolpolys_to_bigpark_calc)


#### Driving distance calculations ####
edges <- st_read("data/networks/auckland_waiheke_network_drive.gpkg", layer='edges') |> 
  st_transform(27291) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area
#get network
network <- as_sfnetwork(edges, directed = FALSE) |> 
  st_transform(27291) |> 
  activate("edges")

emergency <- st_read("data/emergencies_auck.gpkg") |> st_transform(4326)
ev_charge <- st_read("data/EV_NZ_charging_stations.geojson") |> st_transform(4326)
#crash <- st_read("uli-nz/data/Crash_Analysis_System_(CAS)_data.geojson") |> st_transform(4326)

sa1 <- get_distance(emergency)
sa1 <- get_distance(ev_charge)
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

#option 1
# Intersect roads with SA1 areas
roadlens <- sf::st_intersection(edges, sa1_polys) %>% # Find the intersections, which should all be points or multilines
  dplyr::mutate(len_m = sf::st_length(geom)) %>% # Find the length of each line
  dplyr::group_by(SA12018_V1_00) %>% # Here you need to insert all the columns from your shapes
  dplyr::summarize(len_m = sum(len_m))
roadlens <- st_drop_geometry(roadlens)

sa1_roadlens <- left_join(sa1_polys, roadlens, by="SA12018_V1_00")
tm_shape(sa1_roadlens)+tm_fill("len_m", style="jenks")
#st_write(sa1_roadlens, "sa1_roadlens.gpkg")
summary(sa1_roadlens)

# Simple calculate road lengths
sa1_roadsSimple <- sa1_roads %>%
  group_by(SA12018_V1_00) %>% # group by area
  summarize(total_road_length = sum(length))
tm_shape(sa1_roadsSimple)+tm_fill("total_road_length", style="jenks")
sa1_polys[which(is.na(sa1_roadsSimple$total_raod_length),), "total_raod_length"] <- 0

# Buffering #
smol <- head(edges, 500)
l1.sf.buf <- st_buffer(smol, dist = 50)
l1.sf.buf.dis <- l1.sf.buf %>% 
  group_by()  %>% 
  summarise()
ggplot(l1.sf.buf.dis) + geom_sf() 

# get crashes data
crash <- st_read("data/auckland_CAS.gpkg") |> 
  st_transform(27291) #transforming to the same coordinate system

sa1_crash <- mutate(sa1_polys, n = lengths(st_intersects(sa1_polys, crash))) 
sa1_crash$area <- as.numeric(st_area(sa1_crash))
sa1_crash <- left_join(sa1_crash, roadlens, by="SA12018_V1_00")

sa1_crash <- sa1_crash |> 
  mutate(crashesperarea = n/area) |> 
  mutate(crash_per_roadlen = n/len_m) |> 
  mutate(crash_risk = crash_per_roadlen/(censusnightpop/area))

tm_shape(sa1_crash)+tm_fill("crash_risk", style="jenks")
sa1_crash %>%
  ggplot(aes(x = crash_risk)) +
  geom_histogram(bins = 10)

# finally join to the rest of the data
sa1_crash <- sa1_crash |> 
  select(SA12018_V1_00, crash_risk, crash_per_roadlen) |> 
  st_drop_geometry()
sa1 <- left_join(sa1, sa1_crash, by="SA12018_V1_00")

#### Bikeability ####
edges_bike <- st_read("data/networks/auckland_waiheke_network_bike.gpkg", layer='edges') |> 
  st_transform(27291) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area

sa1 <- st_read("data/sa1_kuli_all.gpkg") |> 
  st_transform(27291) |> 
  subset(select = c(SA12018_V1_00))

bike_lengths <- sf::st_intersection(edges_bike, sa1_polys) %>% # Find the intersections, which should all be points or multilines
  dplyr::mutate(len_m = sf::st_length(geom)) %>% # Find the length of each line
  dplyr::group_by(SA12018_V1_00) %>% # Here you need to insert all the columns from your shapes
  dplyr::summarize(len_m = sum(len_m))
bike_lengths <- st_drop_geometry(bike_lengths)

sa1_polys$area <- st_area(sa1_polys)
sa1_bikeability <- left_join(sa1_polys, bike_lengths, by="SA12018_V1_00")
sa1_bikeability <- sa1_bikeability |> 
  mutate(bikeability = as.numeric(len_m/area))
sa1_bikeability <- dplyr::select(sa1_bikeability, SA12018_V1_00, bikeability)

tm_shape(sa1_bikeability)+tm_fill("bikeability", style="jenks")

#st_write(sa1_bikeability, "sa1_bikeability.gpkg")

# finally join to the rest of the data
sa1 <- left_join(sa1, sa1_bikeability, by="SA12018_V1_00")

st_write(sa1_bikeability, "data/sa1_bikeability.gpkg")
