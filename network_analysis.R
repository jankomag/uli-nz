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
edges <- st_read("data/network_analysis/auckland_waiheke_network_walk.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area

# Validate edges geometry
if (!all(st_is_valid(edges))) {
  warning("Some network edges have invalid geometries. Consider running st_make_valid()")
}

sa1 <- st_read("data/upload/sa1_auckland_waiheke_urban.gpkg") |> 
  st_transform(4326) |> 
  subset(select = c(SA12018_V1_00))

#get network
network <- as_sfnetwork(edges, directed = FALSE) |> 
  st_transform(4326) |> 
  activate("edges")


get_distance <- function(supply, sa1, network) {
  # Input validation
  if (!inherits(supply, "sf")) stop("supply must be an sf object")
  if (!inherits(sa1, "sf")) stop("sa1 must be an sf object")
  if (!inherits(network, "sfnetwork")) stop("network must be an sfnetwork object")
  
  # Check CRS compatibility
  if (st_crs(supply) != st_crs(sa1)) {
    warning("CRS mismatch between supply and sa1. Converting supply to sa1 CRS.")
    supply <- st_transform(supply, st_crs(sa1))
  }
  
  supplyname <- deparse(substitute(supply))
  supplydist <- str_glue("dist_{supplyname}")
  
  # Calculate network distances
  message("Calculating network distances...")
  distances <- tryCatch({
    dist_matrix <- st_network_cost(network, 
                                   from = sa1, 
                                   to = supply, 
                                   weights = "length")
    data.frame(dist_matrix)
  }, error = function(e) {
    stop("Failed to calculate network distances: ", e$message)
  })
  
  # Find minimum distance to nearest facility
  distances[toString(supplydist)] <- do.call(pmin, distances)
  last_data <- distances[, ncol(distances), drop = FALSE]
  
  # Find rows with Inf values
  inf_rows <- which(is.infinite(last_data[[1]]))
  
  # If there are Inf values, calculate Euclidean distances for those
  if (length(inf_rows) > 0) {
    message("Calculating Euclidean distances for ", length(inf_rows), " areas...")
    
    # Calculate Euclidean distances for areas with Inf values
    euclidean_distances <- st_distance(
      st_centroid(sa1[inf_rows,]), 
      supply
    )
    
    # Replace Inf values with minimum Euclidean distances
    last_data[inf_rows, 1] <- apply(euclidean_distances, 1, min)
    
    message("Used Euclidean distance fallback for ", length(inf_rows), 
            " out of ", nrow(sa1), " areas")
  }
  
  # Return results
  result <- cbind(sa1, last_data)
  return(result)
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
stations <- st_read("data/upload/trains_auckland.gpkg") |> st_transform(4326)
busstopsfreq <- st_read("data/upload/frequentBusStopts.gpkg") |> st_transform(4326)
marae <- st_read("data/upload/auckland_marae_final.gpkg") |> st_transform(4326)
primary <- st_read("data/upload/primary_schools.gpkg") |> st_transform(4326)
secondary <- st_read("data/upload/secondary_schools.gpkg") |> st_transform(4326)
sport <- st_read("data/upload/sport_facilities.gpkg") |> st_transform(4326)
bigpark <- st_read("data/auckland_parks_access_points.gpkg") |> st_transform(4326)

#### Walking distance calculations ####
sa1 <- get_distance(cafe, sa1, network)
sa1 <- get_distance(restaurant, sa1, network)
sa1 <- get_distance(pub, sa1, network)
sa1 <- get_distance(bbq, sa1, network)
sa1 <- get_distance(cinema, sa1, network)
sa1 <- get_distance(theatre, sa1, network)
sa1 <- get_distance(gallery, sa1, network)
sa1 <- get_distance(museum, sa1, network)
sa1 <- get_distance(library, sa1, network)
sa1 <- get_distance(chemist, sa1, network)
sa1 <- get_distance(dentist, sa1, network)
sa1 <- get_distance(hospital, sa1, network)
sa1 <- get_distance(healthcentre, sa1, network)
sa1 <- get_distance(childcare, sa1, network)
sa1 <- get_distance(conveniencestore, sa1, network)
sa1 <- get_distance(supermarket, sa1, network)
sa1 <- get_distance(gym, sa1, network)
sa1 <- get_distance(beach, sa1, network)
sa1 <- get_distance(bigpark, sa1, network)
sa1 <- get_distance(stations, sa1, network)
sa1 <- get_distance(busstopsfreq, sa1, network)
sa1 <- get_distance(marae, sa1, network)
sa1 <- get_distance(primary, sa1, network)
sa1 <- get_distance(secondary, sa1, network)
sa1 <- get_distance(sport, sa1, network)

tm_shape(sa1) +
  tm_borders(lwd = 0) +
  tm_fill("dist_cinema", title = "Distance") +
  tm_layout(legend.position = c("left", "bottom"))

#### Driving distance calculations ####
edges <- st_read("data/network_analysis/auckland_waiheke_network_drive.gpkg", layer='edges') |> 
  st_transform(4326) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, access, bridge,
                     width, junction, tunnel)) |> st_transform(4326)
#get network
network <- as_sfnetwork(edges, directed = FALSE) |> 
  st_transform(4326) |> 
  activate("edges")

bbox_coords <- c(174.489515, -37.159816, 175.258558, -36.570920)
bbox <- matrix(
  c(bbox_coords[1], bbox_coords[2], 
    bbox_coords[3], bbox_coords[4]), 
  ncol = 2, 
  byrow = TRUE
)

# Create a function to fetch and process the data
fetch_ev_stations <- function(url, bbox) {
  # Read GeoJSON directly from URL
  tryCatch({
    ev_charge <- st_read(
      url,
      quiet = TRUE
    ) |> 
      st_transform(4326)  # Ensure WGS84 projection
    
    # Create bbox as sf object
    bbox_poly <- st_as_sfc(st_bbox(
      c(xmin = bbox[1,1], 
        ymin = bbox[1,2], 
        xmax = bbox[2,1], 
        ymax = bbox[2,2]
      ), 
      crs = 4326
    ))
    
    # Filter points within bbox
    ev_charge_filtered <- ev_charge |>
      st_intersection(bbox_poly)
    
    # Return filtered data
    return(ev_charge_filtered)
    
  }, error = function(e) {
    message("Error fetching or processing data: ", e$message)
    return(NULL)
  })
}

# URL for the EV charging stations
url <- "https://services.arcgis.com/CXBb7LAjgIIdcsPt/arcgis/rest/services/EV_Roam_charging_stations/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

# Fetch and filter the data
ev_charge <- fetch_ev_stations(url, bbox)
sa1 <- get_distance(ev_charge, sa1, network)

tm_shape(sa1) +
  tm_borders(lwd = 0) +
  tm_fill("dist_ev_charge", title = "Distance") +
  tm_layout(legend.position = c("left", "bottom"))

#### Road Crash Risk ####
edges <- st_transform(edges, 27291)
  
# get polygon data
sa1_polys <- st_read("data/upload/sa1_auckland_waiheke_urban.gpkg") |>
  subset(select = c(SA12018_V1_00)) |> 
  st_transform(27291) #transforming to the same coordinate system

# get population data
census <- read.csv('data/upload/auckland_census_3.csv') |>
  subset(select = c(censusnightpop, code)) |> 
  mutate(code = as.character(code))
census[which(is.na(census$censusnightpop),), "censusnightpop"] <- 1

sa1_polys <- left_join(sa1_polys, census, by=c("SA12018_V1_00"="code"))

# Intersect roads with SA1 areas
roadlens <- sf::st_intersection(edges, sa1_polys) %>% # Find the intersections, which should all be points or multilines
  dplyr::mutate(len_m = sf::st_length(geom)) %>% # Find the length of each line
  dplyr::group_by(SA12018_V1_00) %>%
  dplyr::summarize(len_m = sum(len_m))
roadlens <- st_drop_geometry(roadlens)

sa1_roadlens <- left_join(sa1_polys, roadlens, by="SA12018_V1_00")
tm_shape(sa1_roadlens)+tm_fill("len_m", style="jenks")

# get crashes data
crash <- st_read("data/upload/auckland_CAS.gpkg") |> 
  st_transform(27291) #transforming to the same coordinate system

sa1_crash <- mutate(sa1_polys, n = lengths(st_intersects(sa1_polys, crash))) 
sa1_crash$area <- as.numeric(st_area(sa1_crash))
sa1_crash <- left_join(sa1_crash, roadlens, by="SA12018_V1_00")

sa1_crash <- sa1_crash |> 
  mutate(crashesperarea = n/area) |> 
  mutate(crash_per_roadlen = n/len_m) |> 
  mutate(crash_risk = as.numeric(crash_per_roadlen/censusnightpop))
sa1_crash[which(is.na(sa1_crash$crash_per_roadlen),), "crash_per_roadlen"] <- 0.01
sa1_crash$crash_risk <- ifelse(is.na(sa1_crash$crash_risk), min(sa1_crash$crash_risk,na.rm = T), sa1_crash$crash_risk)

tm_shape(sa1_crash)+tm_fill(c("crashesperarea","crash_per_roadlen","crash_risk"), style="jenks")

sa1_crash <- sa1_crash |> 
  dplyr::select(SA12018_V1_00, crash_risk, crash_per_roadlen)

st_write(sa1_crash, "data/sa1_crash_risk.gpkg", append=F, driver="GPKG")

# finally join to the rest of the data
sa1_crash <- st_drop_geometry(sa1_crash)
sa1 <- left_join(sa1, sa1_crash, by="SA12018_V1_00")

#### Bikeability ####
edges_bike <- st_read("data/network_analysis/auckland_waiheke_network_bike.gpkg", layer='edges') |> 
  st_transform(27291) |> 
  subset(select = -c(u,v,key,osmid, lanes, name, highway, oneway, reversed, from, to,ref, service, access, bridge,
                     width, junction, tunnel)) #area

#sa1 <- st_read("data/upload/sa1_auckland_waiheke_urban.gpkg") |> 
#  st_transform(27291) |> 
#  subset(select = c(SA12018_V1_00))

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
sa1_bikeability <- st_drop_geometry(sa1_bikeability)

# finally join to the rest of the data
sa1 <- left_join(sa1, sa1_bikeability, by="SA12018_V1_00")
st_write(sa1, "data/sa1_out_dist.gpkg", append=F)
