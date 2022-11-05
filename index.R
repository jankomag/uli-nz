# load the packages
library(sf)
library(sp)
library(tmap)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(geosphere)
library(spdep)
library(Hmisc)
library(corrplot)
library(car)
library(gclus)
library(corrr)
library(tidyr)
 
#### Data imports ####
# geographic data
grid <- st_read("data/geographic/grids/grid_all-v-04.11.22_19.52.gpkg")
#transforming to the same coordinate system
grid <- st_transform(grid, 27291)
grid$id <- 1:nrow(grid)

# test plotting
tm_shape(grid) +
  tm_dots(col="cinemas_dist",
          style = "kmeans", palette = "Reds")

#### EDA ####
summary(grid)
# strip from geography for EDA
grid_df <- grid %>% st_drop_geometry()

##### inspecting correlations #####
cor <- cor(x = grid_df[1:21], y = grid_df[1:21], use="complete.obs")
corrplot(cor, tl.srt = 25)
corr <- rcorr(as.matrix(grid_df))

# function to make correlation matrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Get correlation matrix with coefficients and p-values
corrmatrix <- flattenCorrMatrix(corr$r, corr$P)
corrmatrix <- corrmatrix %>%
  arrange(row) %>%
  filter(row == 'income')
  
# plot correlations
grid_df %>%
  gather(-no_intersections_in_100m, key = "var", value = "value") %>% 
  ggplot(aes(x = no_intersections_in_100m, y = value)) +
  facet_wrap(~ var, scales = "free") +
  geom_point(alpha=0.2) +
  theme_bw() +
  geom_smooth(method="lm")

#### Index Construcion ####
summary(grid_df)

#normalise columns function
normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
reward=1.1
# indicators
grid_modified <- grid_df %>% 
  mutate(station_meas = ifelse(station_dist<=800,
                           reward*(1-(normalise(station_dist))),  # reward for being within threshold distance
                           normalise(1-(normalise(station_dist))))) %>% # penalty for being beyond threshold distance
  mutate(bus_meas = ifelse(bus_dist<=400,
                           reward*(1-(normalise(bus_dist))), # reward for being within threshold distance
                           normalise(1-(normalise(bus_dist))))) %>%     # penalty for being beyond threshold distance
  mutate(intersections_mea = normalise(no_intersections_in_100m)) %>%
  mutate(conv_st_mea = normalise(1-(normalise(conv_st_dist)))) %>% 
  mutate(ev_mea = normalise(1-(normalise(ev_dist)))) %>% 
  mutate(petrol_mea = normalise(1-(normalise(petrol_st_dist)))) %>% 
  mutate(second_mea = normalise(1-(normalise(second_dist)))) %>% 
  mutate(primary_mea = normalise(1-(normalise(primary_dist)))) %>% 
  mutate(childcare_mea = normalise(1-(normalise(childcare_dist)))) %>% 
  mutate(cinema_mea = normalise(1-(normalise(cinemas_dist)))) %>% 
  mutate(gallery_mea = normalise(1-(normalise(galleries_dist)))) %>% 
  mutate(library_mea = normalise(1-(normalise(libraries_dist)))) %>% 
  mutate(museum_mea = normalise(1-(normalise(museum_dist)))) %>% 
  mutate(theatre_mea = normalise(1-(normalise(theatre_dist)))) %>% 
  mutate(biking_mea = normalise(station_meas)) %>% 
  mutate(kuli = station_meas + bus_meas + intersections_mea + conv_st_mea +
           ev_mea + petrol_mea + second_mea + primary_mea + childcare_mea +
           cinema_mea + gallery_mea + library_mea + museum_mea + theatre_mea + biking_mea) %>% 
  mutate(kuli_norm = normalise(kuli))
  
# normalise each column
#grid_df_n <- data.frame(grid_df[22], lapply(grid_df[1:21], normalise))

# rejoin with geometry
grid_geom = subset(grid, select = c(id, geom))
grid_normed <- left_join(grid_geom, grid_modified, by = c("id"="id"))

summary(grid_normed)

#tmap_mode("plot")
tm_shape(grid_normed) +
  tm_dots(col = "kuli", style = "fisher",
          palette = "Reds") #+
  #tm_layout(legend.position = c("right", "top"), title=reward), title.position = c('left', 'top'))
st_write(grid_normed, "data/geographic/grids/grid_with_kuli.gpkg")

