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
library(cowplot)
library(COINr6)

#### Data imports ####
# geographic data
grid <- st_read("data/geographic/grids/grid_all-v-07.11.22_20.02.gpkg")
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

##### distributions #####
head(testdf)

df_to_see_dist = grid_modified # define parameter

my_plots <- lapply(names(df_to_see_dist), function(var_x){
  p <- ggplot(df_to_see_dist) +
    aes_string(var_x)
  if(is.numeric(df_to_see_dist[[var_x]])) {
    p <- p + geom_density()
  } else {
    p <- p + geom_bar()
  } 
})
plot_grid(plotlist = my_plots)

##### correlations #####
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

###### Different Normalisation methods ####

# min-max normalise columns function
minmaxNORM <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# robust scalar normalisation
robust_scalar<- function(x){
  return (x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))
}

# distance to threshold normalisation
TNORM<- function(x){
  return ((1- (800-x)) / (max(x) - min(x)))
}

# normalise against target value
#targetnorm<- function(x){
 # return min(1, 1 - (800 - x) / (max(x) - min(x)))
#}

grid_df %>% 
  mutate(station_meas = TNORM(station_dist)) %>%
  ggplot() +
  geom_density(aes(station_meas))

grid_df %>% 
  mutate(station_meas = ifelse(station_dist<=800,
                               minmax_normalise(station_dist),
                               minmax_normalise(station_dist))) %>%
  ggplot() +
  geom_density(aes(station_meas))

grid_df %>% 
  mutate(station_meas = ifelse(station_dist<=800,
                               minmax_normalise(station_dist)*(0-1)+1,
                               minmax_normalise(station_dist)*(0-1)+1)) %>%
  ggplot() +
  geom_density(aes(station_meas))
#title("station_meas")

# define parameters for index construction
reward=1.1 
normalise = robust_scalar
# indicator
grid_modified <- grid_df %>% 
  mutate(station_meas = ifelse(station_dist<=800,
                           reward*(normalise(station_dist)),  # reward for being within threshold distance
                           normalise(station_dist))) %>% # penalty for being beyond threshold distance
  mutate(bus_meas = ifelse(bus_dist<=400,
                           reward*(normalise(bus_dist)), # reward for being within threshold distance
                           normalise(bus_dist))) %>%     # penalty for being beyond threshold distance
  mutate(intersections_mea = normalise(no_intersections_in_100m)) %>%
  mutate(conv_st_mea = normalise(conv_st_dist)) %>% 
  mutate(ev_mea = normalise(ev_dist)) %>% 
  mutate(petrol_mea = normalise(petrol_st_dist)) %>% 
  mutate(second_mea = normalise(second_dist)) %>% 
  mutate(primary_mea = normalise(primary_dist)) %>% 
  mutate(childcare_mea = normalise(childcare_dist)) %>% 
  mutate(cinema_mea = normalise(cinemas_dist)) %>% 
  mutate(gallery_mea = normalise(galleries_dist)) %>% 
  mutate(library_mea = normalise(libraries_dist)) %>% 
  mutate(museum_mea = normalise(museum_dist)) %>% 
  mutate(theatre_mea = normalise(theatre_dist)) %>% 
  mutate(biking_mea = normalise(biking_len_100m)) %>% 
  mutate(bigpark_mea = normalise(bigpark_dist)) %>% 
  mutate(smallpark_mea = normalise(smallpark_dist)) %>% 
  mutate(chemists_mea = normalise(chemists_dist)) %>% 
  mutate(dentist_mea = normalise(dentist_dist)) %>% 
  mutate(supermarket_mea = normalise(supermarket_dist)) %>% 
  mutate(kuli = station_meas + bus_meas + intersections_mea + conv_st_mea +
           ev_mea + petrol_mea + second_mea + primary_mea + childcare_mea +
           cinema_mea + gallery_mea + library_mea + museum_mea + theatre_mea + biking_mea +
           bigpark_mea + smallpark_mea + chemists_mea + dentist_mea + supermarket_mea) %>% 
  mutate(kuli_norm = normalise(kuli))
  
# rejoin with geometry
grid_geom = subset(grid, select = c(id, geom))
grid_normed <- left_join(grid_geom, grid_modified, by = c("id"="id"))

summary(grid_normed)

#tmap_mode("plot")
tm_shape(grid_normed) +
  tm_dots(col = "kuli_norm", style = "equal",
          #breaks = c(0,0.1,0.3,0.5,0.61,0.8,0.9,0.99,1),
          palette = "-Reds") #+
st_write(grid_normed, "data/geographic/grids/grid_with_kuli.gpkg")

