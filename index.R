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

#### EDA ####
summary(grid)
# strip from geography for EDA
grid_df <- grid |> st_drop_geometry()

##### distributions #####
df_to_see_dist = grid_df # define parameter
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
corrmatrix <- corrmatrix |>
  arrange(row) |>
  filter(row == 'income')
  
# plot correlations
grid_df |>
  gather(-no_intersections_in_100m, key = "var", value = "value") |> 
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
  return (((x - min(x))) / (max(x) - min(x))*(10-0)+0)
}

# robust scalar normalisation
robust_scalar<- function(x){
  return (x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))
}

# distance to threshold normalisation
distTNORM<- function(x){
  return ((1- (800-x)) / (max(x) - min(x)))
}

# threshold normalisation
targetnorm <- function(x, threshold, penalty, lim){
  x <- ifelse(x<=threshold,
                   x, #normal linear
                   min(x*penalty,lim)) # penalty for not meeting the target
  return (x)
}

# other normalisation
targetnormlog <- function(x, threshold){
  trans_x = if_else(x<threshold,
                   x,1) #normal linear
                   #-log(1/x)) # penalty for not meeting the target
  return (trans_x)
}

#vis different standarisation methods
grid_df |>
  ggplot() +
  geom_density(aes(smallpark_dist))

grid_df |>
  mutate(bus_meas = minmaxNORM(-smallpark_dist)) |> 
  ggplot() +
  geom_density(aes(bus_meas))

grid_df |>
  mutate(smallpark_dist = minmaxNORM(-sapply(smallpark_dist, targetnorm, 600, 2000))) |> 
  ggplot() +
  geom_density(aes(smallpark_dist))

# indicator
penalty = 1.5

grid_modified <- grid_df |> 
  mutate(bus_meas = minmaxNORM(-sapply(bus_dist, targetnorm, 400, penalty, 2000))) |> # penalty for being beyond threshold distance
  mutate(station_meas = minmaxNORM(-sapply(station_dist, targetnorm, 800, penalty, 10000))) |>  # penalty for being beyond threshold distance
  mutate(intersections_mea = minmaxNORM(no_intersections_in_100m)) |>
  mutate(conv_st_mea = minmaxNORM(-conv_st_dist)) |> 
  mutate(ev_mea = minmaxNORM(-ev_dist)) |> 
  mutate(petrol_mea = minmaxNORM(-petrol_st_dist)) |> 
  mutate(second_mea = minmaxNORM(-second_dist)) |> 
  mutate(primary_mea = minmaxNORM(-primary_dist)) |> 
  mutate(childcare_mea = minmaxNORM(-childcare_dist)) |> 
  mutate(cinema_mea = minmaxNORM(-cinemas_dist)) |> 
  mutate(gallery_mea = minmaxNORM(-galleries_dist)) |> 
  mutate(library_mea = minmaxNORM(-libraries_dist)) |> 
  mutate(museum_mea = minmaxNORM(-museum_dist)) |> 
  mutate(theatre_mea = minmaxNORM(-theatre_dist)) |> 
  mutate(biking_mea = minmaxNORM(biking_len_100m)) |> 
  mutate(bigpark_mea = minmaxNORM(-sapply(bigpark_dist, targetnorm, 1500, penalty, 3000))) |> 
  mutate(smallpark_mea = minmaxNORM(-sapply(smallpark_dist, targetnorm, 600, penalty, 2000))) |> 
  mutate(chemists_mea = minmaxNORM(-chemists_dist)) |> 
  mutate(dentist_mea = minmaxNORM(-dentist_dist)) |> 
  mutate(supermarket_mea = minmaxNORM(-supermarket_dist)) |> 
  mutate(kuli = station_meas + bus_meas + intersections_mea + conv_st_mea +
           ev_mea + petrol_mea + second_mea + primary_mea + childcare_mea +
           cinema_mea + gallery_mea + library_mea + museum_mea + theatre_mea + biking_mea +
           bigpark_mea + smallpark_mea + chemists_mea + dentist_mea + supermarket_mea) |> 
  mutate(kuli_norm = minmaxNORM(kuli))
  
# rejoin with geometry
grid_geom = subset(grid, select = c(id, geom))
grid_normed <- left_join(grid_geom, grid_modified, by = c("id"="id"))

summary(grid_normed)

#tmap_mode("plot")
tm_shape(grid_normed) +
  tm_dots(col = "kuli_norm", #style = "equal",
          breaks = c(0,1,3,5,6.5,7.5,8,8.5,9,9.5,9.9,10),
          palette = "Reds", title = str_glue('Penalty= {penalty}'))
#st_write(grid_normed, "data/geographic/grids/grid_with_kuli.gpkg")

