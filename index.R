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
library(stringr)
library(DescTools)

#### Data imports ####
# geographic data
grid <- st_read("data/geographic/grids/grid_all-v-11.11.22-19.22.gpkg")
#transforming to the same coordinate system
grid <- st_transform(grid, 27291)
grid$id <- 1:nrow(grid)

#### EDA ####
summary(grid)
# strip from geography for EDA
grid_df <- grid |> st_drop_geometry()
# save geometry only for later
grid_geom = subset(grid, select = c(id, geom))

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

##### distributions #####
df_to_see_dist = grid_logged # define parameter
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

#### Index Construcion ####
##### Different Normalisation methods ####
# min-max normalise columns function
minmaxNORM <- function(x) {
  return (((x - min(x))) / (max(x) - min(x))*(10-0)+0)
}

# threshold normalisation
targetnorm <- function(x, threshold, penalty, lim){
  x <- ifelse(x<=threshold,
                   min(x,lim), #normal linear
                   min(
                     x+penalty*(-log(x/threshold)),
                     lim)) # penalty for not meeting the target
  return (x)
}

#test function
testfunc <- function(x, threshold, penalty, lim){
  trans_x = if_else(x<=10,
                    x,
                    x+12*(-log(x/10))) #
  return (trans_x)
}
curve(testfunc, from=1, to=50, xlab="x", ylab="y")

#vis different standarisation methods
grid_df |>
  ggplot() +
  geom_density(aes(minmaxNORM(-log(smallpark_dist))))

grid_df |>
  mutate(bus_meas = Winsorize((no_households),minval=0,maxval=100)) |> 
  ggplot() +
  geom_density(aes(bus_meas))

grid_df |>
  mutate(smallpark_dist = minmaxNORM(-log(sapply(smallpark_dist, targetnorm, 600, penalty, 2000)))) |> 
  ggplot() +
  geom_density(aes(smallpark_dist))

##### Normalaise variables #####
# indicator
penalty = 5

grid_modified <- grid_df |> 
  mutate(bus_meas = minmaxNORM(-log(sapply(bus_dist, targetnorm, 400, penalty, 10000)))) |>
  mutate(station_mea = minmaxNORM(-log(station_dist))) |>
  mutate(intersections_mea = minmaxNORM(Winsorize(no_intersections_in_100m, maxval=10))) |>
  mutate(conv_st_mea = minmaxNORM(-log(conv_st_dist))) |> 
  mutate(second_mea = minmaxNORM(-log(second_dist))) |> 
  mutate(primary_mea = minmaxNORM(-log(primary_dist))) |> 
  mutate(childcare_mea = minmaxNORM(-log(childcare_dist))) |> 
  mutate(cinema_mea = minmaxNORM(-log(cinemas_dist))) |> 
  mutate(gallery_mea = minmaxNORM(-log(galleries_dist))) |> 
  mutate(library_mea = minmaxNORM(-log(libraries_dist))) |> 
  mutate(museum_mea = minmaxNORM(-museum_dist)) |> 
  mutate(theatre_mea = minmaxNORM(-log(theatre_dist))) |> 
  mutate(biking_mea = minmaxNORM(Winsorize(biking_len_100m,minval=0.0001,maxval=10000))) |> 
  mutate(bigpark_mea = minmaxNORM(-log(bigpark_dist))) |> 
  mutate(smallpark_mea = minmaxNORM(-log(smallpark_dist))) |> 
  mutate(chemists_mea = minmaxNORM(-log(chemists_dist))) |> 
  mutate(dentist_mea = minmaxNORM(-log(dentist_dist))) |> 
  mutate(supermarket_mea = minmaxNORM(-log(supermarket_dist))) |> 
  mutate(crimes_mea = minmaxNORM(minmaxNORM(-log(crimes)+1))) |> 
  mutate(dwellings_mea = minmaxNORM(Winsorize(no_households,minval=0,maxval=10000))) |>
  mutate(kuli = station_mea + bus_meas + intersections_mea + conv_st_mea +
           second_mea + primary_mea + childcare_mea +
           cinema_mea + gallery_mea + library_mea + museum_mea + theatre_mea +
           biking_mea +  bigpark_mea + smallpark_mea + chemists_mea +
           dentist_mea + supermarket_mea + crimes_mea + dwellings_mea) |> 
  mutate(kuli_norm = minmaxNORM(kuli))

# rejoin with geometry
grid_normed <- left_join(grid_geom, grid_modified, by = c("id"="id"))

#tmap_mode("plot")
tm_shape(grid_normed) +
  tm_dots(col = "kuli_norm",# style = "equal",
          breaks = c(0,1,2,3,4,5,5.7,6,7.5,8,9.5,10),
          palette = "Reds", title = str_glue('Penalty= {penalty}'))
#st_write(grid_normed, "data/geographic/grids/grid_with_kuli.gpkg")

