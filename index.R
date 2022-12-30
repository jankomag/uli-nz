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
sa1_allg <- st_read("data/geographic/sa1_allvars.gpkg") |> st_transform(27291) #transforming to the same coordinate system
#sa1_allg$id <- 1:nrow(sa1_allg)

#### EDA ####
summary(sa1_allg)
# strip from geography for EDA
sa1_all <- sa1_allg |> st_drop_geometry()
##### correlations #####
cor <- cor(x = sa1_all[2:43], y = sa1_all[2:43], use="complete.obs")
corrplot(cor, tl.srt = 25)
corr <- rcorr(as.matrix(sa1_all))

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
sa1_all |>
  gather(key = "var", value = "value") |> 
  ggplot(aes(x = "shannon", y = value)) +
  facet_wrap(~ var, scales = "free") +
  geom_point(alpha=0.2) +
  theme_bw() +
  geom_smooth(method="lm")

##### Distributions #####
df_to_see_dist = sa1_all # define parameter
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
  return (((x - min(x))) / (max(x) - min(x))*(1-0)+0)
}
if (threshold != 0) {
  x <- ifelse(x<=threshold,
              Winsorize(x,maxval=lim), #normal linear
              Winsorize(x,maxval=lim)) # penalty for not meeting the target
} else {
  x <- Winsorize(x, maxval=lim)
}

# normalisation func
targetnorm <- function(x, threshold, penalty, lim, direction, log = FALSE){
  x <- if (lim != 0) {
    Winsorize(x, maxval=lim)
  } else if (log==T) {
    
  }
  else if (direction == 1) {
    return (minmaxNORM(x))
  } else {
    return (minmaxNORM(-x))
    }
}
#vis different standarisation methods
sa1_all |>
  ggplot() +
  geom_density(aes((fatalcrashes_per)))

sa1_all |>
  ggplot() +
  geom_density(aes(Winsorize((fatalcrashes_per), minval=-0, maxval = 0.00005)))

sa1_all_test <- sa1_all |> 
  mutate(fatalcrashes_permeas = minmaxNORM(Winsorize(log10(fatalcrashes_per), maxval = -4, minval = -6)))
ggplot(sa1_all_test) +
  geom_density(aes(fatalcrashes_permeas))

##### Normalaise variables #####
sa1_all_index <- sa1_all |> 
  mutate(popdens1 = minmaxNORM(Winsorize(log10(popdens), maxval = -2, minval = -2.7))) |>
  mutate(housedens1 = minmaxNORM(Winsorize(log10(no_households/area.x), maxval = -2, minval = -4))) |> 
  mutate(damp1 = minmaxNORM(-dampness)) |>
  mutate(diversity1 = minmaxNORM(shannon)) |>
  mutate(crime1 = minmaxNORM(-Winsorize((crime_perarea), maxval = 0.0030, minval = 0)))
  mutate()
  
  
  
  
  
sa1_all_index <- sa1_all |> 
  mutate(dist_stations = targetnorm(dist_stations, threshold=0, penalty=0, lim=20000, direction=0))
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

