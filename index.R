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

#normalise columns function
normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalise each column
grid_df_n <- data.frame(grid_df[22], lapply(grid_df[1:21], normalise))

# rejoin with geometry
grid_geom = subset(grid, select = c(id, geom))
grid_normed <- left_join(grid_geom, grid_df_n, by = c("id"="id"))

summary(grid_normed)

grid_normed <- grid_normed %>%
  mutate(kuli =  station_dist + bus_dist + ev_dist + conv_st_dist - no_intersections_in_100m +
           petrol_st_dist - biking_len_100m + second_dist + primary_dist + childcare_dist +
           cinemas_dist + galleries_dist + libraries_dist + museum_dist + theatre_dist)

tmap_mode("plot")
tm_shape(grid_normed) +
  tm_dots(col = "kuli", style = "equal", palette = "-Reds") +
  tm_xlab("equal") +
  label(x="equal")

