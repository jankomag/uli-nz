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

# test plotting
tm_shape(grid) +
  tm_dots(col="cinemas_dist",
          style = "kmeans", palette = "Reds")

#### EDA ####
summary(grid)
# strip from geography for EDA
grid_df <- grid %>% st_drop_geometry()

# inspecting correlations
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
  gather(-income, key = "var", value = "value") %>% 
  ggplot(aes(x = income, y = value)) +
  facet_wrap(~ var, scales = "free") +
  geom_point(alpha=0.2) +
  theme_bw() +
  geom_smooth(method="lm")

#### Index Construcion ####

