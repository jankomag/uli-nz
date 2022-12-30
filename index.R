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
  geom_density(aes(dist_petrol))
sa1_all |>
  ggplot() +
  geom_density(aes(minmaxNORM(-Winsorize(dist_evs, maxval = 5000))))

sa1_all_test <- sa1_all |> 
  mutate(fatalcrashes_permeas = minmaxNORM(Winsorize(log10(fatalcrashes_per), maxval = -4, minval = -6)))
ggplot(sa1_all_test) +
  geom_density(aes(fatalcrashes_permeas))

##### Normalaise variables #####
sa1_all_index <- sa1_all |> 
  mutate(popdens1 = minmaxNORM(Winsorize(log10(popdens.x), maxval = -2, minval = -2.7))) |>
  mutate(housedens1 = minmaxNORM(Winsorize(log10(no_households/area), maxval = -2, minval = -4))) |> 
  mutate(damp1 = minmaxNORM(-dampness)) |>
  mutate(diversity1 = minmaxNORM(shannon)) |>
  mutate(crime1 = minmaxNORM(-Winsorize((crime_perarea), maxval = 0.0030, minval = 0))) |> 
  mutate(crashes1 = minmaxNORM(-Winsorize(crashesperarea.x, minval=0, maxval = 0.001))) |> 
  mutate(flood1 = minmaxNORM(-Winsorize(floodprone_prc, minval=0, maxval = 0.5))) |> 
  mutate(alcohol1 = alcoprohibited) |> 
  mutate(trstation1 = minmaxNORM(-Winsorize(dist_stations, maxval = 50000))) |> 
  mutate(bustop1 = minmaxNORM(-Winsorize(dist_stations, maxval = 5000))) |> 
  mutate(marae1 = minmaxNORM(-dist_marae)) |> 
  mutate(cinema1 = minmaxNORM(-Winsorize(dist_cinema, maxval = 20000))) |> 
  mutate(galler1 = minmaxNORM(-Winsorize(dist_galleries, maxval = 30000))) |> 
  mutate(library1 = minmaxNORM(-Winsorize(dist_libraries, maxval = 7000))) |> 
  mutate(museum1 = minmaxNORM(-Winsorize(dist_museums, maxval = 25000))) |> 
  mutate(theatre1 = minmaxNORM(-Winsorize(dist_theatre, maxval = 20000))) |> 
  mutate(chemist1 = minmaxNORM(-Winsorize(dist_chemist, maxval = 20000))) |> 
  mutate(dentist1 = minmaxNORM(-Winsorize(dist_dentist, maxval = 5000))) |> 
  mutate(healthcr1 = minmaxNORM(-Winsorize(dist_healthcentre, maxval = 6000))) |> 
  mutate(hospital1 = minmaxNORM(-Winsorize(dist_hospital, maxval = 15000))) |>
  mutate(childcare1 = minmaxNORM(-Winsorize(dist_childcare, maxval = 10000))) |> 
  mutate(sport1 = minmaxNORM(-Winsorize(dist_sport, maxval = 5000))) |>
  mutate(convstor1 = minmaxNORM(-Winsorize(dist_conveniencestore, maxval = 2000))) |>
  mutate(supermarket1 = minmaxNORM(-Winsorize(dist_supermarket, maxval = 5000))) |>
  mutate(secondary1 = minmaxNORM(-Winsorize(dist_secondary, maxval = 7000))) |>
  mutate(primary1 = minmaxNORM(-Winsorize(dist_primary, maxval = 3000))) |>
  mutate(petrol1 = minmaxNORM(-Winsorize(dist_petrol, maxval = 5000))) |>
  mutate(evch1 = minmaxNORM(-Winsorize(dist_evs, maxval = 5000)))
  
sa1_all_index <- sa1_all_index |> 
  mutate(kuli = popdens1 + housedens1 + damp1 + diversity1 +
           crime1 + crashes1 + flood1 +
           alcohol1 + trstation1 + bustop1 + marae1 + cinema1 +
           galler1 +  library1 + museum1 + theatre1 +
           chemist1 + dentist1 + healthcr1 + hospital1 + childcare1 +
           sport1 + convstor1 + supermarket1 + secondary1 + primary1 +
           petrol1 + evch1) |> 
  mutate(kuli_norm = minmaxNORM(kuli))

# rejoin with geometry
index_sa1g <- left_join(sa1_allg, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))

tmap_mode("view")
tm_shape(index_sa1g) +
  tm_polygons(col = "kuli_norm", style = "fixed", lwd=0.1,
          breaks = c(0,.2,.3,.5,.7,.8,.9,.95,.99,1))
          #palette = "Reds", title = str_glue('Penalty= {penalty}'))
#st_write(grid_normed, "data/geographic/grids/grid_with_kuli.gpkg")

