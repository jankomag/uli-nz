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
library(equatiomatic)


#### Data imports ####
# geographic data
sa1_allg <- st_read("data/geographic/sa1_allvars.gpkg") |> st_transform(27291) #transforming to the same coordinate system
#sa1_allg$id <- 1:nrow(sa1_allg)

#### EDA ####
summary(sa1_allg)
# strip from geography for EDA
sa1_all <- sa1_allg |> st_drop_geometry()
##### Correlations #####
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

testfunc <- function(x, threshold, penalty){
  trans_x = if_else(x<=10,
                    x,
                    x+12*(log10(x/10))) #
  return (x)
}
curve(testfunc, from=1, to=50, xlab="x", ylab="y")

#vis different standarisation methods
sa1_all |>
  ggplot() +
  geom_density(aes(dist_busstopsfreq))
sa1_all |>
  ggplot() +
  geom_density(aes(minmaxNORM(Winsorize(dist_busstopsfreq, maxval = 1000))))

##### Normalaise variables #####
sa1_all_index <- sa1_all |> 
  mutate(popdens1 = minmaxNORM(Winsorize(log10(popdens), maxval = -2, minval = -2.7))) |>
  mutate(housedens1 = minmaxNORM(Winsorize(log10(no_households/area.x), maxval = -2, minval = -4))) |> 
  mutate(damp1 = minmaxNORM(-dampness)) |>
  mutate(diversity1 = minmaxNORM(shannon)) |>
  mutate(crime1 = minmaxNORM(-Winsorize((crime_perarea), maxval = 0.0030, minval = 0))) |> 
  mutate(crashes1 = minmaxNORM(-Winsorize(crashesperarea, minval=0, maxval = 0.001))) |> 
  mutate(flood1 = minmaxNORM(-Winsorize(floodprone_prc, minval=0, maxval = 0.5))) |> 
  mutate(alcohol1 = alcoprohibited) |> 
  mutate(station1 = minmaxNORM(-Winsorize(dist_stations, maxval = 50000))) |> 
  mutate(bustop1 = minmaxNORM(-Winsorize(dist_busstops, maxval = 1000))) |> 
  mutate(freqbusstop1 = minmaxNORM(-Winsorize(dist_busstopsfreq, maxval= 5000))) |> 
  mutate(marae1 = minmaxNORM(-dist_marae)) |> 
  mutate(cinema1 = minmaxNORM(-Winsorize(dist_cinema, maxval = 20000))) |> 
  mutate(gallery1 = minmaxNORM(-Winsorize(dist_galleries, maxval = 30000))) |> 
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
  mutate(evch1 = minmaxNORM(-Winsorize(dist_evs, maxval = 5000))) |> 
  mutate(strconnectivity1 = minmaxNORM(Winsorize(str_connectivity, maxval = 0.0003))) |> 
  mutate(bigpark1 = minmaxNORM(-Winsorize(dist_bigpark, minval=50, maxval = 1000))) |> 
  mutate(smallpark1 = minmaxNORM(-Winsorize(dist_smallpark, minval=50, maxval = 1000)))

reward=1
sa1_all_index$station1[sa1_all_index$dist_stations < 1000] <- sa1_all_index$station1[sa1_all_index$dist_stations < 1000] + reward
sa1_all_index$freqbusstop1[sa1_all_index$dist_busstopsfreq < 400] <- sa1_all_index$freqbusstop1[sa1_all_index$dist_busstopsfreq < 400] + reward
sa1_all_index$bigpark1[sa1_all_index$dist_bigpark < 1000] <- sa1_all_index$bigpark1[sa1_all_index$dist_bigpark < 1000] + reward
sa1_all_index$smallpark1[sa1_all_index$dist_smallpark < 300] <- sa1_all_index$smallpark1[sa1_all_index$dist_smallpark < 300] + reward

sa1_all_index <- sa1_all_index |> 
  mutate(walkability1 = popdens1 + housedens1 +convstor1 + supermarket1 +strconnectivity1) |> 
  mutate(medical1 = chemist1 + dentist1 + healthcr1 + hospital1) |> 
  mutate(education1 = secondary1 + primary1 + childcare1) |> 
  mutate(safety1 = 2*crime1 + crashes1 + flood1 + alcohol1) |> 
  mutate(pt1 = station1 + bustop1 +freqbusstop1) |> 
  mutate(culture1 = diversity1 +marae1) |> 
  mutate(leisure1 = cinema1 + gallery1 +  library1 + museum1 + theatre1 + sport1) |> 
  mutate(greenspace1 = bigpark1 + smallpark1) |> 
  mutate(other1 = damp1 + petrol1 + evch1) |> 
  mutate(kuli_subs = minmaxNORM(walkability1+other1+greenspace1+leisure1+medical1 +culture1+education1 +pt1+ safety1)) |> 
  mutate(kuli = popdens1 + housedens1 + damp1 + diversity1 + 
           2*crime1 + crashes1 + flood1 + freqbusstop1 +
           alcohol1 + station1 + bustop1 + marae1 + cinema1 +
           gallery1 +  library1 + museum1 + theatre1 +
           chemist1 + dentist1 + healthcr1 + hospital1 + childcare1 +
           sport1 + convstor1 + supermarket1 + secondary1 + primary1 +
           petrol1 + evch1 + strconnectivity1 + bigpark1 + smallpark1) |> 
  mutate(kuli_norm = minmaxNORM(kuli))

# rejoin with geometry
index_sa1g <- left_join(sa1_allg, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))

tmap_mode("plot")
tm_shape(index_sa1g) +
  tm_polygons(col = "kuli_subs", style = "fixed", lwd=0,
          breaks = c(0,.35,.5,.75,.85,.9,.99,1), palette = "Reds")#, title = str_glue('Penalty= {penalty}'))
st_write(index_sa1g, "data/geographic/sa1_kuli.gpkg")

