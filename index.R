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
library(moments)
library(envalysis)
require(gridExtra)
library(stargazer)

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

sa1_all |>
  ggplot() +
  geom_density(aes(log10(popdens)))

sa1_alltest <- sa1_all |> 
  mutate(popdens1t = minmaxNORM(Winsorize(log10(popdens), minval = -3, maxval=-2)))

skewness(sa1_alltest$popdens1t)
kurtosis(sa1_alltest$popdens1t)

##### Common Normalaise variables #####
commonMaxval = 5000
sa1_all_index_commonmax <- sa1_all |> 
  mutate(popdens1 = minmaxNORM(Winsorize(log10(popdens), maxval = -2, minval = -2.7))) |>
  mutate(housedens1 = minmaxNORM(Winsorize(log10(no_households/area), maxval = -2, minval = -4))) |> 
  mutate(damp1 = minmaxNORM(-dampness)) |>
  mutate(diversity1 = minmaxNORM(shannon)) |>
  mutate(crime1 = minmaxNORM(-Winsorize((crime_perarea), maxval = 0.0030, minval = 0))) |> 
  mutate(crashes1 = minmaxNORM(-Winsorize(crashesperarea, minval=0, maxval = 0.001))) |> 
  mutate(flood1 = minmaxNORM(-Winsorize(floodprone_prc, minval=0, maxval = 0.5))) |> 
  mutate(alcohol1 = alcoprohibited) |> 
  mutate(station1 = minmaxNORM(-Winsorize(dist_stations, maxval = commonMaxval))) |> 
  mutate(bustop1 = minmaxNORM(-Winsorize(dist_busstops, maxval = commonMaxval))) |> 
  mutate(freqbusstop1 = minmaxNORM(-Winsorize(dist_busstopsfreq, maxval= commonMaxval))) |> 
  mutate(marae1 = minmaxNORM(-Winsorize(dist_marae, maxval=commonMaxval))) |> 
  mutate(cinema1 = minmaxNORM(-Winsorize(dist_cinema, maxval = commonMaxval))) |> 
  mutate(gallery1 = minmaxNORM(-Winsorize(dist_galleries, maxval = commonMaxval))) |> 
  mutate(library1 = minmaxNORM(-Winsorize(dist_libraries, maxval = commonMaxval))) |> 
  mutate(museum1 = minmaxNORM(-Winsorize(dist_museums, maxval = commonMaxval))) |> 
  mutate(theatre1 = minmaxNORM(-Winsorize(dist_theatre, maxval = commonMaxval))) |> 
  mutate(chemist1 = minmaxNORM(-Winsorize(dist_chemist, maxval = commonMaxval))) |> 
  mutate(dentist1 = minmaxNORM(-Winsorize(dist_dentist, maxval = commonMaxval))) |> 
  mutate(healthcr1 = minmaxNORM(-Winsorize(dist_healthcentre, maxval = commonMaxval))) |> 
  mutate(hospital1 = minmaxNORM(-Winsorize(dist_hospital, maxval = commonMaxval))) |>
  mutate(childcare1 = minmaxNORM(-Winsorize(dist_childcare, maxval = commonMaxval))) |> 
  mutate(sport1 = minmaxNORM(-Winsorize(dist_sport, maxval = commonMaxval))) |>
  mutate(convstor1 = minmaxNORM(-Winsorize(dist_conveniencestore, maxval = commonMaxval))) |>
  mutate(supermarket1 = minmaxNORM(-Winsorize(dist_supermarket, maxval = commonMaxval))) |>
  mutate(secondary1 = minmaxNORM(-Winsorize(dist_secondary, maxval = commonMaxval))) |>
  mutate(primary1 = minmaxNORM(-Winsorize(dist_primary, maxval = commonMaxval))) |>
  mutate(petrol1 = minmaxNORM(-Winsorize(dist_petrol, maxval = commonMaxval))) |>
  mutate(evch1 = minmaxNORM(-Winsorize(dist_evs, maxval = commonMaxval))) |> 
  mutate(strconnectivity1 = minmaxNORM(Winsorize(str_connectivity, maxval = 0.0003))) |> 
  mutate(bigpark1 = minmaxNORM(-Winsorize(dist_bigpark, minval=50, maxval = commonMaxval))) |> 
  mutate(smallpark1 = minmaxNORM(-Winsorize(dist_smallpark, minval=50, maxval = commonMaxval)))
#add reward parameters
reward=1
sa1_all_index_commonmax$station1[sa1_all_index$dist_stations < 1000] <- sa1_all_index_commonmax$station1[sa1_all_index$dist_stations < 1000] + reward
sa1_all_index_commonmax$freqbusstop1[sa1_all_index$dist_busstopsfreq < 400] <- sa1_all_index_commonmax$freqbusstop1[sa1_all_index$dist_busstopsfreq < 400] + reward
sa1_all_index_commonmax$bigpark1[sa1_all_index$dist_bigpark < 1000] <- sa1_all_index_commonmax$bigpark1[sa1_all_index$dist_bigpark < 1000] + reward
sa1_all_index_commonmax$smallpark1[sa1_all_index$dist_smallpark < 300] <- sa1_all_index_commonmax$smallpark1[sa1_all_index$dist_smallpark < 300] + reward

##### Custom Normalaise variables #####
sa1_all_index <- sa1_all |> 
  mutate(popdens1 = minmaxNORM(Winsorize(log10(popdens), maxval = -2, minval = -3))) |>
  mutate(housedens1 = minmaxNORM(Winsorize(log10(no_households/area), maxval = -2, minval = -4))) |> 
  mutate(damp1 = minmaxNORM(-dampness)) |>
  mutate(diversity1 = minmaxNORM(shannon)) |>
  mutate(crime1 = minmaxNORM(-Winsorize(crime_perarea, maxval = 0.002, minval = 0))) |> 
  mutate(crashes1 = minmaxNORM(-Winsorize(crashesperarea, minval=0, maxval = 0.001))) |> 
  mutate(flood1 = minmaxNORM(-Winsorize(floodprone_prc, minval=0, maxval = 0.5))) |> 
  mutate(alcohol1 = alcoprohibited) |> 
  mutate(station1 = minmaxNORM(-Winsorize(dist_stations, maxval = 50000))) |> 
  mutate(bustop1 = minmaxNORM(-Winsorize(dist_busstops, maxval = 1000))) |> 
  mutate(freqbusstop1 = minmaxNORM(-Winsorize(dist_busstopsfreq, maxval= 1500))) |> 
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
  mutate(primary1 = minmaxNORM(-Winsorize(dist_primary, maxval = 2000))) |>
  mutate(petrol1 = minmaxNORM(-Winsorize(dist_petrol, maxval = 5000))) |>
  mutate(evch1 = minmaxNORM(-Winsorize(dist_evs, maxval = 10000))) |> 
  mutate(strconnectivity1 = minmaxNORM(Winsorize(str_connectivity, maxval = 0.0005))) |> 
  mutate(bigpark1 = minmaxNORM(-Winsorize(dist_bigpark, minval=50, maxval = 1000))) |> 
  mutate(smallpark1 = minmaxNORM(-Winsorize(dist_smallpark, minval=50, maxval = 1000)))

#add reward parameters
#reward=1
#sa1_all_index$station1[sa1_all_index$dist_stations < 1000] <- sa1_all_index$station1[sa1_all_index$dist_stations < 1000] + reward
#sa1_all_index$freqbusstop1[sa1_all_index$dist_busstopsfreq < 400] <- sa1_all_index$freqbusstop1[sa1_all_index$dist_busstopsfreq < 400] + reward
#sa1_all_index$bigpark1[sa1_all_index$dist_bigpark < 1000] <- sa1_all_index$bigpark1[sa1_all_index$dist_bigpark < 1000] + reward
#sa1_all_index$smallpark1[sa1_all_index$dist_smallpark < 300] <- sa1_all_index$smallpark1[sa1_all_index$dist_smallpark < 300] + reward

#### Final Index Construction ####
sa1_all_index <- sa1_all_index |> 
  mutate(walkability1 = minmaxNORM(popdens1 + housedens1 +convstor1 + supermarket1 +strconnectivity1)) |> 
  mutate(medical1 = minmaxNORM(chemist1 + dentist1 + healthcr1 + hospital1)) |> 
  mutate(education1 = minmaxNORM(secondary1 + primary1 + childcare1)) |> 
  mutate(safety1 = minmaxNORM(2*crime1 + crashes1 + flood1 + alcohol1)) |> 
  mutate(pt1 = minmaxNORM(station1 + bustop1 +freqbusstop1)) |> 
  mutate(culture1 = minmaxNORM(diversity1 +marae1)) |> 
  mutate(leisure1 = minmaxNORM(cinema1 + gallery1 +  library1 + museum1 + theatre1 + sport1)) |> 
  mutate(greenspace1 = minmaxNORM(bigpark1 + smallpark1)) |> 
  mutate(other1 = minmaxNORM(damp1 + petrol1 + evch1)) |> 
  mutate(kuli_subs = minmaxNORM(walkability1+other1+greenspace1+leisure1+medical1 +culture1+education1 +pt1+ safety1)) |> 
  mutate(kuli = popdens1 + housedens1 + damp1 + diversity1 + 
           2*crime1 + crashes1 + flood1 + freqbusstop1 +
           alcohol1 + station1 + bustop1 + marae1 + cinema1 +
           gallery1 +  library1 + museum1 + theatre1 +
           chemist1 + dentist1 + healthcr1 + hospital1 + childcare1 +
           sport1 + convstor1 + supermarket1 + secondary1 + primary1 +
           petrol1 + evch1 + strconnectivity1 + bigpark1 + smallpark1) |> 
  mutate(kuli_norm = minmaxNORM(kuli_subs))

#vis transformation methods
densityplot = function(xpre, xpost, varN) {
  xpre <- deparse(substitute(xpre))
  xpost <- deparse(substitute(xpost))
  varN <- toString(varN)
  Data <- c("Raw","Transformed")
  Skewness <- c(round(skewness(sa1_all_index[[xpre]]),3), round(skewness(sa1_all_index[[xpost]]),3))
  Kurtosis <- c(round(kurtosis(sa1_all_index[[xpre]]),3), round(kurtosis(sa1_all_index[[xpost]]),3))
  df <- data.frame(Data, Skewness, Kurtosis)
  print(df)
  
  
  pre_out = ggplot() +
    geom_density(aes(sa1_all_index[[xpre]])) + theme_publish() +
    xlab(varN) + ylab("Density") + ggtitle("Pre Transformation") +
    theme(plot.title = element_text(hjust = 0.5))
  post_out = ggplot() +
    geom_density(aes(sa1_all_index[[xpost]])) + theme_publish() +
    xlab(varN) + ylab("Density") + ggtitle("Post Transformation") +
    theme(plot.title = element_text(hjust = 0.5))
  grid.arrange(pre_out, post_out, ncol=2)
}
densityplot(crime_perarea, crime1, "Crime")
densityplot(dist_stations, station1, "Train Station")
densityplot(dist_busstops, bustop1, "Bus Stop")
densityplot(dist_busstopsfreq,freqbusstop1, "Frequent Buses")
densityplot(popdens, popdens1, "Pop Density")
densityplot(no_households/area, housedens1, "House Density")
densityplot(dampness, damp1, "Dampness")
densityplot(shannon, diversity1, "Shannon Index")
densityplot(crashesperarea, crashes1, "Crashes")
densityplot(floodprone_prc,flood1, "Floods")
densityplot(alcoprohibited,alcohol1, "Alcohol Prohibited")
densityplot(dist_marae,marae1, "Marae")
densityplot(dist_cinema,cinema1, "Cinema")
densityplot(dist_galleries, gallery1,"Gallery")
densityplot(dist_libraries,library1, "Library")
densityplot(dist_museums,museum1, "Museum")
densityplot(dist_theatre,theatre1, "Theatre")
densityplot(dist_chemist, chemist1, "Chemist")
densityplot(dist_dentist, dentist1, "Dentist")
densityplot(dist_healthcentre, healthcr1, "Healthcentre")
densityplot(dist_hospital, hospital1, "Hospital")
densityplot(dist_childcare, childcare1, "Childcare")
densityplot(dist_sport, sport1, "Sport Facilities")
densityplot(dist_conveniencestore, convstor1, "Convenience Store")
densityplot(dist_supermarket, supermarket1, "Supermarket")
densityplot(dist_secondary, secondary1, "Secondary")
densityplot(dist_primary, primary1 , "Primary")
densityplot(dist_petrol, petrol1, "Petrol")
densityplot(dist_evs, evch1, "EVs")
densityplot(str_connectivity, strconnectivity1, "Street Connectivity")
densityplot(dist_bigpark, bigpark1, "Big Park")
densityplot(dist_smallpark, smallpark1, "Small Park")

# testing table
colname <- c("Variable","Skewness", "Kurtosis")
SmallPark <- c("Small Park", skewness(sa1_all_index$dist_smallpark), skewness(sa1_all_index$smallpark1))
BigPark <- c("Big Park",skewness(sa1_all_index$dist_bigpark), skewness(sa1_all_index$bigpark1))
table <- rbind(colname, SmallPark, BigPark)
stargazer(table, type="text")

Transf <- function(xpre, xpost) {
  xpre <- deparse(substitute(xpre))
  xpost <- deparse(substitute(xpost))
  Data <- c("Raw","Transformed")
  Skewness <- c(skewness(sa1_all_index[[xpre]]), skewness(sa1_all_index[[xpost]]))
  Kurtosis <- c(kurtosis(sa1_all_index[[xpre]]), kurtosis(sa1_all_index[[xpost]]))
  df <- data.frame(Data, Skewness, Kurtosis)
  print(df)
  par(mfrow=c(1,2))
  hist(sa1_all_index[[xpre]])
  hist(sa1_all_index[[xpost]])
}
Transf(popdens, popdens1)


# rejoin with geometry
index_sa1g <- left_join(sa1_allg, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))

tmap_mode("plot")
tm_shape(index_sa1g) +
  tm_polygons(col = "strconnectivity1", palette = "Reds", style = "kmeans", lwd=0)
          #breaks = c(0,.35,.5,.75,.85,.9,.99,1), )#, title = str_glue('Penalty= {penalty}'))
st_write(index_sa1g, "data/geographic/sa1_kuli_5kmax.gpkg")

