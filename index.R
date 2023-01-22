# load packages
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
library(MASS)
library(gstat)

#### Data imports ####
# geographic data
sa1_allold <- st_read("data/geographic/sa1_allvars.gpkg") |> st_transform(27291) |> st_drop_geometry() #transforming to the same coordinate system
sa1_boundry <- st_read("data/geographic/sa1_auckland_waiheke_urban_new_final.gpkg") |> st_transform(27291) |> subset(select = c(SA12018_V1_00, area.x.x)) #transforming to the same coordinate system
sa1_allg <- left_join(sa1_boundry, sa1_allold, by = c("SA12018_V1_00"="SA12018_V1_00"))

summary(sa1_allg)
# strip from geography for EDA
sa1_all <- sa1_allg |> st_drop_geometry()

#### EDA ####
##### BoxCox with ChatGPT code ####
box_cox_transformerold <- function(dataframe, column_name) {
  #dataframe <- deparse(substitute(dataframe))
  model <- lm(as.formula(paste(column_name, "~ 1")), data = dataframe)
  b <- MASS::boxcox(model)
  return(b)
  #lambda <- b$x[which.max(b$y)] # Determine the optimal lambda value for the Box-Cox transformation
  # Perform the Box-Cox transformation
  #return ((dataframe[, column_name] ^ lambda - 1) / lambda)
}
create_model <- function(dataframe, column_name) {
  model <- lm(as.formula(paste(column_name, "~ 1")), data = dataframe)
  return(model)
}
box_cox_transformer <- function(dataframe, column_name) {
  #odel <- create_model(dataframe, column_name)
  model <- lm(as.formula(paste(column_name, "~ 1")), data = dataframe)
  
  MASS::boxcox(model$residuals)
  #lambda <- b$x[which.max(b$y)] # Determine the optimal lambda value for the Box-Cox transformation
  # Perform the Box-Cox transformation
  #return ((dataframe[,column_name] ^ lambda - 1) / lambda)
}
box_cox_transformer(sa1_all, "testvar")
sa1_all$testvar = as.numeric(sa1_all$dist_dentist + 1)

histogram(sa1_all$dist_dentist, bins=100)

transformed <- 3
histogram(sa1_all$dist_dentist, bins=100)


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

##### Examine Distributions of all variables #####
my_plots <- lapply(names(sa1_all), function(var_x){
  p <- ggplot(sa1_all) +
    aes_string(var_x)
  if(is.numeric(sa1_all[[var_x]])) {
    p <- p + geom_density()
  } else {
    p <- p + geom_bar()
  } 
})
plot_grid(plotlist = my_plots)

#### Index Construcion ####
# min-max normalise function 
minmaxNORM <- function(x) {
  return (((x - min(x))) / (max(x) - min(x))*(10-1)+1)
} #1-10
minmaxNORM01 <- function(x) {
  return (((x - min(x))) / (max(x) - min(x))*(1-0)+0)
}
minmaxNORM1max <- function(x) {
  return (((x - min(x))) / (max(x) - min(x))*(max(x)-0.0001)+0.0001)
}

# testing box cox transformation - no function
b <- boxcox(lm(((dist_childcare+0.1)) ~ 1, data=sa1_all))
lambda <- b$x[which.max(b$y)]
sa1_alltest <- sa1_all |> 
  mutate(testvar = ((dist_childcare+0.1) ^ lambda - 1) / lambda)
sa1_all |>
  ggplot() +
  geom_histogram(aes(dist_childcare+0.1), bins=1000)
sa1_alltest |>
  ggplot() +
  geom_histogram(aes((testvar)), bins=400)

sa1_all |>
  ggplot() +
  geom_histogram(aes(log(medianRent+0.1)), bins=400)
sa1_all |>
  ggplot() +
  geom_histogram(aes(Winsorize((dist_beach+0.1), minval=0, maxval=10000)), bins=400)


lambdaFlood <- -0.06060606
lambdahealth <- 0.3434343
lambdachildcare <- 0.06060606
lambdasport <- 0.3838384
lambdasecond <- 0.3434343
lambdaprimary <- 0.4242424
lambdapetrol <- 0.4242424
lambdaev <- 0.6262626
lambdapubs <- 0.3434343
lambdabike <- -2
lambdarent <- 1.151515
##### Custom Transformation of each variable ##### original no box-cox
sa1_all_index <- sa1_all |> 
  mutate(househdens = no_households/area) |> 
  mutate(rentBC = ((medianRent+0.1) ^ lambdarent - 1) / lambdarent) |> 
  mutate(floodBC = ((floodprone_prc+1) ^ lambdaFlood - 1) / lambdaFlood) |> 
  mutate(healthcBC = ((dist_healthcentre+0.1) ^ lambdahealth - 1) / lambdahealth) |> 
  mutate(childcareBC = ((dist_childcare+0.1) ^ lambdachildcare - 1) / lambdachildcare) |> 
  mutate(petrolBC = ((dist_petrol+0.1) ^ lambdapetrol - 1) / lambdapetrol) |> 
  mutate(evchBC = ((dist_evs+0.1) ^ lambdaev - 1) / lambdaev) |> 
  mutate(pubBC = ((dist_pubs+0.1) ^ lambdapubs - 1) / lambdapubs) |> 
  mutate(sportBC = ((dist_sport+0.1) ^ lambdasport - 1) / lambdasport) |> 
  mutate(secondaryBC = ((dist_secondary+0.1) ^ lambdasecond - 1) / lambdasecond) |> 
  mutate(primaryBC = ((dist_primary+0.1) ^ lambdaprimary - 1) / lambdaprimary) |> 
  mutate(bikeBC = ((bikeperarea+0.1) ^ lambdabike - 1) / lambdabike) |> 
  mutate(petrol1 = minmaxNORM(-Winsorize(petrolBC, minval=1, maxval = 80))) |>
  mutate(evch1 = minmaxNORM(-(evchBC))) |> 
  mutate(housedens1 = minmaxNORM(Winsorize(log10(househdens), maxval = -2, minval = -4))) |> 
  mutate(popdens1 = minmaxNORM(Winsorize(log10(popdens), maxval = -1, minval = -4))) |>
  mutate(damp1 = minmaxNORM(-Winsorize(dampness, maxval = 0.5, minval = 0))) |>
  mutate(diversity1 = minmaxNORM(shannon)) |>
  mutate(crime1 = minmaxNORM(-Winsorize(log(crime_perarea), minval=-11, maxval=-5))) |> 
  mutate(crashes1 = minmaxNORM(Winsorize(log(dist_crash), minval=4, maxval=10))) |> 
  mutate(flood1 = minmaxNORM(-Winsorize(floodBC, minval=-6, maxval=1))) |> 
  mutate(alcohol1 = minmaxNORM(alcoprohibited)) |> 
  mutate(station1 = minmaxNORM(-Winsorize(log(dist_stations), minval=5, maxval=11))) |> 
  mutate(bustop1 = minmaxNORM(-Winsorize(dist_busstops, minval=0, maxval = 1000))) |> 
  mutate(freqbusstop1 = minmaxNORM(-Winsorize(dist_busstopsfreq, minval=0, maxval= 3500))) |> 
  mutate(marae1 = minmaxNORM(-dist_marae)) |> 
  mutate(cinema1 = minmaxNORM(-Winsorize(log(dist_cinema), minval=6, maxval=max(log(dist_cinema))))) |> 
  mutate(gallery1 = minmaxNORM(-Winsorize(log(dist_galleries), minval=6, maxval=max(log(dist_galleries))))) |> 
  mutate(library1 = minmaxNORM(-Winsorize(log(dist_libraries), minval=5.3, maxval=max(log(dist_libraries))))) |> 
  mutate(museum1 = minmaxNORM(-Winsorize(dist_museums, minval=0, maxval=18000))) |> 
  mutate(theatre1 = minmaxNORM(-Winsorize(dist_theatre, minval=0, maxval=11000))) |> 
  mutate(chemist1 = minmaxNORM(-Winsorize(log(Winsorize(dist_chemist, minval=0, maxval=20000)),minval=6.2, maxval=10))) |> 
  mutate(dentist1 = minmaxNORM(-Winsorize(log(dist_dentist), minval=4.8, maxval=9))) |> 
  mutate(healthcr1 = minmaxNORM(-Winsorize(healthcBC, minval=0, maxval = 58))) |> 
  mutate(hospital1 = minmaxNORM(-Winsorize(dist_hospital,minval=0, maxval=11000))) |>
  mutate(childcare1 = minmaxNORM(-Winsorize(childcareBC, minval=4.5, maxval = 12))) |> 
  mutate(sport1 = minmaxNORM(-Winsorize((sportBC), minval=0, maxval=60))) |>
  mutate(convstor1 = minmaxNORM(-Winsorize(log(dist_conveniencestore+0.1), minval=3, maxval=10))) |>
  mutate(supermarket1 = minmaxNORM(-Winsorize(log(dist_supermarket+0.1), minval=4.5, maxval=10))) |>
  mutate(secondary1 = minmaxNORM(-secondaryBC)) |>
  mutate(primary1 = minmaxNORM(-Winsorize(primaryBC,minval=2,maxval=70))) |>
  mutate(strconnectivity1 = minmaxNORM(Winsorize(log(str_connectivity+0.1), minval=-3, maxval=-2.297))) |> 
  mutate(bigpark1 = minmaxNORM(-Winsorize(dist_bigpark, minval=0, maxval=1700))) |> 
  mutate(smallpark1 = minmaxNORM(-Winsorize(dist_smallpark, minval=0, maxval=1500))) |> 
  mutate(cafe1 = minmaxNORM(-Winsorize(dist_cafe, minval=0, maxval=3000))) |> 
  mutate(restaurant1 = minmaxNORM(-Winsorize(dist_restaurants, minval=0, maxval=4000))) |> 
  mutate(pub1 = minmaxNORM(-pubBC)) |> 
  mutate(bbq1 = minmaxNORM(-Winsorize(log(dist_bbq+0.1), minval=5.5, maxval=10))) |> 
  mutate(bikeability1 = minmaxNORM(Winsorize(bikeBC, minval=-50, maxval = -25))) |> 
  mutate(gym1 = minmaxNORM(-Winsorize(log(dist_gym+0.1), minval=4.5, maxval=10))) |> 
  mutate(beach1 = minmaxNORM(-Winsorize(dist_beach, minval=0, maxval=10000))) |> 
  mutate(affordability1 = minmaxNORM(-Winsorize(rentBC, minval=0, maxval=2500))) |> 
  mutate(carInfrastructure1 = minmaxNORM(evch1 + petrol1))

##### Final Index Construction ####
sa1_all_index <- sa1_all_index |> 
  mutate(walkability2 = minmaxNORM(popdens1 + housedens1 +convstor1 + supermarket1 + strconnectivity1)) |> 
  mutate(medical2 = minmaxNORM(chemist1 + dentist1 + healthcr1 + hospital1)) |> 
  mutate(education2 = minmaxNORM(secondary1 + primary1 + childcare1)) |> 
  mutate(safety2 = minmaxNORM(crime1 + crashes1 + flood1 + alcohol1)) |> 
  mutate(transport2 = minmaxNORM(station1 + bustop1 +freqbusstop1)) |> 
  mutate(culture2 = minmaxNORM(diversity1+marae1)) |> 
  mutate(leisure2 = minmaxNORM(cinema1 + gallery1 +  library1 + museum1 + theatre1 + sport1 + gym1)) |> 
  mutate(food2 = minmaxNORM(cafe1 + restaurant1 +  pub1 + bbq1)) |> 
  mutate(greenspace2 = minmaxNORM(bigpark1 + smallpark1 + beach1)) |> 
  mutate(housing2 = affordability1 + damp1) |> 
  mutate(kuli_subs = minmaxNORM(carInfrastructure1+bikeability1+walkability2+greenspace2+leisure2+medical2 +culture2+education2 +transport2+ safety2 + food2 + housing2)) |> 
  mutate(kuli = popdens1 + housedens1 +convstor1 + supermarket1 + strconnectivity1 +
           chemist1 + dentist1 + healthcr1 + hospital1 +
           secondary1 + primary1 + childcare1 +
           minmaxNORM01(crime1*2) + crashes1 + flood1 + alcohol1 +
           station1 + bustop1 +freqbusstop1 +
           diversity1+marae1 +
           cinema1 + gallery1 + library1 + museum1 + theatre1 + sport1 + gym1 +
           cafe1 + restaurant1 +  pub1 + bbq1 +
           bigpark1 + smallpark1 + beach1 +
           affordability1 + damp1+
           carInfrastructure1 + bikeability1) |> 
  mutate(arithm_kuli = minmaxNORM01(kuli/39)) |> 
  mutate(kuli_norm = minmaxNORM01(kuli))

#Geometric Mean
a_gmean <- function(x, w = NULL){
  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    #message("No weights specified for geometric mean, using equal weights.")
  }
  if(any(!is.na(x))){
    if(any((x <= 0), na.rm = TRUE)){
      stop("Negative or zero values found when applying geometric mean. This doesn't work because geometric
         mean uses log. Normalise to remove negative/zero values first or use another aggregation method.")}
    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA
    # calculate geom mean
    gm <- exp( sum(w * log(x), na.rm = TRUE)/sum(w, na.rm = TRUE) )
  } else {
    gm <- NA
  }
  gm
}
sa1_all_index$geom_kuli <- apply(sa1_all_index[,65:103], 1, FUN = a_gmean) #103
sa1_all_index$geom_kuli <- minmaxNORM01(sa1_all_index$geom_kuli)

# Evaluate the transformation method of each indicator
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
    geom_histogram(aes(sa1_all_index[[xpre]]), bins=70) + theme_publish() +
    xlab(varN) + ylab("Density") + ggtitle("Pre Transformation") +
    theme(plot.title = element_text(hjust = 0.5))
  post_out = ggplot() +
    geom_histogram(aes(sa1_all_index[[xpost]]), bins=70) + theme_publish() +
    xlab(varN) + ylab("Density") + ggtitle("Post Transformation") +
    theme(plot.title = element_text(hjust = 0.5))
  grid.arrange(pre_out, post_out, ncol=2)
}
densityplot(dist_stations, station1, "Train Station")
densityplot(dist_busstops, bustop1, "Bus Stop")
densityplot(dist_busstopsfreq,freqbusstop1, "Frequent Buses")
densityplot(popdens, popdens1, "Pop Density")
densityplot(househdens, housedens1, "House Density")
densityplot(dampness, damp1, "Dampness")
densityplot(shannon, diversity1, "Shannon Index")
densityplot(crime_perarea, crime1, "Crime")
densityplot(dist_crash, crashes1, "Crashes")
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
densityplot(dist_cafe, cafe1, "Cafe")
densityplot(dist_restaurants, restaurant1, "Restaurant")
densityplot(dist_pubs, pub1, "Pub")
densityplot(dist_bbq, bbq1, "BBQ")
densityplot(bikeperarea, bikeability1, "Bikeability")
densityplot(dist_gym, gym1, "Gym")
densityplot(dist_beach, beach1, "Beach")
densityplot(medianRent, affordability1, "Affordability")

sa1_all_index |>
  ggplot() +
  geom_histogram(aes(geom_kuli), bins=500)

# rejoin with geometry
index_sa1g <- left_join(sa1_allg, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))
#walkability1 other1 greenspace1 leisure1 medical1 culture1 education1pt1 safety1
tmap_mode("plot")
tm_shape(index_sa1g) +
  tm_polygons(col = c("geom_kuli","kuli_norm","arithm_kuli"), palette = "Reds", style = "kmeans", lwd=0)
          #breaks = c(0,.1,.3,.6,.7,.8,.95,1))#, title = str_glue('Penalty= {penalty}'))
st_write(index_sa1g, "data/geographic/sa1_kuli_all.gpkg")


#### Other ####
# lambdas
#Common maximum value for tranformations
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

# testing table
colname <- c("Variable","Skewness", "Kurtosis")
SmallPark <- c("Small Park", skewness(sa1_all_index$dist_smallpark), skewness(sa1_all_index$smallpark1))
BigPark <- c("Big Park", skewness(sa1_all_index$dist_bigpark), skewness(sa1_all_index$bigpark1))
table <- rbind(colname, SmallPark, BigPark)
stargazer(table, type="text")
