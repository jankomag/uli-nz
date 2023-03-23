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
library(ltm)
library(Compind)

#### Data imports ####
# geographic data
sa1_allold <- st_read("data/geographic/sa1_allvars.gpkg") |> st_transform(27291) |> st_drop_geometry() #transforming to the same coordinate system
sa1_boundry <- st_read("data/geographic/sa1_auckland_waiheke_urban_new_final.gpkg") |> st_transform(27291) |> subset(select = c(SA12018_V1_00)) #transforming to the same coordinate system
sa1_allg <- left_join(sa1_boundry, sa1_allold, by = c("SA12018_V1_00"="SA12018_V1_00"))

summary(sa1_allg)
# strip from geography for EDA
sa1_all <- sa1_allg |> st_drop_geometry()

#### EDA ####
##### Correlations #####
cor <- cor(x = sa1_all[c(3:4,11:35,38:48)], y = sa1_all[c(3:4,11:35,38:48)], use="complete.obs")
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

#### Index Construction ####
# min-max normalise function 
minmaxNORM <- function(x) {
  return (((x - min(x))) / (max(x) - min(x))*(10-1)+1)
} #1-10
minmaxNORM0_10 <- function(x) {
  return (((x - min(x))) / (max(x) - min(x))*(10-0)+0)
} #0-10
minmaxNORM0_1 <- function(x) {
  return (((x - min(x))) / (max(x) - min(x))*(1-0)+0)
} #0-1
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

# testing box cox transformation
b <- boxcox(lm(((dist_emergency+0.0001)) ~ 1, data=sa1_all))
lambda <- b$x[which.max(b$y)]
sa1_alltest <- sa1_all |> 
  mutate(testvar = ((dist_emergency+0.0001) ^ lambda - 1) / lambda) |> 
  mutate(testvar2 = )

# Choosing indicator transformations
sa1_alltest <- sa1_allg |> 
  mutate(raw = dwelldensity_buff200_NUMPOINTS) |> 
  mutate(testvar2 = (Winsorize(raw, minval=0, maxval=1500)))

sa1_alltest |>
  ggplot() +
  geom_histogram(aes(testvar2), bins=50)

tm_shape(sa1_alltest) +
  tm_polygons(c("raw", "testvar2"),lwd=0, style="kmeans", palette="Reds")

# optimised lambda values for chosen variables
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
lambdaemergency <- 0.4646465
##### Custom Transformation of each variable ##### original no box-cox
sa1_all_index <- sa1_all |> 
  # BoxCox prep
  mutate(rentBC = ((medianRent+0.1) ^ lambdarent - 1) / lambdarent) |> 
  mutate(healthcBC = ((dist_healthcentre+0.1) ^ lambdahealth - 1) / lambdahealth) |> 
  mutate(childcareBC = ((dist_childcare+0.1) ^ lambdachildcare - 1) / lambdachildcare) |> 
  mutate(petrolBC = ((dist_petrol+0.1) ^ lambdapetrol - 1) / lambdapetrol) |> 
  mutate(evchBC = ((dist_evs+0.1) ^ lambdaev - 1) / lambdaev) |> 
  mutate(pubBC = ((dist_pubs+0.1) ^ lambdapubs - 1) / lambdapubs) |> 
  mutate(sportBC = ((dist_sport+0.1) ^ lambdasport - 1) / lambdasport) |> 
  mutate(secondaryBC = ((dist_secondary+0.1) ^ lambdasecond - 1) / lambdasecond) |> 
  mutate(bikeBC = ((bikeperarea+0.1) ^ lambdabike - 1) / lambdabike) |> 
  mutate(emergencyBC = ((dist_emergency+0.1) ^ lambdaemergency - 1) / lambdaemergency) |> 
  mutate(petrol1 = minmaxNORM(-Winsorize(petrolBC, minval=1, maxval = 80))) |>
  mutate(evch1 = minmaxNORM(-(evchBC))) |> 
  
  # TRANSPORTATION
  mutate(station1 = minmaxNORM(-Winsorize(log(dist_stations), minval=5, maxval=11))) |> #checked
  mutate(freqbusstop1 = minmaxNORM(-Winsorize(dist_busstopsfreq, minval=0, maxval= 2000))) |> #checked
  mutate(bikeability1 = minmaxNORM(Winsorize(bikeBC, minval=-50, maxval = -25))) |> #checked
  mutate(carInfrastructure1 = minmaxNORM((evch1 + petrol1)/18)) |> #checked
  
  # WALKABILITY
  mutate(convstor1 = minmaxNORM(-Winsorize(log(dist_conveniencestore+0.1), minval=3, maxval=10))) |>#checked 
  mutate(supermarket1 = minmaxNORM(-Winsorize(log(dist_supermarket+0.1), minval=4.5, maxval=10))) |> #checked 
  mutate(strconnectivity1 = minmaxNORM(Winsorize(streetconn, minval=0, maxval=15))) |> #checked
  mutate(housedens1 = minmaxNORM(dwelldensity_buff200_NUMPOINTS)) |> #checked
  
  # LEISURE
  mutate(cinema1 = minmaxNORM(-Winsorize(log(dist_cinema), minval=6, maxval=max(log(dist_cinema))))) |> #checked
  mutate(gym1 = minmaxNORM(-Winsorize(log(dist_gym+0.1), minval=5, maxval=10))) |> #checked
  mutate(theatre1 = minmaxNORM(-Winsorize(dist_theatre, minval=0, maxval=11000))) |> #checked 
  mutate(library1 = minmaxNORM(-Winsorize(log(dist_libraries), minval=5.3, maxval=max(log(dist_libraries))))) |> #checked 
  mutate(museum1 = minmaxNORM(-Winsorize(dist_museums, minval=0, maxval=18000))) |> #checked 
  mutate(gallery1 = minmaxNORM(-Winsorize(log(dist_galleries), minval=6, maxval=max(log(dist_galleries))))) |> #checked
  mutate(sport1 = minmaxNORM(-Winsorize((sportBC), minval=0, maxval=60))) |> #checked 
  
  # HOUSING
  mutate(affordability1 = minmaxNORM(-Winsorize(rentBC, minval=0, maxval=2500))) |> #checked
  mutate(damp1 = minmaxNORM(-Winsorize(dampness, maxval = 0.4, minval = 0))) |> #checked
  
  #SAFETY
  mutate(alcohol1 = minmaxNORM(-alcoprohibited)) |> #checked
  mutate(crime1 = minmaxNORM(-Winsorize(crime_perarea, minval=0, maxval=.0015))) |> #checked
  mutate(crashes1 = minmaxNORM(Winsorize(log(dist_crash), minval=4, maxval=10))) |> 
  mutate(flood1 = minmaxNORM(-Winsorize(floodprone_prc, minval=0, maxval=.125))) |> #checked
  mutate(emergency1 = minmaxNORM(-Winsorize(emergencyBC, minval=0, maxval=90))) |> #checked
  
  #CULTURE
  mutate(diversity1 = minmaxNORM(shannon)) |> #checked
  mutate(marae1 = minmaxNORM(-Winsorize(dist_marae, minval=0, maxval=10000))) |> #checked
  
  #MEDICAL
  mutate(chemist1 = minmaxNORM(-Winsorize(log(dist_chemist), minval=6.2, maxval=10))) |> #checked
  mutate(dentist1 = minmaxNORM(-Winsorize(log(dist_dentist), minval=4.8, maxval=9))) |> #checked 
  mutate(healthcr1 = minmaxNORM(-Winsorize(healthcBC, minval=0, maxval = 58))) |> #checked 
  mutate(hospital1 = minmaxNORM(-Winsorize(dist_hospital,minval=0, maxval=11000))) |> #checked 
  
  #EDUCATION
  mutate(childcare1 = minmaxNORM(-Winsorize(childcareBC, minval=4.5, maxval = 12))) |> #checked 
  mutate(primary1 = minmaxNORM(-Winsorize((dist_primary), minval=200, maxval=2000))) |> #checked
  mutate(secondary1 = minmaxNORM(-secondaryBC)) |> #checked 
  
  #FOOD OUTLETS
  mutate(cafe1 = minmaxNORM(-Winsorize(dist_cafe, minval=0, maxval=3000))) |> #checked
  mutate(restaurant1 = minmaxNORM(-Winsorize(dist_restaurants, minval=0, maxval=4000))) |> #checked
  mutate(pub1 = minmaxNORM(-pubBC)) |> #checked
  mutate(bbq1 = minmaxNORM(-Winsorize(log(dist_bbq+0.1), minval=5.5, maxval=10))) |> #checked
  
  # GREEN SPACE
  mutate(bigpark1 = minmaxNORM(-Winsorize(dist_bigpark, minval=0, maxval=1500))) |> #checked
  mutate(beach1 = minmaxNORM(-Winsorize(dist_beach, minval=0, maxval=10000))) #checked

  # Other Indicators
  #mutate(popdens1 = minmaxNORM(Winsorize(log(popdens+0.0001), maxval = -2, minval = -10))) |>
  #mutate(bustop1 = minmaxNORM(-Winsorize(dist_busstops, minval=0, maxval = 700))) |> #checked
  #mutate(smallpark1 = minmaxNORM(-Winsorize(dist_smallpark, minval=0, maxval=1500))) |> #checked

##### Final Index Construction ####
sa1_all_index <- sa1_all_index |> 
  # KULI aggregation - without subindicators
  mutate(kuli_addAgg = convstor1 + supermarket1 + strconnectivity1 + housedens1 +
           chemist1 + dentist1 + healthcr1 + hospital1 +
           secondary1 + primary1 + childcare1 +
           crime1 + crashes1 + flood1 + alcohol1 + emergency1 +
           station1 +freqbusstop1 +carInfrastructure1 + bikeability1 + #bustop1
           diversity1+marae1 +
           cinema1 + gallery1 + library1 + museum1 + theatre1 + sport1 + gym1 +
           cafe1 + restaurant1 +  pub1 + bbq1 +
           bigpark1 + beach1 +
           affordability1 + damp1) |> 
  # KULI aggregation - arithmetic average method
  mutate(kuli_arithAgg = minmaxNORM0_1(kuli_addAgg/37)) |>
  # KULI aggregation - additive method
  mutate(kuli_addAgg = minmaxNORM0_1(kuli_addAgg))
  # KULI aggregation - geometric average method
sa1_all_index$kuli_geomAgg <- minmaxNORM0_1(apply(sa1_all_index[,62:98], 1, FUN = a_gmean))
sa1_all_index$kuli_MPIAgg <- minmaxNORM0_1(ci_mpi(sa1_all_index,c(62:98),penalty="POS")$ci_mpi_est)

# rejoin with geometry
index_sa1g <- left_join(sa1_allg, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))

tm_shape(index_sa1g) +
  tm_polygons(col = c("kuli_geomAgg","kuli_MPIAgg"),
              palette = "-RdBu", style = "kmeans", lwd=0, n=12)

index_sa1g |> ggplot() + geom_histogram(aes(c(kuli_arithAgg)),bins=100)
st_write(index_sa1g, "data/geographic/sa1_kuli_all_renewed.gpkg")

# Interpolate to SA2 for web-map
sa2 = st_read('data/geographic/sa2.gpkg', quiet = T) # transform to OSGB projection
sz_sf = index_sa1g[,c(113:151,154)]
tz_sf <- sa2 |> 
  subset(select = c(SA22023_V1_00)) |> st_transform(27291)
sa2agg <- st_interpolate_aw(sz_sf, tz_sf, extensive = F)
summary(sa2agg)
tm_shape(sa2agg) + 
  tm_polygons("kuli_geomAgg", palette = "YlGnBu", style="kmeans",
              lwd=.1) + tm_layout(frame = F)
sa2agg <- st_transform(sa2agg, 4326)
st_write(sa2agg, "data/geographic/sa2agg.geojson")

#### EDA2 ####
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
    ggtitle("Raw") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) #ylab("Density") + xlab(varN) + xlab(varN)
  post_out = ggplot() +
    geom_histogram(aes(sa1_all_index[[xpost]]), bins=70) + theme_publish() +
    ggtitle("Transformed") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  # save as png image in specific directory with 600*350 resolution
  png(file=str_glue("outputs/distributions/distributions_{xpost}.png"),
      width=300, height=100)
  # a histogram we want to save
  grid.arrange(pre_out, post_out, ncol=2)
  # a function call to save the file
  dev.off()
  grid.arrange(pre_out, post_out, ncol=2)
}
densityplot(dist_stations, station1, "Train Station")
#densityplot(dist_busstops, bustop1, "Bus Stop")
densityplot(dist_busstopsfreq,freqbusstop1, "Frequent Buses")
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
densityplot(dist_cafe, cafe1, "Cafe")
densityplot(dist_restaurants, restaurant1, "Restaurant")
densityplot(dist_pubs, pub1, "Pub")
densityplot(dist_bbq, bbq1, "BBQ")
densityplot(bikeperarea, bikeability1, "Bikeability")
densityplot(dist_gym, gym1, "Gym")
densityplot(dist_beach, beach1, "Beach")
densityplot(medianRent, affordability1, "Affordability")
densityplot(carInfrastructure2_add, carInfrastructure2_add, "Car Infrastructure")
densityplot(dist_emergency, emergency1, "Emergency")
#densityplot(popdens, popdens1, "Pop Density")
#densityplot(househdens, housedens1, "House Density")
#densityplot(dist_smallpark, smallpark1, "Small Park")

df_indicators <- sa1_all_index[,c(62:98,101)]
colnames(df_indicators) <- c("Station","FrequentBusStop","Bikeability","CarInfrastructure",
                             "ConvenienceStore","Supermarket","StreetConnectivity",
                             "Cinema","Gym","Theatre","Library",
                             "Museum","Gallery","Sport","Affordability",
                             "Dampness","AlcoholEnvs","Crime","RoadSafety",
                             "FloodProneness","EmergencyServices","Diversity","Marae",
                             "Chemist","Dentist","HealthCentre","Hospital",
                             "Childcare","Primary","Secondary","Cafe",
                             "Restaurant","Pub","BBQ","BigPark","Beach","KULI")
# Cronbach Alpha #
cronbach.alpha(df_indicators)

# Correlations #
cor <- cor(x = df_indicators, y = df_indicators, use="complete.obs")
stargazer(cor, type = "latex")
corrplot(cor, tl.srt = 45, type = "lower", method = "ellipse",
         order = "FPC", tl.cex = 0.8,
        tl.col = "black", diag = T, cl.cex=0.7,cl.offset=0.3)
corr <- rcorr(as.matrix(df_indicators))

#### Mapping indicators ####
border <- st_read("data/geographic/sa1_auckland_waiheke_urban_new_final.gpkg") |> st_transform(27291)

indicators_sa1g <- left_join(sa1_allg, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))
indic_map_func = function(var_name, titl) {
  mapout = tm_shape(border) + tm_polygons(col="black", lwd=1)+
    tm_shape(indicators_sa1g) +
    tm_polygons(var_name, lwd=0,style = "kmeans", title = "Indicator",
                title.fontfamily="serif", palette="Greys", legend.show = FALSE) +
    tm_style("col_blind") +
    tm_layout(main.title = titl, frame = T,# legend.outside = F,
              main.title.size = 4, main.title.position = "center",
              main.title.fontfamily="serif")
  mapout
}
station = indic_map_func("station1", "Train Station")
#busstop = indic_map_func("bustop1", "Bus Stop")
freqb = indic_map_func("freqbusstop1", "Frequent Bus Stop")
hous=indic_map_func("housedens1", "House Density")
damp = indic_map_func("damp1", "Dampness")
diver = indic_map_func("diversity1", "Diversity")
crime = indic_map_func("crime1", "Crime")
crash = indic_map_func("crashes1", "Crashes")
flood = indic_map_func("flood1", "Floods")
alco = indic_map_func("alcohol1", "Alcohol Prohibited")
marae = indic_map_func("marae1", "Marae")
cinem=indic_map_func("cinema1", "Cinema")
gall = indic_map_func("gallery1","Gallery")
libr = indic_map_func("library1", "Library")
museum = indic_map_func("museum1", "Museum")
theat = indic_map_func("theatre1", "Theatre")
chemi = indic_map_func("chemist1", "Chemist")
denti = indic_map_func("dentist1", "Dentist")
health = indic_map_func("healthcr1", "Healthcentre")
hospi = indic_map_func("hospital1", "Hospital")
childc = indic_map_func("childcare1", "Childcare")
sport = indic_map_func("sport1", "Sport Facilities")
convs = indic_map_func("convstor1", "Convenience Store")
superm = indic_map_func("supermarket1", "Supermarket")
secon = indic_map_func("secondary1", "Secondary School")
prim = indic_map_func("primary1" , "Primary School")
stcon = indic_map_func("strconnectivity1", "Street Connectivity")
bigp <- indic_map_func("bigpark1", "Big Park")
cafe <- indic_map_func("cafe1", "Cafe")
resta <- indic_map_func("restaurant1", "Restaurant")
pub <- indic_map_func("pub1", "Pub")
bbq <- indic_map_func("bbq1", "BBQ")
bikeab <- indic_map_func("bikeability1", "Bikeability")
gym <- indic_map_func("gym1", "Gym")
beach <- indic_map_func("beach1", "Beach")
afford <- indic_map_func("affordability1", "Affordability")
carinf <- indic_map_func("carInfrastructure1", "Car Infrastructure")
emer <- indic_map_func("emergency1", "Emergency Service")
#smolp <- indic_map_func("smallpark1", "Small Park")
#popden = indic_map_func("popdens1", "Population Density")

#png(file="outputs/mapsindics2.png",width=4000, height=6400)
png(file="outputs/mapsindics_full.png",width=4000, height=6400)

tmap_arrange(station,freqb,bikeab,carinf,
             convs,superm,stcon,
             cinem,gym,theat,libr,museum,gall,sport,#popden,hous,
             afford,damp,
             alco,crime,crash,flood,emer,
             marae,diver,
             chemi,denti,health,hospi,childc,prim,secon,
             cafe, resta, pub, bbq,
             bigp,beach,
             nrow=8, ncol=5)
dev.off()

#### Map of indicators 2 ####
map_func = function(var_name, titl) {
  mapout = tm_shape(indicators_sa1g) +
    tm_polygons(var_name, lwd=0,style = "kmeans", title = "Indicator", title.fontfamily="serif") +
    tm_style("col_blind") +
    tm_layout(main.title = titl, legend.position = c("left","bottom"), frame = T, legend.outside = F,
              legend.title.fontfamily = "serif", main.title.size = 2, main.title.position = "center",
              legend.width=2, legend.height=2, legend.text.size=1.5,legend.title.size=2,
              legend.bg.color="grey100", legend.bg.alpha=.7, main.title.fontfamily="serif")
  #tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.4) +
  #tm_compass(type = "4star", size = 1, position = c("left", "top"))
  
  mapout
}
museu=map_func("museum1","Museum Indicator")
libr=map_func("library1","Library Indicator")
smolp=map_func("smallpark1","Small Park Indicator")
  
png(file="outputs/mapsindics3.png",width=2000, height=600)
tmap_arrange(smolp,libr,museu, ncol = 3, nrow=1)
dev.off()
#### Other ####
# SUBINDICATORS
# second level aggregation - geometric average method
sa1_all_index$carInfra2_geom <- minmaxNORM(apply(sa1_all_index[,65:66], 1, FUN = a_gmean))
sa1_all_index$transport2_geom <- minmaxNORM(apply(sa1_all_index[,c(75:77,106)], 1, FUN = a_gmean))
sa1_all_index$walkability2_geom <- minmaxNORM(apply(sa1_all_index[,c(65:66,88:89,92)], 1, FUN = a_gmean))
sa1_all_index$medical2_geom <- minmaxNORM(apply(sa1_all_index[,82:85], 1, FUN = a_gmean))
sa1_all_index$education2_geom <- minmaxNORM(apply(sa1_all_index[,c(86,90:91)], 1, FUN = a_gmean))
sa1_all_index$safety2_geom <- minmaxNORM(apply(sa1_all_index[,69:72], 1, FUN = a_gmean))
sa1_all_index$culture2_geom <- minmaxNORM(apply(sa1_all_index[,c(68,76)], 1, FUN = a_gmean))
sa1_all_index$sport2_geom <- minmaxNORM(apply(sa1_all_index[,c(87,100)], 1, FUN = a_gmean))
sa1_all_index$leisure2_geom <- minmaxNORM(apply(sa1_all_index[,77:81], 1, FUN = a_gmean))
sa1_all_index$food2_geom <- minmaxNORM(apply(sa1_all_index[,95:98], 1, FUN = a_gmean))
sa1_all_index$greenspace2_geom <- minmaxNORM(apply(sa1_all_index[,c(93,94)], 1, FUN = a_gmean))
sa1_all_index$housing2_geom <- minmaxNORM(apply(sa1_all_index[,c(67,102)], 1, FUN = a_gmean))

sa1_all_index_2level <- sa1_all_index |> 
  # second level aggregation - additive method
  mutate(walkability2_add = minmaxNORM(housedens1+convstor1 + supermarket1 + strconnectivity1)) |> 
  mutate(medical2_add = minmaxNORM(chemist1 + dentist1 + healthcr1 + hospital1)) |> 
  mutate(education2_add = minmaxNORM(secondary1 + primary1 + childcare1)) |> 
  mutate(safety2_add = minmaxNORM(crime1 + crashes1 + flood1 + alcohol1 + emergency1)) |> 
  mutate(transport2_add = minmaxNORM(station1 + bustop1 +freqbusstop1)) |> 
  mutate(culture2_add = minmaxNORM(diversity1+marae1)) |> 
  mutate(sport2_add = minmaxNORM(sport1 + gym1)) |> 
  mutate(leisure2_add = minmaxNORM(cinema1 + gallery1 +  library1 + museum1 + theatre1)) |> 
  mutate(food2_add = minmaxNORM(cafe1 + restaurant1 +  pub1 + bbq1)) |> 
  mutate(greenspace2_add = minmaxNORM(bigpark1 + smallpark1 + beach1)) |> 
  mutate(housing2_add = minmaxNORM(affordability1 + damp1)) |> 
  mutate(carInfrastructure2_add = minmaxNORM(evch1 + petrol1)) |> 
  # second level aggregation - arithmetic average method
  mutate(walkability2_mean = (popdens1 + housedens1 +convstor1 + supermarket1 + strconnectivity1)/45) |> 
  mutate(medical2_mean = (chemist1 + dentist1 + healthcr1 + hospital1)/36) |> 
  mutate(education2_mean = (secondary1 + primary1 + childcare1)/27) |> 
  mutate(safety2_mean = (crime1 + crashes1 + flood1 + alcohol1 + emergency1)/45) |> 
  mutate(transport2_mean = (station1 + bustop1 +freqbusstop1 + carInfra2_geom)/36) |> 
  mutate(culture2_mean = (diversity1+marae1)/18) |> 
  mutate(sport2_mean = (sport1 + gym1)/18) |> 
  mutate(leisure2_mean = (cinema1 + gallery1 +  library1 + museum1 + theatre1)/45) |> 
  mutate(food2_mean = (cafe1 + restaurant1 +  pub1 + bbq1)/20) |> 
  mutate(greenspace2_mean = (bigpark1 + smallpark1 + beach1)/27) |> 
  mutate(housing2_mean = (affordability1 + damp1)/18) |> 
  mutate(carInfrastructure2_mean = (evch1 + petrol1)/18) |> 
  # KULI aggregation - with 2nd level agg - additive method
  mutate(kuli_add2s_addAgg = minmaxNORM01(carInfrastructure2_add+bikeability1+walkability2_add+
                                            medical2_add+education2_add+safety2_add +
                                            transport2_add+culture2_add +sport2_add+
                                            leisure2_add+ food2_add + greenspace2_add + housing2_add))
# KULI aggregation - with 2nd level agg(geom) - geometric average method
#sa1_all_index$kuli_geom2s_geomAgg <- minmaxNORM01(apply(sa1_all_index[,103:114], 1, FUN = a_gmean))
# KULI aggregation - with 2nd level agg(add) - geometric average method
#sa1_all_index$kuli_add2s_geomAgg <- minmaxNORM01(apply(sa1_all_index[,c(115:126)], 1, FUN = a_gmean))
# KULI aggregation - with 2nd level agg(arith) - geometric average method
#sa1_all_index$kuli_arith2s_geomAgg <- minmaxNORM01(apply(sa1_all_index[,c(127:138)], 1, FUN = a_gmean))

# KULI aggregation -  MPI aggregation method
kuli_MPI <- ci_mpi(sa1_all_index,c(67:105,117),penalty="POS")
sa1_all_index$kuli_no2s_MPIAgg <- minmaxNORM01(kuli_MPI$ci_mpi_est)

# Reward Points
reward = 0.1
sa1_all_index$kuli_no2s_geomAgg_wrewards <- sa1_all_index$kuli_no2s_geomAgg
sa1_all_index$kuli_no2s_MPIAgg_wrewards <- sa1_all_index$kuli_no2s_MPIAgg
sa1_all_index$kuli_no2s_geomAgg_wrewards[sa1_all_index$dist_stations < 1000 & sa1_all_index$dist_busstopsfreq < 300 & sa1_all_index$dist_bigpark < 800] <- minmaxNORM01(sa1_all_index$kuli_no2s_geomAgg_wrewards[sa1_all_index$dist_stations < 1000 & sa1_all_index$dist_busstopsfreq < 300 & sa1_all_index$dist_bigpark < 800] + reward)
sa1_all_index$kuli_no2s_MPIAgg_wrewards[sa1_all_index$dist_stations < 1000 & sa1_all_index$dist_busstopsfreq < 300 & sa1_all_index$dist_bigpark < 800] <- minmaxNORM01(sa1_all_index$kuli_no2s_MPIAgg_wrewards[sa1_all_index$dist_stations < 1000 & sa1_all_index$dist_busstopsfreq < 300 & sa1_all_index$dist_bigpark < 800] + reward)

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

# testing that the geometric mean function actually does what I think it does
x <- c(1,3,6,1,10,3)
mean(x)
product <- prod(x)
product^(1/6)
exp(sum(log(x), na.rm = TRUE)/6)

