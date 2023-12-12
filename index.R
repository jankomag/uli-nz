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
prekuli <- st_read("data/sa1_allvars.gpkg") |> st_transform(27291) #transforming to the same coordinate system
summary(prekuli)

#### EDA ####
column_names <- colnames(prekuli)
column_indices <- seq_along(prekuli)
for (i in seq_along(column_names)) {
  print(paste("Column Name:", column_names[i], "Index:", column_indices[i]))
}
# drop geometry for EDA
sa1_all <- prekuli |> st_drop_geometry()
##### Correlations #####
datacorr <- sa1_all[c(4:6,8:42)]
cor <- cor(x = datacorr, y = datacorr, use="complete.obs")
corrplot(cor, tl.srt = 25)
corr <- rcorr(as.matrix(datacorr))

##### Examine Distributions of all variables #####
my_plots <- lapply(names(datacorr), function(var_x){
  p <- ggplot(datacorr, aes(x = .data[[var_x]]))
  if(is.numeric(datacorr[[var_x]])) {
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
reverseMinmaxNORM <- function(y, original_min, original_max) {
  x <- ((y - 1) / (10 - 1)) * (original_max - original_min) + original_min
  return(x)
} # from 1-10 back to original distribution
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
         mean uses log. Normalise to remove negative/zero values first.")}
    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA
    # calculate geom mean
    gm <- exp( sum(w * log(x), na.rm = TRUE)/sum(w, na.rm = TRUE) )
  } else {
    gm <- NA
  }
  gm
}


# Test box cox function
transform_to_normal <- function(column, small_constant = 0.000001) {
  # Add a small constant to the column
  transformed_column <- column + small_constant
  
  # Find optimal lambda using Box-Cox transformation
  b <- boxcox(lm((transformed_column ~ 1,data=sa1_all)))
  print(b)
  lambda <- b$x[which.max(b$y)]
  
  # Apply Box-Cox transformation
  transformed_column <- ((transformed_column) ^ lambda - 1) / lambda
  
  return (transformed_column)
}
transform_to_normal(sa1_all$dist_marae, small_constant = 0.1)

# box cox transformation
b <- boxcox(lm((crash_risk_LM+0.0000000000000001) ~ 1, data=sa1_all))
lambda <- b$x[which.max(b$y)]
sa1_alltest <- prekuli |> 
  mutate(boxc = ((crash_risk_LM) ^ lambda - 1) / lambda)

# Choosing indicator prekuli
sa1_alltest <- prekuli |> 
  mutate(raw = crash_risk_LM) |> 
  mutate(testvar = log(crash_risk_LM+0.00000001)) |> 
  mutate(testvar2 = Winsorize(testvar, maxval=-1,minval=-10)) |> 
  mutate(newtestvar = as.numeric(testvar2))#,minval=-8,maxval=2))

sa1_alltest %>%
  pivot_longer(cols = testvar2) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 100)

tm_shape(sa1_alltest) +
  tm_fill(c("testvar2"),lwd=0, style="jenks", palette="Blues")

# optimised lambda values for chosen variables
lambdahealth <- 0.3434343
lambdachildcare <- 0.06060606
lambdasport <- 0.3838384
lambdasecond <- 0.3434343
lambdapetrol <- 0.4242424
lambdaev <- 0.6262626
lambdapubs <- 0.3434343
lambdarent <- 1.151515
lambdaemergency <- 0.4646465
lambdaconv <- 0.3434343
lambdabike <- 0.4646465
lambdacrashrisk <- -0.06060606

##### Custom Transformation of each variable ##### original no box-cox
sa1_all_index <- sa1_all |> 
  # BoxCox prep
  mutate(rentBC = ((medianRent+0.1) ^ lambdarent - 1) / lambdarent) |> 
  mutate(healthcBC = ((dist_healthcentre+0.1) ^ lambdahealth - 1) / lambdahealth) |> 
  mutate(childcareBC = ((dist_childcare+0.1) ^ lambdachildcare - 1) / lambdachildcare) |> 
  mutate(petrolBC = ((dist_petrol+0.1) ^ lambdapetrol - 1) / lambdapetrol) |> 
  mutate(evchBC = ((dist_ev_charge+0.1) ^ lambdaev - 1) / lambdaev) |> 
  mutate(pubBC = ((dist_pub+0.1) ^ lambdapubs - 1) / lambdapubs) |> 
  mutate(sportBC = ((dist_sport+0.1) ^ lambdasport - 1) / lambdasport) |> 
  mutate(secondaryBC = ((dist_secondary+0.1) ^ lambdasecond - 1) / lambdasecond) |> 
  mutate(emergencyBC = ((dist_emergency+0.1) ^ lambdaemergency - 1) / lambdaemergency) |> 
  mutate(convstoreBC = ((dist_conveniencestore+0.000001) ^ lambdaconv - 1) / lambdaconv) |> 
  mutate(petrol1 = minmaxNORM(-Winsorize(petrolBC, minval=1, maxval = 80))) |>
  mutate(evch1 = minmaxNORM(-(evchBC))) |> 
  
  # TRANSPORTATION
  mutate(station1 = minmaxNORM(-Winsorize(log(dist_stations), minval=5, maxval=11))) |> #checked
  mutate(freqbusstop1 = minmaxNORM(-Winsorize(dist_busstopsfreq, minval=0, maxval= 2000))) |> #checked
  mutate(bikeability1 = minmaxNORM(Winsorize(bikeability,maxval=0.04))) |> #checked
  mutate(carInfrastructure1 = minmaxNORM((evch1 + petrol1)/18)) |> #checked
  
  # WALKABILITY
  mutate(convstor1 = minmaxNORM(-Winsorize(convstoreBC, maxval=45,minval=-1))) |>
  mutate(supermarket1 = minmaxNORM(-Winsorize(log(dist_supermarket+0.1), minval=4.5, maxval=10))) |> #checked 
  mutate(strconnectivity1 = minmaxNORM(Winsorize(streetconn, minval=0, maxval=15))) |> #checked
  mutate(dwelldensity1 = minmaxNORM(dwelldensity_transf_LM)) |> #checked
  
  # LEISURE
  mutate(cinema1 = minmaxNORM(-Winsorize(log(dist_cinema), minval=6, maxval=max(log(dist_cinema))))) |> #checked
  mutate(gym1 = minmaxNORM(-Winsorize(log(dist_gym+0.1), minval=5, maxval=10))) |> #checked
  mutate(theatre1 = minmaxNORM(-Winsorize(dist_theatre, minval=0, maxval=11000))) |> #checked 
  mutate(library1 = minmaxNORM(-Winsorize(log(dist_library), minval=5.3, maxval=max(log(dist_library))))) |> #checked 
  mutate(museum1 = minmaxNORM(-Winsorize(dist_museum, minval=0, maxval=18000))) |> #checked 
  mutate(gallery1 = minmaxNORM(-Winsorize(log(dist_gallery), minval=6, maxval=max(log(dist_gallery))))) |> #checked
  mutate(sport1 = minmaxNORM(-Winsorize((sportBC), minval=0, maxval=60))) |> #checked 
  
  # HOUSING
  mutate(affordability1 = minmaxNORM(-Winsorize(rentBC, minval=0, maxval=2500))) |> #checked
  mutate(damp1 = minmaxNORM(-Winsorize(dampness, maxval = 0.4, minval = 0))) |> #checked
  
  #SAFETY
  mutate(alcohol1 = minmaxNORM(-alcoprohibited)) |> #checked
  mutate(crime1 = minmaxNORM(-Winsorize(log(crimerisk+0.00000001), maxval=10,minval=-11))) |> #checked
  mutate(crashes1 = minmaxNORM(-Winsorize(log(crash_risk_LM+0.00000001), maxval=-1,minval=-10))) |> 
  mutate(flood1 = minmaxNORM(-Winsorize(flood_pc, minval=0, maxval=.125))) |> #checked
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
  mutate(restaurant1 = minmaxNORM(-Winsorize(dist_restaurant, minval=0, maxval=4000))) |> #checked
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
  mutate(kuli_addAgg = convstor1 + supermarket1 + strconnectivity1 + dwelldensity1 +
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

# show column index
column_names <- colnames(sa1_all_index)
column_indices <- seq_along(sa1_all_index)
for (i in seq_along(column_names)) {
  print(paste("Column Name:", column_names[i], "Index:", column_indices[i]))
}
  # KULI aggregation - geometric average method
sa1_all_index$kuli_geomAgg <- minmaxNORM0_1(apply(sa1_all_index[,55:91], 1, FUN = a_gmean))
sa1_all_index$kuli_MPIAgg <- minmaxNORM0_1(ci_mpi(sa1_all_index,c(55:91),penalty="POS")$ci_mpi_est)
#sa1_all_index$kuli_MPIAgg <- minmaxNORM0_1(Winsorize(sa1_all_index$kuli_MPIAgg, maxval = 150, minval = 86))
hist(sa1_all_index$kuli_MPIAgg, breaks=50)

# rejoin with geometry
sa1_boundry <- st_read("data/sa1_auckland_waiheke_urban.gpkg") |> st_transform(27291) #transforming to the same coordinate system
index_sa1g <- left_join(sa1_boundry, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))
#preview kuli
tm_shape(index_sa1g) +
  tm_polygons(col = c("kuli_geomAgg","kuli_MPIAgg"),
              palette = "-RdBu", style = "jenks", lwd=0)
index_sa1g |> ggplot() + geom_histogram(aes(c(kuli_MPIAgg)),bins=100)

# save the KULI
kuli <- index_sa1g[,c(1,55:91,95)]
st_write(kuli, "data/sa1_kuli.gpkg")
# save for webmap
kuli <- st_transform(kuli,3857)
st_write(kuli, "web-map/sa1_kuli.geojson")

# Interpolate to SA2 for web-map
sa2 = st_read('data/geographic/sa2.gpkg', quiet = T) |>
  st_transform(27291) |> select(SA22023_V1_00)
sa2agg <- st_interpolate_aw(kuli, sa2, extensive = F)
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
densityplot(dist_busstopsfreq,freqbusstop1, "Frequent Buses")
densityplot(bikeperarea, bikeability1, "Bikeability")
densityplot(carInfrastructure1, carInfrastructure1, "Car Infrastructure")

densityplot(dist_conveniencestore, convstor1, "Convenience Store")
densityplot(dist_supermarket, supermarket1, "Supermarket")
densityplot(streetconn, strconnectivity1, "Street Connectivity")
densityplot(dwelldensity_buff200_NUMPOINTS, housedens1, "House Density")

densityplot(dist_bigpark, bigpark1, "Big Park")
densityplot(dist_beach, beach1, "Beach")

densityplot(dist_marae,marae1, "Marae")
densityplot(shannon, diversity1, "Shannon Index")

densityplot(crime_perarea, crime1, "Crime")
densityplot(dist_crash, crashes1, "Crashes")
densityplot(floodprone_prc,flood1, "Floods")
densityplot(alcoprohibited,alcohol1, "Alcohol Prohibited")
densityplot(dist_emergency, emergency1, "Emergency")

densityplot(dist_cinema,cinema1, "Cinema")
densityplot(dist_galleries, gallery1,"Gallery")
densityplot(dist_libraries,library1, "Library")
densityplot(dist_museums,museum1, "Museum")
densityplot(dist_theatre,theatre1, "Theatre")

densityplot(dist_chemist, chemist1, "Chemist")
densityplot(dist_dentist, dentist1, "Dentist")
densityplot(dist_healthcentre, healthcr1, "Healthcentre")
densityplot(dist_hospital, hospital1, "Hospital")

densityplot(dist_sport, sport1, "Sport Facilities")

densityplot(dist_childcare, childcare1, "Childcare")
densityplot(dist_secondary, secondary1, "Secondary")
densityplot(dist_primary, primary1 , "Primary")

densityplot(dist_cafe, cafe1, "Cafe")
densityplot(dist_restaurants, restaurant1, "Restaurant")
densityplot(dist_pubs, pub1, "Pub")
densityplot(dist_bbq, bbq1, "BBQ")
densityplot(dist_gym, gym1, "Gym")

densityplot(medianRent, affordability1, "Affordability")
densityplot(dampness, damp1, "Dampness")

df_indicators <- sa1_all_index[,c(55:91,95)]
colnames(df_indicators) <- c("Station","FrequentBusStop","Bikeability","CarInfrastructure",
                             "ConvenienceStore","Supermarket","StreetConnectivity","DwellingDensity",
                             "Cinema","Gym","Theatre","Library",
                             "Museum","Gallery","Sport","Affordability",
                             "Dampness","AlcoholEnvs","Crime","RoadSafety",
                             "FloodProneness","EmergencyServices","Diversity","Marae",
                             "Pharmacy","Dentist","HealthCentre","Hospital",
                             "Childcare","Primary","Secondary","Cafe",
                             "Restaurant","Pub","BBQ","Park","Beach","KULI")
# Cronbach Alpha #
cronbach.alpha(df_indicators)

# Correlations #
cor <- cor(x = df_indicators, y = df_indicators, use="complete.obs", method="pearson")
stargazer(cor, type = "text")
corrplot(cor, tl.srt = 45, type = "lower", method = "ellipse",
         order = "FPC", tl.cex = 0.8,
        tl.col = "black", diag = T, cl.cex=0.7,cl.offset=0.3)

round(cor(x = df_indicators$KULI, y = df_indicators$Affordability),3)

plot(x = df_indicators$KULI, y = df_indicators$Affordabilit)

#### Mapping indicators ####
border <- st_read("data/sa1_auckland_waiheke_urban.gpkg") |> st_transform(27291)

indicators_sa1g <- left_join(sa1_polys, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))
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
freqb = indic_map_func("freqbusstop1", "Frequent Bus Stop")
hous=indic_map_func("dwelldensity1", "Dwelling Density")
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


png(file="outputs/mapsindics_full.png",width=4000, height=6400)

tmap_arrange(station,freqb,bikeab,carinf,
             convs,hous,superm,stcon,
             cinem,gym,theat,libr,museum,gall,sport,#popden,hous,
             afford,damp,
             alco,crime,crash,flood,emer,
             marae,diver,
             chemi,denti,health,hospi,childc,prim,secon,
             cafe, resta, pub, bbq,
             bigp,beach,
             nrow=8, ncol=5)
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