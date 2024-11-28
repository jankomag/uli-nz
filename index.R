# load packages
library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(tmap)
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
library(biscale)
library(cluster)
library(factoextra)

#### Data imports ####
# geographic data
preauli <- st_read("data/sa1_allvars.gpkg") |> st_transform(27291) #transforming to the same coordinate system
preauli <- preauli %>% dplyr::select(c(-occupiedPrivateDwellings, -crash_per_roadlen, -crash_risk))

#### EDA ####
column_names <- colnames(preauli)
column_indices <- seq_along(preauli)
for (i in seq_along(column_names)) {
  print(paste("Column Name:", column_names[i], "Index:", column_indices[i]))
}
# drop geometry for EDA
sa1_all <- preauli |> st_drop_geometry()
summary(sa1_all)
##### Correlations #####
datacorr <- sa1_all[c(4:37)]
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

winsorize_column <- function(data, column_name, percentage) {
  maxval <- quantile(data[[column_name]], 1 - percentage)
  
  data[[column_name]] <- pmin(pmax(data[[column_name]], 0), maxval)
  
  return(data)
}
max_perc <- 0.1

# adjust street connectivity
sa1_all$streetconn[sa1_all$streetconn == 0] <- min(unique(sa1_all$streetconn)[unique(sa1_all$streetconn) != min(unique(sa1_all$streetconn))])

sa1_all_index <- sa1_all %>%
  # LEISURE
  winsorize_column("dist_gym", max_perc) |> 
  winsorize_column("dist_sport", max_perc) |> 
  winsorize_column("dist_cinema", max_perc) |> 
  winsorize_column("dist_theatre", max_perc) |> 
  winsorize_column("dist_library", max_perc) |> 
  winsorize_column("dist_museum", max_perc) |> 
  winsorize_column("dist_gallery", max_perc) |> 
  # TRANSPORTATION
  winsorize_column("dist_stations", max_perc) |> 
  winsorize_column("dist_busstopsfreq", max_perc) |> 
  winsorize_column("dist_ev_charge", max_perc) |> 
  # ESSENTIAL AMENITIES
  winsorize_column("dist_conveniencestore", max_perc) |> 
  winsorize_column("dist_supermarket", max_perc) |> 
  # HOUSING
  winsorize_column("medianRent", max_perc) |> 
  winsorize_column("dampness", max_perc) |> 
  # ETHNIC DIVERSITY
  winsorize_column("dist_marae", max_perc) |> 
  # MEDICAL
  winsorize_column("dist_chemist", max_perc) |> 
  winsorize_column("dist_dentist", max_perc) |> 
  winsorize_column("dist_healthcentre", max_perc) |> 
  winsorize_column("dist_hospital", max_perc) |> 
  # EDUCATION
  winsorize_column("dist_childcare", max_perc) |> 
  winsorize_column("dist_primary", max_perc) |> 
  winsorize_column("dist_secondary", max_perc) |> 
  # FOOD OUTLETS
  winsorize_column("dist_cafe", max_perc) |> 
  winsorize_column("dist_restaurant", max_perc) |> 
  winsorize_column("dist_pub", max_perc) |> 
  winsorize_column("dist_bbq", max_perc) |> 
  # GREEN SPACE
  winsorize_column("dist_bigpark", max_perc) |> 
  winsorize_column("dist_beach", max_perc)

##### Custom Transformation of each variable #####
sa1_all_index <- sa1_all_index |> 
  mutate(gym1 = minmaxNORM(dist_gym)) |>
  mutate(sport1 = minmaxNORM(dist_sport)) |>
  
  mutate(cinema1 = minmaxNORM(dist_cinema)) |>
  mutate(theatre1 = minmaxNORM(dist_theatre)) |> #checked 
  mutate(library1 = minmaxNORM(dist_library)) |> #checked 
  mutate(museum1 = minmaxNORM(dist_museum)) |> #checked 
  mutate(gallery1 = minmaxNORM(dist_gallery)) |> #checked
  # LEISURE
  mutate(leisuresport1 = minmaxNORM(-(gym1+sport1))) |> #checked
  mutate(leisureart1 = minmaxNORM(DescTools::Winsorize(minmaxNORM(-(cinema1+theatre1+library1+museum1+gallery1)), val=c(3,10)))) |> #checked
  
  # TRANSPORTATION
  mutate(station1 = minmaxNORM(-dist_stations)) |> #checked
  mutate(freqbusstop1 = minmaxNORM(-dist_busstopsfreq)) |> #checked
  mutate(bikeability1 = minmaxNORM(log(bikeability+0.001))) |> #checked
  mutate(strconnectivity1 = minmaxNORM(log(streetconn))) |> #checked
  mutate(evch1 = minmaxNORM(-dist_ev_charge)) |> 
  
  # ESSENTIAL AMENITIES
  mutate(convstor1 = minmaxNORM(-dist_conveniencestore)) |>
  mutate(supermarket1 = minmaxNORM(-dist_supermarket)) |> 
  # HOUSING
  mutate(affordability1 = minmaxNORM(-medianRent)) |> #checked
  mutate(damp1 = minmaxNORM(-dampness)) |> #checked
  # SAFETY
  mutate(alcohol1 = minmaxNORM(-alcoprohibited)) |> #checked
  mutate(crime1 = minmaxNORM(-DescTools::Winsorize(log(crimerisk+0.00000001), val=c(-11,10)))) |> #checked
  mutate(crashes1 = minmaxNORM(-DescTools::Winsorize(log(crash_risk_LM+0.00000001), val=c(-10,-1)))) |> 
  # mutate(flood1 = minmaxNORM(-Winsorize(flood_pc, minval=0, maxval=.125))) |> #checked
  # mutate(emergency1 = minmaxNORM(-Winsorize(emergencyBC, minval=0, maxval=90))) |> #checked
  
  # ETHNIC DIVERSITY
  mutate(diversity1 = minmaxNORM(shannon)) |> #checked
  mutate(marae1 = minmaxNORM(-dist_marae)) |>
  
  # MEDICAL
  mutate(chemist1 = minmaxNORM(-dist_chemist)) |> 
  mutate(dentist1 = minmaxNORM(-dist_dentist)) |> 
  mutate(healthcr1 = minmaxNORM(-dist_healthcentre)) |> 
  mutate(hospital1 = minmaxNORM(-dist_hospital)) |>
  
  # EDUCATION
  mutate(childcare1 = minmaxNORM(-dist_childcare)) |> 
  mutate(primary1 = minmaxNORM(-dist_primary)) |>
  mutate(secondary1 = minmaxNORM(-dist_secondary)) |> 
  
  # FOOD OUTLETS
  mutate(cafe1 = minmaxNORM(-dist_cafe)) |> 
  mutate(restaurant1 = minmaxNORM(-dist_restaurant)) |> 
  mutate(pub1 = minmaxNORM(-dist_pub)) |> 
  mutate(bbq1 = minmaxNORM(-dist_bbq)) |>
  
  # GREEN SPACE
  mutate(park1 = minmaxNORM(-dist_bigpark)) |>
  mutate(beach1 = minmaxNORM(-dist_beach))

##### Final Index Construction ####
# show column index
column_names <- colnames(sa1_all_index)
column_indices <- seq_along(sa1_all_index)
for (i in seq_along(column_names)) {
  print(paste("Column Name:", column_names[i], "Index:", column_indices[i]))
}

# AULI aggregation
sa1_all_index$auli_geomAgg <- minmaxNORM(apply(sa1_all_index[,45:73], 1, FUN = a_gmean))
sa1_all_index$auli_MPIAgg <- minmaxNORM(ci_mpi(sa1_all_index[,45:73],penalty="POS")$ci_mpi_est)
#sa1_all_index$auli_MPIAgg <- Winsorize(sa1_all_index$kuli_MPIAgg, minval=0.1, maxval=max(sa1_all_index$kuli_MPIAgg))
#sa1_all_index |> ggplot() + geom_histogram(aes(c(kuli_MPIAgg)),bins=100)

# rejoin with geometry
sa1_boundry <- st_read("data/upload/sa1_auckland_waiheke_urban.gpkg") |> st_transform(27291) #transforming to the same coordinate system
index_sa1g <- left_join(sa1_boundry, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))
#preview kuli
tm_shape(index_sa1g) +
  tm_polygons(col = c("auli_MPIAgg","auli_geomAgg"),
              palette = "Reds", style = "jenks", lwd=0,n=7)

# save the KULI
column_names <- colnames(index_sa1g)
column_indices <- seq_along(index_sa1g)
for (i in seq_along(column_names)) {
  print(paste("Column Name:", column_names[i], "Index:", column_indices[i]))
}
auli <- index_sa1g[,c(1,45:73,75,76)]

# save for webmap
auli_web <- st_transform(auli,4326) |> 
  dplyr::select(-c(SA12018_V1_00))
colnames(auli_web) <- c("LeisureSport","LeisureArt","TrainStation","BusStop",
                             "Bikeability","StreetConnectivity","EVcharger",
                             "ConvenienceStore","Supermarket","Affordability",
                             "Dampness","AlcoholEnvs","Crime","RoadSafety",
                             "Diversity","Marae",
                             "Chemist","Dentist","HealthCentre","Hospital",
                             "Childcare","PrimarySchool","SecondarySchool","Cafe",
                             "Restaurant","Pub","BBQ","Park","Beach","AULI","geom")
auli_web <- st_simplify(auli_web, preserveTopology = FALSE, dTolerance = 6)
st_write(auli_web, "web-map/sa1_auli.geojson", append=F)

numeric_data <- auli_web[, sapply(auli_web, is.numeric)] %>% st_drop_geometry()
cor <- cor(x = numeric_data, y = numeric_data, use="complete.obs", method="pearson")
par(mfrow = c(1, 1))
corrplot(cor, tl.srt = 45, type = "lower", method = "ellipse",
         order = "FPC", tl.cex = 0.8,
         tl.col = "black", diag = T, cl.cex=0.7,cl.offset=0.3)

# Set up the plotting device
png(filename = "outputs/correlation_plot.png", 
    width = 8, height = 8, units = "in", res = 300)

# Create the plot
par(mfrow = c(1, 1))
corrplot(cor, tl.srt = 45, type = "lower", method = "ellipse",
         order = "FPC", tl.cex = 0.8,
         tl.col = "black", diag = T, cl.cex=0.7, cl.offset=0.3)

# Close the plotting device
dev.off()

#### AULI Visualisation ####
### MAP
bckgd <- st_read('data/land_auck.gpkg', quiet = T) # transform to OSGB projection
aulimap <- tm_shape(auli) +
  tm_polygons("grey",  lwd=0.05) +
  tm_shape(bckgd) + tm_polygons(col="#e2e2e2", lwd=0.1) +
  tm_shape(auli, bbox=bbox) + 
  tm_polygons(col = "auli_MPIAgg",
              palette = rev(hcl.colors(7, "YlGnBu")),
              title="AULI",
              legend.hist = TRUE,
              lwd = 0, style="jenks", n=7) +
  tm_layout(
    #main.title = "Auckland Urban Liveability Index", main.title.fontfamily = "serif",
    frame = FALSE, legend.title.fontfamily = "serif", main.title.size = 1.7,
    legend.outside = FALSE,
    legend.text.size = 0.00001,
    legend.hist.size = 0.6,
    legend.hist.width = 0.6,legend.hist.height = 0.2,
    bg.color="#edfbff", main.title.position = c('center', 'top')) +
  tm_compass(type = "4star", size = 1, position = c("left", "top"))
tmap_save(aulimap, filename = "outputs/auli_map.png", width = 8, height = 8)

#### Clustering ####
auli_features <- st_drop_geometry(auli[, 2:30])
k_values <- 1:5
wss <- numeric(length(k_values))

for (i in k_values) {
  kmeans_result <- kmeans(auli_features, centers = i, nstart = 10)
  wss[i] <- kmeans_result$tot.withinss
}

# Plot the elbow plot
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares (WSS)",
     main = "Elbow Plot for Optimal k")

kmeans_result <- kmeans(auli_features, centers = 2, nstart = 10)
auli$cluster <- kmeans_result$cluster
tm_shape(auli) +
  tm_polygons(col = "cluster", lwd=0, palette="Set2")

#### Indicator details for Appendix ####
# Evaluate the transformation method of each indicator
densityplot = function(xpre, xpost, varN) {
  xpre <- deparse(substitute(xpre))
  xpost <- deparse(substitute(xpost))
  varN <- toString(varN)
  Data <- c("Raw","Transformed")
  Skewness <- c(round(skewness(sa1_all[[xpre]]),3), round(skewness(sa1_all_index[[xpost]]),3))
  Kurtosis <- c(round(kurtosis(sa1_all[[xpre]]),3), round(kurtosis(sa1_all_index[[xpost]]),3))
  df <- data.frame(Data, Skewness, Kurtosis)
  print(df)
  
  pre_out = ggplot() +
    geom_histogram(aes(sa1_all[[xpre]]), bins=20) + theme_publish() +
    ggtitle("Raw") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) #ylab("Density") + xlab(varN) + xlab(varN)
  post_out = ggplot() +
    geom_histogram(aes(sa1_all_index[[xpost]]), bins=20) + theme_publish() +
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

# TRANSPORTATION
densityplot(dist_stations, station1, "Train Station")
densityplot(dist_busstopsfreq,freqbusstop1, "Frequent Buses")
densityplot(bikeability, bikeability1, "Bikeability")
densityplot(streetconn, strconnectivity1, "Street Connectivity")
densityplot(dist_ev_charge, evch1, "EV Chargers")

# ESSENTIAL AMENITIES
densityplot(dist_conveniencestore, convstor1, "Convenience Store")
densityplot(dist_supermarket, supermarket1, "Supermarket")
 
# HOUSING
densityplot(medianRent, affordability1, "Affordability")
densityplot(dampness, damp1, "Dampness")

# GREEN SPACE
densityplot(dist_bigpark, park1, "Big Park")
densityplot(dist_beach, beach1, "Beach")

# LEISURE
densityplot(leisureart1,leisureart1, "Marae")
densityplot(leisuresport1,leisuresport1, "Marae")

# SAFETY
densityplot(crimerisk, crime1, "Crime")
densityplot(crash_risk_LM, crashes1, "Crashes")
densityplot(alcoprohibited,alcohol1, "Alcohol Prohibited")

# ETHNIC DIVERSITY
densityplot(dist_marae,marae1, "Marae")
densityplot(shannon, diversity1, "Shannon Index")

# MEDICAL
densityplot(dist_chemist, chemist1, "Chemist")
densityplot(dist_dentist, dentist1, "Dentist")
densityplot(dist_healthcentre, healthcr1, "Healthcentre")
densityplot(dist_hospital, hospital1, "Hospital")

# EDUCATION
densityplot(dist_childcare, childcare1, "Childcare")
densityplot(dist_secondary, secondary1, "Secondary")
densityplot(dist_primary, primary1 , "Primary")

# FOOD OUTLETS
densityplot(dist_cafe, cafe1, "Cafe")
densityplot(dist_restaurant, restaurant1, "Restaurant")
densityplot(dist_pub, pub1, "Pub")
densityplot(dist_bbq, bbq1, "BBQ")

#densityplot(dist_cinema,cinema1, "Cinema")
#densityplot(dist_galleries, gallery1,"Gallery")
#densityplot(dist_libraries,library1, "Library")
#densityplot(dist_museums,museum1, "Museum")
#densityplot(dist_theatre,theatre1, "Theatre")
#densityplot(dist_sport, sport1, "Sport Facilities")
#densityplot(dist_gym, gym1, "Gym")

df_indicators <- st_drop_geometry(auli[,2:31])
colnames(df_indicators) <- c("LeisureArt","LeisureSport","TrainStation","BusStop",
                             "Bikeability","StreetConnectivity","EVcharger",
                             "ConvenienceStore","Supermarket","Affordability",
                             "Dampness","AlcoholEnvs","Crime","RoadSafety",
                             "Diversity","Marae",
                             "Chemist","Dentist","HealthCentre","Hospital",
                             "Childcare","PrimarySchool","SecondarySchool","Cafe",
                             "Restaurant","Pub","BBQ","Park","Beach","AULI")
# Cronbach Alpha #
cronbach.alpha(df_indicators)

# Correlations #
cor <- cor(x = df_indicators, y = df_indicators, use="complete.obs", method="pearson")
corrplot(cor, tl.srt = 60, type = "lower", method = "ellipse",
         order = "FPC", tl.cex = 0.8,
        tl.col = "black", diag = T, cl.cex=0.7,cl.offset=0.3)

##### Mapping all indicators #####
border <- st_read("data/upload/sa1_auckland_waiheke_urban.gpkg") |> st_transform(27291)
indicators_sa1g <- left_join(sa1_polys, sa1_all_index, by = c("SA12018_V1_00"="SA12018_V1_00"))
indic_map_func = function(var_name, titl) {
  mapout = tm_shape(border) + tm_polygons(col="black", lwd=1)+
    tm_shape(auli) +
    tm_polygons(var_name, lwd=0,style = "kmeans", title = "Indicator",
                title.fontfamily="serif", palette="Greys", legend.show = FALSE) +
    tm_style("col_blind") +
    tm_layout(main.title = titl, frame = T,# legend.outside = F,
              main.title.size = 4, main.title.position = "center",
              main.title.fontfamily="serif")
  mapout
}

# TRANSPORTATION
station = indic_map_func("station1", "Train Station")
freqb = indic_map_func("freqbusstop1", "Frequent Bus Stop")
bikeab <- indic_map_func("bikeability1", "Bikeability")
evch <- indic_map_func("evch1", "EV Chargers")
stcon = indic_map_func("strconnectivity1", "Street Connectivity")

# LEISURE
lart <- indic_map_func("leisureart1", "Leisure Art")
lsport <- indic_map_func("leisuresport1", "Leisure Sport")

# ESSENTIAL AMENITIES
convs = indic_map_func("convstor1", "Convenience Store")
superm = indic_map_func("supermarket1", "Supermarket")

# HOUSING
afford <- indic_map_func("affordability1", "Affordability")
damp = indic_map_func("damp1", "Dampness")

# SAFETY
crime = indic_map_func("crime1", "Crime")
crash = indic_map_func("crashes1", "Crashes")
alco = indic_map_func("alcohol1", "Alcohol Prohibited")

# ETHNIC DIVERSITY
divers = indic_map_func("diversity1", "Diversity")
marae = indic_map_func("marae1", "Marae")

# MEDICAL
denti = indic_map_func("dentist1", "Dentist")
health = indic_map_func("healthcr1", "Healthcentre")
hospi = indic_map_func("hospital1", "Hospital")
chemi = indic_map_func("chemist1", "Chemist")

# EDUCATION
childc = indic_map_func("childcare1", "Childcare")
secon = indic_map_func("secondary1", "Secondary School")
prim = indic_map_func("primary1" , "Primary School")

# FOOD OUTLETS
cafe <- indic_map_func("cafe1", "Cafe")
resta <- indic_map_func("restaurant1", "Restaurant")
pub <- indic_map_func("pub1", "Pub")
bbq <- indic_map_func("bbq1", "BBQ")

# GREEN SPACE
bigp <- indic_map_func("park1", "Big Park")
beach <- indic_map_func("beach1", "Beach")

png(file="outputs/mapsindics_full.png",width=4000, height=5000)

tmap_arrange(station,freqb,bikeab,evch,stcon,
             convs,superm,afford,damp,
             lart, lsport,
             alco,crime,crash,
             marae,divers,
             chemi,denti,health,hospi,childc,prim,secon,
             cafe, resta, pub, bbq,
             bigp,beach,
             nrow=6, ncol=5)
dev.off()