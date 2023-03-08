# load the packages
library(sf)
library(sp)
library(tmap)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(geosphere)
library(spdep)
library(tidyr)
library(mice)
library(VIM)
library(stringr)
library(nngeo)

#### Data imports ####
# geographic data
sa1_polys <- st_read("data/geographic/sa1_auckland_waiheke_urban_new_final.gpkg")
sa1_polys <- sa1_polys |>
  subset(select = c(SA12018_V1_00, area.x.x))
#transforming to the same coordinate system
sa1_base <- st_transform(sa1_polys, 27291) |> st_drop_geometry()

##### Census####
census <- as.data.frame(read.csv("data/geographic/Census/auckland_census.csv"))
census <- census |>
  subset(select = -c(maori_desc, median_income, born_overseas, PacificNum)) |> 
  mutate(code = as.character(code)) |> 
  mutate(dampness = as.numeric(dampness)) |> 
  mutate(European = as.numeric(European)) |> 
  mutate(Maori = as.numeric(Maori)) |>
  mutate(Pacific = as.numeric(Pacific)) |>
  mutate(Asian = as.numeric(Asian)) |>
  mutate(MiddleEasternLatinAmericanAfrican = as.numeric(MiddleEasternLatinAmericanAfrican)) |>
  mutate(OtherEthnicity = as.numeric(OtherEthnicity)) |>
  mutate(pop_usual = as.numeric(pop_usual)) |> 
  mutate(medianRent = as.numeric(medianRent))

# Dealing with missing values #
# find NAs
md.pattern(census)
aggr_plot <- aggr(census, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(census), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# replace with 0s
census[which(census$pop_usual == 0,),5:10] <- 0 # assign ethnicity values to 0 in rows where no population is registered

#calculate percentages ethnicity
census <- census |>
  mutate(pcEuropean = European/pop_usual, na.rm=T) |> 
  mutate(pcMaori = Maori/pop_usual, na.rm=T) |> 
  mutate(pcPacific = Pacific/pop_usual, na.rm=T) |> 
  mutate(pcAsian = Asian/pop_usual, na.rm=T) |> 
  mutate(pcMiddleEasternLatinAmericanAfrican = MiddleEasternLatinAmericanAfrican/pop_usual, na.rm=T) |> 
  mutate(pcOtherEthnicity = OtherEthnicity/pop_usual, na.rm=T)
census[which(census$pop_usual == 0,),12:18] <- 0 # 0 in rows where no population is registered
census[which(census$pcEuropean>1), "pcEuropean"] <- 1 # 3 rows where white pop is larger than total pop

#### NN impute - impute NAs based on neighbouring values
sa1_imp <- left_join(sa1_polys, census, by = c("SA12018_V1_00"="code"))
index <- st_touches(sa1_imp, sa1_imp)
sa1_imped <- sa1_imp %>% 
  mutate(dampness = ifelse(is.na(dampness),
                       apply(index, 1, function(i){mean(.$dampness[i], na.rm=T)}),
                       dampness))
sa1_imped <- sa1_imped %>% 
  mutate(medianRent = ifelse(is.na(medianRent),
                             apply(index, 1, function(i){mean(.$medianRent[i], na.rm=T)}),
                             medianRent))
sa1_imped <- sa1_imped %>% 
  mutate(pcEuropean = ifelse(is.na(pcEuropean),
                             apply(index, 1, function(i){mean(.$pcEuropean[i], na.rm=T)}),
                             pcEuropean))
sa1_imped <- sa1_imped %>% 
  mutate(pcMaori = ifelse(is.na(pcMaori),
                             apply(index, 1, function(i){mean(.$pcMaori[i], na.rm=T)}),
                             pcMaori))
sa1_imped <- sa1_imped %>% 
  mutate(pcPacific = ifelse(is.na(pcPacific),
                             apply(index, 1, function(i){mean(.$pcPacific[i], na.rm=T)}),
                             pcPacific))
sa1_imped <- sa1_imped %>% 
  mutate(pcAsian = ifelse(is.na(pcAsian),
                             apply(index, 1, function(i){mean(.$pcAsian[i], na.rm=T)}),
                             pcAsian))
sa1_imped <- sa1_imped %>% 
  mutate(pcMiddleEasternLatinAmericanAfrican = ifelse(is.na(pcMiddleEasternLatinAmericanAfrican),
                             apply(index, 1, function(i){mean(.$pcMiddleEasternLatinAmericanAfrican[i], na.rm=T)}),
                             pcMiddleEasternLatinAmericanAfrican))
sa1_imped <- sa1_imped %>% 
  mutate(pcOtherEthnicity = ifelse(is.na(pcOtherEthnicity),
                             apply(index, 1, function(i){mean(.$pcOtherEthnicity[i], na.rm=T)}),
                             pcOtherEthnicity))
sa1_imped[which(is.na(sa1_imped$dampness),), "dampness"] <- mean(sa1_imped$dampness, na.rm=T)
sa1_imped[which(is.na(sa1_imped$medianRent),), "medianRent"] <- mean(sa1_imped$medianRent, na.rm=T)
census <- st_drop_geometry(sa1_imped) |> 
  subset(select = -c(na.rm, area.x.x, European,Maori, Pacific, Asian, MiddleEasternLatinAmericanAfrican, OtherEthnicity))
##### Diversity ####
# Compute Diversity Index #
shannon <- function(p){
  if (0 %in% p) {
    p = replace(p,p==0,0.0001)
  } else {
      p
    }
  H = -sum(p*log(p))
  return (H)
}
census$shannon <- apply(census[,6:11], 1, shannon)
sa1_all <- left_join(sa1_base, census, by = c("SA12018_V1_00"="SA12018_V1_00"))
##### Crimes ####
# Compute crime measure
crime <- as.data.frame(read.csv("data/safety/crime/crimes_originaldata.csv"))
crime$Area.Unit = substr(crime$Area.Unit,1,nchar(crime$Area.Unit)-1)
crime <- crime |> 
  mutate(terrau = as.factor(Territorial.Authority)) |> 
  mutate(Meshblock = as.factor(Meshblock)) |> 
  mutate(Area.Unit = as.factor(Area.Unit))
crime <- crime[crime$terrau == 'Auckland.',]

# sum all cases by Area Units
crime_agg <- aggregate(crime["Victimisations"], by=crime["Area.Unit"], sum)
areaunits <- st_read("data/geographic/areaunit/area-units.gpkg")
areaunits <- st_transform(areaunits, 27291)
areaunits_crimes <- left_join(areaunits, crime_agg, by = c("name"="Area.Unit"))
#st_write(areaunits_crimes, "data/safety/crime/crimes_aggregated_areaunit.gpkg")
tm_shape(areaunits_crimes)+
  tm_polygons("Victimisations")

# aggregation to SA1 level was done in QGIS
sa1_crime <- st_read("data/safety/crime/sa1_crimes_final.gpkg") |> st_drop_geometry()
sa1_crime <- sa1_crime |>
  subset(select = c(SA12018_V1_00, crime_perarea))

sa1_all <- left_join(sa1_all, sa1_crime, by = c("SA12018_V1_00"="SA12018_V1_00"))
sa1_all[which(is.na(sa1_all$crime_perarea),), "crime_perarea"] <- 0

##### Road Safety ####
crashes_sa1 <- st_read("data/safety/crash/sa1_crashdist.gpkg") |> st_drop_geometry()
crashes_sa1 <- crashes_sa1 |>
  subset(select = c(SA12018_V1_00, dist_crash))

sa1_all <- left_join(sa1_all, crashes_sa1, by = c("SA12018_V1_00"="SA12018_V1_00"))

##### Floods ####
floods_sa1 <- st_read("data/safety/floods/sa1_floods_final.gpkg") |> st_drop_geometry()
floods_sa1 <- floods_sa1 |>
  subset(select = c(SA12018_V1_00, flood_pc))
names(floods_sa1)[names(floods_sa1) == "flood_pc"] <- "floodprone_prc"

sa1_all <- left_join(sa1_all, floods_sa1, by = c("SA12018_V1_00"="SA12018_V1_00"))
sa1_all[which(is.na(sa1_all$floodprone_prc),), "floodprone_prc"] <- 0

##### Alcohol Environments ####
alco_sa1 <- st_read("data/safety/alcoholenvs/sa1_cents_alcoenvs.gpkg") |> st_drop_geometry()
alco_sa1 <- alco_sa1 |>
  subset(select = c(SA12018_V1, alcoprohibited))
sa1_all <- left_join(sa1_all, alco_sa1, by = c("SA12018_V1_00"="SA12018_V1"))
sa1_all[which(is.na(sa1_all$alcoprohibited),), "alcoprohibited"] <- 0

##### Street Connectivity ####
stconnectivity <- st_read("data/walkability/streetconnectivity.gpkg") |> st_drop_geometry()
sa1_all <- left_join(sa1_all, stconnectivity, by = c("SA12018_V1_00"="SA12018_V1_00"))

#### Distances ####
sa1_dists <- st_read("data/geographic/sa1_alldist_final.gpkg")|> st_drop_geometry() |> subset(select = -c(dist_marae))
sa1_all <- left_join(sa1_all, sa1_dists, by = c("SA12018_V1_00"="SA12018_V1_00"))

#add frequent bus stops
sa1_busfreq <- st_read("data/transport/public_transport/sa1_frequentbuses.gpkg")|> st_drop_geometry()
sa1_busfreq <- sa1_busfreq |>
  subset(select = c(SA12018_V1_00, dist_busstopsfreq))
sa1_all <- left_join(sa1_all, sa1_busfreq, by = c("SA12018_V1_00"="SA12018_V1_00"))

# add marae
sa1_marae <- st_read("data/kiwi/sa1_maraefinal.gpkg")|> st_drop_geometry()
sa1_marae <- sa1_marae |>
  subset(select = c(SA12018_V1_00, dist_marae))
sa1_all <- left_join(sa1_all, sa1_marae, by = c("SA12018_V1_00"="SA12018_V1_00"))

# add other distances
sa1_restdistances <- st_read("data/geographic/sa1_cafeandrestaurantsandother_andallestnesestDONE.gpkg")|> st_drop_geometry()
sa1_restdistances <- sa1_restdistances |>
  subset(select = c(SA12018_V1_00, dist_cafe, dist_restaurants, dist_pubs, dist_bbq, dist_gym, dist_beach))
sa1_all <- left_join(sa1_all, sa1_restdistances, by = c("SA12018_V1_00"="SA12018_V1_00"))

#add bikeability
sa1_bikeability <- st_read("data/transport/bikeability.gpkg")|> st_drop_geometry()
sa1_all <- left_join(sa1_all, sa1_bikeability, by = c("SA12018_V1_00"="SA12018_V1_00"))

#add emergency services
sa1_emergency <- st_read("data/safety/emergency/emergency_dist.gpkg")|> st_drop_geometry()
sa1_all <- left_join(sa1_all, sa1_emergency, by = c("SA12018_V1_00"="SA12018_V1_00"))

#join with spatial
sa1_allg <- left_join(sa1_polys, sa1_all, by=c("SA12018_V1_00"="SA12018_V1_00"))
sa1_allg <- sa1_allg |> 
  subset(select = -c(area.x.x.x,area.x.x.y))

sa1_allg[which(is.infinite(sa1_allg$dist_stations),), "dist_stations"] <- 100000
sa1_allg[which(is.infinite(sa1_allg$dist_childcare),), "dist_childcare"] <- 100000
sa1_allg[which(is.infinite(sa1_allg$dist_hospital),), "dist_hospital"] <- 100000
sa1_allg[which(is.infinite(sa1_allg$dist_chemist),), "dist_chemist"] <- 100000
sa1_allg[which(is.infinite(sa1_allg$dist_gym),), "dist_gym"] <- 100000

# plot
tm_shape(sa1_allg) +
  tm_polygons(col="dist_emergency",style="kmeans", lwd=0)

st_write(sa1_allg, "data/geographic/sa1_allvars.gpkg")