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
#library(gstat)
#library(rgeos)

#### Data imports ####
# geographic data
#sa1_dist <- st_read("data/geographic/sa1_allNetDist.gpkg")
sa1_base <- st_read("data/geographic/urban_sa1_landvalid.gpkg")
#transforming to the same coordinate system
#sa1_dist <- st_transform(sa1_dist, 27291)
sa1_base <- st_transform(sa1_base, 27291)

sa1_base <- sa1_base |>
  subset(select = c(SA12018_V1_00, area, geom))

tm_shape(sa1_base)+
  tm_polygons(lwd=0, style="kmeans")

##### Census####
census <- as.data.frame(read.csv("data/geographic/Census/auckland_census.csv"))
census <- census |>
  select(subset = -c(maori_desc, median_income, born_overseas, PacificNum)) |> 
  mutate(code = as.character(code)) |> 
  mutate(dampness = as.numeric(dampness), na.rm=T) |> 
  mutate(European = as.numeric(European), na.rm=T) |> 
  mutate(Maori = as.numeric(Maori), na.rm=T) |>
  mutate(Pacific = as.numeric(Pacific), na.rm=T) |>
  mutate(Asian = as.numeric(Asian), na.rm=T) |>
  mutate(MiddleEasternLatinAmericanAfrican = as.numeric(MiddleEasternLatinAmericanAfrican), na.rm=T) |>
  mutate(OtherEthnicity = as.numeric(OtherEthnicity), na.rm=T) |>
  mutate(pop_usual = as.numeric(pop_usual))

# Dealing with missing values #
#observe NAs
md.pattern(census)
aggr_plot <- aggr(census, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(census), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# replace with 0s
census[which(census$pop_usual == 0,),5:10] <- 0 # assign ethnicity values to 0 in rows where no population is registered
census[which(is.na(census$European),), "European"] <- 0
census[which(is.na(census$Maori),), "Maori"] <- 0
census[which(is.na(census$Pacific),), "Pacific"] <- 0
census[which(is.na(census$Asian),), "Asian"] <- 0
census[which(is.na(census$MiddleEasternLatinAmericanAfrican),), "MiddleEasternLatinAmericanAfrican"] <- 0
census[which(is.na(census$OtherEthnicity),), "OtherEthnicity"] <- 0

#calculate percentages ethnicity
census <- census |>
  mutate(pcEuropean = European/pop_usual, na.rm=T) |> 
  mutate(pcMaori = Maori/pop_usual, na.rm=T) |> 
  mutate(pcPacific = Pacific/pop_usual, na.rm=T) |> 
  mutate(pcAsian = Asian/pop_usual, na.rm=T) |> 
  mutate(pcMiddleEasternLatinAmericanAfrican = MiddleEasternLatinAmericanAfrican/pop_usual, na.rm=T) |> 
  mutate(pcOtherEthnicity = OtherEthnicity/pop_usual, na.rm=T)

# again clean rows where population is zero
census[which(is.na(census$pcEuropean),), "pcEuropean"] <- 0
census[which(is.na(census$pcMaori),), "pcMaori"] <- 0
census[which(is.na(census$pcPacific),), "pcPacific"] <- 0
census[which(is.na(census$pcAsian),), "pcAsian"] <- 0
census[which(is.na(census$pcMiddleEasternLatinAmericanAfrican),), "pcMiddleEasternLatinAmericanAfrican"] <- 0
census[which(is.na(census$pcOtherEthnicity),), "pcOtherEthnicity"] <- 0
census[which(census$pcEuropean>1), "pcEuropean"] <- 1 # 3 rows where white pop is larger than total pop

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
census$shannon <- apply(census[,12:17], 1, shannon)

sa1_all <- left_join(sa1_base, census, by = c("SA12018_V1_00"="code"))
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
sa1_crime <- st_read("data/safety/crime/sa1_crimes_final.gpkg")
sa1_crime <- st_transform(sa1_crime, 27291)
sa1_crime <- sa1_crime |>
  subset(select = c(SA12018_V1_00, crime_perarea))
sa1_crime[which(is.na(sa1_crime$crime_perarea),), "crime_perarea"] <- 0

sa1_all <- st_join(sa1_all, sa1_crime, by = c("SA12018_V1_00"="SA12018_V1_00"))

##### Road Safety ####
crashes_sa1 <- st_read("data/safety/crash/sa1_cents_crashes.gpkg")
crashes_sa1 <- st_transform(crashes_sa1, 27291)
crashes_sa1 <- crashes_sa1 |>
  subset(select = c(SA12018_V1_00, Count_fatal_crashes_per_area))
names(crashes_sa1)[names(crashes_sa1) == "Count_fatal_crashes_per_area"] <- "fatalcrashes_per"
crashes_sa1[which(is.na(crashes_sa1$crime_perarea),), "Count_fatal_crashes_per_area"] <- 0

sa1_all <- st_join(sa1_all, crashes_sa1, by = c("SA12018_V1_00"="SA12018_V1_00"))

##### Floods ####
floods_sa1 <- st_read("data/safety/floods/sa1_cents_floods.gpkg")
floods_sa1 <- st_transform(floods_sa1, 27291)
floods_sa1 <- floods_sa1 |>
  subset(select = c(SA12018_V1, sa1s_floodproneareas...auckland_urba_sa1s_pr_flood))
names(floods_sa1)[names(floods_sa1) == "sa1s_floodproneareas...auckland_urba_sa1s_pr_flood"] <- "floodprone_prc"
floods_sa1[which(is.na(floods_sa1$floodprone_prc),), "floodprone_prc"] <- 0
sa1_all <- st_join(sa1_all, floods_sa1, by = c("SA12018_V1_00"="SA12018_V1"))

##### Alcohol Environments ####
alco_sa1 <- st_read("data/safety/alcoholenvs/sa1_cents_alcoenvs.gpkg")
alco_sa1 <- st_transform(alco_sa1, 27291)
alco_sa1 <- alco_sa1 |>
  subset(select = c(SA12018_V1, alcoprohibited))
alco_sa1[which(is.na(alco_sa1$alcoprohibited),), "alcoprohibited"] <- 0
sa1_all <- st_join(sa1_all, alco_sa1, by = c("SA12018_V1_00"="SA12018_V1"))

#### Rest ####
summary(sa1_all)
# plot
tm_shape(sa1_all) +
  tm_polygons(col="crime_perarea", lwd=0)


#imputation by neighbouring values - not working yet
#sa1_all <- mutate(sa1_all, dampness = as.numeric(as.character(dampness)))
#sa1 <- SpatialPoints(sa1)
#knn5 <- knn2nb(knearneigh(sa1, k = 5))
#sapply(1:length(knn5), function(N){mean(dampness[N])})
#sa1s_na_damp <- sa1_all[which(is.na(sa1_all$dampness)),]
#sa1s_na_damp$dampness




#### Spatial Interpolation ####
grid <- st_sample(sa1, 10000, type = "regular")
grid <- st_transform(grid, 27291)
grid <- st_as_sf(grid)
#st_write(grid, "grid_10000.gpkg")

tmap_mode("view")
tm_shape(grid) +
  tm_dots(col="grey") +
  tm_shape(stations) +
  tm_dots("black")

# IDW for census variables
idw_income <- idw(formula = median_income~1, locations = sa1_all, 
               newdata = grid, idp = 1, nmax=10, na.action=na.pass)
idw_hs <- idw(formula = no_households~1, locations = sa1_all, 
                  newdata = grid, idp = 1, nmax=10, na.action=na.pass)
idw_maori <- idw(formula = maori_pr~1, locations = sa1_all, 
                newdata = grid, idp = 1, nmax=10, na.action=na.pass)
idw_damp <- idw(formula = dampness~1, locations = sa1_all, 
                 newdata = grid, idp = 1, nmax=10, na.action=na.pass)

tm_shape(idw_income) +
  tm_dots(col="var1.pred", style="kmeans") +
  tm_basemap("OpenStreetMap")

idw_joined <- cbind(idw_income, idw_hs, idw_maori, idw_damp)
idw_joined = subset(idw_joined, select = -c(var1.var, var1.var.1, var1.var.2,
                                            var1.var.3, geometry.1, geometry.2,geometry.3))

colnames(idw_joined) <- c("income", "no_households", "maori_pr", "dampness", "geometry")
st_write(idw_joined,"data/grid_auckland_census_10000.gpkg")

# bind results with original points
pts.wit.dist <- cbind(pts, dist.mat)
pts.wit.dist[1:3,]