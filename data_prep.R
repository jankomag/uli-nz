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
library(rgoes)

#### Data imports ####
# geographic data
sa1_polys <- st_read("data/geographic/urban_sa1_landvalid.gpkg")
#transforming to the same coordinate system
#sa1_dist <- st_transform(sa1_dist, 27291)
sa1_base <- st_transform(sa1_polys, 27291) |> st_drop_geometry()

sa1_base <- sa1_base |>
  subset(select = c(SA12018_V1_00, area))

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
# find NAs
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

# NAs bade on neighbours
sa1_geo_impute <- left_join(sa1_polys, census, by = c("SA12018_V1_00"="code"))
tm_shape(sa1_geo_impute) + tm_polygons(col="dampness", lwd=0)

sa1.nb <- poly2nb(sa1_geo_impute)

idx <- which(is.na(sa1_geo_impute$dampness))
nongeosa1_imp <- st_drop_geometry(sa1_geo_impute)

#impute neigbouring values
for (i in idx) {
  neigidx <- sa1.nb[[idx[i]]]
  nongeosa1_imp[i, "dampness"] <- mean(nongeosa1_imp[neigidx, "dampness"])
}
geosa1_imputed <- left_join(sa1_polys, nongeosa1_imp, by = c("SA12018_V1_00"="SA12018_V1_00"))
tm_shape(geosa1_imputed) + tm_polygons(col="dampness", lwd=0)
summary(census)


## edit the islands
g.nb[[101]] = as.integer(100)
g.nb[[125]] = as.integer(c(247, 124, 123))
g.nb[[126]] = as.integer(127)
g.nb[[171]] = as.integer(c(165, 174))
g.nb[[174]] = as.integer(171)
g.nb[[179]] = as.integer(165)

g.lw = nb2listw(g.nb)


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
sa1_crime <- st_read("data/safety/crime/sa1_crimes_final.gpkg") |> st_drop_geometry()
sa1_crime <- sa1_crime |>
  subset(select = c(SA12018_V1_00, crime_perarea))

sa1_all <- left_join(sa1_all, sa1_crime, by = c("SA12018_V1_00"="SA12018_V1_00"))
sa1_all[which(is.na(sa1_all$crime_perarea),), "crime_perarea"] <- 0

##### Road Safety ####
crashes_sa1 <- st_read("data/safety/crash/sa1_cents_crashes.gpkg") |> st_drop_geometry()
crashes_sa1 <- crashes_sa1 |>
  subset(select = c(SA12018_V1, Count_fatal_crashes_per_area))
names(crashes_sa1)[names(crashes_sa1) == "Count_fatal_crashes_per_area"] <- "fatalcrashes_per"

sa1_all <- left_join(sa1_all, crashes_sa1, by = c("SA12018_V1_00"="SA12018_V1"))
sa1_all[which(is.na(sa1_all$fatalcrashes_per),), "fatalcrashes_per"] <- 0

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

#### Rest ####
sa1_dists <- st_read("data/geographic/allsa1_dist.gpkg")|> st_drop_geometry()
sa1_all <- left_join(sa1_all, sa1_dists, by = c("SA12018_V1_00"="SA12018_V1_00"))

sa1_allg <- left_join(sa1_polys, sa1_all, by=c("SA12018_V1_00"="SA12018_V1_00"))
sa1_allg <- sa1_allg |> 
  subset(select = -c(LANDWATER, LANDWATER_NAME, LAND_AREA_SQ_KM, AREA_SQ_KM, Shape_Length, fid_2, TA2018_V1_, TA2018_V_1, LAND_AREA_, AREA_SQ_KM_2, Shape_Leng, area.y, na.rm))

# plot
tm_shape(sa1_allg) +
  tm_polygons(col="dist_secondary", lwd=0)

st_write(sa1_allg, "data/geographic/sa1_allvars.gpkg")

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