# load the packages
library(sf)
library(sp)
library(tmap)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(geosphere)
library(spdep)
#library(gstat)
#library(VIM)
#library(rgeos)

#### Data imports ####
# geographic data
#sa1_dist <- st_read("data/geographic/sa1_allNetDist.gpkg")
sa1_base <- st_read("data/geographic/sa1_centroids_base.gpkg")
#transforming to the same coordinate system
sa1_dist <- st_transform(sa1_dist, 27291)
sa1_base <- st_transform(sa1_base, 27291)

sa1_base <- sa1_base |>
  subset(select = c(SA12018_V1, LAND_AREA_, AREA_SQ_KM, geom))
summary(sa1_base)

# census variables
census <- read.csv("data/geographic/Census/auckland_census.csv")
census <- census |>
  mutate(code = as.character(code))
sa1_all <- left_join(sa1_base, census, by = c("SA12018_V1"="code"))

sa1_all <- sa1_all |>
  mutate(European = as.numeric(European), na.rm=T) |> 
  mutate(Māori = as.numeric(Māori), na.rm=T) |>
  mutate(Pacific = as.numeric(Pacific), na.rm=T) |>
  mutate(Asian = as.numeric(Asian), na.rm=T) |>
  mutate(MiddleEasternLatinAmericanAfrican = as.numeric(MiddleEasternLatinAmericanAfrican), na.rm=T) |>
  mutate(OtherEthnicity = as.numeric(OtherEthnicity), na.rm=T) |>
  mutate(pop_usual = as.numeric(pop_usual), na.rm=T) |> 
  mutate(pcEuropean = European/pop_usual, na.rm=T) |> 
  mutate(pcMāori = Māori/pop_usual, na.rm=T) |> 
  mutate(pcPacific = Pacific/pop_usual, na.rm=T) |> 
  mutate(pcMiddleEasternLatinAmericanAfrican = MiddleEasternLatinAmericanAfrican/pop_usual, na.rm=T) |> 
  mutate(pcOtherEthnicity = OtherEthnicity/pop_usual, na.rm=T)
  

ggplot(sa1_all) +
  geom_line(aes(x=lat, y=lon), col="Māori")
  
#test function
shannons <- function(x){
  x = -sum(0.5*log(0.5))
  return (x)
}
curve(shannons(1), from=1, to=50, xlab="x", ylab="y")



#### Adding Census vars ####
census <- census %>%
  mutate(code = as.character(code)) |> 
  mutate(no_households = as.numeric(no_households)) |>
  mutate(born_overseas = as.numeric(born_overseas)) |>
  mutate(European = as.numeric(European)) |>
  mutate(Māori = as.numeric(Māori)) |>
  mutate(Pacific = as.numeric(Pacific)) |>
  mutate(Asian = as.numeric(Asian)) |>
  mutate(MiddleEasternLatinAmericanAfrican = as.numeric(MiddleEasternLatinAmericanAfrican)) |>
  mutate(OtherEthnicity = as.numeric(OtherEthnicity)) |>
  mutate(median_income = as.numeric(median_income)) |>
  mutate(maori_desc = as.numeric(maori_desc)) |>
  mutate(pop_usual = as.numeric(pop_usual)) |>
  mutate(maori_pr = maori_desc/pop_usual) |> 
  mutate(dampness = as.numeric(as.factor(dampness)))


sa1_all <- left_join(sa1_base, census, by = c("SA12018_V1"="code"))
# Compute crime measure

# Compute diversity measure

##### Impute missing values #####
summary(aggr(census))
sa1_all$maori_pr[is.na(sa1_all$maori_pr)] <- 0
sa1_all$median_income[is.na(sa1_all$median_income)] <- 0


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