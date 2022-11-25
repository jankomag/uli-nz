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
#library(gstat)
#library(rgeos)

#### Data imports ####
# geographic data
#sa1_dist <- st_read("data/geographic/sa1_allNetDist.gpkg")
sa1_base <- st_read("data/geographic/sa1_centroid_base.gpkg")
#transforming to the same coordinate system
#sa1_dist <- st_transform(sa1_dist, 27291)
sa1_base <- st_transform(sa1_base, 27291)

sa1_base <- sa1_base |>
  subset(select = c(SA12018_V1, LAND_AREA_, AREA_SQ_KM, geom))

# census variables
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

#cleaning missing values for ethnicity
census[which(is.na(census$pcEuropean),), "pcEuropean"] <- 0
census[which(is.na(census$pcMaori),), "pcMaori"] <- 0
census[which(is.na(census$pcPacific),), "pcPacific"] <- 0
census[which(is.na(census$pcAsian),), "pcAsian"] <- 0
census[which(is.na(census$pcMiddleEasternLatinAmericanAfrican),), "pcMiddleEasternLatinAmericanAfrican"] <- 0
census[which(is.na(census$pcOtherEthnicity),), "pcOtherEthnicity"] <- 0

sa1_cen_spatial <- left_join(sa1_base, census, by = c("SA12018_V1"="code"))
tm_shape(sa1_cen_spatial) +
  tm_dots(col="pcEuropean", breaks="equal")

ggplot(census) +
  geom_density(aes(x=pcMaori))

# Compute Diversity Index #
p <- census[,12:17]
p <- as.vector(t(p))

shannon(p)
df1 <- census |> 
  mutate(diversityIndex = sapply(as.vector(t(census[,12:17])), shannon))


#shannon function testing
shannon <- function(p){
  if (0 %in% p) {
    p = replace(p,p==0,0.0001)
  } else {
    p
  }
  H = -sum(p*log(p))
  df1 <- mutate(df1, shIdx = H)
  return (df1)
}
shannon(p)

#shannon function working
shannon <- function(p){
  if (0 %in% p) {
    p = replace(p,p==0,0.0001)
  } else {
      p
    }
  H = -sum(p*log(p))
  return (H)
}
shannon(p)


# Compute crime measure
crime <- as.data.frame(read.csv("data/safety/crime/crime_agged_mesh.gpkg"))

# sum all cases by area
cases_agg <- aggregate(crime["newCasesBySpecimenDateRollingSum"], by=df["areaCode"], sum)
head(cases_agg)
#write.csv(cases_agg, "data/cases.csv")



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