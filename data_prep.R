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
library(spatstat)

#### Data imports ####
# geographic data
sa1_polys <- st_read("data/sa1_auckland_waiheke_urban.gpkg") |>
  subset(select = c(SA12018_V1_00)) |> 
  st_transform(27291) #transforming to the same coordinate system
sa1_base <- sa1_polys |> st_drop_geometry()

#### Census ####
census <- as.data.frame(read.csv("data/auckland_census.csv"))
census <- census |>
  subset(select = -c(maori_desc, median_income, born_overseas, PacificNum, Degree)) |> 
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
aggr_plot <- aggr(census, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(census), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

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
census[which(census$pcEuropean>1), "pcEuropean"] <- 1 # 3 rows where european pop is larger than total pop

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
# any remaining missing values replaced with global mean
sa1_imped[which(is.na(sa1_imped$medianRent),), "medianRent"] <- mean(sa1_imped$medianRent, na.rm=T)
census <- st_drop_geometry(sa1_imped) |> 
  subset(select = -c(na.rm, European,Maori, Pacific, Asian, MiddleEasternLatinAmericanAfrican, OtherEthnicity))

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
sa1_all <- select(sa1_all, SA12018_V1_00, no_households, pop_usual, dampness, medianRent, shannon)
##### Crime Risk ####
# load raw crime counts data
crime <- as.data.frame(read.csv("data/crime_23.csv"))|> 
  select(Area.Unit, Victimisations, Meshblock, Weapon, ANZSOC.Subdivision, ANZSOC.Group)
crime$Area.Unit = substr(crime$Area.Unit,1,nchar(crime$Area.Unit)-1) # clean Area unit field
crime <- crime %>%
  group_by(Area.Unit) %>%
  summarise(Sum_Victimisations = sum(Victimisations)) |> #aggregate across all types of crimes
  as.data.frame()

# load Area Unit spatial Data
au <- st_read("data/area-unit-2017-generalised-version.gpkg") |> 
  select(AU2017_NAME, LAND_AREA_SQ_KM) |> 
  st_transform(27291) #transforming to the same coordinate system
au$area <- st_area(au)

# Join crime data with spatial data
crimes <- left_join(au, crime, by = c("AU2017_NAME" = "Area.Unit")) %>%
  mutate(across("Sum_Victimisations", ~replace(., is.na(.), 0))) |> #replace areas with no crimes reported with 0
  mutate(crimerisk = as.numeric(Sum_Victimisations/area)) # calculate crimes by area measure
sa1_crime <- st_join(sa1_polys, crimes, by = FALSE) |> st_drop_geometry() |> # spatial join with to SA1
  select(SA12018_V1_00, crimerisk) |> 
  group_by(SA12018_V1_00) |> 
  summarize(crimerisk = mean(crimerisk))

# add to sa1_all
sa1_all <- left_join(sa1_all, sa1_crime, by = "SA12018_V1_00")

##### Road Safety ####
crash <- st_read("data/Crash_Analysis_System_(CAS)_data.geojson") |> 
  st_transform(27291) #transforming to the same coordinate system
# Option 1) crash risk = no of crashes / area
sa1_polys$area <- as.numeric(st_area(sa1_polys))
sa1_crash <- st_join(sa1_polys, crash)
sa1_crash <- sa1_crash %>%
  group_by(SA12018_V1_00) %>%
  mutate(crash_count = n()) %>%
  ungroup() %>%
  select(SA12018_V1_00, area, crash_count, geom) |> 
  mutate(crash_risk = crash_count / area)

# Option 2) Distance weighted method
sa1_centroids <- st_centroid(sa1_crash)
sa1_buffers <- st_buffer(sa1_centroids, dist = 1000)
sa1_pp <- as.ppp(sa1_centroids)
crash_pp <- as.ppp(crash)
density <- density(crash_pp, sigma = 1000)
sa1_crash$crash_risk <- as.vector(crash_kernel[sa1_pp])

sa1_crash <- left_join(sa1_crash, census[,c("SA12018_V1_00","pop_usual")], by="SA12018_V1_00") # add population column
sa1_crash <- sa1_crash |> 
  mutate(popdens = pop_usual/area) |> # get population density
  mutate(crash_risk_weighted = ifelse(popdens == 0, crash_risk,crash_risk / popdens)) # TO BE CHANGED

sa1_crash <- st_drop_geometry(sa1_crash) |> 
  select(SA12018_V1_00, crash_risk, crash_risk_weighted) |> 
  group_by(SA12018_V1_00) |>
  summarize(
    crash_risk = mean(crash_risk),
    crash_risk_weighted = mean(crash_risk_weighted))
  
tm_shape(sa1_crash) +
  tm_fill(col="crash_risk_weighted", style="jenks", lwd=0)

sa1_all <- left_join(sa1_all, sa1_crash, by = c("SA12018_V1_00"="SA12018_V1_00")) #join to full data

#### Remaining Variables ####
## Flood Proneness ##
floods_sa1 <- st_read("data/sa1_floods_final.gpkg") |> st_drop_geometry() |>
  select(SA12018_V1_00, flood_pc)
floods_sa1[which(is.na(floods_sa1$flood_pc),), "flood_pc"] <- 0 # replace NAs with 0
sa1_all <- left_join(sa1_all, floods_sa1, by = c("SA12018_V1_00"="SA12018_V1_00"))

## Alcohol Environments ##
alco_sa1 <- st_read("data/sa1_cents_alcoenvs.gpkg") |> st_drop_geometry() |> 
  select(SA12018_V1, alcoprohibited)

sa1_all <- left_join(sa1_all, alco_sa1, by = c("SA12018_V1_00"="SA12018_V1"))
sa1_all[which(is.na(sa1_all$alcoprohibited),), "alcoprohibited"] <- 0

## Street Connectivity ##
stconnectivity_sa1 <- st_read("data/streetconnectivity_new.gpkg") |> st_drop_geometry()|>
  select(SA12018_V1_00, streetconn)
sa1_all <- left_join(sa1_all, stconnectivity_sa1, by = c("SA12018_V1_00"="SA12018_V1_00"))

## Bikeability ##
sa1_bikeability <- st_read("data/bikeability.gpkg")|> st_drop_geometry()
sa1_all <- left_join(sa1_all, sa1_bikeability, by = c("SA12018_V1_00"="SA12018_V1_00"))

## Dwelling Density ##
sa1_dwelldens <- st_read("data/newdwellingdensity.gpkg")|> st_drop_geometry()
sa1_all <- left_join(sa1_all, sa1_dwelldens, by = c("SA12018_V1_00"="SA12018_V1_00"))

#### Distances ####
sa1_dists <- st_read("data/sa1_out_dist.gpkg")|> st_drop_geometry()
sa1_all <- left_join(sa1_all, sa1_dists, by = c("SA12018_V1_00"="SA12018_V1_00"))

#join with spatial
sa1_allg <- left_join(sa1_polys, sa1_all, by=c("SA12018_V1_00"="SA12018_V1_00"))

sa1_allg[which(is.infinite(sa1_allg$dist_stations),), "dist_stations"] <- 100000
sa1_allg[which(is.infinite(sa1_allg$dist_childcare),), "dist_childcare"] <- 100000
sa1_allg[which(is.infinite(sa1_allg$dist_hospital),), "dist_hospital"] <- 100000
sa1_allg[which(is.infinite(sa1_allg$dist_chemist),), "dist_chemist"] <- 100000

st_write(sa1_allg, "data/geographic/sa1_allvars.gpkg")