library("dplyr")
library("ggplot2")
library("lubridate")
library("tseries")

# importing data
df <- read.csv("data/msoa_2022-07-13.csv")
summary(df)
head(df)

# Changing date to be a date variable
df$date <- as.Date(df$date, "%Y-%m-%d")
head(df)
# plot Rolling Sum
ggplot(df, aes(date, newCasesBySpecimenDateRollingSum)) +
  geom_line()

# sum all cases by area
cases_agg <- aggregate(df["newCasesBySpecimenDateRollingSum"], by=df["areaCode"], sum)
head(cases_agg)
#write.csv(cases_agg, "data/cases.csv")

# join with population data
population <- read.csv("data/population.csv")
df <- left_join(cases_agg, population, by=c("areaCode"="MSOA_Code"))
# calculate cases by population
df$case_per_100 <- (df$newCasesBySpecimenDateRollingSum / df$population)
head(df)

# add all other population variables
allvars <- read.csv("data/allvariables.csv")
allvars <- left_join(allvars, df, by=c("code"="areaCode"))
#write.csv(allvars, "data/final_all.csv")

df <- read.csv("data/final_all.csv")

#### REGRSSION ANALYSIS####
# 1) LINEAR REGRESSION
head(df)
reg1 <- lm(case_per_100 ~ Asian_prop + limiLots_prop + income + 
     pop_dens + carstairs + AB_prop + Unemployment +
     no.car.ownership + mean_age, data = df)

summary(reg1)

# 2) SPATIAL ECONOMETRICS
library("spdep")
library("rgdal")
library(rgeos)
library(stargazer)
library(spatialreg)

england <- readOGR(dsn = "data/", layer = "spatial")

queen.nb <- read.gal("data/queen.gal", region.id=england$code)
summary(queen.nb)
listw1 <- nb2listw(queen.nb)

# Moran's I
moran(england$case_per_1, nb2listw(queen.nb), length(england$case_per_1), Szero(nb2listw(queen.nb)))

moran.test(england$case_per_1, nb2listw(queen.nb))

# OLS Regression
reg.eq1 <- case_per_1~White_prop+mean_age+
  limiLots_p+income+pop_dens+carstairs+AB_prop+no.car.own

options(scipen = 7)

reg1 <- lm(reg.eq1, england)
summary(reg1)

# Residual test
lm.morantest(reg1,listw1) # H0 says no spatial correlation in the residuals

# LaGrenge Multiplier Tests
lm.LMtests(reg1, listw1, test='all') #error and Spatial Lag model (lag y)

# Spatially Lagged X (lag X -SLX)
reg2 <- lmSLX(reg.eq1, england, listw1)
summary(reg2)

# Spaitally Lagged y Model (Autoregressive)
reg3 <- lagsarlm(reg.eq1, england, listw1)

# Spatial error model
reg4 <- errorsarlm(reg.eq1, england, listw1)
summary(reg4)
impacts(reg4, listw1)
summary(impacts(reg4,listw1, R=500), zstat=TRUE)

###### Spatial Error Model #####
sp_err_sa1 <- errorsarlm(formula, data = gdf, sa1.lw, method = "eigen")
save(sp_err_sa1, file = "outputs/models/sp_err_sa1.Rdata")
load("outputs/models/sp_err_sa1.Rdata")
tidy(sp_err_sa1)
sp_err_sa2 <- errorsarlm(formula, data = sa2agg, sa2.lw, method = "eigen")

lrtest(lm_sa2, sp_err_sa2)

gdf$residuals_sperr <- residuals(sp_err_sa1)
sa2agg$residuals_sperr <- residuals(sp_err_sa2)

tm_shape(gdf) + 
  tm_polygons(col='residuals_sperr',lwd=0, style="jenks")
tm_shape(sa2agg) + 
  tm_polygons(col='residuals_sperr',lwd=0, style="jenks")

# Plot the residuals
dfresid_sperr_sa1 <- data.frame(Fitted = sp_err_sa1$fitted.values, Residuals = sp_err_sa1$residuals)
ggplot(dfresid_sperr_sa1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

dfresid_sperr_sa2 <- data.frame(Fitted = sp_err_sa2$fitted.values, Residuals = sp_err_sa2$residuals)
ggplot(dfresid_sperr_sa2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# compare model LM
summary(lm.LMtests(lm_sa2, sa2.lw, test=c("LMlag","LMerr")))

###### KULI weighted by population ethnicities #####popgdf <- left_join(sa1_polys, popdf, by = c("SA12018_V1_00"="SA12018_V1_00"))summary(popgdf)popgdf <- popgdf %>%  mutate(    European_kuli_avg = (((European * kuli_MPIAgg)/European)-1),    Maori_kuli_avg = (Maori * kuli_MPIAgg),    Pacific_kuli_avg = (Pacific * kuli_MPIAgg),    Asian_kuli_avg = (Asian * kuli_MPIAgg),    MiddleEasternLatinAmerican_kuli_avg = (MiddleEasternLatinAmericanAfrican * kuli_MPIAgg) ,    OtherEthnicity_kuli_avg = (OtherEthnicity * kuli_MPIAgg)    )# preview variablestm_shape(popgdf) +  tm_fill("European_kuli_avg")sum(popgdf$European_kuli_avg)/sum(popgdf$European)sum(popgdf$Pacific_kuli_avg)/sum(popgdf$Pacific)sum(popgdf$Asian_kuli_avg)/sum(popgdf$Asian)sum(popgdf$Maori_kuli_avg)/sum(popgdf$Maori)sum(popgdf$MiddleEasternLatinAmerican_kuli_avg)/sum(popgdf$MiddleEasternLatinAmericanAfrican)sum(popgdf$OtherEthnicity_kuli_avg)/sum(popgdf$OtherEthnicity)#### Kuli Normalised ####cbd <- st_read("data/sa1_cbddist.gpkg") |>  dplyr::select(SA12018_V1_00,dist_cbd_point.2) |>   st_drop_geometry()cbdsa1 <- left_join(kuli, cbd)minmaxNORM <- function(x) {  return (((x - min(x))) / (max(x) - min(x))*(10-1)+1)} #1-10sa1_census$area <- as.numeric(st_area(sa1_census))sa1_census$popdens <- sa1_census$popUsual / sa1_census$areasa1_census$popdenlog <- log(sa1_census$popdens+0.0001)sa1_census <- st_drop_geometry(sa1_census)cbdsa1 <- left_join(cbdsa1, sa1_census)kulinorm <- cbdsa1 |>   mutate(cbdistnorm = minmaxNORM(dist_cbd_point.2)) |>   mutate(kuli_by_distCBD = kuli_MPIAgg/cbdistnorm) |>   mutate(kuli_by_popdens = kuli_MPIAgg/minmaxNORM(popdenlog))tm_shape(kulinorm) +  tm_fill("kuli_by_popdens", style="jenks", lwd=0,n=6)