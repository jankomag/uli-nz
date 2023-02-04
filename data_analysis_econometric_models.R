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
