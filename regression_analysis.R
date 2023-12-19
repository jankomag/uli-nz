## Load packages
library(sf)
library(tmap)
library(MASS)
library(GWmodel)
library(spdep)
library(ggplot2)
library(ggthemes)
library(envalysis)
library(grid)
library(cowplot)
library(mice)
library(reshape2)
library(corrr)
library(Hmisc)
library(corrplot)
library(car)
library(gclus)
library(corrr)
library(tidyr)
library(cowplot)
library(stringr)
library(DescTools)
library(dplyr)
library(MuMIn)
library(VIM)
library(spdep)
library(rgdal)
library(stargazer)
library(spatialreg)
library(rgeoda)
library(geosphere)
library(raster)
library(rgbif)
library(viridis)
library(gridExtra)
library(rasterVis)
library(ggspatial)
library(spdep)
library(gstat)
library(geodaData)
library(data.table)
library(collapse)
library(cluster)
library(see)
library(lmtest)
library(tidyverse)
library(tidymodels)

#### Loading data ####
# load the kuli data
kuli <- st_read('data/sa1_kuli.gpkg', quiet = T)
sa1_polys <- st_read("data/sa1_auckland_waiheke_urban.gpkg") |>
  subset(select = c(SA12018_V1_00)) |> 
  st_transform(27291)
nongkuli <- st_drop_geometry(kuli) |> 
  dplyr::select(SA12018_V1_00, kuli_MPIAgg)

#load additional data
census <- read.csv('data/auckland_census_2.csv')
census <- census |>
  subset(select = -c(European, Maori, Pacific, Asian, MiddleEasternLatinAmericanAfrican, numberdriveToWork, OtherEthnicity, PacificNum, medianRent)) |> 
  mutate(code = as.character(code)) |> 
  mutate(bornOverseas = as.numeric(bornOverseas)) |> 
  mutate(privateTransporTtoWork = as.numeric(privateTransporTtoWork)) |>
  mutate(PTtoWork = as.numeric(PTtoWork)) |>
  mutate(cycleToWork = as.numeric(cycleToWork)) |>
  mutate(noCar = as.numeric(noCar)) |>
  mutate(carsPerPreson = as.numeric(carsPerPreson)) |> 
  mutate(PrEuropeanDesc = as.numeric(PrEuropeanDesc)) |> 
  mutate(PrMaoriDesc = as.numeric(PrMaoriDesc)) |> 
  mutate(Degree = as.numeric(Degree))

#load deprivation data
deprivation <- read.csv('data/deprivation_joined.csv') |>
  mutate(SA12018_V1_00 = as.character(SA12018_V1_00))
census <- left_join(census, deprivation, by = c("code"="SA12018_V1_00"))

#load age data
age <- read.csv('data/auckland_census_3.csv') |> 
  dplyr::select(medianage,code) |> 
  mutate(code=as.character(code)) |> 
  mutate(medianage=as.numeric(medianage))
census <- left_join(census, age, by = c("code"="code"))

#### Missing Vlaues Imputation ####
# Dealing with missing values #
md.pattern(census)
aggr_plot <- aggr(census, col=c('navyblue','orange'), numbers=TRUE, sortVars=TRUE, labels=names(census), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

sa1_census <- left_join(sa1_polys, census, by = c("SA12018_V1_00"="code"))
sa1_census$bornOverseas <- (sa1_census$bornOverseas/sa1_census$popUsual)
tmap_mode("plot")
tm_shape(sa1_census) + tm_polygons(col="bornOverseas", lwd=0, style="kmeans")

#### NN impute - impute NAs based on neighbouring values
index <- st_touches(sa1_census, sa1_census)
sa1_imped <- sa1_census %>% 
  mutate(maoriDescent = ifelse(is.na(maoriDescent),
                           apply(index, 1, function(i){mean(.$maoriDescent[i], na.rm=T)}),
                           maoriDescent))
sa1_imped <- sa1_imped %>% 
  mutate(medianIncome = ifelse(is.na(medianIncome),
                             apply(index, 1, function(i){mean(.$medianIncome[i], na.rm=T)}),
                             medianIncome))
sa1_imped <- sa1_imped %>% 
  mutate(bornOverseas = ifelse(is.na(bornOverseas),
                               apply(index, 1, function(i){mean(.$bornOverseas[i], na.rm=T)}),
                               bornOverseas))
sa1_imped <- sa1_imped %>% 
  mutate(PrEuropeanDesc = ifelse(is.na(PrEuropeanDesc),
                               apply(index, 1, function(i){mean(.$PrEuropeanDesc[i], na.rm=T)}),
                               PrEuropeanDesc))
sa1_imped <- sa1_imped %>% 
  mutate(privateTransporTtoWork = ifelse(is.na(privateTransporTtoWork),
                               apply(index, 1, function(i){mean(.$privateTransporTtoWork[i], na.rm=T)}),
                               privateTransporTtoWork))
sa1_imped <- sa1_imped %>% 
  mutate(PTtoWork = ifelse(is.na(PTtoWork),
                               apply(index, 1, function(i){mean(.$PTtoWork[i], na.rm=T)}),
                           PTtoWork))
sa1_imped <- sa1_imped %>% 
  mutate(cycleToWork = ifelse(is.na(cycleToWork),
                               apply(index, 1, function(i){mean(.$cycleToWork[i], na.rm=T)}),
                              cycleToWork))
sa1_imped <- sa1_imped %>% 
  mutate(noCar = ifelse(is.na(noCar),
                               apply(index, 1, function(i){mean(.$noCar[i], na.rm=T)}),
                        noCar))
sa1_imped <- sa1_imped %>% 
  mutate(carsPerPreson = ifelse(is.na(carsPerPreson),
                        apply(index, 1, function(i){mean(.$carsPerPreson[i], na.rm=T)}),
                        carsPerPreson))
sa1_imped <- sa1_imped %>% 
  mutate(PrMaoriDesc = ifelse(is.na(PrMaoriDesc),
                                apply(index, 1, function(i){mean(.$PrMaoriDesc[i], na.rm=T)}),
                              PrMaoriDesc))
sa1_imped <- sa1_imped %>% 
  mutate(deprivation = ifelse(is.na(deprivation),
                              apply(index, 1, function(i){mean(.$deprivation[i], na.rm=T)}),
                              deprivation))
sa1_imped <- sa1_imped %>% 
  mutate(Degree = ifelse(is.na(Degree),
                              apply(index, 1, function(i){mean(.$Degree[i], na.rm=T)}),Degree))
sa1_imped <- sa1_imped %>% 
  mutate(medianage = ifelse(is.na(medianage),
                         apply(index, 1, function(i){mean(.$medianage[i], na.rm=T)}),medianage))

sa1_imped[which(is.na(sa1_imped$privateTransporTtoWork),), "privateTransporTtoWork"] <- mean(sa1_imped$privateTransporTtoWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$PTtoWork),), "PTtoWork"] <- mean(sa1_imped$PTtoWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$cycleToWork),), "cycleToWork"] <- mean(sa1_imped$cycleToWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$noCar),), "noCar"] <- mean(sa1_imped$noCar, na.rm=T)
sa1_imped[which(is.na(sa1_imped$carsPerPreson),), "carsPerPreson"] <- mean(sa1_imped$carsPerPreson, na.rm=T)
sa1_imped[which(is.na(sa1_imped$Degree),), "Degree"] <- mean(sa1_imped$Degree, na.rm=T)
sa1_imped[which(is.na(sa1_imped$medianage),), "Degree"] <- mean(sa1_imped$medianage, na.rm=T)

# calculate population density measure
sa1_imped$area <- as.numeric(st_area(sa1_imped))
sa1_imped$popdens <- sa1_imped$popUsual / sa1_imped$area
sa1_imped$popdenlog <- log(sa1_imped$popdens+0.0001)

# select columns for regression
sa1_reg <- sa1_imped |> 
  dplyr::select(SA12018_V1_00, medianIncome, bornOverseas, privateTransporTtoWork, PTtoWork, cycleToWork, noCar, carsPerPreson,
         PrEuropeanDesc, PrMaoriDesc, Degree, deprivation, medianage, popdenlog)

gdf <- left_join(sa1_imped, nongkuli, by = c("SA12018_V1_00"="SA12018_V1_00"))
df <- st_drop_geometry(gdf)
tm_shape(gdf) + tm_fill("medianage", style="jenks")

#### EDA ####
numeric_data <- df[, sapply(df, is.numeric)]
cor <- cor(x = numeric_data, y = numeric_data, use="complete.obs", method="pearson")
par(mfrow = c(1, 1))
corrplot(cor, tl.srt = 45, type = "lower", method = "ellipse",
         order = "FPC", tl.cex = 0.8,
         tl.col = "black", diag = T, cl.cex=0.7,cl.offset=0.3)

ggplot(gdf) + geom_histogram(aes(medianage))

#### KULI Visualisation ####
### MAP
bbox_coords <- c(174.489515, -37.159816, 175.258558, -36.570920)
bbox <- matrix(c(bbox_coords[1], bbox_coords[2], bbox_coords[3], bbox_coords[4]), ncol = 2, byrow = TRUE)

bckgd <- st_read('data/land.gpkg', quiet = T) # transform to OSGB projection
tm_shape(gdf) +tm_polygons("grey",  lwd=0) +
tm_shape(bckgd) + tm_polygons(col="#DCDACB", lwd=0) +
tm_shape(gdf) +
  tm_polygons(col = "kuli_MPIAgg", midpoint = 6, legend.hist = T, lwd=0.01, n=14,
              style = "kmeans", title = "KULI in Auckland", title.fontfamily="serif",
              palette="Reds", n=6) +
#  tm_style("col_blind") +
  tm_layout(legend.position = c("left","bottom"), frame = F, legend.outside = F,
            legend.title.fontfamily = "serif", bg.color="#CCEAE5") +
            #legend.width=1, legend.height=1, legend.text.size=1,legend.title.size=1,
            #legend.bg.color="grey100", legend.bg.alpha=.7,
            #legend.hist.height=.2, legend.hist.width=.3, legend.hist.bg.color="grey90", legend.hist.bg.alpha=.4) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.6) +
  tm_compass(type = "4star", size = 2, position = c("left", "top"))

tmaptools::palette_explorer()

kulimap <- tm_shape(gdf) +
  tm_polygons("grey",  lwd=0.1) +
tm_shape(bckgd, bbox=bbox) + tm_polygons(col="#FFFFDD", lwd=0) +
tm_shape(gdf, bbox=bbox) + 
  tm_polygons(col = "kuli_MPIAgg",
              #fill.palette = "GnBu",
              #palette = c("lightyellow","orange","darkorange","red","darkred"),
              palette = rev(hcl.colors(9, "GnBu")),
              title="KULI",
              legend.hist = TRUE,
              lwd = 0, style="jenks", n=9) +
  tm_layout(main.title = "Kiwi Urban Liveability Index in Auckland", main.title.fontfamily = "serif",
            frame = FALSE, legend.title.fontfamily = "serif", main.title.size = 1.7,
            legend.outside = FALSE,
            legend.text.size = 0.00001,
            legend.hist.size = 0.7,
            legend.hist.width = 0.5,legend.hist.height = 0.2,
            bg.color="#AEC3AE", main.title.position = c('center', 'top'),
            legend.bg.color = "white",  # Set the background color
            legend.bg.alpha = 0.9 ) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.3) +
  tm_compass(type = "4star", size = 1, position = c("left", "top")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 10)
tmap_save(kulimap, filename = "outputs/kuli_map.png", width = 8, height = 8)

### Histogram
ggplot(gdf)+
    geom_histogram(aes(kuli_MPIAgg)) +
    theme_minimal() +
    theme(legend.position="right",
          text=element_text(size=13,  family="serif")) +
    ylab(" ") + xlab (" ")
numeric_data <- data[, sapply(data, is.numeric)]

#### Aggregation to SA2 ####
sa2 <- st_read('data/sa2.gpkg', quiet = T) |> 
  subset(select = c(SA22023_V1_00)) |> st_transform(27291)
sa2agg <- st_interpolate_aw(gdf[2:20], sa2, extensive = F)

tm_shape(sa2agg) + 
  tm_polygons("kuli_MPIAgg", palette = "YlGnBu", style="kmeans",
              lwd=.1) + tm_layout(frame = F)
#st_write(sa2agg, "data/geographic/sa2agg.gpkg")

#### Modelling ####
##### OLS #####
# Get rid PTtoWork carsPerPreson, median age
formula = as.formula(kuli_MPIAgg ~ medianIncome + privateTransporTtoWork +
                       cycleToWork + noCar + PrEuropeanDesc +
                       deprivation + Degree + popdenlog)
lm_sa1 = lm(formula, data = gdf)
lm_sa2 = lm(formula, data = sa2agg)
summary(lm_sa1)
summary(lm_sa2)
vif(lm_sa1)

# stepwise model
steplmsa1 = stepAIC(lm_sa1, trace = 0)
steplmsa2 = stepAIC(lm_sa2, trace = 0)
summary(steplmsa1)
summary(steplmsa2)

par(mfrow=c(1,1))
plot(lm_sa2)
check_model(lm_sa1, check="all")

#### Residuals
residuals_sa1 <- residuals(lm_sa1)
residuals_sa2 <- residuals(lm_sa2)
gdf$residuals <- residuals_sa1
sa2agg$residuals <- residuals_sa2

# Map the residuals
tm_shape(sa2agg) +
  tm_fill("residuals", lwd=0, style='jenks')

##### Spatial Regression Model ####
## Weights Matrices
sa1.lw = nb2listw(poly2nb(gdf, queen=T), style="W")
sa2.lw = nb2listw(poly2nb(sa2agg, queen=T), style="W")

#plot spatially lagged mean
gdf$lagged.means <- lag.listw(sa1.lw, gdf$kuli_MPIAgg)
sa2agg$lagged.means <- lag.listw(sa2.lw, sa2agg$kuli_MPIAgg)

tm_shape(sa2agg) + 
  tm_polygons(col='lagged.means',
              palette = "YlGnBu", lwd=0, style="kmeans")

# test for spatial autocorrelation in residuals
moran.test(sa2agg$kuli_MPIAgg, sa2.lw)
lm.morantest(sa2agg$residuals, sa2.lw, alternative="two.sided")
lm.morantest(gdf$residuals, sa1.lw, alternative="two.sided")

# test what model best to run
lm.LMtests(lm_sa2, sa2.lw, c("LMerr","LMlag"))

###### Spatial Lag Model #####
sp_lag_sa1 <- lagsarlm(formula, data = gdf, sa1.lw, method = "eigen")
save(sp_lag_sa1, file = "outputs/models/sp_lag_sa1.Rdata")
#load("outputs/models/sp_lag_sa1.Rdata")
sp_lag_sa1$coefficients
tidy(sp_lag_sa1)

sp_lag_sa2 <- lagsarlm(formula, data = sa2agg, sa2.lw, method = "eigen")

gdf$residuals_splag <- residuals(sp_lag_sa1)
sa2agg$residuals_splag <- residuals(sp_lag_sa2)

par(mfrow=c(2,2))
plot(sp_lag_sa1)

# test significance
lrtest(lm_sa2, sp_lag_sa2)

# Plot the residuals
dfresid_splag_sa1 <- data.frame(Fitted = sp_lag_sa1$fitted.values, Residuals = sp_lag_sa1$residuals)
ggplot(dfresid_splag_sa1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

dfresid_splag <- data.frame(Fitted = sp_lag_sa2$fitted.values, Residuals = sp_lag_sa2$residuals)
ggplot(dfresid_splag, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# Map residuals
tm_shape(gdf) + tm_polygons(col='residuals_splag',lwd=0, style="jenks")
tm_shape(sa2agg) + tm_polygons(col='residuals_splag',lwd=0, style="jenks")

###### Spatial Error Model #####
sp_err_sa1 <- errorsarlm(formula, data = gdf, sa1.lw, method = "eigen")
save(sp_err_sa1, file = "outputs/models/sp_err_sa1.Rdata")
#load("outputs/models/sp_err_sa1.Rdata")
sp_err_sa1
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

##### Geographically Weighted Regression #####
# convert to sp
sa2.sp = as(sa2agg, "Spatial")
sa1.sp = as(gdf, "Spatial")

# specify MGWR model
mgwr <- gwr.multiscale(formula,
                         data = sa2.sp,
                         adaptive = TRUE, max.iterations = 10000,
                         criterion = "dCVR", approach = "AIC",
                         kernel = "gaussian",
                         bws0 = rep(100, 12),
                         verbose = FALSE, predictor.centered = rep(TRUE, 11))

mgwrsa1 <- gwr.multiscale(formula,
                       data = sa1.sp,
                       adaptive = TRUE, max.iterations = 5000,
                       criterion = "dCVR", approach = "AIC",
                       kernel = "gaussian",
                       bws0 = rep(100, 12),
                       verbose = FALSE, predictor.centered = rep(TRUE, 11))
  
# Save the model
save(mgwr, file = "outputs/models/mgwr_sa2_new.Rdata")

save(mgwrsa1, file = "outputs/models/mgwr_sa1.Rdata")
#load("outputs/models/mgwr_sa2_new.Rdata")
mbwa <- mgwr[[2]]$bws #save bandwidths for later
mgwr

# show diagnostics
mgwr$GW.diagnostic
c(mgwr$GW.diagnostic$AICc, mgwr$GW.diagnostic$R2.val)

##### Model Summary tables #####
# Examine Boxplots of coefficients distributions
olssum <- data.frame(lm_sa1$coefficients)
olssum <- cbind(variable = rownames(olssum), olssum)
rownames(olssum) <- 1:nrow(olssum)
colnames(olssum)[2] <- "value"
olssum$Model <- "OLS"
olssum$id <- 1:nrow(olssum)
olssum$variable[olssum$variable == '(Intercept)'] <- 'Intercept'
olssum <- olssum[,c(4,3,1,2)]

splag_coefs <- data.frame(sp_lag_sa1$coefficients)
splag_coefs <- cbind(variable = rownames(splag_coefs), splag_coefs)
rownames(splag_coefs) <- 1:nrow(splag_coefs)
colnames(splag_coefs)[2] <- "value"
splag_coefs$Model <- "Spatial Lag"
splag_coefs$id <- 1:nrow(splag_coefs)
splag_coefs$variable[splag_coefs$variable == '(Intercept)'] <- 'Intercept'
splag_coefs <- splag_coefs[,c(4,3,1,2)]

mgwr_coef_cols <- data.frame(mgwr$SDF@data[, 1:13])
mgwr_coef_cols$id <- 1:nrow(mgwr_coef_cols)
mgwr_coef_cols$Model <- "MGWR"
mgwr_long <- melt(mgwr_coef_cols, id = c("id","Model"))

allcoefs <- rbind(olssum, splag_coefs)
allcoefs <- rbind(allcoefs, mgwr_long)

boxplot_coefs <- ggplot() +
  geom_boxplot(allcoefs, mapping = aes(x = variable, y = value, col= Model), position="dodge2") +
  stat_boxplot(geom ='errorbar') +
  theme_minimal() +
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=16,  family="serif")) +
  scale_color_manual(values=c("#6ECCAF","blue","red")) +
  ylab('Coefficient estimate') +xlab ('')+#coord_cartesian(ylim = c(-2, 4)) +
  labs(title ="Boxplots of Coefficient estimates") + theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave("outputs/boxplot_coefs.png", plot = boxplot_coefs, dpi = 300)

# Boxplots of SE distributions
mgwr_se <- mgwr$SDF@data[, 17:28] # select standard errors
mgwr_se$id <- 1:nrow(mgwr_se)
mgwr_se$kind <- "MGWR"
mgwr_se_long <- melt(mgwr_se, id = c("id","kind"))
ggplot(mgwr_se_long, aes(x = variable, y = value, col= kind)) +
  geom_boxplot()+
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=13,  family="serif"))

# Summary table with models' coefficients
coefs_msgwr = apply(mgwr$SDF@data[, 1:13], 2, summary)
tab.mgwr = data.frame(Bandwidth = mbwa, t(round(coefs_msgwr,3)))
names(tab.mgwr)[c(3,4,6)] = c("Q1", "MedianMGWR","Q3")
tab.mgwr

olssum <- data.frame(lm_sa1$coefficients)
olssum <- cbind(variable = rownames(olssum), olssum)
rownames(olssum) <- 1:nrow(olssum)
colnames(olssum)[2] <- "OLS"
olssum <- data.frame(olssum[1], round(olssum[2],3))

splag_summ <- data.frame(sp_lag_sa2$coefficients)
splag_summ <- cbind(variable = rownames(splag_summ), splag_summ)
rownames(splag_summ) <- 1:nrow(splag_summ)
colnames(splag_summ)[2] <- "SpatialLag"
splag_summ <- data.frame(splag_summ[1], round(splag_summ[2],3))

summary_table <- data.frame(olssum, splag_summ[2], tab.mgwr[,c(4,1)])
summary_table
stargazer(summary_table)

###### Mapping GW results ######
mgwr_sf = st_as_sf(mgwr$SDF)

# simplest maps
tm_shape(mgwr_sf) +
  tm_fill(c("medianIncome","privateTransporTtoWork", "PTtoWork", "cycleToWork","noCar","carsPerPreson","PrMaoriDesc","Degree","popdenlog"),
          palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)

# plot diverging coefs for MGWR
tm_shape(mgwr_sf) +
  tm_fill(c("privateTransporTtoWork", "PTtoWork", "cycleToWork","noCar","carsPerPreson","PrMaoriDesc","medianage"),midpoint = 0, style = "kmeans") +
  tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"), frame = F)

# all plotting functions
allmgwr_map_func = function(data, var_name, titl) {
  mapout = tm_shape(data) +
    tm_polygons(var_name, lwd=0.25,style = "kmeans", title = titl, title.fontfamily="serif") +
    tm_style("col_blind") +
    tm_layout(main.title = "MGWR", legend.position = c("left","bottom"), frame = T, legend.outside = F,
              legend.title.fontfamily = "serif", main.title.size = 1, main.title.position = "center",
              #legend.width=.5, legend.height=1, legend.text.size=.8,legend.title.size=2,
              legend.bg.color="grey100", legend.bg.alpha=.7, main.title.fontfamily="serif")
    #tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.4) +
    #tm_compass(type = "4star", size = 1, position = c("left", "top"))
  
    mapout
}
map_signif_coefs_func = function(x, var_name, var_name_TV, method) {
  # determine which are significant
  tval = x %>% dplyr::select(all_of(var_name_TV)) %>% st_drop_geometry()
  signif = tval < -1.64 | tval > 1.64
  # map the counties
  p_out = tm_shape(x) +
    tm_polygons(var_name, legend.hist = TRUE, lwd=0.05,
                style = "kmeans", title = var_name, title.fontfamily="serif", palette = "viridis") +
    #tm_style("col_blind") +
    # now add the tvalues layer
    tm_shape(x[signif,], title ="Significance") + tm_borders(lwd = 0.5) +
    tm_layout(legend.position = c("right","top"), frame = F, legend.outside = F,
              main.title=method, legend.title.fontfamily = "serif", main.title.fontfamily="serif",
              legend.width=.5,legend.text.size=0.7,legend.title.size=1, legend.height=.5, 
              legend.hist.height=.2, legend.hist.bg.color="grey90", legend.hist.bg.alpha=.4) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.6) +
    tm_compass(type = "4star", size = 2, position = c("left", "top"))
  # return the output
  p_out
}
map_signif_coefs_diverging_func = function(x, var_name, var_name_TV, method, varN) {
  # determine which are significant
  tval = x %>% dplyr::select(all_of(var_name_TV)) %>% st_drop_geometry()
  signif = tval < -1.96 | tval > 1.96
  # map the counties
  p_out = tm_shape(x) +
    tm_polygons(var_name, midpoint = 0, legend.hist = F, legend.show=T, lwd=0.04,
                style = "kmeans", title = varN, title.fontfamily="serif",
                n=8, palette = "seq") +
    #tm_style("col_blind") +
    # now add the tvalues layer
    tm_shape(x[signif,]) + tm_borders(lwd = 0.3, col="black") +
    tm_layout(main.title = method, legend.position = c("left","bottom"),
              frame = T, legend.outside = F,
              legend.format = list(fun = function(x) formatC(x, digits = 2, format = "f")),
              legend.title.fontfamily = "serif", main.title.size = 4, main.title.position = "center",
              legend.width=2, legend.height=2, legend.text.size=2,legend.title.size=3,
              legend.bg.color="grey100", legend.bg.alpha=.7, main.title.fontfamily="serif",
              aes.palette = list(seq = "-RdBu"))
             # legend.hist.width = 1,legend.hist.height = 0.5)
  #legend.hist.height=.2, legend.hist.width=.3, legend.hist.bg.color="grey90", legend.hist.bg.alpha=.4)
  #tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.6) +
  #tm_compass(type = "4star", size = 2, position = c("left", "top"))
  p_out
}
mapmgwr_signif_coefs_diverging_func = function(x, var_name, var_name_TV, method, varN) {
  # determine which are significant
  tval = x %>% dplyr::select(all_of(var_name_TV)) %>% st_drop_geometry()
  signif = tval < -1.96 | tval > 1.96
  # map the counties
  p_out = tm_shape(x) +
    tm_polygons(var_name, midpoint = 0, legend.hist = F, legend.show=T, lwd=0.04,
                style = "kmeans", title = varN, title.fontfamily="serif",
                n=6, palette = "seq") +
    # now add the tvalues layer
    tm_shape(x[signif,]) + tm_borders(lwd = 0.25, col="black") +
    tm_layout(main.title = method, legend.position = c("left","bottom"),
              frame = T, legend.outside = F,
              legend.format = list(fun = function(x) formatC(x, digits = 1, format = "f")),
              legend.title.fontfamily = "serif", main.title.size = 1, main.title.position = "center",
              legend.width=1, legend.height=1, legend.text.size=,legend.title.size=1,
              legend.bg.color="grey100", legend.bg.alpha=.7, main.title.fontfamily="serif",
              aes.palette = list(seq = "-RdBu"))
  # legend.hist.width = 1,legend.hist.height = 0.5)
  #legend.hist.height=.2, legend.hist.width=.3, legend.hist.bg.color="grey90", legend.hist.bg.alpha=.4)
  #tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.6) +
  #tm_compass(type = "4star", size = 2, position = c("left", "top"))
  p_out
}

# MGWR coefficients
mgwr_cars <- map_signif_coefs_diverging_func(x = mgwr_sf, "carsPerPreson", "carsPerPreson_TV", "Cars per Person", "Coefficient")
mgwr_income <- map_signif_coefs_diverging_func(x = mgwr_sf, "medianIncome", "medianIncome_TV", "Median Income", "Coefficient")
mgwr_pt <- map_signif_coefs_diverging_func(x = mgwr_sf, "PTtoWork", "PTtoWork_TV", "% Public Transport to Work", "Coefficient")
mgwr_cycle <- map_signif_coefs_diverging_func(x = mgwr_sf, "cycleToWork", "cycleToWork_TV", "% Cycle to Work", "Coefficient")
mgwr_privtr <- map_signif_coefs_diverging_func(x = mgwr_sf, "privateTransporTtoWork", "privateTransporTtoWork_TV", "% Private Transport to Work", "Coefficient")
mgwr_maori <- map_signif_coefs_diverging_func(x = mgwr_sf, "PrMaoriDesc", "PrMaoriDesc_TV", "% Maori Descent", "Coefficient")
mgwr_nocar <- map_signif_coefs_diverging_func(x = mgwr_sf, "noCar", "noCar_TV", "% No Car", "Coefficient")
mgwr_degree <- map_signif_coefs_diverging_func(x = mgwr_sf, "Degree", "Degree_TV", "% Degree", "Coefficient")
mgwr_depriv <- map_signif_coefs_diverging_func(x = mgwr_sf, "deprivation", "deprivation_TV", "Deprivation", "Coefficient")
mgwr_popdenslog <- map_signif_coefs_diverging_func(x = mgwr_sf, "popdenlog", "popdenlog_TV", "Log(Population Density)", "Coefficient")
mgwr_medianage <- map_signif_coefs_diverging_func(x = mgwr_sf, "medianage", "medianage_TV", "Median Age)", "Coefficient")

png(file="outputs/allmgwr2.png",width=3000, height=2000)
tmap_arrange(mgwr_income, mgwr_cars, mgwr_pt, mgwr_cycle, mgwr_privtr, mgwr_maori, mgwr_nocar, mgwr_degree, mgwr_depriv, mgwr_popdenslog, mgwr_medianage,ncol=6)
dev.off()

#### Quantile - analysis ####
population <- read.csv('data/auckland_census_2.csv')
population <- population |>
  subset(select = c(code, European, Maori, Pacific, Asian, MiddleEasternLatinAmericanAfrican, OtherEthnicity, PacificNum, popUsual)) |> 
  mutate(code = as.character(code)) |> 
  mutate(European = as.numeric(European)) |> 
  mutate(Maori = as.numeric(Maori)) |>
  mutate(Pacific = as.numeric(Pacific)) |>
  mutate(Asian = as.numeric(Asian)) |>
  mutate(MiddleEasternLatinAmericanAfrican = as.numeric(MiddleEasternLatinAmericanAfrican)) |>
  mutate(OtherEthnicity = as.numeric(OtherEthnicity)) |> 
  mutate(popUsual = as.numeric(popUsual)) |> 
  mutate(PacificNum = as.numeric(PacificNum))

popdf <- left_join(nongkuli, population, by = c("SA12018_V1_00"="code"))
#imputation
popdf[which(is.na(popdf$European),), "European"] <- 0
popdf[which(is.na(popdf$Maori),), "Maori"] <- 0
popdf[which(is.na(popdf$Pacific),), "Pacific"] <- 0
popdf[which(is.na(popdf$Asian),), "Asian"] <- 0
popdf[which(is.na(popdf$MiddleEasternLatinAmericanAfrican),), "MiddleEasternLatinAmericanAfrican"] <- 0
popdf[which(is.na(popdf$OtherEthnicity),), "OtherEthnicity"] <- 0
popdf[which(is.na(popdf$PacificNum),), "PacificNum"] <- 0

# Calculate percentage of ethnicities at each KULI percentile
popdf1 <- within(popdf, quartile <- as.integer(cut(kuli_MPIAgg, quantile(kuli_MPIAgg, seq(0,1,.01)), include.lowest=T)))
popdf2 <- popdf1 |>
  subset(select = -c(SA12018_V1_00)) |> 
  dplyr::group_by(quartile) |>
  summarise(across(everything(), list(sum))) |> 
  mutate(csum_popusual = cumsum(popUsual_1)/1354329) |> 
  mutate(csum_maori = cumsum(Maori_1)/155307) |> 
  mutate(csum_pacific = cumsum(Pacific_1)/235359) |> 
  mutate(csum_asian = cumsum(Asian_1)/425376) |> 
  mutate(csum_melaa = cumsum(MiddleEasternLatinAmericanAfrican_1)/34209) |> 
  mutate(csum_other = cumsum(OtherEthnicity_1)/13878) |> 
  mutate(csum_eur = cumsum(European_1)/654951)

# Plot the ECDF
colors <- c("Pakeha" = "blue", "Pasifika" = "red", "Asian" = "black", "Maori"="purple")
ggplot(popdf2) +
  geom_step(aes(quartile, csum_eur, color = "Pakeha")) +
  geom_step(aes(quartile, csum_pacific, color="Pasifika")) +
  geom_step(aes(quartile, csum_asian, color="Asian")) +
  geom_step(aes(quartile, csum_maori, color="Maori")) +
  theme_minimal() +
  ylab('Share of Population') +xlab ('KULI Percentile') +
  #labs(title="Percentage of Population at Liveability Percentiles") +
  scale_color_manual(values = colors) +
  labs(color = "Legend") +
  theme(text=element_text(family="serif", size=12))

# The KS test to test the statistical significance of these results
ks.test(popdf2$csum_popusual, popdf2$csum_maori)
ks.test(popdf2$csum_popusual, popdf2$csum_maori_rescaled)
ks.test(popdf2$csum_popusual, popdf2$csum_pacific)
ks.test(popdf2$csum_popusual, popdf2$csum_popusual)
ks.test(popdf2$csum_popusual, popdf2$csum_asian)
ks.test(popdf2$csum_popusual, popdf2$csum_melaa)
ks.test(popdf2$csum_popusual, popdf2$csum_eur)

###### KULI weighted by population ethnicities #####
popgdf <- left_join(sa1_polys, popdf, by = c("SA12018_V1_00"="SA12018_V1_00"))
summary(popgdf)

popgdf <- popgdf %>%
  mutate(
    European_kuli_avg = (((European * kuli_MPIAgg)/European)-1),
    Maori_kuli_avg = (Maori * kuli_MPIAgg),
    Pacific_kuli_avg = (Pacific * kuli_MPIAgg),
    Asian_kuli_avg = (Asian * kuli_MPIAgg),
    MiddleEasternLatinAmerican_kuli_avg = (MiddleEasternLatinAmericanAfrican * kuli_MPIAgg) ,
    OtherEthnicity_kuli_avg = (OtherEthnicity * kuli_MPIAgg)
    )
# preview variables
tm_shape(popgdf) +
  tm_fill("European_kuli_avg")

sum(popgdf$European_kuli_avg)/sum(popgdf$European)
sum(popgdf$Pacific_kuli_avg)/sum(popgdf$Pacific)
sum(popgdf$Asian_kuli_avg)/sum(popgdf$Asian)
sum(popgdf$Maori_kuli_avg)/sum(popgdf$Maori)
sum(popgdf$MiddleEasternLatinAmerican_kuli_avg)/sum(popgdf$MiddleEasternLatinAmericanAfrican)
sum(popgdf$OtherEthnicity_kuli_avg)/sum(popgdf$OtherEthnicity)



# Note this file contains only the code needed to replicate the results of the research.
# Additional, messier analysis that was also done is provided in the file 'additional_analysis.R'