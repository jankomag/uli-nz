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
library(spatialreg)

#### Loading data ####
# load the auli data
auli <- st_read('data/sa1_auli.gpkg', quiet = T)
sa1_polys <- st_read("data/sa1_auckland_waiheke_urban.gpkg") |>
  subset(select = c(SA12018_V1_00)) |> 
  st_transform(27291)
nongauli <- st_drop_geometry(auli) |> 
  dplyr::select(SA12018_V1_00, auli_MPIAgg)

only_auli_sp <- st_read('data/sa1_auli.gpkg', quiet = T) |> 
  dplyr::select(SA12018_V1_00, auli_MPIAgg)

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

#### Missing Vlaues Imputation ####
# Dealing with missing values #
md.pattern(census)
aggr_plot <- aggr(census, col=c('navyblue','orange'), numbers=TRUE, sortVars=TRUE, labels=names(census), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

sa1_census <- left_join(sa1_polys, census, by = c("SA12018_V1_00"="code"))
sa1_census$bornOverseas <- (sa1_census$bornOverseas/sa1_census$popUsual)

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

sa1_imped[which(is.na(sa1_imped$privateTransporTtoWork),), "privateTransporTtoWork"] <- mean(sa1_imped$privateTransporTtoWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$PTtoWork),), "PTtoWork"] <- mean(sa1_imped$PTtoWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$cycleToWork),), "cycleToWork"] <- mean(sa1_imped$cycleToWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$noCar),), "noCar"] <- mean(sa1_imped$noCar, na.rm=T)
sa1_imped[which(is.na(sa1_imped$carsPerPreson),), "carsPerPreson"] <- mean(sa1_imped$carsPerPreson, na.rm=T)
sa1_imped[which(is.na(sa1_imped$Degree),), "Degree"] <- mean(sa1_imped$Degree, na.rm=T)

# calculate population density measure
sa1_imped$area <- as.numeric(st_area(sa1_imped))
sa1_imped$popdens <- (sa1_imped$popUsual + 1) / sa1_imped$area
sa1_imped <- sa1_imped |> 
  mutate(popdenlog = minmaxNORM(log(popdens)))

# select columns for regression
gdf <- left_join(sa1_imped, nongauli, by = c("SA12018_V1_00"="SA12018_V1_00"))
df <- st_drop_geometry(gdf)

#### Data preparation ####
gdf_x <- dplyr::select(gdf, -c(auli_MPIAgg,popdens, area))
gdf_y <- dplyr::select(gdf, c(SA12018_V1_00, auli_MPIAgg))
# Z-score standardisation on the independent variables before all modelling
columns_to_exclude <- names(gdf)[c(1,18:19)]
columns_to_scale <- setdiff(names(gdf), columns_to_exclude)
gdf[columns_to_scale] <- scale(st_drop_geometry(gdf[columns_to_scale]))

# plot correlation matrix of the variables for EDA
numeric_data <- df[, sapply(df, is.numeric)]
cor <- cor(x = numeric_data, y = numeric_data, use="complete.obs", method="pearson")
par(mfrow = c(1, 1))
corrplot(cor, tl.srt = 45, type = "lower", method = "ellipse",
         order = "FPC", tl.cex = 0.8,
         tl.col = "black", diag = T, cl.cex=0.7,cl.offset=0.3)

#### Aggregation to SA2 #### 
sa2 <- st_read('data/sa2.gpkg', quiet = T) |> 
  subset(select = c(SA22023_V1_00)) |> st_transform(27291)
sa2agg <- st_interpolate_aw(gdf[2:18], sa2, extensive = F)

# Z-score scale selected columns
columns_to_exclude <- names(sa2agg)[17:18]
columns_to_scale <- setdiff(names(sa2agg), columns_to_exclude)
sa2agg[columns_to_scale] <- scale(st_drop_geometry(sa2agg[columns_to_scale]))
#tm_shape(sa2agg) +tm_polygons(c("privateTransporTtoWork","cycleToWork","noCar","popdenlog","auli_MPIAgg","deprivation","Degree","PrEuropeanDesc"))
#st_write(sa2agg, "data/sa2_auli_agg.gpkg")

#### LISA ####
sa1.lw = nb2listw(poly2nb(gdf, queen=T), style="W")
moran.test(x = gdf$auli_MPIAgg, listw = sa1.lw) 

# Local Statistic #
gdf$li <- localmoran(gdf$auli_MPIAgg, sa1.lw)[, 1]
gdf$localmoranpval <- localmoran(gdf$auli_MPIAgg, sa1.lw)[, 5]
index = gdf$localmoranpval <= 0.05

map1 <- tm_shape(gdf) +
  tm_fill(col = "li", style = "quantile",title="Local Moran's I Statistic") +
  tm_shape(gdf[index,]) + tm_borders(lwd=0.15, col="black") +
  tm_layout(legend.position = c("left","bottom"), frame = T, legend.outside = F,
            legend.title.fontfamily = "serif", main.title.position = "center",
            legend.width=1, legend.height=1, legend.text.size=0.8, legend.title.size = 1.2,
            legend.bg.color="grey100", legend.bg.alpha=.7, title.fontfamily="serif", legend.text.fontfamily = "serif")

# Local Moran Cluster Map #
queen_wts <- queen_weights(gdf)
moran <- local_moran(queen_wts, gdf["auli_MPIAgg"])
moran_lbls <- lisa_labels(moran)[1:5]
moran_colors <- setNames(lisa_colors(moran)[1:5], moran_lbls)

gdf_lmm <- gdf |>
  mutate(cluster_num = lisa_clusters(moran) + 1,
         cluster = factor(moran_lbls[cluster_num], levels = moran_lbls))

map2 <- tm_shape(gdf_lmm) +
  tm_polygons("cluster", lwd=0, title="Moran Clusters",
              palette = moran_colors) +
  tm_layout(legend.position = c("left","bottom"), frame = T, legend.outside = F,
            legend.title.fontfamily = "serif", main.title.position = "center",
            legend.width=0.7, legend.height=1, legend.text.size=0.8, legend.title.size = 1.2,
            legend.bg.color="grey100", legend.bg.alpha=.7, title.fontfamily="serif", legend.text.fontfamily = "serif")

tmap_arrange(map1, map2,ncol = 2)

#### Modelling ####
##### OLS #####
formula_old = as.formula(auli_MPIAgg ~ medianIncome + privateTransporTtoWork +
                       cycleToWork + noCar + PrEuropeanDesc +
                       deprivation + Degree + popdenlog)
lm_sa1 = lm(formula_old, data = gdf)
lm_sa2 = lm(formula_old, data = sa2agg)
summary(lm_sa1)
summary(lm_sa2)
vif(lm_sa1)

# stepwise model
steplmsa1 = stepAIC(lm_sa1, trace = 0)
steplmsa2 = stepAIC(lm_sa2, trace = 0)
summary(steplmsa1)
summary(steplmsa2)

# for final model we drop median income
formula = as.formula(auli_MPIAgg ~ privateTransporTtoWork +
                       cycleToWork + noCar + PrEuropeanDesc +
                       deprivation + Degree + popdenlog)
lm_sa1 = lm(formula, data = gdf)
tidy(lm_sa1)
summary(lm_sa1)
par(mfrow=c(1,1))
plot(lm_sa2)

#### Residuals
gdf$residuals <- residuals(lm_sa1)

# Map the residuals
tm_shape(gdf) +
  tm_fill("residuals", lwd=0, style='jenks')

##### Spatial Regression Models ####
## Weights Matrices
sa2.lw = nb2listw(poly2nb(sa2agg, queen=T), style="W")

#plot spatially lagged mean
gdf$lagged.means <- lag.listw(sa1.lw, gdf$auli_MPIAgg)
sa2agg$lagged.means <- lag.listw(sa2.lw, sa2agg$auli_MPIAgg)

tm_shape(gdf) + 
  tm_polygons(col='lagged.means',
              palette = "YlGnBu", lwd=0, style="kmeans")

# test for spatial autocorrelation in residuals
lm.morantest(lm_sa1, sa1.lw, alternative="two.sided")

# test what model best to run
lm.LMtests(lm_sa2, sa2.lw, c("LMerr","LMlag"), zero.policy=TRUE)

###### Spatial Lag Model #####
sp_lag_sa1 <- lagsarlm(formula, data = gdf, sa1.lw, method = "eigen")
save(sp_lag_sa1, file = "outputs/models/sp_lag_sa1.Rdata")
#load("outputs/models/sp_lag_sa1.Rdata")
summary(sp_lag_sa1)

W <- as(sa1.lw, "CsparseMatrix")
#creates a vector of traces of powers of the spatial weights matrix 
trMC <- trW(W, type="MC")
#direct, indirect and total effects
fit.lag.effects.sparse <- impacts(sp_lag_sa1, tr = trMC, R = 999)
fit.lag.effects.sparse
fit.lag.effects <- impacts(sp_lag_sa1, listw = sa1.lw, R = 999)
fit.lag.effects

gdf$residuals_splag <- residuals(sp_lag_sa1)

par(mfrow=c(2,2))
plot(sp_lag_sa1)

# test significance
lrtest(lm_sa1, sp_lag_sa1)

# Plot the residuals
dfresid_splag_sa1 <- data.frame(Fitted = sp_lag_sa1$fitted.values, Residuals = sp_lag_sa1$residuals)
ggplot(dfresid_splag_sa1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# Map residuals
breaks <- c(-0.35, -0.1, -0.05, 0, 0.05, 0.1, 0.35)
olsresidsmap <- tm_shape(gdf) + tm_polygons(col='residuals',lwd=0, style="fixed",
                            breaks=breaks, midpoint=0, title = "OLS Residuals")
splagresidsmap <- tm_shape(gdf) + tm_polygons(col='residuals_splag',lwd=0, style="fixed",
                            breaks=breaks, midpoint=0, title = "Spatial Lag Residuals")
maps_residuals <- tmap_arrange(olsresidsmap, splagresidsmap, ncol = 2)

##### Geographically Weighted Regression #####
# convert to sp
sa2.sp = as(sa2agg, "Spatial")
sa1.sp = as(gdf, "Spatial")

# specify MGWR model
mgwr <- gwr.multiscale(formula,
                         data = sa2.sp,
                         adaptive = TRUE, max.iterations = 7000,
                         criterion = "dCVR", approach = "AIC",
                         kernel = "bisquare",
                         #bws0 = rep(100, 8),
                         # predictor.centered = rep(TRUE, 7),
                         verbose = FALSE)

mgwrsa1 <- gwr.multiscale(formula,
                       data = sa1.sp,
                       adaptive = TRUE, max.iterations = 5000,
                       criterion = "dCVR", approach = "AIC",
                       kernel = "bisquare",
                       #bws0 = rep(100, 9),
                       #predictor.centered = rep(TRUE, 8)
                       verbose = FALSE)
  
# Save the model
save(mgwr, file = "outputs/models/mgwr_sa2.Rdata")

#save(mgwrsa1, file = "outputs/models/mgwr_sa1.Rdata")
#load("outputs/models/mgwr_sa2_new.Rdata")
mbwa <- mgwr[[2]]$bws #save bandwidths for later
mgwr

# show diagnostics
mgwr$GW.diagnostic
c(mgwr$GW.diagnostic$AICc, mgwr$GW.diagnostic$R2.val)

##### Model Summary tables #####
# Examine Boxplots of coefficients distributions
ols_coefs <- data.frame(lm_sa1$coefficients)
ols_coefs <- ols_coefs[-1, , drop = FALSE]
ols_coefs <- cbind(variable = rownames(ols_coefs), ols_coefs)
rownames(ols_coefs) <- 1:nrow(ols_coefs)
colnames(ols_coefs)[2] <- "value"
ols_coefs$Model <- "OLS"
ols_coefs$id <- 1:nrow(ols_coefs)
ols_coefs <- ols_coefs[,c(4,3,1,2)]

splag_coefs <- data.frame(sp_lag_sa1$coefficients)
splag_coefs <- splag_coefs[-1, , drop = FALSE]
splag_coefs <- cbind(variable = rownames(splag_coefs), splag_coefs)
rownames(splag_coefs) <- 1:nrow(splag_coefs)
colnames(splag_coefs)[2] <- "value"
splag_coefs$Model <- "Spatial Lag"
splag_coefs$id <- 1:nrow(splag_coefs)
splag_coefs <- splag_coefs[,c(4,3,1,2)]

mgwr_coef_cols <- data.frame(mgwr$SDF@data[, 2:8])
mgwr_coef_cols$id <- 1:nrow(mgwr_coef_cols)
mgwr_coef_cols$Model <- "MGWR"
mgwr_long <- reshape2::melt(mgwr_coef_cols, id = c("id","Model"))

allcoefs <- rbind(ols_coefs, splag_coefs)
allcoefs <- rbind(allcoefs, mgwr_long)
allcoefs$Model <- factor(allcoefs$Model, levels = c("OLS", "Spatial Lag", "MGWR"))

boxplot_coefs <- ggplot() +
  geom_boxplot(allcoefs, mapping = aes(x = variable, y = value, col= Model), position="dodge2") +
  stat_boxplot(geom ='errorbar') +
  theme_minimal() +
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=16,  family="serif")) +
  scale_color_manual(values=c("blue","red","#6ECCAF")) +
  ylab('Coefficient estimate') +xlab ('')+#coord_cartesian(ylim = c(-2, 4)) +
  labs(title ="") + theme(
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
coefs_mgwr = apply(mgwr$SDF@data[, 1:8], 2, summary)
tab.mgwr = data.frame(Bandwidth = mbwa, t(round(coefs_mgwr,3)))
names(tab.mgwr)[c(3,4,6)] = c("Q1", "MedianMGWR","Q3")
tab.mgwr

olssum <- data.frame(steplmsa1$coefficients)
olssum <- cbind(variable = rownames(olssum), olssum)
rownames(olssum) <- 1:nrow(olssum)
colnames(olssum)[2] <- "OLS"
olssum <- data.frame(olssum[1], round(olssum[2],3))

splag_summ <- data.frame(sp_lag_sa1$coefficients)
splag_summ <- cbind(variable = rownames(splag_summ), splag_summ)
rownames(splag_summ) <- 1:nrow(splag_summ)
colnames(splag_summ)[2] <- "SpatialLag"
splag_summ <- data.frame(splag_summ[1], round(splag_summ[2],3))

summary_table <- data.frame(olssum, splag_summ[2], tab.mgwr[,c(4,1)])
summary_table
#stargazer(summary_table)

###### Mapping GW results ######
mgwr_sf = st_as_sf(mgwr$SDF)

my_palette <- brewer.pal(n = 7, name = "Blues")
breaks=c(-1.5,-1,-0.5,-0.1,0.1,0.5,1,1.5)
# simplest maps
tm_shape(mgwr_sf) +
  tm_fill(c("PrEuropeanDesc","deprivation","privateTransporTtoWork", "cycleToWork","noCar","Degree","popdenlog"),midpoint = 0,
          style = "fixed", breaks=breaks) +
  tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"), frame = F)

# all plotting functions
map_signif_coefs_diverging_func = function(x, var_name, var_name_TV, method, varN) {
  # determine which are significant
  tval = x %>% dplyr::select(all_of(var_name_TV)) %>% st_drop_geometry()
  signif = tval < -1.96 | tval > 1.96
  # map the sa1s
  p_out = tm_shape(x) +
    tm_polygons(var_name, midpoint = 0, legend.hist = F, legend.show=T, lwd=0.04,
                style = "fixed", breaks=breaks, title = varN, title.fontfamily="serif",
                n=8, palette = "seq") +
    # now add the tvalues layer
    tm_shape(x[signif,]) + tm_borders(lwd = 0.3, col="black") +
    tm_layout(main.title = method, legend.position = c("left","bottom"),
              frame = T, legend.outside = F,
              legend.format = list(fun = function(x) formatC(x, digits = 2, format = "f")),
              legend.title.fontfamily = "serif", main.title.size = 4, main.title.position = "center",
              legend.width=2, legend.height=2, legend.text.size=2,legend.title.size=3,
              legend.bg.color="grey100", legend.bg.alpha=.7, main.title.fontfamily="serif",
              aes.palette = list(seq = "-RdBu"))
  
  p_out
}

# MGWR coefficients
mgwr_cycle <- map_signif_coefs_diverging_func(x = mgwr_sf, "cycleToWork", "cycleToWork_TV", "% Cycle to Work", "Coefficient")
mgwr_eur <- map_signif_coefs_diverging_func(x = mgwr_sf, "PrEuropeanDesc", "PrEuropeanDesc_TV", "% European", "Coefficient")
mgwr_privtr <- map_signif_coefs_diverging_func(x = mgwr_sf, "privateTransporTtoWork", "privateTransporTtoWork_TV", "% Private Transport to Work", "Coefficient")
mgwr_nocar <- map_signif_coefs_diverging_func(x = mgwr_sf, "noCar", "noCar_TV", "% No Car", "Coefficient")
mgwr_degree <- map_signif_coefs_diverging_func(x = mgwr_sf, "Degree", "Degree_TV", "% Degree", "Coefficient")
mgwr_depriv <- map_signif_coefs_diverging_func(x = mgwr_sf, "deprivation", "deprivation_TV", "Deprivation", "Coefficient")
mgwr_popdenslog <- map_signif_coefs_diverging_func(x = mgwr_sf, "popdenlog", "popdenlog_TV", "Log(Population Density)", "Coefficient")

png(file="outputs/allmgwr2.png",width=3000, height=2000)
tmap_arrange(mgwr_eur, mgwr_cycle, mgwr_privtr, mgwr_nocar, mgwr_degree, mgwr_depriv, mgwr_popdenslog,ncol=4)
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

income <- dplyr::select(census, c(medianIncome, code))
popdf <- left_join(popdf, income, by = c("SA12018_V1_00"="code"))
popdf[which(is.na(popdf$medianIncome),), "medianIncome"] <- 35400

# Calculate percentage of ethnicities at each KULI percentile
popdf1 <- within(popdf, quartile <- as.integer(cut(kuli_MPIAgg, quantile(kuli_MPIAgg, seq(0,1,.01)), include.lowest=T)))
popdf2 <- popdf1 |>
  subset(select = -c(SA12018_V1_00)) |> 
  dplyr::group_by(quartile) |>
  summarise(across(everything(), list(sum))) |> 
  mutate(csum_popusual = cumsum(popUsual_1)/sum(popdf1$popUsual)) |> 
  mutate(csum_maori = cumsum(Maori_1)/sum(popdf1$Maori)) |> 
  mutate(csum_pacific = cumsum(Pacific_1)/sum(popdf1$Pacific)) |> 
  mutate(csum_asian = cumsum(Asian_1)/sum(popdf1$Asian)) |> 
  mutate(csum_melaa = cumsum(MiddleEasternLatinAmericanAfrican_1)/sum(popdf1$MiddleEasternLatinAmericanAfrican)) |> 
  mutate(csum_other = cumsum(OtherEthnicity_1)/sum(popdf1$OtherEthnicity)) |> 
  mutate(csum_eur = cumsum(European_1)/sum(popdf1$European))
  #mutate(csum_medianincome = cumsum(medianIncome_1)/sum(popdf1$medianIncome))

# Plot the ECDF
colors <- c("Pakeha" = "blue", "Pasifika" = "red", "Asian" = "black", "Maori"="purple") #, "MedianIncome"="darkgreen")
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


#### Bivariate Map ####
data <- bi_class(gdf, x = auli_MPIAgg, y = deprivation, style = "quantile", dim = 3)
bimap <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0, show.legend = F, lwd=0) +
  bi_scale_fill(pal = "GrPink2", dim = 3) + bi_theme() 
legend <- bi_legend(pal = "GrPink2",
                    dim = 3,
                    xlab = "Higher AULI ",
                    ylab = "Higher Deprivation ",
                    size = 8)
ggdraw() +
  draw_plot(bimap, 0, 0, 1, 1) +
  draw_plot(legend, .1, .1, .2, .2)

#### Population Density Analysis ####
gdf_popdens <- gdf %>%
  dplyr::select(c(SA12018_V1_00,popdenlog,auli_MPIAgg)) |> 
  arrange(desc(popdenlog))

# Add a new column with population density bins
gdf_popdens <- gdf_popdens %>%
  mutate(pop_density_bins = as.factor(ntile(popdenlog, 4)))

ggplot(gdf_popdens, aes(x = popdenlog, fill = factor(pop_density_bins))) +
  geom_histogram(binwidth = 0.03, position = "identity", alpha = 0.5) +
  labs(title = "Histogram with Colored Bins", x = "Value", y = "Frequency") +
  scale_fill_discrete(name = "Bin")
tm_shape(gdf_popdens) +
  tm_fill("pop_density_bins", lwd=0)

library(lme4)
model <- lmer(auli_MPIAgg ~ 1 + (1 | pop_density_bins), data=gdf_popdens)
model2 <- lmer(auli_MPIAgg ~ 1 + (popdenlog + 1 | pop_density_bins), data=gdf_popdens)
summary(model2)

#improve.fit(model2, model)
gdf_popdens$mult_reg_res <- residuals(model)
tm_shape(gdf_popdens) +
  tm_fill("mult_reg_res", lwd=0)

gdf_popdens$randintslopepred = predict(model)

ggplot(data = gdf_popdens, aes(x = popdenlog, y = auli_MPIAgg,
                               color = pop_density_bins)) +
  geom_point(alpha=0.1) +
  geom_line(aes(y = randintslopepred))

library(brms)
odel <- brm(
  auli_MPIAgg ~ 1 + popdenlog + (1 | pop_density_bins),
  data = gdf_popdens,
  family = gaussian(),
  #autocor = cor_sar(geom = sa1.lw, rho = b),  # SAR spatial correlation structure
  chains = 4, cores = 4
)

# Summary of the model
summary(odel)
plot(conditional_effects(odel, effects = "pop_density"))

spatial_pred <- marginal_effects(model, effects = "pop_density")
plot(spatial_pred, points = TRUE)


# Note this file contains only the code needed to replicate the results of the research.
# Additional, messier analysis that was also done is provided in the file 'additional_analysis.R'