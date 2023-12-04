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
library(performance)

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

# Dealing with missing values #
md.pattern(census)
aggr_plot <- aggr(census, col=c('navyblue','orange'), numbers=TRUE, sortVars=TRUE, labels=names(census), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

sa1_census <- left_join(sa1_polys, census, by = c("SA12018_V1_00"="code"))
tmap_mode("plot")
tm_shape(sa1_census) + tm_polygons(col="deprivation", lwd=0, style="kmeans")

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

sa1_imped[which(is.na(sa1_imped$maoriDescent),), "maoriDescent"] <- mean(sa1_imped$maoriDescent, na.rm=T)
sa1_imped[which(is.na(sa1_imped$PrMaoriDesc),), "PrMaoriDesc"] <- mean(sa1_imped$PrMaoriDesc, na.rm=T)
sa1_imped[which(is.na(sa1_imped$PrEuropeanDesc),), "PrEuropeanDesc"] <- mean(sa1_imped$PrEuropeanDesc, na.rm=T)
sa1_imped[which(is.na(sa1_imped$medianIncome),), "medianIncome"] <- mean(sa1_imped$medianIncome, na.rm=T)
sa1_imped[which(is.na(sa1_imped$bornOverseas),), "bornOverseas"] <- mean(sa1_imped$bornOverseas, na.rm=T)
sa1_imped[which(is.na(sa1_imped$privateTransporTtoWork),), "privateTransporTtoWork"] <- mean(sa1_imped$privateTransporTtoWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$PTtoWork),), "PTtoWork"] <- mean(sa1_imped$PTtoWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$cycleToWork),), "cycleToWork"] <- mean(sa1_imped$cycleToWork, na.rm=T)
sa1_imped[which(is.na(sa1_imped$noCar),), "noCar"] <- mean(sa1_imped$noCar, na.rm=T)
sa1_imped[which(is.na(sa1_imped$carsPerPreson),), "carsPerPreson"] <- mean(sa1_imped$carsPerPreson, na.rm=T)
sa1_imped[which(is.na(sa1_imped$Degree),), "Degree"] <- mean(sa1_imped$Degree, na.rm=T)

# calculate population density measure
sa1_imped$area <- st_area(sa1_imped)
sa1_imped$popdens <- as.numeric(sa1_imped$popUsual / sa1_imped$area)
sa1_imped$popdenlog <- log(sa1_imped$popdens+0.0001)

gdf <- left_join(sa1_imped, nongkuli, by = c("SA12018_V1_00"="SA12018_V1_00"))
df <- st_drop_geometry(gdf)

#### KULI Visualisation ####
### MAP
bckgd <- st_read('data/land.gpkg', quiet = T) # transform to OSGB projection
tm_shape(gdf) +tm_polygons("grey",  lwd=0) +
tm_shape(bckgd) + tm_polygons(col="#DCDACB", lwd=0) +
tm_shape(gdf) +
  tm_polygons("kuli_MPIAgg", midpoint = 6, legend.hist = T, lwd=0.01, n=14,
              style = "kmeans", title = "KULI in Auckland", title.fontfamily="serif") +
  tm_style("col_blind") +
  tm_layout(legend.position = c("left","bottom"), frame = F, legend.outside = F,
            legend.title.fontfamily = "serif", bg.color="#CCEAE5") +
            #legend.width=1, legend.height=1, legend.text.size=1,legend.title.size=1,
            #legend.bg.color="grey100", legend.bg.alpha=.7,
            #legend.hist.height=.2, legend.hist.width=.3, legend.hist.bg.color="grey90", legend.hist.bg.alpha=.4) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.6) +
  tm_compass(type = "4star", size = 2, position = c("left", "top"))
### Histogram
ggplot(gdf)+
    geom_histogram(aes(kuli_MPIAgg)) +
    theme_minimal() +
    theme(legend.position="right",
          text=element_text(size=13,  family="serif")) +
    ylab(" ") + xlab (" ")

#### Aggregation binning ####
### SA2 ###
sa2 = st_read('data/sa2.gpkg', quiet = T) |> 
  subset(select = c(SA22023_V1_00)) |> st_transform(27291)
sa2agg <- st_interpolate_aw(gdf[2:19], sa2, extensive = F)

tm_shape(sa2agg) + 
  tm_polygons("kuli_MPIAgg", palette = "YlGnBu", style="kmeans",
              lwd=.1) + tm_layout(frame = F)
#st_write(sa2agg, "data/geographic/sa2agg.gpkg")

#### Modelling ####
##### OLS #####
formula = as.formula(kuli_MPIAgg ~ medianIncome + privateTransporTtoWork +
                       PTtoWork + cycleToWork + noCar + carsPerPreson + PrEuropeanDesc +
                       PrMaoriDesc + deprivation + Degree + popdenlog)
lm_sa1 = lm(formula, data = gdf)
lm_sa2 = lm(formula, data = sa2agg)
summary(lm_sa1)
summary(lm_sa2)
plot(gdf$carsPerPreson, gdf$privateTransporTtoWork)
# stepwise model
steplmsa1 = stepAIC(lm_sa1, trace = 0)
steplmsa2 = stepAIC(lm_sa2, trace = 0)
summary(steplmsa1)
summary(steplmsa2)

formulastep = as.formula(kuli_MPIAgg ~ medianIncome + privateTransporTtoWork +
                       PTtoWork + cycleToWork + noCar + carsPerPreson +
                       PrMaoriDesc + deprivation + Degree + popdenlog)

##### Residuals #####
residuals <- residuals(lm_sa1)
gdf$residuals <- (residuals)

# Map the residuals
tm_shape(gdf) +
  tm_fill("residuals", lwd=0, style='jenks')

par(mfrow=c(2,2))
plot(lm_sa1)

check_model(lm_sa1, check="all")

##### Spatial Weights Matrix ####
gdf.nb <- poly2nb(gdf, queen=T)
gdf.lw = nb2listw(gdf.nb, style="W")

#plot spatially lagged mean
gdf$lagged.means <- lag.listw(gdf.lw, gdf$kuli_MPIAgg)
tm_shape(gdf) + 
  tm_polygons(col='lagged.means',
              palette = "YlGnBu", lwd=0, style="kmeans")

# test for spatial autocorrelation in residuals
moran.test(gdf$residuals, gdf.lw)

##### Geographically Weighted Regression #####
# convert to sp
sa2.sp = as(sa2agg, "Spatial")

# specify MGWR model
mgwr <- gwr.multiscale(formulastep,
                         data = sa2.sp,
                         adaptive = TRUE, max.iterations = 10000,
                         criterion = "dCVR", approach = "AIC",
                         kernel = "gaussian",
                         bws0 = rep(100, 11),
                         verbose = FALSE, predictor.centered = rep(TRUE, 10))
  
# Save the model
save(mgwr, file = "outputs/models/mgwr_sa2.Rdata")
mbwa <- mgwr[[2]]$bws #save bandwidths for later

mgwr
mgwr$GW.diagnostic
# show diagnostics
c(mgwr$GW.diagnostic$AICc, mgwr$GW.diagnostic$R2.val)

# Examine Boxplots of coef distributions
mgwr_coef_cols <- data.frame(mgwr$SDF@data[, 1:11])
mgwr_coef_cols$id <- 1:nrow(mgwr_coef_cols)
mgwr_coef_cols$Model <- "MGWR"
mgwr_long <- melt(mgwr_coef_cols, id = c("id","Model"))

olssum <- data.frame(steplmsa1$coefficients)
olssum <- cbind(variable = rownames(olssum), olssum)
rownames(olssum) <- 1:nrow(olssum)
colnames(olssum)[2] <- "value"
olssum$Model <- "OLS"
olssum$id <- 1:nrow(olssum)
olssum$variable[olssum$variable == '(Intercept)'] <- 'Intercept'
olssum <- olssum[,c(4,3,1,2)]

allcoefs <- rbind(mgwr_long, olssum)

ggplot() +
  geom_boxplot(allcoefs, mapping = aes(x = variable, y = value, col= Model), position="dodge2") +
  stat_boxplot(geom ='errorbar') +
  theme_minimal() +
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=13,  family="serif")) +
  scale_color_manual(values=c("#6ECCAF","#344D67","red")) +
  ylab('Coefficient estimate') +xlab ('')+coord_cartesian(ylim = c(-2, 4)) +
  labs(title ="Boxplots of Coefficient estimates")
  
ggplot(mgwr_long) +
  geom_boxplot(mapping = aes(x = variable, y = value), position="dodge2")
  
#plot distributions of coefficients
my_plots <- lapply(names(mgwr_coef_cols), function(var_x){
  p <- ggplot(mgwr_coef_cols) +
    aes_string(var_x)
  if(is.numeric(mgwr_coef_cols[[var_x]])) {
    p <- p + geom_density()
  } else {
    p <- p + geom_bar()
  } 
})
plot_grid(plotlist = my_plots)

# Examine Boxplots of SE distributions
mgwr_se <- mgwr_2$SDF@data[, 14:24] # select standard errors
mgwr_se$id <- 1:nrow(mgwr_se)
mgwr_se$kind <- "MGWR"
mgwr_se_long <- melt(mgwr_se, id = c("id","kind"))
ggplot(mgwr_se_long, aes(x = variable, y = value, col= kind)) +
  geom_boxplot()+
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=13,  family="serif"))

##### Model Summary tables #####
# create a table with coefficient stats for MGWR
coefs_msgwr = apply(mgwr$SDF@data[, 1:11], 2, summary)
tab.mgwr = data.frame(Bandwidth = mbwa, t(round(coefs_msgwr,3)))
names(tab.mgwr)[c(3,6)] = c("Q1", "Q3")
tab.mgwr

olssum <- data.frame(steplmsa1$coefficients)
olssum <- cbind(variable = rownames(olssum), olssum)
rownames(olssum) <- 1:nrow(olssum)
colnames(olssum)[2] <- "coef"

summary_table <- data.frame(olssum, tab.mgwr[,c(5,1)])
summary_table

##### Mapping GW results #####
mgwr_sf = st_as_sf(mgwr$SDF)

# simplest maps
tm_shape(mgwr_sf) +
  tm_fill(c("medianIncome","privateTransporTtoWork", "PTtoWork", "cycleToWork","noCar","carsPerPreson","PrMaoriDesc","Degree","popdenlog"),
          palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)

# plot diverging coefs for MGWR
tm_shape(mgwr_sf) +
  tm_fill(c("privateTransporTtoWork", "PTtoWork", "cycleToWork","noCar","carsPerPreson","PrMaoriDesc"),midpoint = 0, style = "kmeans") +
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

png(file="outputs/allmgwr.png",width=3000, height=2000)
tmap_arrange(mgwr_income, mgwr_cars, mgwr_pt, mgwr_cycle, mgwr_privtr, mgwr_maori, mgwr_nocar, mgwr_degree, mgwr_depriv, mgwr_popdenslog,ncol=5)
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
    European_kuli_avg = (European * kuli_MPIAgg) / European,
    Maori_kuli_avg = (Maori * kuli_MPIAgg) / Maori,
    Pacific_kuli_avg = (Pacific * kuli_MPIAgg) / Pacific,
    Asian_kuli_avg = (Asian * kuli_MPIAgg) / Asian,
    MiddleEasternLatinAmerican_kuli_avg = (MiddleEasternLatinAmericanAfrican * kuli_MPIAgg) / MiddleEasternLatinAmericanAfrican,
    OtherEthnicity_kuli_avg = (OtherEthnicity * kuli_MPIAgg) / OtherEthnicity,
    PacificNum_kuli_avg = (PacificNum * kuli_MPIAgg) / PacificNum
  )
# preview variables
tm_shape(popgdf) +
  tm_fill("Pacific_kuli_avg")

# Note this file contains only the code needed to replicate the results presented in the research.
# Additional, messier analysis that was also done is provided in the file 'additional_analysis.R'