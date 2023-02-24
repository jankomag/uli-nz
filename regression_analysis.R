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
library(stargazer)
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
library(rgeos)
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
library(spatmap)


#### Loading data ####
# load the kuli data
kuli = st_read('data/geographic/sa1_kuli_all.gpkg', quiet = T) # transform to OSGB projection
kuli <- kuli |> 
  subset(select = c(SA12018_V1_00, kuli_no2s_geomAgg)) |> st_transform(27291)

sa1_polys <- kuli |>
  subset(select = c(SA12018_V1_00))

#load additional data
census <- read.csv('data/additionalData/auckland_census_2.csv')
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
  mutate(PrMaoriDesc = as.numeric(PrMaoriDesc))

#load additional data
deprivation <- read.csv('data/additionalData/deprivation_joined.csv')
deprivation <- deprivation |>
  mutate(SA12018_V1_00 = as.character(SA12018_V1_00))
census <- left_join(census, deprivation, by = c("code"="SA12018_V1_00"))

# Dealing with missing values #
# find NAs
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

#sa1_census <- left_join(sa1_polys, census, by = c("SA12018_V1_00"="code"))
tmap_mode("plot")
tm_shape(sa1_imped) + tm_polygons(col=c("PrEuropeanDesc","carsPerPreson","noCar",
                                        "cycleToWork","PTtoWork","privateTransporTtoWork",
                                        "bornOverseas","medianIncome","PrMaoriDesc","maoriDescent"), lwd=0, style="kmeans")
kulinong <- st_drop_geometry(kuli)
dfg <- left_join(sa1_imped, kulinong, by = c("SA12018_V1_00"="SA12018_V1_00"))
df <- st_drop_geometry(dfg)
summary(dfg)
#### EDA ####
cor <- cor(x = df[3:14], y = df[3:14], use="complete.obs")
corrplot(cor, tl.srt = 25)
corr <- rcorr(as.matrix(df[3:14]))

# function to make correlation matrix§-
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Get correlation matrix with coefficients and p-values
corrmatrix <- flattenCorrMatrix(corr$r, corr$P)
corrmatrix <- corrmatrix |>
  arrange(row) |>
  filter(row == 'income')

# plot correlations
df[3:14] |>
  gather(-kuli_no2s_geomAgg, key = "var", value = "value") |> 
  ggplot(aes(x = kuli_no2s_geomAgg, y = value)) +
  facet_wrap(~ var, scales = "free") +
  geom_point(alpha=0.2) +
  theme_bw() +
  geom_smooth(method="lm")

### Hexagon binning ####
gb.sp = as(dfg, "Spatial")
hex_points <- spsample(gb.sp, type = "hexagonal", n = 1630)
tz_sf <- HexPoints2SpatialPolygons(hex = hex_points)
sz_sf = dfg[2:15]
hexgrid <- st_interpolate_aw(sz_sf, tz_sf, extensive = F)
tm_shape(hexgrid) + 
  tm_polygons("kuli_no2s_geomAgg", palette = "YlGnBu", style="kmeans",
              lwd=0) +
  tm_layout(frame = F)
hexgrid <- st_transform(hexgrid, 4326)

# for contiguity matrix
hexgrid.nb <- poly2nb(hexgrid)
hexgrid$rn = rownames(hexgrid) 
tmap_mode("plot")
tm_shape(hexgrid) + 
  tm_borders() +
  tm_text(text = "rn") +
  tm_basemap("OpenStreetMap")
#hexgrid.nb[[651]] = as.integer(c(1036,1037))
hexgrid.nb[[999]] = as.integer(943)

hex.lw = nb2listw(hexgrid.nb)

gg.net2 <- nb2lines(hexgrid.nb,coords=st_geometry(st_centroid(hexgrid)), as_sf = F) 
tm_shape(hexgrid) + tm_borders(col='grey') + 
  tm_shape(gg.net2) + tm_lines(col='red', alpha = 1)

#### OLS model ####
# SA1 geometry
formula = as.formula(kuli_no2s_geomAgg ~ medianIncome + bornOverseas + privateTransporTtoWork +
                       PTtoWork + cycleToWork + noCar + carsPerPreson + PrEuropeanDesc + PrMaoriDesc + deprivation) # construct the OLS model
lm = lm(formula, data = df)
summary(lm)

step.res = stepAIC(lm, trace = 0)
summary(step.res)

# Hexagonal geometry
hex.lm = lm(formula, data = hexgrid)
summary(hex.lm)

stargazer(lm, hex.lm, flip=F, type="text", single.row = T, style="qje")
#### Autocorrelation ####
##### Moran's I####
g.nb <- poly2nb(dfg)
g.lw = nb2listw(g.nb)
#plot spatially lagged mean
dfg$lagged.means <- lag.listw(g.lw, dfg$kuli_no2s_geomAgg)
tm_shape(dfg) + 
  tm_polygons(col='lagged.means', 
              title='KULI',
              palette = "YlGnBu", lwd=0, style="kmeans")
# moran scatterplot
ggplot(data = dfg, aes(x = kuli_no2s_geomAgg, y = lagged.means)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_hline(yintercept = mean(dfg$lagged.means), lty = 2) +
  geom_vline(xintercept = mean(dfg$kuli_no2s_geomAgg), lty = 2) +
  geom_abline() +
  coord_equal()
# create and assign the Moran plot - with more details
moran.plot(x = dfg$kuli_no2s_geomAgg, listw = g.lw)
# get Moran's I statistic
moran.test(x = dfg$kuli_no2s_geomAgg, listw = g.lw) 
# normlaise Moran's I to interpret
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/ 2) $values))
} 
#moran.range(g.lw)

# Moran's I at different distances
start <- 0 # Starting distance in meters (the From)
end <- 5000 # Ending distance in meters (the To)
incr <- 200 # Distance increment (which also defines the annulus width)
incr.v <- seq(start, end, incr)
s.center <- st_point_on_surface(dfg)
s.coord <- st_coordinates(s.center)
# Define empty vector elements to store the I and p-values
morI.mc <- vector()
sign.mc <- vector()

# Loop through each distance band
for (i in (2:length(incr.v))) {
  s.dist <- dnearneigh(s.coord, incr.v[i - 1], incr.v[i])
  s.lw <- nb2listw(s.dist, style = "W", zero.policy=T)
  s.mor <- moran.mc(dfg$kuli_no2s_geomAgg, s.lw, nsim=599, zero.policy = TRUE)
  sign.mc[i] <- s.mor$p.value
  morI.mc[i] <- s.mor$statistic
}

# Modify p-value to reflect extremes at each end
sign.mc <- ifelse(sign.mc > 0.5, 1 - sign.mc, sign.mc)
# First, generate an empty plot
plot(morI.mc ~ eval(incr.v - incr * 0.5), type = "n", ann = FALSE, axes = FALSE)

# Set the background plot to grey then add white grids
u <- par("usr") # Get plot are coordinates
rect(u[1], u[3], u[2], u[4], col = "#EEEEEE", border = NA)
axis(1, lab = ((incr.v) / 1000), at = (incr.v), tck = 1, col = "#FFFFFF", lty = 1)
axis(2, tck = 1, col = "#FFFFFF", lty = 1, labels = FALSE)

# Add the theoretical "no autocorelation" line
abline(h = -1 / (length(dfg$kuli_no2s_geomAgg)), col = "grey20")

# Add the plot to the canvas
par(new = TRUE)
plot(morI.mc ~ eval(incr.v - incr * 0.5),
     type = "b", xaxt = "n", las = 1,
     xlab = "Distance (km)", ylab = "Moran's I")
points(morI.mc ~ eval(incr.v - incr * 0.5), 
       col = ifelse(sign.mc < 0.01, "red", "grey"), 
       pch = 16, cex = 2.0)

# Add numeric values to points
text(eval(incr.v - incr * 0.5), morI.mc, round(sign.mc,3), pos = 3, cex = 0.5)

##### LISA ####
# Local Statistic #
dfg$li <- localmoran(dfg$kuli_no2s_geomAgg, g.lw)[, 1] 
dfg$localmoranpval <- localmoran(dfg$kuli_no2s_geomAgg,g.lw)[, 5]
index  = dfg$localmoranpval <= 0.05 # outline significant pvals as borders

map1 <- tm_shape(dfg) +
  tm_fill(col = "li", style = "quantile",title="Local Moran Statistic") +
  tm_shape(dfg[index,]) + tm_borders(lwd=0.3, col="black") +
  tm_layout(legend.position = c("left","bottom"), frame = T, legend.outside = F,
            legend.title.fontfamily = "serif", main.title.position = "center",
            legend.width=1, legend.height=1, legend.text.size=0.8, legend.title.size = 1.2,
            legend.bg.color="grey100", legend.bg.alpha=.7, title.fontfamily="serif", legend.text.fontfamily = "serif")

# Local Moran Cluster Map #
queen_wts <- queen_weights(dfg)
moran <- local_moran(queen_wts, dfg["kuli_no2s_geomAgg"])
moran_lbls <- lisa_labels(moran)[1:5]
moran_colors <- setNames(lisa_colors(moran)[1:5], moran_lbls)

dfg_lmm <- dfg |>
  mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
         cluster = factor(moran_lbls[cluster_num], levels = moran_lbls))

map2 <- tm_shape(dfg_lmm) +
  tm_polygons("cluster", lwd=0, title="Moran Clusters",
              palette = moran_colors) +
  tm_layout(legend.position = c("left","bottom"), frame = T, legend.outside = F,
            legend.title.fontfamily = "serif", main.title.position = "center",
            legend.width=0.7, legend.height=1, legend.text.size=0.8, legend.title.size = 1.2,
            legend.bg.color="grey100", legend.bg.alpha=.7, title.fontfamily="serif", legend.text.fontfamily = "serif")

tmap_arrange(map1, map2,ncol = 2)

##### Getis Ord ####
# tutorial
local_g <- localG(dfg$kuli_no2s_geomAgg, g.lw)
local_g <- cbind(dfg, as.matrix(local_g))
names(local_g)[6] <- "gstat"
tm_shape(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty", lwd=0) +
  tm_borders(alpha=.4)
# other 
fips <- order(dfg$kuli_no2s_geomAgg)
gi.fixed <- localG(dfg$kuli_no2s_geomAgg, g.lw)
hunan.gi <- cbind(dfg, as.matrix(gi.fixed))
#names(hunan.gi)[16] <- "gstat"
tm_shape(hunan.gi) +
  tm_fill(col = "as.matrix.gi.fixed.", 
          style = "pretty",
          palette="-RdBu",
          title = "local Gi", lwd=0) +
  tm_borders(alpha = 0.5)

#### LEEDS class ###
g.nb2 <- dnearneigh(st_centroid(dfg), 0, 1000)
g.nb2

g.nb2[[541]] = as.integer(544)
g.nb2[[542]] = as.integer(544)
g.nb2[[543]] = as.integer(544)
g.nb2[[607]] = as.integer(608)
g.nb2[[979]] = as.integer(980)
g.nb2[[1020]] = as.integer(1021)
g.nb2[[1498]] = as.integer(1499)
g.nb2[[1695]] = as.integer(1696)
g.nb2[[3344]] = as.integer(3346)
g.nb2[[3979]] = as.integer(3982)
g.nb2[[3980]] = as.integer(3982)
g.nb2[[5304]] = as.integer(5305)
g.nb2[[5306]] = as.integer(5305)
g.nb2[[5053]] = as.integer(5054)
g.nb2[[5050]] = as.integer(5054)
g.nb2[[5301]] = as.integer(5054)
g.nb2[[5303]] = as.integer(5054)
g.nb2[[5357]] = as.integer(5054)
g.nb2[[5777]] = as.integer(5054)
g.nb2[[5916]] = as.integer(5054)
g.nb2[[6023]] = as.integer(5054)
g.nb2[[6221]] = as.integer(5054)

g.nb2[[5360]] = as.integer(5361)
g.nb2[[5780]] = as.integer(5781)
g.nb2[[5919]] = as.integer(5920)
g.nb2[[6026]] = as.integer(6027)
g.nb2[[6224]] = as.integer(6225)

g.lw2 = nb2listw(g.nb2, style = 'B')
dfg$gstat <- localG(dfg$kuli_no2s_geomAgg, g.lw2)
tm_shape(dfg) + 
  tm_polygons(col= 'gstat',title= "G Statistic", lwd = 0.01, 
              legend.format=list (flag= "+"), palette = "RdBu", midpoint = 0) + 
  tm_layout(frame = F)

#### GWR ####
# convert to sp
hex.sp = as(hexgrid, "Spatial")
# determine the kernel bandwidth
bw_adap <- bw.gwr(data=hex.sp, formula=formula, approach = "AIC", kernel="bisquare",
             adaptive = T) # adaptive as no of neighbors
bw_fixed <- bw.gwr(data=hex.sp, formula=formula,approach = "AIC", kernel="bisquare",
             adaptive = F) # fixed bandwidth (in m)
summary(as.vector(st_distance(hexgrid)))

# specify GWR model
gwr_n1408 <- gwr.basic(formula, 
                   adaptive = T,
                   data = hex.sp,
                   bw = bw_adap)
gwr <- gwr_n1408
save(gwr, file="outputs/models/gwr_1_n1408_2.Rdata")

# specify MGWR model
mgwr_n1408 <- gwr.multiscale(formula,
                        data = hex.sp,
                        adaptive = T, max.iterations = 10000,
                        criterion="CVR",
                        kernel = "bisquare",
                        bws0=rep(100, 11),
                        verbose = F, predictor.centered=rep(T, 10))
save(mgwr_n1408, file="outputs/models/mgwr_1_n1408_2.Rdata")
#load("outputs/models/gwr_1_n1408.Rdata")
#load("outputs/models/mgwr_1_n1408.Rdata")

# second MGWR model
#mgwr_2 <- gwr.multiscale(formula, data = gb.sp, adaptive = T,max.iterations = 10000,criterion="CVR", kernel = "bisquare",bws0=c(mbwa),bw.seled=rep(T, 13),verbose = F, predictor.centered=rep(F, 12))

mbwa <- mgwr_n1408[[2]]$bws
mgwr_2 <- mgwr_n1408
gwr
mgwr_2
mgwr_2$GW.diagnostic
mbwa_2 <- round(mgwr_2[[2]]$bws,1)

# take out coefficients
# Examine Boxplots of coef distributions
gwr_coef_cols <- data.frame(gwr$SDF@data[, 1:11])
gwr_coef_cols$id <- 1:nrow(gwr_coef_cols)
gwr_coef_cols$Model <- "GWR"
gwr_long <- melt(gwr_coef_cols, id = c("id","Model"))
mgwr_coef_cols <- data.frame(mgwr_2$SDF@data[, 1:11])
mgwr_coef_cols$id <- 1:nrow(mgwr_coef_cols)
mgwr_coef_cols$Model <- "MGWR"
mgwr_long <- melt(mgwr_coef_cols, id = c("id","Model"))
olssum <- data.frame(lm$coefficients)
olssum <- cbind(variable = rownames(olssum), olssum)
rownames(olssum) <- 1:nrow(olssum)
colnames(olssum)[2] <- "value"
olssum$Model <- "POLS"
olssum$id <- 1:nrow(olssum)
olssum$variable[olssum$variable == '(Intercept)'] <- 'Intercept'
olssum <- olssum[,c(4,3,1,2)]

allcoefs <- rbind(gwr_long, mgwr_long)
allcoefs <- rbind(allcoefs, olssum)

ggplot() +
  geom_boxplot(allcoefs, mapping = aes(x = variable, y = value, col= Model), position="dodge2") +
  stat_boxplot(geom ='errorbar') +
  theme_minimal() +
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=13,  family="serif")) +
  scale_color_manual(values=c("#6ECCAF","#344D67", "black")) +
  ylab('Coefficient estimate') +xlab ('')+
  #labs(title ="Boxplots of Coefficient estimates") +
  coord_cartesian(ylim = c(-11, 15))

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

# Examine Boxplots of SE distributions - not that useful - would need RMSE
gwr_se <- gwr$SDF@data[, 17:29]
gwr_se$id <- 1:nrow(gwr_se)
gwr_se$kind <- "GWR"
gwr_se_long <- melt(gwr_se, id = c("id","kind"))
mgwr_se <- mgwr_2$SDF@data[, 14:24]
mgwr_se$id <- 1:nrow(mgwr_se)
mgwr_se$kind <- "MGWR"
mgwr_se_long <- melt(mgwr_se, id = c("id","kind"))
allses <- rbind(gwr_se_long, mgwr_se_long)
ggplot(allses, aes(x = variable, y = value, col= kind)) +
  geom_boxplot()

#####Summary tables #####
# create a table with coefficient stats for GWR
tab.gwr <- apply(gwr$SDF@data[, 1:11], 2, summary)

tab.gwr <- t(round(tab.gwr, 3))
gwr_meancoef <- data.frame(tab.gwr[,4])
stargazer(gwr_meancoef, summary=FALSE, digits=2, type="text")

# create a table with coefficient stats for MGWR
coefs_msgwr = apply(mgwr_2$SDF@data[, 1:11], 2, summary)
tab.mgwr = data.frame(Bandwidth = mbwa_2, t(round(coefs_msgwr,3)))
names(tab.mgwr)[c(3,6)] = c("Q1", "Q3")
tab.mgwr

olssum <- data.frame(lm$coefficients)
olssum <- cbind(variable = rownames(olssum), olssum)
rownames(olssum) <- 1:nrow(olssum)
colnames(olssum)[2] <- "coef"

summary_table <- data.frame(olssum, tab.gwr)
summary_table
##### Moran #####
#test for residuals from MGWR
moran.test(gwr$SDF$residual, listw=g.lw)
moran.test(mgwr_2$SDF$residual, listw=g.lw)
moran.test(gb$ols.resids, listw=g.lw)
moran.test(gb$olsopt.resids, listw=g.lw)

# show moran's I
c(unlist(gwr.morani[3])[1],unlist(gwr.morani[2]))

# show diagnostics
c(gwr$GW.diagnostic$AICc, gwr$GW.diagnostic$gwR2.adj)
c(mgwr_8_12$GW.diagnostic$AICc, mgwr_8_12$GW.diagnostic$R2.val)
c(mgwr_2$GW.diagnostic$AICc, mgwr_2$GW.diagnostic$R2.val)

##### Plotting results #####
mgwr2_sf = st_as_sf(mgwr_2$SDF)
#mgwr1_sf = st_as_sf(mgwr_8_12$SDF)
gwr_sf = st_as_sf(gwr$SDF)

#plot coefficients for GWR
tmap_mode("plot")
tm_shape(gwr_sf) +
  tm_fill(c("medianIncome", "bornOverseas", "privateTransporTtoWork", "PTtoWork", "cycleToWork","noCar","carsPerPreson","PrEuropeanDesc","PrMaoriDesc","deprivation"), palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)

tm_shape(mgwr2_sf) +
  tm_fill(c("privateTransporTtoWork", "PTtoWork", "cycleToWork","noCar","carsPerPreson","PrEuropeanDesc","PrMaoriDesc"), palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)

# plot diverging coefs for GWR
tm_shape(mgwr2_sf) +
  tm_fill(c("privateTransporTtoWork", "PTtoWork", "cycleToWork","noCar","carsPerPreson","PrEuropeanDesc","PrMaoriDesc"),midpoint = 0, style = "kmeans") +
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
    tm_polygons(var_name, midpoint = 0, legend.hist = F, lwd=0.05,
                style = "kmeans", title = varN, title.fontfamily="serif") +
    tm_style("col_blind") +
    # now add the tvalues layer
    tm_shape(x[signif,]) + tm_borders(lwd = 0.4) +
    tm_layout(main.title = "GWR", legend.position = c("left","bottom"), frame = T, legend.outside = F,
              legend.title.fontfamily = "serif", main.title.size = 1, main.title.position = "center",
              #legend.width=.5, legend.height=1, legend.text.size=.8,legend.title.size=2,
              legend.bg.color="grey100", legend.bg.alpha=.7, main.title.fontfamily="serif") #,, legend.height=.5, 
  #legend.hist.height=.2, legend.hist.width=.3, legend.hist.bg.color="grey90", legend.hist.bg.alpha=.4)
  #tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.6) +
  #tm_compass(type = "4star", size = 2, position = c("left", "top"))
  # return the output
  p_out
}

# significant GWR maps
allmgwr_map_func(gwr_sf, "carsPerPreson", "CarsPerPerson")
allmgwr_map_func(gwr_sf, "PTtoWork", "PTtoWork")
allmgwr_map_func(gwr_sf, "cycleToWork", "Cycle to work")
allmgwr_map_func(gwr_sf, "privateTransporTtoWork", "Private transport\nto work")
allmgwr_map_func(gwr_sf, "PrEuropeanDesc", "EU Desc")
allmgwr_map_func(gwr_sf, "PrMaoriDesc", "Maori Desc")
allmgwr_map_func(gwr_sf, "noCar", "No Car")

# significant MGWR maps
allmgwr_map_func(mgwr2_sf, "carsPerPreson", "CarsPerPerson")
allmgwr_map_func(mgwr2_sf, "PTtoWork", "PTtoWork")
allmgwr_map_func(mgwr2_sf, "cycleToWork", "Cycle to work")
allmgwr_map_func(mgwr2_sf, "privateTransporTtoWork", "Private transport\nto work")
allmgwr_map_func(mgwr2_sf, "PrEuropeanDesc", "EU Desc")
allmgwr_map_func(mgwr2_sf, "PrMaoriDesc", "Maori Desc")
allmgwr_map_func(mgwr2_sf, "noCar", "No Car")

# MGWR_2 Only significant coefficients
map_signif_coefs_func(x = mgwr2_sf, "carsPerPreson", "carsPerPreson_TV", "MGWR", "PrMaoriDesc")
map_signif_coefs_func(x = mgwr2_sf, "PTtoWork", "PTtoWork_TV", "MGWR", "PTtoWork") # insignificant
map_signif_coefs_func(x = mgwr2_sf, "cycleToWork", "cycleToWork_TV", "MGWR", "cycleToWork") #insignificant
map_signif_coefs_func(x = mgwr2_sf, "privateTransporTtoWork", "privateTransporTtoWork_TV", "MGWR", "privateTransporTtoWork")
map_signif_coefs_func(x = mgwr2_sf, "PrEuropeanDesc", "PrEuropeanDesc_TV", "MGWR", "PrEuropeanDesc")
map_signif_coefs_func(x = mgwr2_sf, "PrMaoriDesc", "PrMaoriDesc_TV", "MGWR", "PrMaoriDesc")# insignificant
map_signif_coefs_func(x = mgwr2_sf, "noCar", "noCar_TV", "MGWR", "noCar") #insignificant

# MGWR_2 Only significant coefficients
map_signif_coefs_diverging_func(x = mgwr2_sf, "carsPerPreson", "carsPerPreson_TV", "MGWR", "PrMaoriDesc")
map_signif_coefs_diverging_func(x = mgwr2_sf, "PTtoWork", "PTtoWork_TV", "MGWR", "PTtoWork") # insignificant
map_signif_coefs_diverging_func(x = mgwr2_sf, "cycleToWork", "cycleToWork_TV", "MGWR", "cycleToWork") #insignificant
map_signif_coefs_diverging_func(x = mgwr2_sf, "privateTransporTtoWork", "privateTransporTtoWork_TV", "MGWR", "privateTransporTtoWork")
map_signif_coefs_diverging_func(x = mgwr2_sf, "PrEuropeanDesc", "PrEuropeanDesc_TV", "MGWR", "PrEuropeanDesc")
map_signif_coefs_diverging_func(x = mgwr2_sf, "PrMaoriDesc", "PrMaoriDesc_TV", "MGWR", "PrMaoriDesc")# insignificant
map_signif_coefs_diverging_func(x = mgwr2_sf, "noCar", "noCar_TV", "MGWR", "noCar") #insignificant

#tmap_arrange(, , widths = c(.5,.5))

##### Residuals #####
#minmaxNORM <- function(x) {return (((x - min(x))) / (max(x) - min(x))*(0.1166441+0.0992775)-0.0992775)}
# determine studentised residuals and attach
gb$ols.resids <- rstudent(m)
#gb <- mutate(gb, ols.resids = minmaxNORM(ols.resids))
gb$olsopt.resids <- rstudent(ols_opt)
#gb <- mutate(gb, olsopt.resids = minmaxNORM(olsopt.resids))
gb$gwr.resids <- gwr$SDF$residual
gb$mgwr.resids <- mgwr_2$SDF$residual

#scatterplots of residuals
ggplot(gb) +
  geom_point(aes(x = share_leave, y=mgwr.resids), col="black", alpha=0.5) +
  geom_point(aes(x = share_leave, y=gwr.resids), col="blue", alpha=0.2)

# map residuals
residual_map_func = function(var_name, resid) {
  p_out = tm_shape(gb) +
  tm_polygons(var_name, midpoint = 0, legend.hist = TRUE, lwd=0.05, palette= "PuOr",
              frame = T, style = "kmeans", title = resid, title.fontfamily="serif") +
  tm_style("col_blind") +
  tm_layout(legend.position = c("right","top"), frame = T, legend.outside = F,
            legend.title.fontfamily = "serif", legend.just="right",
            legend.width=.5, legend.height=1, legend.text.size=.8,legend.title.size=2,
            legend.bg.color="grey100", legend.bg.alpha=.7,
            legend.hist.height=.1, legend.hist.width =.5, legend.hist.bg.color="grey90", legend.hist.bg.alpha=.5)+
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.4) +
  tm_compass(type = "4star", size = 1, position = c("left", "top"))
  
  p_out
}
polsresmap <- residual_map_func("olsopt.resids","POLS Residuals") #
gwrresmap <- residual_map_func("gwr.resids","GWR Residuals")
mgwrresmap <- residual_map_func("mgwr.resids","MGWR Residuals")

tmap_arrange(polsresmap, gwrresmap, mgwrresmap, widths = c(.5,.5), ncol = 3)



#### Spatial Econometric models ####
# first Moran's I
moran(hexgrid$kuli_no2s_geomAgg, hex.lw, length(hexgrid$kuli_no2s_geomAgg), Szero(hex.lw))
moran.test(hexgrid$kuli_no2s_geomAgg, hex.lw)

# Residual test
lm.morantest(hex.lm,hex.lw) # H0 says no spatial correlation in the residuals

# LaGrenge Multiplier Tests
lm.LMtests(hex.lm, hex.lw, test='all') #error and Spatial Lag model (lag y)

# Spatially Lagged X (lag X -SLX)
reg2 <- lmSLX(formula, hexgrid, hex.lw)
summary(reg2)

# Spaitally Lagged y Model (Autoregressive)
reg3 <- lagsarlm(formula, hexgrid, hex.lw)
summary(reg3)
save(reg3, file="outputs/models/spatiallyLaggedModel.Rdata")
auto <- spautolm(formula, hexgrid, hex.lw)

# Spatial error model
reg4 <- errorsarlm(formula, hexgrid, hex.lw)
save(reg4, file="outputs/models/spatialErrorModel.Rdata")
summary(reg4)
impacts(reg4, hex.lw)
summary(impacts(reg4,hex.lw, R=500), zstat=TRUE)

