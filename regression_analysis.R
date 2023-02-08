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

#### Loading data ####
# load the kuli data
kuli = st_read('data/geographic/sa1_kuli_all.gpkg', quiet = T) # transform to OSGB projection
kuli <- kuli |> 
  subset(select = c(SA12018_V1_00, kuli_no2s_MPIAgg)) |> st_transform(27291)

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

# Dealing with missing values #
# find NAs
md.pattern(census)
aggr_plot <- aggr(census, col=c('navyblue','orange'), numbers=TRUE, sortVars=TRUE, labels=names(census), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

sa1_census <- left_join(sa1_polys, census, by = c("SA12018_V1_00"="code"))
tmap_mode("plot")
tm_shape(sa1_census) + tm_polygons(col="maoriDescent", lwd=0, style="kmeans")

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
cor <- cor(x = df[3:13], y = df[3:13], use="complete.obs")
corrplot(cor, tl.srt = 25)
corr <- rcorr(as.matrix(df[3:13]))

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
df[3:13] |>
  gather(-kuli_no2s_geomAgg_wrewards, key = "var", value = "value") |> 
  ggplot(aes(x = kuli_no2s_geomAgg_wrewards, y = value)) +
  facet_wrap(~ var, scales = "free") +
  geom_point(alpha=0.2) +
  theme_bw() +
  geom_smooth(method="lm")

#### OLS model ####
formula = as.formula(kuli_no2s_MPIAgg ~ medianIncome + bornOverseas + privateTransporTtoWork +
                       PTtoWork + cycleToWork + noCar + carsPerPreson + PrEuropeanDesc + PrMaoriDesc) # construct the OLS model
m = lm(formula, data = df)
summary(m)
#AICc(m)
#deviance(m)

# find optimal variables - same
step.res = stepAIC(m, trace = 0)
summary(step.res)

stargazer(m, flip=F, type="latex", single.row = F, style="qje")
#### Autocorrelation ####
##### Moran's I####
g.nb <- poly2nb(dfg)

g.lw = nb2listw(g.nb)
#plot spatially lagged mean
dfg$lagged.means <- lag.listw(g.lw, dfg$kuli_no2s_MPIAgg)
tm_shape(dfg) + 
  tm_polygons(col='lagged.means', 
              title='KULI',
              palette = "YlGnBu", lwd=0, style="kmeans")

# moran scatterplot
ggplot(data = dfg, aes(x = kuli_no2s_MPIAgg, y = lagged.means)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_hline(yintercept = mean(dfg$lagged.means), lty = 2) +
  geom_vline(xintercept = mean(dfg$kuli_no2s_MPIAgg), lty = 2) +
  geom_abline() +
  coord_equal()
# create and assign the Moran plot - with more details
moran.plot(x = dfg$kuli_no2s_MPIAgg, listw = g.lw)
# get Moran's I statistic
moran.test(x = dfg$kuli_no2s_MPIAgg, listw = g.lw) 
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
  s.mor <- moran.mc(dfg$kuli_no2s_MPIAgg, s.lw, nsim=599, zero.policy = TRUE)
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
abline(h = -1 / (length(dfg$kuli_no2s_MPIAgg)), col = "grey20")

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
# Tutorial #
local <- localmoran(x = dfg$kuli_no2s_MPIAgg, listw = g.lw)
moran.map <- cbind(dfg, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic") 

quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.qualification <- dfg$kuli_no2s_MPIAgg - mean(dfg$kuli_no2s_MPIAgg)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.2

# builds a data quadrant
quadrant[m.qualification >0 & m.local>0] <- 4  
quadrant[m.qualification <0 & m.local<0] <- 1      
quadrant[m.qualification <0 & m.local>0] <- 2
quadrant[m.qualification >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(dfg,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

### from LEEDS UNI ###
# note how the result is assigned directly to gb
dfg$lI <- localmoran(x = dfg$kuli_no2s_MPIAgg, listw = g.lw)[, 1] 
# create the map
p1 = tm_shape(dfg) +
  tm_polygons(col= 'lI',title= "Local Moran's I", lwd = 0,
              legend.format=list (flag= "+")) +
  tm_style('col_blind') +
  tm_layout(legend.position = c("left", "top")) + tm_layout(frame = F)
# print the map
dfg$localmoranpval <- localmoran(dfg$kuli_no2s_MPIAgg,g.lw)[, 5]
# create the map
mypalette = c("#31A354", "#A1D99B","#E5F5E0", "lightgrey")
p2 = tm_shape(dfg) +
  tm_polygons(col= 'localmoranpval',title= "p-value",breaks= c(0, 0.01, 0.05, 0.10, 1), 
              border.col = "black", lwd = 0,
              palette = mypalette) +
  tm_layout(legend.position = c("left", "top")) + tm_layout(frame = F) 
#plot all 3
p0 = tm_shape(dfg) + tm_fill("kuli_no2s_MPIAgg") +
  tm_layout(legend.position = c("left", "top")) 
tmap_arrange(p0, p1, p2,ncol = 3)
# outline significant pvals as borders
index  = dfg$localmoranpval <= 0.05
p0 + tm_shape(dfg[index,]) + tm_borders(lwd=0.5)

##### Getis Ord ####
# tutorial
local_g <- localG(dfg$kuli_no2s_MPIAgg, g.lw)
local_g <- cbind(dfg, as.matrix(local_g))
names(local_g)[6] <- "gstat"
tm_shape(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty", lwd=0) +
  tm_borders(alpha=.4)
# other 
fips <- order(dfg$kuli_no2s_MPIAgg)
gi.fixed <- localG(dfg$kuli_no2s_MPIAgg, g.lw)
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
dfg$gstat <- localG(dfg$kuli_no2s_geomAgg_wrewards, g.lw2)
tm_shape(dfg) + 
  tm_polygons(col= 'gstat',title= "G Statistic", lwd = 0.01, 
              legend.format=list (flag= "+"), palette = "RdBu", midpoint = 0) + 
  tm_layout(frame = F)

#### Clustering ####
cents <- st_centroid(dfg[3:13])
D0 <- distm(cents) # the socio-economic distances
tree <- hclustgeo(D0)
plot(tree,hang = -1, label = FALSE, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 5, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

#### Spatial Econometric models ####
#st_write(dfg, "data/geographic/sa1_analysis_kuli.gpkg")
queen.nb <- read.gal("data/geographic/analysis/sa1_queenW.gal", region.id=dfg$SA12018_V1_00)
summary(queen.nb)
listw1 <- nb2listw(queen.nb)

# Moran's I
moran(dfg$kuli_no2s_geomAgg_wrewards, listw1, length(dfg$kuli_no2s_geomAgg_wrewards), Szero(listw1))
moran.test(dfg$kuli_no2s_geomAgg_wrewards, listw1)
#OLS
summary(m)

# Residual test
lm.morantest(m,listw1) # H0 says no spatial correlation in the residuals

# LaGrenge Multiplier Tests
lm.LMtests(m, listw1, test='all') #error and Spatial Lag model (lag y)

# Spatially Lagged X (lag X -SLX)
reg2 <- lmSLX(formula, dfg, listw1)
summary(reg2)

# Spaitally Lagged y Model (Autoregressive)
reg3 <- lagsarlm(formula, dfg, listw1)
summary(reg3)
save(reg3, file="outputs/models/spatiallyLaggedModel.Rdata")

# Spatial error model
reg4 <- errorsarlm(formula, dfg, listw1)
save(reg4, file="outputs/models/spatialErrorModel.Rdata")
summary(reg4)
impacts(reg4, listw1)
summary(impacts(reg4,listw1, R=500), zstat=TRUE)

#### GWR ####
# convert to sp
gb.sp = as(dfg, "Spatial")
# determine the kernel bandwidth
bw_adap <- bw.gwr(data=gb.sp, formula=formula, approach = "AIC", kernel="bisquare",
             adaptive = T) # adaptive in no of neighbors
bw_fixed <- bw.gwr(data=gb.sp, formula=formula,approach = "AIC", kernel="bisquare",
             adaptive = F) # fixed bandwidth (in m)
summary(as.vector(st_distance(gb.sp)))

# specify GWR model
gwr <- gwr.basic(formula, 
                   adaptive = T,
                   data = gb.sp,
                   bw = bw_adap)
gwr
save(gwr, file="outputs/models/gwr_adapnew.Rdata")
load("outputs/models/gwr_adap.Rdata")

# specify MGWR model
mgwr_1 <- gwr.multiscale(formula,
                        data = gb.sp,
                        adaptive = T, max.iterations = 10000,
                        criterion="CVR",
                        kernel = "bisquare",
                        bws0=rep(100, 10),
                        verbose = F, predictor.centered=rep(T, 9))
save(mgwr_1, file="outputs/models/mgwr_1.new.Rdata")
load("outputs/models/mgwr_1.Rdata")

# assign bandwidths 
mbwa <- mgwr_1[[2]]$bws

# Now re-run multiscale GWR with bandwidths found from above but with predictors NOT centered
mgwr_2 <- gwr.multiscale(formula_opt, data = gb.sp, adaptive = T,
                          max.iterations = 10000,
                          criterion="CVR", kernel = "bisquare",
                          bws0=c(mbwa),
                          bw.seled=rep(T, 10),
                          verbose = F, predictor.centered=rep(F, 9))
save(mgwr_2, file="outputs/models/mgwr_2.Rdata")
load("outputs/models/mgwr_2.Rdata")

mgwr_2$GW.diagnostic
mbwa_2 <- round(mgwr_2[[2]]$bws,1)

# take out coefficients
# Examine Boxplots of coef distributions
gwr_coef_cols <- data.frame(gwr$SDF@data[, 1:10])
gwr_coef_cols$id <- 1:nrow(gwr_coef_cols)
gwr_coef_cols$Model <- "GWR"
gwr_long <- melt(gwr_coef_cols, id = c("id","Model"))
mgwr_coef_cols <- data.frame(mgwr_2$SDF@data[, 1:10])
mgwr_coef_cols$id <- 1:nrow(mgwr_coef_cols)
mgwr_coef_cols$Model <- "MGWR"
mgwr_long <- melt(mgwr_coef_cols, id = c("id","Model"))

olssum <- data.frame(ols_opt$coefficients)
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
  ylab('Coefficient estimate') +xlab ('') +
  #labs(title ="Boxplots of Coefficient estimates") +
  coord_cartesian(ylim = c(-2.5, 6.1))

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
gwr_se <- gwr$SDF@data[, 16:25]
gwr_se$id <- 1:nrow(gwr_se)
gwr_se$kind <- "GWR"
gwr_se_long <- melt(gwr_se, id = c("id","kind"))
mgwr_se <- mgwr_2$SDF@data[, 13:22]
mgwr_se$id <- 1:nrow(mgwr_se)
mgwr_se$kind <- "MGWR"
mgwr_se_long <- melt(mgwr_se, id = c("id","kind"))
allses <- rbind(gwr_se_long, mgwr_se_long)
ggplot(allses, aes(x = variable, y = value, col= kind)) +
  geom_boxplot()

#####Summary tables #####
# create a table with coefficient stats for GWR
tab.gwr <- apply(gwr$SDF@data[, 1:10], 2, summary)
tab.gwr <- t(round(tab.gwr, 3))
gwr_meancoef <- data.frame(tab.gwr[,4])
stargazer(gwr_meancoef, summary=FALSE, digits=2, type="text")

# create a table with coefficient stats for MGWR
coefs_msgwr = apply(mgwr_2$SDF@data[, 1:10], 2, summary)
tab.mgwr = data.frame(Bandwidth = mbwa_2, t(round(coefs_msgwr,3)))
names(tab.mgwr)[c(3,6)] = c("Q1", "Q3")
tab.mgwr

olssum <- data.frame(ols_opt$coefficients)
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
tm_shape(gwr_sf) +
  tm_fill(c("younger_adults", "white", "christian", "english_speaking", "single_ethnicity_household","not_good_health","degree_educated","private_transport_to_work","eu_born"), palette = "viridis", style = "kmeans") +
  tm_layout(legend.position = c("right","top"), frame = F)

# plot diverging coefs for GWR
tm_shape(gwr_sf) +
  tm_fill(c("younger_adults", "white", "christian", "english_speaking", "single_ethnicity_household","not_good_health","degree_educated","private_transport_to_work","eu_born"),midpoint = 0, style = "kmeans") +
  tm_style("col_blind")+
  tm_layout(legend.position = c("right","top"), frame = F)

# all plotting functions
allmgwr_map_func = function(data, var_name, titl) {
  mapout = tm_shape(data) +
    tm_polygons(var_name, lwd=0.25,style = "kmeans", title = titl, title.fontfamily="serif") +
    tm_style("col_blind") +
    tm_layout(main.title = "MGWR", legend.position = c("right","top"), frame = T, legend.outside = F,
              legend.title.fontfamily = "serif", main.title.size = 1, main.title.position = "center",
              #legend.width=.5, legend.height=1, legend.text.size=.8,legend.title.size=2,
              legend.bg.color="grey100", legend.bg.alpha=.7, main.title.fontfamily="serif")
    #tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.4) +
    #tm_compass(type = "4star", size = 1, position = c("left", "top"))
  
    mapout
}
my_map_signif_coefs_func = function(x, var_name, var_name_TV, method) {
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
my_map_signif_coefs_diverging_func = function(x, var_name, var_name_TV, method, varN) {
  # determine which are significant
  tval = x %>% dplyr::select(all_of(var_name_TV)) %>% st_drop_geometry()
  signif = tval < -1.96 | tval > 1.96
  # map the counties
  p_out = tm_shape(x) +
    tm_polygons(var_name, midpoint = 0, legend.hist = F, lwd=0.05,
                style = "kmeans", title = varN, title.fontfamily="serif") +
    tm_style("col_blind") +
    # now add the tvalues layer
    tm_shape(x[signif,]) + tm_borders(lwd = 0.25) +
    tm_layout(main.title = "GWR", legend.position = c("right","top"), frame = T, legend.outside = F,
              legend.title.fontfamily = "serif", main.title.size = 1, main.title.position = "center",
              #legend.width=.5, legend.height=1, legend.text.size=.8,legend.title.size=2,
              legend.bg.color="grey100", legend.bg.alpha=.7, main.title.fontfamily="serif") #,, legend.height=.5, 
  #legend.hist.height=.2, legend.hist.width=.3, legend.hist.bg.color="grey90", legend.hist.bg.alpha=.4)
  #tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.6) +
  #tm_compass(type = "4star", size = 2, position = c("left", "top"))
  # return the output
  p_out
}

# significant MGWR maps
esmap <- allmgwr_map_func(mgwr2_sf, "english_speaking", "English speaking")
demap <- allmgwr_map_func(mgwr2_sf, "degree_educated", "Degree educated")
eubmap <- allmgwr_map_func(mgwr2_sf, "eu_born", "EU born")
pttwmap <- allmgwr_map_func(mgwr2_sf, "private_transport_to_work", "Private transport\nto work")

esmgwrmap <- my_map_signif_coefs_diverging_func(x = gwr_sf, "english_speaking", "english_speaking_TV", "GWR", "English speaking")
demgwrmap <- my_map_signif_coefs_diverging_func(x = gwr_sf, "degree_educated", "degree_educated_TV", "GWR", "Degree educated")
eubmgwrmap <- my_map_signif_coefs_diverging_func(x = gwr_sf, "eu_born", "eu_born_TV", "GWR", "EU born")
pttmgwrmap <- my_map_signif_coefs_diverging_func(x = gwr_sf, "private_transport_to_work", "private_transport_to_work_TV", "GWR", "Private transport\nto work")

tmap_arrange(esmap, demap, pttwmap, eubmap, esmgwrmap, demgwrmap, pttmgwrmap, eubmgwrmap)

# MGWR_2 Only significant coefficients - not_sinif: younger_adults, white, single_ethnicity_household, not_good_health
ngh_MGWR <- allmgwr_map_func("not_good_health_SE", "not_good_health")
tmap_arrange(ngh_GWR, ngh_MGWR)

my_map_signif_coefs_diverging_func(x = mgwr2_sf, "not_good_health", "eu_born_TV", "MGWR", "EU born")
my_map_signif_coefs_func(x = mgwr2_sf, "english_speaking", "english_speaking_TV", "MGWR")

#not_sinif: younger_adults, white, single_ethnicity_household, not_good_health
yagwrmap <- my_map_signif_coefs_diverging_func(x = gwr_sf, "younger_adults", "younger_adults_TV", "GWR", "Younger Adults")
whitgwrmap <- my_map_signif_coefs_diverging_func(x = gwr_sf, "white", "white_TV", "GWR", "White")
sehgwrmap <- my_map_signif_coefs_diverging_func(x = gwr_sf, "single_ethnicity_household", "single_ethnicity_household_TV", "GWR", "Single Ethnicity Household")
nghgwrmap <- my_map_signif_coefs_diverging_func(x = gwr_sf, "not_good_health", "not_good_health_TV", "GWR", "Not Good Health")

yamap <- allmgwr_map_func("younger_adults", "Younger Adults")
whmap <- allmgwr_map_func("white", "White")
sehmap <-allmgwr_map_func("single_ethnicity_household", "Single Ethnicity Household")
nghmap <- allmgwr_map_func("not_good_health", "Not Good Health")
tmap_arrange(yagwrmap, whitgwrmap, sehgwrmap, nghgwrmap, yamap, whmap, sehmap, nghmap)

tmap_arrange(chri_gwr_map, chri_mgwr_2_map, widths = c(.5,.5))

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