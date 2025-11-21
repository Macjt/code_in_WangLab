install.packages("spacetime")
install.packages("gstate")


install.packages(c("gstat", "raster", "rgdal", "rgeos"))


library(sp)
library(spacetime)
library(gstat)
library(raster)
library(rgdal)
library(rgeos) 



setwd("...")
data <- read.table("ozon_tram1_14102011_14012012.csv", sep=",", header=T)
paste(data$generation_time[1])
data$TIME <- as.POSIXlt(as.numeric(substr(paste(data$generation_time), 1, 10)), origin="1970-01-01")

as.POSIXlt(as.numeric(substr(paste(data$generation_time[1]), start=1, stop=10)), origin="1970-01-01")
# [1] "2011-10-14 11:14:46 CEST" 
data$longitude[1]
# [1] 832.88198
# 76918 Levels: 829.4379 829.43822 829.44016 829.44019 829.4404 ... NULL
data$latitude[1]
# [1] 4724.22833
# 74463 Levels: 4721.02182 4721.02242 4721.02249 4721.02276 ... NULL 

data$LAT <- as.numeric(substr(paste(data$latitude),1,2))+(as.numeric(substr(paste(data$latitude),3,10))/60)
data$LON <- as.numeric(substr(paste(data$longitude),1,1))+(as.numeric(substr(paste(data$longitude),2,10))/60) 
data <- na.omit(data)
min(data$TIME)
# [1] "2011-10-14 11:14:46 CEST"
max(data$TIME)
# [1] "2012-01-14 13:40:43 CET" 
sub <- data[data$TIME>=as.POSIXct('2011-12-12 00:00 CET')&data$TIME<=as.POSIXct('2011-12-14 23:00 CET'),]
nrow(sub)
# [1] 6734 

#Create a SpatialPointsDataFrame
coordinates(sub)=~LON+LAT
projection(sub)=CRS("+init=epsg:4326")
#Transform into Mercator Projection
ozone.UTM <- spTransform(sub,CRS("+init=epsg:3395")) 
ozoneSP <- SpatialPoints(ozone.UTM@coords,CRS("+init=epsg:3395")) 
dupl <- zerodist(ozoneSP) 

ozoneDF <- data.frame(PPB=ozone.UTM$ozone_ppb[-dupl[,2]]) 
ozoneTM <- as.POSIXct(ozone.UTM$TIME[-dupl[,2]],tz="CET") 
timeDF <- STIDF(ozoneSP,ozoneTM,data=ozoneDF) 
stplot(timeDF) 
var <- variogramST(PPB~1,data=timeDF,tunit="hours",assumeRegular=F,na.omit=T) 

plot(var,map=F) 
plot(var,map=T) 
plot(var,wireframe=T) 

demo(stkrige) 

# lower and upper bounds
pars.l <- c(sill.s = 0, range.s = 10, nugget.s = 0,sill.t = 0, range.t = 1, nugget.t = 0,sill.st = 0, range.st = 10, nugget.st = 0, anis = 0)
pars.u <- c(sill.s = 200, range.s = 1000, nugget.s = 100,sill.t = 200, range.t = 60, nugget.t = 100,sill.st = 200, range.st = 1000, nugget.st = 100,anis = 700) 

separable <- vgmST("separable", space = vgm(-60,"Sph", 500, 1),time = vgm(35,"Sph", 500, 1), sill=0.56) 
plot(var,separable,map=F) 

separable_Vgm <- fit.StVariogram(var, separable, fit.method=0)
attr(separable_Vgm,"MSE")
# [1] 54.96278 

plot(var,separable_Vgm,map=F) 
extractPar(separable_Vgm)
range.s nugget.s range.t nugget.t sill
# 199.999323 10.000000 99.999714 1.119817 17.236256 

prodSumModel <- vgmST("productSum",space = vgm(1, "Exp", 150, 0.5),time = vgm(1, "Exp", 5, 0.5),k = 50) 
prodSumModel_Vgm <- fit.StVariogram(var, prodSumModel,method = "L-BFGS-B",lower=pars.l)
attr(prodSumModel_Vgm, "MSE")
# [1] 215.6392 

metric <- vgmST("metric", joint = vgm(50,"Mat", 500, 0), stAni=200) 
metric_Vgm <- fit.StVariogram(var, metric, method="L-BFGS-B",lower=pars.l)
attr(metric_Vgm, "MSE")
# [1] 79.30172 

sumMetric <- vgmST("sumMetric", space = vgm(psill=5,"Sph", range=500, nugget=0),time = vgm(psill=500,"Sph", range=500, nugget=0), joint = vgm(psill=1,"Sph", range=500, nugget=10), stAni=500) 
sumMetric_Vgm <- fit.StVariogram(var, sumMetric, method="L-BFGS-B",lower=pars.l,upper=pars.u,tunit="hours")
attr(sumMetric_Vgm, "MSE")
# [1] 58.98891 

SimplesumMetric <- vgmST("simpleSumMetric",space = vgm(5,"Sph", 500, 0),time = vgm(500,"Sph", 500, 0), joint = vgm(1,"Sph", 500, 0), nugget=1, stAni=500) 
SimplesumMetric_Vgm <- fit.StVariogram(var, SimplesumMetric,method = "L-BFGS-B",lower=pars.l)
attr(SimplesumMetric_Vgm, "MSE")
# [1] 59.36172 
plot(var,list(separable_Vgm, prodSumModel_Vgm, metric_Vgm, sumMetric_Vgm, SimplesumMetric_Vgm),all=T,wireframe=T) 

roads <- shapefile("VEC25_str_l_Clip/VEC25_str_l.shp") 
Klass1 <- roads[roads$objectval=="1_Klass",] 
Klass1.UTM <- spTransform(Klass1,CRS("+init=epsg:3395")) 
Klass1.cropped <- crop(Klass1.UTM,ozone.UTM) 
plot(Klass1.cropped)
plot(ozone.UTM,add=T,col="red") 

sp.grid.UTM <- spsample(Klass1.cropped,n=1500,type="random") 
tm.grid <- seq(as.POSIXct('2011-12-12 06:00 CET'),as.POSIXct('2011-12-14 09:00 CET'),length.out=5) 
grid.ST <- STF(sp.grid.UTM,tm.grid) 
# Kriging
pred <- krigeST(PPB~1, data=timeDF, modelList=sumMetric_Vgm, newdata=grid.ST) 
stplot(pred) 

vignette("st", package = "gstat") 
demo(stkrige) 

# References
# Gräler, B., 2012. Different concepts of spatio-temporal kriging [WWW Document]. URL geostat-course.org/system/files/part01.pdf (accessed 8.18.15).
# 
# Gräler, B., Pebesma, Edzer, Heuvelink, G., 2015. Spatio-Temporal Interpolation using gstat.
# 
# Gräler, B., Rehr, M., Gerharz, L., Pebesma, E., 2013. Spatio-temporal analysis and interpolation of PM10 measurements in Europe for 2009.
# 
# Oliver, M., Webster, R., Gerrard, J., 1989. Geostatistics in Physical Geography. Part I: Theory. Trans. Inst. Br. Geogr., New Series 14, 259–269. doi:10.2307/622687
# 
# Sherman, M., 2011. Spatial statistics and spatio-temporal data: covariance functions and directional properties. John Wiley & Sons.


