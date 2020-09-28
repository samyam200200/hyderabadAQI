setwd("C:/Users/gocoo/OneDrive/Documents/")
library(dplyr)
hbrd<-read.csv("2020mean.csv")
Hrbd<-read.csv("2020AQILonLat.csv")
hbrd$location<-rep(c(unique(Hrbd$location)),each=3) 
Hrbd<-Hrbd[,c('location', 'lon', 'lat')]
hbrd<-left_join(hbrd, unique(Hrbd), NULL)
fst<-subset(x = hbrd, tri==1)
fst<-fst[,c("mean_aqi", "location", "lon", "lat")]
library(sp)
library(raster)
r1 <- shapefile("~/TSDM/District_Boundary.shp")
#("Hyderabad", "Sangareddy","Rangareddy","Medchal Malkajgiri")]
r1<-r1[c(30,26,28,17),]
r2<-aggregate(r1)
plot(r2)
dsp<-SpatialPoints(fst[,3:4], proj4string = CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))
crs(r2)<-"+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
dsp <- SpatialPointsDataFrame(dsp, fst)
dsp@bbox<-r2@bbox
library(tmap)
tm_shape(r2)+tm_polygons() +tm_shape(dsp)+tm_dots(col="mean_aqi", palette = "RdBu", auto.palette.mapping = FALSE,title="Sampled Aqi)", size=0.7)+tm_text("location", just="centre", xmod=.2, size = 0.7)+tm_legend(legend.outside=TRUE)

library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
th  <-  as(dirichlet(as.ppp(dsp)), "SpatialPolygons")
proj4string(th) <- proj4string(dsp)
th.z     <- over(th, dsp)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
th.clp   <- raster::intersect(r2,th.spdf)


tm_shape(th.clp) + 
  tm_polygons(col="mean_aqi", palette="RdBu",
              title="Predicted AQI") +
  tm_legend(legend.outside=TRUE)

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(dsp, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(dsp) <- proj4string(dsp) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(dsp)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(mean_aqi ~ 1, dsp, newdata=grd, idp=2.0)

# Convert to raster object then clip to Hyderabad
r       <- raster(P.idw)
r.m     <- mask(r, r2)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted AQI") + 
  tm_shape(dsp) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
