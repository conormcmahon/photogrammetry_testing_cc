
library(raster)
library(here)
library(viridis)
library(sf)

# Definition for north arrow function, from:
#    https://www.jstatsoft.org/article/view/v019c01/v19c01.pdf
northarrow <- function(loc,size,bearing=0,cols,cex=1,...) {
  # checking arguments
  if(missing(loc)) stop("loc is missing")
  if(missing(size)) stop("size is missing")
  # default colors are white and black
  if(missing(cols)) cols <- rep(c("white","black"),8)
  # calculating coordinates of polygons
  radii <- rep(size/c(1,4,2,4),4)
  x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
  y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
  # drawing polygons
  for (i in 1:15) {
    x1 <- c(x[i],x[i+1],loc[1])
    y1 <- c(y[i],y[i+1],loc[2])
    polygon(x1,y1,col=cols[i])
  }
  # drawing the last polygon
  polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16])
  # drawing letters
  b <- c("E","N","W","S")
  for (i in 0:3) text((size+par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
                      (size+par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],
                      cex=cex)
}


# Digital Surface Model
dsm <- raster(here::here("../output_imagery/dsm/dsm.tif"))
dsm_m <- projectRaster(dsm, crs="+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" )
plot(dsm_m, zlim=c(535,595), col=viridis(30), xaxt='n', yaxt='n', box=FALSE, axes=FALSE)
raster::scalebar(10, type='bar', below='Meters', divs=2, label=c(0,5,10))
#north.arrow(6565070,1873980,3,lab="NORTH",cex.lab=0.75,tcol='black',col='white')
northarrow(c(6565160,1873950),size=10)

# Orthophoto
ortho <- brick(here::here("../output_imagery/ortho/ortho.tif"))
#   Change no-value to white, not black:
ortho_new <- reclassify(ortho, cbind(0,255))
# resample to 
ortho_m <- projectRaster(ortho_new, crs="+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" )
plotRGB(ortho_m)
#raster::scalebar(10, type='bar', below='feet', divs=2)
#raster::scalebar(50, type='bar', below='feet', divs=2)
# Add Checkpoint Locations
checkpoints <- st_read(here::here("..","output_imagery","checkpoints.shp"))
plot(checkpoints[2], add=TRUE, col="magenta", fill="magenta", pch=19)
#north.arrow(6565070,1873980,3,lab="NORTH",cex.lab=0.75,tcol='white',col='white')
#northarrow(c(6565160,1873950),size=10)