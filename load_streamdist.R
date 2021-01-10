
library(raster)
library(rgdal)
library(sp)
library(tidyverse)

streamdist <- read_csv("streamdist_pendleton_coarse.csv")
coordinates(streamdist) =~ x+y
crs(streamdist) <- CRS("+init=epsg:3500")

dem <- raster("D:/SERDP/Pendleton/LiDAR/DEM/dem_mosaic_10m.tif")

vert_mat <- t(matrix(streamdist$dist_vert, nrow=ncol(dem), ncol=nrow(dem)))
horz_mat <- t(matrix(streamdist$dist_horz, nrow=ncol(dem), ncol=nrow(dem)))
order_mat <- t(matrix(streamdist$order, nrow=ncol(dem), ncol=nrow(dem)))

stream_raster <- stack(raster(vert_mat), raster(horz_mat), raster(order_mat))
crs(stream_raster) <- crs(dem)
extent(stream_raster) <- extent(dem)
origin(stream_raster) <- origin(dem)

writeRaster(stream_raster, "streamdist_data_coarse.tif", overwrite=TRUE)