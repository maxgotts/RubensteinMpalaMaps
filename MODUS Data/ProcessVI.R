rm(list=ls())
library(raster)
library(dplyr)

source('~/Desktop/MPALA/mpala.R')

NDVI_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/Recent/NDVI.tif")
NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
colnames(NDVI) <- c("Longitude","Latitude","NDVI")
NDVI <- filter(NDVI, !is.na(NDVI), Latitude>=lat_line)
rownames(NDVI) <- paste(NDVI$Longitude,NDVI$Latitude,sep=":")

EVI_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/Recent/EVI.tif")
EVI <- as.data.frame(EVI_raster, xy = TRUE)
colnames(EVI) <- c("Longitude","Latitude","EVI")
EVI <- filter(EVI, !is.na(EVI), Latitude>=lat_line)
rownames(EVI) <- paste(EVI$Longitude,EVI$Latitude,sep=":")

cat(sum(rownames(NDVI)!=rownames(EVI))," differing rows",sep="")

shared <- intersect(rownames(NDVI),rownames(EVI))

if (sum(rownames(NDVI)!=rownames(EVI)) == 0) {
  NDVI$EVI <- EVI$EVI
  cat("Columns combined\n")
} else {
  cat("Error: column failure\n")
}


cat("Writing...\n")
write.csv(NDVI,"/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/VegIndex.csv",row.names=FALSE)


if (FALSE) {
  source("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/ProcessVI.csv")
}


