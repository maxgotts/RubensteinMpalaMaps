rm(list=ls())

library(dplyr)

## Load Habitat
hb <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat.csv")

## Load VI
lat_line <- 0.409859

NDVI_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/Recent/EVI.tif")
NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
colnames(NDVI) <- c("Longitude","Latitude","NDVI")
NDVI <- filter(NDVI, !is.na(NDVI), Latitude>=lat_line)
NDVI$NDVI <- NDVI$NDVI * 0.0001

EVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/Recent/NDVI.tif")
EVI <- as.data.frame(EVI_raster, xy = TRUE)
colnames(EVI) <- c("Longitude","Latitude","EVI")
EVI <- filter(EVI, !is.na(EVI), Latitude>=lat_line)
EVI$EVI <- EVI$EVI * 0.0001

## Fit NDVI and habitat together
rownames(hb) <- paste(hb$Longitude,hb$Latitude,sep=":")
rownames(NDVI) <- paste(NDVI$Longitude,NDVI$Latitude,sep=":")
shared <- intersect(rownames(hb),rownames(NDVI))
hb[shared,"NDVI"] <- NDVI[shared,"NDVI"]
print(setdiff(rownames(hb),rownames(NDVI)))
print("NDVI done")

## Fit EVI and habitat together
rownames(hb) <- paste(hb$Longitude,hb$Latitude,sep=":")
rownames(EVI) <- paste(EVI$Longitude,EVI$Latitude,sep=":")
shared <- intersect(rownames(hb),rownames(EVI))
print(length(shared))
print("None shared (EVI)")

hb <- hb %>% arrange(Longitude) %>% arrange(Latitude)
evi <- EVI %>% arrange(Longitude) %>% arrange(Latitude)
evis <- split(evi,evi$Longitude)








