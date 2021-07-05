rm(list=ls())

library(dplyr)

find_replace <- function(vec, dictionary) {
  new_vec <- rep(NA,times=length(vec))
  for (vid in 1:length(vec)) {
    v <- vec[vid]
    found <- FALSE
    for (entry in 1:nrow(dictionary)) {
      if (v == "") {
        new_vec[vid] <- ""
        found <- TRUE
      }
      if (v == dictionary[entry,1]) {
        new_vec[vid] <- dictionary[entry,2]
        found <- TRUE
      }
    }
    if (!found) {
      cat("Warning: `",v,"` not found in supplied dictionary, returned without changing\n",sep="")
      new_vec[vid] <- v
    }
  }
  return(new_vec)
}

Habitat_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat_2021_06_30_clipped.tif")
Habitat <- as.data.frame(Habitat_raster, xy = TRUE)
colnames(Habitat) <- c("Longitude","Latitude","Habitat")
lat_line <- 0.409859
Habitat <- filter(Habitat, !is.na(Habitat), Habitat!=0, Latitude>=lat_line)

# Replace integer with string
bush <- data.frame(inp=0:3,out=c(NA,"OB","LB","MB"))
Habitat$Habitat <- find_replace(Habitat$Habitat, bush)

# Add NDVI
lat_line <- 0.409859

NDVI_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/Recent/NDVI.tif")
NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
colnames(NDVI) <- c("Longitude","Latitude","NDVI")
NDVI <- filter(NDVI, !is.na(NDVI), Latitude>=lat_line)
NDVI$NDVI <- NDVI$NDVI
rownames(Habitat) <- paste(Habitat$Longitude,Habitat$Latitude,sep=":")
rownames(NDVI) <- paste(NDVI$Longitude,NDVI$Latitude,sep=":")
shared <- intersect(rownames(Habitat),rownames(NDVI))
Habitat[shared,"NDVI"] <- NDVI[shared,"NDVI"]
if (length(setdiff(rownames(Habitat),rownames(NDVI))) == 0) {
  cat("NDVI done\n")
} else {
  cat("Error: NDVI failure\n")
}


EVI_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/Recent/EVI.tif")
EVI <- as.data.frame(EVI_raster, xy = TRUE)
colnames(EVI) <- c("Longitude","Latitude","EVI")
EVI <- filter(EVI, !is.na(EVI), Latitude>=lat_line)
EVI$EVI <- EVI$EVI
rownames(Habitat) <- paste(Habitat$Longitude,Habitat$Latitude,sep=":")
rownames(EVI) <- paste(EVI$Longitude,EVI$Latitude,sep=":")
shared <- intersect(rownames(Habitat),rownames(EVI))
Habitat[shared,"EVI"] <- EVI[shared,"EVI"]
if (length(setdiff(rownames(Habitat),rownames(EVI))) == 0) {
  cat("EVI done\n")
} else {
  cat("Error: EVI failure\n")
}


cat("Writing...\n")
write.csv(Habitat,"/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat.csv",row.names=FALSE)


if (FALSE) {
  source("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/ProcessHabitat.R")
}


