rm(list=ls())

library(raster)
source('~/Desktop/MPALA/mpala.R')

Habitat_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat_2021_06_30_clipped.tif")
Habitat <- as.data.frame(Habitat_raster, xy = TRUE)
colnames(Habitat) <- c("Longitude","Latitude","Habitat")
lat_line <- 0.409859
Habitat <- filter(Habitat, !is.na(Habitat), Habitat!=0, Latitude>=lat_line)

# Replace integer with string
bush <- data.frame(inp=0:3,out=c(NA,bushland))
Habitat$Habitat <- find_replace(Habitat$Habitat, bush)



# VI start dates
dates <- vi_dates

rownames(Habitat) <- paste(Habitat$Longitude,Habitat$Latitude,sep=":")
nan_to_na <- function(vec) {
  vec[is.nan(vec)]<-NA
  return(vec)
}
for (date_id in 1:nrow(dates)) {
  start <- dates[date_id,"start"]
  end <- dates[date_id,"end"]
  date_range <- paste0(start,"-",end)
  date_range_mod <- gsub("-","_",date_range)
  
  # Add NDVI
  NDVI_raster <- raster(paste0("/Users/maxgotts/Desktop/MPALA/VI Data/mpala-NDVI-",date_range,".tif"))
  NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
  colnames(NDVI) <- c("Longitude","Latitude","NDVI")
  NDVI <- filter(NDVI, Latitude>=lat_line) #!is.na(NDVI)
  rownames(NDVI) <- paste(NDVI$Longitude,NDVI$Latitude,sep=":")
  shared <- intersect(rownames(Habitat),rownames(NDVI))
  Habitat[shared,paste0("NDVI_",date_range_mod)] <- nan_to_na(NDVI[shared,"NDVI"])
  if (length(setdiff(rownames(Habitat),rownames(NDVI))) == 0) {
    cat("NDVI done",date_range,"\n")
  } else {
    cat("Error: NDVI failure",date_range,
        "intersect:",length(shared),
        "desired:",nrow(Habitat),
        "difference:",length(setdiff(rownames(Habitat),rownames(NDVI))),"\n")
  }
  
  EVI_raster <- raster(paste0("/Users/maxgotts/Desktop/MPALA/VI Data/mpala-EVI-",start,"-",end,".tif"))
  EVI <- as.data.frame(EVI_raster, xy = TRUE)
  colnames(EVI) <- c("Longitude","Latitude","EVI")
  EVI <- filter(EVI, Latitude>=lat_line) #!is.na(EVI)
  rownames(EVI) <- paste(EVI$Longitude,EVI$Latitude,sep=":")
  shared <- intersect(rownames(Habitat),rownames(EVI))
  Habitat[shared,paste0("EVI_",date_range_mod)] <- nan_to_na(EVI[shared,"EVI"])
  if (length(setdiff(rownames(Habitat),rownames(EVI))) == 0) {
    cat("EVI done",date_range,"\n")
  } else {
    cat("Error: EVI failure",date_range,
        "intersect:",length(shared),
        "desired:",nrow(Habitat),
        "difference:",length(setdiff(rownames(Habitat),rownames(EVI))),"\n")
  }
}



cat("Writing...\n")
write.csv(Habitat,"/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat.csv",row.names=FALSE)


if (FALSE) {
  source("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/ProcessHabitat.R")
}
