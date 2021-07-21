rm(list=ls())

library(raster)
source('~/Desktop/MPALA/mpala.R')

Habitat_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/mpala-habitat-mosaic.tiff")
Habitat <- as.data.frame(Habitat_raster, xy = TRUE)
colnames(Habitat) <- c("Longitude","Latitude","Habitat")
lat_line <- 0.409859
Habitat <- filter(Habitat, !is.na(Habitat), Habitat!=0, Latitude>=lat_line)

# Replace integer with string
bush <- data.frame(inp=0:3,out=c(NA,bushland))
Habitat$Habitat <- find_replace(Habitat$Habitat, bush)
rownames(Habitat) <- paste(Habitat$Longitude,Habitat$Latitude,sep=":")

cat("Habitat done...\n")



# VI start dates
dates <- vi_dates
subdir <- "MODIS_frames"
# subdir <- "MODIS_frames_Loops_1_2"


empty_ndvi_raster <- raster(paste0("/Users/maxgotts/Desktop/MPALA/Maps/VI Data/",subdir,"/mpala-NDVI-",dates[1,1],".tif"))
empty_ndvi <- as.data.frame(empty_ndvi_raster, xy = TRUE)
colnames(empty_ndvi) <- c("Longitude","Latitude","NDVI")
empty_ndvi <- filter(empty_ndvi, Latitude>=lat_line)
rownames(empty_ndvi) <- paste(empty_ndvi$Longitude,empty_ndvi$Latitude,sep=":")
empty_ndvi$NDVI <- NULL
vi <- empty_ndvi


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
  NDVI_raster <- raster(paste0("/Users/maxgotts/Desktop/MPALA/Maps/VI Data/",subdir,"/mpala-NDVI-",start,".tif"))
  NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
  colnames(NDVI) <- c("Longitude","Latitude","NDVI")
  NDVI <- filter(NDVI, Latitude>=lat_line) #!is.na(NDVI)
  rownames(NDVI) <- paste(NDVI$Longitude,NDVI$Latitude,sep=":")
  shared <- intersect(rownames(vi),rownames(NDVI))
  vi[shared,paste0("NDVI_",date_range_mod)] <- nan_to_na(NDVI[shared,"NDVI"])
  if (length(setdiff(rownames(vi),rownames(NDVI))) == 0) {
    cat("NDVI done",date_range,"\n")
  } else {
    cat("Error: NDVI failure",date_range,
        "intersect:",length(shared),
        "desired:",nrow(vi),
        "difference:",length(setdiff(rownames(vi),rownames(NDVI))),"\n")
  }
  
  EVI_raster <- raster(paste0("/Users/maxgotts/Desktop/MPALA/Maps/VI Data/",subdir,"/mpala-EVI-",start,".tif"))
  EVI <- as.data.frame(EVI_raster, xy = TRUE)
  colnames(EVI) <- c("Longitude","Latitude","EVI")
  EVI <- filter(EVI, Latitude>=lat_line) #!is.na(EVI)
  rownames(EVI) <- paste(EVI$Longitude,EVI$Latitude,sep=":")
  shared <- intersect(rownames(vi),rownames(EVI))
  vi[shared,paste0("EVI_",date_range_mod)] <- nan_to_na(EVI[shared,"EVI"])
  if (length(setdiff(rownames(vi),rownames(EVI))) == 0) {
    cat("EVI done",date_range,"\n")
  } else {
    cat("Error: EVI failure",date_range,
        "intersect:",length(shared),
        "desired:",nrow(vi),
        "difference:",length(setdiff(rownames(vi),rownames(EVI))),"\n")
  }
}





# 
# Habitat[,colnames(vi)[3:ncol(vi)]] <- NA
# rounding_resolution <- 10
# for (habitat_pixel in 1:200) {#nrow(Habitat)) {
#   vi.arr <- vi %>%
#     filter(abs(round(Longitude,rounding_resolution)-round(Habitat[habitat_pixel,"Longitude"],rounding_resolution))<=1/10^rounding_resolution,abs(round(Latitude,rounding_resolution)-Habitat[habitat_pixel,"Latitude"])<=1/10^rounding_resolution) %>%
#     mutate("Distance" = sqrt(((Longitude - Habitat[habitat_pixel,"Longitude"])^2 + (Latitude - Habitat[habitat_pixel,"Latitude"])^2))) %>% 
#     arrange(Distance) %>%
#     slice_head()
#   Habitat[habitat_pixel,colnames(vi)[3:ncol(vi)]] <- vi.arr[1,colnames(vi)[3:ncol(vi)]]/10000
# }
# 
# cat("Habitat, VI merge done...\n")




vi[,colnames(vi)[3:ncol(vi)]] <- vi[,colnames(vi)[3:ncol(vi)]]/10000
Habitat <- filter(Habitat, !is.na(Habitat))


cat("Writing...\n")
write.csv(Habitat,"/Users/maxgotts/Desktop/MPALA/Maps/HabitatTIFF.csv",row.names=FALSE)
write.csv(vi,"/Users/maxgotts/Desktop/MPALA/Maps/VITIFF.csv",row.names=FALSE)
# write.csv(vi,"/Users/maxgotts/Desktop/MPALA/Maps/Loops_1_2_VITIFF.csv",row.names=FALSE)


if (FALSE) {
  source("/Users/maxgotts/Desktop/MPALA/Maps/ProcessTIFFs2.R")
}
