rm(list=ls())

library(dplyr)
library(ggplot2)
library(raster)

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
# value_at_minimal_distance <- function(DF, value) {
#   min.distance <- Inf
#   min.value <- -Inf
#   for (row in 1:nrow(DF)) {
#     if (DF[row,"Distance"] < min.distance) {
#       min.distance <- DF[row,"Distance"]
#       min.value <- DF[row,value]
#     }
#   }
#   return(min.value)
# }



# if (FALSE) { # If Habitat/NDVI/EVI CSV needs to be updated

lat_line <- 0.409859

Habitat_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat_2021_06_30_clipped.tif")
Habitat <- as.data.frame(Habitat_raster, xy = TRUE)
colnames(Habitat) <- c("Longitude","Latitude","Habitat")
Habitat <- filter(Habitat, !is.na(Habitat), Habitat!=0, Latitude>=lat_line)

bush <- data.frame(inp=0:3,out=c(NA,"OB","LB","MB"))
Habitat$Habitat <- find_replace(Habitat$Habitat, bush)

NDVI_raster <- raster("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/Recent/EVI.tif")
NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
colnames(NDVI) <- c("Longitude","Latitude","NDVI")
NDVI <- filter(NDVI, !is.na(NDVI), NDVI!=0, Latitude>=lat_line)
NDVI$NDVI <- NDVI$NDVI * 0.0001

EVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS Data/Recent/NDVI.tif")
EVI <- as.data.frame(EVI_raster, xy = TRUE)
colnames(EVI) <- c("Longitude","Latitude","EVI")
EVI <- filter(EVI, !is.na(EVI), EVI!=0, Latitude>=lat_line)
EVI$EVI <- EVI$EVI * 0.0001
  
chd <- Habitat
chd$NDVI <- chd$EVI <- NA


chd <- chd %>% sample_n(1000)
for (pixelID in 1:nrow(chd)) {
  pixel <- chd[pixelID,]
  chd[pixelID,"NDVI"] <- (NDVI %>% mutate("Distance" = ((Longitude - chd$Longitude[pixelID])^2 + (Latitude - chd$Latitude[pixelID])^2)) %>% 
                            arrange(NDVI))[1,"NDVI"]
  chd[pixelID,"EVI"] <- (EVI %>% mutate("Distance" = ((Longitude - chd$Longitude[pixelID])^2 + (Latitude - chd$Latitude[pixelID])^2)) %>% 
                           arrange(EVI))[1,"EVI"]
}
chd$Habitat <- as.factor(chd$Habitat)

chd <- filter(chd, !is.na(EVI), !is.na(NDVI))

(groups.compare <- kruskal.test(NDVI ~ Habitat, data=chd)) #aov #kruskal.test
summary(groups.compare)
# TukeyHSD(hab.zeb)








# rownames(chd) <- paste0(chd$Longitude,":",chd$Latitude)
# rownames(NDVI) <- paste0(NDVI$Longitude,":",NDVI$Latitude)
# rownames(EVI) <- paste0(EVI$Longitude,":",EVI$Latitude)
# 
# shared.NDVI <- intersect(rownames(chd), rownames(NDVI))
# shared.EVI <- intersect(rownames(chd), rownames(EVI))
# 
# 
# chd.arr <- chd %>% arrange(Longitude) %>% arrange(Latitude)
# chd.arr$ID <- 1:nrow(chd.arr)
# ndvi.arr <- NDVI %>% arrange(Longitude) %>% arrange(Latitude)
# ndvi.arr$ID <- 1:nrow(ndvi.arr)
# evi.arr <- EVI %>% arrange(Longitude) %>% arrange(Latitude)
# evi.arr$ID <- 1:nrow(evi.arr)
# 
# chd.arr$label <- "Habitat"
# ndvi.arr$label <- "NDVI"
# evi.arr$label <- "EVI"
# chd.arr$NDVI <- chd.arr$EVI <- NULL
# colnames(chd.arr) <- colnames(ndvi.arr) <- colnames(evi.arr) <- c("Longitude","Latitude", "GeoTIFF","Row","Label")
# 
# arr <- rbind(chd.arr,ndvi.arr,evi.arr)
# 
# ggplot(arr, aes(x=, y=Longitude)) + geom_point(fill=arr$Label)



#





# for (pixelID in 1:10) {
#   print(pixelID)
#   pixel <- chd[pixelID,]
#   chd[pixelID,"NDVI"] <- NDVI %>% mutate("Distance" = ((Longitude - chd$Longitude[pixelID])^2 + (Latitude - chd$Latitude[pixelID])^2)) %>%
#                            value_at_minimal_distance("NDVI")
#   chd[pixelID,"EVI"] <- EVI %>% mutate("Distance" = ((Longitude - chd$Longitude[pixelID])^2 + (Latitude - chd$Latitude[pixelID])^2)) %>%
#                            value_at_minimal_distance("EVI")
# }


# write.csv(chd,"/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat_NDVI_EVI.csv",row.names=FALSE)








# } else {
#   chd <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat_NDVI_EVI.csv")
# }



OG <- filter(chd, Habitat==1)
LG <- filter(chd, Habitat==2)
MG <- filter(chd, Habitat==3)

find_replace(chd$Habitat, data.frame(
  inp=c(1,2,3),
  out=c("OG","LG","MG")
))




