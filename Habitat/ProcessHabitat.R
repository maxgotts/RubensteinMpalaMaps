rm(list=ls())

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

bush <- data.frame(inp=0:3,out=c(NA,"OB","LB","MB"))
Habitat$Habitat <- find_replace(Habitat$Habitat, bush)

write.csv(Habitat,"/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat.csv",row.names=FALSE)


if (FALSE) {
  source("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/ProcessHabitat.R")
}


