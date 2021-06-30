rm(list=ls())

library(lubridate)
library(dplyr)


## Useful functions
military_to_24 <- function(military) { # military is a number
  military_time <- paste0(military)
  split_military_time <- strsplit(military_time,"")[[1]]
  if (length(split_military_time) == 3) split_military_time <- c("0", split_military_time)
  split_hour24_time <- c(split_military_time[1], split_military_time[2], ":", split_military_time[3], split_military_time[4])
  if (split_hour24_time[1] == 0) split_hour24_time <- split_hour24_time[2:5]
  hour24_time <- paste(split_hour24_time, collapse = "")
  return(hour24_time)
}



whitesheets <- read.csv("~/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")


# Create a simplified sheet for a basic heat map
ws <- whitesheets[,c("Date", "Time",  "Latitude", "Longitude", "Species", "Multispecies", "Activity", "Grass.height", "Grass.color", "Grass.spp.1", "Grass.spp.2", "Grass.spp.3", "Grass.spp.4", "Bush.type", "Total.animals")]
df <- filter(ws,!is.na(Time), !is.na(Total.animals))

df$Date <- mdy(df$Date)
df$Population <- (4/75)*(df$Total.animals)^(0.75)


df_simple <- df[,c("Latitude","Longitude","Total.animals","Date","Species","Population")]



camel.abbr <- c("Camel","ZC","Comm_Camel")
cattle.abbr <- c("Cattle","CKC","CC","MC")
zebra.abbr <- c("GZ","PZ")

write.csv(filter(df_simple,Species=="GZ"),"~/Desktop/MPALA/Maps/Heatmap/MODUSHeatmap/MODUSHeatmap_Whitesheet_GZ.csv",row.names = FALSE)
write.csv(filter(df_simple,Species=="PZ"),"~/Desktop/MPALA/Maps/Heatmap/MODUSHeatmap/MODUSHeatmap_Whitesheet_PZ.csv",row.names = FALSE)
write.csv(filter(df_simple,Species%in%cattle.abbr),"~/Desktop/MPALA/Maps/Heatmap/MODUSHeatmap/MODUSHeatmap_Whitesheet_Cattle.csv",row.names = FALSE)
write.csv(filter(df_simple,Species%in%zebra.abbr),"~/Desktop/MPALA/Maps/Heatmap/MODUSHeatmap/MODUSHeatmap_Whitesheet_Zebras.csv",row.names = FALSE)
write.csv(filter(df_simple,Species%in%camel.abbr),"~/Desktop/MPALA/Maps/Heatmap/MODUSHeatmap/MODUSHeatmap_Whitesheet_Camels.csv",row.names = FALSE)





if (FALSE) {
  source('~/Desktop/MPALA/Maps/Heatmap/MODUSHeatmap/MODUSHeatmap_Whitesheet.R')
}


