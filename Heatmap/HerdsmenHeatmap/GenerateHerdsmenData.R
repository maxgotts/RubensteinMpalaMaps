rm(list=ls())

library(dplyr)
library(lubridate)
library(zoo)


## Cut whitesheets to time range ## Thank you to http://math.furman.edu/~dcs/courses/math47/R/library/zoo/doc/zoo-quickref.pdf
lastDay <- function(x,t) 7 * floor(as.numeric(x - t)/7) + as.Date(t)
lastfri <- function(x) lastDay(x,1)
lastsat <- function(x) lastDay(x,2)
lastsun <- function(x) lastDay(x,3)
lastmon <- function(x) lastDay(x,4)
lasttue <- function(x) lastDay(x,5)
lastwed <- function(x) lastDay(x,6)
lastthur <- function(x) lastDay(x,7)
yesterday <- function() as.Date(as.numeric(today()) - 1)

lastlastwed <- function(x) lastDay(x,6+7)
lastlastfri <- function(x) lastDay(x,1+7)

df <- read.csv("/Users/maxgotts/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")

# if (weekdays(today()) %in% c("Monday")) {
#   from.date <- lastfri(today())
# } else if (weekdays(today()) %in% c("Thursday")) {
#   from.date <- lasttue(today())
# } else {
#   cat("Warning: you are running this program outside of its specified date\n")
#   from.date <- lastfri(today()) #lastwed(yesterday())
# }

from.date <- lastwed(yesterday())

# from.days <- time_length(interval(ymd("2021-01-01"),from.date), "day")
# df$NumberDays <- time_length(interval(ymd("2021-01-01"),mdy(df$Date)), "day")
df$IntervalDays <- time_length(interval(from.date, mdy(df$Date)), "day") # Most elegant solution

df$Population <- 10*df$Total.animals^(0.75)

df.recents <- filter(df, IntervalDays >= 0)
df.recents$IntervalDays <- NULL


for (species in unique(df$Species)) {
  df.recents.species <- filter(df.recents, Species==species)
  write.csv(df.recents.species,paste0("/Users/maxgotts/Desktop/MPALA/Maps/Heatmap/HerdsmenHeatmap/Whitesheets/HerdsmenWhitesheets_",species,".csv"), row.names=FALSE)
  if (nrow(df.recents.species) == 0) cat("Warning: `HerdsmenWhitesheets_",species,".csv` is empty\n",sep="")
}

write.csv(df.recents,"/Users/maxgotts/Desktop/MPALA/Maps/Heatmap/HerdsmenHeatmap/Whitesheets/HerdsmenWhitesheets_AllAnimals.csv", row.names=FALSE)

cattle.abbr <- c("Cattle","CKC","CC","MC")
recents.cattle <- filter(df.recents, Species %in% cattle.abbr)
write.csv(recents.cattle,"/Users/maxgotts/Desktop/MPALA/Maps/Heatmap/HerdsmenHeatmap/Whitesheets/HerdsmenWhitesheets_AllCattle.csv", row.names=FALSE)

camel.abbr <- c("Camel","ZC","Comm_Camel")
recents.camels <- filter(df.recents, Species %in% camel.abbr)
write.csv(recents.camels,"/Users/maxgotts/Desktop/MPALA/Maps/Heatmap/HerdsmenHeatmap/Whitesheets/HerdsmenWhitesheets_AllCamels.csv", row.names=FALSE)

zebra.abbr <- c("GZ","PZ")
recents.zebras <- filter(df.recents, Species %in% zebra.abbr)
write.csv(recents.zebras,"/Users/maxgotts/Desktop/MPALA/Maps/Heatmap/HerdsmenHeatmap/Whitesheets/HerdsmenWhitesheets_AllZebras.csv", row.names=FALSE)





## Move most recent NDVI

# file.copy("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/Recent/NDVI.tif", "/Users/maxgotts/Desktop/MPALA/Maps/Heatmap/HerdsmenHeatmap/Rasters/NDVI.tif", overwrite=TRUE)
# file.copy("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/Recent/EVI.tif", "/Users/maxgotts/Desktop/MPALA/Maps/Heatmap/HerdsmenHeatmap/Rasters/EVI.tif", overwrite=TRUE)





if (FALSE) {
  source('/Users/maxgotts/Desktop/MPALA/Maps/Heatmap/HerdsmenHeatmap/GenerateHerdsmenData.R')
}


