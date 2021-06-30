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



whitesheets <- read.csv("~/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/Gotts_RawWhitesheets.csv")


# Create a simplified sheet for a basic heat map
ws <- whitesheets[,c("Date", "Time",  "Latitude", "Longitude", "Species", "Multispecies", "Activity", "Grass.height", "Grass.color", "Grass.spp.1", "Grass.spp.2", "Grass.spp.3", "Grass.spp.4", "Bush.type", "Total.zebras")]
df <- filter(ws,!is.na(Time), Species != "Cattle")


df$Date <- mdy(df$Date)
# df$Date <- paste0(year(df$Date.saved),"-",month(df$Date.saved),"-",day(df$Date.saved))
# df$Date.saved <- NULL


df_simple <- df[,c("Latitude","Longitude","Total.zebras","Date")]
df_simple$Population <- df_simple$Total.zebras/5
write.csv(df_simple,"~/Desktop/MPALA/Maps/Heatmap/WeeklyMotionHeatmap/WeeklyMotionWhitesheet.csv",row.names = FALSE)



################################################################################

FillDelta_t <- function(df,t) {
  # if (typeof(t=="character")) {
  #   t <- mdy_hm(t)
  # }
  Delta_t <- rep(NA,nrow(df))
  for (dazzle in 1:nrow(df)) {
    formatted_t_dazzle <- paste(df[dazzle,"Date"],military_to_24(df[dazzle,"Time"]))
    t_dazzle <- mdy_hm(formatted_t_dazzle) # Date-time at the current dazzle
    Delta_t_value <- time_length(interval(t_dazzle,t),"hour") # Change in time since been there, in hours
    Delta_t[dazzle] <- Delta_t_value
  }
  return(Delta_t)
}

df$Delta_t <- FillDelta_t(df,now())



num.days <- 4#strtoi(readline("How many days/frame > "))
start.beginning <- FALSE#strtoi(readline("Start from beginning (1) or end (2) > ")) == 1

num.hours <- num.days*24
min.dt <- min(df_simple$Delta_t)
max.dt <- max(df_simple$Delta_t)
num.sheets <- floor((max.dt-min.dt)/num.hours)
lower.breaks <- NA
if (start.beginning) {
  lower.breaks <- sort(max.dt+0.1 - num.hours*(0:num.sheets))
} else {
  lower.breaks <- sort(min.dt-0.1 + num.hours*(0:num.sheets))
}
num.sheets <- num.sheets+1 # to include the 0th sheet


df_temp = list()
df_abbr <- df_simple
for (break.id in 1:length(lower.breaks)) {
  break.value <- lower.breaks[break.id]+1
  df_temp[[break.id]] <- subset(df_abbr,Delta_t<=break.value)
  df_abbr <- subset(df_abbr,Delta_t>break.value)
}


for (id in 1:length(lower.breaks)) {
  write.csv(df_temp[[id]],paste0("~/Desktop/MPALA/Maps/Heatmap/WeeklyMotionHeatmap/WeeklyMotionWhitesheet_",id,"_",today(),".csv"), row.names=FALSE)
}





if (FALSE) {
  source('~/Desktop/MPALA/Maps/Heatmap/WeeklyMotionHeatmap/Weekly_motionGIF_Whitesheet.R')
}


