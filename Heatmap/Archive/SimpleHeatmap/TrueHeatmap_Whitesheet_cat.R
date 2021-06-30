library(dplyr)

whitesheets <- read.csv("~/Desktop/MPALA/Whitesheets/CompleteWhitesheetsCSV/Gotts_CompleteWhitesheets_6-11.csv")
df <- whitesheets[,c("Date","Time","Latitude","Longitude","Species","Multispecies","Total.zebras")]
df <- filter(df,!is.na(Time), Species != "Cattle")


ROWS <- nrow(df)

for (dazzle in 1:ROWS) {
  print(dazzle)
  if (dazzle <= nrow(df)) {
    if (df[dazzle,"Multispecies"] == 1) {
      df[dazzle,"Total.zebras"] <-df[dazzle,"Total.zebras"] + df[dazzle+1,"Total.zebras"]
      df[dazzle,"Species"] <- "GZPZ"
      df[dazzle,"Multispecies"] <- 2
      df[dazzle+1,] <- NA
      df <- filter(df,!is.na(Time))
    }
  }
}

df$DazzleID <- 1:nrow(df)

for (dazzle in 1:ROWS) {
  temp <- df[dazzle,]
  df <- rbind(df, temp[rep(1, temp["Total.zebras"]), ])
}

grevy <- subset(df, Species=="GZ")
plains <- subset(df, Species=="PZ")
mixed <- subset(df, Species=="GZPZ")

write.csv(grevy,"~/Desktop/MPALA/Whitesheets/CompleteWhitesheetsCSV/TrueHeatmap_Grevy_6-11.csv")
write.csv(plains,"~/Desktop/MPALA/Whitesheets/CompleteWhitesheetsCSV/TrueHeatmap_Plains_6-11.csv")
write.csv(mixed,"~/Desktop/MPALA/Whitesheets/CompleteWhitesheetsCSV/TrueHeatmap_Mixed_6-11.csv")
write.csv(df,"~/Desktop/MPALA/Whitesheets/CompleteWhitesheetsCSV/TrueHeatmap_Simple_6-11.csv")


