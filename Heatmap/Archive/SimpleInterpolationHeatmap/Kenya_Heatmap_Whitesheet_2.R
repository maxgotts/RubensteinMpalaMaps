library(fitnesslandscapes) # git@github.com:maxgotts/fitnesslandscapes.git
library(lubridate)
library(dplyr)
library(scatterplot3d)


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




####### CREATE CSV #######



whitesheets <- read.csv("~/Desktop/MPALA/Whitesheets/Whitesheet\ Processing/Gotts_RawWhitesheets.csv")


# Create a simplified sheet for a basic heat map
ws <- whitesheets[,c("Date", "Time",  "Latitude", "Longitude", "Species", "Multispecies", "Activity", "Grass.height", "Grass.color", "Grass.spp.1", "Grass.spp.2", "Grass.spp.3", "Grass.spp.4", "Bush.type", "Total.zebras")]
df <- filter(ws,!is.na(Time), Species != "Cattle")


df <- filter(df, Longitude >= 36.5, Longitude <= 37.5, Latitude >= 0.35, Latitude <= 0.55)


Fitness <- function(Delta_t, Population, Species) { # Species not yet incorporated
  T <- 3.9
  p <- 0.87
  alpha <- 58.7
  beta <- 0.9
  gamma <- 2.2
  return(Population*T*(p*exp(-Delta_t/alpha)*beta+(1-p)*exp(-Delta_t/gamma)))
}


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


df$Fitness <- Fitness(df$Delta_t, df$Total.zebras, df$Species)
df$Fitness <- df$Fitness/max(df$Fitness)


TPS_landscape(DF=df, VarName1="Longitude", VarName2="Latitude", output="plotly", Theta=30, Phi=30, FitnessMetric="Fitness",Lambda=0.0001) #, DispVar1=VarName1, DispVar2=VarName2, DispFitness=FitnessMetric
TPS_landscape(DF=df, VarName1="Longitude", VarName2="Latitude", output="wireframe", Theta=-20, Phi=30, FitnessMetric="Fitness",Lambda=0.0001) #, DispVar1=VarName1, DispVar2=VarName2, DispFitness=FitnessMetric
TPS_landscape(DF=df, VarName1="Longitude", VarName2="Latitude", output="contour", Theta=30, Phi=30, FitnessMetric="Fitness",Lambda=0.0001) #, DispVar1=VarName1, DispVar2=VarName2, DispFitness=FitnessMetric
scatterplot3d(x=df$Longitude, y=df$Latitude, z=df$Fitness, color=df$Fitness, tick.marks=TRUE, grid = TRUE, type="h") 


df_simple <- df[,c("Latitude","Longitude","Fitness","Delta_t","Total.zebras")]
df_simple$Population <- df_simple$Total.zebras/3
write.csv(df_simple,paste0("~/Desktop/MPALA/Maps/Heatmap/SimpleInterpolationHeatmap/SimpleWhitesheet_",today(),".csv"), row.names=FALSE)



View(df)





##### RASTERIZE ##### 

library(raster)

# TPS prior to raster
# https://www.rdocumentation.org/packages/raster/versions/2.5-8/topics/interpolate

# example data
temp <- read.csv(text="temp,lat,lon,
  24.1,40.503,-69.248,
  21.7,38.461,-74.703,
  19.2,40.694,-72.048,
  18.5,41.443,-70.187,
  22.5,40.251,-73.164,
  14.3,44.287,-67.307,
  13.9,44.106,-68.109,
  20,40.369,-73.703", header=TRUE)

# if these are points on a regular raster, you can do
# r <- rasterFromXYZ(temp[, c('lon', 'lat', 'temp')])

# in your case (no need to compute rows/colums):
x <- raster(xmn=-125, xmx=-65, ymn=25.5, ymx=50.5, res=0.75, crs="+proj=longlat +datum=WGS84")

us_fire <- rasterize(temp[, c('lon', 'lat')], x, temp[, 'temp'], fun=mean)
plot(us_fire)




