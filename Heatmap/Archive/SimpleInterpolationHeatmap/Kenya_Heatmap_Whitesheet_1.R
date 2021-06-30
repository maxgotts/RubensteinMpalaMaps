## Useful functions
military_to_24 <- function(military) { # military is a number
  military_time <- paste0(military)
  split_military_time <- str_split(military_time,"")[[1]]
  if (length(split_military_time) == 3) split_military_time <- c("0", split_military_time)
  split_hour24_time <- c(split_military_time[1], split_military_time[2], ":", split_military_time[3], split_military_time[4])
  if (split_hour24_time[1] == 0) split_hour24_time <- split_hour24_time[2:5]
  hour24_time <- paste(split_hour24_time, collapse = "")
  return(hour24_time)
}




####### CREATE CSV #######

library(lubridate) # For date/time stuff



whitesheets <- read.csv("~/Desktop/MPALA/Whitesheets/CompleteWhitesheetsCSV/Gotts_CompleteWhitesheets_6-11.csv")
# View(whitesheets)




# Create a simplified sheet for a basic heat map
simple <- whitesheets[,c("Date", "Time",  "Latitude", "Longitude", "Species", "Multispecies", "Activity", "Grass.height", "Grass.color", "Grass.spp.1", "Grass.spp.2", "Grass.spp.3", "Grass.spp.4", "Bush.type", "Total.zebras")]
simple <- filter(simple,!is.na(Time), Species != "Cattle")




# Create the species-separate numbers-weighted time-decay for a singular heat map
LatLength <- 0.025 # Side length (deg) of square for a single zebra-instance, 0.01=1.11km
LonLength <- 0.025 # Make sure the circles you use will overlap, because we are doing squares here,
      # and radial stuff for the map

Contribution <- function(Delta_t, Population, Species) Population*exp(-Delta_t/20)*Delta_t # Species not yet incorporated

Fitness <- function(lat,lon,t,species) { # lat,lon in deg format, t in date-time (mdy_hm) format
  fitness <- 0
  
  for (dazzle in 1:nrow(simple)) {
    
    # Space
    lat_dazzle <- simple[dazzle,"Latitude"]
    lon_dazzle <- simple[dazzle,"Longitude"]
    lat_okay <- (lat_dazzle <= lat+LatLength/2) && (lat_dazzle >= lat-LatLength/2)
    lon_okay <- (lon_dazzle <= lon+LonLength/2) && (lon_dazzle >= lon-LatLength/2)
    
    # Species
    species_dazzle <- simple[dazzle,"Species"]
    species_okay <- species_dazzle == species
    
    # Time
    formatted_t_dazzle <- paste(simple[dazzle,"Date"],military_to_24(simple[dazzle,"Time"]))
    t_dazzle <- mdy_hm(formatted_t_dazzle) # Date-time at the current dazzle
    Delta_t <- time_length(interval(t_dazzle,mdy_hm(t)),"hour") # Change in time since been there, in hours
    t_okay <- Delta_t >= 0
    
    
    if (lat_okay && lon_okay && species_okay && t_okay) {
      fitness <- fitness + Contribution(Delta_t,simple[dazzle,"Total.zebras"],species_dazzle)
      # cat(lat_dazzle,lon_dazzle,formatted_t_dazzle,Delta_t,species_dazzle,"\n")
    }
  }
  return(fitness)
}


# Fitness(0.47077,36.85997,"6/3/2021 13:45","PZ")


minLat <- 0.39+LatLength/2
maxLat <- 0.51-+LatLength/2
minLon <- 36+LonLength/2
maxLon <- 37-LonLength/2

lat_seq <- seq(minLat,maxLat,LatLength)
lon_seq <- seq(minLon,maxLon,LonLength)

FitnessGrid <- function(t,species) {
  fitness_df <- expand.grid(lat=lat_seq,lon=lon_seq)
  print(nrow(fitness_df))
  fitness_df$Fitness <- NA
  for (row in 1:nrow(fitness_df)) {
    print(row)
    fitness_df[row,"fitness"] <- Fitness(fitness_df[row,"lat"],fitness_df[row,"lon"],t,species)
  }
  return(fitness_df)
}

fg <- FitnessGrid("6/3/2021 13:45","PZ")
write.csv(fg,"~/Desktop/MPALA/Whitesheets/CompleteWhitesheetsCSV/SimplifiedWhitesheet_6-11.csv")





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




