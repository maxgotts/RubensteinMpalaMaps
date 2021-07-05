rm(list=ls())

library(dplyr)
library(ggplot2)
source('~/Desktop/MPALA/mpala.R')


hb <- read.csv("/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat.csv")


# n <- hb$NDVI-mean(hb$NDVI)
# np <- n[n>3*sd(n)]
# nn <- abs(n[n< -3*sd(n)])
# np.s <- np[sample(length(np),min(length(np),length(nn)))]
# nn.s <- nn[sample(length(nn),min(length(np),length(nn)))]
# 
# (s <- shapiro.test((np.s-nn.s)[sample(length(np.s),min(5000,length(np.s)))])) # Non-normal
# (t <- t.test(np.s, nn.s, paired = TRUE, alternative = "two.sided"))
# (w <- wilcox.test(np.s, nn.s, paired = TRUE, alternative = "two.sided"))


# NDVIsigma <- filter(hb, NDVI<=mean(hb$NDVI)-3*sd(hb$NDVI) || NDVI>=mean(hb$NDVI)+3*sd(hb$NDVI))
# NDVIsigmaOB <- filter(NDVIsigma, Habitat=="OB")
# NDVIsigmaMB <- filter(NDVIsigma, Habitat=="MB")
# Delta <- mean(NDVIsigmaMB$NDVI)-mean(NDVIsigmaOB$NDVI)

NDVI_OB <- filter(hb, Habitat=="OB")
NDVI_MB <- filter(hb, Habitat=="MB")
# Delta <- mean(NDVI_MB$NDVI)-mean(NDVI_OB$NDVI)
(T <- min(NDVI_MB$NDVI)/0.9)

hb$TreeCover <- find_replace(hb$Habitat, data.frame(x=c("OB","LB","MB"),y=c(0.1,0.5,0.9)))
hb$GrassNDVI <- (1/(1-hb$TreeCover))*(hb$NDVI-hb$TreeCover*T)


write.csv(hb,"/Users/maxgotts/Desktop/MPALA/Maps/Habitat/Habitat.csv",row.names=FALSE)
