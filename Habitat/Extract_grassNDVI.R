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

# NDVI_OB <- filter(hb, Habitat=="OB")
# Delta <- mean(NDVI_MB$NDVI)-mean(NDVI_OB$NDVI)

NDVI_MB <- filter(hb, Habitat=="MB")[,c(1:4,6)]
NDVI_MB.1 <- NDVI_MB
NDVI_MB.1[,5] <- NULL
colnames(NDVI_MB.1)[4] <- "NDVI"
NDVI_MB.2 <- NDVI_MB
NDVI_MB.2[,4] <- NULL
colnames(NDVI_MB.2)[4] <- "NDVI"
NDVI_MB <- rbind(NDVI_MB.1,NDVI_MB.2)

(T.1 <- mean(filter(NDVI_MB.1, NDVI<=quantile(NDVI_MB.1$NDVI, 0.1, na.rm=T)[[1]])$NDVI)/0.9)
(T.2 <- mean(filter(NDVI_MB.2, NDVI<=quantile(NDVI_MB.2$NDVI, 0.1, na.rm=T)[[1]])$NDVI)/0.9)
Tx <- mean(c(T.1,T.2))


TreeCover <- find_replace(hb$Habitat, data.frame(x=bushland,y=c(0.1,0.5,0.9)))
even <- function(vec) vec[vec%%2==0]
for (NDVI_colname in colnames(hb)[even(4:ncol(hb))]) {
  hb[,paste0("Grass",NDVI_colname)] <- (1/(1-TreeCover))*(hb[,NDVI_colname]-TreeCover*Tx)
}


write.csv(hb,"/Users/maxgotts/Desktop/MPALA/Maps/TIFFs.csv",row.names=FALSE)
