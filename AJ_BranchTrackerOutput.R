# This script simply reads in BranchTracker files and plots them so you can watch the files as you're writing them

# Load required libraries
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(reshape2)
library(lubridate)
library(plyr)
library(zoo)

# Read in Stem Movement file
file.loc <- '/Users/alesia/Desktop/BranchTracker/'
list.zombie.files <- list.files(file.loc, pattern='branches')
list.zombie.files <- paste(file.loc, list.zombie.files, sep = '')

mergezom <- read.csv(list.zombie.files[1], header=T, strip.white=T)
mergezom <-  mergezom[,-c(2,3)]

for(i in 2:length(list.zombie.files)) {
  next.file <- read.csv(list.zombie.files[i], header=T, strip.white=T)
  next.file <- next.file[,-c(2,3)]
  mergezom <- merge(mergezom, next.file, by=intersect(names(mergezom), names(next.file)), all=T) }

mergezom$timestamp <- as.POSIXct(strptime(paste(mergezom$Year, mergezom$JulianDate, mergezom$Hour), format="%Y %j %H"))
mergezom <- mergezom[,5:length(mergezom)]
mergezom[mergezom==0] <- NA


meltzombies <- melt(mergezom, id.vars=c("timestamp"))
meltzombies <- meltzombies[!is.na(meltzombies$value),]
colnames(meltzombies)[2:3] <- c("StemID", "YPosition")
meltzombies <- meltzombies[grep("y", meltzombies$StemID),]

# Find % of total movement
percentiles <- ddply(meltzombies, c("StemID"), function(x)
  data.frame(ninetyfive = quantile(x$YPosition, probs=0.95, na.rm=TRUE),
             five = quantile(x$YPosition, probs=0.05, na.rm=TRUE)))
meltzombies <- merge(meltzombies, percentiles, by='StemID', all=T)
# percentage and inverse to make the coords make sense
meltzombies$PercentY <- 1 - ((meltzombies$YPosition- meltzombies$five) / (meltzombies$ninetyfive- meltzombies$five))

meltzombies$PercentY[meltzombies$PercentY > 1 | meltzombies$PercentY < 0] <- NA

# Subset
meltzombiessub <- meltzombies
meltzombiessub <- meltzombiessub[grep("z1_", meltzombies$StemID),]

stems.plot <- ggplot(meltzombiessub, aes(x=timestamp, y=YPosition, group=StemID, colour=StemID)) +
  geom_point() + geom_line() + theme_bw()

stems.plot.two <- ggplot(meltzombiessub, aes(x=timestamp, y=PercentY, group=StemID, colour=StemID)) +
  geom_point(alpha=0.5) + geom_line() + theme_bw() 

grid.arrange(stems.plot, stems.plot.two, ncol=1)


