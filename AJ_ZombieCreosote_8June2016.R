# This script combines data files for AJ's zombie creosote project
# Needed: Flux_all files from all years plus latest wireless download from both dataloggers
# CR7 DATA files plus lastest wireless download
# ICT files
# Excel datasheet of manually tracked branch movements

### Before beginning, run AJ_FluxProcess code and shrubCR7_dataprocess code. These will use most updated wireless files, convert SWC and use calibration factors to correct psychrometer values

# Load required libraries
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(reshape2)
library(lubridate)
library(plyr)
library(zoo)

# Make MasterMerge df of all date-times
MasterMerge <- data.frame(seq.POSIXt(from = ISOdatetime(2010,1,1,0,0,0), to = Sys.time(), by = "30 min"))
colnames(MasterMerge) <- c("timestamp")

# Read in processed Ameriflux, flux_all, and wireless data files
file.loc <- '/Users/alesia/Documents/data_AJ_output/'
list.flux.files <- list.files(file.loc, pattern='fluxall')
list.flux.files <- paste(file.loc, list.flux.files, sep = '')

mergeflux <- read.csv(list.flux.files[1], header=T, strip.white=T, sep=",")
mergeflux$timestamp <- as.POSIXct(strptime(mergeflux$timestamp, format="%Y-%m-%d %H:%M:%S"))


# Read in CR7 and ICT data
file.loc <- '/Users/alesia/Documents/data_AJ_output/'
list.CRsoil.files <- list.files(file.loc, pattern='soil_psy')
list.CRstem.files <- list.files(file.loc, pattern='allstem_psy')
list.CRsoil.files <- paste(file.loc, list.CRsoil.files, sep = '')
list.CRstem.files <- paste(file.loc, list.CRstem.files, sep = '')

CRsoil <- read.csv(list.CRsoil.files[1], header=T, strip.white=T)
CRstem <- read.csv(list.CRstem.files[1], header=T, strip.white=T)

CRsoil <- subset(CRsoil, select = c(TimeStamp, SensorTemp, WaterPot, Pit, Depth))
CRstem <- subset(CRstem, select = c(TimeStamp, SensorName, WaterPot))
colnames(CRsoil) <- c("timestamp", "SensorTemp", "SoilWaterPot", "Pit", "Depth")
colnames(CRstem) <- c("timestamp", "StemSensor", "StemWaterPot")
CRsoil$timestamp <- as.POSIXct(strptime(CRsoil$timestamp, format="%Y-%m-%d %H:%M:%S"))
CRstem$timestamp <- as.POSIXct(strptime(CRstem$timestamp, format="%Y-%m-%d %H:%M:%S"))
# Round CR7 timestamps to nearest 30 minutes (30min*60sec)
CRsoil$timestamp <- as.POSIXct(round(as.double(CRsoil$timestamp)/(30*60))*(30*60),origin=(as.POSIXlt('1970-01-01')))
CRstem$timestamp <- as.POSIXct(round(as.double(CRstem$timestamp)/(30*60))*(30*60),origin=(as.POSIXlt('1970-01-01')))

CRsoil$SoilWaterPot[CRsoil$SoilWaterPot < -25 | CRsoil$SoilWaterPot > 5] <- NA
CRstem$StemWaterPot[CRstem$StemWaterPot < -15 | CRstem$StemWaterPot > 5] <- NA

mergeCR <- merge(MasterMerge, CRsoil, by="timestamp", all=T)
mergeCR <- merge(mergeCR, CRstem, by="timestamp", all=T)

# Read dates of all images
file.loc <- list.dirs('/Users/alesia/FTPmountpoint/phenocams/All_Phenocam_images_and_matlab_programs/Shrub/renamed_images/renamed_current')
list.pics <- list.files(file.loc[2])

for(i in 3:length(file.loc)) {
  one.file.pics <- list.files(file.loc[i], pattern= ".jpeg")
  list.pics <- c(list.pics, one.file.pics)
}
list.pics <- data.frame(grep(".jp" , list.pics, value=T))
colnames(list.pics) <- "pic.file.name"

# Find date-times from each title
list.pics$Scene <- list.pics$year <- list.pics$DOY <- list.pics$time <- list.pics$timestamp <- NA

pic.info <- unlist(strsplit(as.character(list.pics$pic.file.name), split=".jp"))
pic.info <- as.vector(pic.info[pic.info!="eg" & pic.info!="g"])
info.split <- data.frame(matrix(unlist(strsplit(pic.info, split="_")), ncol=5, byrow=T))

list.pics$Scene <- info.split[,2]
list.pics$year <- info.split[,3]
list.pics$DOY <- info.split[,4]
list.pics$time <- info.split[,5]
list.pics$timestamp <- as.POSIXct(strptime(paste(list.pics$year, list.pics$DOY, list.pics$time), format="%Y %j %H"))

mergepics <- merge(MasterMerge, list.pics, by="timestamp", all=T)
# Sum photos/day
mergepics$date <- as.Date(mergepics$timestamp)
photos <- ddply(mergepics, c("date"), function(x) 
  data.frame(dailypics = sum(!is.na(x$pic.file.name))))
mergepics <- merge(mergepics, photos, by="date", all=T)
remove(photos)
mergepics$pics15sum <- rollapply(mergepics$dailypics, 15, sum, by=1, fill=NA, partial=TRUE, align="center")

# Read in Stem Movement file
file.loc <- '/Users/alesia/Documents/ZombieCreosote/'
list.zombie.files <- list.files(file.loc, pattern='ZombieBranches')
list.zombie.files <- paste(file.loc, list.zombie.files, sep = '')

mergezom <- read.csv(list.zombie.files[1], header=T, strip.white=T)
mergezom$timestamp <- as.POSIXct(strptime(paste(mergezom$Year, mergezom$DOY, mergezom$Time), format="%Y %j %H"))

mergezom <- merge(MasterMerge, mergezom, by="timestamp", all=T)
mergezom$Year <- year(mergezom$timestamp)
mergezom$DOY <- yday(mergezom$timestamp)
mergezom$Time <- hour(mergezom$timestamp)
mergezom$Minute <- minute(mergezom$timestamp)
mergezom <- mergezom[mergezom$Minute==0,]

# write out file of all times
# write.csv(mergezom, '/Users/alesia/Documents/ZombieCreosote/ZombieBranches2.csv', row.names=F)

# Now merge everything
allzomdat <- merge(mergeflux, mergeCR, by="timestamp", all=T)
allzomdat <- merge(allzomdat, mergezom, by="timestamp", all=T)
allzomdat <- merge(allzomdat, mergepics, by="timestamp", all=T)
allzomdat$date <- date(allzomdat$timestamp)

# Savepoint
keep.cols <- 
  names(allzomdat[,c("timestamp", "wind.speed", "RH", "VPD",
                     "precip", "air.temp", "FC", "LE", "air.press",
                     "PAR", "LW_OUT", "LW_IN", "RNET",
                     "SensorTemp", "SoilWaterPot", "Pit", "Depth", 
                     "StemSensor", "StemWaterPot", "Creosote", 
                     "Branch", "StemPosition", "pic.file.name", 
                     "Scene", "dailypics", "pics15sum",
                     names(allzomdat[,c(
                       grep("Rad_", names(allzomdat)),
                       grep("NetR", names(allzomdat)),
                       grep("SoilT", names(allzomdat)),
                       grep("par_face", names(allzomdat)),
                       grep("SWC", names(allzomdat)),
                       grep("MPS6", names(allzomdat)))]))])
allzomdat <- subset(allzomdat, select = intersect(names(allzomdat), keep.cols))
allzomdat <- allzomdat[!is.na(allzomdat$timestamp),]
plot(allzomdat$timestamp, allzomdat$SWC_S2_2p5)

# Savepoint!
write.csv(allzomdat, file="/Users/alesia/Documents/data_AJ_output/allzomdat18July2016.csv", row.names=F)

# remove(CRsoil, CRstem, list.pics, mergeCR, mergeflux, mergepics, mergezom, info.split, MasterMerge)

# allzomdat <- read.csv("/Users/alesia/Documents/data_AJ_output/allzomdat28June2016.csv", header=T, strip.white=T)
allzomdat$timestamp <- as.POSIXct(strptime(allzomdat$timestamp, format="%Y-%m-%d %H:%M:%S"))
allzomdat$date <- as.Date(allzomdat$timestamp)

# short in is the same thing as par
# keep rad long
# Get everything


# Reduce resolution to hourly
# First must get rid of all char or factor variables so that everything remaining can have a SUM
zomdathr <- allzomdat
# zomdathr <- aggregate(zomdathr, list(datehour=cut(as.POSIXct(zomdathr$timestamp), "hour")), mean)

zomdathr$Pit <- as.character(zomdathr$Pit)
zomdathr$Pit[grep("Open", zomdathr$Pit)] <- "Open3"
zomdathr$Pit <- as.factor(zomdathr$Pit)

# Crop to the desired time periods
# For now, just look at late 2011 and late 2015 
zomdathr <- subset(zomdathr, (zomdathr$timestamp > as.POSIXct(strptime("2015-7-15", format="%Y-%m-%d")) & zomdathr$timestamp < as.POSIXct(strptime("2015-12-31", format="%Y-%m-%d"))))

# zomdathr <- zomdathr[zomdathr$pics15sum > 0,]

# Melt SWC, SoilTemp, SoilPsy, StemPsy
# Aggregate into daily values first if needed
# SWC
SWCcols <- c(names(zomdathr[,grep("SWC_", names(zomdathr))]))
zomdathr.two <- zomdathr[,!(names(zomdathr) %in% SWCcols)]
SWCsub <- subset(zomdathr, select=c("timestamp", SWCcols),
                 hour(timestamp) >= 0 & hour(timestamp) <= 3)
SWCsub <- melt(SWCsub, id.vars=c("timestamp"))
SWCsub <- SWCsub[!is.na(SWCsub$value),]
colnames(SWCsub)[2:3] <- c("pit.tag", "SWC")
SWCsub$date <- date(SWCsub$timestamp)

SWCsub <- aggregate(SWC ~ pit.tag + date, data=SWCsub, mean)

# Label Pit
SWCsub$Pit <- as.character(SWCsub$pit.tag)
SWCsub$Pit[grep("SWC_O1", SWCsub$Pit)] <- "Open1"
SWCsub$Pit[grep("SWC_O2", SWCsub$Pit)] <- "Open2"
SWCsub$Pit[grep("SWC_S1", SWCsub$Pit)] <- "Shrub1"
SWCsub$Pit[grep("SWC_S2", SWCsub$Pit)] <- "Shrub2"
SWCsub$Pit <- as.factor(SWCsub$Pit)
# Label Depth
SWCsub$Depth <- as.character(SWCsub$pit.tag)
SWCsub$Depth[grep("_2p5", SWCsub$Depth)] <- "02.5cm"
SWCsub$Depth[grep("12p5", SWCsub$Depth)] <- "12.5cm"
SWCsub$Depth[grep("22p5", SWCsub$Depth)] <- "22.5cm"
SWCsub$Depth[grep("37p5", SWCsub$Depth)] <- "37.5cm"
SWCsub$Depth[grep("52p5", SWCsub$Depth)] <- "52.5cm"
SWCsub$Depth <- as.factor(SWCsub$Depth)

# Merge
zomdathr.two <- merge(zomdathr.two, SWCsub[,c("date", "Pit", "Depth", "SWC")], by=c("date", "Pit", "Depth"), all=T)
#remove(SWCsub)

# Soil Temp
SoilTcols <- c(names(zomdathr.two[,grep("SoilT_", names(zomdathr.two))]), names(zomdathr.two[,intersect(grep("MPS6",names(zomdathr.two)),grep("T_Avg",names(zomdathr.two)))]))
SoilTsub <- subset(zomdathr.two, select=c("timestamp", SoilTcols))

SoilTsub <- melt(SoilTsub, id.vars=c("timestamp"))
SoilTsub <- SoilTsub[!is.na(SoilTsub$value),]
colnames(SoilTsub)[2:3] <- c("pit.tag", "SoilTemp")

# Label Sensor Type
SoilTsub$TempType <- as.character(SoilTsub$pit.tag)
SoilTsub$TempType[grep("SoilT", SoilTsub$pit.tag)] <- "CS616"
SoilTsub$TempType[grep("MPS6", SoilTsub$pit.tag)] <- "MPS6"
SoilTsub$TempType <- as.factor(SoilTsub$TempType)

# Label Pit
SoilTsub$Pit <- as.character(SoilTsub$pit.tag)
SoilTsub$Pit[grep("MPS6_O3", SoilTsub$pit.tag)] <- "Open3"
SoilTsub$Pit[grep("MPS6_S1", SoilTsub$pit.tag)] <- "Shrub1"
SoilTsub$Pit[grep("MPS6_S2", SoilTsub$pit.tag)] <- "Shrub2"
SoilTsub$Pit[grep("MPS6_S3", SoilTsub$pit.tag)] <- "Shrub3"
SoilTsub$Pit[grep("SoilT_O1", SoilTsub$pit.tag)] <- "Open1"
SoilTsub$Pit[grep("SoilT_O2", SoilTsub$pit.tag)] <- "Open2"
SoilTsub$Pit[grep("SoilT_S1", SoilTsub$pit.tag)] <- "Shrub1"
SoilTsub$Pit[grep("SoilT_S2", SoilTsub$pit.tag)] <- "Shrub2"
SoilTsub$Pit <- as.factor(SoilTsub$Pit)

# Label Depth
SoilTsub$Depth <- as.character(SoilTsub$pit.tag)
SoilTsub$Depth[grep("_2p5", SoilTsub$pit.tag)] <- "02.5cm"
SoilTsub$Depth[grep("12p5", SoilTsub$pit.tag)] <- "12.5cm"
SoilTsub$Depth[grep("15_", SoilTsub$pit.tag)] <- "15cm"
SoilTsub$Depth[grep("22p5", SoilTsub$pit.tag)] <- "22.5cm"
SoilTsub$Depth[grep("37p5", SoilTsub$pit.tag)] <- "37.5cm"
SoilTsub$Depth[grep("52p5", SoilTsub$pit.tag)] <- "52.5cm"
SoilTsub$Depth <- as.factor(SoilTsub$Depth)

# Merge
zomdathr.three <- zomdathr.two[,!(names(zomdathr.two) %in% SoilTcols)]
zomdathr.three <- merge(zomdathr.three, SoilTsub[,c("timestamp", "SoilTemp", "TempType", "Pit", "Depth")], by=c("timestamp", "Pit", "Depth"), all=T)
#remove(SoilTsub)

#Combine MPS6 and CR7 soil water potential
MPSpsycols <- c(names(zomdathr[,intersect(grep("MPS6",names(zomdathr)),grep("_P",names(zomdathr)))]))
CRsoilpsy <- subset(zomdathr, select=c("timestamp", "SoilWaterPot", "Pit", "Depth"))
CRsoilpsy <- CRsoilpsy[!is.na(CRsoilpsy$SoilWaterPot),]
CRsoilpsy$PsyType <- as.factor("CR")
halfsoilpsy <- subset(zomdathr, select=c("timestamp", MPSpsycols))

halfsoilpsy <- melt(halfsoilpsy, id.vars=c("timestamp"))
halfsoilpsy <- halfsoilpsy[!is.na(halfsoilpsy$value),]
halfsoilpsy <- halfsoilpsy[halfsoilpsy$value > -9999,]
colnames(halfsoilpsy)[2:3] <- c("pit.tag", "SoilWaterPot")

halfsoilpsy$SoilWaterPot <- halfsoilpsy$SoilWaterPot/1000

# Label Sensor Type
halfsoilpsy$PsyType <- as.factor("MPS6")

# Label Pit
halfsoilpsy$Pit <- as.character(halfsoilpsy$pit.tag)
halfsoilpsy$Pit[grep("MPS6_O3", halfsoilpsy$pit.tag)] <- "Open3"
halfsoilpsy$Pit[grep("MPS6_S1", halfsoilpsy$pit.tag)] <- "Shrub1"
halfsoilpsy$Pit[grep("MPS6_S2", halfsoilpsy$pit.tag)] <- "Shrub2"
halfsoilpsy$Pit[grep("MPS6_S3", halfsoilpsy$pit.tag)] <- "Shrub3"
halfsoilpsy$Pit <- as.factor(halfsoilpsy$Pit)

# Label Depth
halfsoilpsy$Depth <- as.character(halfsoilpsy$pit.tag)
halfsoilpsy$Depth[grep("15_", halfsoilpsy$pit.tag)] <- "15cm"
halfsoilpsy$Depth[grep("22p5", halfsoilpsy$pit.tag)] <- "22.5cm"
halfsoilpsy$Depth[grep("37p5", halfsoilpsy$pit.tag)] <- "37.5cm"
halfsoilpsy$Depth <- as.factor(halfsoilpsy$Depth)

allsoilpsy <- rbind(CRsoilpsy[,c("timestamp", "SoilWaterPot", "Pit", "Depth", "PsyType")], halfsoilpsy[,c("timestamp", "SoilWaterPot", "Pit", "Depth", "PsyType")])


zomdathr.four <- zomdathr.three[,!(names(zomdathr.three) %in% MPSpsycols)]
zomdathr.four$PsyType <- NA
zomdathr.four$PsyType[!is.na(zomdathr.four$SoilWaterPot)] <- "WireCage"
zomdathr.four <- merge(zomdathr.four, halfsoilpsy[,c("timestamp", "PsyType", "SoilWaterPot", "Pit", "Depth")], by=c("timestamp", "Pit", "Depth", "PsyType", "SoilWaterPot"), all=T)
#remove(halfsoilpsy)

# Savepoint
write.csv(zomdathr.four, file="/Users/alesia/Documents/data_AJ_output/longzomdat1July2016.csv", row.names=F)
longzomdat <- zomdathr.four
# longzomdat <- read.csv("/Users/alesia/Documents/data_AJ_output/longzomdat1July2016.csv", header=T, strip.white=T)
longzomdat$timestamp <- as.POSIXct(strptime(longzomdat$timestamp, format="%Y-%m-%d %H:%M:%S"))
longzomdat$date <- as.Date(longzomdat$date)



# Plots
summary(longzomdat$timestamp)
start.date <- as.POSIXct(strptime("2015-07-20 00:00:00", format="%Y-%m-%d %H:%M:%S"))
end.date <- as.POSIXct(strptime("2015-12-30 00:00:00", format="%Y-%m-%d %H:%M:%S"))

photo.plot <- ggplot(zomdathr, aes(x=date, y=dailypics)) +
  geom_point() + theme_bw() + 
  theme() +
  xlim(date(start.date), date(end.date)) #+ ylim(0,15)

stems.plot <- ggplot(zomdathr, aes(x=timestamp, y=StemPosition, group=as.factor(Branch+Creosote), colour=as.factor(Branch+Creosote))) +
  geom_point() + theme_bw() + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(start.date, end.date) #+ ylim(0,15)

SWC.plot <- ggplot(SWCsub, aes(x=date, y=SWC, group=Depth, colour=Depth)) + facet_grid(Pit~.) +
  geom_point() + theme_bw() + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(date(start.date), date(end.date)) #+ ylim(0,15)

SoilWP.plot <- ggplot(allsoilpsy, aes(x=timestamp, y=SoilWaterPot, group=Depth, colour=Depth, shape=PsyType)) + facet_grid(Pit~.) +
  geom_point() + theme_bw() + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(start.date, end.date) #+ ylim(0,15)


StemWP.plot <- ggplot(allzomdat, aes(x=timestamp, y=StemWaterPot, group=StemSensor, colour=StemSensor)) +
  geom_point() + geom_line() + theme_bw() + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(start.date, end.date) + ylim(-9,1)
StemWP.plot

humid.plot <- ggplot(zomdathr, aes(x=timestamp, y=RH)) +
  geom_point() + theme_bw() + 
  xlim(start.date, end.date)

SoilT.plot <- ggplot(SoilTsub, aes(x=timestamp, y=SoilTemp, group=Depth, colour=Depth)) + geom_point(aes(alpha=0.3)) + theme_bw() + facet_grid(Pit~.)



grid.arrange(photo.plot, SWC.plot, SoilWP.plot, StemWP.plot, ncol=1)



# subset for impt columns and clean up
subzomdat <- subset(zomdathr, select=c(timestamp, 
  temp_mean, rH, RH_Avg, wnd_spd, 
  "Rad_short_Up_Avg", "Rad_short_Dn_Avg", "Rad_long_Up__Avg", "Rad_long_Dn__Avg",
  rain_Tot, 
  "MPS6_O3_15_P_Avg", "MPS6_O3_15_T_Avg", "MPS6_O3_22p5_P_Avg", "MPS6_O3_22p5_T_Avg", "MPS6_O3_37p5_P_Avg", "MPS6_O3_37p5_T_Avg", "MPS6_S1_15_P_Avg", "MPS6_S1_15_T_Avg", "MPS6_S1_22p5_P_Avg", "MPS6_S1_22p5_T_Avg", "MPS6_S1_37p5_P_Avg", "MPS6_S1_37p5_T_Avg", "MPS6_S2_15_P_Avg", "MPS6_S2_15_T_Avg", "MPS6_S2_22p5_P_Avg", "MPS6_S2_22p5_T_Avg", "MPS6_S2_37p5_P_Avg", "MPS6_S2_37p5_T_Avg", "MPS6_S3_15_P_Avg", "MPS6_S3_15_T_Avg", "MPS6_S3_22p5_P_Avg", "MPS6_S3_22p5_T_Avg", "MPS6_S3_37p5_P_Avg", "MPS6_S3_37p5_T_Avg", "SoilWaterPot", "Pit", "Depth", "StemSensor", "StemWaterPot", "Creosote", "Branch", "StemPosition", "pic.file.name", "Scene"))

# And plot time series of all data.
