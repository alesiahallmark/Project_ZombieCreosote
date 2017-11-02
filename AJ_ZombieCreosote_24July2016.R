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

mergeflux <- read.csv(list.flux.files[2], header=T, strip.white=T, sep=",")
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

mergezom <- merge(MasterMerge, mergezom, by="timestamp", all=T)
#mergezom$Year <- year(mergezom$timestamp)
#mergezom$DOY <- yday(mergezom$timestamp)
#mergezom$Time <- hour(mergezom$timestamp)
#mergezom$Minute <- minute(mergezom$timestamp)
#mergezom <- mergezom[mergezom$Minute==0,]

# write out file of all times
# write.csv(mergezom, '/Users/alesia/Documents/ZombieCreosote/ZombieBranches2.csv', row.names=F)

# Now merge everything
allzomdat <- merge(mergeflux, mergeCR, by="timestamp", all=T)
allzomdat <- merge(allzomdat, mergezom, by="timestamp", all=T)
#allzomdat <- merge(allzomdat, mergepics, by="timestamp", all=T)
allzomdat$date <- date(allzomdat$timestamp)

# Savepoint
allzomdat$VPD <- NA
keep.cols <- 
  names(allzomdat[,c("timestamp", "wind.speed", "RH", "VPD",
                     "precip", "air.temp", "FC", "LE", "air.press",
                     "PAR", "LW_OUT", "LW_IN", "RNET",
                     "SensorTemp", "SoilWaterPot", "Pit", "Depth", 
                     "StemSensor", "StemWaterPot",
                     names(allzomdat[,c(
                       grep("Rad_", names(allzomdat)),
                       grep("NetR", names(allzomdat)),
                       grep("SoilT", names(allzomdat)),
                       grep("par_face", names(allzomdat)),
                       grep("SWC", names(allzomdat)),
                       grep("MPS6", names(allzomdat)),
                       grep("z1_", names(allzomdat)),
                       grep("z2_", names(allzomdat)),
                       grep("z3_", names(allzomdat)))]))])
allzomdat <- subset(allzomdat, select = intersect(names(allzomdat), keep.cols))
allzomdat <- allzomdat[!is.na(allzomdat$timestamp),]
plot(allzomdat$timestamp, allzomdat$SWC_S2_2p5)

# Savepoint!
write.csv(allzomdat, file="/Users/alesia/Documents/data_AJ_output/allzomdat20Aug2016.csv", row.names=F)

# remove(CRsoil, CRstem, list.pics, mergeCR, mergeflux, mergepics, mergezom, info.split, MasterMerge)

# allzomdat <- read.csv("/Users/alesia/Documents/data_AJ_output/allzomdat20Aug2016.csv", header=T, strip.white=T)
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
zomdathr <- subset(zomdathr, (zomdathr$timestamp > as.POSIXct(strptime("2015-07-15", format="%Y-%m-%d")) & zomdathr$timestamp < as.POSIXct(strptime("2015-12-30", format="%Y-%m-%d"))))


# Melt SWC, SoilTemp, SoilPsy, StemPsy
# Aggregate into daily values first if needed
# Humidity
RHsub <- unique(subset(zomdathr, select = c(timestamp, date, RH)))
RHsub <- RHsub[with(RHsub, order(timestamp)),]
RHsub$dRH <- c(NA,diff(RHsub$RH,2),NA) / (c(NA,diff(RHsub$timestamp,2),NA)/60)

detrend <- lm(RH ~ poly(hour(timestamp),3,raw=T), data=RHsub, na.action=na.exclude)
RHsub$resid.hr <- residuals(detrend)
RHsub$trend.hr <- coefficients(detrend)[1] + hour(RHsub$timestamp)*coefficients(detrend)[2] + hour(RHsub$timestamp)^2*coefficients(detrend)[3] + hour(RHsub$timestamp)^3*coefficients(detrend)[4]
              
minmaxmean <- ddply(RHsub, c("date"), function(x)
  data.frame(minRH = min(x$RH, na.rm=TRUE),
             maxRH = max(x$RH, na.rm=TRUE),
             meanRH = mean(x$RH, na.rm=TRUE),
             deltaRH = max(x$RH, na.rm=TRUE) - min(x$RH, na.rm=TRUE)))
RHsub <- merge(RHsub, minmaxmean, by='date', all=T)

# PAR
PARsub <- unique(subset(zomdathr, select = c(timestamp, date, PAR)))
PARsub <- PARsub[with(PARsub, order(timestamp)),]
PARsub$dPAR <- c(NA,diff(PARsub$PAR,2),NA) / (c(NA,diff(PARsub$timestamp,2),NA)/60)

PARsub.nonzero <- PARsub[PARsub$PAR > 1,]
detrend <- lm(PAR ~ poly(hour(timestamp),2,raw=T), data=PARsub.nonzero, na.action=na.exclude)
summary(detrend)
PARsub.nonzero$resid.hr <- residuals(detrend)
PARsub.nonzero$trend.hr <- coefficients(detrend)[1] + hour(PARsub.nonzero$timestamp)*coefficients(detrend)[2] + hour(PARsub.nonzero$timestamp)^2*coefficients(detrend)[3] 
PARsub <- merge(PARsub, PARsub.nonzero[,c("timestamp", "resid.hr", "trend.hr")], by="timestamp", all=T)
PARsub <- PARsub[!is.na(PARsub$timestamp),]

minmaxmean <- ddply(PARsub, c("date"), function(x)
  data.frame(sumPAR = sum(x$PAR, na.rm=TRUE),
             deltaPAR = max(x$PAR, na.rm=TRUE) - min(x$PAR, na.rm=TRUE)))
PARsub <- merge(PARsub, minmaxmean, by='date', all=T)

# Air Temp
AirTsub <- unique(subset(zomdathr, select = c(timestamp, date, air.temp)))
AirTsub <- AirTsub[with(AirTsub, order(timestamp)),]
AirTsub$dAirT <- c(NA,diff(AirTsub$air.temp,2),NA) / (c(NA,diff(AirTsub$timestamp,2),NA)/60)

detrend <- lm(air.temp ~ poly(hour(timestamp),3,raw=T), data=AirTsub, na.action=na.exclude)
summary(detrend)
AirTsub$resid.hr <- residuals(detrend)
AirTsub$trend.hr <- coefficients(detrend)[1] + hour(AirTsub$timestamp)*coefficients(detrend)[2] + hour(AirTsub$timestamp)^2*coefficients(detrend)[3] + hour(AirTsub$timestamp)^3*coefficients(detrend)[4]

minmaxmean <- ddply(AirTsub, c("date"), function(x)
  data.frame(sumAirT = sum(x$air.temp, na.rm=TRUE),
             minAirT = min(x$air.temp, na.rm=TRUE),
             maxAirT = max(x$air.temp, na.rm=TRUE),
             deltaAirT = max(x$air.temp, na.rm=TRUE) - min(x$air.temp, na.rm=TRUE)))
AirTsub <- merge(AirTsub, minmaxmean, by='date', all=T)

# Precip
precipsub <- unique(zomdathr[,c("timestamp", "precip")])
precipsub$Precip15 <- rollapply(precipsub$precip, (15*2*12), sum, align="right", fill=NA)


# SWC
SWCcols <- c(names(zomdathr[,grep("SWC_", names(zomdathr))]))
SWCsub <- subset(zomdathr, select=c("timestamp", SWCcols),
                 hour(timestamp) >= 0 & hour(timestamp) <= 3)
SWCsub <- melt(SWCsub, id.vars=c("timestamp"))
SWCsub <- SWCsub[!is.na(SWCsub$value),]
colnames(SWCsub)[2:3] <- c("pit.tag", "SWC")
SWCsub$date <- date(SWCsub$timestamp)

SWCsub <- aggregate(SWC ~ pit.tag + date, data=SWCsub, mean)

SWCsub$pit.tag[SWCsub$pit.tag=="SWC_S2_37p5" & SWCsub$date > ymd("2015-11-06")] <- "SWC_S2_22p5"
SWCsub$pit.tag[SWCsub$pit.tag=="SWC_S2_12p5" & SWCsub$date > ymd("2015-11-06")] <- "SWC_S2_52p5"

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

SWCsub$SWC[SWCsub$Pit=="Open2" & SWCsub$Depth=="52.5cm" & SWCsub$date > ymd("2015-10-18")] <- NA

SWCsubmean <- aggregate(SWC ~ date + Depth, data = SWCsub, mean)
colnames(SWCsubmean)[3] <- "meanSWC" 
SWCsub <- merge(SWCsub, SWCsubmean, by = c("date", "Depth"), all=T)
  

# Merge
#zomdathr.two <- merge(zomdathr.two, SWCsub[,c("date", "Pit", "Depth", "SWC")], by=c("date", "Pit", "Depth"), all=T)
#remove(SWCsub)

# Soil Temp
SoilTcolsone <- c(names(zomdathr[,grep("SoilT_", names(zomdathr))]))
SoilTcolstwo <- c(names(zomdathr[,intersect(grep("MPS6",names(zomdathr)),grep("15_T",names(zomdathr)))]))
SoilTsubone <- unique(subset(zomdathr, select=c("timestamp", SoilTcolsone)))
SoilTsubtwo <- unique(subset(zomdathr, select=c("timestamp", SoilTcolstwo)))

SoilTsub <- rbind(melt(SoilTsubone, id.vars=c("timestamp")), melt(SoilTsubtwo, id.vars=c("timestamp")))
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

SoilTsub <- subset(SoilTsub, select=c(timestamp, Pit, Depth, SoilTemp))
SoilTsub <- aggregate(SoilTemp ~., data = SoilTsub, mean)

SoilTsub$Cover <- as.character(SoilTsub$Pit)
SoilTsub$Cover[grep("Open", SoilTsub$Cover)] <- "Open"
SoilTsub$Cover[grep("Shrub", SoilTsub$Cover)] <- "Shrub"
SoilTsub$Cover <- as.factor(SoilTsub$Cover)

SoilTdif <- subset(SoilTsub, select=c(timestamp, Cover, Depth, SoilTemp))
SoilTdif <- aggregate(SoilTemp ~., data = SoilTdif, mean)
SoilTdif <- dcast(SoilTdif, timestamp + Depth ~ Cover)
SoilTdif$SoilT.dif <- SoilTdif$Shrub - SoilTdif$Open
SoilTsub <- merge(SoilTsub, SoilTdif[,c("timestamp", "Depth", "SoilT.dif")], by=c("timestamp", "Depth"), all=T)
  


# Stem Water Potential
StemWPsub <- unique(subset(zomdathr, select=c(timestamp, StemWaterPot, StemSensor)))
StemWPsub <- StemWPsub[!is.na(StemWPsub$StemWaterPot),]
StemWPsub$StemWaterPot[StemWPsub$StemWaterPot>0] <- 0

StemWPsubone <- StemWPsub[StemWPsub$StemSensor=="ICT1809",]
StemWPsutwo <- subset(StemWPsub, StemSensor=="Dixon4" & timestamp < as.POSIXct(strptime("2015-08-15", format="%Y-%m-%d")))
StemWPsub <- rbind(StemWPsubone, StemWPsutwo)
StemWPsubmorn <- subset(StemWPsub, hour(timestamp) >= 0 & hour(timestamp) <= 3)
StemWPsubmorn$date <- date(StemWPsubmorn$timestamp)
StemWPsubmorn <- aggregate(StemWaterPot ~ date, data=StemWPsubmorn, mean)
StemWPsubmorn$StemWPmornsm <- rollapply(StemWPsubmorn$StemWaterPot, 5, mean, fill=NA)

StemWPsub$StemWPsm <- rollapply(StemWPsub$StemWaterPot, 11, mean, fill=NA)
StemWPsub$date <- date(StemWPsub$timestamp)
StemWPsub <- merge(StemWPsub, StemWPsubmorn[,c("date", "StemWPmornsm")], by="date", all=T)

detrend <- lm(StemWPsm ~ poly(hour(timestamp),3,raw=T), data=StemWPsub, na.action=na.exclude)
summary(detrend)
StemWPsub$resid.hr <- residuals(detrend)
StemWPsub$trend.hr <- coefficients(detrend)[1] + 
  hour(StemWPsub$timestamp)*coefficients(detrend)[2] + 
  hour(StemWPsub$timestamp)^2*coefficients(detrend)[3] + 
  hour(StemWPsub$timestamp)^3*coefficients(detrend)[4] 

# Combine MPS6 and CR7 soil water potential
MPSpsycols <- c(names(zomdathr[,intersect(grep("MPS6",names(zomdathr)),grep("_P",names(zomdathr)))]))
CRsoilpsy <- subset(zomdathr, select=c("timestamp", "SoilWaterPot", "Pit", "Depth"))
CRsoilpsy <- CRsoilpsy[!is.na(CRsoilpsy$SoilWaterPot),]
CRsoilpsy <- CRsoilpsy[CRsoilpsy$Pit != "Shrub1",]
CRsoilpsy$SoilWaterPot[CRsoilpsy$SoilWaterPot>0] <- 0
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

allsoilpsy <- subset(allsoilpsy, hour(timestamp) >= 0 & hour(timestamp) <= 3)
allsoilpsy$date <- date(allsoilpsy$timestamp)
allsoilpsy <- aggregate(SoilWaterPot ~ date + Pit + Depth + PsyType, data=allsoilpsy, mean)

allsoilpsy$Cover <- as.character(allsoilpsy$Pit)
allsoilpsy$Cover[grep("Open", allsoilpsy$Cover)] <- "Open"
allsoilpsy$Cover[grep("Shrub", allsoilpsy$Cover)] <- "Shrub"
allsoilpsy$Cover <- as.factor(allsoilpsy$Cover)

soilpsymeans <- aggregate(SoilWaterPot ~ date + Depth + Cover + PsyType, data = allsoilpsy, mean)
colnames(soilpsymeans)[5] <- "SoilWP.covmean"

allsoilpsy <- merge(allsoilpsy, soilpsymeans, by = c("date", "Depth", "Cover", "PsyType"), all=T)


# Stem Movement Files
stemcols <- intersect(c(names(zomdathr[,grep("z1", names(zomdathr))]), names(zomdathr[,grep("z2", names(zomdathr))]), names(zomdathr[,grep("z3", names(zomdathr))])), names(zomdathr[,grep("y", names(zomdathr))]))
Wigglesub <- subset(zomdathr, select=c("timestamp", stemcols))

# Fill, Smooth, Fill
firstbranchcolumn <- min(grep ("z", names(Wigglesub)))
lastbranchcolumn <- max(grep ("z", names(Wigglesub)))
Wigglesub <- Wigglesub[with(Wigglesub, order(timestamp)),]
for (i in firstbranchcolumn:lastbranchcolumn) {
  Wigglesub[,i] <- na.approx(Wigglesub[,i], Wigglesub$timestamp, na.rm=F, maxgap = 6)
}
for (i in firstbranchcolumn:lastbranchcolumn) {
  Wigglesub[,i] <- rollapply(Wigglesub[,i], 5, mean, fill=NA)
}

# Label bushes and branch status
metazom <- data.frame(cbind(
  c("z1_1x","z1_1y","z1_2x","z1_2y","z1_3x","z1_3y","z1_4x","z1_4y","z1_5x","z1_5y",
    "z2_1x","z2_1y","z2_2x","z2_2y","z2_3x","z2_3y","z2_4x","z2_4y","z2_5x","z2_5y","z2_6x","z2_6y","z2_7x","z2_7y",
    "z3_1x","z3_1y","z3_2x","z3_2y","z3_3x","z3_3y","z3_4x","z3_4y","z3_5x","z3_5y","z3_6x","z3_6y","z3_7x","z3_7y","z3_8x","z3_8y"),
  c("z1_1","z1_1","z1_2","z1_2","z1_3","z1_3","z1_4","z1_4","z1_5","z1_5",
    "z2_1","z2_1","z2_2","z2_2","z2_3","z2_3","z2_4","z2_4","z2_5","z2_5","z2_6","z2_6","z2_7","z2_7",
    "z3_1","z3_1","z3_2","z3_2","z3_3","z3_3","z3_4","z3_4","z3_5","z3_5","z3_6","z3_6","z3_7","z3_7","z3_8","z3_8"),
  c("z1_sh1","z1_sh1","z1_sh2","z1_sh2","z1_sh2","z1_sh2","z1_sh3","z1_sh3","z1_sh4","z1_sh4",
    "z2_sh1","z2_sh1","z2_sh2","z2_sh2","z2_sh3","z2_sh3","z2_sh4","z2_sh4","z2_sh4","z2_sh4","z2_sh4","z2_sh4","z2_sh4","z2_sh4",
    "z3_sh1","z3_sh1","z3_sh1","z3_sh1","z3_sh1","z3_sh1","z3_sh2","z3_sh2","z3_sh2","z3_sh2","z3_sh2","z3_sh2","z3_sh2","z3_sh2","z3_sh2","z3_sh2"),
  c("L","L","D","D","D","D","L","L","L","L",
    "L","L","L","L","L","L","D","D","D","D","D","D","L","L",
    "D","D","L","L","L","L","L","L","D","D","D","D","D","D","L","L")))
colnames(metazom) <- c("StemIDxy", "StemID", "ShrubID", "LDStatus")

Wigglesub <- melt(Wigglesub, id.vars=c("timestamp"))
Wigglesub <- unique(Wigglesub)
Wigglesub <- Wigglesub[!is.na(Wigglesub$value),]
colnames(Wigglesub)[2:3] <- c("StemIDxy", "YPosition")
Wigglesub <- merge(Wigglesub, metazom, by="StemIDxy", all=T)

Wigglesub <- Wigglesub[grep("y", Wigglesub$StemIDxy),]
Wigglesub$YPosition <- 2167 - Wigglesub$YPosition

# Only look at zombie1 branches for now
Wigglesub <- Wigglesub[grep("z1", Wigglesub$StemID),]

# Find difference from mean, % total movement, derivative
means <- ddply(Wigglesub, c("StemID"), function(x)
  data.frame(stem.mean = mean(x$YPosition, na.rm=TRUE),
             stem.maxdif = max(
               (max(x$YPosition, na.rm=T) - 
                  mean(x$YPosition, na.rm=T)),
               (mean(x$YPosition, na.rm=T) - 
                  min(x$YPosition, na.rm=T))),
             perc98 = quantile(x$YPosition, probs=0.98, na.rm=TRUE),
             perc2 = quantile(x$YPosition, probs=0.01, na.rm=TRUE)))
Wigglesub <- merge(Wigglesub, means, by='StemID', all=T)
Wigglesub$mean.dif <- (Wigglesub$YPosition - Wigglesub$stem.mean) /
  (Wigglesub$stem.maxdif)
Wigglesub$PercentY <- (Wigglesub$YPosition - Wigglesub$perc2) / (Wigglesub$perc98 - Wigglesub$perc2)

Wigglesub$PercentY[Wigglesub$PercentY > 1] <- 1
Wigglesub$PercentY[Wigglesub$PercentY < 0] <- 0

# Derivative of mean.dif
deriv <- ddply(Wigglesub, c("StemID"), function(x)
  data.frame(stem.deriv = c(NA,diff(x$mean.dif,2),NA) / (c(NA,diff(x$timestamp,2),NA)/7200)))
Wigglesub$stem.deriv <- deriv$stem.deriv
Wigglesub$stem.deriv[is.infinite(Wigglesub$stem.deriv)] <- NA

# Delta movement per day
Wigglesub$date <- date(Wigglesub$timestamp)
deltastem <- ddply(Wigglesub, c("date", "StemID"), function(x)
  data.frame(min.stem = min(x$mean.dif, na.rm=TRUE),
             max.stem = max(x$mean.dif, na.rm=TRUE),
             delta.stem = max(x$mean.dif, na.rm=TRUE) - min(x$mean.dif, na.rm=TRUE)))
Wigglesub<- merge(Wigglesub, deltastem, by=c("date", "StemID"), all=T)

detrend <- lm(mean.dif ~ StemID + poly(hour(timestamp),3,raw=T), data=Wigglesub, na.action=na.exclude)
Wigglesub$trend.hr <- coefficients(detrend)[1] + hour(Wigglesub$timestamp)*coefficients(detrend)[6] + hour(Wigglesub$timestamp)^2*coefficients(detrend)[7] + hour(Wigglesub$timestamp)^3*coefficients(detrend)[8]
Wigglesub$resid.hr <- Wigglesub$mean.dif-Wigglesub$trend.hr


################################################
# Plots
#summary(longzomdat$timestamp)
start.date <- as.POSIXct(strptime("2015-07-31 00:00:00", format="%Y-%m-%d %H:%M:%S"))
end.date <- as.POSIXct(strptime("2015-12-15 00:00:00", format="%Y-%m-%d %H:%M:%S"))
tenday.start <- as.POSIXct(strptime("2015-10-14 00:00:00", format="%Y-%m-%d %H:%M:%S"))
tenday.end <- as.POSIXct(strptime("2015-10-24 00:00:00", format="%Y-%m-%d %H:%M:%S"))

stems.plot <- ggplot(Wigglesub, aes(x=timestamp, y=YPosition, group=StemID, colour=StemID)) +
  geom_point() + geom_line() + theme_bw() + 
  theme(legend.position="none") +
  xlim(start.date, end.date) 

stems.plot.mean <- ggplot(Wigglesub, aes(x=timestamp, y=mean.dif, group=StemID, colour=StemID)) +
  geom_point(alpha=0.5) + geom_line() + theme_bw() + 
  theme(legend.position="none") +
  xlim(start.date, end.date) 

stems.plot.perc <- ggplot(Wigglesub, aes(x=timestamp, y=PercentY, group=StemID, colour=StemID)) +
  geom_point(alpha=0.5) + geom_line() + theme_bw() + 
  theme(legend.position="none") +
  xlim(start.date, end.date)

SWC.plot.mod <- ggplot(SWCsub, aes(x=date, y=SWC, group=Depth, colour=Depth)) + #facet_grid(Pit~.) +
  geom_point() + geom_line() + theme_bw() + facet_grid(Pit~.) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(date(start.date), date(end.date)) 

SWCall.plot <- ggplot(SWCsub, aes(x=date, y=meanSWC, group=Depth, colour=Depth)) + #facet_grid(Pit~.) +
  geom_point() + geom_line() + theme_bw() +
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(date(start.date), date(end.date)) 

SoilWP.plot <- ggplot(allsoilpsy, aes(x=date, y=SoilWP.covmean, group=Depth, colour=Depth, shape=Cover)) + facet_grid(PsyType~.) +
  geom_point() + theme_bw() + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(as.Date(start.date), as.Date(end.date))

StemWP.plot <- ggplot(StemWPsub, aes(x=timestamp, y=StemWPsm)) +
  geom_line(colour="blue") + theme_bw() +
  geom_line(colour="black", aes(y=StemWPmornsm)) + 
  theme(legend.position="none") +
  xlim(start.date, end.date)

humid.plot <- ggplot(RHsub, aes(x=timestamp, y=RH)) +
  geom_point(alpha=0.5) + theme_bw() +
  geom_point(aes(colour="green", y=RHsub$meanRH)) +
  geom_point(aes(colour="red", y=RHsub$maxRH)) +
  theme(legend.position="none") +
  xlim(start.date, end.date)

PAR.plot <- ggplot(PARsub, aes(x=timestamp, y=PAR)) +
  geom_point(alpha=0.5, colour="orange") + theme_bw() +
  geom_line(alpha=0.5, colour="red", aes(y=deltaPAR)) +
  geom_line(alpha=0.5, colour="black", aes(y=sumPAR/24)) +
  theme(legend.position="none") +
  xlim(start.date, end.date)

AirT.plot <- ggplot(AirTsub, aes(x=timestamp, y=air.temp)) +
  geom_point(aes(alpha=0.3)) + theme_bw() + 
  geom_line(colour="red",aes(y=deltaAirT)) +
  theme(legend.position="none") +
  xlim(start.date, end.date)

SoilT.plot <- ggplot(SoilTsub, aes(x=timestamp, y=SoilTemp, group=Depth, colour=Depth)) + 
  geom_line() + theme_bw() +
  facet_grid(Cover~.) + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(start.date, end.date)

SoilTdif.plot <- ggplot(SoilTsub[SoilTsub$Depth=="02.5cm" | SoilTsub$Depth=="12.5cm",], aes(x=timestamp, y=SoilT.dif, group=Depth, colour=SoilT.dif)) + 
  geom_line() + geom_hline(aes(yintercept=0)) +
  theme_bw() + facet_grid(Depth~.) +
  theme(legend.position="none") +
  scale_colour_gradient(limits=c(-11.5,11.5),high="red",low="blue") +
  xlim(start.date, end.date)

#grid.arrange(humid.plot, photo.plot, ncol=1)
#grid.arrange(SWC.plot, stems.plot.two, ncol=1)
#grid.arrange(SoilWP.plot, stems.plot.two, ncol=1)
#grid.arrange(SoilWPMPS.plot, stems.plot.two, ncol=1)
#grid.arrange(StemWP.plot, stems.plot.two, ncol=1)

print(humid.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/humid_plot.png", width=10, height=6, units="in", dpi=300)

print(PAR.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/PAR_plot.png", width=10, height=6, units="in", dpi=300)

print(AirT.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/AirT_plot.png", width=10, height=6, units="in", dpi=300)

print(stems.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_plot.png", width=10, height=6, units="in", dpi=300)

print(stems.plot.mean)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_plot_mean.png", width=10, height=6, units="in", dpi=300)

print(stems.plot.perc)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_plot_perc.png", width=10, height=6, units="in", dpi=300)

print(SWC.plot.mod)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/SWC_plot.mod.png", width=10, height=6, units="in", dpi=300)

print(SWCall.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/SWC_plot.all.png", width=10, height=6, units="in", dpi=300)

print(StemWP.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/StemWP_plot.png", width=10, height=6, units="in", dpi=300)

print(SoilWP.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/SoilWP_plot.png", width=10, height=6, units="in", dpi=300)

print(SoilT.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/SoilT_plot.png", width=10, height=6, units="in", dpi=300)

print(SoilTdif.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/SoilTdif_plot.png", width=10, height=6, units="in", dpi=300)


# Residual plots
RHresid1 <- ggplot(RHsub, aes(x=timestamp, y=RH)) + geom_point() + theme_bw() +
  xlim(tenday.start, tenday.end)
RHresid2 <- ggplot(RHsub, aes(x=timestamp, y=trend.hr)) + geom_line(colour="red") + theme_bw() +
  xlim(tenday.start, tenday.end)
RHresid3 <- ggplot(RHsub, aes(x=timestamp, y=resid.hr)) + geom_line(colour="blue") + theme_bw() +
  geom_hline(yintercept=0, colour="dark grey") + 
  xlim(tenday.start, tenday.end)
RHresidplots <- arrangeGrob(RHresid1, RHresid2, RHresid3, ncol=1)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/RHresid_plot.png", RHresidplots,  width=10, height=6, units="in", dpi=300)

AirTresid1 <- ggplot(AirTsub, aes(x=timestamp, y=air.temp)) + geom_point() + theme_bw() +
  xlim(tenday.start, tenday.end)
AirTresid2 <- ggplot(AirTsub, aes(x=timestamp, y=trend.hr)) + geom_line(colour="red") + theme_bw() +
  xlim(tenday.start, tenday.end)
AirTresid3 <- ggplot(AirTsub, aes(x=timestamp, y=resid.hr)) + geom_line(colour="blue") + theme_bw() +
  geom_hline(yintercept=0, colour="dark grey") + 
  xlim(tenday.start, tenday.end)
AirTresidplots <- arrangeGrob(AirTresid1, AirTresid2, AirTresid3, ncol=1)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/AirTresid_plot.png", AirTresidplots,  width=10, height=6, units="in", dpi=300)

PARresid1 <- ggplot(PARsub, aes(x=timestamp, y=PAR)) + geom_point() + theme_bw() +
  xlim(tenday.start, tenday.end)
PARresid2 <- ggplot(PARsub, aes(x=timestamp, y=trend.hr)) + geom_line(colour="red") + theme_bw() +
  xlim(tenday.start, tenday.end)
PARresid3 <- ggplot(PARsub, aes(x=timestamp, y=resid.hr)) + geom_line(colour="blue") + theme_bw() +
  geom_hline(yintercept=0, colour="dark grey") + 
  xlim(tenday.start, tenday.end)
PARresidplots <- arrangeGrob(PARresid1, PARresid2, PARresid3, ncol=1)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/PARresid_plot.png", PARresidplots,  width=10, height=6, units="in", dpi=300)

StemWPresid1 <- ggplot(StemWPsub, aes(x=timestamp, y=StemWPsm)) + geom_point() + theme_bw() +
  xlim(tenday.start, tenday.end)
StemWPresid2 <- ggplot(StemWPsub, aes(x=timestamp, y=trend.hr)) + geom_line(colour="red") + theme_bw() +
  xlim(tenday.start, tenday.end)
StemWPresid3 <- ggplot(StemWPsub, aes(x=timestamp, y=resid.hr)) + geom_line(colour="blue") + theme_bw() +
  geom_hline(yintercept=0, colour="dark grey") + 
  xlim(tenday.start, tenday.end)
StemWPresidplots <- arrangeGrob(StemWPresid1, StemWPresid2, StemWPresid3, ncol=1)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/StemWPresid_plot.png", StemWPresidplots,  width=10, height=6, units="in", dpi=300)

Wiggleresid1 <- ggplot(Wigglesub, aes(x=timestamp, y=mean.dif, colour=StemID)) + geom_point() + theme_bw() + geom_line() + 
  theme(legend.position ="none") +
  xlim(tenday.start, tenday.end)
Wiggleresid2 <- ggplot(Wigglesub, aes(x=timestamp, y=trend.hr)) + geom_line(colour="red") + theme_bw() +
  xlim(tenday.start, tenday.end)
Wiggleresid3 <- ggplot(Wigglesub, aes(x=timestamp, y=resid.hr, colour=StemID)) + geom_point() + theme_bw() + geom_line() + 
  geom_hline(yintercept=0, colour="dark grey") + 
  xlim(tenday.start, tenday.end) +
  theme(legend.position ="none")
Wiggleresidplots <- arrangeGrob(Wiggleresid1, Wiggleresid2, Wiggleresid3, ncol=1)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/Wiggleresid_plot.png", Wiggleresidplots,  width=10, height=6, units="in", dpi=300)

# Residual regressions
allregs <- merge(Wigglesub[,c("timestamp", "date", "StemID", "LDStatus", "mean.dif", "stem.deriv", "delta.stem", "resid.hr")], RHsub[,c("timestamp", "RH", "dRH", "deltaRH", "resid.hr")], by="timestamp", all=T)
allregs <- merge(allregs, AirTsub[,c("timestamp", "air.temp", "sumAirT", "deltaAirT", "dAirT", "resid.hr")], by="timestamp", all=T)
allregs <- merge(allregs, PARsub[,c("timestamp", "PAR", "dPAR", "sumPAR", "deltaPAR", "resid.hr")], by="timestamp", all=T)
allregs <- merge(allregs, StemWPsub[,c("timestamp", "StemWPsm", "StemWPmornsm", "resid.hr")], by="timestamp", all=T)
allregs <- merge(allregs, precipsub[,c("timestamp", "precip", "Precip15")], by="timestamp", all=T)
allregs <- merge(allregs, SoilTsub[,c("timestamp", "Depth", "Pit", "Cover", "SoilTemp", "SoilT.dif")], by=c("timestamp"), all=T)
allregs$date <- date(allregs$timestamp)
allregs <- merge(allregs, allsoilpsy[,c("date", "Depth", "Cover", "Pit", "SoilWaterPot", "SoilWP.covmean")], by=c("date", "Depth", "Pit", "Cover"), all=T)
allregs <- merge(allregs, SWCsub[,c("date", "Depth", "Pit", "SWC", "meanSWC")], by=c("date", "Depth", "Pit"), all=T)

colnames(allregs) <- c("date", "Depth", "Pit", "Cover", "timestamp", "StemID", "LDStatus", "mean.dif", "stem.deriv", "delta.stem", "Wiggle.resid", "RH", "dRH", "deltaRH", "RH.resid", "air.temp", "sumAirT", "deltaAirT", "dAirT", "AirT.resid", "PAR", "dPAR", "sumPAR", "deltaPAR", "PAR.resid", "StemWPsm", "StemWPmornsm", "StemWP.resid", "precip", "Precip15", "SoilTemp", "SoilT.dif", "SoilWaterPot", "SoilWP.covmean", "SWC", "meanSWC")

allregs <- allregs[!is.na(allregs$mean.dif),]
summary(allregs)

# Correlation tests - residuals
resid.corr <- data.frame(
  RHest = cor.test(allregs$RH.resid, allregs$Wiggle.resid)$estimate,
  AirTest = cor.test(allregs$AirT.resid, allregs$Wiggle.resid)$estimate,
  PARest = cor.test(allregs$PAR.resid, allregs$Wiggle.resid)$estimate,
  StemWPest = cor.test(allregs$StemWP.resid, allregs$Wiggle.resid)$estimate,
  RHpval = cor.test(allregs$RH.resid, allregs$Wiggle.resid)$p.value,
  AirTpval = cor.test(allregs$AirT.resid, allregs$Wiggle.resid)$p.value,
  PARpval = cor.test(allregs$PAR.resid, allregs$Wiggle.resid)$p.value,
  StemWPpval = cor.test(allregs$StemWP.resid, allregs$Wiggle.resid)$p.value)

resid.corr$RHest <- paste("R^2 == ", round(resid.corr$RHest, digits=2))
resid.corr$AirTest <- paste("R^2 == ", round(resid.corr$AirTest, digits=2))
resid.corr$PARest <- paste("R^2 == ", round(resid.corr$PARest, digits=2))
resid.corr$StemWPest <- paste("R^2 == ", round(resid.corr$StemWPest, digits=2))
resid.corr$ypos <- .9*max(allregs$Wiggle.resid, na.rm=T)
  
WigRH.resid.plot <- ggplot(allregs, aes(x=RH.resid, y=Wiggle.resid)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=resid.corr, aes(x=mean(allregs$RH.resid,na.rm=T), y=ypos, label=RHest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)
WigAirT.resid.plot <- ggplot(allregs, aes(x=AirT.resid, y=Wiggle.resid)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=resid.corr, aes(x=mean(allregs$RH.resid,na.rm=T), y=ypos, label=AirTest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)
WigPAR.resid.plot <- ggplot(allregs, aes(x=PAR.resid, y=Wiggle.resid)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=resid.corr, aes(x=mean(allregs$RH.resid,na.rm=T), y=ypos, label=PARest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)
WigStemWP.resid.plot <- ggplot(allregs, aes(x=StemWP.resid, y=Wiggle.resid)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=resid.corr, aes(x=mean(allregs$RH.resid,na.rm=T), y=ypos, label=StemWPest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)

fourresidplots <- arrangeGrob(WigRH.resid.plot, WigAirT.resid.plot, WigPAR.resid.plot, WigStemWP.resid.plot, ncol=2)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/allresid_plots.png", fourresidplots,  width=10, height=6, units="in", dpi=300)

# Correlation tests - raw
raw.corr <- data.frame(
  RHest = cor.test(allregs$RH, allregs$mean.dif)$estimate,
  AirTest = cor.test(allregs$air.temp, allregs$mean.dif)$estimate,
  PARest = cor.test(allregs$PAR, allregs$mean.dif)$estimate,
  StemWPest = cor.test(allregs$StemWPsm, allregs$mean.dif)$estimate,
  RHpval = cor.test(allregs$RH, allregs$mean.dif)$p.value,
  AirTpval = cor.test(allregs$air.temp, allregs$mean.dif)$p.value,
  PARpval = cor.test(allregs$PAR, allregs$mean.dif)$p.value,
  StemWPpval = cor.test(allregs$StemWPsm, allregs$mean.dif)$p.value)

raw.corr$RHest <- paste("R^2 == ", round(raw.corr$RHest, digits=2))
raw.corr$AirTest <- paste("R^2 == ", round(raw.corr$AirTest, digits=2))
raw.corr$PARest <- paste("R^2 == ", round(raw.corr$PARest, digits=2))
raw.corr$StemWPest <- paste("R^2 == ", round(raw.corr$StemWPest, digits=2))
raw.corr$ypos <- .9*max(allregs$mean.dif, na.rm=T)

WigRH.cor.plot <- ggplot(allregs, aes(x=RH, y=mean.dif)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=mean(allregs$RH,na.rm=T), y=ypos, label=RHest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)
WigAirT.cor.plot <- ggplot(allregs, aes(x=air.temp, y=mean.dif)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=mean(allregs$air.temp,na.rm=T), y=ypos, label=AirTest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)
WigPAR.cor.plot <- ggplot(allregs, aes(x=PAR, y=mean.dif)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=mean(allregs$PAR,na.rm=T), y=ypos, label=PARest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)
WigStemWP.cor.plot <- ggplot(allregs, aes(x=StemWPsm, y=mean.dif)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=mean(allregs$StemWPsm,na.rm=T), y=ypos, label=StemWPest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)

fourrawplots <- arrangeGrob(WigRH.cor.plot, WigAirT.cor.plot, WigPAR.cor.plot, WigStemWP.cor.plot, ncol=2)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/allraw_plots.png", fourrawplots,  width=10, height=6, units="in", dpi=300)

# Correlations - derivatives
deriv.corr <- data.frame(
  RHest = cor.test(allregs$dRH, allregs$stem.deriv)$estimate,
  AirTest = cor.test(allregs$dAirT, allregs$stem.deriv)$estimate,
  RHpval = cor.test(allregs$dRH, allregs$stem.deriv)$p.value,
  AirTpval = cor.test(allregs$dAirT, allregs$stem.deriv)$p.value)


WigRHderiv.cor.plot <- ggplot(allregs, aes(x=dRH, y=stem.deriv)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=mean(allregs$dRH,na.rm=T), y=ypos, label=RHest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)
WigAirTderiv.cor.plot <- ggplot(allregs, aes(x=dAirT, y=stem.deriv)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=mean(allregs$dAirT,na.rm=T), y=ypos, label=AirTest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)

derivplots <- arrangeGrob(WigRHderiv.cor.plot, WigAirTderiv.cor.plot, ncol=2)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/allderiv_plots.png", derivplots,  width=10, height=6, units="in", dpi=300)


# stem movements v SWC and StemWP
WigWater.corr <- data.frame(
  WigSWCest = cor.test(allregs$meanSWC, allregs$mean.dif)$estimate,
  WigSWPest = cor.test(allregs$StemWPmornsm, allregs$mean.dif)$estimate,
  WigSWCpval = cor.test(allregs$meanSWC, allregs$mean.dif)$p.value,
  WigSWPpval = cor.test(allregs$StemWPmornsm, allregs$mean.dif)$p.value)


WigRHderiv.cor.plot <- ggplot(allregs, aes(x=dRH, y=stem.deriv)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=mean(allregs$dRH,na.rm=T), y=ypos, label=RHest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)
WigAirTderiv.cor.plot <- ggplot(allregs, aes(x=dAirT, y=stem.deriv)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=mean(allregs$dAirT,na.rm=T), y=ypos, label=AirTest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F)

derivplots <- arrangeGrob(WigRHderiv.cor.plot, WigAirTderiv.cor.plot, ncol=2)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/allderiv_plots.png", derivplots,  width=10, height=6, units="in", dpi=300)






# Branch shading
allregsshadesub <- allregs[allregs$Depth=="02.5cm" & !is.na(allregs$Depth),]

lmShade <- ddply(allregsshadesub, c("StemID", "LDStatus"), function(x) data.frame(
  Shadeest = cor.test(x$SoilT.dif, x$mean.dif)$estimate,
  Shadepval = cor.test(x$SoilT.dif, x$mean.dif)$p.value))
lmShade$Shadeest <- paste("R^2 == ", round(lmShade$Shadeest, digits=2))
lmShade$ypos <- seq(1, 0.4, length.out=length(unique(lmShade$StemID)))

Shade.plot <- ggplot(allregsshadesub, aes(x=SoilT.dif, y=mean.dif, group=StemID, colour=StemID)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") +
  geom_text(data=lmShade, aes(x=-10, y=ypos, label=Shadeest, size=0.2), vjust=1, parse=TRUE, show.legend=F)

ShadeLD.plot <- ggplot(allregsshadesub, aes(x=SoilT.dif, y=mean.dif, group=StemID, colour=LDStatus)) + theme_bw() + theme(legend.position="none") + 
  geom_point(aes(alpha=0.3)) + 
  stat_smooth(method="lm") +
  geom_text(data=lmShade, aes(x=-10, y=ypos, label=Shadeest, size=0.2), vjust=1, parse=TRUE, show.legend=F)

shadeplots <- arrangeGrob(Shade.plot, ShadeLD.plot, ncol=2)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/shade_plots.png", shadeplots,  width=20, height=6, units="in", dpi=300)
print(Shade.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/shade_plot.png",  width=10, height=6, units="in", dpi=300)






lmStemHum$ypos <- seq(1, -1, length.out=length(unique(lmStemHum$StemID)))
lmStemHum <- lmStemHum[lmStemHum$pval<0.05,]

lmWigRH <- lm(Wiggle.resid ~ 0 + RH.resid + LDStatus, data=allresids)
summary(lmWigRH)
lmWigAirT <- lm(Wiggle.resid ~ AirT.resid, data=allresids)
summary(lmWigAirT)
lmWigPAR <- lm(Wiggle.resid ~ PAR.resid, data=allresids)
summary(lmWigPAR)
lmWigStemWP <- lm(Wiggle.resid ~ 0 + StemWP.resid + LDStatus, data=allresids)
summary(lmWigStemWP)


# Combine subsets of data to make regression plots

# Soil Water Content and Soil Water Potential
SWC_CRpsy.plot <- ggplot(SWC_CRpsy, aes(x=SWC, y=SoilWaterPot, group=Depth, colour=Depth)) + geom_point(aes(alpha=0.3)) + theme_bw()


# Stem YPos and humidity

lmStemHum <- ddply(Stem_Hum, c("StemID", "LDStatus"), function(x)
  data.frame(est = cor.test(x$RH, x$mean.dif)$estimate,
             pval = cor.test(x$RH, x$mean.dif)$p.value))
lmStemHum$est <- paste("R^2 == ", round(lmStemHum$est, digits=2))
lmStemHum$ypos <- seq(1, -1, length.out=length(unique(lmStemHum$StemID)))
lmStemHum <- lmStemHum[lmStemHum$pval<0.05,]

Stem_Hum_StemID.plot <- ggplot(Stem_Hum, aes(x=RH, y=mean.dif, group=StemID, colour=StemID)) + geom_point(aes(alpha=0.3)) + stat_smooth(method="lm") + theme_bw() +
  geom_text(data=lmStemHum, aes(x=4, y=ypos, label=est, size=0.2),
          vjust=1, parse=TRUE, show.legend=F)

print(Stem_Hum_StemID.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/HumidStemID_plot.png", width=10, height=6, units="in", dpi=300)

Stem_Hum_LD.plot <- ggplot(Stem_Hum, aes(x=RH, y=mean.dif, group=StemID, colour=LDStatus)) + geom_point(aes(alpha=0.3)) + stat_smooth(method="lm") + theme_bw() +
  geom_text(data=lmStemHum, aes(x=4, y=ypos, label=est, size=0.2),
            vjust=1, parse=TRUE, show.legend=F)

print(Stem_Hum_LD.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/HumidStemLD_plot.png", width=10, height=6, units="in", dpi=300)

# Stem YPos and stem water potential

lmStemStem <- ddply(Stem_Stem, c("StemID", "LDStatus"), function(x)
  data.frame(est = cor.test(x$StemWaterPot, x$mean.dif)$estimate,
             pval = cor.test(x$StemWaterPot, x$mean.dif)$p.value))
lmStemStem$est <- paste("R^2 == ", round(lmStemStem$est, digits=2))
lmStemStem$ypos <- seq(1, -1, length.out=length(unique(lmStemStem$StemID)))
lmStemStem <- lmStemStem[lmStemStem$pval<0.05,]

Stem_StemstemID.plot <- ggplot(Stem_Stem, aes(x=StemWaterPot, y=mean.dif, group=StemID, colour=StemID)) + geom_point(aes(alpha=0.3)) + stat_smooth(method="lm") + theme_bw() +
  geom_text(data=lmStemStem, aes(x=-0.5, y=ypos, label=est, size=0.2),
            vjust=1, parse=TRUE, show.legend=F)

print(Stem_StemstemID.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/StemStemID_plot.png", width=10, height=6, units="in", dpi=300)

Stem_StemLDstat.plot <- ggplot(Stem_Stem, aes(x=StemWaterPot, y=mean.dif, group=StemID, colour=LDStatus)) + geom_point(aes(alpha=0.3)) + stat_smooth(method="lm") + theme_bw() +
  geom_text(data=lmStemStem, aes(x=-0.5, y=ypos, label=est, size=0.2),
            vjust=1, parse=TRUE, show.legend=F)

print(Stem_StemLDstat.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/StemStemLD_plot.png", width=10, height=6, units="in", dpi=300)


# Stem YPos and soil water potential
subsoil <- subset(zomdathr, (timestamp > start.date & timestamp < end.date) & (Pit == "Open3" | Pit == "Shrub2" | Pit =="Shrub3") & !is.na(Depth), select = c(timestamp, Pit, Depth, SoilWaterPot))
Stem_Soil <- unique(merge(Wigglesub[,c("timestamp", "YPosition", "StemID", "LDStatus", "PercentY", "mean.dif")], subsoil,
                          by = c("timestamp")))
lmStemSoil <- ddply(Stem_Soil, c("StemID", "LDStatus"), function(x)
  data.frame(est = cor.test(x$SoilWaterPot, x$mean.dif)$estimate,
             pval = cor.test(x$SoilWaterPot, x$mean.dif)$p.value))
lmStemSoil$est <- paste("R^2 == ", round(lmStemSoil$est, digits=2))
lmStemSoil$ypos <- seq(1, -1, length.out=length(unique(lmStemSoil$StemID)))
lmStemSoil <- lmStemSoil[lmStemSoil$pval<0.05,]

Stem_Soil.plot <- ggplot(Stem_Soil, aes(x=SoilWaterPot, y=mean.dif, group=StemID, colour=StemID)) + geom_point(aes(alpha=0.3)) + stat_smooth(method="lm") + theme_bw() +
  #geom_text(data=lmStemSoil, aes(x=1.5, y=ypos, label=est, size=0.2),
  #          vjust=1, parse=TRUE, show.legend=F) +
  facet_grid(Depth~Pit)

print(Stem_Soil.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/StemSoil_plot.png", width=10, height=6, units="in", dpi=300)

