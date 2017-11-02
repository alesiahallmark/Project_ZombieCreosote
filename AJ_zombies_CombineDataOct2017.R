# This script combines data files for AJ's zombie creosote project
# This version of the script was used to create figures for the final paper

### Needed: 
# Flux_all files from all years plus latest wireless download from both dataloggers
# CR7 DATA files plus lastest wireless download
# ICT files
# .csv files of manually tracked branch movements

### Before beginning, run AJ_FluxProcess code and shrubCR7_dataprocess code. These will use most updated wireless files, convert SWC and use calibration factors to correct psychrometer values

# Load required libraries
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(gridExtra)
library(reshape2)
library(lubridate)
library(dplyr)
library(plyr)
library(zoo)
library(scales)

# Make MasterMerge df of all date-times
MasterMerge <- data.frame(seq.POSIXt(from = ISOdatetime(2010,1,1,0,0,0), to = Sys.time(), by = "1 hour"))
colnames(MasterMerge) <- c("timestamp")

# Read in processed Ameriflux, flux_all, and wireless data files
file.loc <- '/Users/alesia/Documents/data_AJ_output/'
list.flux.files <- list.files(file.loc, pattern='fluxall')
list.flux.files <- paste(file.loc, list.flux.files, sep = '')

mergeflux <- read.csv(list.flux.files[2], header=T, strip.white=T, sep=",")
mergeflux$timestamp <- as.POSIXct(strptime(mergeflux$timestamp, format="%Y-%m-%d %H:%M:%S"))

# Read in file with GPP and RE
file.loc <- '/Users/alesia/Documents/data_FTP_raw/'
list.flux.files2015 <- intersect(
  list.files(file.loc, pattern='US-Ses'),
  list.files(file.loc, pattern='2015_gapfilled'))
list.flux.files2015 <- paste(file.loc, list.flux.files2015, sep = '')

GPPfile <- read.csv(list.flux.files2015[1], header=T, strip.white=T)
GPPfile$timestamp <- as.POSIXct(strptime(GPPfile$TIMESTAMP, format= "%Y%m%d%H%M%S"))
GPPfile <- GPPfile[,c("timestamp", "GPP", "RECO")]
colnames(GPPfile) <- c("timestamp", "GPP", "RE")

# Aggregate by hour and merge
GPPfile$timestamp <- as.factor(as.character(round(GPPfile$timestamp, "hour")))
GPPfile <- aggregate(cbind(GPP, RE)~timestamp, data = GPPfile, sum, na.rm=T, na.action="na.pass")
GPPfile$timestamp <- as.POSIXct(strptime(GPPfile$timestamp, format= "%Y-%m-%d %H:%M:%S"))
mergeflux <- merge(mergeflux, GPPfile, by="timestamp", all=T)

# Aggregate by hour and merge
mergeflux$timestamp <- as.factor(as.character(round(mergeflux$timestamp, "hour")))

mergeflux <- aggregate(cbind(air.temp, RH, VPD, precip, LW_IN, RNET, wind.speed, air.press, PAR, LW_OUT, FC, GPP, RE, LE, SWC_O1_2p5, SWC_O1_12p5, SWC_O1_22p5, SWC_O1_37p5, SWC_O1_52p5, SWC_S1_2p5, SWC_S1_12p5, SWC_S1_22p5, SWC_S1_37p5, SWC_S1_52p5, SWC_O2_2p5, SWC_O2_12p5, SWC_O2_22p5, SWC_O2_37p5, SWC_O2_52p5, SWC_S2_2p5, SWC_S2_12p5, SWC_S2_22p5, SWC_S2_37p5, SWC_S2_52p5, SoilT_O1_2p5, SoilT_O1_12p5, SoilT_O1_22p5, SoilT_O1_37p5, SoilT_O1_52p5, SoilT_S1_2p5, SoilT_S1_12p5, SoilT_S1_22p5, SoilT_S1_37p5, SoilT_S1_52p5, SoilT_O2_2p5, SoilT_O2_12p5, SoilT_O2_22p5, SoilT_O2_37p5, SoilT_O2_52p5, SoilT_S2_2p5, SoilT_S2_12p5, SoilT_S2_22p5, SoilT_S2_37p5, SoilT_S2_52p5, MPS6_O3_15_P, MPS6_O3_15_T, MPS6_O3_22p5_P, MPS6_O3_22p5_T, MPS6_O3_37p5_P, MPS6_O3_37p5_T, MPS6_S1_15_P, MPS6_S1_15_T, MPS6_S1_22p5_P, MPS6_S1_22p5_T, MPS6_S1_37p5_P, MPS6_S1_37p5_T, MPS6_S2_15_P, MPS6_S2_15_T, MPS6_S2_22p5_P, MPS6_S2_22p5_T, MPS6_S2_37p5_P, MPS6_S2_37p5_T, MPS6_S3_15_P, MPS6_S3_15_T, MPS6_S3_22p5_P, MPS6_S3_22p5_T, MPS6_S3_37p5_P, MPS6_S3_37p5_T)~timestamp, data = mergeflux, mean, na.rm=T, na.action="na.pass")

mergeflux$timestamp <- as.POSIXct(strptime(mergeflux$timestamp, format="%Y-%m-%d %H:%M:%S"))

mergeflux <- merge(MasterMerge, mergeflux, by="timestamp", all=T)

# Read in CR7 and ICT data
file.loc <- '/Users/alesia/Documents/data_AJ_output/'
list.CRsoil.files <- list.files(file.loc, pattern='soil_psy')
list.CRstem.files <- list.files(file.loc, pattern='allstem_psy')
list.CRsoil.files <- paste(file.loc, list.CRsoil.files, sep = '')
list.CRstem.files <- paste(file.loc, list.CRstem.files, sep = '')

CRsoil <- read.csv(list.CRsoil.files[1], header=T, strip.white=T)
CRstem <- read.csv(list.CRstem.files[1], header=T, strip.white=T)

CRsoil <- subset(CRsoil, select = c(TimeStamp, WaterPot, Pit, Depth))
CRstem <- subset(CRstem, select = c(TimeStamp, SensorName, WaterPot))

colnames(CRsoil) <- c("timestamp", "SoilWaterPot", "Pit", "Depth")
colnames(CRstem) <- c("timestamp", "StemSensor", "StemWaterPot")

# Format timestamps and round to nearest hour
CRsoil$timestamp <- as.POSIXct(strptime(CRsoil$timestamp, format="%Y-%m-%d %H:%M:%S"))
CRstem$timestamp <- as.POSIXct(strptime(CRstem$timestamp, format="%Y-%m-%d %H:%M:%S"))
CRsoil$timestamp <- round(CRsoil$timestamp, "hour")
CRstem$timestamp <- round(CRstem$timestamp, "hour")

CRsoil$SoilWaterPot[CRsoil$SoilWaterPot < -25 | CRsoil$SoilWaterPot > 1] <- NA
CRstem$StemWaterPot[CRstem$StemWaterPot < -15 | CRstem$StemWaterPot > 1] <- NA

# Aggregate by hour and merge
CRsoil$year <- as.factor(year(CRsoil$timestamp))
CRsoil$yday <- as.factor(yday(CRsoil$timestamp))
CRsoil$hour <- as.factor(hour(CRsoil$timestamp))
CRstem$year <- as.factor(year(CRstem$timestamp))
CRstem$yday <- as.factor(yday(CRstem$timestamp))
CRstem$hour <- as.factor(hour(CRstem$timestamp))
CRsoil <- aggregate(SoilWaterPot~year+yday+hour+Pit+Depth, data=CRsoil, mean, na.rm=T, na.action="na.pass")
CRstem <- aggregate(StemWaterPot~year+yday+hour+StemSensor, data=CRstem, mean, na.rm=T, na.action="na.pass")

mergeCR <- merge(CRsoil, CRstem, by=c("year", "yday", "hour"), all=T)
mergeCR$timestamp <- as.POSIXct(strptime(paste(mergeCR$year, mergeCR$yday, mergeCR$hour), format="%Y %j %H"))
mergeCR <- mergeCR[,c("timestamp", "Pit", "Depth", "SoilWaterPot", "StemSensor", "StemWaterPot")]

mergeCR <- merge(MasterMerge, mergeCR, by="timestamp", all=T)

# Read in Stem Movement file
file.loc <- '/Users/alesia/Documents/Project_ZombieCreosote/BranchTracker/'
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


# write out file of all times
# write.csv(mergezom, '/Users/alesia/Documents/ZombieCreosote/ZombieBranches2.csv', row.names=F)

# Now merge everything
allzomdat <- merge(mergeflux, mergeCR, by="timestamp", all=T)
allzomdat <- merge(allzomdat, mergezom, by="timestamp", all=T)
#allzomdat <- merge(allzomdat, mergepics, by="timestamp", all=T)
allzomdat$date <- date(allzomdat$timestamp)

# VPD
es <- 6.1078 * exp((17.269 * allzomdat$air.temp )/(237.3 + allzomdat$air.temp))
allzomdat$VPD <- es - ( allzomdat$RH * es / 100 )
allzomdat$VPD <- allzomdat$VPD / 10
allzomdat$VPD[is.infinite(allzomdat$VPD)] <- NA

# Savepoint
keep.cols <- 
  names(allzomdat[,c("timestamp", "wind.speed", "RH", "VPD",
                     "precip", "air.temp", "FC", "GPP", "RE", 
                     "LE", "air.press",
                     "PAR", "LW_OUT", "LW_IN", "RNET",
                     "Pit", "Depth", "SoilWaterPot", 
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

# Savepoint!
write.csv(allzomdat, file="/Users/alesia/Documents/data_AJ_output/allzomdat5Mar17.csv", row.names=F)

# remove(CRsoil, CRstem, list.pics, mergeCR, mergeflux, mergepics, mergezom, MasterMerge)

# allzomdat <- read.csv("/Users/alesia/Documents/data_AJ_output/allzomdat20Nov2016.csv", header=T, strip.white=T)
allzomdat$timestamp <- as.POSIXct(strptime(allzomdat$timestamp, format="%Y-%m-%d %H:%M:%S"))
allzomdat$date <- as.Date(allzomdat$timestamp)

zomdathr <- allzomdat
zomdathr$date <- as.Date(trunc(zomdathr$timestamp, units="day"))

# Create shiftdate column
zomdathr$shifttime <- zomdathr$timestamp + hours(12)
zomdathr$shiftdate <- as.Date(trunc(zomdathr$shifttime, units="day"))

zomdathr$Pit <- as.character(zomdathr$Pit)
zomdathr$Pit[grep("Open", zomdathr$Pit)] <- "Open3"
zomdathr$Pit <- as.factor(zomdathr$Pit)

# Crop to the desired time periods
zomdathr <- subset(zomdathr, (zomdathr$timestamp > as.POSIXct(strptime("2015-07-15", format="%Y-%m-%d")) & zomdathr$timestamp < as.POSIXct(strptime("2015-12-31", format="%Y-%m-%d"))))


# Melt SWC, SoilTemp, SoilPsy, StemPsy
# Aggregate into daily values first if needed
# Humidity
RHsub <- unique(subset(zomdathr, select = c(timestamp, date, shiftdate, RH)))
RHsub <- RHsub[with(RHsub, order(timestamp)),]

minmaxmean.date <- ddply(RHsub, c("date"), function(x)
  data.frame(minRH = min(x$RH, na.rm=T),
             minRH.time = x$timestamp[x$RH == min(x$RH, na.rm=T)],
             meanRH = mean(x$RH, na.rm=T),
             deltaRH = max(x$RH, na.rm=T) - min(x$RH, na.rm=T)))
minmaxmean.shiftdate <- ddply(RHsub, c("shiftdate"), function(x)
  data.frame(maxRH = max(x$RH, na.rm=T),
             maxRH.time = x$timestamp[x$RH == max(x$RH, na.rm=T)]))

RHsub <- merge(RHsub, minmaxmean.date, by='date', all=T)
RHsub <- merge(RHsub, minmaxmean.shiftdate, by='shiftdate', all=T)

# PAR
PARsub <- unique(subset(zomdathr, select = c(timestamp, date, shiftdate, PAR)))
PARsub <- PARsub[with(PARsub, order(timestamp)),]
PARsub <- PARsub[!is.na(PARsub$timestamp),]

minmaxmean <- ddply(PARsub, c("date"), function(x)
  data.frame(sunrise = min(x$timestamp[hour(x$timestamp) > 2 & x$PAR > 50]),
             highnoon = x$timestamp[x$PAR == max(x$PAR, na.rm=T)],
             sunset = max(x$timestamp[x$PAR > 35]),
             sumPAR = sum(x$PAR, na.rm=T),
             deltaPAR = max(x$PAR, na.rm=T) - min(x$PAR, na.rm=T)))

minmaxmean$daylength <- hour(minmaxmean$sunset) - hour(minmaxmean$sunrise)
PARsub <- merge(PARsub, minmaxmean, by='date', all=T)


# Air Temp
AirTsub <- unique(subset(zomdathr, select = c(timestamp, date, shiftdate, air.temp)))
AirTsub <- AirTsub[with(AirTsub, order(timestamp)),]

minmaxmean.date <- ddply(AirTsub, c("date"), function(x)
  data.frame(sumAirT = sum(x$air.temp, na.rm=T),
             maxAirT = max(x$air.temp, na.rm=T),
             maxAirT.time = mean(x$timestamp[x$air.temp == max(x$air.temp, na.rm=T)], na.rm=T),
             deltaAirT = max(x$air.temp, na.rm=T) - min(x$air.temp, na.rm=T)))

minmaxmean.shiftdate <- ddply(AirTsub, c("shiftdate"), function(x)
  data.frame(minAirT = min(x$air.temp, na.rm=T),
             minAirT.time = mean(x$timestamp[x$air.temp == min(x$air.temp, na.rm=T)], na.rm=T)))

AirTsub <- merge(AirTsub, minmaxmean.date, by='date', all=T)
AirTsub <- merge(AirTsub, minmaxmean.shiftdate, by='shiftdate', all=T)

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

SWCsub <- aggregate(SWC ~ pit.tag + date, data=SWCsub, mean, na.rm=T, na.action="na.pass")

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
SWCsub$SWC[SWCsub$Pit=="Shrub1" & SWCsub$Depth=="02.5cm"] <- SWCsub$SWC[SWCsub$Pit=="Shrub1" & SWCsub$Depth=="02.5cm"] + 0.05

SWCsubmean <- aggregate(SWC ~ date + Depth, data = SWCsub, mean, na.rm=T, na.action="na.pass")
colnames(SWCsubmean)[3] <- "meanSWC" 
SWCsub <- merge(SWCsub, SWCsubmean, by = c("date", "Depth"), all=T)
  
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
SoilTsub <- aggregate(SoilTemp ~., data = SoilTsub, mean, na.rm=T, na.action="na.pass")

SoilTsub$Cover <- as.character(SoilTsub$Pit)
SoilTsub$Cover[grep("Open", SoilTsub$Cover)] <- "Open"
SoilTsub$Cover[grep("Shrub", SoilTsub$Cover)] <- "Shrub"
SoilTsub$Cover <- as.factor(SoilTsub$Cover)

SoilTdif <- subset(SoilTsub, select=c(timestamp, Cover, Depth, SoilTemp))
SoilTdif <- aggregate(SoilTemp ~., data = SoilTdif, mean, na.rm=T, na.action="na.pass")
SoilTdif <- dcast(SoilTdif, timestamp + Depth ~ Cover)
SoilTdif$SoilT.dif <- SoilTdif$Shrub - SoilTdif$Open
SoilTdif$SoilT.dif[SoilTdif$SoilT.dif>=8] <- NA
SoilTsub <- merge(SoilTsub, SoilTdif[,c("timestamp", "Depth", "SoilT.dif")], by=c("timestamp", "Depth"), all=T)
  

# Stem Water Potential
StemWPsub <- unique(subset(zomdathr, select=c(timestamp, StemWaterPot, StemSensor)))
StemWPsub <- StemWPsub[!is.na(StemWPsub$StemWaterPot),]
StemWPsub$StemWaterPot[StemWPsub$StemWaterPot>0] <- 0

StemWPsubone <- StemWPsub[StemWPsub$StemSensor=="ICT1809",]
StemWPsubtwo <- subset(StemWPsub, StemSensor=="Dixon4" & timestamp < as.POSIXct(strptime("2015-08-15", format="%Y-%m-%d")))
StemWPsub <- rbind(StemWPsubone, StemWPsubtwo)
StemWPsubmorn <- subset(StemWPsub, hour(timestamp) >= 0 & hour(timestamp) <= 3)
StemWPsubmorn$date <- date(StemWPsubmorn$timestamp)
StemWPsubmorn <- aggregate(StemWaterPot ~ date, data=StemWPsubmorn, mean, na.rm=T, na.action="na.pass")
StemWPsub
StemWPsubmorn$StemWPmornsm <- rollapply(StemWPsubmorn$StemWaterPot, 5, mean, fill=NA)
StemWPsubmorn <- StemWPsubmorn[with(StemWPsubmorn, order(date)),]
StemWPsub <- StemWPsub[with(StemWPsub, order(timestamp)),]
StemWPsub$StemWPsm <- rollapply(StemWPsub$StemWaterPot, 11, mean, fill=NA)
StemWPsub$date <- date(StemWPsub$timestamp)
StemWPsub <- unique(merge(StemWPsub, StemWPsubmorn[,c("date", "StemWPmornsm")], by="date", all=T))

minmaxmean <- unique(ddply(StemWPsub, c("date"), function(x)
  data.frame(minStemWP = min(x$StemWPsm, na.rm=T),
             minStemWP.time = x$timestamp[x$StemWPsm == 
                                            min(x$StemWPsm, na.rm=T)],
             maxStemWP = max(x$StemWPsm, na.rm=T),
             maxStemWP.time = x$timestamp[x$StemWPsm == 
                                            max(x$StemWPsm, na.rm=T)],
             deltaStemWP = max(x$StemWPsm, na.rm=T) -
                            min(x$StemWPsm, na.rm=T))))
StemWPsub <- unique(merge(StemWPsub, minmaxmean, by='date', all=T))



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
allsoilpsy <- aggregate(SoilWaterPot ~ date + Pit + Depth + PsyType, data=allsoilpsy, mean, na.rm=T, na.action="na.pass")

allsoilpsy$Cover <- as.character(allsoilpsy$Pit)
allsoilpsy$Cover[grep("Open", allsoilpsy$Cover)] <- "Open"
allsoilpsy$Cover[grep("Shrub", allsoilpsy$Cover)] <- "Shrub"
allsoilpsy$Cover <- as.factor(allsoilpsy$Cover)

soilpsymeans <- aggregate(SoilWaterPot ~ date + Depth + Cover + PsyType, data = allsoilpsy, mean, na.rm=T, na.action="na.pass")
colnames(soilpsymeans)[5] <- "SoilWP.covmean"

allsoilpsy <- merge(allsoilpsy, soilpsymeans, by = c("date", "Depth", "Cover", "PsyType"), all=T)


# Stem Movement Files
stemcols <- intersect(c(names(zomdathr[,grep("z1", names(zomdathr))]), names(zomdathr[,grep("z2", names(zomdathr))]), names(zomdathr[,grep("z3", names(zomdathr))])), names(zomdathr[,grep("y", names(zomdathr))]))
# Recast to get rid of weird NA columns
Wigglesub <- unique(subset(zomdathr, select=c("timestamp", stemcols)))
Wigglesub <- dcast(melt(Wigglesub, id.vars='timestamp', na.rm=T), timestamp~variable, mean)

# Fill, Smooth, Fill
firstbranchcolumn <- min(grep ("z", names(Wigglesub)))
lastbranchcolumn <- max(grep ("z", names(Wigglesub)))
Wigglesub <- Wigglesub[with(Wigglesub, order(timestamp)),]

# Look through plots and remove outliers
for (i in firstbranchcolumn:lastbranchcolumn) {
  plot(Wigglesub$timestamp, Wigglesub[,i],
       xlim = c(min(Wigglesub$timestamp[!is.na(Wigglesub[,i])]),
                max(Wigglesub$timestamp[!is.na(Wigglesub[,i])])),
       main = colnames(Wigglesub)[i])
}
plot(Wigglesub$timestamp, Wigglesub[,"z1_4y"],
     xlim = c(as.POSIXct("2015-08-08 12:00:00"),
              as.POSIXct("2015-08-09 00:00:00")))
c(min(Wigglesub$timestamp[!is.na(Wigglesub[,"z1_4y"])]),
  max(Wigglesub$timestamp[!is.na(Wigglesub[,"z1_4y"])]))
# Outliers
Wigglesub[Wigglesub$timestamp==as.POSIXct("2015-08-08 15:00:00"),"z2_4y"]
Wigglesub$z1_4y[Wigglesub$z1_4y>1085] <- NA
Wigglesub$z1_5y[Wigglesub$z1_5y>1120 | Wigglesub$z1_5y<1075] <- NA
Wigglesub$z2_1y[Wigglesub$z2_1y<1095] <- NA
Wigglesub$z2_2y[Wigglesub$z2_2y>1330 | Wigglesub$z2_2y<1295] <- NA
Wigglesub$z2_4y[Wigglesub$z2_4y>1320] <- NA

# Fill in timestamp column. merge to each branch within Wigglesub
alltimes <- data.frame(timestamp =
                         seq.POSIXt(from=as.POSIXct("2015-07-31 00:00:00"),
                                    to=as.POSIXct("2015-12-21 00:00:00"), 
                                    by="hour"))
Wigglesub <- merge(Wigglesub, alltimes, by="timestamp", all=T)
# Fill NA's using a spline method
for (i in firstbranchcolumn:lastbranchcolumn) {
  Wigglesub[,i] <- na.spline(Wigglesub[,i], Wigglesub$timestamp, na.rm=F, maxgap = 5) }  
# Smooth data using a median filter
for (i in firstbranchcolumn:lastbranchcolumn) {
  Wigglesub[,i] <- rollapply(Wigglesub[,i], 5, median, fill=NA)
  plot(Wigglesub$timestamp, Wigglesub[,i],
       xlim = c(min(Wigglesub$timestamp[!is.na(Wigglesub[,i])]),
                max(Wigglesub$timestamp[!is.na(Wigglesub[,i])])),
       main = colnames(Wigglesub)[i])
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

# Look at each branch and remove outliers

# Find difference from mean, % total movement, derivative
means <- ddply(Wigglesub, c("StemID"), function(x)
  data.frame(stem.mean = mean(x$YPosition, na.rm=T),
             stem.maxdif = max(
               (max(x$YPosition, na.rm=T) - 
                  mean(x$YPosition, na.rm=T)),
               (mean(x$YPosition, na.rm=T) - 
                  min(x$YPosition, na.rm=T))),
             perc98 = quantile(x$YPosition, probs=0.98, na.rm=T),
             perc2 = quantile(x$YPosition, probs=0.02, na.rm=T)))
Wigglesub <- merge(Wigglesub, means, by='StemID', all=T)
Wigglesub$mean.dif <- (Wigglesub$YPosition - Wigglesub$stem.mean) /
  (Wigglesub$stem.maxdif)
BranchZs <- ddply(Wigglesub, c("StemID"), function(x)
  data.frame(YPosition = x$YPosition,
             timestamp = x$timestamp, 
             BranchPosZ = scale(x$YPosition, center=T, scale=T)))
Wigglesub <- merge(Wigglesub, BranchZs, by=c("StemID", "YPosition", "timestamp"), all=T)
Wigglesub$PercentY <- (Wigglesub$YPosition - Wigglesub$perc2) / (Wigglesub$perc98 - Wigglesub$perc2)


Wigglesub$PercentY[Wigglesub$PercentY > 1] <- 1
Wigglesub$PercentY[Wigglesub$PercentY < 0] <- 0

# Only look at zombie1 branches for now
Wigglesub <- Wigglesub[grep("z1", Wigglesub$StemID),]

# Delta, mean, min, and max movement per day
Wigglesub$date <- as.Date(trunc(Wigglesub$timestamp, unit="day"))
deltastem <- ddply(Wigglesub[grep("z1", Wigglesub$StemID),], c("date", "StemID"), function(x)
  data.frame(mean.stem = mean(x$mean.dif, na.rm=T),
             min.stem = min(x$mean.dif, na.rm=T),
             minstem.time = x$timestamp[x$mean.dif == min(x$mean.dif, na.rm=T)],
             max.stem = max(x$mean.dif, na.rm=T),
             maxstem.amtime = mean(x$timestamp[
               x$mean.dif == max(x$mean.dif, na.rm=T)]),
             delta.stem = max(x$mean.dif, na.rm=T) - min(x$mean.dif, na.rm=T)))
Wigglesub<- merge(Wigglesub, deltastem, by=c("date", "StemID"), all=T)

# Calculate all-stem averages
allstems <- ddply(Wigglesub, c("timestamp"), function(x)
  data.frame(mean.dif.all = mean(x$mean.dif, na.rm=T)))
LDstems <- ddply(Wigglesub, c("timestamp", "LDStatus"), function(x)
  data.frame(mean.dif.LD = mean(x$mean.dif, na.rm=T),
             sd.dif.LD = var(x$mean.dif, na.rm=T),
             mean.z.LD = mean(x$BranchPosZ, na.rm=T),
             sd.z.LD = var(x$BranchPosZ, na.rm=T)))

Wigglesub<- merge(Wigglesub, allstems, by=c("timestamp"), all=T)
Wigglesub<- merge(Wigglesub, LDstems, by=c("timestamp", "LDStatus"), all=T)




################################################
# Plots
#summary(longzomdat$timestamp)
start.date <- as.POSIXct(strptime("2015-07-31 00:00:00", format="%Y-%m-%d %H:%M:%S"))
end.date <- as.POSIXct(strptime("2015-12-05 00:00:00", format="%Y-%m-%d %H:%M:%S"))
tenday.start <- as.POSIXct(strptime("2015-10-09 00:00:00", format="%Y-%m-%d %H:%M:%S"))
tenday.end <- as.POSIXct(strptime("2015-10-14 00:00:00", format="%Y-%m-%d %H:%M:%S"))

stems.plot.week <- ggplot(Wigglesub[Wigglesub$StemID=="z1_1" | Wigglesub$StemID=="z1_2" | Wigglesub$StemID=="z1_5",], aes(x=timestamp, y=YPosition, group=StemID, colour=StemID)) +
  geom_point() + geom_line() + theme_bw(base_size=20) + 
  theme(legend.position="none") +
  ylab("y-coordinate") + xlab("Time") +
  xlim(tenday.start, tenday.end) 

stems.plot.mean.week <- ggplot(Wigglesub[Wigglesub$StemID=="z1_1" | Wigglesub$StemID=="z1_2" | Wigglesub$StemID=="z1_5",], aes(x=timestamp, y=mean.dif, group=StemID, colour=StemID)) +
  geom_point(alpha=0.5) + geom_line(alpha=0.8) + theme_bw(base_size=20) + 
  geom_point(colour="black", aes(y=mean.dif.all)) + 
  geom_line(colour="black", aes(y=mean.dif.all)) +
  theme(legend.position="none") +
  ylab("Branch Position") + xlab("Time") +
  xlim(tenday.start, tenday.end) 


stems.plot.mean <- ggplot(unique(Wigglesub[,c("timestamp", "LDStatus", "StemID", "mean.dif", "mean.dif.LD", "mean.z.LD", "sd.z.LD", "BranchPosZ", "min.stem")]), aes(x=timestamp, y=BranchPosZ, group=StemID, colour=LDStatus)) +
  theme_bw(base_size=20) + 
  geom_point(alpha=0.1) + geom_line(alpha=0.8) + 
  geom_point(aes(y=mean.z.LD, colour=LDStatus), alpha=0.32, size=1.1) + 
#  geom_ribbon(aes(x=timestamp, ymin=mean.z.LD-sd.z.LD, ymax=mean.z.LD+sd.z.LD, fill=LDStatus, colour=NULL), alpha=0.3) +
  geom_line(aes(y=mean.z.LD, colour=LDStatus), alpha=0.32, size=1) + 
  scale_color_manual(labels = c("Dead", "Live"), values=c("#a65628", "#4daf4a")) +
  scale_fill_manual(labels = c("Dead", "Live"), values=c("#a65628", "#4daf4a")) +
  theme(legend.position="none") +
  ylab("Branch Position") + xlab("Time") +
#  xlim(start.date, end.date) 
  xlim(as.POSIXct("2015-11-30 00:00:00"), as.POSIXct("2015-12-30 00:00:00")) 
stems.plot.mean

SWC.plot.mod <- ggplot(SWCsub, aes(x=date, y=SWC, group=Depth, colour=Depth)) + #facet_grid(Pit~.) +
  geom_point() + geom_line() + theme_bw(base_size=20) + facet_grid(Pit~.) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(date(start.date), date(end.date)) 

SWCall.plot <- ggplot(SWCsub, aes(x=date, y=meanSWC, group=Depth, colour=Depth)) +
  geom_point() + geom_line() + theme_bw(base_size=20) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlab("Time") + ylab("Soil Water Content") +
  xlim(date(start.date), date(end.date)) 

StemWP.plot <- ggplot(StemWPsub, aes(x=timestamp, y=StemWPsm)) +
  geom_line(colour="dark green") + theme_bw(base_size=20) +
  theme(legend.position="none") +
  xlab("Time") + ylab("Stem Water Potential (MPa)") +
  xlim(start.date, end.date)

humid.plot <- ggplot(RHsub, aes(x=timestamp, y=RH)) +
  geom_point(colour="dark blue", alpha=0.5) + 
  theme_bw(base_size=20) + theme(legend.position="none") +
  xlab("Time") + ylab("Relative Humidity (%)") +
  xlim(start.date, end.date)

VPDsub <- unique(subset(zomdathr, select = c(timestamp, VPD)))
VPDsub <- VPDsub[with(VPDsub, order(timestamp)),]
VPD.plot <- ggplot(VPDsub, aes(x=timestamp, y=VPD)) +
  geom_point(colour="purple4", alpha=0.5) + theme_bw(base_size=20) +
  theme(legend.position="none") +
  xlab("Time") + ylab("VPD") +
  xlim(start.date, end.date)

airPsub <- unique(subset(zomdathr, select = c(timestamp, air.press)))
airPsub <- airPsub[with(airPsub, order(timestamp)),]
airP.plot <- ggplot(airPsub, aes(x=timestamp, y=air.press)) +
  geom_point(colour="coral4", alpha=0.5) + theme_bw(base_size=20) +
  theme(legend.position="none") +
  xlab("Time") + ylab("Air Pressure") +
  xlim(start.date, end.date)

PAR.plot <- ggplot(PARsub, aes(x=timestamp, y=PAR)) +
  geom_point(alpha=0.2, colour="orange") + theme_bw(base_size=20) +
  geom_line(aes(y=deltaPAR), colour="tomato2") +
  theme(legend.position="none") +
  xlab("Time") + ylab("Incoming Light (µE m-2 s-1)") +
  xlim(start.date, end.date)

AirT.plot <- ggplot(AirTsub, aes(x=timestamp, y=air.temp)) +
  geom_point(colour="dark red", aes(alpha=0.3)) + theme_bw(base_size=20) + 
  theme(legend.position="none") +
  xlab("Time") + ylab(expression("Air Temperature " ( degree*C))) +
  xlim(start.date, end.date)

GPPsub <- unique(subset(zomdathr, select = c(timestamp, date, GPP)))
GPPsub <- GPPsub[with(GPPsub, order(timestamp)),]
GPP.plot <- ggplot(GPPsub, aes(x=timestamp, y=GPP)) +
  geom_point(colour="dark blue", alpha=0.5) + theme_bw(base_size=20) +
  theme(legend.position="none") +
  xlab("Time") + ylab("GPP") +
  xlim(start.date, end.date)

SoilT.plot <- ggplot(SoilTsub, aes(x=timestamp, y=SoilTemp, group=Depth, colour=Depth)) + 
  geom_line() + theme_bw(base_size=20) +
  facet_grid(Cover~.) + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(start.date, end.date)

SoilTdif.plot <- ggplot(SoilTsub[SoilTsub$Depth=="02.5cm",], aes(x=timestamp, y=SoilT.dif, group=Depth, colour=SoilT.dif)) + 
  geom_point(alpha=0.3) + geom_line(alpha=0.7) + 
  geom_hline(aes(yintercept=0)) +
  stat_smooth(method="lm") +
  theme_bw(base_size=20) + facet_grid(Depth~.) +
  theme(legend.position="none") +
  scale_colour_gradient(limits=c(-11,11),high="red",low="blue") +
  xlab("Time") + ylab(expression("Soil Temperature Difference" ( degree*C))) +
  xlim(start.date, end.date)

### 

print(humid.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/humid_plot.png", width=10, height=6, units="in", dpi=300)

print(VPD.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/VPD_plot.png", width=10, height=6, units="in", dpi=300)

print(PAR.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/PAR_plot.png", width=10, height=6, units="in", dpi=300)

print(AirT.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/AirT_plot.png", width=10, height=6, units="in", dpi=300)

print(stems.plot.week)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_week.png", width=4.5, height=2.8, units="in", dpi=300)

print(stems.plot.mean.week)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_mean_week.png", width=4.5, height=2.8, units="in", dpi=300)

print(stems.plot.mean)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_plot_mean.png", width=6.5, height=5, units="in", dpi=300)

print(stems.plot.dailymax)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_plot_dailymax.png", width=10, height=6, units="in", dpi=300)

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


# Merge all dataframes for regressions
allregs <- unique(zomdathr[,c("timestamp", "VPD", "wind.speed", "air.press", "FC", "GPP", "RE", "LE")])
allregs <- merge(allregs, Wigglesub[,c("timestamp", "date", "StemID", "LDStatus", "mean.dif", "delta.stem", "min.stem", "max.stem", "minstem.time", "maxstem.amtime", "mean.dif.all", "mean.dif.LD", "BranchPosZ", "mean.z.LD", "sd.z.LD")], by="timestamp", all=T)
allregs <- merge(allregs, RHsub[,c("timestamp", "RH", "deltaRH", "minRH", "maxRH", "meanRH", "minRH.time", "maxRH.time")], by="timestamp", all=T)
allregs <- merge(allregs, AirTsub[,c("timestamp", "air.temp", "sumAirT", "deltaAirT", "minAirT", "maxAirT", "minAirT.time", "maxAirT.time")], by="timestamp", all=T)
allregs <- merge(allregs, PARsub[,c("timestamp", "PAR", "sumPAR", "deltaPAR", "sunrise", "highnoon", "sunset", "daylength")], by="timestamp", all=T)
allregs <- merge(allregs, StemWPsub[,c("timestamp", "StemWPsm", "StemWPmornsm", "minStemWP", "minStemWP.time", "maxStemWP", "maxStemWP.time", "deltaStemWP")], by="timestamp", all=T)
allregs <- merge(allregs, precipsub[,c("timestamp", "precip", "Precip15")], by="timestamp", all=T)
allregs <- merge(allregs, SoilTsub[,c("timestamp", "Depth", "Pit", "Cover", "SoilTemp", "SoilT.dif")], by=c("timestamp"), all=T)
allregs$date <- date(allregs$timestamp)
allregs <- merge(allregs, allsoilpsy[allsoilpsy$PsyType=="CR",c("date", "Depth", "Cover", "Pit", "SoilWaterPot", "SoilWP.covmean")], by=c("date", "Depth", "Pit", "Cover"), all=T)
allregs <- merge(allregs, SWCsub[,c("date", "Depth", "Pit", "SWC", "meanSWC")], by=c("date", "Depth", "Pit"), all=T)

colnames(allregs) <- c("date", "Depth", "Pit", "Cover", "timestamp", "VPD", "wind.speed", "air.press", "FC", "GPP", "RE", "LE", "StemID", "LDStatus", "mean.dif", "deltaStemPos", "mindailyStem", "maxdailyStem", "minstem.time", "maxstem.time", "mean.dif.all", "mean.dif.LD", "BranchPosZ", "mean.z.LD", "sd.z.LD", "RH", "deltaRH", "minRH", "maxRH", "meanRH", "minRH.time", "maxRH.time", "air.temp", "sumAirT", "deltaAirT", "minAirT", "maxAirT", "minAirT.time", "maxAirT.time", "PAR", "sumPAR", "deltaPAR", "sunrise", "highnoon", "sunset", "daylength", "StemWPsm", "StemWPmornsm", "minStemWP", "minStemWP.time", "maxStemWP", "maxStemWP.time", "deltaStemWP", "precip", "Precip15", "SoilTemp", "SoilT.dif", "SoilWaterPot", "SoilWP.covmean", "SWC", "meanSWC")

# Get rid of datapoints without associated Wiggle data
allregs <- allregs[!is.na(allregs$mean.dif),]

# Savepoint
write.csv(allregs, "/Users/alesia/Desktop/zombieallregs_4Sept17.csv", row.names=F)




