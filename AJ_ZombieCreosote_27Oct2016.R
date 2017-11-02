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
GPPfile$timestamp <- as.POSIXlt(strptime(GPPfile$TIMESTAMP_START, format= "%Y%m%d%H%M"))
GPPfile <- GPPfile[,c("timestamp", "GPP_PI", "RECO_PI")]
colnames(GPPfile) <- c("timestamp", "GPP", "RE")
mergeflux <- merge(mergeflux, GPPfile, by="timestamp", all=T)

# Aggregate by hour and merge
mergeflux$timestamp <- round(mergeflux$timestamp, "hour")
mergeflux$year <- as.factor(year(mergeflux$timestamp))
mergeflux$yday <- as.factor(yday(mergeflux$timestamp))
mergeflux$hour <- as.factor(hour(mergeflux$timestamp))

mergeflux <- aggregate(cbind(air.temp, RH, VPD, precip, LW_IN, RNET, wind.speed, air.press, PAR, LW_OUT, FC, GPP, RE, LE, SWC_O1_2p5, SWC_O1_12p5, SWC_O1_22p5, SWC_O1_37p5, SWC_O1_52p5, SWC_S1_2p5, SWC_S1_12p5, SWC_S1_22p5, SWC_S1_37p5, SWC_S1_52p5, SWC_O2_2p5, SWC_O2_12p5, SWC_O2_22p5, SWC_O2_37p5, SWC_O2_52p5, SWC_S2_2p5, SWC_S2_12p5, SWC_S2_22p5, SWC_S2_37p5, SWC_S2_52p5, SoilT_O1_2p5, SoilT_O1_12p5, SoilT_O1_22p5, SoilT_O1_37p5, SoilT_O1_52p5, SoilT_S1_2p5, SoilT_S1_12p5, SoilT_S1_22p5, SoilT_S1_37p5, SoilT_S1_52p5, SoilT_O2_2p5, SoilT_O2_12p5, SoilT_O2_22p5, SoilT_O2_37p5, SoilT_O2_52p5, SoilT_S2_2p5, SoilT_S2_12p5, SoilT_S2_22p5, SoilT_S2_37p5, SoilT_S2_52p5, MPS6_O3_15_P, MPS6_O3_15_T, MPS6_O3_22p5_P, MPS6_O3_22p5_T, MPS6_O3_37p5_P, MPS6_O3_37p5_T, MPS6_S1_15_P, MPS6_S1_15_T, MPS6_S1_22p5_P, MPS6_S1_22p5_T, MPS6_S1_37p5_P, MPS6_S1_37p5_T, MPS6_S2_15_P, MPS6_S2_15_T, MPS6_S2_22p5_P, MPS6_S2_22p5_T, MPS6_S2_37p5_P, MPS6_S2_37p5_T, MPS6_S3_15_P, MPS6_S3_15_T, MPS6_S3_22p5_P, MPS6_S3_22p5_T, MPS6_S3_37p5_P, MPS6_S3_37p5_T)~year+yday+hour, data = mergeflux, mean, na.rm=TRUE, na.action="na.pass")

mergeflux$timestamp <- as.POSIXct(strptime(paste(mergeflux$year, mergeflux$yday, mergeflux$hour), format="%Y %j %H"))

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
CRsoil <- aggregate(SoilWaterPot~year+yday+hour+Pit+Depth, data=CRsoil, mean, na.rm=TRUE, na.action="na.pass")
CRstem <- aggregate(StemWaterPot~year+yday+hour+StemSensor, data=CRstem, mean, na.rm=TRUE, na.action="na.pass")

mergeCR <- merge(CRsoil, CRstem, by=c("year", "yday", "hour"), all=T)
mergeCR$timestamp <- as.POSIXct(strptime(paste(mergeCR$year, mergeCR$yday, mergeCR$hour), format="%Y %j %H"))
mergeCR <- mergeCR[,c("timestamp", "Pit", "Depth", "SoilWaterPot", "StemSensor", "StemWaterPot")]

mergeCR <- merge(MasterMerge, mergeCR, by="timestamp", all=T)

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


# write out file of all times
# write.csv(mergezom, '/Users/alesia/Documents/ZombieCreosote/ZombieBranches2.csv', row.names=F)

# Now merge everything
allzomdat <- merge(mergeflux, mergeCR, by="timestamp", all=T)
allzomdat <- merge(allzomdat, mergezom, by="timestamp", all=T)
#allzomdat <- merge(allzomdat, mergepics, by="timestamp", all=T)
allzomdat$date <- date(allzomdat$timestamp)

# VPD
es <- 6.1078 * exp((17.269 * allzomdat$air.temp )/(237.3 + allzomdat$air.temp))
allzomdat$VPD <- es - ( allzomdat$RH * es / 100 );
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
write.csv(allzomdat, file="/Users/alesia/Documents/data_AJ_output/allzomdat9Nov2016.csv", row.names=F)

# remove(CRsoil, CRstem, list.pics, mergeCR, mergeflux, mergepics, mergezom, MasterMerge)

# allzomdat <- read.csv("/Users/alesia/Documents/data_AJ_output/allzomdat26Sept2016.csv", header=T, strip.white=T)
allzomdat$timestamp <- as.POSIXct(strptime(allzomdat$timestamp, format="%Y-%m-%d %H:%M:%S"))
allzomdat$date <- as.Date(allzomdat$timestamp)

# short in is the same thing as par
# keep rad long
# Get everything

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

minmaxmean <- ddply(RHsub, c("date"), function(x)
  data.frame(minRH = min(x$RH, na.rm=TRUE),
             maxRH = max(x$RH, na.rm=TRUE),
             meanRH = mean(x$RH, na.rm=TRUE),
             deltaRH = max(x$RH, na.rm=TRUE) - min(x$RH, na.rm=TRUE)))
RHsub <- merge(RHsub, minmaxmean, by='date', all=T)



length(unique(dataframe$column))



# PAR
PARsub <- unique(subset(zomdathr, select = c(timestamp, date, PAR)))
PARsub <- PARsub[with(PARsub, order(timestamp)),]
PARsub <- PARsub[!is.na(PARsub$timestamp),]

minmaxmean <- ddply(PARsub, c("date"), function(x)
  data.frame(sumPAR = sum(x$PAR, na.rm=TRUE),
             deltaPAR = max(x$PAR, na.rm=TRUE) - min(x$PAR, na.rm=TRUE)))
PARsub <- merge(PARsub, minmaxmean, by='date', all=T)

# Air Temp
AirTsub <- unique(subset(zomdathr, select = c(timestamp, date, air.temp)))
AirTsub <- AirTsub[with(AirTsub, order(timestamp)),]

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
StemWPsubmorn$StemWPmornsm <- rollapply(StemWPsubmorn$StemWaterPot, 5, mean, fill=NA)

StemWPsub$StemWPsm <- rollapply(StemWPsub$StemWaterPot, 11, mean, fill=NA)
StemWPsub$date <- date(StemWPsub$timestamp)
StemWPsub <- merge(StemWPsub, StemWPsubmorn[,c("date", "StemWPmornsm")], by="date", all=T)

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


for (i in firstbranchcolumn:lastbranchcolumn) {
  Wigglesub[,i] <- na.approx(Wigglesub[,i], Wigglesub$timestamp, na.rm=F, maxgap = 8)
}
for (i in firstbranchcolumn:lastbranchcolumn) {
  Wigglesub[,i] <- rollapply(Wigglesub[,i], 5, mean, fill=NA)
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

# Only look at zombie1 branches for now
#Wigglesub <- Wigglesub[grep("z1", Wigglesub$StemID),]

# Look at each branch and remove outliers

# Find difference from mean, % total movement, derivative
means <- ddply(Wigglesub, c("StemID"), function(x)
  data.frame(stem.mean = mean(x$YPosition, na.rm=TRUE),
             stem.maxdif = max(
               (max(x$YPosition, na.rm=T) - 
                  mean(x$YPosition, na.rm=T)),
               (mean(x$YPosition, na.rm=T) - 
                  min(x$YPosition, na.rm=T))),
             perc98 = quantile(x$YPosition, probs=0.98, na.rm=TRUE),
             perc2 = quantile(x$YPosition, probs=0.02, na.rm=TRUE)))
Wigglesub <- merge(Wigglesub, means, by='StemID', all=T)
Wigglesub$mean.dif <- (Wigglesub$YPosition - Wigglesub$stem.mean) /
  (Wigglesub$stem.maxdif)
Wigglesub$PercentY <- (Wigglesub$YPosition - Wigglesub$perc2) / (Wigglesub$perc98 - Wigglesub$perc2)

Wigglesub$PercentY[Wigglesub$PercentY > 1] <- 1
Wigglesub$PercentY[Wigglesub$PercentY < 0] <- 0

# Delta, mean, min, and max movement per day
Wigglesub$date <- date(Wigglesub$timestamp)
deltastem <- ddply(Wigglesub, c("date", "StemID"), function(x)
  data.frame(mean.stem = mean(x$mean.dif, na.rm=TRUE),
             min.stem = min(x$mean.dif, na.rm=TRUE),
             max.stem = max(x$mean.dif, na.rm=TRUE),
             delta.stem = max(x$mean.dif, na.rm=TRUE) - min(x$mean.dif, na.rm=TRUE)))
Wigglesub<- merge(Wigglesub, deltastem, by=c("date", "StemID"), all=T)

# Calculate all-stem averages
allstems <- ddply(Wigglesub, c("timestamp"), function(x)
  data.frame(mean.dif.all = mean(x$mean.dif, na.rm=TRUE)))
LDstems <- ddply(Wigglesub, c("timestamp", "LDStatus"), function(x)
  data.frame(mean.dif.LD = mean(x$mean.dif, na.rm=TRUE)))

Wigglesub<- merge(Wigglesub, allstems, by=c("timestamp"), all=T)
Wigglesub<- merge(Wigglesub, LDstems, by=c("timestamp", "LDStatus"), all=T)



################################################
# Plots
#summary(longzomdat$timestamp)
start.date <- as.POSIXct(strptime("2015-07-31 00:00:00", format="%Y-%m-%d %H:%M:%S"))
end.date <- as.POSIXct(strptime("2015-12-15 00:00:00", format="%Y-%m-%d %H:%M:%S"))
tenday.start <- as.POSIXct(strptime("2015-10-08 00:00:00", format="%Y-%m-%d %H:%M:%S"))
tenday.end <- as.POSIXct(strptime("2015-10-15 00:00:00", format="%Y-%m-%d %H:%M:%S"))

stems.plot.week <- ggplot(Wigglesub[Wigglesub$StemID=="z1_1" | Wigglesub$StemID=="z1_2" | Wigglesub$StemID=="z1_5",], aes(x=timestamp, y=YPosition, group=StemID, colour=StemID)) +
  geom_point() + geom_line() + theme_bw() + 
  theme(legend.position="none") +
  ylab("y-coordinate") + xlab("Time") +
  xlim(tenday.start, tenday.end) 

stems.plot.mean.week <- ggplot(Wigglesub[Wigglesub$StemID=="z1_1" | Wigglesub$StemID=="z1_2" | Wigglesub$StemID=="z1_5",], aes(x=timestamp, y=mean.dif, group=StemID, colour=StemID)) +
  geom_point(alpha=0.5) + geom_line(alpha=0.8) + theme_bw() + 
  geom_point(colour="black", aes(y=mean.dif.all)) + 
  geom_line(colour="black", aes(y=mean.dif.all)) +
  theme(legend.position="none") +
  ylab("Branch Position") + xlab("Time") +
  xlim(tenday.start, tenday.end) 

stems.plot.mean <- ggplot(Wigglesub, aes(x=timestamp, y=mean.dif, group=StemID)) +
  theme_bw() + 
  geom_point(colour="light gray") + geom_line(colour="light gray") + 
  geom_point(colour="black", alpha=0.5, aes(y=mean.dif.all)) + 
  geom_line(colour="black", alpha=0.5, aes(y=mean.dif.all)) +
  theme(legend.position="none") +
  ylab("Branch Position") + xlab("Time") +
  xlim(start.date, end.date) 

stems.plot.dailymax <- ggplot(Wigglesub, aes(x=timestamp, y=max.stem, group=StemID, colour=StemID)) +
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

SWCall.plot <- ggplot(SWCsub, aes(x=date, y=meanSWC, group=Depth, colour=Depth)) +
  geom_point() + geom_line() + theme_bw() +
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlab("Time") + ylab("Soil Water Content") +
  xlim(date(start.date), date(end.date)) 

SoilWP.plot <- ggplot(allsoilpsy[allsoilpsy$PsyType=="CR",], aes(x=date, y=SoilWP.covmean, group=Depth, colour=Depth, shape=Cover)) + facet_grid(Cover~.) +
  geom_point() + theme_bw() + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(as.Date(start.date), as.Date(end.date))

StemWP.plot <- ggplot(StemWPsub, aes(x=timestamp, y=StemWPsm)) +
  geom_line(colour="dark green") + theme_bw() +
  geom_line(alpha=0.5, colour="green", aes(y=StemWPmornsm)) + 
  theme(legend.position="none") +
  xlab("Time") + ylab("Stem Water Potential (MPa)") +
  xlim(start.date, end.date)

humid.plot <- ggplot(RHsub, aes(x=timestamp, y=RH)) +
  geom_point(colour="dark blue", alpha=0.5) + theme_bw() +
  theme(legend.position="none") +
  xlab("Time") + ylab("Relative Humidity (%)") +
  xlim(start.date, end.date)

PAR.plot <- ggplot(PARsub, aes(x=timestamp, y=PAR)) +
  geom_point(alpha=0.2, colour="orange") + theme_bw() +
  geom_line(aes(y=deltaPAR), colour="tomato2") +
  theme(legend.position="none") +
  xlab("Time") + ylab("Incoming Light (µE m-2 s-1)") +
  xlim(start.date, end.date)

AirT.plot <- ggplot(AirTsub, aes(x=timestamp, y=air.temp)) +
  geom_point(colour="dark red", aes(alpha=0.3)) + theme_bw() + 
  theme(legend.position="none") +
  xlab("Time") + ylab(expression("Air Temperature " ( degree*C))) +
  xlim(start.date, end.date)

SoilT.plot <- ggplot(SoilTsub, aes(x=timestamp, y=SoilTemp, group=Depth, colour=Depth)) + 
  geom_line() + theme_bw() +
  facet_grid(Cover~.) + 
  theme(legend.position="bottom", legend.direction="horizontal") +
  xlim(start.date, end.date)

SoilTdif.plot <- ggplot(SoilTsub[SoilTsub$Depth=="02.5cm",], aes(x=timestamp, y=SoilT.dif, group=Depth, colour=SoilT.dif)) + 
  geom_point(alpha=0.3) + geom_line(alpha=0.7) + 
  geom_hline(aes(yintercept=0)) +
  stat_smooth(method="lm") +
  theme_bw() + facet_grid(Depth~.) +
  theme(legend.position="none") +
  scale_colour_gradient(limits=c(-11,11),high="red",low="blue") +
  xlab("Time") + ylab(expression("Soil Temperature Difference" ( degree*C))) +
  xlim(start.date, end.date)

#grid.arrange(humid.plot, stems.plot.mean, ncol=1)
#grid.arrange(SWCall.plot, stems.plot.mean, ncol=1)
#grid.arrange(SoilWP.plot, stems.plot.mean, ncol=1)
#grid.arrange(StemWP.plot, stems.plot.mean, ncol=1)

print(humid.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/humid_plot.png", width=10, height=6, units="in", dpi=300)

print(PAR.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/PAR_plot.png", width=10, height=6, units="in", dpi=300)

print(AirT.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/AirT_plot.png", width=10, height=6, units="in", dpi=300)

print(stems.plot.week)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_week.png", width=6.5, height=5, units="in", dpi=300)

print(stems.plot.mean.week)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/stems_mean_week.png", width=6.5, height=5, units="in", dpi=300)

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
allregs <- merge(allregs, Wigglesub[,c("timestamp", "date", "StemID", "LDStatus", "mean.dif", "delta.stem", "min.stem", "max.stem", "mean.dif.all", "mean.dif.LD")], by="timestamp", all=T)
allregs <- merge(allregs, RHsub[,c("timestamp", "RH", "deltaRH", "minRH", "maxRH")], by="timestamp", all=T)
allregs <- merge(allregs, AirTsub[,c("timestamp", "air.temp", "sumAirT", "deltaAirT", "minAirT", "maxAirT")], by="timestamp", all=T)
allregs <- merge(allregs, PARsub[,c("timestamp", "PAR", "sumPAR", "deltaPAR")], by="timestamp", all=T)
allregs <- merge(allregs, StemWPsub[,c("timestamp", "StemWPsm", "StemWPmornsm")], by="timestamp", all=T)
allregs <- merge(allregs, precipsub[,c("timestamp", "precip", "Precip15")], by="timestamp", all=T)
allregs <- merge(allregs, SoilTsub[,c("timestamp", "Depth", "Pit", "Cover", "SoilTemp", "SoilT.dif")], by=c("timestamp"), all=T)
allregs$date <- date(allregs$timestamp)
allregs <- merge(allregs, allsoilpsy[allsoilpsy$PsyType=="CR",c("date", "Depth", "Cover", "Pit", "SoilWaterPot", "SoilWP.covmean")], by=c("date", "Depth", "Pit", "Cover"), all=T)
allregs <- merge(allregs, SWCsub[,c("date", "Depth", "Pit", "SWC", "meanSWC")], by=c("date", "Depth", "Pit"), all=T)

colnames(allregs) <- c("date", "Depth", "Pit", "Cover", "timestamp", "VPD", "wind.speed", "air.press", "FC", "GPP", "RE", "LE", "StemID", "LDStatus", "mean.dif", "deltaStemPos", "mindailyStem", "maxdailyStem", "mean.dif.all", "mean.dif.LD", "RH", "deltaRH", "minRH", "maxRH", "air.temp", "sumAirT", "deltaAirT", "minAirT", "maxAirT", "PAR", "sumPAR", "deltaPAR", "StemWPsm", "StemWPmornsm", "precip", "Precip15", "SoilTemp", "SoilT.dif", "SoilWaterPot", "SoilWP.covmean", "SWC", "meanSWC")

# Get rid of datapoints without associated Wiggle data
allregs <- allregs[!is.na(allregs$mean.dif),]
summary(allregs)

# Calculate derivatives (based on time stops available)
#
allreg_subset <- unique(allregs[,c("timestamp", "StemID", "mean.dif")])
allreg_subset <- allreg_subset[with(allreg_subset, order(StemID, timestamp)),]
deriv <- ddply(allreg_subset, c("StemID"), function(x)
  data.frame(timestamp = x$timestamp,
             dStemPos = c(NA,diff(x$mean.dif,2),NA) / (c(NA,diff(x$timestamp,2)/2,NA))))
allregs <- merge(deriv, allregs, by=c("timestamp", "StemID"))
allregs$dStemPos[is.infinite(allregs$dStemPos)] <- NA

allreg_subset <- unique(allregs[,c("timestamp", "RH", "air.temp", "PAR", "StemWPsm", "mean.dif.all")])
allreg_subset <- allreg_subset[with(allreg_subset, order(timestamp)),]
allreg_subset$dStemPos.all = c(NA,diff(allreg_subset$mean.dif.all,2),NA) / c(NA,(diff(allreg_subset$timestamp,2)),NA)
allreg_subset$dRH = c(NA,diff(allreg_subset$RH,2),NA) / c(NA,(diff(allreg_subset$timestamp,2)),NA)
allreg_subset$dAirT = c(NA,diff(allreg_subset$air.temp,2),NA) / (c(NA,diff(allreg_subset$timestamp,2),NA))
allreg_subset$dPAR = c(NA,diff(allreg_subset$PAR,2),NA) / (c(NA,diff(allreg_subset$timestamp,2),NA))
allreg_subset$dStemWP = c(NA,diff(allreg_subset$StemWPsm,2),NA) / (c(NA,diff(allreg_subset$timestamp,2),NA))

allregs <- merge(allreg_subset[,c("timestamp", "dStemPos.all", "dRH", "dAirT", "dPAR", "dStemWP")], allregs, by=c("timestamp"))

plot(allregs$dStemWP, allregs$dStemPos)
plot(allregs$FC[allregs$LDStatus=="L"], allregs$mean.dif.LD[allregs$LDStatus=="L"])

plot(allregs$dStemWP, allregs$dStemPos.all)


plot(log10(abs(allregs$StemWPsm)), allregs$mean.dif)


c(cor.test(allregs$meanSWC, allregs$mean.dif.all)$estimate, 
  cor.test(allregs$meanSWC, allregs$mean.dif.all)$p.value)




# Correlation tests
raw.corr <- data.frame(
  RHest = cor.test(allregs$RH, allregs$mean.dif.all)$estimate,
  AirTest = cor.test(allregs$air.temp, allregs$mean.dif.all)$estimate,
  PARest = cor.test(allregs$PAR, allregs$mean.dif.all)$estimate,
  StemWPest = cor.test(allregs$StemWPsm, allregs$mean.dif.all)$estimate,
  RHpval = cor.test(allregs$RH, allregs$mean.dif.all)$p.value,
  AirTpval = cor.test(allregs$air.temp, allregs$mean.dif.all)$p.value,
  PARpval = cor.test(allregs$PAR, allregs$mean.dif.all)$p.value,
  StemWPpval = cor.test(allregs$StemWPsm, allregs$mean.dif.all)$p.value)

raw.corr$RHest <- paste("R^2 == ", round(raw.corr$RHest, digits=2))
raw.corr$AirTest <- paste("R^2 == ", round(raw.corr$AirTest, digits=2))
raw.corr$PARest <- paste("R^2 == ", round(raw.corr$PARest, digits=2))
raw.corr$StemWPest <- paste("R^2 == ", round(raw.corr$StemWPest, digits=2))
raw.corr$ypos <- .9*max(allregs$mean.dif.all, na.rm=T)



c(cor.test(allregs$StemWPsm[allregs$StemWPsm<=-6], allregs$mean.dif[allregs$StemWPsm<=-6])$estimate, cor.test(allregs$StemWPsm[allregs$StemWPsm<=-6], allregs$mean.dif[allregs$StemWPsm<=-6])$p.value)

a <- ggplot(unique(allregs[allregs$StemWPsm>=-5,c("timestamp", "StemID", "dStemWP", "dStemPos", "StemWPsm")]), aes(x=dStemPos, y=dStemWP, group=StemID, colour=StemID)) +
  geom_point() + theme_bw() + stat_smooth(method="lm") + 
  theme(legend.position="none") 
a
+
  ylab("Branch Position") + xlab("Time") +
  xlim(tenday.start, tenday.end) 


Stemoneweek <- ggplot(unique(allregs[,c("timestamp", "mean.dif", "StemID", "mean.dif.all")]), aes(x=timestamp, y=mean.dif, group=StemID)) +
  theme_bw() + 
  geom_point(colour="light gray") + geom_line(colour="light gray") + 
  geom_point(colour="black", alpha=0.5, aes(y=mean.dif.all)) + 
  geom_line(colour="black", alpha=0.5, aes(y=mean.dif.all)) +
  theme(legend.position="none") +
  ylab("Branch Position") + xlab("Time") +
  xlim(tenday.start, tenday.end) 


# Humidity
RHoneweek <- ggplot(unique(allregs[,c("timestamp", "RH")]), aes(x=timestamp, y=RH)) +
  theme_bw() +
  geom_point(alpha=0.5, colour="dark blue") + 
  geom_line(alpha=0.5, colour="dark blue") + 
  theme(legend.position="none") +
  xlab("Time") + ylab("Relative Humidity (%)") +
  xlim(tenday.start, tenday.end)
  
WigRH.cor.plot <- ggplot(unique(allregs[,c("timestamp", "RH", "mean.dif.all")]), aes(x=RH, y=mean.dif.all)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3), colour="dark blue") + stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=20, y=ypos, label=RHest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F) +
  xlab("Relative Humidity (%)") + ylab("Branch Position")
  

RHweekplots <- ggdraw() +
  draw_plot(Stemoneweek, 0, 0.5, 0.5, 0.5) +
  draw_plot(RHoneweek, 0, 0, 0.5, 0.5) +
  draw_plot(stems.plot.mean, 0.5, 0.5, 0.5, 0.5) +
  draw_plot(humid.plot, 0.5, 0, 0.5, 0.5)
save_plot("/Users/alesia/Desktop/ZombiePlots/allRHplot.png", RHweekplots, base_width = 12, base_height = 5)
print(WigRH.cor.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/RHWiggle_plot.png", width=5, height=5, units="in", dpi=300)

# Air temp
AirToneweek <- ggplot(unique(allregs[,c("timestamp", "air.temp")]), aes(x=timestamp, y=air.temp)) +
  geom_point(alpha=0.5, colour="dark red") + geom_line(alpha=0.5, colour="dark red") + theme_bw() +
  theme(legend.position="none") +
  xlab("Time") + ylab(expression("Air Temperature " ( degree*C))) +
  xlim(tenday.start, tenday.end)

WigAirT.cor.plot <- ggplot(unique(allregs[,c("timestamp", "air.temp", "mean.dif.all")]), aes(x=air.temp, y=mean.dif.all)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3), colour="dark red") + stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=30, y=0.6, label=AirTest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F) +
  xlab(expression("Air Temperature " ( degree*C))) + ylab("Branch Position")

AirTweekplots <- ggdraw() +
  draw_plot(Stemoneweek, 0, 0.5, 0.5, 0.5) +
  draw_plot(AirToneweek, 0, 0, 0.5, 0.5) +
  draw_plot(stems.plot.mean, 0.5, 0.5, 0.5, 0.5) +
  draw_plot(AirT.plot, 0.5, 0, 0.5, 0.5) 
save_plot("/Users/alesia/Desktop/ZombiePlots/allAirTplot.png", AirTweekplots, base_width = 12, base_height = 5)
print(WigAirT.cor.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/AirTWiggle_plot.png", width=5, height=5, units="in", dpi=300)

# PAR
PARoneweek <- ggplot(unique(allregs[,c("timestamp", "PAR")]), aes(x=timestamp, y=PAR)) +
  geom_point(alpha=0.5, colour="orange") + geom_line(alpha=0.5, colour="orange") + theme_bw() +
  theme(legend.position="none") +
  xlab("Time") + ylab("Incoming Light (µE m-2 s-1)") +
  xlim(tenday.start, tenday.end)

WigPAR.cor.plot <- ggplot(unique(allregs[,c("timestamp", "PAR", "mean.dif.all")]), aes(x=PAR, y=mean.dif.all)) + theme_bw() + theme(legend.position="none") +
  geom_point(aes(alpha=0.3), colour="orange") + stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=1750, y=ypos, label=PARest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F) +
  xlab("Incoming Light (µE m-2 s-1)") + ylab("Branch Position")
  

PARweekplots <- ggdraw() +
  draw_plot(Stemoneweek, 0, 0.5, 0.5, 0.5) +
  draw_plot(PARoneweek, 0, 0, 0.5, 0.5) +
  draw_plot(stems.plot.mean, 0.5, 0.5, 0.5, 0.5) +
  draw_plot(PAR.plot, 0.5, 0, 0.5, 0.5) 
save_plot("/Users/alesia/Desktop/ZombiePlots/allPARplot.png", PARweekplots, base_width = 12, base_height = 5)
print(WigPAR.cor.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/PARWiggle_plot.png", width=5, height=5, units="in", dpi=300)

# Stem water potential
StemWPoneweek <- ggplot(unique(allregs[,c("timestamp", "StemWPsm")]), aes(x=timestamp, y=StemWPsm)) +
  geom_point(alpha=0.5, colour="dark green") + geom_line(alpha=0.5, colour="dark green") + theme_bw() +
  theme(legend.position="none") +
  xlab("Time") + ylab("Stem Water Potential (MPa)") +
  xlim(tenday.start, tenday.end)

WigStemWP.cor.plot <- ggplot(unique(allregs[,c("timestamp", "StemWPsm", "mean.dif.all")]), aes(x=StemWPsm, y=mean.dif.all)) + theme_bw() + theme(legend.position="none") +
  geom_point(alpha=0.3, colour="dark green") + 
  stat_smooth(method="lm") + 
  geom_text(data=raw.corr, aes(x=-7.2, y=ypos, label=StemWPest, size=0.2), colour="red", vjust=1, parse=TRUE, show.legend=F) +
  xlab("Stem Water Potential (MPa)") + ylab("Branch Position")

StemWPweekplots <- ggdraw() +
  draw_plot(Stemoneweek, 0, 0.5, 0.5, 0.5) +
  draw_plot(StemWPoneweek, 0, 0, 0.5, 0.5) +
  draw_plot(stems.plot.mean, 0.5, 0.5, 0.5, 0.5) +
  draw_plot(StemWP.plot, 0.5, 0, 0.5, 0.5) 
save_plot("/Users/alesia/Desktop/ZombiePlots/allStemWPplot.png", StemWPweekplots, base_width = 12, base_height = 5)
print(WigStemWP.cor.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/StemWPWiggle_plot.png", width=5, height=5, units="in", dpi=300)


# LDStatus plots
########## MORE WORK HERE

raw.corr.LD <- data.frame(
  LDStatus = c("D", "L"),
  RHest = c(cor.test(allregs$RH[allregs$LDStatus=="D"], allregs$mean.dif.LD[allregs$LDStatus=="D"])$estimate,
            cor.test(allregs$RH[allregs$LDStatus=="L"], allregs$mean.dif.LD[allregs$LDStatus=="L"])$estimate),
  AirTest = c(cor.test(allregs$air.temp[allregs$LDStatus=="D"], allregs$mean.dif.LD[allregs$LDStatus=="D"])$estimate,
              cor.test(allregs$air.temp[allregs$LDStatus=="L"], allregs$mean.dif.LD[allregs$LDStatus=="L"])$estimate),
  StemWPest = c(cor.test(allregs$StemWPsm[allregs$LDStatus=="D"], allregs$mean.dif.LD[allregs$LDStatus=="D"])$estimate,
                cor.test(allregs$StemWPsm[allregs$LDStatus=="L"], allregs$mean.dif.LD[allregs$LDStatus=="L"])$estimate),
  Shadeest = c(cor.test(allregs$SoilT.dif[allregs$LDStatus=="D" & allregs$Depth=="02.5cm" & month(allregs$timestamp) <= 9], allregs$mean.dif.LD[allregs$LDStatus=="D" & allregs$Depth=="02.5cm" & month(allregs$timestamp) <= 9])$estimate,
               cor.test(allregs$SoilT.dif[allregs$LDStatus=="L" & allregs$Depth=="02.5cm" & month(allregs$timestamp) <= 9], allregs$mean.dif.LD[allregs$LDStatus=="L" & allregs$Depth=="02.5cm" & month(allregs$timestamp) <= 9])$estimate))

raw.corr.LD$RHest <- paste("R^2 == ", round(raw.corr.LD$RHest, digits=2))
raw.corr.LD$AirTest <- paste("R^2 == ", round(raw.corr.LD$AirTest, digits=2))
raw.corr.LD$StemWPest <- paste("R^2 == ", round(raw.corr.LD$StemWPest, digits=2))
raw.corr.LD$Shadeest <- paste("R^2 == ", round(raw.corr.LD$Shadeest, digits=2))
raw.corr.LD$ypos <- c(0.95, 0.8)


WigRHLD.cor.plot <- ggplot(unique(allregs[,c("timestamp", "RH", "mean.dif.LD", "LDStatus")]), aes(x=RH, y=mean.dif.LD, group=LDStatus, colour=LDStatus)) + theme_bw() + 
  geom_point(alpha=0.3) + stat_smooth(method="lm") + 
  xlab("Relative Humidity (%)") + ylab("Branch Position") + 
  geom_text(data=raw.corr.LD, aes(x=20, y=ypos, label=RHest, size=0.2), vjust=1, parse=TRUE, show.legend=F) +
  labs(colour='Live/Dead') + scale_color_discrete(labels = c("Dead", "Live"))

AirTLD.cor.plot <- ggplot(unique(allregs[,c("timestamp", "air.temp", "mean.dif.LD", "LDStatus")]), aes(x=air.temp, y=mean.dif.LD, group=LDStatus, colour=LDStatus)) + theme_bw() + 
  geom_point(alpha=0.3) + stat_smooth(method="lm") + 
  xlab(expression("Air Temperature " ( degree*C))) + ylab("Branch Position") + 
  geom_text(data=raw.corr.LD, aes(x=30, y=ypos, label=AirTest, size=0.2), vjust=1, parse=TRUE, show.legend=F) +
  labs(colour='Live/Dead') + scale_color_discrete(labels = c("Dead", "Live"))

WigStemWPLD.cor.plot <- ggplot(unique(allregs[,c("timestamp", "StemWPsm", "LDStatus", "mean.dif.LD")]), aes(x=StemWPsm, y=mean.dif.LD, group=LDStatus, colour=LDStatus)) + theme_bw() + 
  geom_point(alpha=0.3) + 
  stat_smooth(method="lm") + 
  xlab("Stem Water Potential (MPa)") + ylab("Branch Position") +
  geom_text(data=raw.corr.LD, aes(x=-7, y=ypos, label=StemWPest, size=0.2), vjust=1, parse=TRUE, show.legend=F) +
  labs(colour='Live/Dead') + scale_color_discrete(labels = c("Dead", "Live"))

ShadeWiggle.plot <- ggplot(unique(allregs[!is.na(allregs$Season) & allregs$Season=="Monsoon" & allregs$Depth=="02.5cm",c("timestamp", "SoilT.dif", "mean.dif.all")]), aes(x=SoilT.dif, y=mean.dif.all)) + theme_bw() + theme(legend.position="none") +
  geom_point(alpha=0.3) + 
  stat_smooth(method="lm") +
  xlab(expression("Soil Temperature Difference " ( degree*C))) + ylab("Branch Position")

allregs$Season <- NA
allregs$Season[month(allregs$timestamp) <= 9] <- "Monsoon"
allregs$Season[month(allregs$timestamp) >= 11] <- "Winter"
allregs$Season <- as.factor(allregs$Season)
ShadeLD.plot <- ggplot(unique(allregs[!is.na(allregs$Season) & allregs$Season=="Monsoon" & allregs$Depth=="02.5cm",c("timestamp", "SoilT.dif", "LDStatus", "mean.dif.LD", "Season")]), aes(x=SoilT.dif, y=mean.dif.LD, group=LDStatus, colour=LDStatus)) + theme_bw() +
  geom_point(alpha=0.3) + 
  stat_smooth(method="lm") +
  geom_text(data=raw.corr.LD, aes(x=-8, y=ypos, label=Shadeest, size=0.2), vjust=1, parse=TRUE, show.legend=F) +
    labs(colour='Live/Dead') + scale_color_discrete(labels = c("Dead", "Live")) +
  xlab(expression("Soil Temperature Difference " ( degree*C))) + ylab("Branch Position")

print(WigRHLD.cor.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/RHWiggleLD_plot.png", width=6, height=5, units="in", dpi=300)
print(AirTLD.cor.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/AirTWiggleLD_plot.png", width=6, height=5, units="in", dpi=300)
print(WigStemWPLD.cor.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/StemWPWiggleLD_plot.png", width=6, height=5, units="in", dpi=300)
print(ShadeWiggle.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/ShadeWiggle_plot.png", width=5, height=5, units="in", dpi=300)
print(ShadeLD.plot)
ggsave(file="/Users/alesia/Desktop/ZombiePlots/ShadeWiggleLD_plot.png", width=6, height=5, units="in", dpi=300)






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
allregsshadesub <- allregs[allregs$Depth=="02.5cm" & !is.na(allregs$Depth) &allregs$LDStatus=="L",]

lmShade <- data.frame(
  Shadeest = cor.test(allregsshadesub$SoilT.dif, allregsshadesub$mean.dif.LD)$estimate,
  Shadepval = cor.test(allregsshadesub$SoilT.dif, allregsshadesub$mean.dif.LD)$p.value)
lmShade$Shadeest <- paste("R^2 == ", round(lmShade$Shadeest, digits=2))
lmShade$ypos <- 1

Shade.plot <- ggplot(allregsshadesub(unique()), aes(x=SoilT.dif, y=mean.dif.LD)) + theme_bw() + theme(legend.position="none") +
  geom_point(alpha=0.3) + 
  stat_smooth(method="lm") +
  geom_text(data=lmShade, aes(x=-9, y=ypos, label=Shadeest, size=0.2), vjust=1, parse=TRUE, show.legend=F)

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

lmStemHum <- ddply(Stem_Hum, c("LDStatus"), function(x)
  data.frame(est = cor.test(x$RH, x$mean.dif)$estimate,
             pval = cor.test(x$RH, x$mean.dif)$p.value))
lmStemHum$est <- paste("R^2 == ", round(lmStemHum$est, digits=2))
lmStemHum$ypos <- seq(1, -1, length.out=length(unique(lmStemHum$StemID)))
lmStemHum <- lmStemHum[lmStemHum$pval<0.05,]

Stem_Hum_StemID.plot <- ggplot(Stem_Hum, aes(x=RH, y=mean.dif, group=LDStatus, colour=LDStatus)) + geom_point(aes(alpha=0.3)) + stat_smooth(method="lm") + theme_bw() +
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

df.thisplot <- unique(allregs[,c("timestamp", "StemWPsm", "mean.dif")])
Stem_StemLDstat.plot <- ggplot(df.thisplot, aes(x=StemWPsm, y=mean.dif)) + geom_point(aes(alpha=0.3)) + stat_smooth(method="lm") + theme_bw() +
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

