# This script reads in Ameriflux and branch movement files in order to create plots and run stats

# Load required libraries
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(car)
library(nortest)
library(reshape2)
library(BSDA)
library(GGally)
library(Hmisc)
library(zoo)
library(lattice)
library(latticeExtra)

# Read in Ameriflux flux files
shrub_flux_2011 <- read.csv("~/Documents/ZombieCreosote/US-Ses_2011_gapfilled.txt", header=TRUE)
shrub_flux_2012 <- read.csv("~/Desktop/ZombieCreosote/US-Ses_2012_gapfilled.txt", header=TRUE)
shrub_flux_2013 <- read.csv("~/Desktop/ZombieCreosote/US-Ses_2013_gapfilled.txt", header=TRUE)
shrub_flux <- rbind(shrub_flux_2011, shrub_flux_2012, shrub_flux_2013)
shrub_flux[shrub_flux<(-9998)] <- NA

# Change some column names
colnames(shrub_flux)[1] <- "Year"
colnames(shrub_flux)[2] <- "JulianDate"
colnames(shrub_flux)[3] <- "Hour"

# Subset flux file
shrub_names <- colnames(shrub_flux)
sub_shrub <- cbind(shrub_flux[,1:3],shrub_flux[,6],shrub_flux[,22:23],shrub_flux[,25],shrub_flux[,27],shrub_flux[,31],shrub_flux[,40:41])
names_subshrub <- c(shrub.names[1:3],shrub.names[6],shrub.names[22:23],shrub.names[25],shrub.names[27],shrub.names[31],shrub.names[40:41])
colnames(sub_shrub) <- names_subshrub

# Read in Ameriflux soil files
shrub_soil_2011 <- read.csv("~/Documents/ZombieCreosote/US-Ses_2011_soil.txt", header=TRUE)
shrub_soil_2012 <- read.csv("~/Documents/ZombieCreosote/US-Ses_2012_soil.txt", header=TRUE)
shrub_soil_2013 <- read.csv("~/Documents/ZombieCreosote/US-Ses_2013_soil.txt", header=TRUE)
shrub_soil <- rbind(shrub_soil_2011, shrub_soil_2012, shrub_soil_2013)
shrub_soil[shrub_soil<(-9998)] <- NA

# Change some column names
colnames(shrub_soil)[1] <- "Year"
colnames(shrub_soil)[2] <- "JulianDate"
colnames(shrub_soil)[3] <- "Hour"

# Melt soil dataframe 
soil.T <- cbind(shrub_soil[,1:3],shrub_soil[,25:29])
melt.soil.T <- melt(soil.T, c('Year','JulianDate','Hour'))
colnames(melt.soil.T)[4:5] <- c("soil_T_depth", "soil_T")
soil.VWC <- cbind(shrub_soil[,1:3],shrub_soil[,60:64])
melt.soil.VWC <- melt(soil.VWC, c('Year','JulianDate','Hour'))
colnames(melt.soil.VWC)[4:5] <- c("soil_VWC_depth", "soil_VWC")
soil.VWC.cover <- cbind(shrub_soil[,1:3],shrub_soil[,65:69])
melt.soil.VWC.cover <- melt(soil.VWC.cover, c('Year','JulianDate','Hour'))
colnames(melt.soil.VWC.cover)[4:5] <- c("soil_VWCcover_depth", "soil_VWCcover")
soil.VWC.open <- cbind(shrub_soil[,1:3],shrub_soil[,70:74])
melt.soil.VWC.open <- melt(soil.VWC.open, c('Year','JulianDate','Hour'))
colnames(melt.soil.VWC.open)[4:5] <- c("soil_VWCopen_depth", "soil_VWCopen")
soil.all <- cbind(shrub_soil[,1:3],shrub_soil[,25:29],shrub_soil[,60:74])
melt.soil <- melt(soil.all, c('Year','JulianDate','Hour'))
colnames(melt.soil)[4:5] <- c("soil_var", "soil_value")

# Read in branch movement files
branch1 <- read.csv("~/Desktop/ZombieCreosote/ZombieBranch1.csv", header=TRUE)
branch2 <- read.csv("~/Desktop/ZombieCreosote/ZombieBranch2.csv", header=TRUE)
branch3 <- read.csv("~/Desktop/ZombieCreosote/ZombieBranch3.csv", header=TRUE)
# BranchY inverse 
branch1$Branch1 <- max(branch1$Branch1)-branch1$Branch1
branch2$Branch1 <- max(branch2$Branch1)-branch2$Branch1
branch3$Branch1 <- max(branch3$Branch1)-branch3$Branch1
allbranches <- rbind(branch1, branch2, branch3)
# Make military time Hour column
allbranches$Time <- allbranches$Time*100
colnames(allbranches)[1] <- "Year"
colnames(allbranches)[2] <- "JulianDate"
colnames(allbranches)[3] <- "Hour"

# Merge flux and soil data
zombiefluxsoil <- merge(sub_shrub, melt.soil, by=c("Year", "JulianDate","Hour"), all=TRUE)
zombiefluxvwc <- merge(sub_shrub, soil.VWC, by=c("Year", "JulianDate","Hour"), all=TRUE)
# Subset to dates of interest
zombiedates <- subset(zombiefluxsoil, (Year==2011 & JulianDate>350 & JulianDate<364) | (Year==2012 & JulianDate>175 & JulianDate<188) | (Year==2013 & JulianDate>210 & JulianDate<220))
zombievwcdates <- subset(zombiefluxsoil, (Year==2011 & JulianDate>350 & JulianDate<364) | (Year==2012 & JulianDate>175 & JulianDate<188) | (Year==2013 & JulianDate>210 & JulianDate<220))
# Merge with branch data
zombiebranchmelt <- merge(allbranches, zombiedates, by=c("Year", "JulianDate","Hour"), all=TRUE)
zombiebranch <- merge(allbranches, zombievwcdates, by=c("Year", "JulianDate","Hour"), all=TRUE)
zbranchmelt <- subset(zombiebranchmelt, Branch1!="NA")
zbranch <- subset(zombiebranch, Branch1!="NA")
# Make DateTime column
zbranchmelt$DateTime <- zbranchmelt$JulianDate + (zbranchmelt$Hour/2400)
zbranch$DateTime <- zbranch$JulianDate + (zbranch$Hour/2400)

zombiesoiltemp <- subset(zbranchmelt, soil_var=="Tsoil_2.5_Avg" | soil_var=="Tsoil_12.5_Avg" | soil_var=="Tsoil_22.5_Avg" | soil_var=="Tsoil_37.5_Avg" | soil_var=="Tsoil_52.5_Avg")
zombiesoilVWC <- subset(zbranchmelt, soil_var=="VWC_37.5_Avg" | soil_var=="VWC_22.5_Avg" | soil_var=="VWC_2.5_Avg" | soil_var=="VWC_12.5_Avg" | soil_var=="VWC_52.5_Avg")

##### PLOTS

# BranchY, RH, PA, VPD, PAR, soilVWC
plot_branch <- ggplot(zbranchmelt, aes(y = Branch1, x = DateTime))
plot_branch <- plot_branch + theme_bw() + xlab("Day of Year") + ylab("BranchPositionY") + geom_point(alpha = 0.8) + geom_line(na.rm=FALSE) + facet_grid(~Year)
plot_soiltemp <- ggplot(zombiesoiltemp, aes(y = soil_value, x = DateTime, group = soil_var, colour = soil_var))
plot_soiltemp <- plot_soiltemp + theme_bw() + xlab("Day of Year") + ylab("Soil temp") + geom_line(alpha = 0.8)  
plot_soilVWC <- ggplot(zombiesoilVWC, aes(y = soil_value, x = DateTime, group = soil_var, colour = soil_var))
plot_soilVWC <- plot_soilVWC + theme_bw() + xlab("Day of Year") + ylab("VWC") + geom_line(alpha = 0.8)  
plot_RH <- ggplot(zombiedates, aes(y = RH, x = DateTime))
plot_RH <- plot_RH + theme_bw() + xlab("Day of Year") + ylab("Rel.Humidity") + geom_line(alpha = 0.8)  
plot_PA <- ggplot(zombiedates, aes(y = PA, x = DateTime))
plot_PA <- plot_PA + theme_bw() + xlab("Day of Year") + ylab("Pressure") + geom_line(alpha = 0.8)  
plot_VPD <- ggplot(zombiedates, aes(y = VPD, x = DateTime))
plot_VPD <- plot_VPD + theme_bw() + xlab("Day of Year") + ylab("VPD") + geom_line(alpha = 0.8)  

grid.arrange(plot_branch, plot_RH, plot_PA, plot_VPD, plot_soilVWC, plot_soiltemp)






# BranchY vs RH, PA, VPD, PAR, soilVWC
plot_soiltemp2 <- ggplot(zombiesoiltemp, aes(y = soil_value, x = Branch1, group = soil_var, colour = soil_var))
plot_soiltemp2 <- plot_soiltemp2 + theme_bw() + xlab("BranchPosition") + ylab("Soil temp") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_soiltemp2

plot_soilVWC2 <- ggplot(zombiesoilVWC, aes(y = soil_value, x = Branch1, group = soil_var, colour = soil_var))
plot_soilVWC2 <- plot_soilVWC2 + theme_bw() + xlab("BranchPosition") + ylab("VWC") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_soilVWC2

plot_RH2 <- ggplot(zbranchmelt, aes(y = RH, x = Branch1, colour=factor(Year)))
plot_RH2 <- plot_RH2 + theme_bw() + xlab("BranchPosition") + ylab("Rel.Humidity") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_RH2

plot_PA2 <- ggplot(zombiedates, aes(y = PA, x = Branch1))
plot_PA2 <- plot_PA2 + theme_bw() + xlab("BranchPosition") + ylab("Pressure") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_PA2

plot_VPD2 <- ggplot(zombiedates, aes(y = VPD, x = Branch1))
plot_VPD2 <- plot_VPD2 + theme_bw() + xlab("BranchPosition") + ylab("VPD") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_VPD2



grid.arrange(plot_branch, plot_RH, plot_PA, plot_VPD, plot_soilVWC, plot_soiltemp)
grid.arrange(plot_RH2, plot_soilVWC2, plot_PA2, plot_soiltemp2, plot_VPD2)

# Correlation values
c(cor.test(zombiedates$RH, zombiedates$Branch1)$estimate, cor.test(zombiedates$RH, zombiedates$Branch1)$p.value)
##       cor           
## 0.6835563 0.0000000 
c(cor.test(zombiedates$PA, zombiedates$Branch1)$estimate, cor.test(zombiedates$PA, zombiedates$Branch1)$p.value)
##           cor                
## -5.555860e-01  3.320572e-114 
c(cor.test(zombiedates$VPD, zombiedates$Branch1)$estimate, cor.test(zombiedates$VPD, zombiedates$Branch1)$p.value)
##           cor                
## -5.737429e-01  2.327819e-123 
c(cor.test(zombievwcdates$VWC_2.5_Avg, zombievwcdates$Branch1)$estimate, cor.test(zombievwcdates$VWC_2.5_Avg, zombievwcdates$Branch1)$p.value)
##          cor              
## 7.115624e-01 5.048184e-12 
c(cor.test(zombievwcdates$VWC_12.5_Avg, zombievwcdates$Branch1)$estimate, cor.test(zombievwcdates$VWC_12.5_Avg, zombievwcdates$Branch1)$p.value)
##       cor           
## 0.8333928 0.0000000 
c(cor.test(zombievwcdates$VWC_22.5_Avg, zombievwcdates$Branch1)$estimate, cor.test(zombievwcdates$VWC_22.5_Avg, zombievwcdates$Branch1)$p.value)
##        cor            
## 0.28310423 0.01755957 
c(cor.test(zombievwcdates$VWC_37.5_Avg, zombievwcdates$Branch1)$estimate, cor.test(zombievwcdates$VWC_37.5_Avg, zombievwcdates$Branch1)$p.value)
##           cor               
## -5.645324e-01  3.577447e-07 
c(cor.test(zombievwcdates$VWC_52.5_Avg, zombievwcdates$Branch1)$estimate, cor.test(zombievwcdates$VWC_52.5_Avg, zombievwcdates$Branch1)$p.value)
##           cor               
## -4.984359e-01  1.127609e-05 

# Plot humidity versus branch position, colored by VWC
plot_RH_25 <- ggplot(zombievwcdates, aes(y = RH, x = Branch1, colour=VWC_2.5_Avg))
plot_RH_25 <- plot_RH_25 + theme_bw() + xlab("BranchPosition") + ylab("Rel.Humidity") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_RH_25
plot_RH_125 <- ggplot(zombievwcdates, aes(y = RH, x = Branch1, colour=VWC_12.5_Avg))
plot_RH_125 <- plot_RH_125 + theme_bw() + xlab("BranchPosition") + ylab("Rel.Humidity") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_RH_125
plot_RH_225 <- ggplot(zombievwcdates, aes(y = RH, x = Branch1, colour=VWC_22.5_Avg))
plot_RH_225 <- plot_RH_225 + theme_bw() + xlab("BranchPosition") + ylab("Rel.Humidity") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_RH_225
plot_RH_375 <- ggplot(zombievwcdates, aes(y = RH, x = Branch1, colour=VWC_37.5_Avg))
plot_RH_375 <- plot_RH_375 + theme_bw() + xlab("BranchPosition") + ylab("Rel.Humidity") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_RH_375
plot_RH_525 <- ggplot(zombievwcdates, aes(y = RH, x = Branch1, colour=VWC_52.5_Avg))
plot_RH_525 <- plot_RH_525 + theme_bw() + xlab("BranchPosition") + ylab("Rel.Humidity") + geom_point(alpha = 0.8) + stat_smooth(method=lm, se=FALSE)  
plot_RH_525


# Multiple regression
lm.branch.all <- lm(Branch1 ~ RH + VWC_12.5_Avg, data = zombievwcdates)
summary(lm.branch.all)

