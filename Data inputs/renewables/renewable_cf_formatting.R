# Formatting of NREL capacity factor data
# Created August 3, 2019
# Brian Sergi

library(openxlsx)
library(maps)
library(mapproj)
library(ggplot2)
library(plyr)

# Note: set working directory to file location

## Solar ####

# county-level annual capacity factors (CF) derived from combination of global horiztonal irradiance (GHI)  
# measurements and regression of GHI and CF

# read in GHI measurements 
ghi <- read.xlsx("solarsummaries.xlsx", sheet="GHI County")

# drop AK and HI from data
ghi <- ghi[!(ghi$State %in% c("AK", "HI")),]

# down-select to relevant columns (note: data is available by month but currently estimating annually)
ghi <- ghi[, c("State", "County", "County.FIPS", "Annual.Average.(kWh/m2/day)", 
               "Annual.Average.Median.(kWh/m2/day)", "Annual.Average.Standard.Deviation.(kWh/m2/day)")]


# regression of net CF on GHI based on LBNL study (see https://emp.lbl.gov/pv-capacity-factors)
# NCF = 0.029529 + 0.0394632 x GHI (assumes fixed-tilt)
b0 <- 0.029529
b1 <- 0.0394632
ghi$cf <- b0 + b1 * ghi$`Annual.Average.(kWh/m2/day)`
colnames(ghi)[colnames(ghi) == "County.FIPS"] <- "fips"


# plot results on map to spot check
counties <- map_data("county", projection  = "albers", par = c(30,0))
states <-  map_data("state", projection  = "albers", par = c(30,0))

counties$polyname <- paste(counties$region, counties$subregion, sep = ",")
counties <- merge(counties, county.fips, by="polyname", all.x=T)  # county.fips included in maps package

# manually adjust some erroneous fips codes
# unique(ghiPlot[is.na(ghiPlot$cf), "polyname"])

# [1] "florida,okaloosa"         "louisiana,st martin"      "north carolina,currituck" "texas,galveston"         
# [5] "virginia,accomack"        "washington,pierce"        "washington,san juan"   
counties[counties$polyname == "florida,okaloosa", "fips"] <- 12091
counties[counties$polyname == "louisiana,st martin", "fips"] <- 22099
counties[counties$polyname == "north carolina,currituck", "fips"] <- 37053
counties[counties$polyname == "texas,galveston", "fips"] <- 48167
counties[counties$polyname == "virginia,accomack", "fips"] <- 51001
counties[counties$polyname == "washington,pierce", "fips"] <- 53053
counties[counties$polyname == "washington,san juan", "fips"] <- 53055

# merge with ghi data and sort
ghiSub <- ghi[,c("fips", "cf")]
ghiPlot <- merge(counties, ghiSub, by="fips", all=T)
ghiPlot <- ghiPlot[order(ghiPlot$group, ghiPlot$order),]

# plot county choropleth
ggplot() + geom_polygon(data=ghiPlot, aes(x=long, y=lat, group = group, fill=cf*100), colour="black", size=0.1) + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") + theme_bw()  + 
  labs(fill = "Capacity\nfactor (%)", title = "", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = states, aes(x=long, y=lat, group = group), colour = "black")   # add state borders back in

ggsave("Annual solar CF.pdf", width = 7, height = 4)


## Wind ####

# site-based capacity factor data taken from the NREL Wind Toolkit
# see https://www.nrel.gov/grid/wind-toolkit.html and
# https://www.sciencedirect.com/science/article/pii/S0306261915004237?via%3Dihub for details  

wind <- read.csv("wtk_site_metadata.csv")

# discard offshore measurements (data does not include AK or HI)
wind <- wind[wind$power_curve != "offshore",]

# convert county/state to polyname for matching with fips code
wind$polyname <- paste(tolower(wind$State), tolower(wind$County), sep=",")
wind <- merge(wind, county.fips, by="polyname", all.x=T)

# average capacity factors by county
windAvg <- ddply(wind, ~ fips + polyname, summarize, cf = mean(capacity_factor))
wndPlot <- merge(counties, windAvg, by=c("fips", "polyname"), all=T)
wndPlot <- wndPlot[order(wndPlot$group, wndPlot$order),]

# plot county choropleth
ggplot() + geom_polygon(data=wndPlot, aes(x=long, y=lat, group = group, fill=cf*100), colour="black", size=0.1) + 
  scale_fill_continuous(low = "thistle2", high = "darkblue", guide="colorbar") + theme_bw()  + 
  labs(fill = "Capacity\nfactor (%)", title = "", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = states, aes(x=long, y=lat, group = group), colour = "black")   # add state borders back in

ggsave("Annual wind CF.pdf", width = 7, height = 4)

# assume NA values mean that wind cannot be built in that county

## Merge CF data sets for export ####
cfData <- merge(windAvg, ghiSub, by="fips", all=T)
colnames(cfData) <- c("fips", "polyname", "windCF", "solarCF")
cfData <- cfData[!is.na(cfData$fips), c("fips", "windCF", "solarCF")]

# remove any missing values (assume 0, meaning no resources will be built there)
cfData[is.na(cfData)] <- 0

write.csv(cfData, 'cfData.csv', row.names=F)
