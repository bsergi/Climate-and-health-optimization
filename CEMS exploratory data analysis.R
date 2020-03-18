# EDA for CEMS data
# Brian Sergi
# December 9, 2018

# note: overlaps with some of the processing done in the python-model

# some assumptions of the model to investigate
# replacing missing emissions with 0's (how is this treated later?)
# interpolating generation levels
# check to make sure number of units is roughly as expected


library(ggplot2)
library(plyr)
library(openxlsx)

## Load CEMS data ####
baseWD <- "~/Documents/Carnegie Mellon/Box Sync/Research/CACES energy modeling/Model V3"
setwd(paste(baseWD,"Data Inputs", sep="/"))

# 1. Emissions data
CEMS <- read.csv("CEMS emissions 2017.csv")

# convert short tons to metric
CEMS$CO2..tons. <- CEMS$CO2..short.tons. * 0.907185
CEMS$CO2..short.tons. <- NULL

CEMS <- CEMS[c('State', 'Facility.Name', 'Facility.ID..ORISPL.', 'Unit.ID', 'Operating.Time',
               'Gross.Load..MW.h.', 'SO2..tons.', 'NOx..tons.', 'CO2..tons.')]


# 2. Facility data
facilities <- read.csv("CEMS facility data 2017.csv")
facilities <- facilities[facilities$Source.Category %in% c("Electric Utility", "Cogeneration", "Small Power Producer"),]

facilities <- facilities[,c('State', 'County', 'FIPS.Code',
                            'Facility.Name', 'Facility.ID..ORISPL.', 'Unit.ID', 
                            'Unit.Type','Fuel.Type..Primary.',
                            'Fuel.Type..Secondary.', 'Commercial.Operation.Date',
                            'Operating.Status', 'Max.Hourly.HI.Rate..MMBtu.hr.',
                            'SO2.Control.s.',  'NOx.Control.s.',  'PM.Control.s.',
                            'Source.Category', 'NERC.Region', 'Year', 
                            'Facility.Latitude',  'Facility.Longitude')]

# merge data
merged <- merge(CEMS, facilities, all=T, by=c("State", "Facility.Name", "Facility.ID..ORISPL.", "Unit.ID"))
merged_sparse <- merge(CEMS, facilities, all=F, by=c("State", "Facility.Name", "Facility.ID..ORISPL.", "Unit.ID"))

super_merge <- rbind(merged_sparse, merged)
is.dup <- duplicated(super_merge)
is.dup <- tail(is.dup, nrow(merged))
missing <- merged[!is.dup,] 

rm(is.dup); rm(merged_sparse)




## Data processing ####

# simply fuels
merged$Fuel <- mapvalues(merged$Fuel.Type..Primary., from=c('Pipeline Natural Gas', 'Coal', 'Natural Gas', 'Other Gas', 'Wood', 'Coal, Natural Gas',
                                                            'Diesel Oil', 'Residual Oil', 'Process Gas', 'Petroleum Coke', 'Other Oil',
                                                            'Other Solid Fuel', 'Other Gas, Pipeline Natural Gas', 'Natural Gas, Pipeline Natural Gas',
                                                            'Coal, Pipeline Natural Gas', 'Coal Refuse', ""), 
                                                     to=c('Natural gas', 'Coal', 'Natural gas', 'Other', 'Wood', 'Other', 
                                                          'Oil', 'Oil', 'Other', 'Other', 'Oil', 
                                                          'Other', 'Natural gas','Natural gas',
                                                          'Other', 'Coal', NA))

# remove dates from unit types
merged$Unit.Type.Mod <- gsub("\\(.*\\)", "", merged$Unit.Type)
merged$Unit.Type.Mod <- trimws(merged$Unit.Type.Mod)

# simplify boiler types
merged$Unit.category <- mapvalues(merged$Unit.Type.Mod, from=c('Tangentially-fired', 'Cell burner boiler', 'Dry bottom wall-fired boiler',
                                                                     'Dry bottom turbo-fired boiler', 'Stoker', 'Dry bottom vertically-fired boiler',
                                                                     'Circulating fluidized bed boiler', 'Cyclone boiler', 'Bubbling fluidized bed boiler',
                                                                     'Wet bottom wall-fired boiler', 'Wet bottom turbo-fired boiler', 'Other boiler',
                                                                     'Integrated gasification combined cycle'), 
                                                              to= c(rep("Boiler", 12), "IGCC"))

merged$Fuel <- droplevels(merged$Fuel)

## Emissions rates ####

ggplot(merged[!is.na(merged$Fuel) & !is.na(merged$Unit.category),], aes(x=CO2..tons./1E6, y=Gross.Load..MW.h./1E6, shape=Fuel, color=Unit.category)) + geom_point() + theme_classic() +
  xlab("Annual CO2 emissions (million tons)") + ylab("Annual generation (TWh)") + guides(color=guide_legend(title="Unit type")) +
geom_smooth(method='lm', formula= y ~ x, se=F)      

setwd(paste(baseWD,"Plots", "CEMS", sep="/"))
ggsave("linear regression CO2 and generation.pdf", width=8)

with(CEMS, plot(Operating.Time, Gross.Load..MW.h.))

#ggplot(CEMS, aes(x=SO2..tons., y=Gross.Load..MW.h., color=Fuel, shape=Unit.category)) + geom_point() 
#ggplot(CEMS, aes(x=NOx..tons., y=Gross.Load..MW.h., color=Fuel, shape=Unit.category)) + geom_point() 

merged$Fuel2 <- factor(merged$Fuel, levels=c(levels(merged$Fuel), "Natural gas-combined cycle"))
merged$Fuel2 <- factor(ifelse(merged$Unit.category=="Combined cycle", "Natural gas-combined cycle", as.character(merged$Fuel2)))

load.lm <- lm(Gross.Load..MW.h. ~ CO2..tons.:Fuel2, merged)

with(CEMS, plot(CO2..tons., Gross.Load..MW.h., col="black"))
plot(CEMS[is.na(CEMS$Gross.Load..MW.h.), "CO2..tons."], col="red")

# NGCC emissions rates for 2017 additions
merged$dateOfOperation <- as.POSIXct(merged$Commercial.Operation.Date, format="%m/%d/%y")
merged$yearOfOperation <- as.numeric(format(merged$dateOfOperation, "%Y"))

# reset any future dates (actually old turbines that get mislabeled as being in the 21st century)
merged[!is.na(merged$yearOfOperation) & merged$yearOfOperation > 2017, "yearOfOperation"] <- merged[!is.na(merged$yearOfOperation) & merged$yearOfOperation > 2017, "yearOfOperation"] - 100 

ERsummary <- ddply(merged[merged$yearOfOperation >= 2000 & merged$yearOfOperation <= 2017,], ~Fuel2, summarize,
                   weightedCO2rate = sum(CO2..tons., na.rm=T) / sum(Gross.Load..MW.h., na.rm=T),
                   weightedSO2rate = sum(SO2..tons., na.rm=T) / sum(Gross.Load..MW.h., na.rm=T),
                   weightedNOxrate = sum(NOx..tons., na.rm=T) / sum(Gross.Load..MW.h., na.rm=T))

merged$CO2rate <- merged$`CO2..tons.`/ merged$`Gross.Load..MW.h.`
merged$SO2rate <- merged$`SO2..tons.`/ merged$`Gross.Load..MW.h.`
merged$NOxrate <- merged$`NOx..tons.`/ merged$`Gross.Load..MW.h.`


# drop zeros
zeroDrops <- merged[merged$Gross.Load..MW.h. > 0, ]
ddply(zeroDrops, ~Fuel, summarize, avgCO2Rate = mean(CO2rate, na.rm=T))

setwd(paste(baseWD,"Plots", "CEMS", sep="/"))

ggplot(data=merged[merged$CO2rate < 10,], aes(x=CO2rate, fill=Fuel2)) + geom_density(alpha=0.5) + theme_classic() + xlab("CO2 rate (tons per MWh)") + 
  guides(fill=guide_legend(title=""))
ggsave("Emissions rates by fuel - CO2.pdf")

ggplot(data=merged[merged$SO2rate*1E3 < 0.05,], aes(x=SO2rate*1E3, fill=Fuel2)) + geom_density(alpha=0.5) + theme_classic() + xlab("SO2 rate (kg per MWh)")  + 
  guides(fill=guide_legend(title=""))
ggsave("Emissions rates by fuel - SO2.pdf")

ggplot(data=merged[merged$NOxrate < 0.01,], aes(x=NOxrate*1E3, fill=Fuel2)) + geom_density(alpha=0.5) + theme_classic()  + xlab("NOx rate (kg per MWh)")  + 
  guides(fill=guide_legend(title=""))
ggsave("Emissions rates by fuel - NOx.pdf")


merged$Fuel <- factor(merged$Fuel, levels=c("Coal", "Natural gas", "Oil", "Wood", "Other"))

merged[merged$Unit.category == "Combustion turbine" & !is.na(merged$Unit.category) & is.na(merged$Fuel), "Fuel"] <- "Natural gas"

output <- ddply(merged, ~ Fuel + Unit.category, summarize, count = length(Unit.ID), 
                gen = sum(Gross.Load..MW.h., na.rm=T)/1E6, CO2 = sum(CO2..tons., na.rm=T) /1E6, 
                SO2 = sum(SO2..tons., na.rm=T)/1E3, NOx = sum(NOx..tons., na.rm=T)/1E3)

naSums <- t(data.frame(count=nrow(merged), naGen = sum(is.na(merged$Gross.Load..MW.h.)), 
                                         naFuel = sum(is.na(merged$Fuel.Type..Primary.)),
                                         naUnit = sum(is.na(merged$Unit.Type)),
                                         naCO2 = sum(is.na(merged$CO2..tons.)),
                                         naSO2 = sum(is.na(merged$SO2..tons.)),
                                         naNOx = sum(is.na(merged$NOx..tons.))))

rownames(naSums) <- c("Total number in CEMS", "Missing generation data", "Missing primary fuel information", "Missing unit type information",
                      "Missing CO2 emissions", "Missing SO2 emissions", "Missing NOx emissions")
colnames(naSums) <- "Number of units"

# test <- rbind(output, c("Total", "Total", colSums(output[,c("count", "gen", "CO2", "SO2", "NOx")])))


save.image("CEMS data.RData")

## Match with NEEDS data ####

setwd(paste(baseWD,"Data Inputs", sep="/"))
needs <- read.xlsx("needs_v6_rev_9-14-2018.xlsx", sheet="NEEDS v6_Active")

needsMerge <- merge(merged, needs, by.x=c("Facility.ID..ORISPL.", "Unit.ID"), by.y=c("ORIS.Plant.Code", "CAMD.Database.UnitID"))

setwd(paste(baseWD,"Data Inputs", "eia8602017", sep="/"))
eia860 <- read.xlsx("3_1_Generator_Y2017.xlsx", sheet="Operable",startRow=2)

eia860$`Minimum.Load.(MW)` <- as.numeric(eia860$`Minimum.Load.(MW)`)
eia860$`Nameplate.Capacity.(MW)` <- as.numeric(eia860$`Nameplate.Capacity.(MW)`)

eia860$minLoadFrac <- eia860$`Minimum.Load.(MW)` / eia860$`Nameplate.Capacity.(MW)`

ggplot(eia860, aes(x=Energy.Source.1, y=minLoadFrac)) + geom_boxplot() + theme_classic()
