library(openxlsx)
library(plyr)
library(ggplot2)
library(reshape2)

needs <- read.xlsx("needs_v6_rev_9-14-2018.xlsx", sheet=1)
cems <- read.csv("CEMS emissions 2017.csv")

# convert short tons to metric
cems$CO2..tons. <- cems$CO2..short.tons. * 0.907185
cems$CO2..short.tons. <- NULL


## Option 1: Partial matching by Unit IDs ####
cemsSub <- cems[,c("Facility.ID..ORISPL.", "Unit.ID", "Unit.Type", "Fuel.Type..Primary.", "Gross.Load..MW.h.", "Operating.Time")]
needsSub <- needs[,c("ORIS.Plant.Code", "Unit.ID", "CAMD.Database.UnitID", "Boiler/Generator/Committed.Unit",  
                     "PlantType", "Firing", "Bottom", "Cogen?", "Modeled.Fuels", "Capacity.(MW)")] 

# some simplifyin modifications to cems data 

cemsSub$Unit.Type <- gsub("\\(.*\\)", "", cemsSub$Unit.Type)
cemsSub$Unit.Type <- trimws(cemsSub$Unit.Type)

# simplify boiler types
cemsSub$Unit.category <- mapvalues(cemsSub$Unit.Type, from=c('Tangentially-fired', 'Cell burner boiler', 'Dry bottom wall-fired boiler',
                                                               'Dry bottom turbo-fired boiler', 'Stoker', 'Dry bottom vertically-fired boiler',
                                                               'Circulating fluidized bed boiler', 'Cyclone boiler', 'Bubbling fluidized bed boiler',
                                                               'Wet bottom wall-fired boiler', 'Wet bottom turbo-fired boiler', 'Other boiler',
                                                               'Integrated gasification combined cycle'), 
                                  to= c(rep("Boiler", 12), "IGCC"))

cemsSub$averagePower <- round(cemsSub[,"Gross.Load..MW.h."] / cemsSub[,"Operating.Time"])
colnames(needsSub)[colnames(needsSub) == "Boiler/Generator/Committed.Unit"] <- "Unit.Type"
colnames(needsSub)[colnames(needsSub) == "CAMD.Database.UnitID"] <- "CAMD.Unit"

# initial pass at matching
cemsSub$Unit.ID <- as.character(cemsSub$Unit.ID)

# manual ID adjustments (based on ruling out units using generation)
needsSub[needsSub$ORIS.Plant.Code == 1391 & needsSub$Unit.ID == "4A" & needsSub$PlantType == "O/G Steam", "Unit.ID"] <- "4A_1"
needsSub[needsSub$ORIS.Plant.Code == 1391 & needsSub$Unit.ID == "5A" & needsSub$PlantType == "O/G Steam", "Unit.ID"] <- "5A_1"
needsSub[needsSub$ORIS.Plant.Code == 2511 & needsSub$Unit.ID == "10" & needsSub$PlantType == "Combustion Turbine", "Unit.ID"] <- "10_1"
needsSub[needsSub$ORIS.Plant.Code == 4042 & needsSub$Unit.ID == "3" & needsSub$PlantType == "Combustion Turbine", "Unit.ID"] <- "3_1"

# adjust dates (excel mistakes 2-1 for Feb 1 in csv)
# cemsSub[grepl("-[A-Z][a-z]", cemsSub$Unit.ID), ]
cemsSub$Unit.ID <- mapvalues(cemsSub$Unit.ID, from = c("1-Jan", "2-Jan", "1-Feb", "2-Feb", "1-Mar", "2-Mar", "1-Apr", "2-Apr", "1-Jun", "2-Jun", "1-Jul", "2-Jul" ),
                                              to = c("1-1", "1-2", "2-1", "2-2", "3-1", "3-2", "4-1", "4-2", "6-1", "6-2", "7-1", "7-2"))

# merge on ORIS and unit IDs
merged <- merge(cemsSub, needsSub, by.x=c("Facility.ID..ORISPL.", "Unit.ID"), by.y=c("ORIS.Plant.Code", "Unit.ID"))
unmerged <- merge(cemsSub, needsSub, all.x=T, by.x=c("Facility.ID..ORISPL.", "Unit.ID"), by.y=c("ORIS.Plant.Code", "Unit.ID"))
unmerged <- unmerged[is.na(unmerged$`Capacity.(MW)`),]

# total should equal nrow(cemsSubs)
nrow(merged) + nrow(unmerged)

# write to excel for matching using crosswalk
# write.csv(unmerged[,c("Facility.ID..ORISPL.", "Unit.ID")], "Unit ID crosswalk V2.csv", row.names=F)

# function to compare units from different data sets
compareUnits <- function(fips){
  print("CEMS:")
  print(cemsSub[cemsSub$Facility.ID..ORISPL. == fips,])
  
  print("NEEDS:")
  print(needsSub[needsSub$ORIS.Plant.Code == fips,])
}

# read in crosswalk
cross <- read.xlsx("Unit ID crosswalk.xlsx")

# merge with needs to add capacity values
cross <- merge(cross, needsSub[,c("ORIS.Plant.Code", "Unit.ID", "Capacity.(MW)")], all.x=T, 
               by.x=c("ORISPL", "Unit.ID.NEEDS"), by.y=c("ORIS.Plant.Code", "Unit.ID"))

# split NEEDS capacity across shared units
cross <- ddply(cross, ~ ORISPL + Unit.ID.NEEDS, transform, capacityMod = sum(`Capacity.(MW)`)/length(Unit.ID.NEEDS)^2)

# summarize by facility ID and CEMS unit number
crossSum <- ddply(cross, ~ ORISPL + Unit.ID.CEMS, summarize, capacity = sum(capacityMod))

# add to unmerged list
unmerged$merged <- "Y"
crossSum$cross <- "Y"
unmerged <- merge(unmerged, crossSum, all=T, by.x=c("Facility.ID..ORISPL.", "Unit.ID"), by.y=c("ORISPL", "Unit.ID.CEMS"))

# check to make sure all entries have a match
mismatch <- unmerged[is.na(unmerged$merged) | is.na(unmerged$cross), ]

unmerged[,"Capacity.(MW)"] <- unmerged$capacity
combined <- rbind(merged[,c("Facility.ID..ORISPL.", "Unit.ID", "Capacity.(MW)")], unmerged[,c("Facility.ID..ORISPL.", "Unit.ID", "Capacity.(MW)")])
rm(merged); rm(unmerged)

# drop any extra from crosswalk that have already been merged (updated dates an issue)
combined <- combined[!duplicated(combined[,c("Facility.ID..ORISPL.", "Unit.ID")]),]
rm(mismatch)

# check against original CEMS data
cemsSub <- cemsSub[order(cemsSub$Facility.ID..ORISPL., cemsSub$Unit.ID),]
cemsSub$Facility.ID..ORISPL. <- as.numeric(cemsSub$Facility.ID..ORISPL.)

combined <- combined[order(combined$Facility.ID..ORISPL., combined$Unit.ID),]

all.equal(cemsSub[,c("Facility.ID..ORISPL.", "Unit.ID")], combined[,c("Facility.ID..ORISPL.", "Unit.ID")])
sum(cemsSub$Facility.ID..ORISPL. == combined$Facility.ID..ORISPL.)
sum(cemsSub$Unit.ID == combined$Unit.ID)

cemsSub <- merge(cemsSub, combined, all=T, by=c("Facility.ID..ORISPL.", "Unit.ID"))

# deal with remaining missing units
nrow(cemsSub[is.na(cemsSub$`Capacity.(MW)`),])

# check for patterns in operating time / capacity
# ggplot(cemsSub, aes(x=Operating.Time, y=`Gross.Load..MW.h.`, size=`Capacity.(MW)`)) + facet_wrap(~Unit.category) + geom_point()

# take average capacity by unit type and use value to replace missing numbers
cemsSub <- ddply(cemsSub, ~ Unit.Type + Fuel.Type..Primary., transform, avgCap = round(median(`Capacity.(MW)`, na.rm=T)))
cemsSub$capacityFinal <- ifelse(is.na(cemsSub[, "Capacity..MW."]), cemsSub[,"avgCap"], cemsSub[, "Capacity..MW."] )

# second round (missing a few unit type + fuel combinations)
cemsSub <- ddply(cemsSub, ~ Unit.Type, transform, avgCap2 = round(median(Capacity..MW., na.rm=T)))
cemsSub$capacityFinal <- ifelse(is.na(cemsSub[, "capacityFinal"]), cemsSub[,"avgCap2"], cemsSub[, "capacityFinal"] )


# check for units where max capacity is lower than average power
sum(cemsSub$capacityFinal < cemsSub$averagePower, na.rm=T)   # 461 units
View(cemsSub[!is.na(cemsSub$averagePower) & cemsSub$capacityFinal < cemsSub$averagePower, ])

# for these, replace capacity
cemsSub$capacityFinal <- ifelse(!is.na(cemsSub$averagePower) & cemsSub$capacityFinal < cemsSub$averagePower, cemsSub$averagePower, cemsSub$capacityFinal)

# calculate max and minimun operating loads for the year
# minimun load fractions: Jenkins et al. 2018, Salazar et al. 2017


# write output to file
write.xlsx(cemsSub[,c("Facility.ID..ORISPL.", "Unit.ID", "capacityFinal")], "Unit capacity values.xlsx")







## Option 2: Allocate capacity based on generation ####
# Note: I'm not super confident this works...going with option 1 for now

# gen <- cems[,c("Facility.Name", "Facility.ID..ORISPL.", "Unit.ID", "Unit.Type",  "Fuel.Type..Primary.", "Gross.Load..MW.h.", 
#                "Heat.Input..MMBtu.", "Operating.Time", "CO2..tons.", "X..of.Months.Reported")]
# 
# 
# # 97% of units report data for all 12 months
# 
# 
# # regression of CO2 and load for interpolation
# # remove dates from unit types
# gen$Unit.Type <- gsub("\\(.*\\)", "", gen$Unit.Type)
# gen$Unit.Type <- trimws(gen$Unit.Type)
# 
# # simplify fuels
# gen$Fuel <- mapvalues(gen$Fuel.Type..Primary., from=c('Pipeline Natural Gas', 'Coal', 'Natural Gas', 'Other Gas', 'Wood', 'Coal, Natural Gas',
#                                                       'Diesel Oil', 'Residual Oil', 'Process Gas', 'Petroleum Coke', 'Other Oil',
#                                                       'Other Solid Fuel', 'Other Gas, Pipeline Natural Gas', 'Natural Gas, Pipeline Natural Gas',
#                                                       'Coal, Pipeline Natural Gas', 'Coal Refuse', ""), 
#                       to=c('Natural gas', 'Coal', 'Natural gas', 'Other', 'Wood', 'Other', 
#                            'Oil', 'Oil', 'Other', 'Other', 'Oil', 
#                            'Other', 'Natural gas','Natural gas',
#                            'Other', 'Coal', NA))
# 
# gen$Fuel <- factor(gen$Fuel, levels=c(levels(gen$Fuel), "Natural gas-combined cycle"))
# gen$Fuel <- factor(ifelse(gen$Unit.Type=="Combined cycle", "Natural gas-combined cycle", as.character(gen$Fuel)))
# gen$Fuel.Type..Primary. <- NULL
# 
# load.lm <- lm(Gross.Load..MW.h. ~ CO2..tons.:Fuel, gen)
# # 20% of units appear to be underreporting CO2 emissions
# 
# # add results using interpolation
# gen$predictedLoad <- predict(load.lm, gen)
# gen$predictedLoad[gen$predictedLoad < 0] <- NA
# 
# gen$Gross.Load..MW.h. <- ifelse(is.na(gen$Gross.Load..MW.h.), gen$predictedLoad, gen$Gross.Load..MW.h.)
# gen$predictedLoad <- NULL
# 
# 
# # extrapolate for units that are missing months of data
# gen$Gross.Load..MW.h. <- gen$Gross.Load..MW.h. / gen$X..of.Months.Reported * 12
# 
# cemsPlants <- ddply(gen, ~ Facility.ID..ORISPL., summarize, loadPlant=sum(Gross.Load..MW.h., na.rm=T), timePlant=sum(Operating.Time, na.rm=T))
# needsPlants <- ddply(needs, ~ ORIS.Plant.Code, summarize, capacityPlant=sum(`Capacity.(MW)`, na.rm=T))
# 
# # merge in capacity values by plant
# cemsPlants <- merge(cemsPlants, needsPlants, all.x=T, by.x="Facility.ID..ORISPL.", by.y="ORIS.Plant.Code")
# 
# # add plant level generation and capacity to unit level 
# gen <- merge(gen, cemsPlants, by="Facility.ID..ORISPL.", all.x=T)
# 
# # calculate fraction of generation
# gen$genFraction <- gen$Gross.Load..MW.h. / gen$loadPlant
# 
# gen$capFraction <- gen$genFraction * gen$capacityPlant             # MW
# gen$averagePower <- gen$Gross.Load..MW.h. / gen$Operating.Time     # in MWh / h = MW
# 
# gen <- gen[order(gen$averagePower),]
# gen$uniqueID <- 1:nrow(gen)
# 
# plotGen <- melt(gen, id.vars="uniqueID", measure.vars = c("capFraction", "averagePower"))
# ggplot(plotGen, aes(x=uniqueID, y=value, color=variable)) + geom_line() + theme_classic()
# 
# 








 