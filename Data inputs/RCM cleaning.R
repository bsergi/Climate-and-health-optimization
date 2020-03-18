# RCM MD cleaning
# Brian Sergi
# November 7, 2018
library(tidyr)

mdCleaning <- function(filename, dr){
  mds <- read.csv(filename)       # load RCM results
  mds <- mds[mds$season == "annual" & mds$elevated == "high stack", ]     # select annual averages
  mds <- mds[, c("fips", "pollutant", "model", "damage")]                 # subset columns
  mds$pollutant <- mapvalues(mds$pollutant, from=c("so2", "nox"), to=c("SO2", "NOx"))  # rename columns
  mds <- dcast(mds, fips ~ pollutant + model, value.var = "damage")                    # convert to wide format
  colnames(mds)[-1] <- paste(colnames(mds)[-1], dr, sep="_")              # add dose response information
  return(mds)
}

acs <- mdCleaning("RCMs-VSL-2017-ACS.csv", dr="ACS")
h6c <- mdCleaning("RCMs-VSL-2017-H6C.csv", dr="H6C")

combined <- merge(acs, h6c, by="fips", all=T)

write.csv(combined, "Final MDs combined.csv", row.names = F)         # save results
