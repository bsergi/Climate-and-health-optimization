# Map plotting functions

library(maps)
library(mapproj)
library(gridExtra)
library(grid)
library(tidyr)

# extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

buildMapData <- function(){
  all_counties <- map_data("county", projection  = "albers", par = c(30,0))
  all_counties$polyname <- paste(all_counties$region, all_counties$subregion, sep = ",")
  all_counties <- merge(all_counties, county.fips, by="polyname", all.x=T)    # county.fips comes with map package
  # some fips code adjustments
  all_counties[!is.na(all_counties$fips) & all_counties$fips == 8014 , "fips"] <- 8013     # Broomfield, CO
  all_counties[all_counties$polyname == "florida,okaloosa", "fips"] <- 12091
  all_counties[all_counties$polyname == "virginia,accomack", "fips"] <- 51001
  all_counties[all_counties$polyname == "louisiana,st martin", "fips"] <- 22099
  all_counties[all_counties$polyname == "washington,pierce", "fips"] <- 53053
  all_counties[all_counties$polyname == "texas,galveston", "fips"] <- 48167
  all_counties[all_counties$polyname == "north carolina,currituck", "fips"] <- 37053
  all_counties[all_counties$polyname == "washington,san juan", "fips"] <- 53055
  return(all_counties)
}

# function to bin points for plotting (accepts either number of quantiles or set breaks)
mapToBins <- function(vector.df, quants=5, set.breaks){
  if(is.null(set.breaks)){
    quant.breaks <- quantile(vector.df, seq(0, 1, length.out = quants), na.rm=T)
  } else {
    quant.breaks <- set.breaks
    quants <- length(set.breaks)
  }
  factor.labels <- rep(paste0("[", signif(quant.breaks[1], 2), ", ", signif(quant.breaks[2],2), "]"), length(vector.df))
  factor.levels <- paste0("[", signif(quant.breaks[1], 2), ", ", signif(quant.breaks[2],2),"]")
  for(i in 2:(quants-1)){
    factor.labels <- ifelse(vector.df > quant.breaks[i], 
                            paste0("[", signif(quant.breaks[i], 2), ", ", signif(quant.breaks[i+1],2), "]"),
                            factor.labels)
    factor.levels <- c(factor.levels, paste0("[", signif(quant.breaks[i], 2), ", ", signif(quant.breaks[i+1],2), "]"))
  }
  # set order
  factor.labels <- factor(factor.labels, levels = factor.levels)
  return(factor.labels)
}


## SR mapping functions ####

# function to plot map of damages 
countyDamagesMap <- function(damages, targetVar, legend.title, title, millions=T, limits=NULL, countyBorder=T,
                             set.breaks=NULL, state.subset=NULL, discrete=T, quants=5, cols=NULL, save=T){
  
  damages <- damages[!is.na(damages[,targetVar]),]
  
  all_counties <- buildMapData()
  damages[!is.na(damages$fips) & damages$fips == 12025, "fips"] <- 12086    # Miami-Dade, FL
  
  # subset states
  if(!is.null(state.subset)){
    all_counties <- subset(all_counties, region %in% state.subset)
  }
  county.plot <- merge(all_counties, damages, by="fips", all=T)
  
  # sort
  county.plot <- county.plot[order(county.plot$group, county.plot$order),]
  
  # convert damages to millions
  if(millions){
    county.plot[,targetVar] <- county.plot[,targetVar] / 1E6
  }

  if(discrete){
    county.plot$plot.cat <- mapToBins(county.plot[,targetVar], quants = quants, set.breaks)
    if(is.null(cols)){
      cols <- c("#f1eef6", "#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d")[1:quants]
    }
    
    # County choropleth map
    g.plot <- ggplot() + 
      geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=plot.cat), 
                   colour=NA) + 
      scale_fill_manual(values=cols, na.value="purple") + theme_bw()  +
      labs(fill = legend.title, title = "", x="", y="") + 
      scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

  # linear scale
  } else {     
    if(is.null(cols)){
      cols <- c("#2b8cbe", "#de2d26")
    }
    
    if(countyBorder==F){
      border <-0
      borderColor <- NA
    } else{
      border <- 0.01
      borderColor <- "black"
    }
    county.plot$target <- county.plot[,targetVar]
    g.plot <- ggplot() + 
      geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=target), 
                   colour=borderColor, size=border) + 
      scale_fill_gradient2(low=cols[1], high=cols[2], midpoint=0, na.value="white") + theme_bw()  +
      labs(fill = legend.title, title = "", x="", y="") + 
      scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  }
  # add state borders
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  # subset states
  if(!is.null(state.subset)){
    all_states <- subset(all_states, region %in% state.subset)
  }
  g.plot <- g.plot + geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black")
  if(save){
    ggsave(paste0(title, ".pdf"), g.plot, width = 8, height = 4.5)
  }
  return(g.plot)
}

# function to plot map of damages 
bubbleMap <- function(damages, targetVar, legend.title, title, millions=T, limits=NULL, countyBorder=T,
                               state.subset=NULL, save=T, cols=c("#132B43", "#56B1F7"), breakPoints=c(5,10,15,20)){
  
  damages <- damages[!is.na(damages[,targetVar]),]
  all_counties <- buildMapData()
  damages[!is.na(damages$fips) & damages$fips == 12025, "fips"] <- 12086    # Miami-Dade, FL
  
  # subset states
  if(!is.null(state.subset)){
    all_counties <- subset(all_counties, region %in% state.subset)
  }

  # county centroids
  centroids <- aggregate(cbind(long, lat) ~ subregion + fips, data=all_counties, FUN=function(x)mean(range(x)))
  centroids <- merge(centroids, damages, by="fips", all.y=T)
  
  if(countyBorder==F){
    border <-0
    borderColor <- NA
  } else{
    border <- 0.01
    borderColor <- "black"
  }
  
  # convert to millions
  if(millions){
    centroids[,targetVar] <- centroids[,targetVar] / 1E6
  }
  
  # replace zeros with NAs
  centroids[,targetVar] <- ifelse(centroids[,targetVar]==0, NA, centroids[,targetVar])
  
  # replace in easily named variable
  centroids$target <- centroids[,targetVar]
  
  g.plot <- ggplot() + 
    geom_point(data=centroids, aes(x=long, y=lat, color=target, size=target), alpha=0.9) +
    scale_color_continuous(legend.title, high=cols[1], low=cols[2], breaks=breakPoints) + 
    scale_size_continuous(legend.title, breaks=breakPoints) +
    labs(title = "", x="", y="") + theme_bw() +
    guides(color = guide_legend(), size = guide_legend()) +
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  
  # add state borders
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  # subset states
  if(!is.null(state.subset)){
    all_states <- subset(all_states, region %in% state.subset)
  }
  g.plot <- g.plot + geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black")
  if(save){
    ggsave(paste0(title, ".pdf"), g.plot, width = 8, height = 4.5)
  }
  return(g.plot)
}


# reads receptor damages by model and dose response
readDamages <- function(md, dr, suffix=""){
  setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
  damages <- read.csv(paste0("Results - source receptor ", md, " ", dr, suffix, ".csv"))
  # drop all but total health damages and convert to wide
  damages <- damages[,c("FIPS.Code", "Health.Damages....", "Scenario")]
  colnames(damages) <- c("fips", "damages", "scenario")
  damages <- spread(damages, key="scenario", value="damages")
  
  colnames(damages)[colnames(damages) == "Climate"] <- "Climate-only"
  
  return(damages)
}

srAttribution <- function(scenario){
  setwd(paste(baseWD, "Results", "SR", scenario, sep="/"))
  damages <- matrix(0, nrow=3109, ncol=3109)
  
  for (p in c("SO2", "NOx")){
    pollutant <- read.csv(paste(p, "emissions.csv"), header=F)
    colnames(pollutant) <- c("fips", "emissions")
    pollutant_SR <- read.csv(paste(p, "SR.csv"))
    
    rownames(pollutant_SR) <- pollutant_SR[,1]; pollutant_SR[,1] <- NULL
    damages <- damages + sweep(pollutant_SR, pollutant$emissions, MARGIN=1, FUN="*")
    
    if(p == "SO2"){
      emissions <- pollutant
    } else {
      emissions <- merge(emissions, pollutant, by="fips")
      colnames(emissions) <- c("fips", "SO2", "NOx")
    }
  }
  colnames(damages) <- rownames(damages)
  return(damages)
}

aggregateByRegion <- function(damageMatrix, mapping, indx){
  
  if (!(identical(factor(rownames(damageMatrix)), mapping[,"fips"]) & 
        identical(factor(colnames(damageMatrix)), mapping[,"fips"]))){
    print("Error: fips code do not match.")
    return(NULL)
  } 
  # sum by sources 
  reducedMatrix <- t(sapply(by(damageMatrix, mapping[,indx], colSums), identity))
  # invert
  reducedMatrix <- t(reducedMatrix)
  # sum by receptors
  reducedMatrix <- t(sapply(by(reducedMatrix, mapping[,indx], colSums), identity))
  # invert
  reducedMatrix <- t(reducedMatrix)
  return(reducedMatrix)
}

calcPercent <- function(damageMatrix){
  
  damagesIncurred <- colSums(damageMatrix)
  damagesCaused <- c(rowSums(damageMatrix), sum(damagesIncurred))
  
  damageMatrix <- rbind(damageMatrix, damagesIncurred)
  damageMatrix <- cbind(damageMatrix, damagesCaused)
  
  # sweeps matrix by row 
  damages <- round(sweep(damageMatrix, 1, damagesCaused, "/"), 3) * 100
  return(damages)
}



plotDamageMaps <- function(damages, md, dr, sub="Source receptor maps", suffix=""){
  
  linearColors <- c('#fef0d9','#fdd49e','#fdbb84','#fc8d59','#e34a33','#b30000')
  quants <- quantile(damages[,"Baseline"], probs=c(0, 0.2, 0.4, 0.6, 0.8, 1))
  quants <- signif(quants/1E6, 1)                     # quantiles given to 1 signficant digits in millions
  # quants <- c(0, 1, 5, 10, 20, 100, 1300)
  
  plots <- list()
  for (col in colnames(damages)[-1]){
    i <- which(col == colnames(damages)[-1])
    plots[[i]] <- countyDamagesMap(damages, targetVar=col, 
                     legend.title = "Health damages\n(million $)", 
                     title=paste0("SR Map - ", col),
                     millions = TRUE, set.breaks = quants,
                     cols = linearColors, save=F)
  }
  
  # Results as panel figure
  g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Baseline"))
  g2 <- arrangeGrob(plots[[2]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Climate"))
  g3 <- arrangeGrob(plots[[3]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Health + Climate"))

  leg <- g_legend(plots[[1]])
  
  layoutMatrix <- rbind(c(1, NA),
                        c(2, 3),
                        c(4, NA))
                    
  panelFig <- grid.arrange(g1, g2, leg, g3, layout_matrix=layoutMatrix, widths=c(1, 0.3))
  
  setwd(paste(baseWD, "Plots", sub,  sep="/"))
  ggsave(paste0("Source receptor map ", md, " ", dr, suffix, ".pdf"), panelFig, height = 8, width = 5)
  rm(g1); rm(g2); rm(g3); rm(leg); rm(panelFig)
}


## State-wide emissions levels ####

summarizeStateEmissionsRates <- function(md, dr){
  setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
  plants <- read.csv(paste0("Results - plants ", md, " ", dr,  ".csv"))
  gas <- read.csv(paste0("Results - gas ", md, " ", dr,  ".csv"))
  
  baselinePlants <- read.csv(paste0("Baseline - plants ", md, " ", dr,  ".csv"))
  baselinePlants$Scenario <- "Baseline"
  
  # subset
  plants <- subset(plants, select=c(Scenario, FIPS.Code, Gross.Load..MW.h., CO2..tons., SO2..tons., NOx..tons.))
  gas <- subset(gas, select=c(Scenario, FIPS.Code, Gross.Load..MW.h., CO2..tons., SO2..tons., NOx..tons.))
  baselinePlants <- subset(baselinePlants, select=c(Scenario, FIPS.Code, Gross.Load..MW.h., CO2..tons., SO2..tons., NOx..tons.))
  
  # combined new and old plants
  plants <- rbind(baselinePlants, plants, gas)
  colnames(plants)[colnames(plants) == "FIPS.Code"] <- "fips"
  
  # summarize by county
  countySums <- ddply(plants, ~ Scenario + fips, colwise(sum))
  countySums <- merge(countySums, stateFips[,c('fips', 'state')], by="fips", all.x=T)
  
  # summarize by state
  stateSums <- ddply(countySums, ~ Scenario + state, colwise(sum))
  stateSums$fips <- NULL

  return(stateSums)  
} 

readCensusInfo <- function(state=F){
  setwd(paste(baseWD, "Data inputs", sep="/"))
  
  # read census fips code for state summary
  censusFips <- read.csv("Census FIPS codes.txt", header=FALSE, colClasses = "character")
  colnames(censusFips) <- c("state", "state.num", "county.num", "county.name", "fips.class")
  
  # create new column with full fips format (state + county)
  censusFips$fips <- as.numeric(with(censusFips, paste(state.num, county.num, sep = "")))
  censusFips$county.num <- as.numeric(censusFips$county.num)
  
  if(state){
    stateAbbrevs <- read.csv("State abbreviations.csv", header=F)
    colnames(stateAbbrevs) <- c("region", "state")                    # named region to match with map in ggplot
    levels(stateAbbrevs$region) <- tolower(levels(stateAbbrevs$region))
    
    censusFips <- merge(censusFips, stateAbbrevs, all.x=T, by="state")    
  }

  return(censusFips)
}

loadPopData <- function(fips){
  setwd(paste(baseWD, "Data inputs", "ACS_17_5YR_B01003", sep="/"))
  
  pop <- read.csv("ACS_17_5YR_B01003_with_ann.csv", skip=1)
  pop <- pop[,c("Id2", "Estimate..Total")]
  colnames(pop) <- c("fips", "pop")
  
  fips <- merge(fips, pop, by="fips", all.x=T)
  
  setwd(baseWD)
  return(fips)
  
}

stateEmissionsRateMap <- function(df, pollutant, scaleLims, units=1, var="rate"){
  # load map of states dataframe
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  # merge with data
  map.plot <- merge(all_states, df, by="region", all=T)
  # reorder so lines don't get messed up
  map.plot  <- map.plot[order(map.plot$order),]
  
  if (pollutant == "gen"){
    legTitle <- paste0("Annual\nTWh")
    units <- 1E6
    maxCol <- "darkblue"
  } else{
    if (pollutant == "CO2"){
      legTitle <- paste0("Average ", pollutant, "\nemissions rate\n(tons/MWh)")
    } else{
      legTitle <- paste0("Average ", pollutant, "\nemissions rate\n(tons/GWh)")
      units <- 1E-3
    }
    
    plotColors <- c("darkblue", "darkred", "darkgreen")
    maxCol <- plotColors[which(c("CO2", "SO2", "NOx") == pollutant)]
  }
  
  # convert units if specified
  map.plot$var = map.plot[,var] / units
  scaleLims <- scaleLims / units

  g.plot <- ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group=group, fill=var), colour="black") + 
    scale_fill_continuous(low = "white", high = maxCol, guide="colorbar", limits=scaleLims, na.value="white") + theme_bw()  + 
    labs(fill = legTitle, title = "", x="", y="") + 
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  
  # add state borders
  # g.plot <- g.plot + geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black")
  return(g.plot)
}

stateEmissionsMapPanel <- function(stateEmissions, pollutant, md, dr){
  stateEmissions$rate <- stateEmissions[,paste0(pollutant, "..tons.")] / stateEmissions[,"Gross.Load..MW.h."]
  scaleLims <- c(0, max(stateEmissions$rate))
  
  plots <- list(); i <- 1
  for (scenario in c("Baseline", "Climate", "Health + Climate")){
    damageSub <- stateEmissions[stateEmissions$Scenario == scenario,]
    plots[[i]] <- stateEmissionsRateMap(damageSub, pollutant, scaleLims)
    i <- i + 1
  }
  
  # Results as panel figure
  g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Baseline"), 
                    plots[[2]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Climate"), ncol=3, widths=c(1, 1, 0.25))
  g2 <- arrangeGrob(plots[[3]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Health"), 
                    plots[[4]] + theme(plot.title = element_text(face="bold")) + ggtitle("Climate + Health"), ncol=2, widths=c(1, 1.25))
  
  panelFig <- grid.arrange(g1, g2, nrow = 2, heights = c(0.5, 0.5))
  
  setwd(paste(baseWD, "Plots", sep="/"))
  ggsave(paste0("State emissions rates ", pollutant, " ", md, " ", dr, ".pdf"), panelFig, height = 6, width = 10)
  rm(g1); rm(g2); rm(panelFig)
  setwd(baseWD)
}


plotStateEmissionsChanges <- function(stateData){
  
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  
  # merge with data
  mapData <- merge(all_states, stateData, by="region")
  legTitle <- expression(paste("Percent of CO"[2], " reduced"))
  
  # subset & plot (climate-only)
  data1 <- mapData[mapData$Scenario == "Climate",]
  data1  <- data1[order(data1$order),]
  plot1 <- ggplot() + 
    geom_polygon(data=data1, aes(x=long, y=lat, group=group, fill=percentChange), colour="black") + 
    scale_fill_continuous(low = "white", high = "darkblue", guide="colorbar", limits=c(0,100), na.value="white") + theme_bw()  + 
    labs(fill = legTitle, title = "", x="", y="") + 
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  
  # subset & plot (climate-only)
  data2 <- mapData[mapData$Scenario == "Health + Climate",]
  data2  <- data2[order(data2$order),]
  plot2 <- ggplot() + 
    geom_polygon(data=data2, aes(x=long, y=lat, group=group, fill=percentChange), colour="black") + 
    scale_fill_continuous(low = "white", high = "darkblue", guide="colorbar", limits=c(0,100), na.value="white") + theme_bw()  + 
    labs(fill = legTitle, title = "", x="", y="") + 
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  
  leg <- g_legend(plot1 + theme(legend.position = "bottom", legend.title = element_text(face="bold")) + 
                    guides(fill=guide_legend(title.position="top")))
  g1 <- arrangeGrob(plot1 + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("a. Climate-only"), 
                    plot2 + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("b. Health + Climate"), 
                    ncol=2, widths=c(1, 1))
  
  panelFig <- grid.arrange(g1, leg, nrow=2, heights=c(1,0.15))
  
  setwd(paste(baseWD, "Plots", "State emissions rates", sep="/"))
  ggsave(paste0("State CO2 reductions.pdf"), panelFig, height = 4, width = 9)
  setwd(baseWD)
  
}



## Coal generation ####

summarizeCoalGen <- function(md, dr, fuel){
  setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
  plants <- read.csv(paste0("Results - plants ", md, " ", dr,  ".csv"))
  gas <- read.csv(paste0("Results - gas ", md, " ", dr,  ".csv"))
  
  baselinePlants <- read.csv(paste0("Baseline - plants ", md, " ", dr,  ".csv"))
  baselinePlants$Scenario <- "Baseline"
  
  # subset
  plants <- subset(plants, subset=c(Fuel==fuel), select=c(Scenario, FIPS.Code, Gross.Load..MW.h.))
  gas <- subset(gas, select=c(Scenario, FIPS.Code, Gross.Load..MW.h.))
  baselinePlants <- subset(baselinePlants, subset=c(Fuel==fuel), select=c(Scenario, FIPS.Code, Gross.Load..MW.h.))
  
  
  if (fuel == "Natural gas"){
    plants <- rbind(baselinePlants, plants, gas)
  } else {
    plants <- rbind(baselinePlants, plants)
  }
  
  # combined new and old plants
  colnames(plants)[colnames(plants) == "FIPS.Code"] <- "fips"
  
  # summarize by county
  countySums <- ddply(plants, ~ Scenario + fips, colwise(sum))
  countySums <- merge(countySums, stateFips[,c('fips', 'region')], by="fips", all.x=T)
  
  # summarize by state
  stateSums <- ddply(countySums, ~ Scenario + region, colwise(sum))
  stateSums$fips <- NULL
  
  return(stateSums)  
  
}

coalGenMapPanel <- function(coalGen, md, dr, fuelName){
  scaleLims <- range(coalGen$Gross.Load..MW.h.)
  
  plots <- list(); i <- 1
  for (scenario in c("Baseline", "Climate", "Health", "Health + Climate")){
    genSub <- coalGen[coalGen$Scenario == scenario,]
    plots[[i]] <- stateEmissionsRateMap(genSub, "gen", scaleLims, var="Gross.Load..MW.h.")
    i <- i + 1
  }
  
  # Results as panel figure
  g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Baseline"), 
                    plots[[2]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Climate"), ncol=3, widths=c(1, 1, 0.25))
  g2 <- arrangeGrob(plots[[3]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Health"), 
                    plots[[4]] + theme(plot.title = element_text(face="bold")) + ggtitle("Climate + Health"), ncol=2, widths=c(1, 1.25))
  
  panelFig <- grid.arrange(g1, g2, nrow = 2, heights = c(0.5, 0.5))
  
  setwd(paste(baseWD, "Plots", sep="/"))
  ggsave(paste0("Total generation ", fuelName, " ", md, " ", dr, ".pdf"), panelFig, height = 6, width = 10)
  rm(g1); rm(g2); rm(panelFig)
  setwd(baseWD)
}



summarizeNewGas <- function(md, dr){
  setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
  gas <- read.csv(paste0("Results - gas ", md, " ", dr,  ".csv"))
  # subset
  gas <- subset(gas, select=c(Scenario, FIPS.Code, Gross.Load..MW.h.))
  # combined new and old plants
  colnames(gas)[colnames(gas) == "FIPS.Code"] <- "fips"
  # summarize by county
  countySums <- ddply(gas, ~ Scenario + fips, colwise(sum))
  countySums <- merge(countySums, stateFips[,c('fips', 'region')], by="fips", all.x=T)
  # summarize by state
  stateSums <- ddply(countySums, ~ Scenario + region, colwise(sum))
  stateSums$fips <- NULL
  return(stateSums)  
}

## Coal retirements maps ####

plotCoalGen <- function(coalGen, md, dr){
  
  plots <- list()
  for (col in colnames(coalGen)[-1]){
    i <- which(col == colnames(coalGen)[-1])
    plots[[i]] <- bubbleMap(coalGen, targetVar=col, 
                                   legend.title = "Annual\ngen. from\ncoal (TWh)", 
                                   title=paste0("Coal gen - ", col),
                                   millions = TRUE, save=F, countyBorder=F)
  }
  
  # Results as panel figure
  g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Baseline"), 
                    plots[[2]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Climate"), ncol=3, widths=c(1, 1, 0.25))
  g2 <- arrangeGrob(plots[[3]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Health"), 
                    plots[[4]] + theme(plot.title = element_text(face="bold")) + ggtitle("Health + Climate"), ncol=2, widths=c(1, 1.25))
  
  panelFig <- grid.arrange(g1, g2, nrow = 2, heights = c(0.5, 0.5))
  
  setwd(paste(baseWD,"Plots", "Coal retirements", sep="/"))
  ggsave(paste0("Coal generation ", md, " ", dr, ".pdf"), panelFig, height = 6, width = 10)
  rm(g1); rm(g2); rm(panelFig)
}


combinedPlotFunction <- function(coalGen, damages, targetVar, set.breaks, colors, maxBase, countyBorder=T, millions=T){
  all_counties <- buildMapData()

  damages <- damages[!is.na(damages[,targetVar]),]
  coalGen <- coalGen[!is.na(coalGen[,targetVar]),]
  
  damages[!is.na(damages$fips) & damages$fips == 12025, "fips"] <- 12086    # Miami-Dade, FL
  coalGen[!is.na(coalGen$fips) & coalGen$fips == 12025, "fips"] <- 12086    
  
  county.plot <- merge(all_counties, damages, by="fips", all=T)
  county.plot <- county.plot[order(county.plot$group, county.plot$order),]
  
  # county centroids
  centroids <- aggregate(cbind(long, lat) ~ subregion + fips, data=all_counties, FUN=function(x)mean(range(x)))
  centroids <- merge(centroids, coalGen, by="fips", all.y=T)
  
  # aggregate coal plant generation by state for reporting
  #centroids <- aggregate(cbind(long, lat) ~ region, data=all_counties, FUN=function(x)mean(range(x)))
  #coalGen <- merge(coalGen, stateFips[,c("fips", "region")], by="fips", all.x=T)
  #coalGen <- ddply(coalGen, ~ region, summarize, Baseline=sum(Baseline), `Climate-only`=sum(`Climate-only`), `Health + Climate`=sum(`Health + Climate`))
  #centroids <- merge(centroids, coalGen, by="region", all.y=T)
  #maxBase <- max(coalGen$Baseline) / 1E6
  
  # manual adjustment to a few state centroids
  #centroids[centroids$region == "florida", "long"] <- centroids[centroids$region == "florida", "long"] + 0.03
  #centroids[centroids$region == "louisiana", "long"] <- centroids[centroids$region == "louisiana", "long"] - 0.015
  #centroids[centroids$region == "michigan", "long"] <- centroids[centroids$region == "michigan", "long"] + 0.03
  #centroids[centroids$region == "virginia", "long"] <- centroids[centroids$region == "virginia", "long"] + 0.01
  
  # convert damages to millions
  if(millions){
    county.plot[,targetVar] <- county.plot[,targetVar] / 1E6
    centroids[,targetVar] <- centroids[,targetVar] / 1E6
  }
  
  # settings for county lines
  if(countyBorder==F){
    border <-0
    borderColor <- NA
  } else{
    border <- 0.01
    borderColor <- "black"
  }
  
  # replace zeros with NAs
  centroids[,targetVar] <- ifelse(centroids[,targetVar]==0, NA, centroids[,targetVar])
  
  # replace in easily named variables
  county.plot$plot.cat <- mapToBins(county.plot[,targetVar], quants=5, set.breaks)
  centroids$target <- centroids[,targetVar]
  
  # County choropleth map
  g.plot <- ggplot() + 
    geom_polygon(data=county.plot, aes(x=long, y=lat, group= group, fill=plot.cat), 
                 colour=borderColor, size=border) + 
    scale_fill_manual(values=colors, na.value="purple") + theme_bw()  +
    labs(fill = "Annual health\ndamages incurred\n(million $)", title = "", x="", y="") + 
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  
  # add state borders
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  g.plot <- g.plot + geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "grey")
  
  # add in centroids
  g.plot <- g.plot + geom_point(data=centroids, aes(x=long, y=lat, color=target, size=target), shape=1, alpha=1, stroke=0.8) + # stroke=0.4, shape=1
    scale_color_continuous("Annual coal\ngeneration (TWh)", high = "#132B43", low = "#56B1F7", limits=c(0, maxBase), breaks=c(5,10,15,20)) + # breaks=c(5,10,15,20) 
    scale_size_continuous("Annual coal\ngeneration (TWh)", limits=c(0, maxBase), breaks=c(5,10,15,20), range=c(0.25,6)) + # breaks=c(5,10,15,20) # breaks=c(25,75,125)
    guides(color = guide_legend(), size = guide_legend())


  
  return(g.plot)
}


## Coal retirements with SR maps ####
plotCoalGenWithSR <- function(coalGen, damages, md, dr, sub="Coal retirements + damages", suffix=""){
  
  linearColors <- c('#fef0d9','#fdd49e','#fdbb84','#fc8d59','#e34a33','#b30000')
  quants <- quantile(damages[,"Baseline"], probs=c(0, 0.2, 0.4, 0.6, 0.8, 1))
  quants <- signif(quants/1E6, 1)                     # quantiles given to 1 signficant digits in millions
  
  maxBase <- max(coalGen$Baseline) / 1E6
  
  plots <- list()
  
  for (col in colnames(coalGen)[-1]){
    i <- which(col == colnames(coalGen)[-1])
    plots[[i]] <- combinedPlotFunction(coalGen, damages, targetVar=col, set.breaks=quants, maxBase=maxBase, 
                                       colors=linearColors, countyBorder=F, millions=T)
  }
  
  # Results as panel figure
  
  g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("a. Baseline")) 
  g2 <- arrangeGrob(plots[[2]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("b. Climate-only"))
  g3 <- arrangeGrob(plots[[3]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("c. Health + Climate"))
  
  leg <- g_legend(plots[[1]] + theme(legend.box = "horizontal"))
  
  # layoutMatrix <- rbind(c(1, 4),
  #                       c(2, 4),
  #                       c(3, 4))
  # 
  
  layoutMatrix <- rbind(c(1, 2),
                        c(3, 4))

  
  panelFig <- grid.arrange(g1, g2, g3, leg, layout_matrix=layoutMatrix)
  
  setwd(paste(baseWD,"Plots", sub, sep="/"))
  ggsave(paste0("Coal generation with damages - ", md, " ", dr, suffix, ".pdf"), panelFig, height=7, width=10)
  #ggsave(paste0("Coal generation with damages - ", md, " ", dr, suffix, ".pdf"), panelFig, height = 8, width = 10)
  
}


plotInitialDamagesByPlant <- function(plants, md, dr, suffix=""){
  
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  
  # convert plant coordinates
  converted.coords <- mapproject(plants$long, plants$lat)
  plants$longProj <- converted.coords$x
  plants$latProj <- converted.coords$y
  
  # drop plant off of the map
  plants <- plants[plants$Facility.ID..ORISPL. != plants[which.min(plants$latProj), "Facility.ID..ORISPL."], ]
  
  plotColors <- colorRampPalette(brewer.pal(9,"Blues"))(10)
  
  # range for scale
  low <- min(plants$health, plants$climate) / 1E6
  high <- max(plants$health, plants$climate) / 1E6
  breakVals <- signif(seq(low, high, length.out = 6), 2)[-c(1,6)]
  
  # County choropleth map
  healthPlot <- ggplot() + 
    geom_polygon(data=all_states, aes(x=long, y=lat, group=group), fill=NA, colour="black") +
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + theme_classic() +
    labs(title = "", x="", y="")  +
    scale_colour_distiller(palette = "Blues", direction=-1, limits=c(low,high), breaks=breakVals) +
    #scale_colour_gradient2(low="#a6bddb", high="#045a8d") +
    geom_point(data=plants, aes(x=longProj, y=latProj, size=health/1E6, colour=health/1E6)) +
    scale_size_continuous(limits=c(low,high),breaks=breakVals) +
    guides(size=guide_legend(title="Health damages (million $)", title.position="top"), 
           colour=guide_legend(title="Health damages (million $)", title.position="top")) +
    theme(legend.position = "bottom")
  
  climatePlot <- ggplot() + 
    geom_polygon(data=all_states, aes(x=long, y=lat, group=group), fill=NA, colour="black") +
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + theme_classic() +
    labs(title = "", x="", y="")  +
    scale_colour_distiller(palette = "Oranges", direction=-1, limits=c(low,high), breaks=breakVals) +
    geom_point(data=plants, aes(x=longProj, y=latProj, size=climate/1E6, colour=climate/1E6)) +
    scale_size_continuous(limits=c(low,high),breaks=breakVals) +
    guides(size=guide_legend(title="Climate damages (million $)", title.position="top"), 
           colour=guide_legend(title="Climate damages (million $)", title.position="top")) +
    theme(legend.position = "bottom")
  
  # plot plant ratios
  ratioData <- plants
  
  ratioData$ratio <-  ratioData$health / ratioData$climate 
  ratioData <- ratioData[order(ratioData$ratio),]
  ratioData$x <- 1:nrow(ratioData)
  
  ratioData$dominant <- ifelse(ratioData$ratio < 1, "climate", "health")

  ratioPlot <- ggplot(data=ratioData, aes(x=x, y=log(ratio), fill=dominant)) + geom_bar(stat="identity")  + theme_classic() +
    scale_fill_manual(breaks=c("climate", "health"), values=c("orange", "blue")) +
    xlab("") + ylab("Log(health/climate)") +
    theme(axis.text.x = element_blank(),
          legend.position = "none")
  
  layout <- rbind(c(1, 1, 2, 2), 
                  c(NA, 3, 3, NA))
  
  panel <- grid.arrange(climatePlot + ggtitle("a") + theme(plot.title = element_text(face="bold")),
                        healthPlot + ggtitle("b") + theme(plot.title = element_text(face="bold")),
                        ratioPlot + ggtitle("c") + theme(plot.title = element_text(face="bold")), 
                        layout_matrix=layout, heights=c(0.6, 0.4), widths=c(0.2, 0.3, 0.3, 0.2))
  
  setwd(paste(baseWD,"Plots", "CEMS", "Initial plant damages", sep="/"))
  ggsave(paste0("Damages by plant - ", md, " ", dr, suffix, ".pdf"), panel, height = 6, width = 8)
  
}


## old code ####

# choropleth by state, no longer in play
# newGasPanel <- function(gasGen, md, dr, fuelName, var="Gross.Load..MW.h.", title="generation"){
#   scaleLims <- range(gasGen[,var])
#   plots <- list(); i <- 1
#   for (scenario in c("Climate", "Health", "Health + Climate")){
#     genSub <- gasGen[gasGen$Scenario == scenario,]
#     plots[[i]] <- stateEmissionsRateMap(genSub, "gen", scaleLims, var=var)
#     i <- i + 1
#   }
#   layout=rbind(c(1,NA),c(2,4), c(3, NA))
#   # Results as panel figure
#   mylegend <- g_legend(plots[[1]])
#   g1 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Climate"))
#   g2 <- arrangeGrob(plots[[2]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Health"))
#   g3 <- arrangeGrob(plots[[3]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle("Health + Climate"))
#   g4 <- arrangeGrob(mylegend)
#   
#   panelFig <- grid.arrange(g1, g2, g3, g4, layout_matrix=layout, heights = c(0.33, 0.33, 0.33), widths=c(0.8,0.2))
#   
#   setwd(paste(baseWD, "Plots", sep="/"))
#   ggsave(paste0("New gas ", title, " ", md, " ", dr, ".pdf"), panelFig, height = 6, width = 4)
#   rm(g1); rm(g2); rm(panelFig)
#   setwd(baseWD)
# }


loadGenCapData <- function(md, dr, suffix){
  setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
  # read new gas installed
  newGas <- read.csv(paste0("Results - gas ", md, " ", dr, suffix,  ".csv"))
  newGas <- subset(newGas, select=c("Scenario", "FIPS.Code", "Gross.Load..MW.h.", 
                            "New.NGCC.capacity..MW.", "Capital.costs", "Variable.costs", "OM.costs"))
  
  colnames(newGas) <- c("Scenario", "FIPS.Code", "generation", "capacity", "CAPEX", "variable", "OM")
  # note: variable costs here include fixed O&M
  newGasMelt <- melt(newGas, id.vars=c("Scenario", "FIPS.Code"))
  colnames(newGasMelt) <- c("Scenario", "FIPS.Code", "measurement", "value")
  newGasMelt$Fuel <- "Natural gas"
  newGasMelt <- newGasMelt[,c("Scenario", "FIPS.Code", "Fuel", "measurement", "value")]
  
  # renewables
  renewables <- read.csv(paste0("Results - renewables ", md, " ", dr, suffix,  ".csv"))
  renewables <- subset(renewables, select=c("Scenario", "FIPS.Code", "Solar.gen..MWh.", "Solar.installed..MW.", "Solar.CAPEX", "Solar.OM", 
                       "Wind.gen..MWh.", "Wind.installed..MW.", "Wind.CAPEX", "Wind.OM", "Storage.CAPEX"))
  
  # melt
  renewablesMelt <- melt(renewables, id.vars=c("Scenario", "FIPS.Code"))
  renewablesMelt$Fuel <- gsub("\\..*", "", renewablesMelt$variable)
  renewablesMelt$measurement <- gsub("^.*?\\.", "", renewablesMelt$variable)
  renewablesMelt <- renewablesMelt[,c("Scenario", "FIPS.Code", "Fuel", "measurement", "value")]
  
  renewablesMelt$measurement <- mapvalues(renewablesMelt$measurement,
                                          from = c("gen..MWh.", "installed..MW.", "CAPEX", "OM"),
                                          to = c("generation", "capacity", "CAPEX", "OM"))
  # convert kw to MW for capacity
  # renewablesMelt[renewablesMelt$measurement == "capacity","value"] = 1/1000 * renewablesMelt[renewablesMelt$measurement == "capacity","value"]  
  
  # merge
  combined <- rbind(newGasMelt, renewablesMelt)
  colnames(combined)[colnames(combined) == "FIPS.Code"] <- "fips"
  combined$cost <- suffix
  return(combined)
}
  
    
newSourceMap <- function(dataToPlot, maxValue, legendTitle, countyBorder=F){
  all_counties <- buildMapData()
  dataToPlot[!is.na(dataToPlot$fips) & dataToPlot$fips == 12025, "fips"] <- 12086    # Miami-Dade, FL

  county.plot <- merge(all_counties, dataToPlot, by="fips", all=T)
  county.plot <- county.plot[order(county.plot$group, county.plot$order),]
  
  # settings for county lines
  if(countyBorder==F){
    border <-0
    borderColor <- NA
  } else{
    border <- 0.01
    borderColor <- "black"
  }
  
  # county centroids
  centroids <- aggregate(cbind(long, lat) ~ subregion + fips, data=all_counties, FUN=function(x)mean(range(x)))
  centroids <- merge(centroids, county.plot[,c("fips", "value", "Fuel")], by="fips", all.y=T)
  
  # replace zeros with NAs and then drop
  centroids$value <- ifelse(centroids$value==0, NA, centroids$value)
  centroids <- centroids[complete.cases(centroids),]
  
  # County choropleth map
  g.plot <- ggplot() +
    geom_polygon(data=county.plot, aes(x=long, y=lat, group=group), fill="white", colour=borderColor, size=border) +
    theme_bw()  +
    labs(x="", y="") +
    scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
  
  # add in centroids
  g.plot <- g.plot + geom_point(data=centroids, aes(x=long, y=lat, color=Fuel, size=value), 
                                shape=21, alpha=0.9, stroke=0.75) +
    scale_size_continuous(legendTitle, limits=c(0.1, maxValue))+
    scale_color_manual(values=c("Natural gas"="red", 
                                "Wind"="blue", 
                                "Solar"="yellow")) 
  # add state borders
  all_states <- map_data("state", projection  = "albers", par = c(30,0))
  g.plot <- g.plot + geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black")
  
  return(g.plot)
}


## plots of installed capacity, generation by county
plotNewPlants <- function(combined, md, dr, parameter, legend, suffix="", scenario, save=T){
  plots <- list()
  # select parameter
  plotData <- combined[combined$measurement == parameter, ]
  # unit conversion
  if(parameter == "generation"){
    plotData$value <- plotData$value / 1E6
  }
  # set upper bound for common scale
  maxValue <- max(plotData[,"value"])
  plotData <- plotData[plotData$Scenario == scenario & plotData$cost == suffix,]
  
  # summarize by fips
  plotData <- ddply(plotData, ~  fips + Fuel, summarize, value=sum(value))
  plot <- newSourceMap(plotData, maxValue, legend)

  if(save){
    setwd(paste(baseWD,"Plots", "New sources", parameter, sep="/"))
    ggsave(paste0(scenario, " ", parameter, " ",  md, " ", dr, suffix, ".pdf"), plot, height = 4.5, width = 8)    
  } else{
    return(plot)
  }
}


newCostPlot <- function(newPlantData, md, dr, suffix){
  # extract costs
  costs <- newPlantData[newPlantData$measurement %in% c("CAPEX", "variable", "OM"),]
  costsSum <- ddply(costs, ~ Fuel + measurement, summarize, total=sum(value))
  costsSum$total <- costsSum$total / 1E6
  ggplot(costsSum, aes(x=Fuel, y=total, fill=measurement)) + geom_bar(stat="identity") + theme_classic() +
    ylab("Total annual expenditures (million $)") + xlab("") + labs(fill="")
  setwd(paste(baseWD,"Plots", "New sources", "costs", sep="/"))
  ggsave(paste0("costs ", md, " ", dr, suffix, ".pdf"), height = 6, width = 8)
  return(costsSum)  
}
