# Plotting
# Author: Brian Sergi
# Created: Nov. 9, 2018
# Updated: Feb. 22. 2020

# Note: user will need to specify the workding directory (baseWD)
# script excepts all results to be in a folder called 'Results', with sub folders by RCM and C-R scenario

# suspend for 1 hour
# Sys.sleep(60*60)

#  Description: Script includes functions for plotting results from Model V5
library(ggplot2)
library(reshape2)
library(plyr)
library(openxlsx)
library(tidyr)
library(RColorBrewer)
library(gridExtra)
library(ggrepel)

## Settings ####

# updated to Model V5
baseWD <- "~/Documents/Carnegie Mellon/Box Sync/Research/CACES energy modeling/Model V5"
resultsWD <- ""
plotWD <- ""

setwd(baseWD)

source("Plotting functions - maps.R")
source("Plotting functions - sensitivity analysis.R")

# add state and census information
stateFips <- readCensusInfo(state=T)
countyFips <- readCensusInfo(state=F)
countyFips <- loadPopData(countyFips)
new.rows <- data.frame(fips=51560, state="VA", state.num=51, county.num=0, 
                       county.name="Alleghany County", fips.class="H1", pop=0)
countyFips <- rbind(countyFips, new.rows)


doseResponses <- c("ACS", "H6C")
RCMs <- c("AP3", "EASIUR", "InMAP")
plotColors <- colorRampPalette(brewer.pal(9,"Blues"))(10)[c(4,8)]

## Total damage summaries ####

# load summary data for each RCM and dose response
readDamageSummary <- function(rcm, doseResponse, suffix=""){
  scenario <- paste(rcm, doseResponse)
  setwd(paste0(baseWD, "/Results/", scenario))
  temp <- read.csv(paste0("Results - summary ", scenario, suffix, ".csv"))
  return(temp)
}

loadSummaryResults <- function(suffix=""){
  # basline analysis results
  for (rcm in RCMs){
    for (doseResponse in doseResponses){
      temp <- readDamageSummary(rcm, doseResponse, suffix=suffix)
      if(rcm == RCMs[1] & doseResponse == doseResponses[1]){
        summaryResults <- temp
      } else {
        summaryResults <- rbind(summaryResults, temp); rm(temp)
      }
    }
  }
  setwd(baseWD)
  colnames(summaryResults) <- c("scenario", "climate", "health", "cost", "CO2", "SO2", "NOx", "RCM", "dose")
  summaryResults$dose <- factor(summaryResults$dose, levels=c("H6C", "ACS"))
  summaryResults$RCM <- factor(summaryResults$RCM, levels=c("AP3", "InMAP", "EASIUR"))
  
  summaryResults$scenario <- mapvalues(summaryResults$scenario, from="Climate", to="Climate-only")
  return(summaryResults)
}

summaryResults <- loadSummaryResults()
summaryResultsLowSCC <- loadSummaryResults(suffix=" low SCC")
summaryResultsHighGas <- loadSummaryResults(suffix=" high gas price")
summaryResultsLCOElow <- loadSummaryResults(suffix=" LCOE low")
summaryResultsLCOEhigh <- loadSummaryResults(suffix=" LCOE high")
summaryResultshighLeakage <- loadSummaryResults(suffix=" high leakage")
summaryResultshighGWP <- loadSummaryResults(suffix=" high GWP")
summaryResultshighGWPhighLeakage <- loadSummaryResults(suffix=" high GWP, high leakage")
summaryResultsNoLeakage <- loadSummaryResults(suffix=" no leakage")

## Panel plot of damages and health benefits ####

plotPanelFigure <- function(summaryResults, summaryResultsLowSCC=NULL, suffix=""){
  # extract climate damages 
  climateBaseline <- summaryResults[summaryResults$scenario == "Baseline", "climate"][1]
  climateReduction <- summaryResults[summaryResults$scenario == "Climate-only", "climate"][1]
  
  plotText1 <- expression("30% reduction in CO"[2])
  plotText1 <- ""
  plotText2 <- expression(paste("Climate damages"))
  plotText3 <- expression(paste("SCC=$40"))
  
  pointPlot <- ggplot(data=summaryResults, aes(x=scenario, y=health/1E9, color=dose, shape=RCM)) + 
    #geom_point(size=4) + theme_classic() + 
    geom_point(size=2.5) + theme_classic() + 
    scale_color_manual(values=c("#a6bddb", "#2b8cbe")) +
    geom_segment(aes(x=0.5, xend=1.5, y=climateBaseline/1E9, yend=climateBaseline/1E9), color="chocolate1", linetype=2) +  
    geom_segment(aes(x=1.5, xend=3.5, y=climateReduction/1E9, yend=climateReduction/1E9), color="chocolate1", linetype=2) +  
    annotate("text", x=1.6, y=(climateReduction+climateBaseline)/2E9, label=as.character(plotText1), 
             parse = TRUE, hjust = 0, color='chocolate1', fontface="bold") +
    annotate("text", x=3.5, y=climateReduction/1E9+5, label=as.character(plotText2), 
             parse = TRUE, hjust = 1, color='chocolate1', fontface="bold", size=5) +
    annotate("text", x=3.5, y=climateReduction/1E9+5-10, label=as.character(plotText3), 
             parse = TRUE, hjust = 1, color='chocolate1', fontface="bold", size=5) +
    geom_segment(aes(x=1.5, xend=1.5, y=climateBaseline/1E9-1, yend=climateReduction/1E9+1), size = 0.5, color="chocolate1",
                 arrow = arrow(length = unit(0.25, "cm"))) +
    ylab("Annual health damages (billion $)") + xlab("Optimization scenario") + 
    guides(color=guide_legend(title="Concentration-\nresponse", order = 2), shape=guide_legend(title="Air quality\nmodel", order = 1)) +
    theme(text = element_text(size=14),
          axis.text = element_text(size=14, color="black"),
          axis.title = element_text(size=14, color="black"),
          legend.text = element_text(size=12),
          legend.title=element_text(size=12),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))
  
  if(!is.null(summaryResultsLowSCC)){
    climateBaselineLowSCC <- summaryResultsLowSCC[summaryResultsLowSCC$scenario == "Baseline", "climate"][1]
    climateReductionLowSCC <- summaryResultsLowSCC[summaryResultsLowSCC$scenario == "Climate-only", "climate"][1]    
    plotText4 <- expression(paste("SCC=6"))
    
    pointPlot <- pointPlot + 
      annotate("text", x=3.5, y=climateReductionLowSCC/1E9+5, label=as.character(plotText4), parse = TRUE, hjust = 1, color='chocolate1', fontface="bold") +
      geom_segment(aes(x=0.5, xend=1.5, y=climateBaselineLowSCC/1E9, yend=climateBaselineLowSCC/1E9), color="chocolate1", linetype=2) +  
      geom_segment(aes(x=1.5, xend=3.5, y=climateReductionLowSCC/1E9, yend=climateReductionLowSCC/1E9), color="chocolate1", linetype=2) 
  }
  
  setwd(paste(baseWD,"Plots", "Panel figures", sep="/"))
  ggsave(paste0("Summary of damages by scenario" , suffix, ".pdf"), width=7, height=4)

  # plot of health benefits
  resultsMerged <- calcHCBenefit(summaryResults)
  healthSubMelt <- meltHealthBenefits(resultsMerged)

  vals <- ddply(healthSubMelt, ~RCM + dose, summarize, value=sum(value))
  maxY <- max(vals$value)
  
  g4 <- ggplot(healthSubMelt, aes(x=RCM, y=value, fill=variable, label=round(value))) + facet_wrap(~dose) +
    geom_bar(stat="identity") + theme_classic() + 
    geom_text(size = 5, position=position_stack(vjust=0.5), fontface="bold") +
    theme_classic() + # assumes VSL of 9 million
    scale_y_continuous(limits=c(0, maxY*1.025), sec.axis = sec_axis(~.*1E3/9/1E3, 
                                                                    name = "Annual deaths avoided (thousands)")) +
    ylab("Annual health benefits (billion $)") + xlab("Air quality model") + 
    theme(text = element_text(size=14),
          axis.text = element_text(size=12, color="black"),
          legend.text = element_text(size=10),
          legend.position = c(0.4, 0.9),
          legend.title=element_blank(),
          legend.margin=margin(c(0,0,0,0)),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
    scale_fill_manual(values=plotColors,
                      breaks=rev(c("Climate-only", "additionalHealth")),
                      labels=rev(c("Climate-only health benefits", "Additional benefits from health + climate"))) 
  
  # benefits panel - all models
  p1 <- arrangeGrob(pointPlot + ggtitle("a") + theme(plot.title = element_text(face="bold"),
                                                     text = element_text(size=16),
                                                     legend.title = element_text(size=16),
                                                     legend.text = element_text(size=15),
                                                     axis.text.x = element_text(size=16),
                                                     axis.text.y = element_text(size=16),
                                                     axis.title.x = element_text(size=18),
                                                     axis.title.y = element_text(size=18),
                                                     legend.position = c(0.7, 0.9),
                                                     #legend.position = "bottom",
                                                     #legend.direction = "vertical",
                                                     legend.box = "horizontal") +
                      guides(color=guide_legend(title="Concentration-\nresponse", order = 2), 
                             shape=guide_legend(title="Air quality model", order = 1))) 
  
  
  p2 <- arrangeGrob(g4 + ggtitle("b") + theme(plot.title = element_text(face="bold"),
                                              text = element_text(size=16),
                                              legend.text = element_text(size=15),
                                              axis.text.x = element_text(size=16),
                                              axis.text.y = element_text(size=16),
                                              axis.title.x = element_text(size=18),
                                              axis.title.y = element_text(size=18),
                                              legend.position = c(0.42, 0.9)))
                                              #legend.position = "bottom",
                                              #legend.direction = "vertical"))  
  
  panelFig <- grid.arrange(grobs = list(p1, p2), widths=c(1, 1.1))
  
  ggsave(paste0("Panel plot - damages and benefits", suffix, ".pdf"), panelFig, height=5.5, width=12.5)
  
}

calcHCBenefit <- function(summaryResults){
  
  baselineSummary <- summaryResults[summaryResults$scenario == "Baseline",]
  scenarioResults <- summaryResults[summaryResults$scenario != "Baseline",]
  
  colnames(baselineSummary) <- c("scenario", "baseline_climate", "baseline_health", "cost", 
                                 "baseline_CO2", "baseline_SO2", "baseline_NOx", "RCM", "dose")
  baselineSummary <- subset(baselineSummary, select=-c(scenario, cost))
  
  resultsMerged <- merge(scenarioResults, baselineSummary, by=c("RCM", "dose"))
  
  # calculate net benefits
  resultsMerged$netClimateBenefit <- resultsMerged$baseline_climate - resultsMerged$climate
  resultsMerged$netHealthBenefit <- resultsMerged$baseline_health - resultsMerged$health
  
  resultsMerged$netBenefits <- resultsMerged$netClimateBenefit + resultsMerged$netHealthBenefit - resultsMerged$cost
  
  resultsMerged$combined <- paste(resultsMerged$RCM, resultsMerged$dose, sep="-") 
  
  return(resultsMerged)
}

meltHealthBenefits <- function(resultsMerged){
  netHealth <- resultsMerged[,c("RCM", "dose", "scenario", "netHealthBenefit")]
  netHealth$netHealthBenefit  <- netHealth$netHealthBenefit / 1E9
  
  # focus on climate and health+climate scenarios
  healthSub <- netHealth[netHealth$scenario %in% c("Climate-only", "Health + Climate"), ]
  
  # 1. health + climate additional benefits alone 
  healthSub <- spread(healthSub, key="scenario", value="netHealthBenefit")
  healthSub$additionalHealth <- healthSub[,"Health + Climate"] - healthSub[,"Climate-only"]
  
  healthSub$dose <- factor(healthSub$dose, levels=c("ACS", "H6C"))
  healthSub$RCM <- factor(healthSub$RCM, levels=c("EASIUR", "InMAP", "AP3"))
  #ggplot(healthSub, aes(x=RCM, y=additionalHealth, fill=dose)) + 
  #  geom_bar(stat="identity", position=position_dodge()) + theme_classic()
  
  # 2. health from climate + additional health benefits from climate + health
  healthSubMelt <- healthSub
  healthSubMelt[,"Health + Climate"] <- NULL
  healthSubMelt <- melt(healthSubMelt, id.vars=c("RCM", "dose"))
  healthSubMelt$variable <- factor(healthSubMelt$variable, levels=c("additionalHealth", "Climate-only"))
  return(healthSubMelt)
}


plotPanelFigure(summaryResults, suffix="")
plotPanelFigure(summaryResultsHighGas, suffix=" high gas price")
plotPanelFigure(summaryResultsLCOElow, suffix=" LCOE low")
plotPanelFigure(summaryResultsLCOEhigh, suffix=" LCOE high")

## Net benefits barplot (ACS only) ####

netBenefitsPlot <- function(summaryResults, suffix=""){
  resultsMerged <- calcHCBenefit(summaryResults)
  resultsMerged$cost <- -1 * resultsMerged$cost
  
  resultsMerged$healthPerTonCO2 <- round(resultsMerged$netHealthBenefit / (resultsMerged$baseline_CO2 - resultsMerged$CO2), 1)
  resultsMerged$totalBenPerTonCO2 <- round(resultsMerged$netBenefits / (resultsMerged$baseline_CO2 - resultsMerged$CO2), 1)
  resultsMerged[,c("RCM", "dose", "scenario", "healthPerTonCO2", "totalBenPerTonCO2")]
  
  netBenefits <- melt(resultsMerged, id.vars = c("RCM", "dose", "combined", "scenario", "netBenefits"), 
                      measure.vars = c("netClimateBenefit", "netHealthBenefit", "cost"))
  
  netBenefitsOneDR <- netBenefits[netBenefits$dose == "ACS",]
  netBenefitsOneDR$scenario <- mapvalues(netBenefitsOneDR$scenario, from=c("Climate-only", "Health + Climate"), to=c("C", "H+C"))
  netBenefitsOneDR$netBenefitsTitle <- "Net\nbenefits"
  
  netBenefitsOneDR$variable <- factor(netBenefitsOneDR$variable, levels=c("netHealthBenefit", "netClimateBenefit", "cost")) 
  netBenefitsOneDR$RCM <- factor(netBenefitsOneDR$RCM, levels=c("EASIUR", "InMAP", "AP3")) 
  
  netBenefitsOneDR$value[netBenefitsOneDR$variable == "netClimateBenefit"] <- floor(netBenefitsOneDR$value[netBenefitsOneDR$variable == "netClimateBenefit"] / 1E9) * 1E9
  
  # manually calculate positions for text labels (need to avoid overlap with net benefits point
  ggplot(data=netBenefitsOneDR, aes(x=scenario, y=value/1E9, fill=variable, label=signif(value/1E9, 2))) + 
    geom_bar(stat="identity") + theme_classic() + facet_wrap(~RCM) + 
    # ylim(c(-20, 80)) +
    geom_hline(yintercept=0, linetype=1, color="black") +
    geom_point(aes(x=scenario, y=netBenefits/1E9, shape=netBenefitsTitle), size=2.5, fill="black") +
    geom_text(size = 4, position = position_stack(vjust = 0.2), fontface="bold") +
    ylab("Annual benefits (billion $)") + xlab("Optimization scenario") + 
    guides(fill=guide_legend(title=""), shape=guide_legend(title="")) +
    scale_fill_manual(values=c("#2b8cbe","chocolate1", "darkgreen"), 
                      labels = c( "Health", "Climate", "Mitigation\ncosts"),
                      breaks = c("netHealthBenefit", "netClimateBenefit", "cost")) +
    scale_shape_manual(values=c(23)) +
    theme(text = element_text(size=14),
          axis.text = element_text(size=12, color="black"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))
  
  setwd(paste(baseWD,"Plots", "Net benefits", sep="/"))
  ggsave(paste0("Net benefits - bar plots ACS only", suffix, ".pdf"), width=8, height=4)
  setwd(baseWD)
  
  return(resultsMerged)
}

netBenefitsBase <- netBenefitsPlot(summaryResults, suffix="")
netBenefitsHighGas <- netBenefitsPlot(summaryResultsHighGas, suffix=" high gas price")
netBenefitsLCOElow <- netBenefitsPlot(summaryResultsLCOElow, suffix=" LCOE low")
netBenefitsLCOEhigh <- netBenefitsPlot(summaryResultsLCOEhigh, suffix=" LCOE high")

# benefits per ton
netBenefitsBase[, c(1,2,3, 19, 20)]
netBenefitsHighGas[, c(1,2,3, 19, 20)]

range(netBenefitsBase[netBenefitsBase$scenario=="Climate-only" & netBenefitsBase$dose == "ACS", "healthPerTonCO2"])
range(netBenefitsBase[netBenefitsBase$scenario=="Health + Climate" & netBenefitsBase$dose == "ACS", "healthPerTonCO2"])
range(netBenefitsBase[netBenefitsBase$scenario=="Climate-only" & netBenefitsBase$dose == "ACS", "totalBenPerTonCO2"])
range(netBenefitsBase[netBenefitsBase$scenario=="Health + Climate" & netBenefitsBase$dose == "ACS", "totalBenPerTonCO2"])

range(netBenefitsHighGas[netBenefitsHighGas$scenario=="Climate-only" & netBenefitsHighGas$dose == "ACS", "healthPerTonCO2"])
range(netBenefitsHighGas[netBenefitsHighGas$scenario=="Health + Climate" & netBenefitsHighGas$dose == "ACS", "healthPerTonCO2"])
range(netBenefitsHighGas[netBenefitsHighGas$scenario=="Climate-only" & netBenefitsHighGas$dose == "ACS", "totalBenPerTonCO2"])
range(netBenefitsHighGas[netBenefitsHighGas$scenario=="Health + Climate" & netBenefitsHighGas$dose == "ACS", "totalBenPerTonCO2"])

netBenefitsBase[netBenefitsBase$dose == "ACS", c(1,2,3,6)]
netBenefitsHighGas[netBenefitsHighGas$dose == "ACS", c(1,2,3,6)]

## Net benefits barplot (comparison) ####
netBenefitsCompPlot <- function(resultsList){
  
  scenarios <- c("Baseline", "Baseline\n(LCOE approach)", "High gas", "High gas\n(LCOE approach)")
  plots <- list()
  netBenefitsAll <- data.frame()
  for(i in 1:4){
    resultsMerged <- calcHCBenefit(resultsList[[i]])
    resultsMerged$cost <- -1 * resultsMerged$cost
    
    resultsMerged$healthPerTonCO2 <- round(resultsMerged$netHealthBenefit / (resultsMerged$baseline_CO2 - resultsMerged$CO2), 1)
    resultsMerged$totalBenPerTonCO2 <- round(resultsMerged$netBenefits / (resultsMerged$baseline_CO2 - resultsMerged$CO2), 1)
    resultsMerged[,c("RCM", "dose", "scenario", "healthPerTonCO2", "totalBenPerTonCO2")]
    
    netBenefits <- melt(resultsMerged, id.vars = c("RCM", "dose", "combined", "scenario", "netBenefits"), 
                        measure.vars = c("netClimateBenefit", "netHealthBenefit", "cost"))
    
    netBenefitsOneDR <- netBenefits[netBenefits$dose == "ACS",]
    netBenefitsOneDR$scenario <- mapvalues(netBenefitsOneDR$scenario, from=c("Climate-only", "Health + Climate"), to=c("C", "H+C"))
    netBenefitsOneDR$netBenefitsTitle <- "Net benefits"
    
    netBenefitsOneDR$variable <- factor(netBenefitsOneDR$variable, levels=c("netClimateBenefit", "netHealthBenefit","cost")) 
    netBenefitsOneDR$RCM <- factor(netBenefitsOneDR$RCM, levels=c("EASIUR", "InMAP", "AP3")) 
    #netBenefitsOneDR$value[netBenefitsOneDR$variable == "netClimateBenefit"] <- floor(netBenefitsOneDR$value[netBenefitsOneDR$variable == "netClimateBenefit"] / 1E9) * 1E9
    netBenefitsOneDR$cost <- scenarios[i]
    netBenefitsAll <- rbind(netBenefitsAll, netBenefitsOneDR)
  }
    
  ggplot(data=netBenefitsAll, aes(x=scenario, y=value/1E9, fill=variable, label=signif(value/1E9, 2))) + 
      geom_bar(stat="identity") + theme_classic() + facet_grid(rows=vars(RCM), cols=vars(cost)) + 
      geom_hline(yintercept=0, linetype=1, color="black") +
      geom_point(aes(x=scenario, y=netBenefits/1E9, shape=netBenefitsTitle), size=2.5, fill="black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5), fontface="bold") + 
      ylab("Annual benefits (billion $)") + xlab("Optimization scenario") + 
      guides(fill=guide_legend(title=""), shape=guide_legend(title="")) +
    
      scale_fill_manual(values=c("#2b8cbe", "chocolate1", "darkgreen"), 
                        labels = c("Health", "Climate", "Mitigation\ncosts"),
                        breaks = c("netHealthBenefit", "netClimateBenefit", "cost")) +
      scale_shape_manual(values=c(23)) +
      theme(text = element_text(size=14),
            axis.text = element_text(size=12, color="black"),
            legend.text = element_text(size=10),
            legend.title = element_text(size=12),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))
  
  
  setwd(paste(baseWD,"Plots", "Net benefits", sep="/"))
  ggsave(paste0("Net benefits - bar plots cost comparison.pdf"), width=10, height=8)
  setwd(baseWD)
  
}

#scenarios <- c("Baseline", "Baseline\n(LCOE approach)", "High gas", "High gas\n(LCOE approach)")
netBenefitsCompPlot(list(summaryResults,summaryResultsLCOElow, summaryResultsHighGas, summaryResultsLCOEhigh))

## Source receptor matrices ####
# See plotting functions - maps.R script for functions

md <- "AP3"
dr <- "ACS"
# baseline
damagesSR <- readDamages(md, dr)      # load data
plotDamageMaps(damagesSR, md, dr)     #  plot

# scatterplot of damages by county
countyDamages <- merge(damagesSR, countyFips[,c("fips", "state", "pop")], by="fips", all.x=T)
countyDamages$Climate <- countyDamages$Baseline - countyDamages$Climate 
countyDamages$`Health + Climate` <- countyDamages$Baseline - countyDamages$`Health + Climate` 

countyDamages$region <- mapvalues(countyDamages$state, 
                                from = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "MD", "DC", "DE", "PA", 
                                         "WV", "VA", "NC", "SC", "GA", "FL", "AL", "MS", "TN", "KY", "LA","AR",
                                         "OH", "IN", "IL", "WI", "MI", "MN", "KS", "MO", "IA", "NE", "ND", "SD",
                                         "TX",  "OK", "NM",  "AZ",
                                         "NV","CA", "WA", "OR", "ID", "CO", "UT", "WY", "MT"),
                                to = c(rep("Northeast", 12),
                                       rep("Southeast", 12),
                                       rep("Midwest", 12),
                                       rep("Southwest", 4),
                                       rep("West", 9)))


countyDamages <- countyDamages[,c("fips", "region", "pop", "Climate", "Health + Climate")]

countyDamages <- countyDamages[complete.cases(countyDamages),]

ggplot(data=countyDamages, aes(x=Climate/1E6, y=`Health + Climate`/1E6, size=pop/1E6, color=region)) + 
  geom_abline(slope=1, linetype=2) + geom_point() + 
  theme_classic() +
  xlab("Annual health benefits,\nclimate-only scenario (million $)") + 
  ylab("Annual health benefits,\nhealth + climate scenario (million $)") +
  guides(color=guide_legend(title="Region"), size=guide_legend("Population\n(millions)")) +
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) + 
  theme(text = element_text(size=14),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

setwd(paste(baseWD,"Plots", "Net benefits", sep="/"))
ggsave("Health benefits by county.pdf")

# per capita version
countyDamages$CperCap <- countyDamages$Climate / countyDamages$pop
countyDamages$HCperCap <- countyDamages$`Health + Climate` / countyDamages$pop

countyDamages <- merge(countyDamages, countyFips[,c("fips", "state", "county.name")], all.x=T, by="fips")


countyDamages$label <- paste(countyDamages$county.name, countyDamages$state, sep=", ")
countyDamages$label <- ifelse(countyDamages$label %in% c("Massac County, IL", 
                                                         "Mason County, KY",
                                                         "Westmoreland County, PA",
                                                         "Jackson County, OH",
                                                         "Los Angeles County, CA",
                                                         "Pleasants County, WV",
                                                         "Falls County, TX"), countyDamages$label, "")

labelData <- countyDamages[countyDamages$label != "", ]

labelData$x <- c(160, 900, 625, 475, 700, 380, 160)
labelData$y <- c(-10, 1125, 1220, 1500, 940, 238, 935)

ggplot(data=countyDamages, aes(x=Climate/pop, y=`Health + Climate`/pop, size=pop/1E6, color=region, label=label)) + 
  geom_abline(slope=1, linetype=2) + geom_point() + 
  #geom_text(data=labelData, aes(x=x, y=y), size=3, show.legend=F) +
  theme_classic() +
  xlab("Annual per capita health benefits,\nclimate-only scenario ($ per person)") + 
  ylab("Annual per capita health benefits,\nhealth + climate scenario ($ per person)") +
  guides(color=guide_legend(title="Region"), size=guide_legend("Population\n(millions)")) +
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) + 
  scale_size_continuous(breaks = c(0.5, 2.5, 5, 7.5, 10)) +
  theme(text = element_text(size=14),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

setwd(paste(baseWD,"Plots", "Net benefits", sep="/"))
ggsave("Health benefits per capita by county.pdf")

damagesSR <- merge(stateFips[c("fips", "state")], damagesSR, by="fips", all.y=T)
    
# summarize damages by state
damagesSRstate <- ddply(damagesSR, ~ state, colwise(sum))
damagesSRstate$fips <- NULL; damagesSRstate <- damagesSRstate[complete.cases(damagesSRstate),]

# calculate benefits
for(scenario in c("Climate-only", "Health + Climate")){
  damagesSRstate[,paste0("Benefits - ", scenario)] <- damagesSRstate[, "Baseline"] - damagesSRstate[,scenario]
}

# additional co-optimization benefits
damagesSRstate[, "Co-opt benefits"] <- damagesSRstate[,"Benefits - Health + Climate"] - damagesSRstate[, "Benefits - Climate-only"]

stateSRSumS <- damagesSRstate[order(damagesSRstate$`Co-opt benefits`, decreasing=T), c("state", "Co-opt benefits")] 
stateSRSumS$`Co-opt benefits` <- round(stateSRSumS$`Co-opt benefits` / 1E6)
rm(stateSRSumS)

# breakdown of attribution from source-receptor

damagesSR_Base <- srAttribution(scenario="Baseline")
damagesSR_C <- srAttribution(scenario="Climate")
damagesSR_HC <- srAttribution(scenario="Health + Climate")

# benefits <- damagesSR_C - damagesSR_HC
benefits <- damagesSR_Base - damagesSR_HC

mapping <- data.frame(fips = rownames(benefits))
mapping <- merge(mapping, countyFips[,c("fips", "state")], by="fips", all.x=T, sort=F)

benefitsState <- aggregateByRegion(benefits, mapping, indx=2)
benefitsStatePercent <- calcPercent(benefitsState)
benefitsStatePercent <- benefitsStatePercent[, -50]
benefitsStatePercent <- benefitsStatePercent[-50,]

selfBenefits <- data.frame(state=colnames(benefitsStatePercent), 
                           benefitsSelf=diag(benefitsState),
                           totalBenefitsReceived=colSums(benefitsState),
                           totalBenefitsProvided=rowSums(benefitsState),
                           benefitsSelfPercent=diag(benefitsStatePercent))

# check on results
# alFips <- mapping[mapping$state == "AL", "fips"]
# arFips <- mapping[mapping$state == "AR", "fips"]
# alDamages <- damages[as.numeric(rownames(damages)) %in% alFips, as.numeric(colnames(damages)) %in% arFips]
# sum(alDamages)
# rm(alFips); rm(arFips); rm(alDamages)


## VSL & SCC sensitivity ####
allSens <- data.frame()

# load VSL and SCC sensitivity run data
for (md in RCMs){
  for (dr in doseResponses){
    setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
    sens <- read.csv(paste0("Sensitivity - VSL and SCC ", md, " ", dr, ".csv"))
    allSens <- rbind(allSens, sens)
  }
}


# create categories
allSens$catVSL <- mapvalues(allSens$VSL, from=c(3, 9, 18), to=c("Low VSL", "Baseline VSL", "High VSL"))
allSens$catSCC <- mapvalues(allSens$SCC, from=c(6, 40, 100), to=c("Low SCC", "Baseline SCC", "High SCC"))

allSens$cat <- paste(allSens$catSCC, allSens$catVSL, sep=", ")
allSensPlot <- allSens[allSens$cat %in% c("Baseline SCC, Baseline VSL", 
                                          "Low SCC, Low VSL", "Low SCC, High VSL",
                                          "High SCC, Low VSL", "High SCC, High VSL"),]

allSensPlot <- ddply(allSensPlot, ~ cat, transform, healthLower = min(Health.benefits....),
                                                    healthUpper = max(Health.benefits....))

allSensPlot <- allSensPlot[allSensPlot$RCM == "AP3" & allSensPlot$Dose.response == "ACS",]
allSensPlot <- melt(allSensPlot, measure.vars = c("Climate.benefits....", 
                                                  "Health.benefits...."))

allSensPlot$variable <- mapvalues(allSensPlot$variable, from=c("Climate.benefits....", "Health.benefits...."), to=c("Climate", "Health"))

allSensPlot[allSensPlot$variable == "Climate", "healthLower"] <- NA
allSensPlot[allSensPlot$variable == "Climate", "healthUpper"] <- NA

allSensPlot$cat <- factor(allSensPlot$cat, levels =  c("Baseline SCC, Baseline VSL", 
                                                       "Low SCC, Low VSL", "High SCC, Low VSL", 
                                                       "Low SCC, High VSL", "High SCC, High VSL"))

allSensPlot$cat <- mapvalues(allSensPlot$cat, from = c("Baseline SCC, Baseline VSL", 
                                                       "Low SCC, Low VSL", "Low SCC, High VSL",
                                                       "High SCC, Low VSL", "High SCC, High VSL"),
                                              to = c("Baseline", 
                                                     "Low SCC,\nLow VSL", "Low SCC,\nHigh VSL",
                                                     "High SCC,\nLow VSL", "High SCC,\nHigh VSL"))

ggplot(data=allSensPlot, aes(x=cat, y=value/1e9, fill=variable)) + geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=healthLower/1e9, ymax=healthUpper/1e9), position=position_dodge(.9), width=0.15) + theme_classic() + 
  geom_vline(xintercept = 1.5, linetype=2) + xlab("") + ylab("Annual benefits (billion $)") + 
  scale_fill_manual(values=rev(c("#599ad3", "#f9a65a")), labels=c("Climate benefits", "Health benefits")) + 
  guides(fill=guide_legend(title="")) + 
  theme(text = element_text(size=14),
        axis.text = element_text(size=12, color="black"),
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.margin=margin(c(0,0,0,0)))

setwd(paste(baseWD,"Plots", "Sensitivity grids", sep="/"))
ggsave("VSL and SCC sensitivity - bar plot.pdf", width=8, height=5)

# grid plot
#comparativeSensitivity(RCMs, doseResponses)
sensData <-loadAllSens(RCMs, doseResponses)
  
# formatting for grid plot  
sensData$benRounded <- round(sensData$benefits/1E9)
sensData$SCC <- factor(sensData$SCC, levels=sort(unique(sensData$SCC), decreasing=T))
sensData$RCM <- factor(sensData$RCM, levels=c("EASIUR", "InMAP", "AP3"))

sensData$baseline <- ifelse(sensData$VSL == 9 & sensData$SCC == 40, T, F)
cells2highlight <- sensData[sensData$baseline,]

ggplot(data=sensData, aes(x=factor(VSL), y=factor(SCC), fill=benRounded)) + geom_tile() + facet_grid(RCM ~ Dose.response) +
  geom_tile(data=cells2highlight, colour="red", size=1, show.legend=F) +
  geom_text(aes(label=benRounded)) + xlab("VSL (million $)") + ylab("SCC ($ per ton)") + 
  guides(fill=guide_legend(title="H+C additional\nannual benefits\n(billion $)")) +
  scale_fill_gradient(low="#FFFFFF", high="darkblue") +
  theme(axis.text=element_text(size=14, color="black"), 
        axis.title=element_text(size=16, color="black"),
        strip.text = element_text(size=12, color="black")) 
  
setwd(paste(baseWD,"Plots", "Sensitivity grids", sep="/"))
ggsave("VSL and SCC sensitivity grid ggplot.pdf", width=8, height=6)


# net benefits bar charts
netBenefitsSensitivityPlot <- function(sensDataFull, dr="ACS", md="AP3"){
  
  # downselect results
  sensToPlot <- sensDataFull[sensDataFull$RCM == md & sensDataFull$Dose.response == dr, ]
  
  sensToPlot$netBenefits <- sensToPlot[,"Climate.benefits...."] + sensToPlot[,"Health.benefits...."]  - sensToPlot[,"Annual.costs...."]  
  sensToPlot <- sensToPlot[, c("VSL", "SCC", "Scenario", "Climate.benefits....", "Health.benefits....", "Annual.costs....", "netBenefits")]
  colnames(sensToPlot) <- c("VSL", "SCC", "Scenario", "Climate", "Health", "Mitigation costs", "netBenefits")
  
  sensToPlot[, "Mitigation costs"] <- sensToPlot[, "Mitigation costs"] * -1
  
  sensToPlot <- melt(sensToPlot, id.vars=c("VSL", "SCC", "Scenario", "netBenefits"))
  sensToPlot$netBenefitsTitle <- "Net\nbenefits"
  sensToPlot$Scenario <- mapvalues(sensToPlot$Scenario, from=c("Climate", "Health + Climate"), to=c("C", "H+C"))
  
  sensToPlot$variable <- factor(sensToPlot$variable, levels=c("Health", "Climate", "Mitigation costs"))
  
  ggplot(data=sensToPlot, aes(x=Scenario, y=value/1E9, fill=variable, label=signif(value/1E9, 2))) + 
    geom_bar(stat="identity") + theme_classic() + facet_grid(VSL ~ SCC) + 
    geom_hline(yintercept=0, linetype=1, color="black") +
    geom_point(aes(x=Scenario, y=netBenefits/1E9, shape=netBenefitsTitle), size=2.5, fill="black") +
    geom_text(size = 2, position = position_stack(vjust = 0.5), fontface="bold") + 
    ylab("Annual benefits (billion $)") + xlab("Optimization scenario") + 
    guides(fill=guide_legend(title=""), shape=guide_legend(title="")) +
    scale_fill_manual(values=c("#2b8cbe", "chocolate1", "darkgreen")) +
    scale_shape_manual(values=c(23)) +
    theme(text = element_text(size=14),
          axis.text = element_text(size=12, color="black"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))
  
  
  setwd(paste(baseWD,"Plots", "Sensitivity grids", sep="/"))
  ggsave(paste0("Net benefits - bar plots VSL SCC comparison.pdf"), width=10, height=8)
  setwd(baseWD)
  
  range(sensToPlot[sensToPlot$Scenario == "Climate", "netBenefits"]) / 1E9
  range(sensToPlot[sensToPlot$Scenario == "Health + Climate", "netBenefits"]) / 1E9
}

coalGenChangeSensitivity <- function(sensDataFull, dr="ACS", md="AP3"){
  
  # downselect results
  #sensToPlot <- sensDataFull[ sensDataFull$Dose.response == dr, ]
  sensToPlot <- sensDataFull[ sensDataFull$RCM == 'AP3', ]
  
  sensToPlot <- sensToPlot[, c("VSL", "SCC", "Dose.response", "Scenario", "Coal.gen.change..MWh.")]
  colnames(sensToPlot) <- c("VSL", "SCC", "dr", "Scenario", "coalChange")
  
  # plot in TWh
  ggplot(data=sensToPlot, aes(x=SCC, y=coalChange/1E6, fill=Scenario, label=signif(coalChange/1E6, 2))) + 
    geom_bar(stat="identity", position=position_dodge()) + theme_classic() + facet_grid(VSL ~ dr) + 
    ylab("Reduction in nnual coal generation (TWh)") + xlab("SCC ($ per ton)") + 
    guides(fill=guide_legend(title=""), shape=guide_legend(title="")) +
    theme(text = element_text(size=14),
          axis.text = element_text(size=12, color="black"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))
  
  
  setwd(paste(baseWD,"Plots", "Sensitivity grids", sep="/"))
  ggsave(paste0("Net benefits - bar plots VSL SCC comparison.pdf"), width=10, height=8)
  setwd(baseWD)
  
}

sensDataFull <- loadAllSens(RCMs, doseResponses, format=F)
netBenefitsSensitivityPlot(sensDataFull)

# same bar charts but for coal generation

ggplot(data=sensToPlot, aes(x=Scenario, y=value/1E9, fill=variable, label=signif(value/1E9, 2))) + 
  geom_bar(stat="identity") + theme_classic() + facet_grid(VSL ~ SCC) + 
  geom_hline(yintercept=0, linetype=1, color="black") +
  geom_point(aes(x=Scenario, y=netBenefits/1E9, shape=netBenefitsTitle), size=2.5, fill="black") +
  geom_text(size = 2, position = position_stack(vjust = 0.5), fontface="bold") + 
  ylab("Annual benefits (billion $)") + xlab("Optimization scenario") + 
  guides(fill=guide_legend(title=""), shape=guide_legend(title="")) +
  scale_fill_manual(values=c("#2b8cbe", "chocolate1", "darkgreen")) +
  scale_shape_manual(values=c(23)) +
  theme(text = element_text(size=14),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

## Environmental Justice ####

# measure against county-level household median income
setwd(paste(baseWD,"Data Inputs", "ACS_17_5YR_S1903", sep="/"))
income <- read.csv("ACS_17_5YR_S1903_with_ann.csv", skip=1)

# subset data
# margin of errors reflect 90% confidence intervals
incomeSub <- income[,c("Id2", "Geography", "Median.income..dollars...Estimate..Households", 
                       "Median.income..dollars...Margin.of.Error..Households", "Number..Estimate..Households")]

colnames(incomeSub) <- c("fips", "county", "medianIncome", "marginError", "households")

# minor fips code adjustment for Oglala Lakota County, South Dakota
incomeSub[incomeSub$fips == 46102, "fips"] <- 46113

# cut data into fifths by year
cutPoints <- quantile(incomeSub$medianIncome, probs=seq(0,1,by=0.2))
incomeSub$incomeCategory=cut(incomeSub$medianIncome, breaks=cutPoints, include.lowest=T, 
                             labels = c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%"))

# merge with damages
incomeSub <- merge(incomeSub, damagesSR, by="fips")

# calculate benefits per household
incomeSub$benefitsC <- (incomeSub$Baseline - incomeSub$Climate) / incomeSub$households
incomeSub$benefitsHC <- (incomeSub$Baseline - incomeSub$`Health + Climate`) / incomeSub$households
incomeSub$benefitsHCaboveC <- (incomeSub$Climate - incomeSub$`Health + Climate`) / incomeSub$households

ggplot(incomeSub, aes(x=incomeCategory, y=benefitsHCaboveC, fill=factor(incomeCategory))) + 
   geom_boxplot(position=position_dodge()) + theme_classic()

ggplot(incomeSub, aes(x=benefitsHCaboveC, fill=factor(incomeCategory))) + 
  geom_density(alpha=0.5) + theme_classic()

ggplot(incomeSub[incomeSub$benefitsHCaboveC < 600,], aes(x=benefitsHCaboveC, fill=factor(incomeCategory))) + 
  geom_density(alpha=0.25) + theme_classic()


# weights within income quantiles
incomeSub <- ddply(incomeSub, ~ incomeCategory, transform, weight=households/sum(households))
incomeSub <- incomeSub[order(incomeSub$fips),]

# check weights
ddply(incomeSub, ~ incomeCategory, summarize, weightSum=sum(weight))

# function to estimate weighted median
weightedMedian <- function(values, weights){
  if (length(values) != length(weights)){
    print("Weighted median error: values and weights vectors unequal.")
    return(NA)
  } 
  if(length(values) == 0){
    return(NA)
  }
  if(sum(weights < 0 | weights > 1)){
    print("Weighted median error: invalid weight.")
    return(NA)
  } else if(abs(sum(weights) - 1) > 0.0001){
    print("Weighted median error: weights do not sum to 1.")
    return(NA)
  }
  # sort by values
  newOrder <- order(values)
  values <- values[newOrder]
  weights <- weights[newOrder]
  weightsRev <- sort(weights, decreasing=T)
  
  cumWeight <- cumsum(weights)
  cumWeightRev <- cumsum(weightsRev)  # find median from left and right
  k_left <- which(cumWeight >= 0.5)[1]
  k_right <- which(cumWeightRev >= 0.5)[1] 
  
  x_k_left <- values[k_left]
  x_k_right <- rev(values)[k_right]
  
  if(x_k_left == x_k_right){            # if values are equal, return 1
    return(x_k_left)
  } else {                              # otherwise, take mean
    return(mean(c(x_k_left, x_k_right)))  
  }
}
# weightedMedian(values=(c(1,2,3,4,5)), weights=c(0.15,0.1,0.2,0.3,0.25))   # should = 4
# weightedMedian(values=(c(1,2,3,4)), weights=c(0.25, 0.25, 0.25, 0.25))    # should = 2.5
# weightedMedian(values=(c(1,2,3,4)), weights=c(0.49, 0.01, 0.25, 0.25))    # should = 2.5
# weightedMedian(values=(c()), weights=c(0.49, 0.01, 0.25, 0.25))           # should = NA
# weightedMedian(values=(c(1,2,3,4)), weights=c(0.49, -1, 0.25, 0.25))      # should = NA
# weightedMedian(values=(c(1,2,3,4,5)), weights=c(0.1,0.1,0.2,0.3,0.25))    # should = NA

# calculate weighted median by income category
incomeSummary <- ddply(incomeSub, ~ incomeCategory, summarize, 
                       weightedC=NA, weightedHC=NA, weightedHCaboveC=NA)

for (incomeLevel in unique(incomeSub$incomeCategory)){
  incomeByLevel <- incomeSub[incomeSub$incomeCategory == incomeLevel,]
  weightedC <- weightedMedian(incomeByLevel$benefitsC, incomeByLevel$weight)
  weightedHC <- weightedMedian(incomeByLevel$benefitsHC, incomeByLevel$weight)
  weightedHCaboveC <- weightedMedian(incomeByLevel$benefitsHCaboveC, incomeByLevel$weight)
  
  incomeSummary[incomeSummary$incomeCategory == incomeLevel, "weightedC"] <- weightedC
  incomeSummary[incomeSummary$incomeCategory == incomeLevel, "weightedHC"] <- weightedHC
  incomeSummary[incomeSummary$incomeCategory == incomeLevel, "weightedHCaboveC"] <- weightedHCaboveC
  
}

# random sampling w/ replacement of vector data 
bootstrapFunction <- function(data, estimator){
  # draw bootstrap sample, sampling with replacement
  data.b <- data[sample(1:nrow(data), nrow(data), replace=T),]
  # recalculate weights
  data.b <- ddply(data.b, ~ incomeCategory, transform, weight=households/sum(households))
  # calculate parameter
  theta.hat.star <- estimator(data.b[,"benefitsHC"], data.b[,"weight"])
  return(theta.hat.star)
}
bootstrapHelperFunction <- function(data, estimator, B, alpha=0.05){
  theta.hat <- estimator(data[,"benefitsHC"], data[,"weight"])
  # bootstrap
  theta.hat.stars <- replicate(B, bootstrapFunction(data, estimator))
  # confidence interval
  ci.lower <- 2 * theta.hat - quantile(theta.hat.stars, 1 - alpha/2)  
  ci.upper <- 2 * theta.hat - quantile(theta.hat.stars, alpha/2)
  return(data.frame(estimate=theta.hat, ci.lower=unname(ci.lower), ci.upper = unname(ci.upper)))
}

set.seed(123)
incomeBootResults <- data.frame()
for (income in c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%")){
  incomeToBoot <- incomeSub[incomeSub$incomeCategory == income, ]
  bootData <- bootstrapHelperFunction(incomeToBoot, weightedMedian, 5000)
  incomeBootResults <- rbind(incomeBootResults, cbind(income, bootData))
}; rm(bootData); rm(incomeToBoot); rm(income)


incomeSummary <- merge(incomeSummary, incomeBootResults, by.y="income", by.x="incomeCategory")
incomeSummary <- incomeSummary[order(incomeSummary$income),]
incomeSummary$estimate <- NULL

incomeSummaryMelt <- melt(incomeSummary, id.vars=c("incomeCategory", "ci.lower", "ci.upper"))

# plot formatting
incomeSummaryMeltSub <- droplevels(incomeSummaryMelt[incomeSummaryMelt$variable != "weightedHC",])
incomeSummaryMeltSub$variable <- factor(incomeSummaryMeltSub$variable, levels=c("weightedHCaboveC", "weightedC"))

ggplot(data=incomeSummaryMeltSub, 
       aes(x=incomeCategory, y=value, fill=variable, label=signif(value,2))) + 
  geom_bar(stat="identity", colour="black") + theme_classic() +
  geom_text(size = 3, position = position_stack(vjust = 0.5), 
            fontface="bold", colour="black", alpha=1) +
  xlab("Income quintile") + ylab("Median annual health benefits\n($ per household)") +
  guides(fill=guide_legend(title="Scenario"), alpha=guide_legend(title="Optimization scenario")) +
  scale_fill_manual(values=plotColors,
                       breaks=c("weightedHCaboveC", "weightedC"),
                       labels=c("Health + climate", "Climate only")) +
  theme(axis.text = element_text(size=10, color="black"))
        
        
  #geom_errorbar(aes(ymax = incomeSummaryMeltSub$ci.upper, ymin = incomeSummaryMeltSub$ci.lower), 
  #              colour="black", width=0.2)
setwd(paste(baseWD,"Plots", sep="/"))
ggsave("EJ - Median health benefits by income.pdf", width=8, height=4)




# by race  

# measure against county-level household median income
setwd(paste(baseWD,"Data Inputs", "ACS_17_5YR_DP05", sep="/"))
race <- read.csv("ACS_17_5YR_DP05_with_ann.csv", skip=1)

# subset data
# margin of errors reflect 90% confidence intervals
raceSub <- race[,c("Id2", "Geography", "Percent..RACE...One.race...White", 
                   "Percent.Margin.of.Error..RACE...One.race...White", "Estimate..SEX.AND.AGE...Total.population")]

colnames(raceSub) <- c("fips", "county", "percentWhite", "marginError", "population")
# reverse direction (% non-white instead of % white)
raceSub$percentWhite <- 100 - raceSub$percentWhite

# minor fips code adjustment for Oglala Lakota County, South Dakota
raceSub[raceSub$fips == 46102, "fips"] <- 46113

# cut data into fifths by year
cutPoints <- quantile(raceSub$percentWhite, probs=seq(0,1,by=0.2))

raceSub$raceCategory=cut(raceSub$percentWhite, breaks=cutPoints, include.lowest=T, 
                             labels = c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%"))

# merge with damages
raceSub <- merge(raceSub, damagesSR, by="fips")

# calculate benefits per person
raceSub$benefitsC <- (raceSub$Baseline - raceSub$Climate) / raceSub$population
raceSub$benefitsHC <- (raceSub$Baseline - raceSub$`Health + Climate`) / raceSub$population
raceSub$benefitsHCaboveC <- (raceSub$Climate - raceSub$`Health + Climate`) / raceSub$population

# weights within income quantiles
raceSub <- ddply(raceSub, ~ raceCategory, transform, weight=population/sum(population))
raceSub <- raceSub[order(raceSub$fips),]

# check weights
ddply(raceSub, ~ raceCategory, summarize, weightSum=sum(weight))


# calculate weighted median by income category
raceSummary <- ddply(raceSub, ~ raceCategory, summarize, 
                       weightedC=NA, weightedHC=NA, weightedHCaboveC=NA)

for (raceLevel in unique(raceSub$raceCategory)){
  raceByLevel <- raceSub[raceSub$raceCategory == raceLevel,]
  weightedC <- weightedMedian(raceByLevel$benefitsC, raceByLevel$weight)
  weightedHC <- weightedMedian(raceByLevel$benefitsHC, raceByLevel$weight)
  weightedHCaboveC <- weightedMedian(raceByLevel$benefitsHCaboveC, raceByLevel$weight)
  
  raceSummary[raceSummary$raceCategory == raceLevel, "weightedC"] <- weightedC
  raceSummary[raceSummary$raceCategory == raceLevel, "weightedHC"] <- weightedHC
  raceSummary[raceSummary$raceCategory == raceLevel, "weightedHCaboveC"] <- weightedHCaboveC
}

# random sampling w/ replacement of vector data 
bootstrapFunctionRace <- function(data, estimator){
  # draw bootstrap sample, sampling with replacement
  data.b <- data[sample(1:nrow(data), nrow(data), replace=T),]
  # recalculate weights
  data.b <- ddply(data.b, ~ raceCategory, transform, weight=population/sum(population))
  # calculate parameter
  theta.hat.star <- estimator(data.b[,"benefitsHC"], data.b[,"weight"])
  return(theta.hat.star)
}
bootstrapHelperFunctionRace <- function(data, estimator, B, alpha=0.05){
  theta.hat <- estimator(data[,"benefitsHC"], data[,"weight"])
  # bootstrap
  theta.hat.stars <- replicate(B, bootstrapFunctionRace(data, estimator))
  # confidence interval
  ci.lower <- 2 * theta.hat - quantile(theta.hat.stars, 1 - alpha/2)  
  ci.upper <- 2 * theta.hat - quantile(theta.hat.stars, alpha/2)
  return(data.frame(estimate=theta.hat, ci.lower=unname(ci.lower), ci.upper = unname(ci.upper)))
}


set.seed(123)
raceBootResults <- data.frame()
for (raceLevel in c("Lowest 20%", "Second 20%", "Third 20%", "Fourth 20%", "Highest 20%")){
  raceToBoot <- raceSub[raceSub$raceCategory == raceLevel, ]
  bootData <- bootstrapHelperFunctionRace(raceToBoot, weightedMedian, 5000)
  raceBootResults <- rbind(raceBootResults, cbind(raceLevel, bootData))
}; rm(bootData); rm(raceToBoot); rm(raceLevel)


raceSummary <- merge(raceSummary, raceBootResults, by.y="raceLevel", by.x="raceCategory")
raceSummary <- raceSummary[order(raceSummary$raceCategory),]
raceSummary$estimate <- NULL

#raceSummaryMelt <- melt(raceSummary, id.vars=c("raceCategory", "ci.lower", "ci.upper"))
raceSummaryMelt <- melt(raceSummary, id.vars=c("raceCategory", "ci.lower", "ci.upper"))

# plot formatting
raceSummaryMeltSub <- droplevels(raceSummaryMelt[raceSummaryMelt$variable != "weightedHC",])
raceSummaryMeltSub$variable <- factor(raceSummaryMeltSub$variable, levels=c("weightedHCaboveC", "weightedC"))

ggplot(data=raceSummaryMeltSub, 
       aes(x=raceCategory, y=value, fill=variable, label=signif(value,2))) + 
  geom_bar(stat="identity", colour="black") + theme_classic() +
  geom_text(size = 3, position = position_stack(vjust = 0.5), 
            fontface="bold", colour="black", alpha=1) +
  xlab("Minority quintile") + ylab("Median annual health benefits\n($ per person)") +
  guides(fill=guide_legend(title="Scenario"), alpha=guide_legend(title="Optimization scenario")) +
  scale_fill_manual(values=plotColors,
                    breaks=c("weightedHCaboveC", "weightedC"),
                    labels=c("Health + climate", "Climate only")) +
  theme(axis.text = element_text(size=10, color="black"))


setwd(paste(baseWD,"Plots", sep="/"))
ggsave("EJ - Median health benefits by race.pdf", width=8, height=4)


## Total generation by scenario ####

totalGenerationBarPlot <- function(md, dr, suffix){
  setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
  # read baseline
  baselinePlants <- read.csv(paste0("Baseline - plants ", md, " ", dr, ".csv"))
  # read scenario
  plants <- read.csv(paste0("Results - plants ", md, " ", dr, suffix, ".csv"))
  newGas <- read.csv(paste0("Results - gas ", md, " ", dr, suffix,  ".csv"))
  # renewables
  renewables <- read.csv(paste0("Results - renewables ", md, " ", dr, suffix, ".csv"))
  
  totalGen <- subset(baselinePlants, select=c(Fuel, Unit.category, Gross.Load..MW.h.))
  totalGen$Scenario <- "Baseline"
  plantSub <- subset(plants, select=c(Fuel, Unit.category, Gross.Load..MW.h., Scenario))
  gasSub <- subset(newGas, select=c(Gross.Load..MW.h., Scenario))
  gasSub$Fuel <- "Natural gas"
  gasSub$Unit.category <- "Combined cycle"
  gasSub <- gasSub[,c("Fuel", "Unit.category", "Gross.Load..MW.h.", "Scenario")]
  
  renewGen <- subset(renewables, select=c(Wind.gen..MWh., Solar.gen..MWh., Scenario))
  renewGen <- melt(renewGen, id.vars="Scenario")
  colnames(renewGen) <- c("Scenario", "Unit.category", "Gross.Load..MW.h.")
  renewGen$Fuel <- mapvalues(renewGen$Unit.category, from=c("Wind.gen..MWh.", "Solar.gen..MWh."), to = c("Wind", "Solar"))
  renewGen <- renewGen[,c("Fuel", "Unit.category", "Gross.Load..MW.h.", "Scenario")]
  
  totalGen <- rbind(totalGen, plantSub, gasSub, renewGen)
  rm(plantSub)
  
  # split out natural gas by CC or CT
  levels(totalGen$Fuel) <- c(levels(totalGen$Fuel), "Natural gas - CC", "Natural gas - CT", "Natural gas - other", "Natural gas - new CC")
  totalGen[totalGen$Fuel == "Natural gas" & totalGen$Unit.category == "Combined cycle", "Fuel"] <- "Natural gas - CC"
  totalGen[totalGen$Fuel == "Natural gas" & totalGen$Unit.category == "Combustion turbine", "Fuel"] <- "Natural gas - CT"
  totalGen[totalGen$Fuel == "Natural gas",  "Fuel"] <- "Natural gas - other"
  
  totalGenSum <- ddply(totalGen, ~ Scenario + Fuel, summarize, gen=sum(Gross.Load..MW.h.))
  
  # add in other generation not in CEMS (renewables, nuclear)
  
  totalGenSum$Fuel <- droplevels(totalGenSum$Fuel)
  
  # manually relevel as desired
  totalGenSum$Fuel <- factor(totalGenSum$Fuel, levels = c("Other", "Wood", "Oil", "Natural gas - other", 
                                                          "Natural gas - CC", "Natural gas - CT", "Coal", "Solar", "Wind"))
  
  # indicate baseline is 2017 (EIA electricity data browser)
  # data in thousand MWh (i.e. GWh)
  solar <- 53286 # utility-scale
  nuclear <- 804950
  wind <-  254303
  hydro <- 300333
  
  #newRows <- expand.grid(Fuel=c("Solar", "Nuclear", "Wind", "Hydro"), Scenario=c("Baseline", "Climate"))
  
  # generation plot by scenario
  ggplot(data=totalGenSum, aes(x=Scenario, y=gen/1E6, fill=Fuel, label=signif(gen/1E6, 2))) + geom_bar(stat='identity') +
    ylab("Total fossil generation (TWh)") + xlab("") + theme_classic() +
    scale_fill_manual(values=rev(c('#238b45', "#ffff00", '#cc4c02','#0570b0', '#74a9cf', '#f1eef6', '#525252', "#B22222", '#88419d'))) +
    theme(axis.text = element_text(size=12, color="black")) 
  
  setwd(paste0(baseWD, "/Plots", "/New Sources", "/generation totals"))
  ggsave(paste0("Generation by scenario ", md, " ", dr, suffix, ".pdf"), height = 6, width = 10)
  return(totalGenSum)
}

totalGenPlotWrapper <- function(suffix=""){
  final <- data.frame()
  for (md in RCMs){
    for (dr in doseResponses){
      results <- totalGenerationBarPlot(md, dr, suffix)
      results$md <- md
      results$dr <- dr
      final <- rbind(final, results)
    }
  }
  return(final)
}

summaryGeneration <- totalGenPlotWrapper()
summaryGenerationSCC <- totalGenPlotWrapper(suffix=" low SCC")
summaryGenerationHighGas <- totalGenPlotWrapper(suffix=" high gas price")
summaryGenerationLCOElow <- totalGenPlotWrapper(suffix=" LCOE low")
summaryGenerationLCOEhigh <- totalGenPlotWrapper(suffix=" LCOE high")
summaryGenerationhighLeakage <- totalGenPlotWrapper(suffix=" high leakage")
summaryGenerationhighGWP <- totalGenPlotWrapper(suffix=" high GWP")
summaryGenerationhighGWPhighLeakage <- totalGenPlotWrapper(suffix=" high GWP, high leakage")
summaryGenerationNoLeakage <- totalGenPlotWrapper(suffix=" no leakage")

## New generation / capacity by county ####

newGenByScenario <- function(suffix=""){
  final <- data.frame()
  for (md in RCMs){
    for (dr in doseResponses){
      # read in data on new builds
      newPlantData <- loadGenCapData(md, dr, suffix=suffix)
      
      for(scenario in unique(newPlantData$Scenario)){
        plotNewPlants(newPlantData, md, dr, parameter="generation", legend="Generation (TWh)", suffix=suffix, scenario=scenario)
        plotNewPlants(newPlantData, md, dr, parameter="capacity", legend="Capacity (MW)", suffix=suffix, scenario=scenario)
      }

      # plot costs (in million $)
      newPlantSummary <- ddply(newPlantData, ~ Fuel + measurement, summarize, total=sum(value))
      newPlantSummary <- spread(newPlantSummary, key="measurement", value="total")
      # convert financials to million $
      newPlantSummary$CAPEX <- round(newPlantSummary$CAPEX / 1E6)
      newPlantSummary$variable <- round(newPlantSummary$variable / 1E6)
      newPlantSummary$OM <- round(newPlantSummary$OM / 1E6)
      # convert capacity to GW
      newPlantSummary$capacity <- round(newPlantSummary$capacity / 1E3, 1)
      #convert generation to TWh
      newPlantSummary$generation <- round(newPlantSummary$generation / 1E6, 1)
    
      # add scenario info
      newPlantSummary$dr <- dr
      newPlantSummary$md <- md
      
      # store results
      final <- rbind(final, newPlantSummary)
      
      # get total model costs by technology
      totalCosts <- newCostPlot(newPlantData, md, dr, suffix)
      
    }
  }
  return(final)
}


summaryNewGen <- newGenByScenario()
summaryNewGenSCC <- newGenByScenario(suffix=" low SCC")
summaryNewGenHighGas <- newGenByScenario(suffix=" high gas price")
summaryNewGenLCOElow <- newGenByScenario(suffix=" LCOE low")
summaryNewGenLCOEhigh <- newGenByScenario(suffix=" LCOE high")
summaryNewGenhighLeakage <- newGenByScenario(suffix=" high leakage")
summaryNewGenhighGWP <- newGenByScenario(suffix=" high GWP")
summaryNewGenhighGWPhighLeakage <- newGenByScenario(suffix=" high GWP, high leakage")
summaryNewGensNoLeakage <- newGenByScenario(suffix=" no leakage")

# multi-panel new gen
newPlantData <- loadGenCapData(md="AP3", dr="ACS", suffix=" LCOE high")
newPlantData <- rbind(newPlantData, loadGenCapData(md="AP3", dr="ACS", suffix=" LCOE low"))
newPlantData <- rbind(newPlantData, loadGenCapData(md="AP3", dr="ACS", suffix=""))

plots <- list(); i <- 1
plotNames <- c()
for (scenario in unique(newPlantData$Scenario)){
  for (cost in unique(newPlantData$cost)){
    plotNames <- c(plotNames, paste(scenario, cost, sep=','))
    plots[[i]] <- plotNewPlants(newPlantData, md="AP3", dr="ACS", parameter="generation", legend="Generation (TWh)", 
                                suffix=cost, scenario=scenario, save=F)
    i <- i +1
  }
}

plotNames[plotNames == "Climate,"] <- "Climate, Baseline"
plotNames[plotNames == "Health + Climate,"] <- "Health + Climate, Baseline"

leg <- g_legend(plots[[1]] + theme(legend.box = "vertical"))

g1 <- arrangeGrob(plots[[3]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle(paste("a.", plotNames[3])))
g2 <- arrangeGrob(plots[[6]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle(paste("b.", plotNames[6])))
g3 <- arrangeGrob(plots[[2]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle(paste("c.", plotNames[2])))
g4 <- arrangeGrob(plots[[5]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle(paste("d.", plotNames[5])))
g5 <- arrangeGrob(plots[[1]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle(paste("e.", plotNames[1])))
g6 <- arrangeGrob(plots[[4]] + theme(legend.position = "none", plot.title = element_text(face="bold")) + ggtitle(paste("f.", plotNames[4])))

layoutMatrix <- rbind(c(1, 2, 7), 
                      c(3, 4, 7),
                      c(5, 6, 7))
panelFig <- grid.arrange(g1, g2, g3, g4, g5, g6, leg, layout_matrix=layoutMatrix, widths=c(1, 1, 0.5))

setwd(paste(baseWD,"Plots", "New sources", sep="/"))
ggsave("Generation by cost and scenario.pdf", panelFig, height = 10, width = 10)    




## New gas (generation and capacity) ####

# newGas <- summarizeNewGas(md, dr)
# newGasPanel(newGas, md, dr)

loadGasCapacity <- function(md, dr, suffix=""){
  setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
  gas <- read.csv(paste0("Results - gas ", md, " ", dr, suffix, ".csv"))
  gas$Scenario <- mapvalues(gas$Scenario, from="Climate", to="Climate-only")
  return(gas)
}

plotNewCapacityResults <- function(suffix="", md="AP3", dr="ACS"){
  # gas capacity
  gasResults <- loadGasCapacity(md, dr, suffix)
  gasResults  <- merge(gasResults, stateFips[,c("state", "fips")], by.x="FIPS.Code", by.y="fips", all.x=T)
  
  # renewables
  setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
  renewableResults <- read.csv(paste0("Results - renewables ", md, " ", dr, suffix,  ".csv"))
  renewableResults <- subset(renewableResults, select=c("FIPS.Code", "Scenario",  "Solar.installed..MW.", "Wind.installed..MW."))
  renewableResults$Scenario <- mapvalues(renewableResults$Scenario, from="Climate", to="Climate-only")
                            
  totalCapacity <- merge(gasResults, renewableResults, by=c("FIPS.Code", "Scenario"), all=T)
  
  # summarize new capacity by state
  stateTechResults <- ddply(totalCapacity, ~ Scenario + state, summarize, gasCapacity=sum(New.NGCC.capacity..MW.),
                                                                          solarCapacity=sum(Solar.installed..MW.),
                                                                          windCapacity=sum(Wind.installed..MW.))
  
  # melt
  stateTechResultsMelt <- melt(stateTechResults, id.vars = c("Scenario", "state"))
  
  stateGasResults <- ddply(stateTechResultsMelt, ~ Scenario + state, summarize, capacity=sum(value))
  #stateGasResults <- spread(stateGasResults, key="Scenario", value="capacity")
  
  # compare to installed capacity
  setwd(paste(baseWD,"Data Inputs", sep="/"))
  eGrid2016 <- read.xlsx("egrid2016_data.xlsx", sheet="ST16", startRow=2)
  
  stateGasResults <- merge(stateGasResults, eGrid2016[,c("PSTATABB", "NAMEPCAP")], all.x=T, by.x="state", by.y="PSTATABB")
  stateGasResults$share <- stateGasResults$capacity / stateGasResults$NAMEPCAP * 100
  
  # top states by climate
  numStates <- 10
  stateGasResults <- stateGasResults[order(stateGasResults$capacity, decreasing=T),]
  statesClimate <- stateGasResults[stateGasResults$Scenario == "Climate-only", "state"][1:numStates]
  
  # top states by health & climate
  statesHealth <- stateGasResults[stateGasResults$Scenario == "Health + Climate", "state"][1:numStates]
  
  # subset states
  stateGasSub <- stateGasResults[stateGasResults$state %in% c(statesClimate, statesHealth),]
  
  extraStates <- statesHealth[!(statesHealth %in% statesClimate)]
  temp <- stateGasResults[stateGasResults$Scenario =="Climate-only" & stateGasResults$state %in% extraStates, ]
  temp <- temp[order(temp$capacity, decreasing=T),]
  extraStates <- temp$state
  
  # order by climate
  stateGasSub$state <- factor(stateGasSub$state, levels=c(statesClimate, extraStates))
  
  ggplot(data=stateGasSub, aes(x=state, y=capacity/1E3, fill=Scenario, label=round(share))) + 
    geom_bar(stat="identity", position="dodge", alpha=0.95) + theme_classic() + xlab("") + ylab("New capacity installed (GW)") +
    ylim(c(0, max(stateGasSub$capacity/1E3)*1.05)) + 
    guides(fill=guide_legend(title="Optimization Scenario", title.position="top")) +
    scale_fill_manual(values=c("Climate-only"="#f9a65a", "Health + Climate"="#599ad3")) +   #"#9e66ab"
    #geom_text(size = 4, position = position_dodge(0.9),  vjust=-0.5, fontface="bold") +
    theme(text = element_text(size=14),
          axis.text = element_text(size=12, color="black"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14),
          legend.position=c(0.75, 0.9),
          legend.direction = "horizontal") +
    #geom_text(inherit.aes = F, x=6.5, y=19, label="% capacity replaced (right axis)", 
    #          size=4, hjust=0, fontface="plain", family="mono") +
    geom_segment(x=10.5, y=17.25, xend=12.5, yend=17.25, linetype=1, arrow=arrow(length=unit(0.15,"cm"), type = "closed")) +
    geom_point(aes(y=share/3.5, group=Scenario, x=state), 
               inherit.aes=F, position = position_dodge(width = 0.90), shape=23, fill="black", color="black") + 
    scale_y_continuous(sec.axis = sec_axis(~.*3.5, name="Share of state's capacity replaced (%)")) 
  
    #  guides(color = FALSE)
    #scale_color_manual(name = NULL, values = c("Grid share (right axis)" = "black")) 
    #guides(fill = guide_legend(title="Optimization scenatio", order = 1, title.position = "top"), 
    #       color = guide_legend(order = 2, title.position = "top"))
    
  
  setwd(paste(baseWD,"Plots", "Gas results", sep="/"))
  ggsave(paste0("Installed new capacity by state", suffix, ".pdf"), width=8, height=5)
  return(stateGasResults)
}

gasResults <- plotNewCapacityResults()
#plotGasResults(suffix=" high gas price")
#plotGasResults(suffix=" LCOE low")
#plotGasResults(suffix=" LCOE high")

ggplot(data=gasResults, aes(x=capacity/1E3, y=share, label=state, colour=Scenario)) + 
  geom_text_repel(show.legend = FALSE) + geom_point() +
  facet_wrap(~Scenario) + theme_classic() +
  scale_colour_manual(values = c("Climate-only"="#599ad3", "Health + Climate"="#f9a65a")) +
  xlab("New capacity installed (GW)") +
  ylab("Percent of installed capacity replaced in state (%)") + 
  theme(legend.position = "none") 

setwd(paste(baseWD,"Plots", "Gas results", sep="/"))
ggsave("New capacity absolute vs. relative.pdf", width=10)

# newGasPanel(stateGasResults, md, dr, var="capacity", title="capacity")
gasResults <- gasResults[order(gasResults$state, gasResults$Scenario),]

# gasIncrease <- ddply(gasResults, ~ state, summarize, gasIncrease=diff(share))
#selfBenefits <- merge(selfBenefits, gasIncrease)

selfBenefits <- merge(selfBenefits, gasResults[gasResults$Scenario == "Health + Climate", ])
selfBenefits$region <- mapvalues(selfBenefits$state, 
                                  from = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "MD", "DC", "DE", "PA", 
                                           "WV", "VA", "NC", "SC", "GA", "FL", "AL", "MS", "TN", "KY", "LA","AR",
                                           "OH", "IN", "IL", "WI", "MI", "MN", "KS", "MO", "IA", "NE", "ND", "SD",
                                           "TX",  "OK", "NM",  "AZ",
                                           "NV","CA", "WA", "OR", "ID", "CO", "UT", "WY", "MT"),
                                  to = c(rep("Northeast", 12),
                                         rep("Southeast", 12),
                                         rep("Midwest", 12),
                                         rep("Southwest", 4),
                                         rep("West", 9)))

# merge in population
setwd(paste(baseWD,"Data Inputs", "state_pop", sep="/"))
popData <- read.csv("PEP_2017_PEPANNRES_with_ann.csv", skip=1)
popData <- popData[,c("Geography", "Population.Estimate..as.of.July.1....2017")]
colnames(popData) <- c("state", "population")

popData$state <- tolower(popData$state)

stateFipsUnique <- stateFips[,c("state", "region")]
stateFipsUnique <- stateFipsUnique[!duplicated(stateFipsUnique),]
colnames(stateFipsUnique)[1] <- "stateAbb"

popData <- merge(popData, stateFipsUnique, by.x="state", by.y="region", all.x=T)

selfBenefits <- merge(selfBenefits, popData[,c("stateAbb", "population")], by.x="state", by.y="stateAbb", all.x=T)

ggplot(data=selfBenefits, aes(x=share, y=benefitsSelfPercent, label=state, colour=region)) + 
  geom_point() + theme_classic() +
  theme(legend.position = "top",
        axis.text=element_text(size=14),
        legend.text=element_text(size=15)) +
  ylab("Percent of health benefits that remain in state (%)") +
  xlab("Percent of installed capacity replaced in state (%)") +
  geom_text_repel(show.legend = FALSE) +
  labs(colour="")

setwd(paste(baseWD,"Plots", "Gas results", sep="/"))
ggsave("Share of grid vs. share of benefits.pdf", width=10)

selfBenSub <- selfBenefits[,c("state", "region", "benefitsSelf", "totalBenefitsReceived", "share")]
selfBenSub <- melt(selfBenSub, measure.vars = c("benefitsSelf", "totalBenefitsReceived"))
selfBenSub$variable <- mapvalues(selfBenSub$variable, from=c("benefitsSelf", "totalBenefitsReceived"),
                                 to=c("Self-benefits only", "Self + imported benefits"))

ggplot(data=selfBenSub, aes(x=share, y=value/1E9, label=state, colour=region)) + 
  facet_wrap(~variable) +
  geom_point() + theme_classic() +
  ylab("Annual health benefits (billion $)") +
  xlab("Percent of installed capacity replaced in state (%)") +
  geom_text_repel(show.legend = FALSE) +
  labs(colour="") + 
  theme(legend.position = "top",
        text = element_text(size=14),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(size=15))

setwd(paste(baseWD,"Plots", "Gas results", sep="/"))
ggsave("Share of grid vs. total health benefits.pdf", width=10, height=8)

## Model-retired coal by county ####
plotGenWithDamages <- function(sub="Coal retirements + damages", suffix=""){
  for (md in RCMs){
    for (dr in doseResponses){
      setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
      # read scenarios
      plants <- read.csv(paste0("Results - plants ", md, " ", dr, suffix,  ".csv"))
      # read baseline
      baselinePlants <- read.csv(paste0("Baseline - plants ", md, " ", dr, ".csv"))
    
      plants$Scenario <- mapvalues(plants$Scenario, from="Climate", to="Climate-only")
  
      # subset
      plants <- subset(plants, subset=c(Fuel=="Coal"), select=c(Scenario, FIPS.Code, Gross.Load..MW.h.))
      baselinePlants <- subset(baselinePlants, subset=c(Fuel=="Coal"), select=c(FIPS.Code, Gross.Load..MW.h.))
      #plants <- subset(plants, subset=c(Fuel=="Coal"), select=c(Scenario, FIPS.Code, Gross.Load..MW.h., Facility.ID..ORISPL., Unit.ID, Fuel, Unit.category))
      #baselinePlants <- subset(baselinePlants, subset=c(Fuel=="Coal"), select=c(FIPS.Code, Gross.Load..MW.h., Facility.ID..ORISPL., Unit.ID, Fuel, Unit.category))
      
      baselinePlants$Scenario <- "Baseline"
      baselinePlants <- baselinePlants[,c("Scenario", "FIPS.Code", "Gross.Load..MW.h.")]
      #baselinePlants <- baselinePlants[,c("Scenario", "FIPS.Code", "Gross.Load..MW.h.", "Facility.ID..ORISPL.", "Unit.ID", "Fuel","Unit.category")]
      
      allPlants <- rbind(plants, baselinePlants)
      allPlants <- ddply(allPlants, ~ Scenario + FIPS.Code, summarize, load=sum(Gross.Load..MW.h.))
      
      # # sum by county
      # plants <- ddply(plants, ~ Scenario + FIPS.Code, summarize, load=sum(Gross.Load..MW.h.))
      # baselinePlants <- ddply(baselinePlants, ~ FIPS.Code, summarize, load=sum(Gross.Load..MW.h.))
      # 
      # # subtract baseline
      # colnames(baselinePlants)[colnames(baselinePlants) == "load"] <- "baseLoad"
      # plants <- merge(plants, baselinePlants, by="FIPS.Code")
      # plants$coalReduction <- plants$baseLoad - plants$load
      
      # spread to wide
      allPlants <- spread(allPlants, key="Scenario", value="load")
      allPlants <- allPlants[c("FIPS.Code", "Baseline", "Climate-only", "Health + Climate")]
      colnames(allPlants)[1] <- "fips"
      
      # regular bubble charts
      # plotCoalGen(allPlants, md, dr)
      
      # plot plants with source receptor damages
      damages <- readDamages(md, dr, suffix) 
      plotCoalGenWithSR(allPlants, damages, md, dr, sub, suffix)
    }
  }
}

plotGenWithDamages(sub="Coal retirements + damages", suffix="")
plotGenWithDamages(sub="Gas price sensitivity", suffix=" high gas price")
plotGenWithDamages(sub="Gas LCOE approach", suffix=" LCOE low")
plotGenWithDamages(sub="Gas LCOE approach", suffix=" LCOE high")

  
## Initial damages by facility ####
plotInitialDamages <- function(md, dr, suffix=""){

      setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
      # read baseline
      baselinePlants <- read.csv(paste0("Baseline - plants ", md, " ", dr, ".csv"))
      
      baselinePlants <- baselinePlants[,c("FIPS.Code", "Facility.ID..ORISPL.", "Gross.Load..MW.h.", "Facility.Latitude", "Facility.Longitude", 
                                          "Health.Damages....", "Climate.Damages....")]
      
      # sum by facility
      facility <- ddply(baselinePlants, ~ Facility.ID..ORISPL., summarize, gen = sum(Gross.Load..MW.h., na.rm=T),
                                                                           climate = sum(Climate.Damages...., na.rm=T),
                                                                           health = sum(Health.Damages...., na.rm=T),
                                                                           lat = unique(Facility.Latitude),
                                                                           long = unique(Facility.Longitude))
      
      facility$climatePerMWh <- facility$climate / facility$gen
      facility$healthPerMWh <- facility$health / facility$gen
      
      # plot
      plotInitialDamagesByPlant(facility, md, dr, suffix)
    }

for (md in RCMs){
  for (dr in doseResponses){
    plotInitialDamages(md=md, dr=dr)
  }
}

## Per ton mitigation costs ####

calcMitCosts <- function(summaryResults, scenario){
  mitCosts <- summaryResults
  mitCosts <- melt(mitCosts, id.vars = c("scenario", "RCM", "dose", "climate", "health", "cost"))
  colnames(mitCosts)[colnames(mitCosts) == "value"] <- "emissions"
  colnames(mitCosts)[colnames(mitCosts) == "variable"] <- "pollutant"
  
  # calculate avoided CO2
  baselineEmissions <- mitCosts[mitCosts$scenario=="Baseline" & mitCosts$RCM == "AP3" & mitCosts$dose == "ACS",]
  colnames(baselineEmissions)[colnames(baselineEmissions) == "emissions"] <- "baseline"
  baselineEmissions <- baselineEmissions[,c("pollutant", "baseline")]
  
  mitCosts <- merge(mitCosts, baselineEmissions, by="pollutant")
  mitCosts$avoided <- mitCosts$baseline - mitCosts$emissions 
  
  
  # mitigation costs per ton CO2
  mitCosts$COM <- mitCosts$cost / mitCosts$avoided
  mitCosts$gas <- scenario
  return(mitCosts)
}

calcMitCostSummary <- function(mitCosts){
  mitCosts <- mitCosts[,c("scenario", "gas",  "RCM", "dose", "pollutant",  "COM")]
  mitCosts <- spread(mitCosts, key="pollutant", value="COM")
  mitCosts <- mitCosts[mitCosts$scenario != "Baseline",]
  mitCosts[,c("CO2", "SO2", "NOx")] <- round(mitCosts[,c("CO2", "SO2", "NOx")])
  return(mitCosts)
}

mitCosts <- calcMitCosts(summaryResults, scenario = "Baseline")
mitCostsAll <- rbind(mitCosts, calcMitCosts(summaryResultsHighGas, scenario = "High gas"))
mitCostsAll <- rbind(mitCostsAll, calcMitCosts(summaryResultsLCOElow, scenario = "Baseline w/\ninfrastructure costs"))
mitCostsAll <- rbind(mitCostsAll, calcMitCosts(summaryResultsLCOEhigh, scenario = "High gas w/\ninfrastructure costs"))

mitCostSummary <- calcMitCostSummary(mitCostsAll)


# benfits per ton (see above as well)
coBenefits <- spread(mitCostsAll[,c("gas", "scenario", "RCM", "dose", "pollutant", "avoided", "climate", "health")], 
                     key="pollutant", value="avoided")

# convert damages to benefits
baselineDamages <- mitCosts[mitCosts$scenario=="Baseline" & mitCosts$pollutant == "CO2",]
colnames(baselineDamages)[colnames(baselineDamages) == "health"] <- "baselineHealth"

coBenefits$benClimate <- unique(baselineDamages$climate) - coBenefits$climate
coBenefits <- merge(coBenefits, baselineDamages[,c("RCM", "dose", "baselineHealth")])
coBenefits$benHealth <- coBenefits$baselineHealth - coBenefits$health

coBenefits$coBenCO2 <- coBenefits$benHealth / coBenefits$CO2
coBenefits$totalBenCO2 <- (coBenefits$benClimate + coBenefits$benHealth) / coBenefits$CO2

coBenefits$benHealthBillion <- round(coBenefits$benHealth / 1E9)
healthSummary <- coBenefits[,c("RCM", "dose", "scenario", "benHealthBillion")]


healthSummary[healthSummary$scenario == "Health + Climate", "benHealthBillion"] - healthSummary[healthSummary$scenario == "Climate-only", "benHealthBillion"]



## Check on capacity factors ####

# start analysis with look at baseline levels

setwd(paste(baseWD,"Data Inputs", sep="/"))
eGridPlants <- read.xlsx("egrid2016_data.xlsx", sheet="PLNT16", startRow=2)
eGridPlants <- eGridPlants[, c("ORISPL", "PLFUELCT", "NAMEPCAP", "CAPFAC")]

# read baseline (choice of RCM and DR irrelevant)
baselineUnits <- read.csv(paste0("CEMS emissions 2017.csv"))
baselineUnits <- baselineUnits[,c("Facility.ID..ORISPL.", "Unit.ID", "Gross.Load..MW.h.")]
colnames(baselineUnits) <- c("ORISPL", "Unit.ID", "baselineLoad")

# summarize to plant level
baselinePlants <- ddply(baselineUnits, ~ ORISPL, summarize, baselineLoad = sum(baselineLoad, na.rm=T))

# merge with eGrid
baselinePlants <- merge(baselinePlants, eGridPlants, all.x=T, by="ORISPL")

# estimate capacity factor
baselinePlants$actualCF <- baselinePlants$baselineLoad / (8760 * baselinePlants$NAMEPCAP)

# drop any non-fossil entries 
# 14 plants with strange fuel types from eGrid --> check on these plants manually
baselinePlants <- baselinePlants[baselinePlants$PLFUELCT %in% c("BIOMASS", "COAL", "GAS", "OIL"),]

baselinePlants$PLFUELCT <- as.factor(baselinePlants$PLFUELCT)
baselinePlants$PLFUELCT <- droplevels(baselinePlants$PLFUELCT)

ggplot(baselinePlants, aes(x=CAPFAC, y=actualCF, color=PLFUELCT)) + geom_point() + 
  theme_classic() + geom_abline(intercept=0, slope=1) + 
  geom_segment(x=1, y=0, xend=1, yend=1, linetype=2, color="black") +
  geom_segment(x=0, y=1, xend=1, yend=1, linetype=2, color="black")

ggplot(baselinePlants, aes(x=PLFUELCT, y=actualCF, fill=PLFUELCT)) + geom_boxplot() + 
  theme_classic() 

# look at percentage change by unit in total generation
unitsReduced <- data.frame()
for (md in RCMs){
  for (dr in doseResponses){
    setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
    # read scenarios
    plantsNew <- read.csv(paste0("Results - plants ", md, " ", dr,  ".csv"))
    # subset
    plantsNew <- plantsNew[,c("Scenario", "RCM", "Dose.response", "Facility.ID..ORISPL.", "Unit.ID", "Gross.Load..MW.h.", "Fuel")]
    unitsReduced <- rbind(unitsReduced, plantsNew)
  }
}

colnames(unitsReduced)[colnames(unitsReduced) == "Facility.ID..ORISPL."] <- "ORISPL"

unitsReduced <- merge(unitsReduced, baselineUnits, all.x=T, by=c("ORISPL", "Unit.ID"))

# number of plants dropping more than 70% of original load
capacityFractionCalcs <- function(unitsReduced){
  
  # calculate percentage of  baseline load
  unitsReduced$percentOfOrig <- (unitsReduced$Gross.Load..MW.h. / unitsReduced$baselineLoad) * 100
  unitsReduced$percentReduction <-  100 - unitsReduced$percentOfOrig 
  
  # load fractions
  unitsReduced$shut <- ifelse(unitsReduced$percentReduction >= 100, 1, 0)
  unitsReduced$reduced <- ifelse(unitsReduced$percentOfOrig < 100, 1, 0)
  unitsReduced$partial <- ifelse(unitsReduced$percentOfOrig < 100 & unitsReduced$percentOfOrig > 0, 1, 0)
  unitsReduced$lowPartial <- ifelse(unitsReduced$percentOfOrig < 30 & unitsReduced$percentOfOrig > 0, 1, 0) 
  unitsReduced$verySmall <- ifelse(unitsReduced$percentOfOrig < 1 & unitsReduced$percentOfOrig > 0, 1, 0) 
  unitsReduced$coal <- ifelse(unitsReduced$Fuel == "Coal", 1, 0)
  
  return(unitsReduced)  
}

unitsReduced <- capacityFractionCalcs(unitsReduced)

ggplot(data=unitsReduced[unitsReduced$RCM == "AP3" & unitsReduced$Dose.response == "ACS" & unitsReduced$partial == 1,], 
       aes(x=percentOfOrig, fill=Scenario)) + geom_density(alpha=0.5) + 
       theme_classic()

unitSummary <- ddply(unitsReduced, ~ Scenario + RCM + Dose.response, summarize, 
                                      numShut = sum(shut, na.rm=T),
                                      numPartialUnits = sum(partial, na.rm=T), 
                                      numReductions = sum(reduced, na.rm=T),
                                      numLow = sum(lowPartial, na.rm=T),
                                      numVerySmall = sum(verySmall, na.rm=T), 
                                      totalCoal = sum(coal, na.rm=T),
                                      numCoalShut = sum(coal & shut, na.rm=T),
                                      numCoalPartial = sum(coal & partial, na.rm=T),
                                      allUnits = length(partial))

# summarize to plant level 
plantsReducedSum <- ddply(unitsReduced, ~ ORISPL + Scenario + RCM + Dose.response, summarize, 
                      `Gross.Load..MW.h.`=sum(Gross.Load..MW.h., na.rm=T), baselineLoad=sum(baselineLoad, na.rm=T), 
                      Fuel = names(sort(table(Fuel), decreasing=T)[1]))


# calculate percentage of  baseline load at plant level
plantsReducedSum <- capacityFractionCalcs(plantsReducedSum)

plantSummary <- ddply(plantsReducedSum, ~ Scenario + RCM + Dose.response, summarize, 
                     numShut = sum(shut, na.rm=T),
                     numPartialUnits = sum(partial, na.rm=T), 
                     numReductions = sum(reduced, na.rm=T),
                     numLow = sum(lowPartial, na.rm=T),
                     numVerySmall = sum(verySmall, na.rm=T), 
                     allUnits = length(partial))

# 4% of units operating between 0 and 50% of original load (9% of plants)

# increase of plants operating below capacity factor of 5%
plantsAllSum <- merge(plantsReducedSum, eGridPlants, all.x=T, by="ORISPL")

plantsAllSum$originalCF <- plantsAllSum$baselineLoad / (plantsAllSum$NAMEPCAP * 8760)
plantsAllSum$newCF <- plantsAllSum$`Gross.Load..MW.h.` / (plantsAllSum$NAMEPCAP * 8760)

plantsAllSum$dropInCF <- ifelse(plantsAllSum$originalCF >= 0.05 & plantsAllSum$newCF < 0.05 & plantsAllSum$newCF > 0, 1, 0)

cfSummary <- ddply(plantsAllSum, ~ Scenario + RCM + Dose.response, summarize, 
                      lowCFPlants = sum(dropInCF, na.rm=T),
                      allPlants = length(dropInCF))

cfSummary$percentLow <- cfSummary$lowCFPlants / cfSummary$allPlants
# 6-7% of plants now operate at fairly low CF without shutting down


## Costs ####

readCosts <- function(rcm, doseResponse, suffix=""){
  scenario <- paste(rcm, doseResponse)
  setwd(paste0(baseWD, "/Results/", scenario))
  plants <- read.csv(paste0("Results - plants ", scenario, suffix, ".csv"))
  gas <- read.csv(paste0("Results - gas ", scenario, suffix, ".csv"))
  
  
  # savings from existing plants
  plantSum <- ddply(plants, ~ Scenario + RCM + Dose.response, summarize, savings = sum(Variable.cost.savings....))
  
  # gas costs
  gasSum <- ddply(gas, ~ Scenario + RCM + Dose.response, summarize, gasPlants = sum(Plants.built), 
                                                                    capacity=sum(New.NGCC.capacity..MW.),
                                                                    varCosts = sum(Variable.costs), 
                                                                    fixedOM = sum(OM.costs),
                                                                    capitalCosts = sum(Capital.costs))
  
  # renewable costs
  renewables <- read.csv(paste0("Results - renewables ", rcm, " ", doseResponse, suffix,  ".csv"))
  renewSum <- ddply(renewables, ~ Scenario + RCM + Dose.response, summarize, 
                                  capacity=sum(Wind.installed..MW.) + sum(Solar.installed..MW.),
                                  fixedOM = sum(Wind.OM) + sum(Solar.OM), 
                                  capitalCosts = sum(Wind.CAPEX) + sum(Solar.CAPEX) + sum(Storage.CAPEX))
  
  renewSum$gasPlants <- 0
  renewSum$varCosts <- 0
  renewSum <- renewSum[,c("Scenario", "RCM", "Dose.response", "gasPlants", "capacity", "varCosts", "fixedOM", "capitalCosts")]
  
  combined <- rbind(gasSum, renewSum)
  combined <- ddply(combined, ~ Scenario + RCM + Dose.response, summarize, varCosts = sum(varCosts),
                                                                          fixedOM = sum(fixedOM),
                                                                          capitalCosts = sum(capitalCosts))
  
  costSummary <- merge(plantSum, combined, by=c("Scenario", "RCM", "Dose.response"), all=T)
  costSummary$Scenario <- mapvalues(costSummary$Scenario, from="Climate", to="Climate-only")
  return(costSummary)
}

annualize <- function(NPV, dr, life){
  return((NPV * dr) / (1-(1+dr)^(-1*life)) )
}

costFormat <- function(suffix="", scenario){
  costs <- readCosts("AP3", "ACS", suffix)
  # convert units to millions
  # costs$annualizedCapB <- annualize(costs$capitalCosts, dr=0.07, life=20) / 1E9
  costs$annualizedCapB <- costs$capitalCosts / 1E9
  costs$annualVarCostsB <- costs$varCosts / 1E9
  costs$annualSavingsB <- costs$savings / 1E9
  costs$annaulFixedOM <- costs$fixedOM / 1E9
  costs$netVarB <- costs$annualVarCostsB - costs$annualSavingsB
  costs$total <- costs$annualizedCapB + costs$netVarB + costs$annaulFixedOM
  costs$gas <- scenario
  return(costs)
}

costs <- costFormat(scenario="Baseline")
costs <- rbind(costs, costFormat(" high gas price", "High gas"))
costs <- rbind(costs, costFormat(" LCOE low", "Baseline w/\ninfrastructure costs"))
costs <- rbind(costs, costFormat(" LCOE high", "High gas w/\ninfrastructure costs"))

costs <- merge(costs, mitCostSummary, by.x=c("Scenario", "RCM", "Dose.response", "gas"), 
                                      by.y=c("scenario", "RCM", "dose", "gas") , all.x=T)

costs$gas <- mapvalues(costs$gas, from=c("Baseline w/\ninfrastructure costs", "High gas w/\ninfrastructure costs"), 
                                    to=c("Baseline\n(LCOE approach)", "High gas\n(LCOE approach)"))

ggplot(costs, aes(x=gas, y=total, fill=Scenario)) + geom_bar(stat="identity", position = position_dodge()) + theme_classic() + 
  scale_fill_brewer(palette="Dark2") +
  geom_point(aes(y=CO2/3, group=Scenario), color="black", position = position_dodge(width = 0.90), shape=23, fill="black") + 
  scale_y_continuous(sec.axis = sec_axis(~.*3, name=expression("Cost of mitigation ($ per ton CO"[2]*")"))) +
  xlab("") + ylab("Annual mitigation cost ($ billion)") +
  theme(legend.position = "top",
        text = element_text(size=14),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(size=10),
        legend.title=element_blank()) 

setwd(paste(baseWD,"Plots", sep="/"))
ggsave("Cost and COM comparison.pdf", width=6, height=4)


## health benefits across scenarios ####
listOfSummaryResults <- list(summaryResults, summaryResultsHighGas, summaryResultsLCOElow, summaryResultsLCOEhigh)
listNames <- c("Baseline",  "High gas", "Baseline w/\ninfrastructure costs", "High gas w/\ninfrastructure costs")

for(i in 1:length(listOfSummaryResults)){
  mergedTemp <- calcHCBenefit(listOfSummaryResults[[i]])
  healthSubMelt <- meltHealthBenefits(mergedTemp)
  healthSubMelt$gas <- listNames[i]
  if(i == 1){
      healthSubFinal <- healthSubMelt
  } else {
    healthSubFinal <- rbind(healthSubFinal, healthSubMelt)
  }
}; rm(mergedTemp); rm(healthSubMelt); rm(listOfSummaryResults)


healthSubFinal$gas <- mapvalues(healthSubFinal$gas, from=c("Baseline w/\ninfrastructure costs", "High gas w/\ninfrastructure costs"), 
                                         to=c("Baseline\n(LCOE approach)", "High gas\n(LCOE approach)"))

ggplot(healthSubFinal[healthSubFinal$dose=="ACS", ], aes(x=RCM, y=value, fill=variable, label=round(value))) + 
  facet_wrap(~gas, nrow=1) +
  geom_bar(stat="identity") + theme_classic() + 
  geom_text(size = 3, position=position_stack(vjust=0.5), fontface="bold") +
  theme_classic() + 
  scale_y_continuous(limits=c(0, 70), sec.axis = sec_axis(~.*1E3/9, name = "Annual deaths avoided")) +
  ylab("Annual health benefits (billion $)") + xlab("Air quality model") + 
  theme(text = element_text(size=14),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(size=10),
        legend.position = c(0.18, 0.9),
        legend.title=element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  scale_fill_manual(values=plotColors,
                    breaks=rev(c("Climate-only", "additionalHealth")),
                    labels=rev(c("Climate-only health benefits", "Additional benefits from health + climate"))) 

setwd(paste(baseWD,"Plots", sep="/"))
ggsave("Health benefits by gas scenario.pdf", width=10, height=4)


## linear model results ####
setwd(baseWD)
linearResult <- read.csv("Linear results.csv", header=T)
rownames(linearResult) <- linearResult[,1]
linearResult <- linearResult[,-1]
linearResult <- as.data.frame(t(linearResult))

linearResult$cost <- annualize(linearResult[,"New CAPEX ($)"], life=20, dr=0.07) + linearResult[,"New Variable costs ($)"] - linearResult[,"Variable cost savings ($)"]

linearResult <- linearResult[,c("Health Damages ($)", "Climate Damages ($)", "cost")]
linearResult$scenario <- c("Health", "Climate", "Health + Climate", "Baseline")

linearResult <- melt(linearResult, id.vars="scenario")
linearResult$variable <- mapvalues(linearResult$variable, from = c("Health Damages ($)", "Climate Damages ($)", "cost"),
                                                          to = c("Health damages", "Climate damages", "Mitigation costs"))

linearResult$variable <- factor(linearResult$variable, levels=c("Climate damages", "Health damages","Mitigation costs"))
linearResult$textColor <- ifelse(linearResult$value == 0, "white", "black")

# correct error in cost results
linearResult[linearResult$variable =="Mitigation costs" & linearResult$scenario %in% c("Health", "Health + Climate"), "value" ] <- 0.2E9 +
            linearResult[linearResult$variable =="Mitigation costs" & linearResult$scenario %in% c("Health", "Health + Climate"), "value" ]

ggplot(data=linearResult, aes(x=scenario, y=value/1E9, fill=variable, label=round(value/1E9))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(color=textColor), size = 3, position = position_dodge(0.9), vjust=1.5, fontface="bold") +   #vjust=-0.5
  theme_classic() + 
  ylab("Annual damages or costs (billion $)") + xlab("Optimization scenario") + 
  theme(text = element_text(size=14),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(size=10),
        legend.position = "top",
        legend.title=element_blank(),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  scale_color_manual(breaks=c("black", "white"), values=c("black", "white"), guide=FALSE) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#999999"))    #values=c("chocolate1", "#2b8cbe", "darkgreen"),
                    
setwd(paste(baseWD,"Plots", sep="/"))
ggsave("Linear analysis results.pdf",  width=7, height=4)

## Emissions reductions ####

emissions <- summaryResults[,c("scenario", "RCM", "dose", "CO2", "SO2", "NOx")]
emissions <- melt(emissions, measure.vars = c("CO2", "SO2", "NOx"))
emissionsBaseline <- emissions[emissions$scenario == "Baseline",]
colnames(emissionsBaseline)[colnames(emissionsBaseline) == "value"] <- "baseline"
emissionsBaseline$scenario <- NULL
emissions <- merge(emissions, emissionsBaseline, by=c("RCM", "dose", "variable"))

emissions$change <- emissions$baseline - emissions$value
emissions$percentChange <- round(emissions$change / emissions$baseline * 100, 1)


# climate benefits
summarizeClimateBenefits <- function(results, GWP, leakage){
  results <- results[results$RCM == "AP3" & results$dose == "ACS",  c("scenario", "climate")]
  results$GWP <- GWP
  results$leakage <- leakage
  return(results)
}

climateBen <- summarizeClimateBenefits(summaryResultsNoLeakage, GWP="No leakage", leakage="")
climateBen <- rbind(climateBen, summarizeClimateBenefits(summaryResults, GWP="100-year", leakage="3%"))
climateBen <- rbind(climateBen, summarizeClimateBenefits(summaryResultshighLeakage, GWP="100-year", leakage="5%"))
climateBen <- rbind(climateBen, summarizeClimateBenefits(summaryResultshighGWP, GWP="20-year", leakage="3%"))
climateBen <- rbind(climateBen, summarizeClimateBenefits(summaryResultshighGWPhighLeakage, GWP="20-year", leakage="5%"))

climateBen$combo <- factor(paste(climateBen$GWP, " GWP,\n", climateBen$leakage, " leakage", sep=""))
climateBen$combo <- mapvalues(climateBen$combo, from="No leakage GWP,\n leakage", to="No leakage")
climateBen$combo <- relevel(climateBen$combo, ref="No leakage")

ggplot(climateBen, aes(x=combo, y=climate/1E9, fill=scenario)) + geom_bar(stat="identity", position=position_dodge()) +
  xlab("") + ylab("Annual climate damages ($ billion)") + theme_classic() +
  theme(text=element_text(color="black", size=12),
        axis.text=element_text(color="black", size=10),
        legend.position = "top") +
  guides(fill=guide_legend(title="")) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

setwd(paste(baseWD,"Plots", sep="/"))
ggsave("Climate damages with leakage and GWP.pdf",  width=7, height=4)


## State emissions rates ####
md <- "AP3"
dr <- "ACS"

stateSums <- summarizeStateEmissionsRates(md, dr)

baselineState <- stateSums[stateSums$Scenario == "Baseline",]
baselineState <- baselineState[c("state", "CO2..tons.")]
colnames(baselineState)[colnames(baselineState) == "CO2..tons."] <- "baselineCO2" 

stateSums <- merge(stateSums, baselineState, all=T, by="state")
stateSums$percentChange <- round((stateSums$baselineCO2 - stateSums$`CO2..tons.`) / stateSums$baselineCO2 * 100, 1)

stateSums <- stateSums[stateSums$Scenario != "Baseline",]
stateSums <- stateSums[,c("state", "Scenario", "percentChange")]
stateSumsWide <- spread(stateSums, key=Scenario, value=percentChange)

stateSumsWide$region <- mapvalues(stateSumsWide$state, 
                                  from = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "MD", "DC", "DE", "PA", 
                                           "WV", "VA", "NC", "SC", "GA", "FL", "AL", "MS", "TN", "KY", "LA","AR",
                                           "OH", "IN", "IL", "WI", "MI", "MN", "KS", "MO", "IA", "NE", "ND", "SD",
                                           "TX",  "OK", "NM",  "AZ",
                                           "NV","CA", "WA", "OR", "ID", "CO", "UT", "WY", "MT"),
                                  to = c(rep("Northeast", 12),
                                         rep("Southeast", 12),
                                         rep("Midwest", 12),
                                         rep("Southwest", 4),
                                         rep("West", 9)))

# ggplot(data=stateSumsWide, aes(x=Climate, y=`Health + Climate`, color=region)) + 
#   geom_abline(slope=1, linetype=2) + geom_point() + 
#   theme_classic() +
#   xlab("% CO2 reduction climate-only scenario") + 
#   ylab("% CO2 reduction healht + climate scenario") +
#   guides(color=guide_legend(title="Region")) +
#   scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) + 
#   theme(text = element_text(size=14),
#         axis.text = element_text(size=12, color="black"),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12))


stateSums <- merge(stateSums, stateFips[,c("state", "region")], all.x=T)
plotStateEmissionsChanges(stateSums)

#for (pollutant in c("CO2", "SO2", "NOx")){
#  stateEmissionsMapPanel(stateSums, pollutant, md, dr)
#}

## CO2 target sensitivity plot ####

sensCO2 <- loadCO2Sensitivity()

# calculate differences in health benefits between climate-only and health + climate
healthDiffs <- ddply(sensCO2, ~ RCM + dose + VSL + SCC + target, summarize, healthBenefit=diff(healthBenefit))
healthDiffs$label <- "Diff"
# bar plot of one dr / rcm

ggplot(data=sensCO2, aes(x=target*100, y=healthBenefit/1E9, fill=scenario)) + geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(dose ~ RCM) + theme_classic() +
  geom_line(data=healthDiffs, aes(x=target*100, y=healthBenefit/1E9, color=label), inherit.aes=F) +
  geom_point(data=healthDiffs, aes(x=target*100, y=healthBenefit/1E9, color=label), inherit.aes=F, shape=8, size=2) +
  scale_color_manual(values="black", labels="Additional benefits\nfrom Health + Climate") +
  scale_fill_manual(values=rev(plotColors)) +
  xlab("CO2 reduction (%)") + ylab("Annual health benefits (billion $)") +
  guides(color=guide_legend(title="", order = 2), fill=guide_legend(title="", order = 1, byrow=T, nrow=2)) +
  theme(text = element_text(size=14),
        axis.text = element_text(size=14, color="black"),
        axis.title = element_text(size=14, color="black"),
        legend.text = element_text(size=12),
        legend.title=element_text(size=12),
        legend.position = "bottom",
        legend.spacing.y = unit(0, 'cm'))

setwd(paste(baseWD,"Plots", "Sensitivity grids", sep="/"))
ggsave("Sensitivity of benefits to CO2 target.pdf", width=8, height=6)

## SAVE ####
setwd(baseWD)
save.image("Analysis results.RData")
