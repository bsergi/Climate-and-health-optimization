# Sensitivity analysis plotting functions
library(plotrix)
library(tidyr)
library(viridis)

# formatting (returns matrix in billions of $)
formatSensData <- function(sens){
  #sens$benefits <- sens[, "Climate.benefits...."] + sens[, "Health.benefits...."]
  sens$benefits <- sens[, "Climate.benefits...."] + sens[, "Health.benefits...."] - sens[,'Annual.costs....']
  
  # subset to Health and climate scenarios
  #sens <- sens[sens$Scenario == "Health + Climate",]
  # additional benefits
  sens <- ddply(sens, ~ RCM + Dose.response + VSL + SCC + CO2.target, summarize, benefits=diff(benefits))
  #sens <- sens[, c("VSL", "SCC", "benefits")]
  #sens <- spread(sens, key="VSL", value="benefits") 
  #rownames(sens) <- sens[,1]
  #sens <- as.matrix(sens[,-1]) / 1E9              # convert to billions
  return(sens)
}

loadAllSens <- function(mdNames, drNames, format=T){
  i <- 0
  for (md in mdNames){
    for (dr in drNames){
      setwd(paste(baseWD, "Results", paste(md, dr), sep="/"))
      sens <- read.csv(paste0("Sensitivity - VSL and SCC ", md, " ", dr, ".csv"))
      if(format){
        sens <- formatSensData(sens)              # calc total benefits and reshape
      }
      if (i == 0){
        allData <- sens
      } else {
        allData <- rbind(allData, sens)
      }
      i <- i + 1
    }
  }
  
  return(allData)

}


# labels for multipaneled figures
# Source: https://logfc.wordpress.com/2017/03/15/adding-figure-labels-a-b-c-in-the-top-left-corner-of-the-plotting-region/
fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {
  
  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))
  
  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
    
    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }
  
  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }
  
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
  
  x1 <- switch(pos,
               topleft     =x[1] + sw, 
               left        =x[1] + sw,
               bottomleft  =x[1] + sw,
               top         =(x[1] + x[2])/2,
               center      =(x[1] + x[2])/2,
               bottom      =(x[1] + x[2])/2,
               topright    =x[2] - sw,
               right       =x[2] - sw,
               bottomright =x[2] - sw)
  
  y1 <- switch(pos,
               topleft     =y[2] - sh,
               top         =y[2] - sh,
               topright    =y[2] - sh,
               left        =(y[1] + y[2])/2,
               center      =(y[1] + y[2])/2,
               right       =(y[1] + y[2])/2,
               bottomleft  =y[1] + sh,
               bottom      =y[1] + sh,
               bottomright =y[1] + sh)
  
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
  
  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}

## plot difference in health benefits for different levels of CO2 reduction
readSensitivity <- function(rcm, doseResponse, type){
  scenario <- paste(rcm, doseResponse)
  setwd(paste0(baseWD, "/Results/", scenario))
  temp <- read.csv(paste0("Sensitivity - ", type, " ", scenario, ".csv"))
  return(temp)
}


loadCO2Sensitivity <- function(){
  for (rcm in RCMs){
    for (doseResponse in doseResponses){
      temp <- readSensitivity(rcm, doseResponse, type="CO2 target")
      
      if(rcm == RCMs[1] & doseResponse == doseResponses[1]){
        summaryResults <- temp
      } else {
        summaryResults <- rbind(summaryResults, temp); rm(temp)
      }
    }
  }
  setwd(baseWD)
  colnames(summaryResults) <- c("RCM", "dose", "VSL", "SCC", "target", "scenario", "climateBenefit", "healthBenefit", "cost", "coalGen", "gasCap", "windCap", "solarCap")
  summaryResults$dose <- factor(summaryResults$dose, levels=c("ACS", "H6C"))
  summaryResults$RCM <- factor(summaryResults$RCM, levels=c("AP3", "InMAP", "EASIUR"))
  
  summaryResults$scenario <- mapvalues(summaryResults$scenario, from="Climate", to="Climate-only")
  
  return(summaryResults)
}



