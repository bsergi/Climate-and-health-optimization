# Main model 
# Author: Brian Sergi
# Created: July 17, 2018
# Latest version: March 14, 2020

## Notes for users

# This script controls building and running set of optimization scenarios via pyomo. The model is built to use or convert to $2017. 
# Users must have the gurobi solver available and on the path the current file in order to run; users can also 
# specify the path to solver via executable (see SolverFactory in "buildOptimizationModel.py)

# Currently there are a few slightly negative marginal damages (19 values for NOx using AP2) due to nonlinear chemistry.
# For the purposes of this model, these are replace with values of zero. See details on the AP2/AP3 at https://www.caces.us/ and
# in publications by Nick Muller for more information. 
    
   
## Import libraries

import os 
from loadCEMS import *
from loadMD import *
from buildOptimizationModel import *
from costSummary import *
from functionDefs import timeUpdate

startTotal = timeUpdate("\nBeginning model runs:\n\n")

# default settings for pandas
pd.options.display.max_columns = 45

## Load CEMS data
start = timeUpdate("Pre-processing...")
CEMS, sourceReceptorMatrix, sourceReceptorH6C = buildCEMSData(yearCEMS=2017)
timeUpdate("complete (%0.1f seconds).", start=start)

## Output results

# helper function for exporting optimization results
def exportResults(results, md, doseResponse, model, scenarios, suffix="", saveDir='Results'):    
    # combine results into data frames for export to csv
    plantResults = pd.DataFrame()
    gasResults = pd.DataFrame()
    summaryResults = pd.DataFrame()
    renewableResults = pd.DataFrame()
    sourceReceptorResults = pd.DataFrame()
    
    # specify source receptor matrix
    if doseResponse == "ACS":
        SR = sourceReceptorMatrix
    elif doseResponse == "H6C":
        SR = sourceReceptorH6C 
    
    # Baseline results
    summaryResults = summaryResults.append(model.originalDamages)
    baselineReceptor = summarizeReceptorDamages(model.CEMS, SR, "Baseline")
    sourceReceptorResults = sourceReceptorResults.append(baselineReceptor)
    # Note: currently using AP2/AP3 source-receptor matrix for all RCMs

    os.chdir(os.getcwd() + '/' + saveDir + '/' + md + " " + doseResponse)
    model.CEMS.to_csv("Baseline - plants "  + md + " " + doseResponse + ".csv", index=False)
    os.chdir('../..')
    
    # iterate over results by scenario
    for scenario in scenarios:
        plantResults = plantResults.append(results[scenario]["Plants"])
        gasResults = gasResults.append(results[scenario]["Gas"])
        summaryResults = summaryResults.append(results[scenario]["Summary"])
        renewableResults = renewableResults.append(results[scenario]["Renewables"])
        scenarioReceptor = summarizeReceptorDamages(results[scenario]["Plants"], SR, scenario, results[scenario]["Gas"])
        sourceReceptorResults = sourceReceptorResults.append(scenarioReceptor)
        
    # export results to files
    os.chdir(os.getcwd() + '/' + saveDir + '/' + md + " " + doseResponse)
    plantResults.to_csv("Results - plants " + md + " " + doseResponse + suffix + ".csv", index=False)
    gasResults.to_csv("Results - gas " + md + " " + doseResponse + suffix + ".csv", index=False)
    summaryResults.to_csv("Results - summary " + md + " " + doseResponse + suffix + ".csv")    
    renewableResults.to_csv("Results - renewables " + md + " " + doseResponse + suffix + ".csv", index=False)  
    sourceReceptorResults.to_csv("Results - source receptor " + md + " " + doseResponse + suffix + ".csv", index=False)                                                   
    os.chdir('../..')
            
## Set parameters and run scenarios

# Note: toggle different optimization runs using '' comments (see below)

# settings (targets represented as % reduction from current levels)  
percentTargets = {"CO2": 0.30, "NOx": 0, "SO2": 0}

scenarios = ["Climate", "Health + Climate"]

# parameter settings for dose-response function, RCM air quality models and save directory    
doseResponseList = ["ACS", "H6C"]
rcmList = ["AP3", "EASIUR", "InMAP"]
saveDir = "Results"

# run only one model
md="AP3"
doseResponse="ACS"
results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9)
exportResults(results, md, doseResponse, model, scenarios)

# same  model, lower SCC
'''
results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=7, VSL=9)
exportResults(results, md, doseResponse, model, scenarios, suffix=" low SCC")
'''

# version with MILP formulation
'''
results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=7, VSL=9, MILP=True)
exportResults(results, md, doseResponse, model, scenarios, suffix=" MILP version")
'''

'''
# full range of RCMs (main results)
for md in rcmList:
    for doseResponse in doseResponseList:
                
        # basic model
        
        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9)
        exportResults(results, md, doseResponse, model, scenarios, saveDir=saveDir)
        
        # same  model, lower SCC

        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=6, VSL=9)
        exportResults(results, md, doseResponse, model, scenarios, suffix=" low SCC", saveDir=saveDir)
        
        # high natural gas prices
        
        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9, gasPrice=63)
        exportResults(results, md, doseResponse, model, scenarios, suffix=" high gas price", saveDir=saveDir)
        
        # LCOE approach (used only as a sensitivity)
        
        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9, LCOE='low')
        exportResults(results, md, doseResponse, model, scenarios, suffix=" LCOE low", saveDir=saveDir)
        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9, LCOE='high')
        exportResults(results, md, doseResponse, model, scenarios, suffix=" LCOE high", saveDir=saveDir)
        
        # leakage / GWP
        
        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9, leakage=0.05)
        exportResults(results, md, doseResponse, model, scenarios, suffix=" high leakage", saveDir=saveDir)
                
        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9, GWP=72)
        exportResults(results, md, doseResponse, model, scenarios, suffix=" high GWP", saveDir=saveDir)
        
        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9, GWP=72, leakage=0.05)
        exportResults(results, md, doseResponse, model, scenarios, suffix=" high GWP, high leakage", saveDir=saveDir)  
        
        results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9, GWP=0, leakage=0)
        exportResults(results, md, doseResponse, model, scenarios, suffix=" no leakage", saveDir=saveDir)        

'''
## VSL and SCC analysis

start = timeUpdate("Running VSL and SCC sensitivity analysis...")
print("")

# range for SCC and VSLs in sensitivity analysis
SCCList = [6, 20, 40, 60, 80, 100]
VSLList = [3, 6, 9, 12, 15, 18]

for md in rcmList:
    for doseResponse in doseResponseList:
        sensitivityResults = pd.DataFrame()
        for SCC in SCCList:                     
            for VSL in VSLList:
                for scenario in scenarios:                  
                    temp = runBasicOptimization(CEMS, SCC, VSL, md, doseResponse, percentTargets, scenario)
                    sensitivityResults = sensitivityResults.append(temp)
                
        os.chdir(os.getcwd() + '/' + saveDir + '/' + md + " " + doseResponse)
        sensitivityResults.to_csv("Sensitivity - VSL and SCC " + md + " " + doseResponse + ".csv", index=False)
        os.chdir('../..')

timeUpdate("...complete (%0.2f seconds).", start=start)

## Emissions target sensitivity

start = timeUpdate("Running emissions target sensitivity analysis...")
print("")

# range for CO2 reduction levels
reductionList = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4]
'''
for md in rcmList:
    for doseResponse in doseResponseList:
        sensitivityCO2 = pd.DataFrame()
        for reductionLevel in reductionList: 
            for scenario in scenarios:                  
                newPercentTargets = {"CO2": reductionLevel, "NOx": 0, "SO2": 0}
                # using baseline SCC and VSL values ($40 per ton and $9 million)
                temp = runBasicOptimization(CEMS, 40, 9, md, doseResponse, newPercentTargets, scenario)
                sensitivityCO2 = sensitivityCO2.append(temp)
        os.chdir(os.getcwd() + '/' + saveDir + '/' + md + " " + doseResponse)
        sensitivityCO2.to_csv("Sensitivity - CO2 target " + md + " " + doseResponse + ".csv", index=False)
        os.chdir('../..')

timeUpdate("...complete (%0.2f seconds).", start=start)
'''

## End script

timeUpdate("Model runs complete (%0.1f seconds).", start=startTotal)

#import sys
#sys.exit()