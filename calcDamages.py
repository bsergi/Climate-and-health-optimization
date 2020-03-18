# calculate damages
# Author: Brian Sergi
# Created: July 17, 2018
# Latest version: March 14, 2020

## Notes

# Functions to calculate health and climate damages for a given emissions cenarios
# Note that matrix multiplication may result in slight rounding errors due to precision loss

## LIBRARIES

import pandas as pd
import numpy as np
import os

## Calculate unit level damages

def calcEmissions(CEMS):
    CEMS = CEMS.copy()
    CEMS['NOx (tons)'] = CEMS['NOx emissions rate (tons/MWh)'] * CEMS['Gross Load (MW-h)'] 
    CEMS['SO2 (tons)'] = CEMS['SO2 emissions rate (tons/MWh)'] * CEMS['Gross Load (MW-h)'] 
    CEMS['CO2 (tons)'] = CEMS['CO2 emissions rate (tons/MWh)'] * CEMS['Gross Load (MW-h)'] 
    return CEMS
    
# function defining leakage rate    
def leakageEquation(heatRate, leakRate, GWP_estimate):
    
    heatContentNG = 36643                           # BTU per cubic meter
    densityNG = 0.8                                 # kg per cubic meter
    methaneFraction = 0.844                         # fraction of natural gas that is methane (by mass)
    
    convertedRate = heatRate * 1E6 / heatContentNG * densityNG / 1E3 * methaneFraction        
    return leakRate * GWP_estimate * convertedRate
        
# returns health and climate damages summarized by county
def calculateDamages(df, md, SCC, doseResponse, leakage, GWP, heatRateVal=None):        
    df = df.copy()
    
    # calculate SO2 and NOx damages by unit
    NOxDamage = df['NOx (tons)'] * df['NOx_' + md + '_' + doseResponse]    
    SO2Damage = df['SO2 (tons)'] * df['SO2_' + md + '_' + doseResponse]
    df['Health Damages ($)'] = NOxDamage + SO2Damage

    # calculate CO2 damages
    # include methane leakage as CO2 equivalent 
    if heatRateVal is None:
        df['CO2 eq leakage (tons)'] = df['Heat rate'].apply(leakageEquation, leakRate=leakage, GWP_estimate=GWP) * df['Gross Load (MW-h)']
    else:
        df['CO2 eq leakage (tons)'] = leakageEquation(heatRateVal, leakRate=leakage, GWP_estimate=GWP) * df['Gross Load (MW-h)']
        
    df['Climate Damages ($)'] = (df['CO2 (tons)'] + df['CO2 eq leakage (tons)']) * SCC 
        
    # convert to integers
    df['Health Damages ($)'] = df['Health Damages ($)'].astype('int')
    df['Climate Damages ($)'] = df['Climate Damages ($)'].astype('int')
    return df
    
## Emissions targets 
# returns absolute annual emissions targets based on % reductions from current year
def getTargets(CEMS, percentTargets):    
    CO2 = (1 - percentTargets['CO2']) * CEMS['CO2 (tons)'].sum()
    NOx = (1 - percentTargets['NOx']) * CEMS['NOx (tons)'].sum()
    SO2 = (1 - percentTargets['SO2']) * CEMS['SO2 (tons)'].sum() 
    targets = {"CO2": CO2, "NOx":NOx, "SO2": SO2}    
    return targets
    
## Calculate receptor damages

def summarizeCountyEmissions(CEMS, newGas):
    subCEMS = CEMS[['FIPS Code', 'SO2 (tons)', 'NOx (tons)']].copy()
    if newGas is not None:
        newSubCEMS = newGas[['FIPS Code', 'SO2 (tons)', 'NOx (tons)']].copy()
        subCEMS = subCEMS.append(newSubCEMS)             
    countySums = subCEMS.groupby(['FIPS Code']).agg({"SO2 (tons)": "sum", "NOx (tons)": "sum"})
    #countySums.reset_index(inplace=True)
    return countySums        
            
def createSparseEmissions(fips, countyEmissions):
    sparse = pd.DataFrame({'SO2 (tons)':0, 'NOx (tons)':0}, index=fips)
    sparse.update(countyEmissions)    
    return sparse["NOx (tons)"], sparse["SO2 (tons)"]

def calcReceptorDamages(CEMS, SR_Matrices, scenario, newGas):
    SO2_SR, NOx_SR = SR_Matrices[0], SR_Matrices[1]
    fips = SR_Matrices[0].index                                     # note: fips not sorted because of Miami-Dade
    
    # get county-level emissions summary
    countyEmissions = summarizeCountyEmissions(CEMS, newGas)
    # sparse matrices (0 for no emissions in county)
    NOx, SO2 = createSparseEmissions(fips, countyEmissions)    
    
    # save emissions for outside source receptor analysis
    '''
    prevWD = os.getcwd()
    print(scenario)
    os.chdir(os.getcwd() + '/Results/SR/' + scenario)
    print(os.getcwd())
    SO2.to_csv("SO2 emissions.csv")
    NOx.to_csv("NOx emissions.csv")
    SO2_SR.to_csv("SO2 SR.csv")
    NOx_SR.to_csv("NOx SR.csv")
    os.chdir(prevWD)                         
    '''
    
    # matrix multiplication
    if all(SO2.index == SO2_SR.index):
        SO2_damage = SO2.dot(SO2_SR)
    else:
        print("ERROR: Misaligned fips codes for SO2 SR matrix")   
    if all(NOx.index == NOx_SR.index):
        NOx_damage = NOx.dot(NOx_SR)
    else:
        print("ERROR: Misaligned fips codes for NOx SR matrix")        
            
    damages = pd.DataFrame({"SO2 Damages ($)": SO2_damage, 
                            "NOx Damages ($)": NOx_damage}, index = fips)                            
    damages.reset_index(inplace=True)
    damages.rename(columns={"index": "FIPS Code"}, inplace=True)    
    damages["Health Damages ($)"] = damages["SO2 Damages ($)"] + damages["NOx Damages ($)"]   
    
    return damages
    
def summarizeReceptorDamages(CEMS, SR_Matrices, scenario, newGas=None):    
    plantDamages = calcReceptorDamages(CEMS, SR_Matrices, scenario, newGas=newGas)
    plantDamages['Scenario'] = scenario
    return plantDamages
    
# function to adjust VSL (assumed to be in million $)
# note: this assumes RCM baseline for 2015 is 8.7 (based on EPA value)
def adjustVSL(CEMS, newVSL, md, doseResponse, baseVSL=8.7):
    CEMS['NOx_' + md + '_' + doseResponse] =  CEMS['NOx_' + md + '_' + doseResponse] * newVSL/baseVSL 
    CEMS['SO2_' + md + '_' + doseResponse] =  CEMS['SO2_' + md + '_' + doseResponse] * newVSL/baseVSL 
    return CEMS

## Summarize total damages

def sumDamages(CEMS):
    results = dict()
    results['Climate Damages ($)'] = CEMS['Climate Damages ($)'].sum()
    results['Health Damages ($)'] = CEMS['Health Damages ($)'].sum()
    results["Annualized cost ($)"] = CEMS["Annualized cost ($)"].sum()
    return results

def sumTotalDamages(originalDamages, results, scenarios):    
    totalResults = dict()    
    for scenario in scenarios:
        if scenario == "Baseline":
            totalResults['Baseline'] = originalDamages
        elif scenario == "Standard":
            # results for emission standard ported from model V1
            totalResults['Standard'] = dict({'Climate Damages ($)': 4.736367e+10, 
                                             'Health Damages ($)': 1.991978e+10,
                                             'Annualized cost ($)': 1.614e+10}) 
        else:
            totalResults[scenario] = sumDamages(results[scenario])
        
    # plot (see plotting module)
    # totalDamageBarPlot(totalResults, "Damage barplot.pdf")
    
    resultSummary = pd.DataFrame.from_dict(totalResults)    
    
    os.chdir(os.getcwd() + '/Results')
    resultSummary.to_csv("Costs and damages by scenario.csv", index=True)                                                          
    os.chdir('..')
    
# add in results for emissions standard
    
## Summarize change in emissions

# total emissions by fuel type

# county level emissions
def emissionsChangeByCounty(CEMS, results, saveDir):    
    totalResults = CEMS[["FIPS Code", "CO2 (tons)", "SO2 (tons)", "NOx (tons)", "Gross Load (MW-h)"]].copy()
        
    for scenario in results.keys():
        if all(results[scenario]['FIPS Code'] == totalResults['FIPS Code']):
            for pollutant in ["CO2", "SO2", "NOx"]:
                totalResults[pollutant + " (tons) " + scenario] = results[scenario][pollutant + " (tons)"].copy()
            totalResults["Load (MW-h) " + scenario] = results[scenario]["Gross Load (MW-h)"].copy() + results[scenario]["New NGCC load (MW-h)"].copy()

        else:
            print("Error in saving emissions: FIPS code mismatch for scenario " + scenario)
             
    # save to csv
    baseWD = os.getcwd()   
    os.chdir(baseWD + '/' + saveDir + '/Plots/Maps')
    totalResults.to_csv("County level emissions.csv", index=False)                                                          
    os.chdir(baseWD)  

def fuelEmissisionSummary(CEMS, results, saveDir):

    fuelResults = CEMS[["Fuel", 'Unit category', "CO2 (tons)", "SO2 (tons)", "NOx (tons)", "Gross Load (MW-h)"]].copy()
    
    fuelResults = pd.DataFrame(fuelResults.groupby(['Fuel', 'Unit category']).agg({"CO2 (tons)": "sum",
                                                     "SO2 (tons)": "sum",
                                                     "NOx (tons)": "sum",
                                                     "Gross Load (MW-h)": "sum"})).reset_index()
                                                     
    fuelResults.columns = [str(col) + ' - ' + 'Baseline' if col not in ['Fuel', 'Unit category'] else col for col in fuelResults.columns]

                                                     
    for scenario in results.keys():
        fuelResultsScenario = results[scenario][["Fuel", "Unit category", 
                                                 "CO2 (tons)", "SO2 (tons)", "NOx (tons)", 
                                                 "Gross Load (MW-h)"]].copy()
    
        scenarioSummary = pd.DataFrame(fuelResultsScenario.groupby(['Fuel', 'Unit category']).agg({"CO2 (tons)": "sum",
                                                                                                   "SO2 (tons)": "sum",
                                                                                                   "NOx (tons)": "sum",
                                                                                                   "Gross Load (MW-h)": "sum"})).reset_index()
        # add in NGCC results
        NGCC = results[scenario][["CO2 (tons) NGCC", "SO2 (tons) NGCC", "NOx (tons) NGCC", "New NGCC load (MW-h)"]].copy().sum() 
        NGCC = pd.DataFrame(NGCC).transpose()
        NGCC['Unit category'] = 'New combined cycle'
        NGCC['Fuel'] = 'Natural gas'     
        
        NGCC.rename(columns={'CO2 (tons) NGCC': 'CO2 (tons)',
                             'SO2 (tons) NGCC': 'SO2 (tons)', 
                             'NOx (tons) NGCC': 'NOx (tons)',
                             'New NGCC load (MW-h)': 'Gross Load (MW-h)'}, inplace=True)
               
        scenarioSummary = scenarioSummary.append(NGCC, sort=True)
        # extend to other fuels as needed...
                
        # rename columns
        scenarioSummary.columns = [str(col) + ' - ' + scenario if col not in ['Fuel', 'Unit category'] else col for col in scenarioSummary.columns]
        
        # merge
        fuelResults = pd.merge(fuelResults, scenarioSummary, how='outer', on=["Fuel", "Unit category"])
               
    # save to csv
    baseWD = os.getcwd()   
    os.chdir(baseWD + '/' + saveDir)
    fuelResults.to_csv("Emissions and generation by fuel type.csv", index=False)                                                          
    os.chdir(baseWD)  
            