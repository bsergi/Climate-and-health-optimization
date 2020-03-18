# function defs 
# Author: Brian Sergi
# Created: July 17, 2018
# Latest version: March 14, 2020

## Notes

# Helper functions to build and run a pyomo optimization model. Primarily called by functions in buildOptimizationModel.py

## Import libraries

import os
import time
import pandas as pd
import numpy as np

from loadCEMS import *
from calcDamages import * 
from costSummary import * 
from functionDefs import *

from pyomo.environ import *
from pyomo.opt import SolverStatus, TerminationCondition

## time keeper

# function to provide updates on code progress
def timeUpdate(update, start=None):
    if start == None:
        print(update, end="")
        return time.time()
    else:
        elapsed = time.time() - start
        print(update % elapsed)


##  intialization functions

# maps units to the county where they emit
def getFipsMapping(CEMS):
    fips = CEMS[['FIPS Code']].copy()
    fips['Unit'] = fips.index.map(str)
    fips = fips.groupby('FIPS Code')['Unit'].apply(list)    
    fips = dict(zip(fips.index.map(str), fips.values))    
    return fips
    
# returns dict mapping unit to current generation
def getCurrentGeneration(CEMS):
    gen = CEMS['Gross Load (MW-h)'].copy()
    gen.replace(np.nan, 0, inplace=True)                                                    # replace NAs with zeros
    genDict = dict(zip(gen.index.astype(str), gen.values))
    return genDict

# returns baseline sum of generation at county level
def getCountyGeneration(CEMS):    
    gen = CEMS[['FIPS Code', 'Gross Load (MW-h)']].copy()
    gen['Gross Load (MW-h)'].replace(np.nan, 0, inplace=True)                               # replace NAs with zeros    
    genCounty = gen.groupby('FIPS Code').agg({'Gross Load (MW-h)': 'sum'})       
    genDict = dict(zip(genCounty.index.map(str), genCounty['Gross Load (MW-h)'].values))
    return genDict
  
# returns sum of O&M and fuel costs
def getUnitVarCosts(CEMS):    
    costs = CEMS[['Fuel costs ($/MWh)', 'Var O&M ($/MWh)']].copy()
    costs['Total var costs ($/MWh)'] = costs['Fuel costs ($/MWh)'] + costs['Var O&M ($/MWh)']
    costs.index = costs.index.map(str)
    costDict = dict(zip(costs.index, costs['Total var costs ($/MWh)'].values))
    return costDict  

# set up marginal health damages from RCMs
def formatMarginalDamages(CEMS, MD, dose, SCC, county=False): 
    mdNames = ['SO2_' + MD + '_' + dose, 'NOx_' + MD + '_' + dose]
    MDs = CEMS[mdNames].copy()
    MDs['CO2'] = SCC    
    MDs.rename(columns={mdNames[0]: 'SO2', mdNames[1]:'NOx'}, inplace=True)
    
    # for county MDs for new gas, take average (would affect EASIUR/InMAP estimates)
    if county:
        MDs['County'] = CEMS['FIPS Code']
        MDs = MDs.groupby(['County']).agg({"SO2": "mean", "NOx": "mean", "CO2":"mean"})
    
    # clip any negative marginal damages to 0 (issue for AP2 results)
    MDs.loc[MDs['NOx'] < 0, 'NOx'] = 0
    
    MDs['UnitID'] = MDs.index.astype(str)
    MDs = pd.melt(MDs, id_vars=['UnitID'], value_vars=['CO2', 'SO2', 'NOx'])    
    # tuple keys of index and pollutant
    MDs['Tuple'] = list(zip(MDs.UnitID, MDs.variable))
    MDdict = dict(zip(MDs.Tuple, MDs.value))    
    return MDdict
   
# returns tuples of emissions rates by unit 
def formatEmissionsRates(CEMS): 
    erNames = ['CO2 emissions rate (tons/MWh)', 'SO2 emissions rate (tons/MWh)', 'NOx emissions rate (tons/MWh)']
    rates = CEMS[erNames].copy()
    rates.rename(columns={erNames[0]: 'CO2', erNames[1]: 'SO2', erNames[2]:'NOx'}, inplace=True)
    rates['UnitID'] = rates.index.astype(str)
    rates = pd.melt(rates, id_vars=['UnitID'], value_vars=['CO2', 'SO2', 'NOx'])
    # tuple keys of index and pollutant
    rates['Tuple'] = list(zip(rates.UnitID, rates.variable))
    rateDict = dict(zip(rates.Tuple, rates.value))
    return rateDict
    
# read NGCC LCOE data (used to estimate cost of gas infrastructure)
def loadNatGasLCOE(countyList, scenario):
    os.chdir(os.getcwd() + '/Data inputs')
    LCOEs = pd.read_csv('UT_LCOE_data_' + scenario + '_gas_cost.csv', parse_dates=True)
    os.chdir('..') 
   
    LCOEs.index = LCOEs['FIPS'].map(str)
    LCOEs = LCOEs[LCOEs['FIPS'].isin(countyList)]
    LCOEs = LCOEs.reindex(countyList)
    
    if not LCOEs.isnull().values.any():
        LCOEdict = dict(zip(LCOEs.index, LCOEs.Cost))
        return LCOEdict
    else:
        print("Error reading gas LCOE values.")
        
# read renewable LCOE data (complements gas approach)
def loadRenewableLCOE(countyList):
    os.chdir(os.getcwd() + '/Data inputs')
    LCOEsolar = pd.read_csv('LCOE_data_v1.4.0_solar.csv', parse_dates=True)
    LCOEwind = pd.read_csv('LCOE_data_v1.4.0_wind.csv', parse_dates=True)
    os.chdir('..')

    LCOEsolar.index = LCOEsolar['FIPS'].map(str)
    LCOEsolar = LCOEsolar[LCOEsolar['FIPS'].isin(countyList)]
    LCOEsolar = LCOEsolar.reindex(countyList)
    
    LCOEwind.index = LCOEwind['FIPS'].map(str)
    LCOEwind = LCOEwind[LCOEwind['FIPS'].isin(countyList)]
    LCOEwind = LCOEwind.reindex(countyList)

    if not LCOEsolar.isnull().values.any() and not LCOEwind.isnull().values.any() :
        solarDict = dict(zip(LCOEsolar.index, LCOEsolar.Cost))
        windDict = dict(zip(LCOEwind.index, LCOEwind.Cost))
        return solarDict, windDict
    else:
        print("Error reading renewable LCOE values.")
            
# load renewable capacity factors by county
def loadRenewableCF(countyList):
    os.chdir(os.getcwd() + '/Data inputs/renewables')
    CFs = pd.read_csv('cfData.csv', parse_dates=True)
    os.chdir('../..') 
         
    CFs.index = CFs['fips'].map(str)
    CFs = CFs[CFs['fips'].isin(countyList)]
    CFs = CFs.reindex(countyList)
    
    # allow NA values for wind CF
    windDict = dict(zip(CFs.index, CFs.windCF))
    solarDict = dict(zip(CFs.index, CFs.solarCF))
    
    return windDict, solarDict
        
# load heat rate approximations for methane leakage        
def getGasHeatRates(CEMS):
    hr = CEMS['Heat rate'].copy()
    hrDict = dict(zip(hr.index.astype(str), hr.values))
    return hrDict
    
## format optimization results
# function to extract key information from a solved pyomo model
def formatUnitResults(model):
    
    # subset of original data
    labels = ['Facility ID (ORISPL)', 'Unit ID', 'FIPS Code', 'Fuel', 'Unit category', "Gross Load (MW-h)", "Heat rate"]
    labels.extend([pollutant+model.mdVersion+'_'+model.doseResponse for pollutant in ['NOx_', 'SO2_']])    
    reducedCEMS = model.CEMS.loc[:,model.CEMS.columns.intersection(labels)].copy()
    
    reducedCEMS.rename(columns={"Gross Load (MW-h)": "Original Gross Load (MW-h)"}, inplace=True)
    
    # extract optimization results
    # note: unit index corresponds to index in CEMS 
    unitIndex =  model.unit   
                
    genResults = [model.genUnits[unit].value * model.unitOn[unit] for unit in unitIndex]    
    varCostSavings = [model.varCostSavings[unit].expr() for unit in unitIndex]    
    resultsDF = pd.DataFrame({'Index': unitIndex, "Gross Load (MW-h)": genResults, "Variable cost savings ($)": varCostSavings}) 
    resultsDF.set_index('Index', inplace=True)
    resultsDF.index.name = None
    resultsDF.sort_index(inplace=True)
    
    # merge and compute new emissions and damages
    resultsDF = pd.merge(reducedCEMS, resultsDF, left_index=True, right_index=True)
    
    # separate df for gas (or new technology) results
    countyIndex = model.county
    genNewNGCC = [model.genNewNGCC[county].value for county in countyIndex] 
    newGasCapacity = [model.newGasCapacity[county].expr() for county in countyIndex]              # NGCC capacity in MW 

    kwGasRequired = [model.kWNeeded[county].expr() for county in countyIndex]
    plantsNeeded = [model.plantsNeeded[county].expr() for county in countyIndex]
    plantsBuilt = [model.newGasPlants[county].value for county in countyIndex]
    
    if model.LCOE is not None:
        capitalCosts = [model.gasCostLCOE[county].expr() for county in countyIndex] 
        varCosts = 0 
        #resultsDF.loc[:,'Variable cost savings ($)'] = 0
        fixedOMCosts = 0
    else:
        capitalCosts = [model.gasCapCostsAnnual[county].expr() for county in countyIndex]
        varCosts = [model.varCostGas[county].expr() for county in countyIndex]
        fixedOMCosts = [model.fixedOMGasTotal[county].expr() for county in countyIndex]
        
    resultsNGCC = pd.DataFrame({"FIPS Code": countyIndex, "Gross Load (MW-h)":genNewNGCC,
                                "Plants built": plantsBuilt, "New NGCC capacity (MW)":newGasCapacity, 
                                "Capital costs": capitalCosts, "Variable costs": varCosts, "OM costs": fixedOMCosts})
    
    # calculate new emissions for existing and new units
    pollutants = model.pollutant
    for p in pollutants:
        unitEmissions = [model.emissionsUnit[(unit, p)].expr() for unit in unitIndex]
        unitEmissions = pd.DataFrame({'Index': unitIndex, p+" (tons)": unitEmissions}) 
        unitEmissions.set_index('Index', inplace=True)
        unitEmissions.index.name = None
        resultsDF = pd.merge(resultsDF, unitEmissions, left_index=True, right_index=True)
        
        newEmissions = [model.emissionsNewGas[(county, p)].expr() for county in countyIndex]
        md = [model.marginalDamagesCounty[(county, p)] for county in countyIndex]
        
        
        newEmissions = pd.DataFrame({'FIPS Code': countyIndex,  p+"_"+model.mdVersion+"_"+model.doseResponse: md, 
                                     p+" (tons)": newEmissions}) 
        resultsNGCC = pd.merge(resultsNGCC, newEmissions, on='FIPS Code')
        
    # calculate damages
    resultsDF = calculateDamages(resultsDF, model.mdVersion, model.SCC, model.doseResponse, model.leakage, model.GWP)    
    resultsNGCC = calculateDamages(resultsNGCC, model.mdVersion, model.SCC, model.doseResponse, model.leakage, model.GWP, heatRateVal=model.heatRateNGCC)      
    
    resultsDF.drop("Heat rate", axis=1, inplace=True)
    
    # renewable data    
    solarGen = [model.genSolar[county].value for county in countyIndex]     
    solarBuilt = [model.newSolarCapacity[county].expr() for county in countyIndex]                  
 
    windGen = [model.genWind[county].value for county in countyIndex]     
    windBuilt = [model.newWindCapacity[county].expr() for county in countyIndex]   
    
    storageCosts = [model.storageCostsAnnual[county].expr() for county in countyIndex]   

    if model.LCOE is not None:
        solarCapital = [model.solarCostLCOE[county].expr() for county in countyIndex] 
        solarOM = 0
        windCapital = [model.windCostLCOE[county].expr() for county in countyIndex] 
        windOM = 0
    else:
        solarCapital = [model.solarCapCostsAnnual[county].expr() for county in countyIndex] 
        solarOM = [model.fixedOMSolar[county].expr() for county in countyIndex]
        windCapital = [model.windCapCostsAnnual[county].expr() for county in countyIndex] 
        windOM = [model.fixedOMWind[county].expr() for county in countyIndex]
    
    resultsRenew = pd.DataFrame({"FIPS Code": countyIndex, "Solar gen (MWh)": solarGen, 
                                 "Solar installed (MW)": solarBuilt, "Solar CAPEX": solarCapital, 
                                 "Solar OM": solarOM, "Wind gen (MWh)": windGen, 
                                 "Wind installed (MW)": windBuilt, "Wind CAPEX": windCapital, 
                                 "Wind OM": windOM, "Storage CAPEX": storageCosts}) 
    
    return resultsDF, resultsNGCC, resultsRenew