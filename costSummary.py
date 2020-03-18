# mitigation costs 
# Author: Brian Sergi
# Created: July 17, 2018
# Latest version: March 14, 2020

## Notes

# Supporting functions for cost and financial analysis

## LIBRARIES

import numpy as np
import pandas as pd
import os

## Budget target

def calcNPV(annualCosts, lifetime, dr):
    return annualCosts * ((1-(1+dr)**(-1*lifetime))/dr)
        
def annualizeCost(NPV, lifetime, dr):
    annuityFactor = (1-(1+dr)**(-1*lifetime))/dr
    return NPV/annuityFactor

def calcExpenditures(CEMS, costDict, lifetime, dr):
    # calculate variable costs saved
    currentVar = sum(CEMS['Unit reduction (MW-h)']*(CEMS['Fuel costs ($/MWh)']+CEMS['Var O&M ($/MWh)']))    
    # calculate new gas costs
    newGasGen = CEMS['New NGCC load (MW-h)'].sum()
    newGasVar = newGasGen*costDict['Var']
    # extrapoloate total neceessary NGCC capacity needed to supply annual generation
    KWhperMWh = 1000
    hours = 8760
    newGasCapex = newGasGen* KWhperMWh / (costDict['CF'] * hours) * costDict['Capex']
    
    return newGasCapex + calcNPV(newGasVar - currentVar, lifetime, dr)

## Mitigation costs

    
def calcTotalCosts(CEMS):
    # annualCosts = CEMS["New Variable costs ($)"].sum() - CEMS["Variable cost savings ($)"].sum()    
    # calculate present value of annual costs
    # PV = calcNPV(annualCosts, lifetime, dr)
    # return sume of capital costs and NPV of annual costs (or savings)
    return CEMS["New CAPEX ($)"].sum() #+ CEMS["NPV of change in var costs ($)"].sum()

def calcMitigationCosts(newCEMS, lifetime):
    costs  = calcTotalCosts(newCEMS)
    mitCosts = dict()
    for p in ["CO2", "SO2", "NOx"]:
        originalPollutantLevel = newCEMS[p + " (tons)-original"].sum()
        newLevel = newCEMS[p + " (tons)"].sum()
        # abatement over the lifetime of the NGCC facilities
        abatement = (originalPollutantLevel - newLevel) * lifetime
        mitCosts[p] = np.around(costs / abatement, 1)
    return mitCosts
    
def summarizeMitigationCosts(results, lifetime, saveDir):
    mitCosts = dict() 
       
    for scenario in results.keys():
        mitCosts[scenario] = calcMitigationCosts(results[scenario], lifetime)
        
    mitCosts = pd.DataFrame.from_dict(mitCosts, orient="index")
    
    os.chdir(os.getcwd() + '/' + saveDir)
    mitCosts.to_csv("Mitigation costs.csv", index=True)                                                          
    os.chdir('..')
    
    return mitCosts
    
