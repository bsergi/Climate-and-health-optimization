# Build optimization model 
# Author: Brian Sergi
# Created: July 17, 2018
# Latest version: March 14, 2020

## Notes

# Functions to build and run a pyomo optimization model. Called by functions in __main__.py

## Import libraries

import pandas as pd
import numpy as np

from loadCEMS import *
from calcDamages import * 
from costSummary import * 
from functionDefs import *

from pyomo.environ import *
from pyomo.opt import SolverStatus, TerminationCondition

## build optimization model ##

# creates concrete pyomo model based on inputs. see pyomo documentation for details
def createOptModel(CEMS, MD, SCC, dose, percentTargets, MILP=False, budget=None, gasPrice=19, LCOE=None, leakage=0.03, GWP=28):
    
    model = ConcreteModel(name="(CEMS Model)")
    
    # save model inputs
    model.SCC = SCC             # social cost of carbon for co2 damages
    model.mdVersion = MD        # RCM (air quality model) used for analysis
    model.doseResponse = dose   # concentration-response function
    model.CEMS = CEMS           # initial generating units w/ emissions and generation levels
    model.budget = budget       # budget constraint (seldom used)
    model.LCOE = LCOE           # option to run in LCOE mode (seldom used)
    model.leakage = leakage     # methane leakage rate
    model.GWP = GWP             # global warming potential for converting CH4 to CO2 equivalent
    
    # weights for costs and damages
    weights = {"climate": 1, "health": 1, "cost": 1}
    weightCategories = Set(initialize=["climate", "health", "cost"])
    model.weights = Param(weightCategories, initialize=weights, mutable=True)
        
    # cost values taken from NREL 2018 ATB (in $2016)
    model.varCostNGCC = 3                           # $ per MWh (var O&M)               
    model.fuelCostNGCC = gasPrice                   # default gas price is $19 per MWh (high scenario uses $63 per MWh) from NREL ATB
    model.capexNGCC = 1060                          # $ per kW
    model.fixedOMGas = 10                           # $ per kW per year
    model.capFactorNGCC = 0.56
    model.gasCapacity = 150                         # min. size of new natural gas unit (MW) (based on median gas unit size from NEEDs data)
    model.heatRateNGCC = 6.46                       # NGCC heat rate data taken from NREL ATB
    
    model.capexWind = 1640                          # Assumed to be land-based (ATB range: 1529 to 1751)
    model.fixedOMWindConst = 51                     # $ per kW per year
    #maxWindCapacity = 500                          # option to control max wind capacity at any site (in MW)
    
    model.capexSolar = 1780                         # Assumed to be utility-scale photovoltaic 
    model.fixedOMSolarConst = 14                    # $ per kW per year
        
    model.fixedStorageCosts = 1500                  # $ per kW installed for 60 MW, 4-hr Li ion storage (based on NREL 2018 report)
    model.storageRatio = 0.6                        # for every 100 MW, installed 60 MW of 4-hr storage
    
    model.lifetime = 20
    model.discount = 0.07
        
    # SETS ####################################################################################################################################################

    unit = list(CEMS.index.map(str))                                # use data frame index as unit IDs
    coalUnits = list(CEMS.loc[CEMS['Fuel Type (Primary)'] == "Coal", :].index) # list of units that generate from coal   
     
    county = list(CEMS['FIPS Code'].map(str).unique())              # FIPS code for county index
    pollutant = ["CO2", "SO2", "NOx"] 
    fipsMapping = getFipsMapping(CEMS)
    
    model.unit = unit                                               # generating units
    model.coalUnits = coalUnits                                     # coal units
    model.county = county                                           # counties
    model.pollutant = pollutant                                     # pollutant (CO2, SO2, NOx)
    model.fipsMapping = fipsMapping                                 # mapping of unit to county
    
    # VARIABLES ####################################################################################################################################################
    
    # toggle between mixed integer (used to control unit commitment for existing units)
    # also sets lower bound for generation when appropriate
    if MILP:
        model.unitOn = Var(unit, initialize=1, within=Boolean)                           # binary variable for whether unit is operating   
        lowerBound=0.1   
    else:
        model.unitOn = Param(unit, initialize=1)
        lowerBound=0
              
    model.genNewNGCC = Var(county, initialize=0, within=NonNegativeReals)                # var for county-level NGCC generation (MWh) 
    model.genWind = Var(county, initialize=0, within=NonNegativeReals)                   # vars for new wind and solar generation (MWh)
    model.genSolar = Var(county, initialize=0, within=NonNegativeReals)
    
    currentGen = getCurrentGeneration(CEMS)                 # returns current unit level generation (MWh) 
    def genBounds(model, u):                                # unit generating bounds with MIP: 0.1 MW to 2017 generation total; otherwise 0 to 2017 total
        return (lowerBound, currentGen[u])                     
    model.genUnits = Var(unit, initialize=currentGen, within=NonNegativeReals, bounds=genBounds)              # var for existing unit generation (MWh)
    
    model.newGasPlants = Var(county, initialize=0, within=NonNegativeIntegers)                      # var for number of gas plants (assumed to be "lumpy" investment
    
    # PARAMETERS ####################################################################################################################################################
   
    model.genUnitsOriginal = Param(unit, initialize=currentGen)  # initiliaze generation by current unit
   
    windCF, solarCF = loadRenewableCF(county)       # county-specific capacity factor  for wind and solar                                   
    model.capFactorWind = Param(county, initialize=windCF, within=NonNegativeReals)      
    model.capFactorSolar = Param(county, initialize=solarCF, within=NonNegativeReals)      
    
    # new NGCC parameters
    newGasRates = {'CO2': 0.4, 'NOx': 5.06e-05, 'SO2': 2.38e-06}                             # gen. weighted average of combined cycle plants from 2000-2017
    model.emissionRatesNGCC = Param(pollutant, initialize=newGasRates, within=NonNegativeReals)            # emissions rates for NGCC by pollutant (ton/MWh)
    
    MDvals = formatMarginalDamages(CEMS, MD, dose, SCC)                                   # marginal damages by unit ($/ton)
    MDvalsCounty = formatMarginalDamages(CEMS, MD, dose, SCC, county=True)                # marginal damages by county for NGCC ($/ton)
    model.marginalDamages = Param(unit, pollutant, initialize=MDvals, within=NonNegativeReals)  
    model.marginalDamagesCounty = Param(county, pollutant, initialize=MDvalsCounty, within=NonNegativeReals)  
    
    ERvals = formatEmissionsRates(CEMS)                                        # emissions rates by unit (ton/MWh)
    model.emissionRates = Param(unit, pollutant, initialize=ERvals, within=NonNegativeReals)      
                                        
    countyGen = getCountyGeneration(CEMS)                                      
    model.countyGenOriginal = Param(county, initialize=countyGen)              # original county level generation total (MWh)
    
    def sumCountyGen(model, c):
        unitSub = fipsMapping[c]                                               # list of units in county to sum
        return sum(model.genUnits[u]*model.unitOn[u] for u in unitSub) + model.genNewNGCC[c] + model.genWind[c] + model.genSolar[c]
    model.countyGen = Expression(county, rule=sumCountyGen)                 # expression for summing county generation totals (MWh)
    
    model.percentTargets = Param(pollutant, initialize=percentTargets, mutable=True)   # target for reduction (%)
    targets = getTargets(CEMS, percentTargets)
    model.targets = Param(pollutant, initialize=targets, mutable=True)                 # target for reduction (tons) 
    
    varCosts = getUnitVarCosts(CEMS)
    model.varCosts = Param(unit, initialize=varCosts)                                  # variable costs
    
    if LCOE is not None:
        gasLCOE = loadNatGasLCOE(county, LCOE)                                        # LCOE values for natural gas
        model.gasLCOE = Param(county, initialize=gasLCOE) 
        solarLCOE, windLCOE = loadRenewableLCOE(county)    
        model.solarLCOE = Param(county, initialize=solarLCOE) 
        model.windLCOE = Param(county, initialize=windLCOE) 

    # functions to calculate CO2-equivalent emissions rate from methane leakage
    model.heatRatesLeakage = Param(unit, initialize=getGasHeatRates(CEMS), within=NonNegativeReals)  
    def calcLeakageRateNGCC(model, p):
        if p == "CO2":                      
            return leakageEquation(model.heatRateNGCC, model.leakage, model.GWP)
        else:                              
            return 0
    model.leakageRateNGCC = Expression(pollutant, rule=calcLeakageRateNGCC)
    
    def calcLeakageRateExistingUnits(model, u, p):
        if p == "CO2":                      
            return leakageEquation(model.heatRatesLeakage[u], model.leakage, model.GWP)
        else:                              
            return 0
    model.leakageRateExistingUnits = Expression(unit, pollutant, rule=calcLeakageRateExistingUnits)
    
    # CONSTRAINTS ####################################################################################################################################################
    
    # 1. Annual total generation same in each county (within small tolerance) 
    genGap = 1E-4
    def generationConstraint(model, c):
        return ((model.countyGenOriginal[c] - genGap), model.countyGen[c], (model.countyGenOriginal[c] + genGap))
    model.ConstrGen = Constraint(county, rule=generationConstraint)
        
    # 2. Total emissions less than target  
    def calcUnitEmissions(model, u, p):
        return model.genUnits[u]*model.unitOn[u]*model.emissionRates[(u,p)] 
    model.emissionsUnit = Expression(unit, pollutant, rule=calcUnitEmissions)
    
    def calcUnitLeakge(model, u, p):
        return model.genUnits[u]*model.unitOn[u]*model.leakageRateExistingUnits[(u,p)] 
    model.leakageUnit = Expression(unit, pollutant, rule=calcUnitLeakge)
    
    def calcNewGasEmissions(model, c, p):
        return model.genNewNGCC[c]*model.emissionRatesNGCC[p]     
    model.emissionsNewGas = Expression(county, pollutant, rule=calcNewGasEmissions)   
    
    def calcNGCCLeakge(model, c, p):
        return model.genNewNGCC[c]*model.leakageRateNGCC[p]     
    model.leakageNewGas = Expression(county, pollutant, rule=calcNGCCLeakge)   
    
    emissionsGap = 0.9999
    def calcTotalEmissions(model, p):
        return (sum(model.emissionsUnit[(u,p)] for u in unit) + sum(model.emissionsNewGas[(c,p)] for c in county))
    model.totalEmissions = Expression(pollutant, rule=calcTotalEmissions)
    
    def emissionsConstraint(model, p):
        if model.percentTargets[p] == 0:        # skip constraint if there is no target
            return Constraint.Skip
        else:
            if p == "CO2":                      # introduce lower bound on CO3 reductions to prevent over-mitigation
                return (model.targets[p]*emissionsGap, model.totalEmissions[p], model.targets[p])
            else:                               # for other pollutants, allow full range of reductions
                return (0, model.totalEmissions[p], model.targets[p])
    model.ConstrEmissions = Constraint(pollutant, rule=emissionsConstraint)
    
    # 3. Mitigation costs 
    KWhperMWh = 1000
    hours = 8760
    
    # Variable operating costs #
    
    def calcVarSavings(model, u):
        # calculate variable costs saved
        return (model.genUnitsOriginal[u]-model.genUnits[u]*model.unitOn[u])*model.varCosts[u] 
        
    def calcVarCosts(model, u):
        # calculates var costs from existing units
        return model.genUnits[u]*model.unitOn[u]*model.varCosts[u] 
    
    def calcVarGas(model, c):
        # calculate new gas costs (sum of var + fuel costs * gen
        return model.genNewNGCC[c]*(model.varCostNGCC  + model.fuelCostNGCC) 
        
    def calcGasAnnualCostsLCOE(model, c):
        return model.genNewNGCC[c]*model.gasLCOE[c]
    
    def calcSolarAnnualCostsLCOE(model, c):
        return model.genSolar[c]*model.solarLCOE[c]
        
    def calcWindAnnualCostsLCOE(model, c):
        return model.genWind[c]*model.windLCOE[c]        
    
    # Fixed O&M * capacity (convert to $ per MW) #
    def fixedOMGasRule(model, c):
        return model.fixedOMGas * model.newGasCapacity[c] * KWhperMWh
    
    def calcOMWind(model, c):
        # only fixed O&M (no fuel or variable O&M costs)
        return model.fixedOMWindConst * model.newWindCapacity[c] *KWhperMWh
        
    def calcOMSolar(model, c):
        # only fixed O&M (no fuel or variable O&M costs)
        return model.fixedOMSolarConst * model.newSolarCapacity[c] * KWhperMWh
        
    # Annual mitigation costs costs (new var + new fixed OM - old fuel savings) #
    def calcAnnuallMitCosts(model, c):
        unitSub = fipsMapping[c]                                               # list of units in county to sum
        return (model.varCostGas[c] + model.fixedOMGasTotal[c] + model.fixedOMWind[c] + \
                model.fixedOMSolar[c]) - sum(model.varCostSavings[u] for u in unitSub)

    # Annual costs (var + Fixed OM) #                
    def calcTotalAnnualCosts(model, c):
        unitSub = fipsMapping[c]                                               # list of units in county to sum
        return (model.varCostGas[c] + model.fixedOMGasTotal[c] + model.fixedOMWind[c] + \
                model.fixedOMSolar[c] + sum(model.varCostOld[u] for u in unitSub))
    
    # alternate approach using LCOE (renewables calculated in traditional way)
    def calcAnnualMitCostsLCOE(model, c):
        unitSub = fipsMapping[c]                                               # list of units in county to sum
        return  model.gasCostLCOE[c] + model.solarCostLCOE[c] + model.windCostLCOE[c] - \
                sum(model.varCostSavings[u] for u in unitSub)
                
    # alternate approach using LCOE (renewables calculated in traditional way)
    def calcTotalAnnualCostsLCOE(model, c):
        unitSub = fipsMapping[c]                                               # list of units in county to sum
        return  model.gasCostLCOE[c] + model.solarCostLCOE[c] + model.windCostLCOE[c] + \
                sum(model.varCostOld[u] for u in unitSub)
                            
    # total capacity calculation #
                
    # calculates total kW of capacity needed to meet generation requirements
    def calckWNeeded(model, c):
        return model.genNewNGCC[c] * KWhperMWh / (model.capFactorNGCC * hours)
    model.kWNeeded = Expression(county, rule=calckWNeeded)
        
    # calculates total number of plants need to supply kW (based on capacity of average gas plant)
    # Note: plant number here represented as float; adjusted to integer by new plant constraint
    def calcNumberOfPlants(model, c):
        return model.kWNeeded[c] / (model.gasCapacity * KWhperMWh)         # gas capacity = 150 MW
    model.plantsNeeded = Expression(county, rule=calcNumberOfPlants)
    
    # reformulates the number of plants as an integer variable
    def newPlantConstraint(model, c):
        return model.newGasPlants[c] >= model.plantsNeeded[c]
    model.ConstrNewPlants = Constraint(county, rule=newPlantConstraint)
    
    # re-estimates the total installed gas capacity (returns MW)
    def calcNewGasCapacity(model, c):                       
        return model.newGasPlants[c] * model.gasCapacity                   # gas capacity = 150 MW                                           
    model.newGasCapacity = Expression(county, rule=calcNewGasCapacity)     # capacity in MW after accounting for lumpy investments
    
    # calculate wind and solar capacities (ensure 0 capacity with no wind CF)
    # capacities given in MW
    def calcNewWindCapacity(model, c):  
        if model.capFactorWind[c] == 0:
            return 0
        else:
            return model.genWind[c] / (model.capFactorWind[c] * hours)                     
    model.newWindCapacity = Expression(county, rule=calcNewWindCapacity)     
    
    def calcNewSolarCapacity(model, c):                       
        return model.genSolar[c] / (model.capFactorSolar[c] * hours)                     
    model.newSolarCapacity = Expression(county, rule=calcNewSolarCapacity) 
        
    # total wind capacity constraint
    '''
    def windCapacityConstraint(model, c):
        return model.newWindCapacity[c] <= maxWindCapacity
    model.windCapacityMaxConstraint = Constraint(county, rule=windCapacityConstraint)
    '''        
    # capital costs #
    def capCostWind(model, c):
        return model.newWindCapacity[c] * KWhperMWh * model.capexWind
    model.windCapCosts = Expression(county, rule=capCostWind)
    
    def capCostSolar(model, c):
        return model.newSolarCapacity[c] * KWhperMWh * model.capexSolar
    model.solarCapCosts = Expression(county, rule=capCostSolar)

    def capCostGas(model, c):
        return  model.newGasCapacity[c] * KWhperMWh * model.capexNGCC
    model.gasCapCosts = Expression(county, rule=capCostGas)
    # calculate total costs (convert to kW to use $/kW estimates)
        
    def capitalCosts(model, c):
         return model.windCapCosts[c] + model.solarCapCosts[c] + \
                model.gasCapCosts[c] + model.storageCosts[c]
                # includes storage
                
    # annualize capital costs for output 
    def annualCapCostGas(model, c):
        return annualizeCost(model.gasCapCosts[c], model.lifetime, model.discount)
    model.gasCapCostsAnnual = Expression(county, rule=annualCapCostGas)
    
    def annualCapCostWind(model, c):
        return annualizeCost(model.windCapCosts[c], model.lifetime, model.discount)
    model.windCapCostsAnnual = Expression(county, rule=annualCapCostWind)
    
    def annualCapCostSolar(model, c):
        return annualizeCost(model.solarCapCosts[c], model.lifetime, model.discount)
    model.solarCapCostsAnnual = Expression(county, rule=annualCapCostSolar)
    
    # storage capital costs #
    def calcStorageCosts(model, c):
        storageNeeded = (model.newWindCapacity[c] + model.newSolarCapacity[c]) * model.storageRatio # in MW
        storageCost = storageNeeded * (model.fixedStorageCosts * KWhperMWh) # convert $ per kw to $ per MW
        return storageCost
    model.storageCosts = Expression(county, rule=calcStorageCosts)
    
    def annualCapCostStorage(model, c):
        return annualizeCost(model.storageCosts[c], model.lifetime, model.discount)
    model.storageCostsAnnual = Expression(county, rule=annualCapCostStorage)
                
    # instantiate variables #
    model.varCostSavings = Expression(unit, rule=calcVarSavings)
    model.varCostOld = Expression(unit, rule=calcVarCosts)
    model.varCostGas = Expression(county, rule=calcVarGas)
    
    model.fixedOMWind = Expression(county, rule=calcOMWind)
    model.fixedOMSolar = Expression(county, rule=calcOMSolar)
    model.fixedOMGasTotal = Expression(county, rule=fixedOMGasRule)
    
    model.totalAnnualCosts = Expression(county, rule=calcTotalAnnualCosts) 
    model.mitCosts = Expression(county, rule=calcAnnuallMitCosts)
    
    model.totalCAPEX = Expression(county, rule=capitalCosts)

    # calculate total additional expenditures
    def calcAdditionalExpendituresNPV(model):
        return sum(model.totalCAPEX[c] + calcNPV(model.totalAnnualCosts[c], model.lifetime, model.discount) for c in county)
    def calcAdditionalExpendituresAnnualized(model):
        return sum(annualizeCost(model.totalCAPEX[c], model.lifetime, model.discount) + model.totalAnnualCosts[c] for c in county)        
    def calcMitCostsAnnualized(model):
        return sum(annualizeCost(model.totalCAPEX[c], model.lifetime, model.discount) + model.mitCosts[c] for c in county)
                 
    def calcCostLCOEApproach(model):
        return sum(model.totalAnnualCostsLCOE[c] for c in county) + \
               annualizeCost(sum(model.storageCosts[c] for c in county), model.lifetime, model.discount)
               # includes storage (sum total costs and annualize)
    def calcMitCostLCOEApproach(model):
        return sum(model.totalAnnualMitCostsLCOE[c] for c in county) + \
               annualizeCost(sum(model.storageCosts[c] for c in county), model.lifetime, model.discount)
    
    if LCOE is not None:
        model.gasCostLCOE = Expression(county, rule=calcGasAnnualCostsLCOE)
        model.solarCostLCOE = Expression(county, rule=calcSolarAnnualCostsLCOE)
        model.windCostLCOE = Expression(county, rule=calcWindAnnualCostsLCOE)
        model.totalAnnualCostsLCOE = Expression(county, rule=calcTotalAnnualCostsLCOE)
        model.totalAnnualMitCostsLCOE = Expression(county, rule=calcAnnualMitCostsLCOE)
        
        model.totalCostsAnnual = Expression(rule=calcCostLCOEApproach)    
        model.totalMitCostsAnnual =  Expression(rule=calcMitCostLCOEApproach)
        
    else:
        model.totalCostsAnnual = Expression(rule=calcAdditionalExpendituresAnnualized)
        model.totalMitCostsAnnual = Expression(rule=calcMitCostsAnnualized)
         
    model.totalCostsNPV = Expression(rule=calcAdditionalExpendituresNPV)
    
    # option to set up budget constraint
    def meetBudgetConstraint(model):
        if model.budget == None:
            return Constraint.Skip
        return model.totalCostsNPV <= model.budget    
    model.ConstrBudget = Constraint(rule=meetBudgetConstraint)
    
    # constraint on wind for excluded locations (CF=0)
    '''
    def windConstraint(model, c):
        if model.capFactorWind[c] == 0:
            return model.windGen == 0
        else:
            return Constraint.Skip
    '''
    
    # OBJECTIVE ####################################################################################################################################################
    # minimize damage from health, climate, or both
    # Note: includes climate damages incurred from CH4 leakage
    def calcModelDamages(model, pollutantSub):
        return sum((model.emissionsUnit[(u,p)] + model.leakageUnit[(u,p)])*model.marginalDamages[(u, p)] for u in unit for p in pollutantSub) + \
               sum((model.emissionsNewGas[(c,p)] + model.leakageNewGas[(c,p)])*model.marginalDamagesCounty[(c, p)] for c in county for p in pollutantSub)
    
    def climateDamages(model):
        return calcModelDamages(model, ["CO2"])
    model.totalClimateDamages = Expression(rule=climateDamages)

    def healthDamages(model): 
        return calcModelDamages(model, ["SO2", "NOx"])
    model.totalHealthDamages = Expression(rule=healthDamages)

    def calcObjectiveFunction(model):
        return model.weights["cost"] * model.totalCostsAnnual + model.weights["climate"] * model.totalClimateDamages + \
               model.weights["health"] * model.totalHealthDamages
    model.objTotal = Objective(rule=calcObjectiveFunction)
    
    # toggle active constraints
    model.ConstrBudget.deactivate()
    # model.ConstrEmissions.deactivate()
    # model.objClimate.deactivate()
    # model.objHealth.deactivate()
    
    return model
    

## run optimization

def runOptimization(model, printFull, scenario, VSL, percentTargets, weights=None):
    
    # set weights by scenario
    if weights != None:
        model.weights["cost"] = weights['cost']
        model.weights["climate"] = weights['climate']
        model.weights["health"] = weights['health']
    else:    
        if scenario == "Health":            
            model.weights["cost"] = 1
            model.weights["climate"] = 0
            model.weights["health"] = 1
        elif scenario == "Climate":
            model.weights["cost"] = 1
            model.weights["climate"] = 1
            model.weights["health"] = 0
        else:
            model.weights["cost"] = 1
            model.weights["climate"] = 1
            model.weights["health"] = 1
    
    solver = SolverFactory('gurobi', executable='/Library/gurobi801/mac64/bin/gurobi.sh')
    #solver = SolverFactory('gurobi')
    
    results = solver.solve(model, warmstart=False)
    # results = solver.solve(model, tee=True, symbolic_solver_labels=True)                         # good for assessing solver status
            
    # model updates in terminal
    print("Settings: " + model.mdVersion + ", " + model.doseResponse + ", SCC=" + str(model.SCC) + ", VSL=" + str(VSL))
    
 
    print("Scenario: " + str(scenario) + ", CO2 reduction: " + str(percentTargets['CO2']))
    #print("Weights: climate = %d health = %d cost = %d" % (model.weights["climate"].value, model.weights["health"].value, model.weights["cost"].value))
    
    #print("\n\nPrint results\n--------------------------------------------------------------------------------------------------")
    #model.pprint()                                                                                            # good for debugging
    #print("\nEnd results\n------------------------------------------------------------------------------------------------------")

    # check optimization status
    if (results.solver.status == SolverStatus.ok) and (results.solver.termination_condition == TerminationCondition.optimal):
        
        # Do something when the solution in optimal and feasible
        print("Optimization complete: " + str(results.solver.termination_condition))
        
        # produce formatted output 
        resultsDF, resultsNGCC, resultsRenew = formatUnitResults(model)
        
        if printFull:
            if model.ConstrEmissions.active:
                print("CO2 target: %dm (%d%% reduction)" % (model.targets['CO2'].value/1E6, model.percentTargets['CO2'].value*100))
                
            print("Final emissions (million tons):    ", end="")
            print("CO2:%dm" % (model.totalEmissions['CO2'].expr()/1E6), end=" ")
            print("SO2:%0.1fm" % (model.totalEmissions['SO2'].expr()/1E6), end=" ")
            print("NOx:%0.1fm" % (model.totalEmissions['NOx'].expr()/1E6))
                
            percentCO2 = (model.CEMS['CO2 (tons)'].sum() - model.totalEmissions['CO2'].expr()) / model.CEMS['CO2 (tons)'].sum() * 100
            percentSO2 = (model.CEMS['SO2 (tons)'].sum() - model.totalEmissions['SO2'].expr()) / model.CEMS['SO2 (tons)'].sum() * 100
            percentNOx = (model.CEMS['NOx (tons)'].sum() - model.totalEmissions['NOx'].expr()) / model.CEMS['NOx (tons)'].sum() * 100
            
            print("Final reductions:   ", end="")
            print("CO2:%d%%" % (percentCO2), end="  ")
            print("SO2:%d%%" % (percentSO2), end="  ")
            print("NOx:%d%%" % (percentNOx))
        
        climate = resultsDF['Climate Damages ($)'].sum() + resultsNGCC['Climate Damages ($)'].sum()
        health = resultsDF['Health Damages ($)'].sum() + resultsNGCC['Health Damages ($)'].sum()
        total = climate + health
        #print("New total damages: $%d million (Climate: $%d million, Health $%d million)" % (total/1E6, climate/1E6, health/1E6))
        
        climateModel = model.totalClimateDamages.expr()
        healthModel = model.totalHealthDamages.expr()
        totalModel = climateModel + healthModel
        modelObjective = model.objTotal.expr()
        print("Total damages: $%d million (Climate: $%d million, Health $%d million)" % (totalModel/1E6, climateModel/1E6, healthModel/1E6))
        
        # esimated expenditures
        totalCosts = model.totalCostsNPV.expr()
        annualCosts = model.totalMitCostsAnnual.expr()
        
        if printFull:
            print("Budget: NPV $%d million" % (totalCosts/1E6), end=", ")
            print("Annualized $%d million" % (annualCosts/1E6))
            print("Model objective function: $%d million" % (modelObjective/1E6))
        print('------------------------------------------------------------------------------------------')
        
        # some model diagnostics
        # model.genNewNGCC.pprint()
        # model.kWNeeded.pprint()
        # model.newGasCapacity.pprint()
        
        # print(resultsNGCC)
        # resultsNGCC.to_csv("Test.csv")
        
        # format for output
        resultsDF['Scenario'] = scenario
        resultsNGCC['Scenario'] = scenario
        resultsRenew['Scenario'] = scenario

        resultsDF['RCM'] = model.mdVersion
        resultsNGCC['RCM'] = model.mdVersion
        resultsRenew['RCM'] = model.mdVersion
        
        resultsDF['Dose response'] = model.doseResponse
        resultsNGCC['Dose response'] = model.doseResponse
        resultsRenew['Dose response'] = model.doseResponse
        
        # total emissions
        CO2 = resultsDF['CO2 (tons)'].sum() + resultsNGCC['CO2 (tons)'].sum() 
        SO2 = resultsDF['SO2 (tons)'].sum() + resultsNGCC['SO2 (tons)'].sum()
        NOx = resultsDF['NOx (tons)'].sum() + resultsNGCC['NOx (tons)'].sum()
    
        damageSummary = dict({scenario: [climate, health, annualCosts, CO2, SO2, NOx, model.mdVersion, model.doseResponse]})
        damageSummary = pd.DataFrame.from_dict(damageSummary, orient='index', 
                                        columns=['Climate Damages ($)', 'Health Damages ($)', 'Annualized cost ($)', 
                                                'CO2 (tons)', 'SO2 (tons)','NOx (tons)', 'RCM', 'Dose response'])
                                                
        resultsDict = dict({"Plants": resultsDF, "Gas": resultsNGCC, "Summary": damageSummary, "Renewables": resultsRenew})
        return resultsDict
            
    elif (results.solver.termination_condition == TerminationCondition.infeasible):
        # Do something when model in infeasible
        print("Infeasible solution complete.")
    else:
        # Something else is wrong
        print("Solver Status: ",  results.solver.status, " ", str(results.solver.termination_condition))

    
## Build optimization model

def buildAndRunModel(CEMSoriginal, md, doseResponse, percentTargets, scenarios, SCC=40, VSL=9, gasPrice=19, leakage=0.03, GWP=28, printFull=False, MILP=False, LCOE=None):
    start = timeUpdate("Building optimization model...")
    
    # subset of dataframe (useful for testing purposes)
    # CEMS = CEMS.iloc[0:20,:].copy()
    CEMS = CEMSoriginal.copy()

    # adjust VSL as needed
    CEMS = adjustVSL(CEMS, VSL, md, doseResponse)

    # initial damages
    CEMS = calculateDamages(CEMS, md, SCC, doseResponse, leakage, GWP)
    
    # build model 
    model = createOptModel(CEMS, MD=md, dose=doseResponse, SCC=SCC, percentTargets=percentTargets, MILP=MILP, gasPrice=gasPrice, LCOE=LCOE, leakage=leakage, GWP=GWP)
   
    climate = CEMS['Climate Damages ($)'].sum()
    health = CEMS['Health Damages ($)'].sum()
    
    total = climate + health
    CO2 = CEMS['CO2 (tons)'].sum()
    SO2 = CEMS['SO2 (tons)'].sum()
    NOx = CEMS['NOx (tons)'].sum()
 
    originalDamages = dict({"Baseline": [climate, health, 0, CO2, SO2, NOx, model.mdVersion, model.doseResponse]})
    originalDamages = pd.DataFrame.from_dict(originalDamages, orient='index', 
                                    columns=['Climate Damages ($)', 'Health Damages ($)', 'Annualized cost ($)',
                                            'CO2 (tons)', 'SO2 (tons)','NOx (tons)', 'RCM', 'Dose response'])
    model.originalDamages = originalDamages
    timeUpdate("complete (%0.2f seconds).", start=start)
    
    # Solve model
    # print("RCM: " + model.mdVersion + ", dose response: " + model.doseResponse)
    print('------------------------------------------------------------------------------------------')
    
    print("Original emissions (million tons): ", end="")
    print("CO2:%dm" % (CEMS['CO2 (tons)'].sum()/1E6), end=" ")
    print("SO2:%0.1fm" % (CEMS['SO2 (tons)'].sum()/1E6), end=" ")
    print("NOx:%0.1fm" % (CEMS['NOx (tons)'].sum()/1E6))
    print("Original total damages: $%d million (Climate: $%d million, Health $%d million)" % \
                                                            (total/1E6, climate/1E6, health/1E6))
    print('------------------------------------------------------------------------------------------')
    
    start = timeUpdate("Solving model...")
    print("\n")
    # run optimization for main scenarios                                                        
    results = dict()
    costs = dict()
    for scenario in scenarios:
        results[scenario] = runOptimization(model, printFull, scenario, VSL, percentTargets)
    
    timeUpdate("...complete (%0.2f seconds).", start=start)
    return results, model
    
## optimization with simplified outputs used for sensitivity analysis
def runBasicOptimization(originalCEMS, SCC, VSL, md, doseResponse, percentTargets, scenario, leakage=0.03, GWP=28):
    
    CEMS = originalCEMS.copy()
    # build and solve model (will adjust to VSL and SCC specified)
    results, model = buildAndRunModel(CEMS, md, doseResponse, percentTargets, [scenario], SCC=SCC, VSL=VSL)
    
    # adjust marginal damages based on VSL for baseline (in million $)
    CEMS = adjustVSL(CEMS, VSL, md, doseResponse)
    CEMS = calculateDamages(CEMS, md, SCC, doseResponse, leakage, GWP)
    
    # calculate original damages using adjusted VSL
    climate = CEMS['Climate Damages ($)'].sum()
    health = CEMS['Health Damages ($)'].sum()
    
    # print("Original damages: Climate: $%d million, Health $%d million" % (climate/1E6, health/1E6))
    
    # use new damages to calculate benefits
    climateModel = model.totalClimateDamages.expr()
    healthModel = model.totalHealthDamages.expr()
    
    # calculate benefits    
    climateBenefits = climate - climateModel
    healthBenefits = health - healthModel
    
    # total generation from coal (MWh)
    coalUnits = model.coalUnits
    coalGen = sum([model.genUnits[unit].value for unit in coalUnits])
    
    # baseline coal generation
    coalGenBaseline = sum([model.genUnitsOriginal[unit] for unit in coalUnits])
    
    coalDiff = coalGenBaseline - coalGen
    
    # new capacity (MW)
    countyIndex = model.county
    gasCap = sum([model.newGasCapacity[c].expr() for c in countyIndex])
    windCap = sum([model.newWindCapacity[c].expr() for c in countyIndex])
    solarCap = sum([model.newSolarCapacity[c].expr() for c in countyIndex])
    
    # mitigation costs
    annualCosts = model.totalMitCostsAnnual.expr()

    resultSummary = dict({"Scenario": [model.mdVersion, model.doseResponse, VSL, SCC, percentTargets['CO2'], scenario,
                                       climateBenefits, healthBenefits, annualCosts, coalGen, coalDiff, gasCap, windCap, solarCap]})
                                       
    resultSummary = pd.DataFrame.from_dict(resultSummary, orient='index', 
                                    columns=['RCM', 'Dose response', 'VSL', 'SCC', 'CO2 target', "Scenario",
                                             'Climate benefits ($)', 'Health benefits ($)', 'Annual costs ($)', 
                                             'Coal generation (MWh)', 'Coal gen change (MWh)', 'New gas cap (MW)', 
                                             'New wind cap (MW)', 'New solar cap (MW)'])
                                    
    #print(md + " " + doseResponse + ", SCC=" + str(SCC) + ", VSL=" + str(VSL), end="...")    
    #print("climate: $%d million, health: $%d million)" % (climateModel/1E6, healthModel/1E6))   
    #print('------------------------------------------------------------------------------------------')
                                         
    return resultSummary