# CEMS and eGrid data cleaning and processing 
# Author: Brian Sergi
# Created: July 17, 2018
# Latest version: March 14, 2020

## Notes

# loads plant and emissions data from EPA continuous emissions monitoring system data

## LIBRARIES

import pandas as pd
import numpy as np
import re
import os 

from loadMD import *
from datetime import timedelta

## Build full CEMS data
    
def buildCEMSData(yearCEMS, yearEgrid=2016, yearEASIUR=2017, yearAP3=2014):
    filenameCEMSfacility = "CEMS facility data " + str(yearCEMS) + ".csv"      
    filenameCEMSemissions = "CEMS emissions " + str(yearCEMS) + ".csv"         
    filenameEGRID = "egrid" + str(yearEgrid) + "_data.xlsx"          
    filenameEASIUR = "EASIUR grid cell MD " + str(yearEASIUR) + " 3-6-18.csv"     
    filenameNEEDS = "needs_v6_rev_9-14-2018.xlsx"
           
    # CEMS and eGrid data processing (see dataProcessing.py)
    CEMS = readCEMSfacility(filenameCEMSfacility)
    annualEmissions = readCEMSEmissions(filenameCEMSemissions)
    CEMS = mergeFacilityEmissions(CEMS, annualEmissions)    
    CEMS = fullFIPSCodes(CEMS)
    # simplified fuel types and unit categories
    fuelMapping(CEMS)    
    unitTypeProcessing(CEMS)
    # interpolate generation for missing units
    interpolateGen(CEMS)
    
    # calculate emissions rates
    CEMS = calcEmissionsRates(CEMS)     
    
    # Update grid cells values on EASIUR website using facility lat/lon  if necessary
    # writeMDCoords(CEMS)
    MDs = loadRCMs(filename="Final MDs combined.csv")
    CEMS = mergeCEMSandMDs(CEMS, MDs)
    
    '''old RCM import; 
    # EASIUR values converted into $2016  per short ton to match cost data and CEMS emissions
    EASIUR = readEASIUR(filenameEASIUR)
    CEMS = mergeCEMSandEASIUR(CEMS, EASIUR)
    
    '''
    # AP3 values converted into $2016 to match cost data
    # SRs is a tuple of full source-receptor matrices for (1) SO2 and (2) NOx
    AP3_ACS, AP3_SRs = readAP3(yearAP3, doseResponse="ACS")
    CEMS = mergeCEMSandAP3(CEMS, AP3_ACS)
    
    AP3_H6C, AP3_SRs_H6C = readAP3(yearAP3, doseResponse="H6C")
    CEMS = mergeCEMSandAP3(CEMS, AP3_H6C)
        
    # add operating costs from NREL ATB
    CEMS = addOperatingCostData(CEMS)
    
    # add capacity data from NEEDS
    # needs = readNEEDS(filenameNEEDS)
    
    # add heat rate data for natural gas (used for leakage calculations)
    CEMS = addHeatRate(CEMS) 
        
    # drop rows with missing information or infinite emissions rates
    CEMS.dropna(axis=0, subset=['Gross Load (MW-h)', 'SO2 emissions rate (tons/MWh)', 
                           'NOx emissions rate (tons/MWh)', 'CO2 emissions rate (tons/MWh)'], inplace=True)
                           
    CEMS = CEMS.replace([np.inf, -np.inf], np.nan).dropna(axis=0, subset=['SO2 emissions rate (tons/MWh)', 
                                                    'NOx emissions rate (tons/MWh)', 'CO2 emissions rate (tons/MWh)'])
                                                    
    # create unique ID
    CEMS['Code'] = CEMS['Facility ID (ORISPL)'].map(str) + "-" + CEMS['Unit ID'].map(str)
    CEMS.set_index('Code', inplace=True)
    
    return CEMS, AP3_SRs, AP3_SRs_H6C

## Load component data sets

# reads CEMS facility data at unit level 
def readCEMSfacility(filename):
    
    os.chdir(os.getcwd() + '/Data inputs')
    CEMSData = pd.read_csv(filename, parse_dates=False)                             
    os.chdir('..') 

    # remove leading/trailing whitespace from column names
    CEMSData.columns = [x.strip() for x in CEMSData.columns]   
    
    # subset to relevant columns
    labels = ['State', 'County', 'FIPS Code',
              'Facility Name', 'Facility ID (ORISPL)', 'Unit ID', 
              'Unit Type','Fuel Type (Primary)',
              'Fuel Type (Secondary)', 'Commercial Operation Date',
              'Operating Status', 'Max Hourly HI Rate (MMBtu/hr)',
              'SO2 Control(s)',  'NOx Control(s)',  'PM Control(s)',
              'Source Category', 'NERC Region', 'Year', 
              'Facility Latitude',  'Facility Longitude']
    CEMSData = CEMSData.loc[:,CEMSData.columns.intersection(labels)]
    
    # subset to electric utilities (exclude industrial boilers)
    CEMSData = CEMSData[CEMSData['Source Category'].isin(['Electric Utility',
                                                          'Cogeneration', 
                                                          'Small Power Producer'])]
    return CEMSData
    
# reads annual CEMS emissions data
def readCEMSEmissions(filename):    
    
    os.chdir(os.getcwd() + '/Data inputs')
    CEMSData = pd.read_csv(filename, parse_dates=False) 
    os.chdir('..') 

    # remove leading/trailing whitespace from column names
    CEMSData.columns = [x.strip() for x in CEMSData.columns] 
    
    labels = ['State', 'Facility Name', 'Facility ID (ORISPL)', 'Unit ID', 'Operating Time',
                'Gross Load (MW-h)', 'SO2 (tons)', 'NOx (tons)', 'CO2 (short tons)']
    CEMSData = CEMSData.loc[:,CEMSData.columns.intersection(labels)]
    
    # convert short tons to metric tons
    shortToMetric = 0.907185
    CEMSData['CO2 (short tons)'] = CEMSData['CO2 (short tons)'] * shortToMetric
    CEMSData.rename(columns={'CO2 (short tons)': 'CO2 (tons)'}, inplace=True)
    
    return CEMSData
        
def mergeFacilityEmissions(CEMS, emissions):
    # merge CEMS facility and emmissions data
    commonColumns = ['State', 'Facility Name',  'Facility ID (ORISPL)', 'Unit ID']
    CEMS = pd.merge(CEMS, emissions, how = 'outer', 
                    left_on = commonColumns, right_on = commonColumns)
                    
    # replace NAs for emissions with zeros
    CEMS['SO2 (tons)'].fillna(0, inplace=True)
    CEMS['CO2 (tons)'].fillna(0, inplace=True)
    CEMS['NOx (tons)'].fillna(0, inplace=True)
                    
    return CEMS
    
def readEGrid(filenameEGRID, sheetName):

    eGrid = pd.read_excel(pd.ExcelFile(filenameEGRID), sheetName)
        
    # drop second row with abbreviations
    eGrid = eGrid.drop(eGrid.index[[0]])
    
    # subset to relevant columns
    labels = ['Plant state abbreviation', 'Plant name', 
              'DOE/EIA ORIS plant or facility code', 
              'Generator ID',  'Generator status', 
              'Number of associated boilers', 'Generator prime mover type',
              'Generator primary fuel', 'Generator nameplate capacity (MW)',
              'Generator capacity factor', 'Generator annual net generation (MWh)']
    eGridSub = eGrid.loc[:,eGrid.columns.intersection(labels)]
    
    return(eGridSub)
    
    
def readNEEDS(filenameNEEDS):
    os.chdir(os.getcwd() + '/Data inputs')
    needs = pd.read_excel(pd.ExcelFile(filenameNEEDS), sheet_name="NEEDS v6_Active")
    os.chdir('..') 
    
    # subset to relevant columns
    labels = ['Plant state abbreviation', 'Plant name', 
              'DOE/EIA ORIS plant or facility code', 
              'Generator ID',  'Generator status', 
              'Number of associated boilers', 'Generator prime mover type',
              'Generator primary fuel', 'Generator nameplate capacity (MW)',
              'Generator capacity factor', 'Generator annual net generation (MWh)']
    eGridSub = eGrid.loc[:,eGrid.columns.intersection(labels)]
    
    return needs
    
def fullFIPSCodes(CEMS):
    CEMS = CEMS[pd.notnull(CEMS['FIPS Code'])]
    
    # read list of state abbreviations, county and state fips codes   
    
    os.chdir(os.getcwd() + '/Data inputs')
    censusFIPS = pd.read_csv("Census FIPS codes.txt", header=None, dtype='str') 
    os.chdir('..') 
    
    censusFIPS.rename(columns={0: 'State', 1:'State FIPS', 2:'County FIPS'}, inplace=True)
    
    # combine state and county into full fips 
    censusFIPS['Full fips'] = pd.to_numeric(censusFIPS['State FIPS'] + 
                                            censusFIPS['County FIPS'])
                                            
    censusFIPS['County FIPS'] = pd.to_numeric(censusFIPS['County FIPS'])
    
    CEMS = pd.merge(CEMS, censusFIPS[['State', 'County FIPS', 'Full fips']], how = 'left', 
                                        left_on = ['State', 'FIPS Code'],
                                        right_on = ['State', 'County FIPS'])
    
    # replace original fips code         
    CEMS['FIPS Code'] = CEMS['Full fips']                           
    CEMS = CEMS.drop(columns=['County FIPS', 'Full fips'])
    
    return CEMS
                    
def sortFIPS(CEMS):
    sortVars = ['FIPS Code', 'Facility Name', 'Unit ID']
    return CEMS.sort_values(by=sortVars).reset_index(drop=True)

## Data processing 

def fuelMapping(CEMS):
    
    fuelKey = {'Pipeline Natural Gas': 'Natural gas', 
               'Coal': 'Coal', 
               'Natural Gas': 'Natural gas', 
               'Other Gas': 'Other',
               'Wood': 'Wood', 
               'Coal, Natural Gas': 'Other', 
               'Diesel Oil': 'Oil', 
               'Residual Oil': 'Oil',
               'Process Gas': 'Other', 
               'Petroleum Coke': 'Other', 
               'Other Oil': 'Oil', 
               'Other Solid Fuel': 'Other',
               'Other Gas, Pipeline Natural Gas': 'Natural gas',
               'Natural Gas, Pipeline Natural Gas': 'Natural gas', 
               'Coal, Pipeline Natural Gas': 'Other',
               'Coal Refuse': 'Coal'}
    
    #CEMS['Fuel'] = CEMS['Fuel Type (Primary)'].map(fuelKey)
    CEMS['Fuel'] = CEMS['Fuel Type (Primary)'].replace(fuelKey)
    
    # separate treatment for missing values
    CEMS['Fuel'].fillna('Missing', inplace=True)
    
    
def displayDataFrame(data, rows):
    # get current settings
    currentRows = pd.get_option("display.max_rows")
    
    # number of rows to view
    pd.options.display.max_rows=rows
    print(CEMS[["Fuel Type (Primary)", "Fuel"]])
   
    # revert to original settings
    pd.options.display.max_rows=currentRows
    
def dateToYear(data):
    # convert to date object
    data.loc[:, 'Commercial Operation Date'] = pd.to_datetime(data.loc[:, 'Commercial Operation Date'].copy()) #format='%m/%d/%Y'

    # process any dates past 2040 (incorrectly read as wrong century)
    indices = data['Commercial Operation Date'].apply(lambda x: x.year) > 2040
    data.loc[indices, 'Commercial Operation Date'] = data['Commercial Operation Date']\
                                                        [indices].apply(lambda x: x - 100*timedelta(days=365))
    # extract operation year
    data.loc[:, 'Commercial Operation Year'] = data['Commercial Operation Date'].copy().map(lambda x: x.year)
    
def unitTypeProcessing(data):
    # summary of number of unit types by fuel
    #data.groupby(["Unit Type", "Fuel"]).count()['Unit ID']
        
    # use regular expression to remove dates (in parentheses)
    data.loc[:, 'Unit Type'] = data['Unit Type'].str.replace(r" \(.*\)", "")
    
    # map boiler types
    unitKey = {'Tangentially-fired': 'Boiler', 
               'Cell burner boiler': 'Boiler', 
               'Dry bottom wall-fired boiler': 'Boiler', 
               'Dry bottom turbo-fired boiler': 'Boiler',
               'Stoker': 'Boiler', 
               'Dry bottom vertically-fired boiler': 'Boiler', 
               'Circulating fluidized bed boiler': 'Boiler', 
               'Cyclone boiler': 'Boiler',
               'Bubbling fluidized bed boiler': 'Boiler', 
               'Wet bottom wall-fired boiler': 'Boiler',
               'Wet bottom turbo-fired boiler': 'Boiler',
               'Other boiler': 'Boiler',
               'Integrated gasification combined cycle': 'IGCC'}
    
    data.loc[:, 'Unit category'] = data['Unit Type'].replace(unitKey)
    
    
def emissionsByUnitType(data):
    
    # data cleaning on operation year and unit types
    dateToYear(data)
    # unitTypeProcessing(data)
    data = data.copy()

    # subset to plants that are above some cutoff in generation 
    cutoff = 100 * 24 * 7                   # 100 MW * 24 hours * 7 days = 16800 MWh
    
    data = data[data['Gross Load (MW-h)'] > cutoff]
    # average by year and unit category
    rates = data.groupby(["Unit category", "Commercial Operation Year"]).agg({'Unit ID': 'count',
                                                                              'SO2 emissions rate (tons/MWh)': 'mean', 
                                                                              'NOx emissions rate (tons/MWh)': 'mean',
                                                                              'CO2 emissions rate (tons/MWh)': 'mean'}).reset_index()
                                                                              
    rates.rename(columns={'Unit ID': 'Count'}, inplace=True)
         
    return rates
    
def countyChanges(baseline, units, vars=["State", "FIPS Code"]):
    
    change = units.set_index(vars).subtract(baseline.set_index(vars), fill_value=0)
    
    return change.reset_index()
    
def percentChange(baseCol, changeCol):
    return changeCol / baseCol * 1000 // 1 /10
    
def calcEmissionsRates(CEMS):
    CEMS['SO2 emissions rate (tons/MWh)'] = CEMS['SO2 (tons)'] / CEMS['Gross Load (MW-h)']
    CEMS['NOx emissions rate (tons/MWh)'] = CEMS['NOx (tons)'] / CEMS['Gross Load (MW-h)']
    CEMS['CO2 emissions rate (tons/MWh)'] = CEMS['CO2 (tons)'] / CEMS['Gross Load (MW-h)']
    
    return CEMS
    
def combinedCycleAverage(rates, cutoffStart, cutoffEnd):
    
    combinedCycle = rates['Unit category'] == "Combined cycle"
    yearRange = (rates['Commercial Operation Year'] >= cutoffStart) & (rates['Commercial Operation Year'] <= cutoffEnd)
    ratesSub = rates.loc[combinedCycle & yearRange,:].copy()
    erNames = ['CO2 emissions rate (tons/MWh)', 'SO2 emissions rate (tons/MWh)', 'NOx emissions rate (tons/MWh)']
    ratesSub.rename(columns={erNames[0]: 'CO2', erNames[1]: 'SO2', erNames[2]:'NOx'}, inplace=True)
    
    
    melted = pd.melt(ratesSub, id_vars=['Count'], value_vars=['CO2', 'SO2', 'NOx'])
    
    melted.loc[:, 'numerator'] =  melted.loc[:, 'Count'] *  melted.loc[:, 'value']
    sums = melted.groupby('variable').agg({'numerator': 'sum', 'Count': 'sum'})
        
    sums = sums.loc[:, 'numerator'] / sums.loc[:,'Count']
    
    rateDict = dict(zip(sums.index, sums.values))

    return rateDict

# add operating costs from 2018 NREL ATB   
# costs taken from https://atb.nrel.gov/electricity/2018/summary.html 
# costs for 'other category' set to CT gas cost; oil fuel costs separate estimate + O&M costs from gas
# Oil: 10811 Btu/kWh * 10 $/MMBtu * 1/1,000,000 MMBtu/Btu  * 1000 kWh / 1 MWh = 108 $/MWh 
def addOperatingCostData(CEMS):
    
    # Data from NREL 2018 ATB
    costsByFuel = pd.DataFrame({'Fuel': ['Natural gas', 'Coal', 'Wood', 'Oil', 'Other'],
                                #'Capacity factor': [0.07, 0.55, 0.52, 0.07, 0.07],
                                'Fuel costs ($/MWh)': [28, 19, 39, 100, 28], 
                                'Var O&M ($/MWh)': [7, 5, 5, 7, 7],
                                'Fixed O&M ($/kW-year)': [12, 33, 53, 12, 12]})
                   
    CEMS.loc[CEMS['Fuel'] == 'Missing', 'Fuel'] = 'Other'
    
    CEMS = pd.merge(CEMS, costsByFuel, how='left', on = 'Fuel')
        
    # overwrite values for combined cycle 
    # CEMS.loc[CEMS['Unit category'] == "Combined cycle", 'Capacity factor'] = 0.56
    CEMS.loc[CEMS['Unit category'] == "Combined cycle", 'Fuel costs ($/MWh)'] = 19
    CEMS.loc[CEMS['Unit category'] == "Combined cycle", 'Var O&M ($/MWh)'] = 3
    CEMS.loc[CEMS['Unit category'] == "Combined cycle", 'Fixed O&M ($/kW-year)'] = 10
    
    # overwrite IGCC costs
    CEMS.loc[CEMS['Unit category'] == "IGCC", 'Fuel costs ($/MWh)'] = 19
    CEMS.loc[CEMS['Unit category'] == "IGCC", 'Var O&M ($/MWh)'] = 8
    CEMS.loc[CEMS['Unit category'] == "IGCC", 'Fixed O&M ($/kW-year)'] = 54
    
    return CEMS
    
#  regression results for interporlation done separately (see model documentation / accompanying manuscript)
def interpolateGen(CEMS):
    regression = pd.DataFrame({"Fuel": ["Coal", "Natural gas", "Oil", "Other", "Wood"],
                              "Coefficient": [1.090, 1.774, 1.779, 1.044, 7.868e-01]})
    interceptVal = -1.325e+04
    
    regResults = pd.merge(CEMS[['Gross Load (MW-h)', "Unit category", "Fuel", "CO2 (tons)"]], regression, on="Fuel", how='left')
    regResults.loc[regResults['Unit category'] == "Combined cycle", "Coefficient"] = 2.514
    
    regResults['predicted Gross Load (MW-h)'] = interceptVal + regResults['CO2 (tons)']*regResults['Coefficient']
    regResults.loc[regResults['predicted Gross Load (MW-h)'] < 0 , 'predicted Gross Load (MW-h)'] = np.nan
    
    replaceGen = regResults.loc[regResults['Gross Load (MW-h)'].isnull(), "predicted Gross Load (MW-h)"]
        
    CEMS.loc[CEMS['Gross Load (MW-h)'].isnull(), "Gross Load (MW-h)"] = replaceGen
    
# add heat rate data for natural gas (used to estimate natural gas use for leakage rates)    
def addHeatRate(CEMS):
    fuels = pd.DataFrame({"Fuel": ["Coal", "Natural gas", "Oil", "Other", "Wood"],
                          "Heat rate": [0, 9.92, 0, 0, 0]}) # value taken from NREL ATB (mmBTU per MWh)
                          
    CEMS = pd.merge(CEMS, fuels, how='left', on = 'Fuel')
                          
    # overwrite values for combined cycle 
    CEMS.loc[CEMS['Unit category'] == "Combined cycle", 'Heat rate'] = 6.46  # value taken from NREL ATB (mmBTU per MWh)

    return CEMS      
    
                                            