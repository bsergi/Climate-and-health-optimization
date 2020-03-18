# Load marginal damages 
# Author: Brian Sergi
# Created: July 17, 2018
# Latest version: March 14, 2020

## Notes

# loads marginal damage results from the RCMs
# Note: main model primarily uses the integrated RCM results, but is set up so as to allow use of AP3 or EASIUR results directly

## LIBRARIES
import pandas as pd
import os

## Integrated RCM results
# these results are primarily used in the model

def loadRCMs(filename):
    os.chdir(os.getcwd() + '/Data inputs')
    MDs = pd.read_csv(filename, parse_dates=True)   
    os.chdir('..') 
    return MDs
    
def mergeCEMSandMDs(CEMS, MDs):
    # merge by fips code
    CEMS = pd.merge(CEMS, MDs, how = 'left', left_on = 'FIPS Code', right_on="fips")
    return CEMS

## EASIUR 

def readEASIUR(filename):
    os.chdir(os.getcwd() + '/Data inputs')
    EASIUR = pd.read_csv(filename, parse_dates=True)   
    os.chdir('..') 
    
    # subset to key EASIUR values for now    
    labels = ["Longitude", "Latitude", "NOX Annual 300m", "SO2 Annual 300m"]
    EASIUR = EASIUR.loc[:,EASIUR.columns.intersection(labels)]
    
    EASIUR.rename(columns={'NOX Annual 300m': 'NOx MD-EASIUR ($/ton)',
                           'SO2 Annual 300m': 'SO2 MD-EASIUR ($/ton)'
                          }, inplace=True)
                   
    # EASIUR estimates in $2010 per metric ton, assuming 2017 for income and population 
    # convert from $2010 to $2016 and from metric to short tons
    CPI_2010_to_2016 = 1.09
    metric_to_short = 1.10231
    EASIUR['NOx MD-EASIUR ($/ton)'] = EASIUR['NOx MD-EASIUR ($/ton)'] * CPI_2010_to_2016 * metric_to_short
    EASIUR['SO2 MD-EASIUR ($/ton)'] = EASIUR['SO2 MD-EASIUR ($/ton)'] * CPI_2010_to_2016 * metric_to_short    
    return EASIUR

# save lat and long of units to CSV for calculating MDs    
def writeMDCoords(CEMS):
    CEMS = CEMS.drop_duplicates(subset = ['Facility Latitude', 'Facility Longitude'])
    CEMS.to_csv("Facility coordinates for EASIUR.csv", index=False, header=False,
                columns = ['Facility Longitude', 'Facility Latitude'])
    
def mergeCEMSandEASIUR(CEMS, EASIUR):    
    # merge by lat and long of facility
    CEMS = pd.merge(CEMS, EASIUR, how = 'outer', 
                    left_on = ['Facility Latitude', 'Facility Longitude'], 
                    right_on = ['Latitude', 'Longitude'])    
    return CEMS
        
## AP3

def readAP3(year, doseResponse):
    os.chdir(os.getcwd() + '/Data inputs')
    SO2_SR = pd.read_csv('AP3 SO2 S-R matrix ' + str(year) + ' ' + doseResponse + '.csv', parse_dates=True, index_col=0)   
    NOx_SR = pd.read_csv('AP3 NOx S-R matrix ' + str(year) + ' ' + doseResponse + '.csv', parse_dates=True, index_col=0)     
    os.chdir('..') 
    
    # AP3 estimates in $2014 per short ton, assuming 2014 for population, mortality rates, and emissions
    # convert from $2014 to $2017 
    CPI_2014_to_2017 = 1.04
    
    SO2_SR = SO2_SR * CPI_2014_to_2017
    NOx_SR = NOx_SR * CPI_2014_to_2017
    
    # sum by column (receptor, or location of damage) 
    # result is list of damage by row (source)
    SO2_MDs = pd.DataFrame(SO2_SR.sum(axis=1))
    NOx_MDs = pd.DataFrame(NOx_SR.sum(axis=1))
    
    # join MD summaries
    MDs = pd.merge(SO2_MDs, NOx_MDs, left_index = True, right_index=True)
    MDs.reset_index(inplace=True)

    MDs.rename(columns={'0_y': 'NOx_AP3_' + doseResponse,
                        '0_x': 'SO2_AP3_' + doseResponse,
                        'index':'FIPS Code'}, inplace=True)
                        
    # adjustment for Miami-Dade
    MDs['FIPS Code'].replace(12025, 12086, inplace=True)
    
    fips = SO2_SR.index.tolist()
    idx = fips.index(12025)
    fips[idx] = 12086
    SO2_SR.index, NOx_SR.index = fips, fips
    SO2_SR.columns, NOx_SR.columns = fips, fips
                          
    return (MDs, (SO2_SR, NOx_SR))
    
    
def mergeCEMSandAP3(CEMS, AP3):
    # merge by fips code
    CEMS = pd.merge(CEMS, AP3, how = 'left', on = 'FIPS Code')
    return CEMS