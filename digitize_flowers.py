#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 14 20:36:13 2021

@author: jacksonhawkins
"""

import os
import pandas as pd
import glob


#%%
# Read in .csv file & initialize the output dataframe
headers = ["species", "stage"]
for i in range(1903, 2013):
    headers.append(str(i))

os.chdir('/Users/jacksonhawkins/Documents/Fall_2021/ES401/csv_inputs') # Set working directory

files = glob.glob('/Users/jacksonhawkins/Documents/Fall_2021/ES401/csv_inputs/*.csv') # 

# file = 'FlowersC.csv'
used_letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P']
count = 0

for filename in files:
    letter = used_letters[count]
    df = pd.read_csv(filename, usecols = headers)

    finaldf = pd.DataFrame(columns = ["occurrenceRemarks", "reproductiveCondition", "year", "month", "day", "vernacularName", "scientificName"],)

#%% Do the transcription of data

    for row in range(len(df)):
        
        line = df.iloc[row] # This is the line from the dataframe/spreadsheet being transcribed
        clean_line = line.dropna(axis = 0)
        clean_datesonly = clean_line[2:]
        
        if row % 2 == 0:
            species_names = df.iloc[row:row+2]
            species = species_names[["species"]]
        else:
            species = species
        
    
        for col in range(len(clean_datesonly)):
            
            if str(clean_datesonly[[col]])[8:11] == 'Mar': # use if/else statement to identify month 
                month = 3
            elif str(clean_datesonly[[col]])[8:11] == 'Apr':
                month = 4
            elif str(clean_datesonly[[col]])[8:11] == 'May':
                month = 5
            elif str(clean_datesonly[[col]])[8:11] == 'Jun':
                month = 6
            elif str(clean_datesonly[[col]])[8:11] == 'Jul':
                month = 7
            elif str(clean_datesonly[[col]])[8:11] == 'Aug':
                month = 8
            elif str(clean_datesonly[[col]])[8:11] == 'Sep':
                month = 9
            elif str(clean_datesonly[[col]])[8:11] == 'Oct':
                month = 10
            elif str(clean_datesonly[[col]])[8:11] == 'Nov':
                month = 11
            else:
                month = "ERROR"
           
            
            if clean_line[1] == 'fruiting': # Use another if/else statement to create remarks
                remark = 'first known fruiting date'
            elif clean_line[1] == 'in_bloom':
                remark = 'first known flowering date'
            else:
                remark = 'ERROR'
                
            year = str(clean_datesonly[[col]])[:4] #slice out year
            day = str(clean_datesonly[col])[-2:] #slice out date
            day = day.strip() 
            
            prelim_vernac = str(species.iloc[0:1]) # Select the row with the vernacular name
            prelim_vernac = prelim_vernac.strip() # Remove whitespace from the front of the line
            vernac_name = prelim_vernac[11:] # Select only the vernac. name
            vernac_name = vernac_name.strip() # Again remove extra whitespace
            
            prelim_sciname = str(species.iloc[1:2]) # Select the row with the scientific name
            prelim_sciname = prelim_sciname.strip() # Remove whitespace from the front
            sci_name = prelim_sciname[12:] #select just the section with the scientific name
            sci_name = sci_name.strip() # Remove whitespace from the end
            
            
            new_row = {"occurrenceRemarks" : remark,
                       "reproductiveCondition" : clean_line[1], 
                       "year" : year,
                       "month" : month, 
                       "day" : int(day),
                       "vernacularName" : vernac_name,
                       "scientificName": sci_name }
            finaldf = finaldf.append(new_row, ignore_index = True) #add new dictionary to the final df output
    
    
#%% Export finaldf as a CSV
    
    finaldf.to_excel(r'/Users/jacksonhawkins/Documents/Fall_2021/ES401/transcribed_data/%s_Plants_DarwinCore.xlsx' %letter)
    print(letter)
    count = count + 1


