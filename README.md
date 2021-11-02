Code Required to reproduce results for Ambient Temperature and Risk of Urinary Tract Infection in California: a Time-Stratified Case-Crossover Study Using Electronic Health Records by Elser and Rowland et al. 

Notes: The patient data is HIPPA-protected and not publicly available; I have included code to generate results with simulated ('fake') data to allow researchers to follow the logic and the code and to facilitate code review. 

Code Sections: 
0: setting up the R environment 
1: captain script to prepare all data and run analysis 
a: process data 
	a_xxa: process real data 
	a_xxb: process fake data
b: generate descriptive plots (not in manuscript) 
c: fit main models 
d: fit sensitivity analyses 
e: fit exploratory analyses (not included in manuscript or repo) 
f: generate initial plots (not included in manuscript or repo) 
g: generate plots, tables, and figures for manuscript
