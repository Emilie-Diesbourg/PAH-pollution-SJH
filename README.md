# PAH-pollution-SJH
Examines the concentrations and compositions of sediment PAH contamination in the Saint John Harbour and assesses exposure and impact to the local nekton community.

Raw Data Water Quality, Nekton, Sediment PAH.xlsx : Raw data containing water quality (Temp, salinity, pH, orthophosphate, phosphate), nekton indices, and sediment PAH data between 2018-2022. 

SedimentPAHscript.R : Complete script for data analyses, calling all relevant data files in this repository. 

Study sites R script.R : Code to create map for sampling locations in the Saint John Harbour.

Site coordinates for R.csv : Used to create a map of sampling locations in the Saint John Harbour. 

PAH composition year and site.csv : Contains data on the concentration of each individual PAH and its percentage relative to the total PAH concentration at each site and over the entire sampling period (2018-2022). This data was used to see if there was a temporal difference in percent composition of each individual PAH analyte.

Fish_indices2018-2022.csv : This data was used for GLMs where average sediment PAH concentrations were modelled at 8 different sites against Shannon-Wiener Diversity index, Gini-Simpson Diversity index, Pielou eveness index, and abundance (total catch).

EROD activity for R.csv : Contains hepatic EROD (pmol/min/mg protein) activity values of 10 Atlantic silverside individuals/site. No outliers were removed from this dataset. 

EROD activity for R outlier removed.csv : Contains hepatic EROD (pmol/min/mg protein) activity values of 10 Atlantic silverside individuals/site, except at the Inner Harbour site which had 9 individuals, as one outlier was removed.

LSI.xlsx : Provides site name and Hepatosomatic (liver somatic) index as a percent with n=10 values/site.

K.xlsx : Provides site name and Fulton condition factor (weight/length^3 x 100) values with n=40 fish per site.

SedimentChemistry.xlsx : Has trace metal concentrations (31 metals) measured in sediment at four sites of the Saint John Harbour (n=6 samples/metal/site). Has concentrations of metals and their respective detection limit.

TotalOrganicCarbon2022.csv : Contains total organic carbon measurements and moisture content of sediment in percentage at 4 sites n=6/site. 

SpeciesDiversity.xlsx : Data used for diversity values including Shannon-Wiener, Gini-Simpson, Pielou Evenness, and Abundance.

DiversityCalculations.R : Code used for diversity calculations. 

sedimentPAHyears_fulldatawithoutliers.csv : Contains average PAH values at each site per year. This data was used for outlier testing, however outliers were not removed from the dataset since the values are extremely widespread. 

SedimentPAHsites.csv : Contains total PAH values at each site per year. This data was used for comparison of total PAH values across sites and years and used for Figure 2. 

PAHcomposition2018-2022.csv : Contains percent composition of each individual PAH relative to the total as percentage and concentration. This data was used to create Supplementary Figure 3 which illustrates differences in percent composition of each PAH over the sampling years. 

