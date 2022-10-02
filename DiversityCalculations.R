####Load packages, set working directory#####

library(reshape2)
library(plyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(multcomp)
library(vegan)
theme_set(theme_classic())


#clear workspace and set working directory

diversity <- readxl::read_xlsx("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\SpeciesDiversity.xlsx")

###Overall richness per site
ddply(diversity,~Site,function(x) {
  data.frame(RICHNESS=sum(x[-1 ]>0 ))
})

###Overall abundance per site
ddply(diversity,~Site, function(x) {
  data.frame(ABUNDANCE=sum(x[-1 ]))
})

###Shannon-Weiner Diversity Index
ddply(diversity, ~Site, function(x){
  data.frame(SHANNON=diversity(x[-1 ], index = "shannon"))
})

###Simpson's Index
ddply(diversity, ~Site, function(x){
  data.frame(SIMPSON=diversity(x[-1 ], index = "simpson"))
})

###Species evenness
ddply(diversity, ~Site, function(x){
  data.frame(SHANNON=diversity(x[-1 ], index = "shannon")/log(sum(x[- 1]>0)))
})
