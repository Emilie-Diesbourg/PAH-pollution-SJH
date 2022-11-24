#### Packages/libraries ####
setwd("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\Sediment PAH calculations")

library(outliers)
library(DescTools)
library(FSA)
library(ggplot2)
library(stringr)
library(gridExtra)
library(ggpubr)
library(grid)
library(car)
library(broom)
library(viridis)
library(MASS)
library(reghelper)
library(betareg)
library(ggrepel)

#### Outlier testing ####

#Grubbs test is used to identify single outliers and assumes the data is normally distributed before performing
#the test. Used for relatively large sample sizes, N>30

#Dixon Q test is used to identify single outliers and assumes the data is normally distributed before performing
#the test. Used for small sample sizes, N=3-30. Should never use this test more than once in a dataset

#Rosner test is used to identify multiple outliers and assumes a normal distribution AFTER the test has been 
#performed. Used for relatively large sample sizes N>25. 

#So what can we use if we suspect 2 outliers but the data isn't normal and the sample size is small (n=13)...
#A boxplot?? Is there a way to quantify that? Not sure.

#Another method is Median Absolute Deviation (MAD) which does not assume normality and is arguably better since
#the outliers don't affect the median. Cite Leys et al., 2013 for that MAD paper. The downfall is that it
#is very robust and may identify 'too many' outliers when they are valid data points

PAHyears<-read.csv("sedimentPAHyears_fulldatawithoutliers.csv", header=T)
PAHyears$Year<-as.factor(PAHyears$Year)
str(PAHyears)

BS<-subset(PAHyears, Site=="Bayshore") #p=0.19, no outliers for dixon or boxplot
BB<-subset(PAHyears, Site=="Black Beach") #p < 2.2e-16, 0.17 is an outlier according to dixons test 
#and potentially more, 2-3 outliers according to boxplot
CB<-subset(PAHyears, Site=="Courtenay Bay") #p=0.0546, no outliers for dixon, no outliers for boxplot
DFT<-subset(PAHyears, Site=="Digby Ferry Terminal") #p=0.081, no outliers for dixon and boxplot
HCM<-subset(PAHyears, Site=="Hazen Creek Mouth") #p=< 2.2e-16, 1.71 is an outlier for dixon and boxplot
HCNS<-subset(PAHyears, Site=="Hazen Creek Nearshore") #p < 2.2e-16, 0.44 is an outlier for dixon and boxplot
IH<-subset(PAHyears, Site=="Inner Harbour") #p=0.04, 7.41 is an outlier for dixon and potentially more, 
#boxplot shows 3 outliers
LR<-subset(PAHyears, Site=="Little River") #p=0.015, 15.55 is an outlier for dixon and boxplot
MC<-subset(PAHyears, Site== "Marsh Creek") #p< 2.2e-16, 167.32 is an outlier for dixon and boxplot
#MC1<-read.csv("MC_outlier removed.csv", header=T) #p< 2.2e-16, 142.94 is an outlier for dixon and boxplot
MB<-subset(PAHyears, Site=="Mispec Beach") #p< 2.2e-16, 0.17 is an outlier for dixon and boxplot
SRB<-subset(PAHyears, Site=="Saints Rest Beach") #0.17 is an outlier for dixon and boxplot
SC<-subset(PAHyears, Site=="Spar Cove") #p < 2.2e-16, 84.26 is an outlier for dixon and boxplot
TCB<-subset(PAHyears, Site=="Tin Can Beach") #p < 2.2e-16 49.3 is an outlier

#Using Median Absolute Deviation method
#Bayshore:
median(BS$SedimentPAH)+(mad(BS$SedimentPAH))*2.5 #0.085. Means any value above this is an outlier
median(BS$SedimentPAH)-(mad(BS$SedimentPAH))*2.5 #0.085. This method detects 4 outliers in the data...

#Black Beach:
median(BB$SedimentPAH)+(mad(BB$SedimentPAH))*2.5 #0.085
median(BB$SedimentPAH)-(mad(BB$SedimentPAH))*2.5 #0.085 this method detects 3 outliers

#Courtenay Bay:
median(CB$SedimentPAH)+(mad(CB$SedimentPAH))*2.5 #5.579
median(CB$SedimentPAH)-(mad(CB$SedimentPAH))*2.5 #-1.019 this method detects 2 outliers

#Digby Ferry Terminal
median(DFT$SedimentPAH)+(mad(DFT$SedimentPAH))*2.5 #8.9117 detects 1 outlier
median(DFT$SedimentPAH)-(mad(DFT$SedimentPAH))*2.5 #-4.4317

#Hazen Creek Mouth
median(HCM$SedimentPAH)+(mad(HCM$SedimentPAH))*2.5 #0.085 detects 5 outliers
median(HCM$SedimentPAH)-(mad(HCM$SedimentPAH))*2.5 #0.085

#Hazen Creek Nearshore
median(HCNS$SedimentPAH)+(mad(HCNS$SedimentPAH))*2.5 #0.085
median(HCNS$SedimentPAH)-(mad(HCNS$SedimentPAH))*2.5 #0.085 detects 8 outliers

#Inner Harbour
median(IH$SedimentPAH)+(mad(IH$SedimentPAH))*2.5 #1.204
median(IH$SedimentPAH)-(mad(IH$SedimentPAH))*2.5 #-0.0936 detects 5 outliers

#Little River
median(LR$SedimentPAH)+(mad(LR$SedimentPAH))*2.5 #5.404
median(LR$SedimentPAH)-(mad(LR$SedimentPAH))*2.5 #-1.564 detects 4 outliers

#Mispec Beach
median(MB$SedimentPAH)+(mad(MB$SedimentPAH))*2.5 #0.085
median(MB$SedimentPAH)-(mad(MB$SedimentPAH))*2.5 #0.085 detects 2 outliers

#Marsh Creek
median(MC$SedimentPAH)+(mad(MC$SedimentPAH))*2.5 #61.4742
median(MC$SedimentPAH)-(mad(MC$SedimentPAH))*2.5 #-24.294 detects 2 outliers

#Spar Cove
median(SC$SedimentPAH)+(mad(SC$SedimentPAH))*2.5 #20.243
median(SC$SedimentPAH)-(mad(SC$SedimentPAH))*2.5 #-9.223 detects 2 outliers

#Saints Rest Beach
median(SRB$SedimentPAH)+(mad(SRB$SedimentPAH))*2.5 #0.085
median(SRB$SedimentPAH)-(mad(SRB$SedimentPAH))*2.5 #0.085 detects 2 outliers

#Tin Can Beach
median(TCB$SedimentPAH)+(mad(TCB$SedimentPAH))*2.5 #11.928
median(TCB$SedimentPAH)-(mad(TCB$SedimentPAH))*2.5 #-3.268 detects 1 outlier

#In total this method detected 41/182 (22.53%) of data points as outliers

#Using Boxplot method:
#Bayshore
ggplot(data=BS, aes(x=Site, y=SedimentPAH))+ #no outliers 
  geom_boxplot()

#Black Beach
ggplot(data=BB, aes(x=Site, y=SedimentPAH))+ #3 outliers 
  geom_boxplot()

#Courtenay Bay
ggplot(data=CB, aes(x=Site, y=SedimentPAH))+ #no outliers 
  geom_boxplot()

#Digby Ferry Terminal 
ggplot(data=DFT, aes(x=Site, y=SedimentPAH))+ #1 outlier
  geom_boxplot()

#Hazen Creek Mouth
ggplot(data=HCM, aes(x=Site, y=SedimentPAH))+ #1 outlier 
  geom_boxplot()

#Hazen Creek Nearshore
ggplot(data=HCNS, aes(x=Site, y=SedimentPAH))+ #1 outlier
  geom_boxplot()

#Inner Harbour
ggplot(data=IH, aes(x=Site, y=SedimentPAH))+ #3 outliers 
  geom_boxplot()

#Little River
ggplot(data=LR, aes(x=Site, y=SedimentPAH))+ #1 outlier
  geom_boxplot()

#Mispec Beach
ggplot(data=MB, aes(x=Site, y=SedimentPAH))+ #2 outliers 
  geom_boxplot()

#Marsh Creek
ggplot(data=MC, aes(x=Site, y=SedimentPAH))+ #2 outliers 
  geom_boxplot()

#Spar Cove
ggplot(data=SC, aes(x=Site, y=SedimentPAH))+ #1 outlier 
  geom_boxplot()

#Saints Rest Beach
ggplot(data=SRB, aes(x=Site, y=SedimentPAH))+ #2 outliers 
  geom_boxplot()

#Tin Can Beach
ggplot(data=TCB, aes(x=Site, y=SedimentPAH))+ #1 outlier
  geom_boxplot()

#Using the boxplot method, 18/182 (9.89%) outliers were identified.

#After my research on outliers and when to remove them I believe we should not remove them because they 
#could be legitimate observations at the site. Possibly they are not super representative of the site but
#I think removing them would be worse. 

#### Kruskal-Wallis test [PAH] across years and graph ####

PAHyears<-read.csv("sedimentPAHyears_fulldatawithoutliers.csv", header=T)
PAHyears$Year<-as.factor(PAHyears$Year)
str(PAHyears)

#ANOVA if the data was normal BUT it isn't!
model1<-aov(data=PAHyears, SedimentPAH~Year)
summary(model1)
plot(model1)#not normal


#Because the data is not normal, we use Kruskal Wallis test

kruskal.test(SedimentPAH~Year, data=PAHyears) #p-value is 0.8537. no diff across years with all sites included.

#Subset for each site and test if sig. diff. over time
kruskal.test(SedimentPAH~Year, data=BS) #p=0.2286
kruskal.test(SedimentPAH~Year, data=BB) #p=0.0188***
kruskal.test(SedimentPAH~Year, data=CB) #p=0.5112
kruskal.test(SedimentPAH~Year, data=DFT)#p=0.412
kruskal.test(SedimentPAH~Year, data=HCM) #p=0.4122
kruskal.test(SedimentPAH~Year, data=HCNS) #p=0.7282
kruskal.test(SedimentPAH~Year, data=IH) #p=0.3218
kruskal.test(SedimentPAH~Year, data=LR) #p=0.1077
kruskal.test(SedimentPAH~Year, data=MC) #p=0.3452
kruskal.test(SedimentPAH~Year, data=MB) #p=0.0858
kruskal.test(SedimentPAH~Year, data=SC) #p=0.3789
kruskal.test(SedimentPAH~Year, data=SRB) #p=0.1222
kruskal.test(SedimentPAH~Year, data=TCB) #p=0.09086

#The only site with significant difference over time is Black Beach. But do we care?...not sure

#Plot the data
ggplot(PAHyears, aes(x=Year, y=SedimentPAH))+
  geom_boxplot()+
  theme_classic()+
  ylab("Total sediment PAH \n concentration (mg/kg DW)")+
  stat_summary(fun=mean, size=0.3, color="blue")+
  geom_abline(intercept=0.17, slope=0, col="red", lty=2)+
 scale_y_log10()

#Now do the same analysis with outliers removed
PAH_no_outliers<-read.csv("fullPAHdata_WOoutliersMAD.csv", header=T)

Y2018<-subset(PAH_no_outliers, Year=="2018")
mean(Y2018$SedimentPAH) #2.76
sd(Y2018$SedimentPAH) #3.45

Y2019<-subset(PAH_no_outliers, Year=="2019")
mean(Y2019$SedimentPAH) #3.84
sd(Y2019$SedimentPAH) #7.85

Y2020<-subset(PAH_no_outliers, Year=="2020")
mean(Y2020$SedimentPAH) #2.99
sd(Y2020$SedimentPAH) #7.15

Y2021<-subset(PAH_no_outliers, Year=="2021")
mean(Y2021$SedimentPAH) #3.48
sd(Y2021$SedimentPAH) #6.95

Y2022<-subset(PAH_no_outliers, Year=="2022")
mean(Y2022$SedimentPAH) #2.60
sd(Y2022$SedimentPAH) #4.29

mean(PAH_no_outliers$SedimentPAH) #3.20
sd(PAH_no_outliers$SedimentPAH) #6.53

kruskal.test(SedimentPAH~Year, data=PAH_no_outliers) #0.8499 no diff across years when all sites included

BS1<-subset(PAH_no_outliers, Site=="Bayshore")
BB1<-subset(PAH_no_outliers, Site=="Black Beach")
CB1<-subset(PAH_no_outliers, Site=="Courtenay Bay")
DFT1<-subset(PAH_no_outliers, Site=="Digby Ferry Terminal")
HCM1<-subset(PAH_no_outliers, Site=="Hazen Creek Mouth")
HCNS1<-subset(PAH_no_outliers, Site=="Hazen Creek Nearshore")
IH1<-subset(PAH_no_outliers, Site=="Inner Harbour")
LR1<-subset(PAH_no_outliers, Site=="Little River")
MC1<-subset(PAH_no_outliers, Site=="Marsh Creek")
MB1<-subset(PAH_no_outliers, Site=="Mispec Beach")
SC1<-subset(PAH_no_outliers, Site=="Spar Cove")
SRB1<-subset(PAH_no_outliers, Site=="Saints Rest Beach")
TCB1<-subset(PAH_no_outliers, Site=="Tin Can Beach")

#Subset for each site and test if sig. diff. over time
kruskal.test(SedimentPAH~Year, data=BS1) #p=NA because all values are the same
kruskal.test(SedimentPAH~Year, data=BB1) #p=NA because all values are the same
kruskal.test(SedimentPAH~Year, data=CB1) #p=0.3756
kruskal.test(SedimentPAH~Year, data=DFT1)#p=0.6559
kruskal.test(SedimentPAH~Year, data=HCM1) #p=NA because all values are the same
kruskal.test(SedimentPAH~Year, data=HCNS1) #p=NA because all values are the same
kruskal.test(SedimentPAH~Year, data=IH1) #p=0.8492
kruskal.test(SedimentPAH~Year, data=LR1) #p=0.152
kruskal.test(SedimentPAH~Year, data=MC1) #p=0.5616
kruskal.test(SedimentPAH~Year, data=MB1) #p=NA because all values are the same
kruskal.test(SedimentPAH~Year, data=SC1) #p=0.4977
kruskal.test(SedimentPAH~Year, data=SRB1) #p=NA because all values are the same
kruskal.test(SedimentPAH~Year, data=TCB1) #p=0.09826

#No differences over time when outliers are removed within any site

#Don't have to worry about this because we are not keeping the outlier analysis in the report

#### Kruskal-Wallis test [PAH] across sites and graphs ####

#With potential outliers included
PAHsites<-read.csv("SedimentPAHsites.csv", header=T)
PAHsites$Year<-as.factor(PAHsites$Year)
str(PAHsites)

shapiro.test(PAHsites$SedimentPAH) #p-value < 2.2e-16 , means that data is not normal
leveneTest(SedimentPAH~Site, data=PAHsites) #p=1.316e-05, heterogeneity of variance

#Therefore, we use Kruskal Wallis non-parametric test
kruskal.test(SedimentPAH~Site, data=PAHsites) #p<2.2e-16
dunnTest(SedimentPAH~Site, data=PAHsites)

#Use data without outliers
kruskal.test(SedimentPAH~Site, data=PAH_no_outliers) #p-value < 2.2e-16
dunnTest(SedimentPAH~Site, data=PAH_no_outliers, method="bh")

mean(BS1$SedimentPAH) #0.085 +/- 0
mean(BB1$SedimentPAH) #0.085 +/- 0
mean(CB1$SedimentPAH) #2.46 +/- 1.51
mean(DFT1$SedimentPAH) #2.21 +/- 1.94
mean(HCM1$SedimentPAH) #0.085 +/- 0
mean(HCNS1$SedimentPAH) #0.085 +/- 0
mean(IH1$SedimentPAH) #0.463 +/- 0.175
mean(LR1$SedimentPAH) #1.70 +/- 1.01
mean(MC1$SedimentPAH) #19.9 +/- 12.9
mean(MB1$SedimentPAH) #0.085 +/- 0
mean(SC1$SedimentPAH) #5.59 +/- 4.72
mean(SRB1$SedimentPAH) #0.085 +/- 0
mean(TCB1$SedimentPAH) #4.74 +/- 2.38


ggplot(PAHsites, aes(x=Sitecode, y=SedimentPAH))+ 
  geom_boxplot()+
  stat_summary(fun=mean, col="blue", size=0.3)+
  stat_boxplot(geom="errorbar", width=0.15)+
  theme_classic()+
  ylab("Total sediment PAH \n concentration (mg/kg DW)")+
  xlab("Site")+
  geom_abline(intercept=0.17, slope=0, col="red", lty=2)+
  theme(axis.text.x = element_text(angle = 60, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_log10()

#plotting the total sediment PAH concentration at each site within each year from 2018-2022 (Supplementary info)

D2018<-subset(PAHsites, Year=="2018")
D2019<-subset(PAHsites, Year=="2019")
D2020<-subset(PAHsites, Year=="2020")
D2021<-subset(PAHsites, Year=="2021")
D2022<-subset(PAHsites, Year=="2022")


plot2018<-ggplot(D2018, aes(x=Sitecode, y=SedimentPAH))+ 
  geom_boxplot()+
  stat_summary(fun=mean, col="blue", size=0.3)+
  stat_boxplot(geom="errorbar", width=0.15)+
  theme_classic()+
  ylab("Total sediment PAH \n concentration (mg/kg DW)")+
  xlab("Site")+
  geom_abline(intercept=1.30384, slope=0, col="red", lty=2)+
  theme(axis.text.x = element_text(angle = 60, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_log10()

plot2019<-ggplot(D2019, aes(x=Sitecode, y=SedimentPAH))+ 
  geom_boxplot()+
  stat_summary(fun=mean, col="blue", size=0.3)+
  stat_boxplot(geom="errorbar", width=0.15)+
  theme_classic()+
  ylab("Total sediment PAH \n concentration (mg/kg DW)")+
  xlab("Site")+
  geom_abline(intercept=1.30384, slope=0, col="red", lty=2)+
  theme(axis.text.x = element_text(angle = 60, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_log10()

plot2020<-ggplot(D2020, aes(x=Sitecode, y=SedimentPAH))+ 
  geom_boxplot()+
  stat_summary(fun=mean, col="blue", size=0.3)+
  stat_boxplot(geom="errorbar", width=0.15)+
  theme_classic()+
  ylab("Total sediment PAH \n concentration (mg/kg DW)")+
  xlab("Site")+
  geom_abline(intercept=1.30384, slope=0, col="red", lty=2)+
  theme(axis.text.x = element_text(angle = 60, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_log10()

plot2021<-ggplot(D2021, aes(x=Sitecode, y=SedimentPAH))+ 
  geom_boxplot()+
  stat_summary(fun=mean, col="blue", size=0.3)+
  stat_boxplot(geom="errorbar", width=0.15)+
  theme_classic()+
  ylab("Total sediment PAH \n concentration (mg/kg DW)")+
  xlab("Site")+
  geom_abline(intercept=1.30384, slope=0, col="red", lty=2)+
  theme(axis.text.x = element_text(angle = 60, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_log10()

plot2022<-ggplot(D2022, aes(x=Sitecode, y=SedimentPAH))+ 
  geom_boxplot()+
  stat_summary(fun=mean, col="blue", size=0.3)+
  stat_boxplot(geom="errorbar", width=0.15)+
  theme_classic()+
  ylab("Total sediment PAH \n concentration (mg/kg DW)")+
  xlab("Site")+
  geom_abline(intercept=1.30384, slope=0, col="red", lty=2)+
  theme(axis.text.x = element_text(angle = 60, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_log10()

ggarrange<-ggarrange(plot2018 + rremove("ylab") + rremove("xlab"), plot2019+rremove("ylab")+ rremove("xlab"), plot2020 + rremove("ylab")+ rremove("xlab"), plot2021 + rremove("ylab")+ rremove("xlab"), plot2022 + rremove("ylab")+ rremove("xlab"), 
           labels = c("2018", "2019", "2020", "2021", "2022", "F"),
          hjust=-4.7, align = "hv",
           ncol = 3, nrow = 2, widths = c(1, 1, 1), heights = c(1,1,1))



annotate_figure(ggarrange, left = textGrob("Total sediment PAH \n concentration (mg/kg DW)", rot = 90, vjust = 0.5, gp = gpar(cex = 1)),
                bottom = textGrob("Site code", vjust=0.5, gp = gpar(cex = 1)))

#### PAH composition across years ####

#The following section of code creates bar graphs illustrating the percentage of each analyte to the total 
#PAH concentraiton each year. This final graph can be found in Supplementary information, Figure 3
PAHcomposition<-read.csv("PAHcomposition2018-2022.csv", header=T)
PAHcomposition$Year<-as.factor(PAHcomposition$Year)
str(PAHcomposition)

PAH2018<-subset(PAHcomposition, Year=="2018")
PAH2019<-subset(PAHcomposition, Year=="2019")
PAH2020<-subset(PAHcomposition, Year=="2020")
PAH2021<-subset(PAHcomposition, Year=="2021")
PAH2022<-subset(PAHcomposition, Year=="2022")

comp2018<-ggplot(PAH2018, aes(x=PAH_analyte, y=Percent_of_total))+ 
  geom_col(colour="black", fill="#56B4E9")+
  theme_classic()+
  ylab("Percent of total PAH concentration (%)")+
  xlab("PAH analyte")+
  theme(text = element_text(size=8),axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 26))+
  ylim(0, 30)+
  scale_y_log10()
comp2018

comp2019<-ggplot(PAH2019, aes(x=PAH_analyte, y=Percent_of_total))+ 
  geom_col(colour="black", fill="#56B4E9")+
  theme_classic()+
  ylab("Percent of total PAH concentration (%)")+
  xlab("PAH analyte")+
  theme(text = element_text(size=8),axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 26))+
  ylim(0, 30)+
  scale_y_log10()
comp2019

comp2020<-ggplot(PAH2020, aes(x=PAH_analyte, y=Percent_of_total))+ 
  geom_col(colour="black", fill="#56B4E9")+
  theme_classic()+
  ylab("Percent of total PAH concentration (%)")+
  xlab("PAH analyte")+
  theme(text = element_text(size=8),axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 26))+
  ylim(0, 30)+
  scale_y_log10()
comp2020

comp2021<-ggplot(PAH2021, aes(x=PAH_analyte, y=Percent_of_total))+ 
  geom_col(colour="black", fill="#56B4E9")+
  theme_classic()+
  ylab("Percent of total PAH concentration (%)")+
  xlab("PAH analyte")+
  theme(text = element_text(size=8),axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 26))+
  ylim(0, 30)+
  scale_y_log10()
comp2021

comp2022<-ggplot(PAH2022, aes(x=PAH_analyte, y=Percent_of_total))+ 
  geom_col(colour="black", fill="#56B4E9")+
  theme_classic()+
  ylab("Percent of total PAH concentration (%)")+
  xlab("PAH analyte")+
  theme(text = element_text(size=8), axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 26))+
  ylim(0, 30)+
  scale_y_log10()
comp2022

comp<-ggarrange(comp2018 + rremove("ylab") + rremove("xlab"), comp2019+rremove("ylab")+ rremove("xlab"), comp2020 + rremove("ylab")+ rremove("xlab"), comp2021 + rremove("ylab")+ rremove("xlab"), comp2022 + rremove("ylab")+ rremove("xlab"), 
                     labels = c("2018", "2019", "2020", "2021", "2022"),
                     hjust=-1.7, align = "hv",
                     ncol = 3, nrow = 2, widths = c(1, 1, 1), heights = c(1,1,1))



annotate_figure(comp, left = textGrob("Percent of total PAH concentration (%)", rot = 90, vjust = 0.5, gp = gpar(cex = 1)),
                bottom = textGrob("PAH analyte", vjust=0.5, gp = gpar(cex = 1)))

#The following kruskal tests were done to evaluate whether there was a temporal difference in the 
#percentage of each individual PAH.

PAH_comp<-read.csv("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Raw data\\PAH composition year and site.csv", header=T)
PAH_comp$Year<-as.factor(PAH_comp$Year)
str(PAH_comp)

ace<-subset(PAH_comp, Analyte=="Acenaphthene")
kruskal.test(data=ace, Percent~Year) #p=0.8533

acy<-subset(PAH_comp, Analyte=="Acenaphthylene")
kruskal.test(data=acy, Percent~Year) #p=0.5005

ant<-subset(PAH_comp, Analyte=="Anthracene")
kruskal.test(data=ant, Percent~Year) #p=0.9583

baa<-subset(PAH_comp, Analyte=="Benz[a]anthracene")
kruskal.test(data=baa, Percent~Year) #p=0.3762

bap<-subset(PAH_comp, Analyte=="Benzo[a]pyrene")
kruskal.test(data=bap, Percent~Year) #p=0.0058
dunnTest(data=bap, Percent~Year) #sig diff btw 2018-2021: p=0.016, and 2018-2022: p=0.035

bbf<-subset(PAH_comp, Analyte=="Benzo[b,j]fluoranthene")
kruskal.test(data=bbf, Percent~Year) #p=0.2553

bep<-subset(PAH_comp, Analyte=="Benzo[e]pyrene")
kruskal.test(data=bep, Percent~Year) #p=0.4906

bgp<-subset(PAH_comp, Analyte=="Benzo(g,h,i)perylene-D12")
kruskal.test(data=bgp, Percent~Year) #p=0.01867
dunnTest(data=bgp, Percent~Year) #sig diff btw 2018-2021: p=0.012

bkf<-subset(PAH_comp, Analyte=="Benzo[k]fluoranthene")
kruskal.test(data=bkf, Percent~Year) #p=0.9401

ct<-subset(PAH_comp, Analyte=="Chrysene + Triphenylene")
kruskal.test(data=ct, Percent~Year) #p=9.867e-05
dunnTest(data=ct, Percent~Year) #sig diff btw 2018-2021: p=0.0076, 2018-2022: p=0.0015, 2019-2022: p=0.025, 2020-2022: p=0.015

daa<-subset(PAH_comp, Analyte=="Dibenz[a,h]anthracene")
kruskal.test(data=daa, Percent~Year) #p=0.3257

fla<-subset(PAH_comp, Analyte=="Fluoranthene")
kruskal.test(data=fla, Percent~Year) #p=0.3548

flu<-subset(PAH_comp, Analyte=="Fluorene")
kruskal.test(data=flu, Percent~Year) #p=0.5742

inp<-subset(PAH_comp, Analyte=="Indeno[1,2,3-cd]pyrene")
kruskal.test(data=inp, Percent~Year) #0.003
dunnTest(data=inp, Percent~Year) #sig diff btw 2018-2020: p=0.046, 2018-2021: p=2018-2021, 2019-2021: p=0.044

nap<-subset(PAH_comp, Analyte=="Naphthalene")
kruskal.test(data=nap, Percent~Year) #0.1025

phe<-subset(PAH_comp, Analyte=="Phenanthrene")
kruskal.test(data=phe, Percent~Year) #0.6421

pyr<-subset(PAH_comp, Analyte=="Pyrene")
kruskal.test(data=pyr, Percent~Year) #0.5092

#### PAH composition across sites ####

#The following code uses Kruskal and Dunns tests to see if there are differences in individual PAH
#percentage of total across sites. This information is presented in supplementary Table 

ace<-subset(PAH_comp, Analyte=="Acenaphthene")
kruskal.test(data=ace, Percent~Site) #p-value  < 2.2e-16
dunnTest(data=ace, Percent~Site)

acy<-subset(PAH_comp, Analyte=="Acenaphthylene")
kruskal.test(data=acy, Percent~Site) #p-value = 3.628e-10

ant<-subset(PAH_comp, Analyte=="Anthracene")
kruskal.test(data=ant, Percent~Site) #p-value = 6.313e-08

baa<-subset(PAH_comp, Analyte=="Benz[a]anthracene")
kruskal.test(data=baa, Percent~Site) #p-value = 4.451e-05

bgp<-subset(PAH_comp, Analyte=="Benzo(g,h,i)perylene-D12")
kruskal.test(data=bgp, Percent~Site) #p-value = 4.813e-05

bap<-subset(PAH_comp, Analyte=="Benzo[a]pyrene")
kruskal.test(data=bap, Percent~Site) #p-value = 2.421e-07

bbf<-subset(PAH_comp, Analyte=="Benzo[b,j]fluoranthene")
kruskal.test(data=bbf, Percent~Site) #p-value = 1.849e-06

bep<-subset(PAH_comp, Analyte=="Benzo[e]pyrene")
kruskal.test(data=bep, Percent~Site) #p-value = 0.0005255

bkf<-subset(PAH_comp, Analyte=="Benzo[k]fluoranthene")
kruskal.test(data=bkf, Percent~Site) #p-value = 0.0005788

ct<-subset(PAH_comp, Analyte=="Chrysene + Triphenylene")
kruskal.test(data=ct, Percent~Site) #p-value = 0.04832

daa<-subset(PAH_comp, Analyte=="Dibenz[a,h]anthracene")
kruskal.test(data=daa, Percent~Site) #p-value = 1.21e-08

fla<-subset(PAH_comp, Analyte=="Fluoranthene")
kruskal.test(data=fla, Percent~Site) #p-value = 3.276e-05

flu<-subset(PAH_comp, Analyte=="Fluorene")
kruskal.test(data=flu, Percent~Site) #p-value = 1.379e-10

inp<-subset(PAH_comp, Analyte=="Indeno[1,2,3-cd]pyrene")
kruskal.test(data=inp, Percent~Site) #p-value = 0.001039

nap<-subset(PAH_comp, Analyte=="Naphthalene")
kruskal.test(data=nap, Percent~Site) #p-value = 7.106e-11

phe<-subset(PAH_comp, Analyte=="Phenanthrene")
kruskal.test(data=phe, Percent~Site) #p-value = 0.0004136

pyr<-subset(PAH_comp, Analyte=="Pyrene")
kruskal.test(data=pyr, Percent~Site) #p-value = 8.828e-06

#### TOC and bioavailability ####
#Total organic carbon
TOC<-read.csv("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\TotalOrganicCarbon2022.csv", header=T)
shapiro.test(TOC$TOC) #not normal, use kruskal
kruskal.test(data=TOC, TOC~Site)
dunnTest(data=TOC, TOC~Site)
ggplot(TOC, aes(x=Site, y=TOC))+geom_boxplot()+theme_classic() #spar cove has the highest TOC, Hazen creek has the least

#Organic carbon normalized PAH concentration
shapiro.test(TotalPAHOCnormalized4sites$TotalPAH)
kruskal.test(data=TotalPAHOCnormalized4sites, TotalPAH~Site) #p=7.43e-04
dunnTest(data=TotalPAHOCnormalized4sites, TotalPAH~Site)

#### Fish indices ####
fish<-read.csv("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\Sediment PAH calculations\\Fish indices\\Fish_indices2018-2022.csv", header=T)

model1<-glm.nb(Abundance~SedimentPAH, data=fish) 
summary(model1)#variance is much larger than the mean so use neg binomial instead of Poisson                                
plot(model1)

model2<-glm(SW_index~SedimentPAH, data=fish, family=Gamma(link="log")) 
summary(model2) #SW can be greater than 1. The values tend to be between 0 and 2. So, it's unusual but I don't hate Gamma
plot(model2) #Looks alright except residuals are a bit off

model2.1<-glm(SW_index~SedimentPAH, data=fish, family=Gamma(link="inverse")) 
summary(model2.1)
AIC(model2, model2.1) #model2 slightly better but not much different

model3<-betareg(Simp_index~SedimentPAH, data=fish)
summary(model3) #particular kind of data where a finite number of trials had been done and some proportion of them were successful. I would use a beta distribution, which is for data between 0 and 1, but not based on a finite number of trials
plot(model3)

model4<-betareg(Evenness~SedimentPAH, data=fish)
summary(model4) #particular kind of data where a finite number of trials had been done and some proportion of them were successful. I would use a beta distribution, which is for data between 0 and 1, but not based on a finite number of trials
plot(model4)

#Plotting abundance vs sediment pah
abundance<-ggplot(fish, aes(x=SedimentPAH, y=Abundance, label=Sitecode))+
  geom_point(aes(x = SedimentPAH, y = Abundance), color="blue")+
  #geom_text(hjust=-0, vjust=-0.7, size=3)+
  geom_smooth(aes(x = SedimentPAH, y = Abundance), data = fish, 
              method = "glm.nb", se = F, color = "blue")+
  theme_classic()+
  xlab("Sediment PAH concentration (mg/kg DW)")+
  geom_errorbarh(aes(xmin=SedimentPAH-SE, xmax=SedimentPAH+SE), height=0)+
  scale_x_log10()+
  ylim(0, 12500)+
  geom_text_repel()
abundance

#SW index vs PAH
SW<-ggplot(fish, aes(x=SedimentPAH, y=SW_index, label=Sitecode))+
  #geom_text(hjust=-0, vjust=-0.7, size=3)+
  geom_point(aes(x=SedimentPAH, y=SW_index), color="cyan4")+
  geom_smooth(aes(x=SedimentPAH, y=SW_index), data=fish, method="glm", se=F, color="cyan4", method.args = list(family = "Gamma"))+
  theme_classic()+
  ylab("Shannon-Wiener Diversity Index")+
  xlab("Sediment PAH concentration (mg/kg DW)")+
  geom_errorbarh(aes(xmin=SedimentPAH-SE, xmax=SedimentPAH+SE), height=0)+
  scale_x_log10()+
  ylim(0, 2)+
  geom_text_repel()
SW

#Simpson's index vs PAH
Simp<-ggplot(fish, aes(x=SedimentPAH, y=Simp_index, label=Sitecode))+
  #geom_text(hjust=-0, vjust=-0.7, size=3)+
  geom_point(aes(x=SedimentPAH, y=Simp_index), color="chocolate1")+
  geom_smooth(aes(x=SedimentPAH, y=Simp_index), data=fish, method="glm", se=F, color="chocolate1", method.args = list(family = "binomial"))+
  ylab("Gini-Simpson Dominance Index")+
  xlab("Sediment PAH concentration (mg/kg DW)")+
  geom_errorbarh(aes(xmin=SedimentPAH-SE, xmax=SedimentPAH+SE), height=0)+
  theme_classic()+
  scale_x_log10()+
  ylim(0, 0.8)+
  geom_text_repel()
Simp

#Pielou vs PAH
Pielou<-ggplot(fish, aes(x=SedimentPAH, y=Evenness, label=Sitecode))+
  #geom_text(hjust=-0, vjust=-0.7, size=3)+
  geom_point(aes(x=SedimentPAH, y=Evenness), color="darkorchid4")+
  geom_smooth(aes(x=SedimentPAH, y=Evenness), data=fish, method="glm", se=F, color="darkorchid4", method.args = list(family = "binomial"))+
  ylab("Pielou Evenness Index")+
  xlab("Sediment PAH concentration (mg/kg DW)")+
  geom_errorbarh(aes(xmin=SedimentPAH-SE, xmax=SedimentPAH+SE), height=0)+
  theme_classic()+
  scale_x_log10()+
  ylim(0, 0.8)+
  geom_text_repel() #makes labels not overlap!!
Pielou

indices<-ggarrange(abundance  + rremove("xlab"), SW+ rremove("xlab"), Simp + rremove("xlab"), Pielou +rremove("xlab")) 
annotate_figure(indices, bottom = textGrob("Sediment PAH concentration (mg/kg; DW)", vjust=0.5, gp = gpar(cex = 1)))

#### Fish indices with Marsh Creek removed ##################

#There is no difference in the overall conclusions between keeping Marsh Creek in or out of analysis
#I will present the stats and figure with Marsh Creek removed since it is easier to visualize on a graph

fish1<-read.csv("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\Sediment PAH calculations\\Fish indices\\Summary of fish indices and PAH_noMC.csv", header=T)

model11<-glm.nb(Abundance~SedimentPAH, data=fish1)
summary(model11)

model21<-glm(SW_index~SedimentPAH, data=fish1, family=Gamma(link="log"))
summary(model21)


model31<-glm(data=fish1, Simp_index~SedimentPAH, family=binomial)
beta(model31)

model41<-glm(data=fish1, Evenness~SedimentPAH, family=binomial)
beta(model41)

#Plotting abundance vs sediment pah
abundance1<-ggplot(fish1, aes(x=SedimentPAH, y=Abundance))+
  geom_point()+
  geom_smooth(aes(x = SedimentPAH, y = Abundance), data = fish1, 
              method = "glm.nb", se = F, color = "blue")+
  theme_classic()+
  xlab("Sediment PAH concentration (mg/kg DW)")+
  xlim(0, 13)
abundance1

#SW index vs PAH
SW1<-ggplot(fish1, aes(x=SedimentPAH, y=SW_index))+
  geom_point(aes(x=SedimentPAH, y=SW_index))+
  geom_smooth(aes(x=SedimentPAH, y=SW_index), data=fish1, method="glm", se=F, color="cyan4", method.args = list(family = "poisson"))+
  theme_classic()+
  ylab("Shannon-Wiener Diversity Index")+
  xlab("Sediment PAH concentration (mg/kg DW)")+
  xlim(0, 13)+
  ylim(0,2)
SW1

#Simpson's index vs PAH
Simp1<-ggplot(fish1, aes(x=SedimentPAH, y=Simp_index))+
  geom_point(aes(x=SedimentPAH, y=Simp_index))+
  geom_smooth(aes(x=SedimentPAH, y=Simp_index), data=fish1, method="glm", se=F, color="chocolate1", method.args = list(family = "binomial"))+
  ylab("Simpson's Dominance Index")+
  xlab("Sediment PAH concentration (mg/kg DW)")+
  theme_classic()+
  xlim(0, 13)
Simp1

#Pilou vs PAH
Pilou1<-ggplot(fish1, aes(x=SedimentPAH, y=Evenness))+
  geom_point(aes(x=SedimentPAH, y=Evenness))+
  geom_smooth(aes(x=SedimentPAH, y=Evenness), data=fish1, method="glm", se=F, color="darkorchid4", method.args = list(family = "binomial"))+
  ylab("Pilou Evenness Index")+
  xlab("Sediment PAH concentration (mg/kg DW)")+
  theme_classic()+
  xlim(0, 13)
Pilou1

indices1<-ggarrange(abundance1  + rremove("xlab"), SW1+ rremove("xlab"), Simp1 + rremove("xlab"), Pilou1 +rremove("xlab")) 
annotate_figure(indices1, bottom = textGrob("Sediment PAH concentration (mg/kg; DW)", vjust=0.5, gp = gpar(cex = 1)))

#### Fish indices with the four Atlantic silverside collection sites, TOC normalized PAH data ####
#using only the sediment PAH data from July 2022 (n=6), these data were normalized by the % organic
#carbon found in the sediments to partially account for the bioavilability of PAHs. The average normalized
#PAH concentrations were then regressed against the same species diversity indices to see if anything
#changed. This data is presented in the supplemental information

model5<-glm.nb(Abundance~PAHnorm, data=fish) 
summary(model5)#p=0.649                                

model6<-glm(SW_index~PAHnorm, data=fish, family=Gamma(link="log")) 
summary(model6) #p=0.916

model7<-betareg(Simp_index~PAHnorm, data=fish)
summary(model7) #p=0.117

model8<-betareg(Evenness~PAHnorm, data=fish)
summary(model8) #p=0.137

#Plotting abundance vs TOC normalized sediment pah
abundance_norm<-ggplot(fish, aes(x=PAHnorm, y=Abundance, label=Sitecode))+
  geom_text(hjust=0.5, vjust=1.5, size=2.5)+
  geom_point(aes(x = PAHnorm, y = Abundance), color="blue")+
  geom_smooth(aes(x = PAHnorm, y = Abundance), data = fish, 
              method = "glm.nb", se = F, color = "blue")+
  theme_classic()+
  xlab("TOC normalized sediment PAH concentration (mg/kg/TOC%)")+
  geom_errorbarh(aes(xmin=PAHnorm-Senorm, xmax=PAHnorm+Senorm), height=0)+
  scale_x_log10()
abundance_norm

#Plotting Shannon-Wiener diversity vs TOC normalized sediment PAH
SW_norm<-ggplot(fish, aes(x=PAHnorm, y=SW_index, label=Sitecode))+
  geom_text(hjust=0.5, vjust=-1, size=2.5)+
  geom_point(aes(x=PAHnorm, y=SW_index), color="cyan4")+
  geom_smooth(aes(x=PAHnorm, y=SW_index), data=fish, method="glm", se=F, color="cyan4", method.args = list(family = "Gamma"))+
  theme_classic()+
  ylab("Shannon-Wiener Diversity Index")+
  xlab("TOC normalized sediment PAH concentration (mg/kg/TOC%)")+
  geom_errorbarh(aes(xmin=PAHnorm-Senorm, xmax=PAHnorm+Senorm), height=0)+
  scale_x_log10()
SW_norm

#Simpson's index vs PAH
Simp_norm<-ggplot(fish, aes(x=PAHnorm, y=Simp_index, label=Sitecode))+
  geom_text(hjust=0.5, vjust=-0.5, size=2.5)+
  geom_point(aes(x=PAHnorm, y=Simp_index), color="chocolate1")+
  geom_smooth(aes(x=PAHnorm, y=Simp_index), data=fish, method="glm", se=F, color="chocolate1", method.args = list(family = "binomial"))+
  ylab("Gini-Simpson's Dominance Index")+
  xlab("TOC normalized sediment PAH concentration (mg/kg/TOC%)")+
  geom_errorbarh(aes(xmin=PAHnorm-Senorm, xmax=PAHnorm+Senorm), height=0)+
  theme_classic()+
  scale_x_log10()
Simp_norm

#Pielou vs PAH
Pielou_norm<-ggplot(fish, aes(x=PAHnorm, y=Evenness, label=Sitecode))+
  geom_text(hjust=0.5, vjust=-0.5, size=2.5)+
  geom_point(aes(x=PAHnorm, y=Evenness), color="darkorchid4")+
  geom_smooth(aes(x=PAHnorm, y=Evenness), data=fish, method="glm", se=F, color="darkorchid4", method.args = list(family = "binomial"))+
  ylab("Pielou Evenness Index")+
  xlab("TOC normalized sediment PAH concentration (mg/kg/TOC%)")+
  geom_errorbarh(aes(xmin=PAHnorm-Senorm, xmax=PAHnorm+Senorm), height=0)+
  theme_classic()+
  scale_x_log10()
Pielou_norm

indices<-ggarrange(abundance_norm + rremove("xlab"), SW_norm + rremove("xlab"), Simp_norm + rremove("xlab"), Pielou_norm +rremove("xlab")) 
annotate_figure(indices, bottom = textGrob("TOC normalized sediment PAH concentration (mg/kg/TOC%)", vjust=0.5, gp = gpar(cex = 1)))




#### EROD activity, LSI, K ####
EROD<-read.csv("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\EROD activity for R.csv", header=T)
str(EROD)
cor(EROD$Site, EROD$EROD)
ggplot(EROD, aes(x=Site, y=EROD))+geom_boxplot()

erod1<-aov(EROD~Site, data=EROD)
summary(erod1)
plot(erod1) #Not normal and some issues with heteroskedasticity

shapiro.test(EROD$EROD) #p-value < 2.56 e-05, means that data is not normal when all sites with outliers 
#are included
leveneTest(EROD~Site, data=EROD) #p-value is 0.04382, means there is heteroskedasticity

#Are there outliers?
erodHC<-subset(EROD, Site=="Hazen Creek")
median(erodHC$EROD)+(mad(erodHC$EROD))*2.5 #0.557. No outliers
median(erodHC$EROD)-(mad(erodHC$EROD))*2.5 #-0.183 no outliers
shapiro.test(erodHC$EROD) #p=0.3941 means data is normal

mean(erodHC$EROD) #0.187
sd(erodHC$EROD) #0.127

erodIH<-subset(EROD, Site=="Inner Harbour")
median(erodIH$EROD)+(mad(erodIH$EROD))*2.5 #0.1411. 0.272 is an outlier
median(erodIH$EROD)-(mad(erodIH$EROD))*2.5 #-0.011
shapiro.test(erodIH$EROD) #p=0.0001352 with outlier included

ggplot(erodIH, aes(x=Site, y=EROD))+ #one outlier detected
  geom_boxplot()

#stats with outlier included
mean(erodIH$EROD) #0.082 
sd(erodIH$EROD) #0.070

#stats with outlier excluded
EROD_no_outlier<-read.csv("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\EROD activity for R outlier removed.csv", header=T)
erodIH1<-subset(EROD_no_outlier, Site=="Inner Harbour")
mean(erodIH1$EROD) #0.061
sd(erodIH1$EROD) #0.020


erodSC<-subset(EROD, Site=="Spar Cove")
median(erodSC$EROD)+(mad(erodSC$EROD))*2.5 #0.256.no outliers
median(erodSC$EROD)-(mad(erodSC$EROD))*2.5
shapiro.test(erodSC$EROD) #p=0.4412

mean(erodSC$EROD) #0.099
sd(erodSC$EROD) #0.055

erodTCB<-subset(EROD, Site=="Tin Can Beach")
median(erodTCB$EROD)+(mad(erodTCB$EROD))*2.5 #0.1955. no outliers
median(erodTCB$EROD)-(mad(erodTCB$EROD))*2.5
shapiro.test(erodTCB$EROD) #0.9528

mean(erodTCB$EROD) #0.088
sd(erodTCB$EROD) #0.045

#Use Kruskal-Wallis test instead of ANOVA

kruskal.test(EROD~Site, data=EROD) #p=0.09005, no sig differences in EROD activity btw sites

#Remove outlier and test assumptions again
EROD2<-read.csv("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\EROD activity for R outlier removed.csv", header=T)
shapiro.test(EROD2$EROD) #p=1.247e-05, still not normal
erod2<-aov(EROD~Site, data=EROD2)
summary(erod2)
plot(erod2)

kruskal.test(EROD~Site, data=EROD2) #p=0.03254. Sig differences when IH outlier is removed
mean(erodHC$EROD) #0.187
mean(erodIH$EROD) #0.0817
mean(erodSC$EROD) #0.0994
mean(erodTCB$EROD) #0.0879
dunnTest(EROD~Site, data=EROD2) #sig difference between Hazen Creek and Inner Harbour p=0.019

ggplot(EROD2, aes(x=Site, y=EROD))+
  geom_boxplot()+
  theme_classic()+
  ylab("EROD activity (pmol/mg/min)")+
  stat_summary(fun=mean, col="blue", size=0.3)

#LSI
#Read in LSI.xlsx. This csv was messing up the data for some reason
#LSI<-read.csv("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\UNB 2018-2022\\PAH project summer 2022\\Data\\Modified data\\LSI.csv", header=T)
shapiro.test(LSI$LSI)
kruskal.test(data=LSI, LSI~Site) #p=0.038
dunnTest(data=LSI, LSI~Site) #btw IH and SC is 0.053, not really a big difference

ggplot(LSI, aes(x=Site, y=LSI))+
  geom_boxplot()+
  theme_classic()+
  stat_summary(fun = mean, col="blue", size=0.3)+
  ylab("Liver somatic index (%)")

#Condition factor, K
#Import data, K.xlsx
shapiro.test(K$K)
kruskal.test(data=K, K~Site) #p=7.45e-09
dunnTest(data=K, K~Site) 

ggplot(K, aes(x=Site, y=K))+
  geom_boxplot()+
  theme_classic()+
  stat_summary(fun = mean, col="blue", size=0.3)+
  ylab("Condition factor (K)")

#### Trace metals ####

#Import SedimentChemistry.xlsx data file

Al<-subset(SedimentChemistry, Metal=="Aluminum")
shapiro.test(Al$Result)
kruskal.test(data=Al, Result~Site) #p=0.00039
dunnTest(data=Al, Result~Site)
ggplot(Al, aes(x=Site, y=Result))+geom_boxplot()+theme_classic()+
  stat_summary(fun=mean, col="blue", size=0.3) #highest at spar cove

Sb<-subset(SedimentChemistry, Metal=="Antimony")
shapiro.test(Sb$Result)
kruskal.test(data=Sb, Result~Site) #p=0.00035
dunnTest(data=Sb, Result~Site)
ggplot(Sb, aes(x=Site, y=Result))+geom_boxplot()+theme_classic()+
  stat_summary(fun=mean, col="blue", size=0.3)#highest at tin can beach

As<-subset(SedimentChemistry, Metal=="Arsenic")
shapiro.test(As$Result)
kruskal.test(data=As, Result~Site) #p=0.00016
dunnTest(data=As, Result~Site)
ggplot(As, aes(x=Site, y=Result))+geom_boxplot()+theme_classic()+
  stat_summary(fun=mean, col="blue", size=0.3)#highest at spar cove

Ba<-subset(SedimentChemistry, Metal=="Barium")
shapiro.test(Ba$Result)
kruskal.test(data=Ba, Result~Site) #p=0.00019
dunnTest(data=Ba, Result~Site)
ggplot(Ba, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at TCB

Be<-subset(SedimentChemistry, Metal=="Beryllium")
shapiro.test(Be$Result) 
kruskal.test(data=Be, Result~Site) #p=0.0026
dunnTest(data=Be, Result~Site)
ggplot(Be, aes(x=Site, y=Result))+geom_boxplot()+theme_classic()+
  stat_summary(fun=mean, col="blue", size=0.3)#highest at IH, SC, TCB

Bi<-subset(SedimentChemistry, Metal=="Bismuth")
shapiro.test(Bi$Result) 
kruskal.test(data=Bi, Result~Site) #p=NA, all below RL
ggplot(Bi, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #Same across the board

B<-subset(SedimentChemistry, Metal=="Boron")
shapiro.test(B$Result) 
kruskal.test(data=B, Result~Site) #p=0.00033
dunnTest(data=B, Result~Site)
ggplot(B, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at inner harbour

Cd<-subset(SedimentChemistry, Metal=="Cadmium")
shapiro.test(Cd$Result) 
kruskal.test(data=Cd, Result~Site) #p=0.0016
dunnTest(data=Cd, Result~Site)
ggplot(Cd, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

Ca<-subset(SedimentChemistry, Metal=="Calcium")
shapiro.test(Ca$Result) 
summary(aov(data=Ca, Result~Site)) #p=1.02e-05
TukeyHSD(aov(data=Ca, Result~Site))
ggplot(Ca, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at inner harbour

Cr<-subset(SedimentChemistry, Metal=="Chromium")
shapiro.test(Cr$Result) 
kruskal.test(data=Cr, Result~Site) #p=7.449e-05
dunnTest(data=Cr, Result~Site)
ggplot(Cr, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

Co<-subset(SedimentChemistry, Metal=="Cobalt")
shapiro.test(Co$Result) 
summary(aov(data=Co, Result~Site)) #p=2.96e-09
TukeyHSD(aov(data=Co, Result~Site))
ggplot(Co, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

Cu<-subset(SedimentChemistry, Metal=="Copper")
shapiro.test(Cu$Result) 
kruskal.test(data=Cu, Result~Site) #p=0.0001821
dunnTest(data=Cu, Result~Site)
ggplot(Cu, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

Fe<-subset(SedimentChemistry, Metal=="Iron")
shapiro.test(Fe$Result) 
kruskal.test(data=Fe, Result~Site) #p=0.0001223
dunnTest(data=Fe, Result~Site)
ggplot(Fe, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

Pb<-subset(SedimentChemistry, Metal=="Lead")
shapiro.test(Pb$Result) 
kruskal.test(data=Pb, Result~Site) #p=0.0005699
dunnTest(data=Pb, Result~Site)
ggplot(Pb, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

Li<-subset(SedimentChemistry, Metal=="Lithium")
shapiro.test(Li$Result) 
kruskal.test(data=Li, Result~Site) #p=0.00041
dunnTest(data=Li, Result~Site)
ggplot(Li, aes(x=Site, y=Result))+geom_boxplot()+theme_classic()+
  stat_summary(fun=mean, col="blue", size=0.3)#highest at inner harbour

Mg<-subset(SedimentChemistry, Metal=="Magnesium")
shapiro.test(Mg$Result) 
summary(aov(data=Mg, Result~Site)) #p=3.29e-07
TukeyHSD(aov(data=Mg, Result~Site))
ggplot(Mg, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

Mn<-subset(SedimentChemistry, Metal=="Manganese")
shapiro.test(Mn$Result) 
kruskal.test(data=Mn, Result~Site) #p=0.0026
dunnTest(data=Mn, Result~Site)
ggplot(Mn, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at inner harbour

Mo<-subset(SedimentChemistry, Metal=="Molybdenum")
shapiro.test(Mo$Result) 
kruskal.test(data=Mo, Result~Site) #p=0.00034
dunnTest(data=Mo, Result~Site)
ggplot(Mo, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

Ni<-subset(SedimentChemistry, Metal=="Nickel")
shapiro.test(Ni$Result) 
kruskal.test(data=Ni, Result~Site) #p=0.00014
dunnTest(data=Ni, Result~Site)
ggplot(Ni, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #highest at spar cove

K<-subset(SedimentChemistry, Metal=="Potassium")
shapiro.test(K$Result) 
kruskal.test(data=K, Result~Site) #p=0.00014
dunnTest(data=K, Result~Site)
ggplot(K, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #Inner harbour is highest

Rb<-subset(SedimentChemistry, Metal=="Rubidium")
shapiro.test(Rb$Result) 
kruskal.test(data=Rb, Result~Site) #p=0.00011
dunnTest(data=Rb, Result~Site)
ggplot(Rb, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #inner harbour is highest

Se<-subset(SedimentChemistry, Metal=="Selenium")
shapiro.test(Se$Result) 
kruskal.test(data=Se, Result~Site) #p=0.00296
dunnTest(data=Se, Result~Site)
ggplot(Se, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #inner harbour and spar cove are highest

Ag<-subset(SedimentChemistry, Metal=="Silver")
shapiro.test(Ag$Result) #below detection
kruskal.test(data=Ag, Result~Site) #p=N/A
ggplot(Ag, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #all the same

Na<-subset(SedimentChemistry, Metal=="Sodium")
shapiro.test(Na$Result) 
kruskal.test(data=Na, Result~Site) #p=0.0016
dunnTest(data=Na, Result~Site)
ggplot(Na, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #inner harbour is highest

Sr<-subset(SedimentChemistry, Metal=="Strontium")
shapiro.test(Sr$Result) 
kruskal.test(data=Sr, Result~Site) #p=0.00013
dunnTest(data=Mn, Result~Site)
ggplot(Sr, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #Tin Can Beach is highest

Te<-subset(SedimentChemistry, Metal=="Tellurium")
shapiro.test(Te$Result) #Below detection
kruskal.test(data=Te, Result~Site) #p=N/A
ggplot(Te, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #all the same

Tl<-subset(SedimentChemistry, Metal=="Thallium")
shapiro.test(Tl$Result) 
kruskal.test(data=Tl, Result~Site) #p=0.02475
dunnTest(data=Tl, Result~Site)
ggplot(Tl, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #inner harbour and spar cove are highest

Sn<-subset(SedimentChemistry, Metal=="Tin")
shapiro.test(Sn$Result) 
kruskal.test(data=Sn, Result~Site) #p=0.00029
dunnTest(data=Sn, Result~Site)
ggplot(Sn, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #Tin can beach is highest

U<-subset(SedimentChemistry, Metal=="Uranium")
shapiro.test(U$Result) 
summary(aov(data=U, Result~Site)) #p=2.42e-09
TukeyHSD(aov(data=U, Result~Site))
ggplot(U, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #Highest at Spar Cove

V<-subset(SedimentChemistry, Metal=="Vanadium")
shapiro.test(V$Result) 
summary(aov(data=V, Result~Site)) #p=8.89e-08
TukeyHSD(aov(data=V, Result~Site))
ggplot(V, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #Highest at Spar Cove

Zn<-subset(SedimentChemistry, Metal=="Zinc")
shapiro.test(Zn$Result) 
kruskal.test(data=Zn, Result~Site) #p=0.00026
dunnTest(data=Zn, Result~Site)
ggplot(Zn, aes(x=Site, y=Result))+geom_boxplot()+theme_classic() #Tin can beach and spar cove are highest


#Trying to graph all metals from each site
HCNSgraphdata<-subset(SedimentChemistry, Site=="Hazen Creek Nearshore")
HCNSgraph<-ggplot(HCNSgraphdata, aes(x=Element_symbol, y=Result, color_palette(viridis.map)))+
  geom_col(aes(fill=Result))+
  scale_y_log10()+
  theme_classic()+
  xlab("Trace metal element symbol")+
  ylab("Result value (mg/kg; DW)")
HCNSgraph

IHgraphdata<-subset(SedimentChemistry, Site=="Inner Harbour")
IHgraph<-ggplot(IHgraphdata, aes(x=Element_symbol, y=Result))+
  geom_col()+
  scale_y_log10()+
  theme_classic()+
  xlab("Trace metal element symbol")+
  ylab("Result value (mg/kg; DW)")  
IHgraph

SCgraphdata<-subset(SedimentChemistry)

  



