setwd("C:\\Users\\Emilie\\OneDrive - University of New Brunswick\\Documents\\University\\PAH project summer 2022\\Tables and figures")
coord<-read.csv("Site coordinates for R.csv", header=T)
library(ggplot2)
library(ggmap)
library(raster)
library(broom)
library(ggthemes)
library(ggsn)
library(maps)
library(mapdata)
library(marmap)
library(rgdal)

canada <- getData("GADM", country = "canada", level = 1) 
provinces <- c("New Brunswick")
ca.province <- canada[canada$NAME_1 %in% provinces,]
ca.province.df <- tidy(ca.province)

#Retrieve satellite data
register_google(key="AIzaSyAnPAOqauMYf2ObPbEAk6d9Aapp-DNmmIM")

#plotting the map
ggmap(get_googlemap(center=c(lon=-66.11, lat= 45.235), # estimate where the center of your map would be 
                    zoom=12, 
                    maptype = "roadmap"))+  # your options are "terrain","satellite", "roadmap", "hybrid")
  coord_map()+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  #scalebar(location = "bottomright", dist = 100, dist_unit = "km",
           #transform = TRUE, model = 'WGS84',           
           #x.min = -67.6, x.max = -60.05, st.dist=0.03,
           #y.min = 43.5, y.max = 47.9,inherit.aes = FALSE )+ # from ggsn package
  north(ca.province.df, symbol=12)+
  geom_point(data=coord,
             aes(x=Longitude, y=Latitude), 
             col="red",
             size=1,
             inherit.aes = FALSE)+
  ylab("Latitude")+
  xlab("Longitude")
  
  

