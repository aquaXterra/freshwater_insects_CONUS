#This script is for plotting trait data at occurrence locations across the contiguous US
#Laura Twardochleb
#4_6_2020

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits")

library(tidyverse)
library(rgdal)
library(usmap)
library(maptools)
library(sp)
library(ggpubr)

################# Read in data ####################################
trait_mode<-read.csv("~/Documents/WaterCube/Ch.3/Writing/Data_paper/Files_EDI/trait_revision_for_EDI/Genus_Traits.csv")
trait_mode2<-subset(trait_mode,!is.na(trait_mode$Trait)) #8170 traits in trait mode table
occurrence<-read.csv("~/Documents/WaterCube/Ch.3/Writing/Data_paper/Files_EDI/Genus_Occurrences.csv")


################### Reproject spatial coordinates to US Albers Equal Area Projection #####################################################################################################
#subset records to those in WGS84 and reproject to Albers equal area 
crsaea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
wgs1984.proj <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
crsnad83<-'+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0'
crsnad27<-'+init=epsg:4267 +proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs+nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat'

#subset records to those in WGS84 and reproject to Albers equal area 
recordsWGS84<-subset(occurrence, Horizontal_datum=="WGS84")
wgs84spatial<-with(recordsWGS84, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(wgs1984.proj)))
sptransformwgs84<-spTransform(wgs84spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.wgs <- as.data.frame(sptransformwgs84@coords) 
names(site_coords.wgs)<-c("Longitude_transformed", "Latitude_transformed")
wgs84<-cbind(recordsWGS84, site_coords.wgs)

#reproject those in NAD83
recordsnad83<-subset(occurrence, Horizontal_datum=="NAD83")
nad83spatial<-with(recordsnad83, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(crsnad83)))
sptransformnad83<-spTransform(nad83spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.nad83 <- as.data.frame(sptransformnad83@coords) 
names(site_coords.nad83)<-c("Longitude_transformed", "Latitude_transformed")
nad83<-cbind(recordsnad83, site_coords.nad83)

#reproject those in NAD27
recordsnad27<-subset(occurrence, Horizontal_datum=="NAD27")
nad27spatial<-with(recordsnad27, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(crsnad27)))
sptransformnad27<-spTransform(nad27spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.nad27 <- as.data.frame(sptransformnad27@coords) 
names(site_coords.nad27)<-c("Longitude_transformed", "Latitude_transformed")
nad27<-cbind(recordsnad27, site_coords.nad27)

#occurrence dataset with reprojected points
insect.occurrence<-rbind(nad27, nad83, wgs84)

#subset to just records with insect genera- there are ~2.06 million records of insect genera in streams
genus.occurrence<-subset(insect.occurrence, !is.na(Genus))

#load libraries for mapping
library(maps); library(mapproj); library(ggspatial); library(ggsn)

#reproject state data from map_data
states <- map_data('state')
state_coords <- SpatialPoints(coords = with(states, data.frame(x = long, y = lat)), proj4string = CRS(wgs1984.proj))
state_albers <- spTransform(state_coords, CRSobj = CRS(crsaea))
states$long <- state_albers@coords[,1]
states$lat <- state_albers@coords[,2]

#################### multipanel plot of traits #########################################
#merge trait and occurrence data
insect_traits<-inner_join(genus.occurrence, trait_mode2, by="Genus")

#calculate proportion for each trait by site
traits_percent <- insect_traits %>%
  group_by(Longitude_transformed, Latitude_transformed, Trait_group, Trait)%>%
  mutate(n_genus = length(unique(Genus)))%>%
  ungroup()%>%
  group_by(Longitude_transformed, Latitude_transformed, Trait_group)%>%
  mutate(n_genus_all_values = length(unique(Genus)))%>%
  mutate(percent=n_genus/n_genus_all_values) %>%
  distinct(Longitude_transformed, Latitude_transformed, Trait_group, Trait, percent, .keep_all = TRUE)

labels <- c("Warm eurythermal (15-30 C)" = "Warm eurythermal (15-30 °C)", "Bi_multivoltine" = "Bivoltine-multivoltine", "eros"="Erosional", "Gills"="Gills")

#plot proportion of insects with different traits
sites_map<-ggplot(states, aes(x=long,y=lat)) 
richness_trait<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = traits_percent%>%filter(Trait=="Warm eurythermal (15-30 C)"|Trait=="Bi_multivoltine"|Trait=="eros"|Trait=="Gills"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20, size=0.7)+scale_colour_gradient(low = "steelblue2", high = "red")+
  labs(colour  = "Proportion of genera with trait")+
  guides(colour=guide_legend(override.aes = list(size=8)))+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 25), legend.title = element_text(size=25, face="bold"), legend.position="top")+
  facet_wrap(~Trait, scales = "free", labeller=labeller(Trait = labels))+theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"), strip.text.x = element_text(size = 20, color = "black", face = "bold"))
richness_trait2<-richness_trait+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5.5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.12,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/fig.4.png", height = 12, width = 16, dpi = 600)
richness_trait2
dev.off()

ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/fig.4.pdf", height = 12, width = 16, dpi = 600)
richness_trait2
dev.off()
