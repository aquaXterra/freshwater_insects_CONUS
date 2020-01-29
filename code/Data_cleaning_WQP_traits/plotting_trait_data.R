#This script is for plotting old vs. new trait data
#Laura Twardochleb
#1_17_2020

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits")

library(plyr)
library(reshape2)
library(tidyverse)
library(rgdal)
library(usmap)
library(ggplot2)
library(maptools)
library(sp)
############## read-in datasets #############################################################################################
occurrence22<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/occurrence22.csv", stringsAsFactors = FALSE)

#old dataset before adding traits- overlaps with our dataset
biotraits10 <-  read.csv('~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/biotraits8_11_30_17.csv', stringsAsFactors = FALSE, na.strings=c("","NA"))
length(unique(biotraits10$Genus)) #869 unique genera- all taxa in WQP (only includes USEPA data of taxa in WQP)
#we added 111 taxa before name changes = 980 genera, plus what is in USEPA-only database

epa_old<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/USEPA_only_insects.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

###### Comparing new and old databases-Code to calculate number of genera for each dataset ###############################################
#filtering old dataset to only include certain columns 
#select unique genera for each trait
#make separate figures first
#for now combine datasets with uncleaned nonoverlapping epa traits- will need to revisit after cleaning up nonoverlapping traits
biotraits10.05<-rbind.fill(biotraits10, epa_old) #combines the two relevant epa datasets (taxa in USEPA that are in and not in wqp)
biotraits10.1<-biotraits10.05%>%select(Genus,Voltinism_abbrev,Feed_mode_new,Habit_new,Resp_abbrev,Max_body_size_abbrev,Rheophily_abbrev,Thermal_pref,Female_disp_abbrev, AdultFlyingStrength_abbrev,Emerge_season_1,Emerge_synch_abbrev)
names(biotraits10.1)<-c("Genus","Voltinism_abbrev", "Feed_prim_abbrev","Habit_prim","Resp_abbrev", "Max_body_size_abbrev","Rheophily_abbrev","Thermal_pref","Female_disp_abbrev","AdultFlyingStrength_abbrev","Emerge_season_1","Emerge_synch_abbrev")
biotraits10.2<-melt(biotraits10.1, id="Genus", na.rm=TRUE)
biotraits10.3<-dcast(biotraits10.2, variable~Genus)
biotraits10.4<-biotraits10.3[,-680]
n_Genus1<-rowSums(biotraits10.4 !=0)
biotraits10.5<-cbind(biotraits10.4, n_Genus1) 
colnames(biotraits10.5)[1]<-"Trait"

traits_old<-ggplot(biotraits10.5,aes(x=Trait,y=n_Genus1, fill=Trait))+
  geom_bar(stat="identity")+
  scale_fill_discrete(guide=FALSE)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generations/yr", "Feeding mode", "Habit", "Respiration", "Max. body size", "Flow preference", "Thermal preference", "Flying strength", "Emergence season", "Emergence synchrony"))
traits_old2<-traits_old+theme_classic()+ylab("Number of Genera")+theme(axis.title.x=element_text(size=25), axis.title.y=element_text(size=20), axis.text.x=element_text(size=25, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits_old2

#combine both datasets into one dataset
#initilaize new column for each
#read-in fully-cleaned data that includes USEPA cleaned data to compare to old
trait<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/biotraits7.6.csv", stringsAsFactors = FALSE)
trait2<-trait%>%select(Genus,Voltinism_abbrev,Feed_prim_abbrev,Habit_prim,Resp_abbrev,Max_body_size_abbrev,Rheophily_abbrev,Thermal_pref,Female_disp_abbrev, AdultFlyingStrength_abbrev,Emerge_season_1,Emerge_synch_abbrev)
names(trait2)<-c("Genus","Voltinism_abbrev", "Feed_prim_abbrev","Habit_prim","Resp_abbrev", "Max_body_size_abbrev","Rheophily_abbrev","Thermal_pref","Female_disp_abbrev","AdultFlyingStrength_abbrev","Emerge_season_1","Emerge_synch_abbrev")
trait3<-melt(trait2, id="Genus", na.rm=TRUE)
trait4<-dcast(trait3, variable~Genus)
n_Genus1<-rowSums(trait4 !=0)
trait5<-cbind(trait4, n_Genus1) 
colnames(trait5)[1]<-"Trait"

trait5$Dataset<-"CONUS_traits"
biotraits10.5$Dataset<-"USEPA_traits"

all_traits<-rbind.fill(biotraits10.5, trait5)

#creating the new plot with separation of data based on "dataset" column 
traits<-ggplot(all_traits,aes(x=reorder(Trait, -n_Genus1),y=n_Genus1, group=Dataset)) +
  geom_bar(stat="identity", position="dodge",aes(fill = Dataset))+
  scale_fill_manual(breaks = c("CONUS_traits", "USEPA_traits"), labels=c("CONUS","USEPA"),
                    values=c("lightgreen", "black"))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "Female_disp_abbrev","AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generation/yr", "Feeding style", "Habit", "Respiration", "Max. body size", "Flow preference", "Thermal preference", "Female dispersal","Flying strength", "Emergence season", "Emergence synchrony"))
traits2<-traits+theme_classic()+ylab("Number of Genera")+xlab("Trait Category")+theme(axis.title.x=element_blank(), axis.title.y=element_text(size=30), axis.text.x=element_text(size=25, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits3<-traits2+ theme(legend.position = "top",legend.text = element_text(size = 25), legend.title = element_blank(), panel.border = element_blank())
ggsave('~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/all_Genus_traits_1_17_20.pdf', width=8, height=6.811)
traits2+ theme(legend.position="top",legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))
dev.off()

#just USEPA traits
traits_epa<-ggplot(all_traits,aes(x=reorder(Trait, -n_Genus1),y=n_Genus1, group=Dataset))+
  geom_bar(stat="identity", position="dodge",aes(fill = Dataset))+
  scale_fill_manual(breaks = c("CONUS_traits", "USEPA_traits"),labels=c("","USEPA"),
                    values=c("white", "blue"))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "Female_disp_abbrev","AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generation/yr", "Feeding style", "Habit", "Respiration", "Max. body size", "Flow preference", "Thermal preference", "Female dispersal","Flying strength", "Emergence season", "Emergence synchrony"))
traits_epa2<-traits_epa+theme_classic()+ylab("Number of Genera")+xlab("Trait Category")+theme(axis.title.x=element_text(size=30), axis.title.y=element_text(size=30), axis.text.x=element_text(size=25, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits_epa2+ theme(legend.position="top",legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))

ggsave('~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/EPA_Genus_traits.png', width=8, height=6.811)
traits_epa2+ theme(legend.position="top",legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))
dev.off()

#just axes
traits_legend<-ggplot(all_traits,aes(x=Trait,y=n_Genus1, group=Dataset))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "Female_disp_abbrev","AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generation/yr", "Feeding style", "Habit", "Respiration", "Max. body size", "Flow preference", "Thermal preference", "Female dispersal","Flying strength", "Emergence season", "Emergence synchrony"))
traits_legend2<-traits_legend+theme_classic()+ylab("Number of Genera")+xlab("Trait Category")+theme(axis.title.x=element_text(size=30), axis.title.y=element_text(size=30), axis.text.x=element_text(size=25, angle=45, vjust=0, hjust=1),  axis.text.y=element_text(size=25))
traits_legend2+ theme(legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))

pdf('~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/axes_Genus_traits.pdf', width=8, height=6.811)
traits_legend2+ theme(legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))
dev.off()

##########Plot occurrence data#####################################################################################################
######### Reproject spatial coordinates to US Albers Equal Area Projection ##################################################################################
#subset records to those in WGS84 and reproject to Albers equal area 
crsaea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
wgs1984.proj <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
crsnad83<-'+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0'
crsnad27<-'+init=epsg:4267 +proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs+nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat'

#subset records to those in WGS84 and reproject to Albers equal area 
recordsWGS84<-subset(occurrence22, HorizontalCoordinateReferenceSystemDatumName=="WGS84")
wgs84spatial<-with(recordsWGS84, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(wgs1984.proj)))
sptransformwgs84<-spTransform(wgs84spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.wgs <- as.data.frame(sptransformwgs84@coords) 
names(site_coords.wgs)<-c("Longitude_transformed", "Latitude_transformed")
wgs84<-cbind(recordsWGS84, site_coords.wgs)

#reproject those in NAD83
recordsnad83<-subset(occurrence22, HorizontalCoordinateReferenceSystemDatumName=="NAD83")
nad83spatial<-with(recordsnad83, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(crsnad83)))
sptransformnad83<-spTransform(nad83spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.nad83 <- as.data.frame(sptransformnad83@coords) 
names(site_coords.nad83)<-c("Longitude_transformed", "Latitude_transformed")
nad83<-cbind(recordsnad83, site_coords.nad83)

#reproject those in NAD27
recordsnad27<-subset(occurrence22, HorizontalCoordinateReferenceSystemDatumName=="NAD27")
nad27spatial<-with(recordsnad27, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(crsnad27)))
sptransformnad27<-spTransform(nad27spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.nad27 <- as.data.frame(sptransformnad27@coords) 
names(site_coords.nad27)<-c("Longitude_transformed", "Latitude_transformed")
nad27<-cbind(recordsnad27, site_coords.nad27)

#occurrence dataset with reprojected points
insect.occurrence<-rbind(nad27, nad83, wgs84)

#recombine spatial points for mapping
all.occurrence<-rbind(nad27,nad83,wgs84)%>%
  distinct(Latitude_transformed, Longitude_transformed, .keep_all = TRUE)

#subset to just records with insect genera- there are ~2.06 million records of insect genera in streams
genus.occurrence<-subset(insect.occurrence, !is.na(Genus))

#get all unique locations for all insect taxa - 55,791 unique locations with insect records
lat_long_all<-unique(insect.occurrence[c("Latitude", "Longitude")]) 

#get all unique stream locations for just insect genera - 50,465 unique locations with insect genera
lat_long_genera<-unique(genus.occurrence[c("Latitude", "Longitude")])

occurrencewqp<-subset(insect.occurrence, Provider_name=="STORET") #857,487 insect records in WQP data

occurrencewqp.genera<-subset(genus.occurrence, Provider_name=="STORET") #676,678 insect genus stream records in WQP data

lat_long_wqp<-unique(occurrencewqp[c("Latitude", "Longitude")]) #22,780 insect locations

lat_long_wqp_genera<-unique(occurrencewqp.genera[c("Latitude", "Longitude")]) #17,502 insect genera stream locations

ca_occurrence<-unique(subset(genus.occurrence, Provider_name=="California State Water Resources Control Board")) #203,345 for CA from state- need to also count records from WQP

lat_long_ca_genera<-unique(ca_occurrence[c("Latitude", "Longitude")]) #4,419 unique stream locations for CA- again, only from state

#load libraries for mapping
library(maps); library(ggplot2); library(mapproj); library(dplyr); library(ggspatial); library(ggsn)

#reproject state data from map_data
states <- map_data('state')
state_coords <- SpatialPoints(coords = with(states, data.frame(x = long, y = lat)), proj4string = CRS(wgs1984.proj))
state_albers <- spTransform(state_coords, CRSobj = CRS(crsaea))
states$long <- state_albers@coords[,1]
states$lat <- state_albers@coords[,2]

#color-code richness
#use genus_occurrence df
genus.rich<-genus.occurrence%>%
  group_by(Longitude_transformed, Latitude_transformed)%>%
  mutate(genus_richness=length(unique(Genus)))%>%
  group_by(Order, Longitude_transformed, Latitude_transformed)%>%
  mutate(genus_richness_by_order=length(unique(Genus)))

#plot contrast of WQP and CONUS records
genus.occurrence.conus<-genus.occurrence%>%filter(Provider_name!="STORET")
genus.occurrence.conus$provider2<-"CONUS"
genus.occurrence.wqp<-genus.occurrence%>%filter(Provider_name=="STORET")
genus.occurrence.wqp$provider2<-"WQP"
genus.occurrence.all<-rbind(genus.occurrence.conus, genus.occurrence.wqp)
sites_map<-ggplot(states, aes(x=long,y=lat)) 
sites_map2<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = genus.occurrence.all, aes(x = Longitude_transformed, y = Latitude_transformed, color = provider2, shape=provider2), alpha = 1, size=0.8)+
  scale_color_manual(breaks = c("CONUS", "WQP"),
                     values=c("dodgerblue2", "black"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_blank(), legend.position="top", legend.text = element_text(size = 25), legend.title = element_blank())
sites_map3<-sites_map2+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=7, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.12,symbol=1,location="bottomright")

ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/all_sites2.pdf", sites_map3, height =8, width = 12, dpi = 600)
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/all_sites2.png",sites_map3, height = 8, width = 12, dpi = 600)

#####################Combine traits and occurrence plots#############################################
#install.packages('ggpubr')
library('ggpubr')

fig.2<-ggarrange(traits3, sites_map3, 
          labels = c("A)", "B)"),
          ncol = 1, nrow = 2, font.label=list(size=30, face="bold"))

ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/fig.2.png",fig.2, height = 16, width = 12, dpi = 600)
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/fig.2.pdf",fig.2, height = 16, width = 12, dpi = 600)


############################ Map of raw genus richness by order ##########################################################################################

richness_map<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = genus.rich, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=genus_richness, size=genus_richness), shape=20)+scale_size(range = c(0.05, 5))+scale_colour_gradient(low = "steelblue2", high = "red")+
  labs(colour  = "Genus richness all orders", size = "Genus richness all orders")+
  guides(colour=guide_legend(), size = guide_legend())+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 20), legend.title = element_text(size=20, face="bold"), legend.position="top")
richness_map_all<-richness_map+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=7, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.12,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/Genus_richness.png", height = 8, width = 16, dpi = 300)
richness_map_all
dev.off()

##layout as a facet plot
richness_map_orders<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = genus.rich, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=genus_richness_by_order), shape=20, size=0.7)+scale_colour_gradient(low = "steelblue2", high = "red")+
  labs(colour  = "Genus richness", size = "Genus richness")+
  guides(colour=guide_legend(override.aes = list(size=5)))+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 20), legend.title = element_text(size=20, face="bold"), legend.position="top")+
  facet_wrap(~Order, scales = "free")+theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"), strip.text.x = element_text(
    size = 15, color = "black", face = "bold"))
richness_map_orders2<-richness_map_orders+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=3, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.14,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/Genus_richness_by_order.png", height = 8, width = 16, dpi = 300)
richness_map_orders2
dev.off()

fig.3<-ggarrange(richness_map_all, richness_map_orders, 
                 labels = c("A)", "B)"),
                 ncol = 1, nrow = 2, font.label=list(size=30, face="bold"))
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/fig.3.png",fig.3, height = 16, width = 12, dpi = 600)
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/fig.3.pdf",fig.3, height = 16, width = 12, dpi = 600)
#################### multipanel plot of traits #########################################
##Map proportions with unimputed data 
#read- in unimputed mode trait data
trait_mode_raw<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/trait_mode_table.csv", stringsAsFactors = FALSE)
traits_mode_raw2<-subset(trait_mode_raw,select=c("Genus", "Trait_group", "Trait"))
#merge with occurrence data
insect_traits2<-merge(genus.occurrence, traits_mode_raw2, by="Genus")

#this worked for thermal preference
traits_percent <- insect_traits2 %>%
  group_by(Longitude_transformed, Latitude_transformed, Trait_group, Trait)%>%
  mutate(n_genus = length(unique(Genus)))%>%
  ungroup()%>%
  group_by(Longitude_transformed, Latitude_transformed, Trait_group)%>%
  mutate(n_genus_all_values = length(unique(Genus)))%>%
  mutate(percent=n_genus/n_genus_all_values) %>%
  distinct(Longitude_transformed, Latitude_transformed, Trait_group, Trait, percent, .keep_all = TRUE)

labels <- c("Warm eurythermal (15-30 C)" = "Warm eurythermal (15-30 °C)", "Bi_multivoltine" = "Bivoltine-multivoltine", "eros"="Erosional", "Gills"="Gills")

#plot proportion of insects with different traits
richness_trait<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = traits_percent%>%filter(Trait=="Warm eurythermal (15-30 C)"|Trait=="Bi_multivoltine"|Trait=="eros"|Trait=="Gills"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20, size=0.7)+scale_colour_gradient(low = "steelblue2", high = "red")+
  labs(colour  = "Proportion of genera with trait")+
  guides(colour=guide_legend(override.aes = list(size=5)))+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 20), legend.title = element_text(size=25, face="bold"), legend.position="top")+
  facet_wrap(~Trait, scales = "free", labeller=labeller(Trait = labels))+theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"), strip.text.x = element_text(size = 20, color = "black", face = "bold"))
richness_trait2<-richness_trait+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5.5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.12,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/fig.4.png", height = 12, width = 16, dpi = 600)
richness_trait2
dev.off()

ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/fig.4.pdf", height = 12, width = 16, dpi = 600)
richness_trait2
dev.off()

save.image("trait_plots.RData")

