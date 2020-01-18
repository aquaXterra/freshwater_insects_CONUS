#This script is for plotting old vs. new trait data
#Laura Twardochleb
#1_17_2020

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

library(plyr)
library(reshape2)
library(tidyverse)

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits")

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
                    values=c("blue", "black"))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "Female_disp_abbrev","AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generation/yr", "Feeding style", "Habit", "Respiration", "Max. body size", "Flow preference", "Thermal preference", "Female dispersal","Flying strength", "Emergence season", "Emergence synchrony"))
traits2<-traits+theme_classic()+ylab("Number of Genera")+xlab("Trait Category")+theme(axis.title.x=element_text(size=30), axis.title.y=element_text(size=30), axis.text.x=element_text(size=25, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits2+ theme(legend.position="top",legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))
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
traits_legend2<-traits_legend+theme_classic()+ylab("Number of Genera")+xlab("Trait Category")+theme(axis.title.x=element_text(size=30), axis.title.y=element_text(size=30), axis.text.x=element_text(size=25, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits_legend2+ theme(legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))

pdf('~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/axes_Genus_traits.pdf', width=8, height=6.811)
traits_legend2+ theme(legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))
dev.off()
