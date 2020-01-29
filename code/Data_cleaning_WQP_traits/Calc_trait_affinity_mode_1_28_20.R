#Laura Twardochleb
#This script calculates trait modal values and affinity scores and creates tables to be published online by EDI and with data paper

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits")

#Load workspace image
load(file='Calc_trait_affinity_mode.RData')

#read-in data
biotraits7.4<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/biotraits_wide2.csv")
epa<-read.csv("epa_traits.csv", stringsAsFactors = FALSE)

#load packages
library(tidyverse)
library(reshape2)

################## Summarize trait mode #############################################################
#combine epa and biotraits tables
biotraits7.5<-biotraits7.4[,-c(1,2,21,25)]
names(biotraits7.5)
names(epa)
epa2<-epa %>% select(c("Study_Citation_abbrev", "SubjectTaxonomicName","AdultFlyingStrength_abbrev","Emerge_season_1", "Emerge_season_2", "Emerge_season_comments","Emerge_synch_abbrev","Family","Feed_mode_comments","Feed_mode_sec","Feed_prim_abbrev","Female_disp_abbrev", "Genus", "Habit_comments","Habit_prim_abbrev","Habit_sec","Max_body_size_abbrev", "Order", "Resp_abbrev","Resp_comments","Rheophily_abbrev","Study_location_state","Thermal_pref", "TSN","Volt_comments",  "Voltinism_abbrev","acceptedtsn","Accepted_name", "Study_Citation"))
epa2$Taxonomic_resolution<-NA
epa2$Taxonomic_resolution<-ifelse(as.character(epa2$Accepted_name)==as.character(epa2$Genus),"Genus", NA)
epa2$Taxonomic_resolution<-ifelse(as.character(epa2$Accepted_name)==as.character(epa2$Family),"Family", epa2$Taxonomic_resolution)
epa2$Taxonomic_resolution<-ifelse(as.character(epa2$Accepted_name)==as.character(epa2$Order),"Order", epa2$Taxonomic_resolution)
epa2$Taxonomic_resolution<-ifelse(as.character(epa2$Accepted_name)==as.character(epa2$Order),"Order", epa2$Taxonomic_resolution)
#remove name in parentheses from Genus and Accepted_name
unique(epa2$Accepted_name)
epa2$Accepted_name[which(epa2$Accepted_name=="Gomphus (Gomphurus)")]<-"Gomphus"
epa2$Accepted_name[which(epa2$Accepted_name=="Gomphus (Hylogomphus)")]<-"Gomphus"
epa2$Accepted_name[which(epa2$Accepted_name=="Epitheca (Epicordulia)")]<-"Epitheca"
epa2$Accepted_name[which(epa2$Accepted_name=="Hydrophilus (Dibolocelus)")]<-"Hydrophilus"
epa2$Accepted_name[which(epa2$Accepted_name=="Epitheca (Tetragoneuria)")]<-"Epitheca"

epa2$Taxonomic_resolution[which(epa2$Accepted_name=="Cricotopus (Nostococladius)")]<-"Genus"
epa2$Taxonomic_resolution<-ifelse(is.na(epa2$Taxonomic_resolution),"Species", epa2$Taxonomic_resolution)

names(epa2)
write.csv(epa2, "epa_cleaned.csv")

names(biotraits7.5)
colnames(epa2)<-colnames(biotraits7.5)

biotraits7.6<-rbind(epa2, biotraits7.5)
write.csv(biotraits7.6, "biotraits7.6.csv")

#first convert any with trait "other" to NA
biotraits7.6$Feed_prim_abbrev[which(biotraits7.6$Feed_prim_abbrev=="Other (specify in comments)")]<-NA
biotraits7.6$Feed_mode_sec[which(biotraits7.6$Feed_mode_sec=="Other (specify in comments)")]<-NA
biotraits7.6$Habit_prim[which(biotraits7.6$Habit_prim=="Other (specify in comments)")]<-NA
biotraits7.6$Habit_sec[which(biotraits7.6$Habit_sec=="Other (specify in comments)")]<-NA
# For now, use the most common trait value (mode) per taxon
mode_fn <- function(x) ifelse(any(!is.na(x)), names(which.max(table(x))), as.character(NA))

count_mode <- biotraits7.6 %>% filter(!is.na(Genus)) %>%group_by(Genus) %>%summarize_all(mode_fn)

################### Prep trait mode table for EDI ##################################################
#remove comments columns
count_mode3<-count_mode[,-grep(pattern="comments", colnames(count_mode))] 

#create ancillary taxonomy table- need to add taxa from occurrence table, also
names(count_mode3)
ancillary_taxonomy<-select(count_mode3,c("SubjectTaxonomicName","Accepted_Name", "Accepted_TSN", "Order", "Family", "Genus"))
colnames(ancillary_taxonomy)<-c("Submitted_name", "Accepted_name","Accepted_TSN", "Order", "Family", "Genus")
#create species column
attach(ancillary_taxonomy)
ancillary_taxonomy$Species<-NA
ancillary_taxonomy$Species<-if_else(Accepted_name!=Genus, Accepted_name, as.character(NA))
detach(ancillary_taxonomy)
write.csv(ancillary_taxonomy, "ancillary_taxonomy_table.csv") #ancillary taxonomy with just trait taxa

#merge ancillary_taxonomy for traits and occurrence taxa
occurrence_taxa<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/ancillary_taxonomy_occurrence.csv", stringsAsFactors = FALSE)
occurrence_taxa2<-unique(occurrence_taxa[,-1])
occurrence_taxa2$Accepted_TSN<-as.character(occurrence_taxa2$Accepted_TSN)

#assign missing accepted names to occurrence table- then check & revise in the master and genus occurrence tables
#first change Submitted_name to sentence case
occurrence_taxa3<-occurrence_taxa2 %>% mutate(Submitted_name = str_to_sentence(Submitted_name))

occurrence_taxa3$Accepted_name<-if_else(is.na(occurrence_taxa3$Accepted_name)&occurrence_taxa3$Submitted_name==occurrence_taxa3$Genus, occurrence_taxa3$Genus, occurrence_taxa3$Accepted_name)
occurrence_taxa3$Accepted_name<-if_else(is.na(occurrence_taxa3$Accepted_name)&occurrence_taxa3$Submitted_name==occurrence_taxa3$Species, occurrence_taxa3$Species, occurrence_taxa3$Accepted_name)
occurrence_taxa3$Accepted_name<-if_else(is.na(occurrence_taxa3$Accepted_name)&occurrence_taxa3$Submitted_name==occurrence_taxa3$Family, occurrence_taxa3$Family, occurrence_taxa3$Accepted_name)
occurrence_taxa3$Accepted_name<-if_else(is.na(occurrence_taxa3$Accepted_name)&occurrence_taxa3$Submitted_name==occurrence_taxa3$Order, occurrence_taxa3$Order, occurrence_taxa3$Accepted_name)
occurrence_taxa4<-unique(occurrence_taxa3)

ancillary_taxonomy_master<-full_join(ancillary_taxonomy, occurrence_taxa4)

#write master ancillary taxonomy table
write.csv(ancillary_taxonomy_master, "ancillary_taxonomy_table_master.csv")

#subset mode table to columns we want in final table
count_mode4<-select(count_mode3,-c("Study_Citation_abbrev", "SubjectTaxonomicName", "Family", "Order", "Study_location_state", "Original_TSN", "Accepted_TSN", "Accepted_Name", "Study_Citation", "Taxonomic_resolution"))

#convert trait mode table to long format
keycol=c("Trait_group")
valuecol=c("Trait")
gathercol<-c("AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_synch_abbrev", "Feed_prim_abbrev", "Feed_mode_sec", "Female_disp_abbrev", "Habit_prim", "Habit_sec", "Max_body_size_abbrev", "Resp_abbrev", "Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev")

trait_mode.long<- count_mode4%>%gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments
trait_mode.long2<-trait_mode.long[order(trait_mode.long$Genus),]

#csv of trait mode table for EDI
write.csv(trait_mode.long2, "~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/trait_mode_table.csv")

################## Calculate affinity scores #######################################################
str(biotraits7.6) #all traits are factors
unique(biotraits7.6$Emerge_season_1) #levels consistent- ignore error in reshape2 function about diff. levels
unique(biotraits7.6$Emerge_season_2)
unique(biotraits7.6$Voltinism_abbrev)
unique(biotraits7.6$Thermal_pref)
unique(biotraits7.6$Habit_prim)
unique(biotraits7.6$Habit_sec)
unique(biotraits7.6$AdultFlyingStrength_abbrev)
unique(biotraits7.6$Max_body_size_abbrev)
unique(biotraits7.6$Resp_abbrev)
unique(biotraits7.6$Rheophily_abbrev)
unique(biotraits7.6$Emerge_synch_abbrev)
unique(biotraits7.6$Female_disp_abbrev)

#reshape table into long format
traits.long<- biotraits7.6%>%filter(!is.na(Genus))%>%gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments

affinities<-traits.long%>%filter(!is.na(Trait))%>%
  group_by(Genus, Trait_group, Trait)%>%
  tally%>%
  mutate(Percent = n / sum(n))

#prep for EDI
affinities2<-select(affinities, -"n")
colnames(affinities2)<-c("Genus", "Trait_group", "Trait", "Trait_affinity")

#csv of trait affinities table for EDI
write.csv(affinities2, "~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/trait_affinities_table.csv")

################## Prep raw trait table to for hosting by EDI #####################################################
names(biotraits7.6)

#select columns
biotraits7.7<-select(biotraits7.6, -"Accepted_TSN")

#convert to long format- include trait comments
gathercol2<-c("AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_season_comments" ,"Emerge_synch_abbrev", "Feed_prim_abbrev", "Feed_mode_sec", "Feed_mode_comments","Female_disp_abbrev", "Habit_prim", "Habit_sec", "Habit_comments","Max_body_size_abbrev", "Resp_abbrev", "Resp_comments","Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev", "Volt_comments")

traits.long.raw<- biotraits7.7%>%gather_(keycol, valuecol, gathercol2) #create a column "trait" that holds all trait assignments

#rename columns
colnames(traits.long.raw)<-c("Study_citation_abbrev", "Submitted_name_trait", "Family", "Genus", "Order", "Study_location_state", "Submitted_TSN", "Accepted_name", "Study_citation", "Taxonomic_resolution", "Trait_group", "Trait")

#drop unneeded columns
traits.long.raw2<-traits.long.raw[,-c(3,4,5,8)]

#reorder columns
traits.long.raw2<-traits.long.raw2[,c(6,2,4,7,8,1,5,3)]

#.csv of raw_trait_table for EDI
write.csv(traits.long.raw2, "raw_trait_table_EDI.csv")

write.csv(biotraits7.7, "biotraits_wide3.csv")

save.image("Calc_trait_affinity_mode.RData")

