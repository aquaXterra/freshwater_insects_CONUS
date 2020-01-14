#Laura Twardochleb
#This script calculates trait modal values and affinity scores and creates tables to be published online by EDI and with data paper

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits")

#read-in data
biotraits7.4<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/biotraits_wide2.csv")

#load packages
library(tidyverse)
library(reshape2)

################## Summarize trait mode #############################################################
#first convert any with trait "other" to NA
biotraits7.4$Feed_prim_abbrev[which(biotraits7.4$Feed_prim_abbrev=="Other (specify in comments)")]<-NA
biotraits7.4$Feed_mode_sec[which(biotraits7.4$Feed_mode_sec=="Other (specify in comments)")]<-NA
biotraits7.4$Habit_prim[which(biotraits7.4$Habit_prim=="Other (specify in comments)")]<-NA
biotraits7.4$Habit_sec[which(biotraits7.4$Habit_sec=="Other (specify in comments)")]<-NA
# For now, use the most common trait value (mode) per taxon
mode_fn <- function(x) ifelse(any(!is.na(x)), names(which.max(table(x))), as.character(NA))

count_mode <- biotraits7.4 %>% filter(!is.na(Genus)) %>%group_by(Genus) %>%summarize_all(mode_fn)

################### Prep trait mode table for EDI ##################################################
count_mode2<-count_mode[,-c(2,3)]

#remove comments columns
count_mode3<-count_mode2[,-grep(pattern="comments", colnames(count_mode2))] 

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
count_mode4<-select(count_mode3,-c("Study_Citation_abbrev", "SubjectTaxonomicName", "Family", "Order", "Published", "Study_location_region", "Study_location_state", "Original_TSN", "Accepted_TSN", "Accepted_Name", "Study_Citation", "Taxonomic_resolution"))

#convert trait mode table to long format
keycol=c("Trait_group")
valuecol=c("Trait")
gathercol<-c("AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_synch_abbrev", "Feed_prim_abbrev", "Feed_mode_sec", "Female_disp_abbrev", "Habit_prim", "Habit_sec", "Max_body_size_abbrev", "Resp_abbrev", "Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev")

trait_mode.long<- count_mode4%>%gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments
trait_mode.long2<-trait_mode.long[order(trait_mode.long$Genus),]

#csv of trait mode table for EDI
write.csv(trait_mode.long2, "~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/trait_mode_table.csv")

################## Calculate affinity scores #######################################################
str(biotraits7.4) #all traits are factors
unique(biotraits7.4$Emerge_season_1) #levels consistent- ignore error in reshape2 function about diff. levels
unique(biotraits7.4$Emerge_season_2)
unique(biotraits7.4$Voltinism_abbrev)
unique(biotraits7.4$Thermal_pref)
unique(biotraits7.4$Habit_prim)
unique(biotraits7.4$Habit_sec)
unique(biotraits7.4$AdultFlyingStrength_abbrev)
unique(biotraits7.4$Max_body_size_abbrev)
unique(biotraits7.4$Resp_abbrev)
unique(biotraits7.4$Rheophily_abbrev)
unique(biotraits7.4$Emerge_synch_abbrev)
unique(biotraits7.4$Female_disp_abbrev)

#reshape table into long format
traits.long<- biotraits7.4%>%filter(!is.na(Genus))%>%gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments

#check that warming message makes no difference- TRUE
biotraits7.4.1<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/biotraits_wide2.csv", stringsAsFactors = FALSE)
traits.long.1<- biotraits7.4.1%>%filter(!is.na(Genus))%>%gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments
length(is.na(traits.long.1$Trait))
length(is.na(traits.long$Trait))

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
names(biotraits7.4)

#select columns
biotraits7.5<-select(biotraits7.4, -c("X.1", "X", "Published", "Study_location_region", "Accepted_TSN"))

#convert to long format- include trait comments
gathercol2<-c("AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_season_comments" ,"Emerge_synch_abbrev", "Feed_prim_abbrev", "Feed_mode_sec", "Feed_mode_comments","Female_disp_abbrev", "Habit_prim", "Habit_sec", "Habit_comments","Max_body_size_abbrev", "Resp_abbrev", "Resp_comments","Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev", "Volt_comments")

traits.long.raw<- biotraits7.5%>%gather_(keycol, valuecol, gathercol2) #create a column "trait" that holds all trait assignments

#rename columns
colnames(traits.long.raw)<-c("Study_citation_abbrev", "Submitted_name_trait", "Family", "Genus", "Order", "Study_location_state", "Submitted_TSN", "Accepted_name", "Study_citation", "Taxonomic_resolution", "Trait_group", "Trait")

#drop unneeded columns
traits.long.raw2<-traits.long.raw[,-c(3,4,5)]

#reorder columns
traits.long.raw2<-traits.long.raw2[,c(7,2,4,5,8,9,1,6,3)]

#.csv of raw_trait_table for EDI
write.csv(traits.long.raw2, "raw_trait_table_EDI.csv")

save.image("Calc_trait_affinity_mode.R")
