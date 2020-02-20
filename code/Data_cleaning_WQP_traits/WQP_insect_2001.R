###########Initial Cleaning of USEPA Traits Database
##Author: Laura Twardochleb

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits")

#Load workspace image
load(file='WQP_insect_2001.RData')

###############start here- read in insect and trait data#################################################################
insects3<-read.csv("insects2001v3.csv") #WQP insect data

#getting a vector of insect orders
Order<-unique(insects3$order)
Family<-unique(insects3$family)
Genus<-unique(insects3$genus)

#############read in the USEPA trait database and add taxa from WQP database not currently represented in trait database##############################################################################
biotraits<-read.csv("freshwater_biotraits_transposed.csv", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
citation<-unique(biotraits$Study_Citation_abbrev)
write.csv(citation, "epa_citations.csv")

names(biotraits)
colnames(biotraits)[2]<-"SubjectTaxonomicName"
unique(biotraits$Order)
length(unique(biotraits$Genus)) #1,343 unique macroinvertebrate genera

#How many were insect genera?
biotraits_insects<-subset(biotraits, Order=="Hemiptera"| Order == "Coleoptera" | Order == "Odonata"| Order =="Trichoptera"| Order =="Ephemeroptera"| Order == "Diptera"| Order =="Megaloptera"| Order == "Plecoptera")
genus<-subset(biotraits_insects, !is.na(biotraits_insects$Genus)) #908 insect genera
length(unique(genus$Genus))

#What orders are present? Hemiptera and Heteroptera are listed as separate orders, but are in fact the same order. Need to combine. 
#Order appears to not really indicate order

#remove columns that contain only NAs from trait database- this shifted the database around
#need to assign NAs to all cells that have blank spaces
biotraits2<-biotraits[, colSums(is.na(biotraits)) != nrow(biotraits)] 
names(biotraits2)
#how many after removing all NA occurrences
biotraits_insects2<-subset(biotraits2, Order=="Hemiptera"| Order == "Coleoptera" | Order == "Odonata"| Order =="Trichoptera"| Order =="Ephemeroptera"| Order == "Diptera"| Order =="Megaloptera"| Order == "Plecoptera")
genus2<-subset(biotraits_insects2, !is.na(biotraits_insects2$Genus)) #908 insect genera
length(unique(genus2$Genus))

#subset database to traits that we are interested in- need to add in body size trait, ability to exit
biotraits3<-biotraits2[,c("TSN", "SubjectTaxonomicName", "Genus","Family", "Order", "TraitRecord_ID", "Study_Citation_abbrev", "Study_Citation", "Published", "Study_location_state", "Study_location_county", "Study_location_region", "Study_latitude", "Study_longitude","Study_dates", "Adult", "Data_entry", "Data_entry_date", "Primary_WB_type", "Emerge_synch_abbrev", "Emerge_synch", "Emerge_synch_comments", "Emerge_season_1", "Emerge_season_2", "Emerge_season_comments", "Feed_prim_abbrev", "Feed_mode_prim", "Feed_mode_sec", "Feed_mode_comments", "Adult_disp", "Female_disp_abbrev", "Female_disp_comments", "AdultFlyingStrength_abbrev", "AdultFlyingStrength_comments", "Voltinism_abbrev", "Voltinism", "Volt_comments", "Thermal_pref", "Habit_prim_abbrev", "Habit_prim", "Habit_sec", "Habit_comments", "Rheophily_abbrev", "Rheophily_comments", "Max_body_size_abbrev", "Max_body_size", "Resp_abbrev", "Resp_early", "Resp_late", "Resp_adult", "Resp_comments")]
write.csv(biotraits3, "biotraits3.csv") #writing csv file for Ethan to work from- all taxa in USEPA

#rename Heteroptera to Hemiptera under "Order" column
library("plyr")
revalue(biotraits3$Order[biotraits3$Order == "Heteroptera"] <- "Hemiptera")
#checking to see if all Heteroptera were converted
library("dplyr")
Heteroptera1 <- filter(biotraits3, Order == "Heteroptera")

#subset insect database to only taxonomic information
names(insects3)
insects4<-insects3[,c("genus", "family", "order")]
colnames(insects4)[1:3]<-c("Genus", "Family", "Order")

#subset insects database (from WQP) to only taxa not found in biotraits (USEPA) database
biotraits6<-insects4%>% #taxa in WQP not found in USEPA
  anti_join(biotraits3)%>%
  distinct(Genus, Family, .keep_all = TRUE)
biotraits_n<-biotraits6[which(!is.na(biotraits6$Genus)),] #we added 171 insect genera, but some were terrestrial
#returns Genus, Family, and Order for Genera and Families taxa not in biotraits database
#keep only distinct taxa in SubjectTaxonomicName- 243 Genera/Families not found in biotraits database

######################location where some traits were removed that were in original database- compare Pyne traits to these to get number of additional genera added###########################################
#subset biotraits database to only taxa found in WQP database- then merge with biotraits6 to get the full complement of records we want
biotraits7<-biotraits3%>% #taxa in USEPA found in WQP
  semi_join(insects4)

#insect taxa in USEPA not found in WQP
epa<-biotraits3%>%
  anti_join(insects4)%>%
  filter(Order=="Ephemeroptera"|Order=="Trichoptera"|Order=="Odonata"|Order=="Plecoptera"|Order=="Hemiptera"|Order=="Lepidoptera"|Order=="Megaloptera"|Order=="Coleoptera"|Order=="Diptera")
#subset to insects only
length(unique(epa$Genus)) #212 insect Genera not in WQP data
#write to file
write.csv(epa, "USEPA_only_insects.csv")

#need to merge biotraits6 and biotraits7, adding in the genera from WQP not currently found in traits database
#then subset by orders of interest->Coleoptera, Odonata, Trichoptera, Ephemeroptera, Diptera, Megaloptera, Hemiptera, Plecoptera
biotraits8<-biotraits6%>% #taxa in WQP not in UEPA
  full_join(biotraits7)%>% #taxa in USEPA also in WQP
  filter(Order == "Hemiptera"| Order == "Coleoptera" | Order == "Odonata"| Order =="Trichoptera"| Order =="Ephemeroptera"| Order == "Diptera"| Order =="Megaloptera"| Order == "Plecoptera") #%>%
#11107 observations of orders of interest
length(unique(biotraits8$Genus))

write.csv(biotraits8, "biotraits8.csv") #write to file for Ethan to work from

######Create new column for voltinism traits that assign what is in either voltinism or voltinism_abbrev if there is only one entry###########################################################################
#Using the subset function to see if any Volt or Volt_abbrev don't match up
biotraits_volt<-subset(biotraits8, Voltinism_abbrev!="NA" & Voltinism!="NA")
biotraits_volt1<-subset(biotraits_volt, Voltinism_abbrev == "bi_multivoltine" & Voltinism != "> 1 Generation per year" | Voltinism_abbrev != "bi_multivoltine" & Voltinism == "> 1 Generation per year")
biotraits_volt2<-subset(biotraits_volt, Voltinism_abbrev == "univoltine" & Voltinism != "1 Generation per year" | Voltinism_abbrev != "univoltine" & Voltinism == "1 Generation per year")
biotraits_volt3<-subset(biotraits_volt, Voltinism_abbrev == "semivoltine" & Voltinism != "< 1 Generation per year" | Voltinism_abbrev != "semivoltine" & Voltinism == "< 1 Generation per year")

biotraits8$Volt_new <- biotraits8$Voltinism_abbrev 
biotraits8$Volt_new <-biotraits8$Voltinism
levels(biotraits8$Voltinism) <-c(levels(biotraits8$Voltinism), "bi_multivoltine")
biotraits8$Voltinism[biotraits8$Voltinism == '> 1 Generation per year'] <- 'bi_multivoltine'
levels(biotraits8$Voltinism) <-c(levels(biotraits8$Voltinism), "semivoltine")
biotraits8$Voltinism[biotraits8$Voltinism == '< 1 Generation per year'] <- 'semivoltine'
levels(biotraits8$Voltinism) <-c(levels(biotraits8$Voltinism), "univoltine")
biotraits8$Voltinism[biotraits8$Voltinism == '1 Generation per year'] <- 'univoltine'

#this code works to combine voltinism and voltinism abbreviated columns into a new column
biotraits8$Volt_new<-biotraits8$Voltinism_abbrev
biotraits8$Volt_new[is.na(biotraits8$Voltinism_abbrev)]<-biotraits8$Voltinism[is.na(biotraits8$Voltinism_abbrev)]

#need to drop old voltinism comments
biotraits8$Voltinism<-NULL
biotraits8$Voltinism_abbrev<-NULL

#######Combine Feed_prim_abbrev and Feed_prim in the same way as voltinism and voltinism_abbrev###############################
unique(biotraits8$Feed_mode_prim)

levels(biotraits8$Feed_mode_prim) <-c(levels(biotraits8$Feed_mode_prim), "PR")
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'Predator'] <- 'PR'
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'predator'] <- 'PR'
levels(biotraits8$Feed_mode_prim) <-c(levels(biotraits8$Feed_mode_prim), "CG")
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'Collector-gatherer'] <- 'CG'
levels(biotraits8$Feed_mode_prim) <-c(levels(biotraits8$Feed_mode_prim), "HB")
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'Scraper/grazer'] <- 'HB'
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'Herbivore'] <- 'HB'
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'Piercer herbivore'] <- 'HB'
levels(biotraits8$Feed_mode_prim) <-c(levels(biotraits8$Feed_mode_prim), "SH")
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'Shredder'] <- 'SH'
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'shredder'] <- 'SH'
levels(biotraits8$Feed_mode_prim) <-c(levels(biotraits8$Feed_mode_prim), "CF")
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'Collector-filterer'] <- 'CF'
levels(biotraits8$Feed_mode_prim) <-c(levels(biotraits8$Feed_mode_prim), "PS")
biotraits8$Feed_mode_prim[biotraits8$Feed_mode_prim == 'Parasite'] <- 'PS'

biotraits8$Feed_mode_new<-biotraits8$Feed_prim_abbrev
biotraits8$Feed_mode_new[is.na(biotraits8$Feed_prim_abbrev)]<-biotraits8$Feed_mode_prim[is.na(biotraits8$Feed_prim_abbrev)]

biotraits8$Feed_mode_prim<-NULL
biotraits8$Feed_prim_abbrev<-NULL

#######Combine Habit_prim_abbrev and Habit_prim in the same way as voltinism and voltinism_abbrev###############################
unique(biotraits8$Habit_prim)

levels(biotraits8$Habit_prim_abbrev) <-c(levels(biotraits8$Habit_prim_abbrev), "Sprawler")
biotraits8$Habit_prim_abbrev[biotraits8$Habit_prim_abbrev == 'SP'] <- 'Sprawler'
levels(biotraits8$Habit_prim_abbrev) <-c(levels(biotraits8$Habit_prim_abbrev), "Clinger")
biotraits8$Habit_prim_abbrev[biotraits8$Habit_prim_abbrev == 'CN'] <- 'Clinger'
levels(biotraits8$Habit_prim_abbrev) <-c(levels(biotraits8$Habit_prim_abbrev), "Burrower")
biotraits8$Habit_prim_abbrev[biotraits8$Habit_prim_abbrev == 'BU'] <- 'Burrower'
levels(biotraits8$Habit_prim_abbrev) <-c(levels(biotraits8$Habit_prim_abbrev), "Swimmer")
biotraits8$Habit_prim_abbrev[biotraits8$Habit_prim_abbrev == 'SW'] <- 'Swimmer'
levels(biotraits8$Habit_prim_abbrev) <-c(levels(biotraits8$Habit_prim_abbrev), "Skater")
biotraits8$Habit_prim_abbrev[biotraits8$Habit_prim_abbrev == 'SK'] <- 'Skater'
levels(biotraits8$Habit_prim_abbrev) <-c(levels(biotraits8$Habit_prim_abbrev), "Climber")
biotraits8$Habit_prim_abbrev[biotraits8$Habit_prim_abbrev == 'CB'] <- 'Climber'

biotraits8$Habit_new<-biotraits8$Habit_prim_abbrev
biotraits8$Habit_new[is.na(biotraits8$Habit_prim_abbrev)]<-biotraits8$Habit_prim[is.na(biotraits8$Habit_prim_abbrev)]

biotraits8$Habit_prim_abbrev<-NULL
biotraits8$Habit_prim<-NULL


write.csv(biotraits8, "biotraits8_11_30_17.csv")
#####counting number of unique traits by Genus#####
counts<-biotraits %>% group_by(Genus) %>% summarize(count=n_distinct(Feed_prim_abbrev, na.rm = TRUE))
counts


############Save the data############################################################################################
save.image(file="WQP_insect_2001.RData")


############Prep map data for uploading to HPCC###########################################################################
#subset to create a csv file with only the site lat and lon coordinates and the HUC8 codes
huc_site2001<-cbind(insects3$HUCEightDigitCode, insects3$LatitudeMeasure, insects3$LongitudeMeasure)
head(huc_site2001)
write.csv(huc_site2001, file="site_by_huc2001.csv") #written to WQP_insect_final

##############old code################################################################################################
odonate_traits<-subset(biotraits, Order=="Odonata")

#merge odonates occurrence data with trait data
odonates_2001<-merge(odonate_traits, odonates, by= "SubjectTaxonomicName", all.x = TRUE, all.y = TRUE)

write.csv(odonates_2001, "odonates.csv")
