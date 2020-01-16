#Laura Twardochleb
#This script cleans entries in USEPA database that were not in WQP database

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits")

epa<-read.csv("USEPA_only_insects.csv", stringsAsFactors = FALSE)

library(plyr)
library(tidyverse)
library(reshape2)

#select columns of interest
epa2<-select(epa, -c("X", "TraitRecord_ID", "Published", "Study_location_county", "Study_location_region","Study_latitude", "Study_longitude", "Emerge_synch", "Study_dates", "Adult", "Data_entry", "Data_entry_date", "Primary_WB_type"))

## Data Cleaning
#replacing all rows containing Heteroptera as the Order wih Hemiptera
revalue(epa2$Order[epa2$Order == "Heteroptera"] <- "Hemiptera")
revalue(epa2$Voltinism_abbrev[epa2$Voltinism_abbrev == "semivoltine"] <- "Semivoltine")
epa2$Voltinism_abbrev[epa2$Voltinism_abbrev == " "] <- NA
epa2$Voltinism_abbrev[epa2$Voltinism_abbrev == 'NA'] <- NA
revalue(epa2$Voltinism_abbrev[epa2$Voltinism_abbrev == "bi_multivoltine"] <- "Bi_multivoltine")
revalue(epa2$Voltinism_abbrev[epa2$Voltinism_abbrev == "BI_multivoltine"] <- "Bi_multivoltine")
revalue(epa2$Voltinism_abbrev[epa2$Voltinism_abbrev == "univoltine"] <- "Univoltine")
revalue(epa2$Voltinism_abbrev[epa2$Voltinism_abbrev == "Univoltine "] <- "Univoltine")
revalue(epa2$Voltinism_abbrev[epa2$Voltinism_abbrev == "multivoltine"] <- "Bi_multivoltine")
revalue(epa2$Thermal_pref[epa2$Thermal_pref == "cold-cool eurythermal (0-15 C)"] <- "Cold-cool eurythermal (0-15 C)")
revalue(epa2$Thermal_pref[epa2$Thermal_pref == "warm eurythermal (15-30 C"] <- "Warm eurythermal (15-30 C)")
revalue(epa2$Thermal_pref[epa2$Thermal_pref == "Warm eurythermal (15-30 C"] <- "Warm eurythermal (15-30 C)")
revalue(epa2$Thermal_pref[epa2$Thermal_pref == "Warm eurythermal (15-30 C) "] <- "Warm eurythermal (15-30 C)")
revalue(epa2$Habit_prim[epa2$Habit_prim == "clinger"] <- "Clinger")
revalue(epa2$Habit_prim[epa2$Habit_prim == "Clinger "] <- "Clinger")
revalue(epa2$Habit_prim[epa2$Habit_prim == "Clingers"] <- "Clinger")
revalue(epa2$Habit_prim[epa2$Habit_prim == "Clingers "] <- "Clinger")
revalue(epa2$Habit_prim[epa2$Habit_prim == "Sprawler "] <- "Sprawler")
revalue(epa2$Habit_prim[epa2$Habit_prim == "burrower"] <- "Burrower")
revalue(epa2$Habit_prim[epa2$Habit_prim == "Burrower "] <- "Burrower")
revalue(epa2$Habit_prim[epa2$Habit_prim == "swimmer"] <- "Swimmer")
revalue(epa2$Habit_prim[epa2$Habit_prim == "Swimmer "] <- "Swimmer")
epa2$Habit_prim[epa2$Habit_prim == 'NA'] <- NA
revalue(epa2$Resp_abbrev[epa2$Resp_abbrev == "Gillls"] <- "Gills")
revalue(epa2$Resp_abbrev[epa2$Resp_abbrev == "gils"] <- "Gills")
revalue(epa2$Resp_abbrev[epa2$Resp_abbrev == "gills"] <- "Gills")
revalue(epa2$Resp_abbrev[epa2$Resp_abbrev == "Gills "] <- "Gills")
revalue(epa2$Resp_abbrev[epa2$Resp_abbrev == "tegument "] <- "Tegument")
revalue(epa2$Resp_abbrev[epa2$Resp_abbrev == "tegument"] <- "Tegument")
revalue(epa2$Resp_abbrev[epa2$Resp_abbrev == "Tegument "] <- "Tegument")
revalue(epa2$Max_body_size_abbrev[epa2$Max_body_size_abbrev == "large"] <- "Large")
revalue(epa2$Max_body_size_abbrev[epa2$Max_body_size_abbrev == "medium"] <- "Medium")
revalue(epa2$Max_body_size_abbrev[epa2$Max_body_size_abbrev == "Medium "] <- "Medium")
revalue(epa2$Max_body_size_abbrev[epa2$Max_body_size_abbrev == "small"] <- "Small")
revalue(epa2$Resp_abbrev[epa2$Resp_abbrev == "plastron_spiracle"] <- "Plastron_spiracle")
revalue(epa2$Emerge_synch_abbrev[epa2$Emerge_synch_abbrev == "poorly"] <- "Poorly")
revalue(epa2$Emerge_synch_abbrev[epa2$Emerge_synch_abbrev == "well"] <- "Well")
revalue(epa2$AdultFlyingStrength_abbrev[epa2$AdultFlyingStrength_abbrev == "strong"] <- "Strong")
revalue(epa2$AdultFlyingStrength_abbrev[epa2$AdultFlyingStrength_abbrev == "weak"] <- "Weak")
revalue(epa2$Emerge_season_1[epa2$Emerge_season_1 == "Spring "] <- "Spring")
revalue(epa2$Emerge_season_1[epa2$Emerge_season_1 == "Summer "] <- "Summer")
revalue(epa2$Feed_prim_abbrev[epa2$Feed_prim_abbrev == "PR "] <- "PR")

##########Additional data cleaning- examine levels for each variable###############
#Assign Feed_prim_abbrev based on comments
unique(epa2$Feed_prim_abbrev)
feed<-epa2[which(is.na(epa2$Feed_prim_abbrev) & !is.na(epa2$Feed_mode_prim)),]
revalue(epa2$Feed_prim_abbrev[is.na(epa2$Feed_prim_abbrev)& epa2$Feed_mode_prim=="Piercer herbivore"] <- "HB")
revalue(epa2$Feed_prim_abbrev[is.na(epa2$Feed_prim_abbrev)& epa2$Feed_mode_comments=="Feed on microorganisms, algae, and detritus; some species are predaceous."] <- "CG")
revalue(epa2$Feed_prim_abbrev[is.na(epa2$Feed_prim_abbrev)& epa2$Feed_mode_comments=="Feed on submerged foliage of rice plants."] <- "HB")
revalue(epa2$Feed_prim_abbrev[is.na(epa2$Feed_prim_abbrev)& epa2$Feed_mode_comments=="Adults are scavengers."] <- "CG")

other<-epa2[which(!is.na(epa2$Feed_mode_comments) & is.na(epa2$Feed_prim_abbrev)),]
revalue(epa2$Feed_prim_abbrev[is.na(epa2$Feed_prim_abbrev)& epa2$Feed_mode_comments=="Herbivore; feed on algae."] <- "HB")
revalue(epa2$Feed_prim_abbrev[is.na(epa2$Feed_prim_abbrev)& epa2$Feed_mode_comments=="Sucking mouth parts; primarily detritivores."] <- "CG")

#add resp abbrev from comments and resp_early, resp_late! Need to decide how to assign for juveniles vs. adults- can fuzzy code for early and late as well as by genus
epa2[which(is.na(epa2$Resp_abbrev)&!is.na(epa2$Resp_early)),]

epa2[which(epa2$Resp_early == "Atmospheric breathers"), ] #assign to Plastron/spiracle for (juveniles) Resp_abbrev based on Merritt, Cummins, Berg? Also for fuzzy coding?
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_early == "Atmospheric breathers")] = "Plastron_spiracle"

epa2[which(epa2$Resp_early == "Cutaneous"), ] #assign to tegument  for juveniles for Resp_abbrev based on Merritt, Cummins, Berg. Also for fuzzy coding?
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_early == "Cutaneous")] <- "Tegument"

#assign temporary air store in Resp_early to plastron/spiracle in Resp_abbrev
epa2[which(epa2$Resp_early == "Temporary air store"), ] 
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_early == "Temporary air store")] = "Plastron_spiracle"
#assign Tracheal gills in Resp_early to Resp_abbrev (resp_early and resp_late are consistent between these two categories)
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_early == "Tracheal gills")] = "Gills" #assign to gills based on Merritt, Cummins, Berg
epa2[which(epa2$Resp_early == "Spiracular gills"), ] #applies only to three aquatic beetle taxa
#assign Spiracular gills in Resp_early to Plastron_spiracle in Resp_abbrev (resp_early and resp_late are consistent between these two categories)
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_early == "Spiracular gills")] <-"Plastron_spiracle" # assign to plastron_spiracle based on Merritt, Cummins, Berg
epa2[which(epa2$Resp_early == "Plant breathers"), ] # assign to plastron_spiracle based on Merritt, Cummins, Berg
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_early == "Plant breathers")] <-"Plastron_spiracle"
epa2[which(epa2$Resp_early == "Gills"), ] #assign to gills for Resp_abbrev
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_early == "Gills")] <-"Gills"
epa2[which(is.na(epa2$Resp_abbrev)&!is.na(epa2$Resp_late)),]
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_late == "Tracheal gills")] <-"Gills"
epa2[which(is.na(epa2$Resp_abbrev)&!is.na(epa2$Resp_adult)),]
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_adult == "Tracheal gills")] <-"Gills"
epa2$Resp_abbrev[which(is.na(epa2$Resp_abbrev) & epa2$Resp_adult == "Temporary air store")] <-"Plastron_spiracle"
unique(epa2$Resp_abbrev)

#check habit entries
unique(epa2$Habit_prim)
unique(epa2$Habit_comments)
epa2$Habit_comments[epa2$Habit_comments=="Habit_prim_abbrev: BU = burrower; CB = climber; CN = clinger; SK = skater; SP = sprawler; SW = swimmer"]<-NA
#assign Habit_prim from comments
epa2[which(!is.na(epa2$Habit_comments)& is.na(epa2$Habit_prim)),]

#flying strength and female dispersal
unique(epa2$Adult_disp)
unique(epa2$Female_disp_abbrev)

epa2[which(is.na(epa2$Female_disp_abbrev)&!is.na(epa2$Adult_disp)),]
epa2$Female_disp_abbrev[which(is.na(epa2$Female_disp_abbrev) & epa2$Adult_disp == "1 km or less")] <-"low"
epa2[which(is.na(epa2$Female_disp_abbrev)&!is.na(epa2$Female_disp_comments)),]

epa2[which(is.na(epa2$Emerge_synch_abbrev) &!is.na(epa2$Emerge_synch_comments)), ]
unique(epa2$Emerge_synch_abbrev)

#Feed mode sec
unique(epa2$Feed_mode_comments)
unique(epa2$Feed_mode_sec) #revalue to macth Feed_prim_abbrev
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="Piercer herbivore"]<-"HB")
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="Scraper/grazer"]<-"HB")
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="Herbivore"]<-"HB")
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="Predator"]<-"PR")
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="Shredder"]<-"SH")
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="Collector-gatherer"]<-"CG")
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="collector-gatherer"]<-"CG")
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="Collector-filterer"]<-"CF")
revalue(epa2$Feed_mode_sec[epa2$Feed_mode_sec=="Parasite"]<-"PA")
epa2[which(is.na(epa2$Female_mode_sec)&!is.na(epa2$Feed_mode_comments)),]

#Voltinism
unique(epa2$Voltinism_abbrev)

epa2[which(is.na(epa2$Voltinism_abbrev)&!is.na(epa2$Voltinism)),]
epa2$Voltinism_abbrev[which(is.na(epa2$Voltinism_abbrev)& epa2$Voltinism=="1 Generation per year")]<-"Univoltine"
epa2$Voltinism_abbrev[which(is.na(epa2$Voltinism_abbrev)& epa2$Voltinism=="< 1 Generation per year")]<-"Semivoltine"
epa2$Voltinism_abbrev[which(is.na(epa2$Voltinism_abbrev)& epa2$Voltinism=="> 1 Generation per year")]<-"Bi_multivoltine"

epa2[which(is.na(epa2$Voltinism_abbrev)&!is.na(epa2$Volt_comments)),]

#Body size
unique(epa2$Max_body_size_abbrev)
epa2[which(is.na(epa2$Max_body_size_abbrev)&!is.na(epa2$Max_body_size)),]

#Emerge season
unique(epa2$Emerge_season_1)
unique(epa2$Emerge_season_2)
epa2[which(is.na(epa2$Emerge_season_1)&!is.na(epa2$Emerge_season_comments)),]
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: spring, summer")]<-"Spring"
epa2$Emerge_season_2[which(is.na(epa2$Emerge_season_2)& epa2$Emerge_season_comments=="sexually mature form reported in: spring, summer")]<-"Summer"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: summer, fall")]<-"Summer"
epa2$Emerge_season_2[which(is.na(epa2$Emerge_season_2)& epa2$Emerge_season_comments=="sexually mature form reported in: summer, fall")]<-"Fall"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: winter, spring")]<-"Winter"
epa2$Emerge_season_2[which(is.na(epa2$Emerge_season_2)& epa2$Emerge_season_comments=="sexually mature form reported in: winter, spring")]<-"Spring"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: spring, summer, fall")]<-"Spring"
epa2$Emerge_season_2[which(is.na(epa2$Emerge_season_2)& epa2$Emerge_season_comments=="sexually mature form reported in: spring, summer, fall")]<-"Fall"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: spring")]<-"Spring"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: summer")]<-"Summer"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: fall")]<-"Fall"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: winter")]<-"Winter"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: winter, spring, summer")]<-"Winter"
epa2$Emerge_season_2[which(is.na(epa2$Emerge_season_2)& epa2$Emerge_season_comments=="sexually mature form reported in: winter, spring, summer")]<-"Summer"
epa2$Emerge_season_1[which(is.na(epa2$Emerge_season_1)& epa2$Emerge_season_comments=="sexually mature form reported in: winter, spring, fall")]<-"Winter"
epa2$Emerge_season_2[which(is.na(epa2$Emerge_season_2)& epa2$Emerge_season_comments=="sexually mature form reported in: winter, spring, fall")]<-"Fall"

#Rheophily
unique(epa2$Rheophily_abbrev)
epa2[which(is.na(epa2$Rheophily_abbrev)&!is.na(epa2$Rheophily_comments)),]
epa2$Rheophily_abbrev[which(is.na(epa2$Rheophily_abbrev)& epa2$Rheophily_comments=="Fast")]<-"eros"
epa2$Rheophily_abbrev[which(is.na(epa2$Rheophily_abbrev)& epa2$Rheophily_comments=="Standing")]<-"depo"
epa2$Rheophily_abbrev[which(is.na(epa2$Rheophily_abbrev)& epa2$Rheophily_comments=="Standing-slight")]<-"depo"
epa2$Rheophily_abbrev[which(is.na(epa2$Rheophily_abbrev)& epa2$Rheophily_comments=="Moderate-fast")]<-"eros"
epa2$Rheophily_abbrev[which(is.na(epa2$Rheophily_abbrev)& epa2$Rheophily_comments=="Standing and flowing")]<-"depo_eros"
epa2$Rheophily_abbrev[which(is.na(epa2$Rheophily_abbrev)& epa2$Rheophily_comments=="Moderate")]<-"depo_eros"

################ Name check ############################################################################################################
library(taxize)
unique(epa2$SubjectTaxonomicName)
epa2$Accepted_name<-NA

epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Chelifera/Metachela")]<-"Empipidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Kogotus/Rickera")]<-"Perlodidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Pericoma/Telmatoscopus")]<-"Perlodidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Bezzia/Palpomyia")]<-"Ceraptogonidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Mayatrichia/Neotrichia")]<-"Hydroptilidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Nixe/Leucocruta")]<-"Heptageniidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Centroptilum/Procloeon")]<-"Baetidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Forcipomyia/Probezzia")]<-"Ceraptogonidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Polymeda/Ormosia")]<-"Ormosia"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Agapetus/Culoptila/Protoptila")]<-"Glossosomatidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Alisotrichia/Leucotricia")]<-"Hydroptilidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Cricotopus (Nostococladius)")]<-"Cricotopus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Cricotopus/Orthocladius")]<-"Chironomidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Petrophila     ")]<-"Petrophila"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Acanthametropodidae")]<-"Acanthametropidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Acanthametropus")]<-"Acanthametropus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Acanthomola pubescens")]<-"Anepeorus rusticus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Acentria")]<-"Acentria"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Agabus")]<-"Agabus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Agapetus bifidus")]<-"Agapetus bifidus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Alaskaperla")]<-"Alaskaperla"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Alaskaperla ovibovis")]<-"Alaskaperla ovibovis"


epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Agrionidae")]<-"Calopterygidae"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Allonarcys")]<-"Pteronarcys"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Allonarcys biloba")]<-"Pteronarcys biloba"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Allonarcys comstocki")]<-"Pteronarcys comstocki"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Allonarcys scotti")]<-"Pteronarcys scotti"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Allotrichoma")]<-"Allotrichoma"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Allotrichoma")]<-"Allotrichoma"

epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus cooki")]<-"Ameletus cooki"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus imbellis")]<-"Ameletus imbellis"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus similior")]<-"Ameletus similior"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus velox")]<-"Ameletus velox"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus validus")]<-"Ameletus validus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus dissitus")]<-"Ameletus dissitus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus facilis")]<-"Ameletus vancouverensis"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Acanthametropus pecatonica")]<-"Acanthametropus pecatonica"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Allonarcys proteus")]<-"Pteronarcys proteus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus ludens")]<-"Ameletus ludens"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus oregonensis")]<-"Ameletus oregonensis"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus sparsatus")]<-"Ameletus sparsatus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Ameletus walleyi")]<-"Ameletus walleyi"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Anepeorus")]<-"Anepeorus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Anepeorus rusticus")]<-"Anepeorus rusticus"
epa2$Accepted_name[which(epa2$SubjectTaxonomicName=="Anodocheilus")]<-"Anodocheilus"
na.accepted_names<-unique(epa2$SubjectTaxonomicName[which(is.na(epa2$Accepted_name),)])
na.accepted_names

#invalid names- use if_else statement to assign SubjectTaxonomicName as accepted name
invalid1<-c("Atoperla ephyre","Attenuatella","Bethbilbeckia", "Bolshecapnia sasquatchi","Calopsectra neoflavella","Calopsectra xantha","Ceraclea improcera ","Chernovskia","Cricotopus (Nostococladius)","Cordites","Cyphon", "Dactylobaetis cepheus","Dibolocelus")
replacement1<-c("Perlinella ephyre","Attenella","Macropelopia", "Sasquacapnia sasquatchi", "Tanytarsus neoflavellus", "Microspectra xantha", "Ceraclea cancellata","Chironomidae", "Cricotopus", NA,"Elodes","Camelobaetidius warreni", "Hydrophilus")
invalid2<-c("Elsianus","Elsianus moestus","Elsianus texanus","Elsianus shoemaker","Epicordulia","Hastaperla","Hastaperla brevis","Hastaperla chilnualna","Hayesomyia", "Hayesomyia senata", "Helodidae","Helopelopia","Helopelopia     ", "Helopelopia      ","Hydropyrus", "Hydrous", "Hylogomphus")
replacement2<-c("Macrelmis", "Macrelmis moestus", "Macrelmis texanus","Macrelmis shoemakei","Epitheca","Haploperla", "Haploperla brevis", "Haploperla chilnualna", "Thienemannimyia", "Thienemannimyia senata", "Scirtidae", "Conchapelopia", "Conchapelopia", "Conchapelopia", "Ephydra", "Hydrophilus", "Glomphus")
invalid3<-c("Ironopsis grandis","Isocapnia missourii","Leptocella",  "Limonia     ","Limonia      ","Lipiniella", "Lobodiamesa", "Macromiidae", "Meropelopia","Meropelopia     ", "Meropelopia      ", "Meropelopia flavifrons", "Narpus arizonica", "Neaviperla", "Neaviperla forcipata", "Neohagenulus julio","Neohagenulus luteolus","Neohagenulus tinctus","Nymphomyia")
replacement3<-c("Epeorus grandis","Isocapnia integra","Nectopsyche", "Limonia", "Limonia", NA, NA,"Macromiinae", 	"Conchapelopia","Conchapelopia", "Conchapelopia", "Conchapelopia flavifrons", "Narpus arizonicus","Suwallia", "Suwallia forcipata", NA, NA, NA, NA)
invalid4<-c("Oligoplectrum echo", "Palmocorixa", "Paranyctiophylax","Pedionomus beckae","Phasganophora capitata", "Sphaeriidae", "Spinadis wallacei", "Symposiocladius", "Tetragoneuria", "Thraulodes salinus", "Trissocladius","Zaitzevia parvulus")
replacement4<-c("Brachycentrus echo", "Palmacorixa", "Nyctiophylax","Asheum beckae","Agnetina capitata", "Sphaeriusidae", "Spinadis simplex", "Orthocladius", "Epitheca", "Thraulodes gonzalesi", "Hydrobaenus","Zaitzevia parvula parvula")


invalid_names<-as.character(c(invalid1, invalid2, invalid3, invalid4))
Accepted_name<-as.character(c(replacement1, replacement2, replacement3, replacement4))
new_names<-as.data.frame(cbind(invalid_names, Accepted_name), stringsAsFactors=FALSE)

#merge new_names with subset of epa2 with NA in Accepted_name
epa3<-left_join(epa2, new_names, by=c("SubjectTaxonomicName"="invalid_names"))
epa3$Accepted_name<-ifelse(is.na(epa3$Accepted_name.x), epa3$Accepted_name.y, epa3$Accepted_name.x)
epa3$Accepted_name<-ifelse(is.na(epa3$Accepted_name), epa3$SubjectTaxonomicName, epa3$Accepted_name)

tsn<-get_tsn(unique(epa3$Accepted_name), searchtype="scientific", accepted=TRUE, ask=TRUE) #not working

#get accepted name and tsn
Accepted_TSN<-itis_acceptname(searchtsn = na.omit(epa3$TSN))
TSN2<-unique(Accepted_TSN)
#merge Accepted_TSN with epa3
epa4<-merge(epa3, TSN2, by.x="TSN", by.y="submittedtsn", all.x=TRUE)
epa4$Accepted_name<-ifelse(!is.na(epa4$acceptedname), epa4$acceptedname, epa4$Accepted_name)

#drop unneeded columns
names(epa4)
epa5<-epa4%>%select(-c("Feed_mode_prim", "Adult_disp", "Voltinism", "Habit_prim", "Max_body_size", "Resp_early", "Resp_late", "Resp_adult", "Accepted_name.x", "Accepted_name.y", "acceptedname", "author"))

#get upstream names
utaxon<-unique(epa5$Accepted_name)
valid_names<-tax_name(query=utaxon, get=c("order","family", "genus", "species"), db="itis") 

epa6<-epa5%>%left_join(valid_names,by=c("Accepted_name"="query") )
epa6$Genus<-ifelse(epa6$SubjectTaxonomicName!=epa6$Accepted_name, epa6$genus, epa6$Genus)
epa6$Family<-ifelse(epa6$SubjectTaxonomicName!=epa6$Accepted_name, epa6$family, epa6$Family)
epa6$Order<-ifelse(epa6$SubjectTaxonomicName!=epa6$Accepted_name, epa6$order, epa6$Order)
epa7<-epa6%>%select(-c("order","family", "genus", "db"))

write.csv(epa7, "epa_traits.csv")



