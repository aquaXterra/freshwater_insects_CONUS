####This script is for data cleaning of the freshwater insects biological trait database compiled from 11/2017 to 12/7/2018 by LT, EH, MB, and ER
####Authors: Laura Twardochleb and Ethan Hiltner
####Date created:12/10/18

#######Notes/to-dos#######
#Check names in ITIS- complete
#Check name consistency in SubjectTaxonomicName, Genus, Family, Order- complete
#Merge remaining WQP and our trait dataset
#remove terrestrial taxa from occurrence database- complete
#assign dominant trait at genus level (binary coding)
#fuzzy-coding by Genus

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/")

#Load workspace image
load(file='Trait_cleaning.RData')

##########Read-in data from Google Drive####################################################################

#new dataset- combine Ethan, Minali, Erika datasets
#AquaticInsects/aquaticinsect_database/Data_files/Bhatt_biotraits_12_7_18.csv
data.key="11tuv40H55w7w_DYgFRE535Kg5qOkn7eV"
Bhatt <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key), stringsAsFactors=F, na.strings=c(""," ", "NA"), fileEncoding="UTF-8")
str(Bhatt)

id<-"1fIVjA6nL8WnKCTTkdj8CqUQQuUo9opdJ"
Ralston<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), stringsAsFactors=F, na.strings=c(""," ", "NA"), fileEncoding="UTF-8")

id2<-"1rjzWZA7oNrEJ4HviqmM-h0hgF7tZ7wna"
Hiltner<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id2), stringsAsFactors=F, na.strings=c(""," ", "NA"), fileEncoding="UTF-8")
#Hiltner<-read.csv('Hiltner_biotraits_master.csv', stringsAsFactors = FALSE, na.strings=c(""," ", "NA"))
#Bhatt<-read.csv('Bhatt_biotraits_12_7_18.csv', stringsAsFactors = FALSE, na.strings=c(""," ", "NA"))
#Ralston<-read.csv('Ralston_biotraits_12_07_18.csv', stringsAsFactors = FALSE, na.strings=c(""," ", "NA"))

Bhatt1<-Bhatt[,-c(1:3)]
Ralston1<-Ralston[,-c(1:4)]
Hiltner1<-Hiltner[,-1]
Hiltner1$X.2<-NULL
Hiltner1$X<-NULL
Ralston1$X<-NULL
Bhatt1$X<-NULL

#make sure columns match
PA<-rbind(Bhatt1, Ralston1) 
PA$Adult<-as.factor(PA$Body_size_comments) #revalue "Adult" to Body_size_comments

#now rename Adult_new and revalue to be consistent with Hiltner traits
PA$Body_size_comments<-NULL
PA$Notes<-NA
PA2<-PA[,order(colnames(PA), decreasing=FALSE)]
PA3<-select(PA2, SubjectTaxonomicName, everything())

Hiltner2<-Hiltner1[,order(colnames(Hiltner1), decreasing=FALSE)]
Hiltner3<-select(Hiltner2, SubjectTaxonomicName, everything())
biotraits<-rbind(PA3, Hiltner3) #don't have same columns

## Data Cleaning
#replacing all rows containing Heteroptera as the Order wih Hemiptera
revalue(biotraits$Order[biotraits$Order == "Heteroptera"] <- "Hemiptera")
revalue(biotraits$Voltinism_abbrev[biotraits$Voltinism_abbrev == "semivoltine"] <- "Semivoltine")
biotraits$Voltinism_abbrev[biotraits$Voltinism_abbrev == " "] <- NA
biotraits$Voltinism_abbrev[biotraits$Voltinism_abbrev == 'NA'] <- NA
revalue(biotraits$Voltinism_abbrev[biotraits$Voltinism_abbrev == "bi_multivoltine"] <- "Bi_multivoltine")
revalue(biotraits$Voltinism_abbrev[biotraits$Voltinism_abbrev == "BI_multivoltine"] <- "Bi_multivoltine")
revalue(biotraits$Voltinism_abbrev[biotraits$Voltinism_abbrev == "univoltine"] <- "Univoltine")
revalue(biotraits$Voltinism_abbrev[biotraits$Voltinism_abbrev == "Univoltine "] <- "Univoltine")
revalue(biotraits$Voltinism_abbrev[biotraits$Voltinism_abbrev == "multivoltine"] <- "Bi_multivoltine")
revalue(biotraits$Thermal_pref[biotraits$Thermal_pref == "cold-cool eurythermal (0-15 C)"] <- "Cold-cool eurythermal (0-15 C)")
revalue(biotraits$Thermal_pref[biotraits$Thermal_pref == "warm eurythermal (15-30 C"] <- "Warm eurythermal (15-30 C)")
revalue(biotraits$Thermal_pref[biotraits$Thermal_pref == "Warm eurythermal (15-30 C"] <- "Warm eurythermal (15-30 C)")
revalue(biotraits$Thermal_pref[biotraits$Thermal_pref == "Warm eurythermal (15-30 C) "] <- "Warm eurythermal (15-30 C)")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "clinger"] <- "Clinger")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "Clinger "] <- "Clinger")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "Clingers"] <- "Clinger")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "Clingers "] <- "Clinger")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "Sprawler "] <- "Sprawler")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "burrower"] <- "Burrower")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "Burrower "] <- "Burrower")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "swimmer"] <- "Swimmer")
revalue(biotraits$Habit_prim[biotraits$Habit_prim == "Swimmer "] <- "Swimmer")
biotraits$Habit_prim[biotraits$Habit_prim == "NA+N2343"] <- NA
biotraits$Habit_prim[biotraits$Habit_prim == 'NA'] <- NA
revalue(biotraits$Resp_abbrev[biotraits$Resp_abbrev == "Gillls"] <- "Gills")
revalue(biotraits$Resp_abbrev[biotraits$Resp_abbrev == "gils"] <- "Gills")
revalue(biotraits$Resp_abbrev[biotraits$Resp_abbrev == "gills"] <- "Gills")
revalue(biotraits$Resp_abbrev[biotraits$Resp_abbrev == "Gills "] <- "Gills")
revalue(biotraits$Resp_abbrev[biotraits$Resp_abbrev == "tegument "] <- "Tegument")
revalue(biotraits$Resp_abbrev[biotraits$Resp_abbrev == "tegument"] <- "Tegument")
revalue(biotraits$Resp_abbrev[biotraits$Resp_abbrev == "Tegument "] <- "Tegument")
revalue(biotraits$Max_body_size_abbrev[biotraits$Max_body_size_abbrev == "large"] <- "Large")
revalue(biotraits$Max_body_size_abbrev[biotraits$Max_body_size_abbrev == "medium"] <- "Medium")
revalue(biotraits$Max_body_size_abbrev[biotraits$Max_body_size_abbrev == "Medium "] <- "Medium")
revalue(biotraits$Max_body_size_abbrev[biotraits$Max_body_size_abbrev == "small"] <- "Small")
revalue(biotraits$Resp_abbrev[biotraits$Resp_abbrev == "plastron_spiracle"] <- "Plastron_spiracle")
revalue(biotraits$Emerge_synch_abbrev[biotraits$Emerge_synch_abbrev == "poorly"] <- "Poorly")
revalue(biotraits$Emerge_synch_abbrev[biotraits$Emerge_synch_abbrev == "well"] <- "Well")
revalue(biotraits$AdultFlyingStrength_abbrev[biotraits$AdultFlyingStrength_abbrev == "strong"] <- "Strong")
revalue(biotraits$AdultFlyingStrength_abbrev[biotraits$AdultFlyingStrength_abbrev == "weak"] <- "Weak")
revalue(biotraits$Emerge_season_1[biotraits$Emerge_season_1 == "Spring "] <- "Spring")
revalue(biotraits$Emerge_season_1[biotraits$Emerge_season_1 == "Summer "] <- "Summer")
revalue(biotraits$Feed_prim_abbrev[biotraits$Feed_prim_abbrev == "PR "] <- "PR")

##########Additional data cleaning- examine levels for each variable###############
unique(biotraits$Feed_prim_abbrev) #swimmer in feed prim
biotraits[which(biotraits$Feed_prim_abbrev == "Swimmer"), ] #find swimmer- in genus Hygrotus
biotraits[which(biotraits$Genus == "Hygrotus"), ] #find other Hygrotus
biotraits[990,15]<-"PR" #change instance to "PR" to match other Hygrotus
biotraits$Feed_prim_abbrev[biotraits$Feed_prim_abbrev=="NA"]<-NA
biotraits[which(biotraits$Feed_prim_abbrev == "Other (specify in comments)" ), ]

#Assign Feed_prim_abbrev based on comments
unique(biotraits$Feed_mode_comments)
other<-biotraits[which(!is.na(biotraits$Feed_mode_comments) & biotraits$Feed_prim_abbrev == "Other (specify in comments)"),]

biotraits$Feed_prim_abbrev[which(biotraits$Feed_prim_abbrev == "Other (specify in comments)" & biotraits$Feed_mode_comments == "Larvae and adults are herbivorous.")]<-"HB" 
biotraits$Feed_prim_abbrev[which(biotraits$Feed_prim_abbrev == "Other (specify in comments)" & biotraits$Feed_mode_comments == "Feed on roots, rhizomes, and stems of aquatic plants.")]<-"HB" 
biotraits$Feed_prim_abbrev[which(biotraits$Feed_prim_abbrev == "Other (specify in comments)" & biotraits$Feed_mode_comments == "Larvae feed mostly on algae.  Adults are mostly herbivorous.")]<-"HB" 
biotraits$Feed_prim_abbrev[which(biotraits$Feed_prim_abbrev == "Other (specify in comments)" & biotraits$Feed_mode_comments == "Feed on green algae.")]<-"HB" 

unique(biotraits$Resp_abbrev)
biotraits[which(biotraits$Resp_abbrev == "CF" ), ]#find mislabel of Resp_abbrev
biotraits[which(biotraits$SubjectTaxonomicName == "Haploperla"), ] #look at resp of other Haploperla
biotraits[4058:4059,21]<-"Clinger" #assign habit
biotraits[4058:4059,28]<-NA
#add resp abbrev from comments and resp_early, resp_late! Need to decide how to assign for juveniles vs. adults- can fuzzy code for early and late as well as by genus
unique(biotraits$Resp_early)
revalue(biotraits$Resp_early[biotraits$Resp_early == "Gills "] <- "Gills")
revalue(biotraits$Resp_early[biotraits$Resp_early == "gills"] <- "Gills")
revalue(biotraits$Resp_early[biotraits$Resp_early == "gills"] <- "Gills")
revalue(biotraits$Resp_early[biotraits$Resp_early == "Tegument "] <- "Tegument")
revalue(biotraits$Resp_early[biotraits$Resp_early == "plastron_spiracle"] <- "Plastron_spiracle")

biotraits[which(biotraits$Resp_early == "Atmospheric breathers"), ] #assign to Plastron/spiracle for (juveniles) Resp_abbrev based on Merritt, Cummins, Berg? Also for fuzzy coding?
biotraits$Resp_abbrev[which(biotraits$Resp_early == "Atmospheric breathers")] = "Plastron_spiracle"

biotraits[which(biotraits$Resp_early == "Cutaneous"), ] #assign to tegument  for juveniles for Resp_abbrev based on Merritt, Cummins, Berg. Also for fuzzy coding?
biotraits$Resp_abbrev[which(biotraits$Resp_early == "Cutaneous")] <- "Tegument"

#assign temporary air store in Resp_early to plastron/spiracle in Resp_abbrev
biotraits[which(biotraits$Resp_early == "Temporary air store"), ] 
biotraits$Resp_abbrev[which(biotraits$Resp_early == "Temporary air store")] = "Plastron_spiracle"

biotraits[which(biotraits$Resp_early == "Plastron_spiracle"), ] #Resp_abbrev already assigned

biotraits[which(biotraits$Resp_early == "Tracheal gills"), ] #seems to apply only to aquatic beetles
#aquatic beetles' respiration will need to be revised because all adults have spiracles, but juveniles can have different resp. modes. Need to assign different values for Resp_abbrev (juvenile only) and Resp_adult. In addition, Resp_abbrev will have to be fuzzy-coded by Resp_early and Resp_late
#Reassign plastron_spiracle for Resp_abbrev for Coleoptera to Resp_adult. Then revise Resp_abbrev for those taxa to NA.
biotraits$Resp_adult[which(biotraits$Order=="Coleoptera" & biotraits$Resp_abbrev=="Plastron_spiracle")]<-"Plastron_spiracle"
biotraits$Resp_abbrev[which(biotraits$Order=="Coleoptera" & biotraits$Resp_abbrev=="Plastron_spiracle")]<-NA

#assign Tracheal gills in Resp_early to Resp_abbrev (resp_early and resp_late are consistent between these two categories)
biotraits$Resp_abbrev[which(biotraits$Resp_early == "Tracheal gills")] = "Gills" #assign to gills based on Merritt, Cummins, Berg

biotraits[which(biotraits$Resp_early == "Hemoglobin"), ] #only one instance applied to entire family Chironomidae

biotraits[which(biotraits$Resp_early == "Spiracular gills"), ] #applies only to three aquatic beetle taxa
#assign Spiracular gills in Resp_early to Plastron_spiracle in Resp_abbrev (resp_early and resp_late are consistent between these two categories)
biotraits$Resp_abbrev[which(biotraits$Resp_early == "Spiracular gills")] <-"Plastron_spiracle" # assign to plastron_spiracle based on Merritt, Cummins, Berg

biotraits[which(biotraits$Resp_early == "Plant breathers"), ] # assign to plastron_spiracle based on Merritt, Cummins, Berg
biotraits$Resp_abbrev[which(biotraits$Resp_early == "Plant breathers")] <-"Plastron_spiracle"

biotraits[which(biotraits$Resp_early == "Gills"), ] #assign to gills for Resp_abbrev
biotraits$Resp_abbrev[which(biotraits$Resp_early == "Gills")] <-"Gills"

biotraits[which(biotraits$Resp_early == "Tegument"), ] #resp_abbrev already assigned

unique(biotraits$Resp_late)
revalue(biotraits$Resp_late[biotraits$Resp_late == "gills"] <- "Gills")
revalue(biotraits$Resp_late[biotraits$Resp_late == "Tegument "] <- "Tegument")

unique(biotraits$Habit_prim)
unique(biotraits$Habit_comments)
biotraits$Habit_comments[biotraits$Habit_comments=="Habit_prim_abbrev: BU = burrower; CB = climber; CN = clinger; SK = skater; SP = sprawler; SW = swimmer"]<-NA
#assign Habit_prim from comments
biotraits[which(biotraits$Habit_comments != "NA"& biotraits$Habit_prim=="Other (specify in comments)"),]
biotraits$Habit_prim[which(biotraits$Habit_comments =="Crawl" & biotraits$Habit_prim=="Other (specify in comments)")]<-"Crawler"
biotraits$Habit_prim[which(biotraits$Habit_comments =="Crawler" & biotraits$Habit_prim=="Other (specify in comments)")]<-"Crawler"
biotraits$Habit_prim[which(biotraits$Habit_comments =="Crawl on the substrate." & biotraits$Habit_prim=="Other (specify in comments)")]<-"Crawler"
biotraits$Habit_prim[which(biotraits$Habit_comments =="Attached with sucker disks; larvae move by slowly alternately moving anterior and posterior sucker disks." & biotraits$Habit_prim=="Other (specify in comments)")]<-"Attached/fixed"

unique(biotraits$Adult)
revalue(biotraits$Adult[biotraits$Adult=="adult"]<-"Adult")

unique(biotraits$Adult_disp)

unique(biotraits$AdultFlyingStrength_abbrev)
biotraits[which(biotraits$AdultFlyingStrength_abbrev == "depo" ), ]#find mislabel of AdultFlyingStrength
biotraits[which(biotraits$SubjectTaxonomicName == "Celithemis"), ]
biotraits[15586,4]<-NA #assign AdultFlyingStrength
biotraits[15586,33]<-"depo" #assign Rheophily

unique(biotraits$AdultFlyingStrength_comments)
biotraits[which(biotraits$AdultFlyingStrength_comments == "strong" ), ]

unique(biotraits$Emerge_season_1)

unique(biotraits$Emerge_season_2)
revalue(biotraits$Emerge_season_2[biotraits$Emerge_season_2 == "Fal"] <- "Fall")

unique(biotraits$Emerge_season_comments)

unique(biotraits$Emerge_synch)
biotraits[which(biotraits$Emerge_synch == "Winter" ), ]#find mislabel of Emerge_synch
biotraits[c(3664,3795,3904,3918,3919,4009,4219,4263,4314,4336,4342),6]<-"Winter" #reassign Winter to Emerge_season_1
biotraits[c(3664,3795,3904,3918,3919,4009,4219,4263,4314,4336,4342),9]<-NA #Remove winter from Emerge_synch
biotraits[c(3664,3795,4219,4336),7]<-"Winter" #reassign values from Emerge_synch_comments to Emerge_season_2
biotraits[c(3904,3918,4009,4263,4314,4342,3919),7]<-"Spring" #reassign values from Emerge_synch_comments to Emerge_season_2
biotraits[c(3664,3795,3904,3918,4009,4219,4263,4314,4336,4342, 3919),11]<-NA #Remove winter and spring from Emerge_synch_comments

biotraits[which(biotraits$Emerge_synch == "Spring" ), ]#find mislabel of Emerge_synch
biotraits[c(3674,3676,3771,3773,3791,3928,3933,4022,4057,4148,4149, 4214,4223,4255,4273,4281,4283,4324,4330,4334),6]<-"Spring" #reassign Winter to Emerge_season_1
biotraits[c(3674,3676,3771,3773,3791,3928,3933,4022,4057,4148,4149, 4214,4223,4255,4273,4281,4283,4324,4330,4334),9]<-NA #Remove spring from Emerge_synch
biotraits[c(4223,4330),7]<-"Spring" #Reassign values from Emerge_synch_comments to Emerge_season_2
biotraits[c(3674,3771,3791,3928,4022,4057,4148,4149,4255,4273,4281,4283,4334),7]<-"Summer" #Reassign values from Emerge_synch_comments to Emerge_season_2
biotraits[c(3676,3933,4324),7]<-"Winter" #Reassign values from Emerge_synch_comments to Emerge_season_2
biotraits[c(4214,3773),7]<-"Fall" #Reassign values from Emerge_synch_comments to Emerge_season_2
biotraits[c(3674,3676,3771,3773,3791,3928,3933,4022,4057,4148,4149, 4214,4223,4255,4273,4281,4283,4324,4330,4334),11]<-NA #Remove seasons from Emerge_synch_comments

biotraits[which(biotraits$Emerge_synch == "Summer" ), ]#find mislabel of Emerge_synch
biotraits[c(3755, 4216, 4352),6]<-"Summer"
biotraits[c(3755, 4216, 4352),9]<-NA
biotraits[c(3755),7]<-"Spring"
biotraits[c(4216, 4352),7]<-"Summer"
biotraits[c(3755, 4216, 4352),11]<-NA

biotraits[which(biotraits$Emerge_synch == "Fall" ), ]
biotraits[c(3666:3668,3803,3930,4017,4024,4058,4059,4081,4090,4091,4099,4147,4181,4230,4348,4379,4380,4474,4475),6]<-"Fall"
biotraits[c(3666:3668,3803,3930,4017,4024,4058,4059,4081,4090,4091,4099,4147,4181,4230,4348,4379,4380,4474,4475),9]<-NA
biotraits[c(3666:3668,3803,4081,4230,4348,4474,4475),7]<-"Winter"
biotraits[c(3930,4017,4024,4058,4059,4090,4091,4099,4181,4379,4380),7]<-"Spring"
biotraits[c(4147),7]<-"Fall"
biotraits[c(3666:3668,3803,3930,4017,4024,4058,4059,4081,4090,4091,4099,4147,4181,4230,4348,4379,4380,4474,4475),11]<-NA

biotraits[which(biotraits$Emerge_synch_abbrev == "Well" ), ]
biotraits[which(biotraits$Emerge_synch_comments == "Well" ), ] #instances of well in comments matches those of emerge_synch_abbrev

revalue(biotraits$Emerge_synch[biotraits$Emerge_synch == "Synchronous"] <- "Yes")
biotraits$Emerge_synch[biotraits$Emerge_synch == "Unknown"] <- NA
biotraits[which(biotraits$Emerge_synch == "No" ), ]

unique(biotraits$Feed_mode_comments)
unique(biotraits$Feed_mode_sec) #revalue to macth Feed_prim_abbrev
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="Piercer herbivore"]<-"HB")
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="Scraper/grazer"]<-"HB")
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="Herbivore"]<-"HB")
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="Predator"]<-"PR")
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="Shredder"]<-"SH")
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="Collector-gatherer"]<-"CG")
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="collector-gatherer"]<-"CG")
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="Collector-filterer"]<-"CF")
revalue(biotraits$Feed_mode_sec[biotraits$Feed_mode_sec=="Parasite"]<-"PA")
biotraits[which(biotraits$Feed_mode_sec == "PR and HB" ), ]
biotraits[14388,13]<-"PR and HB" #reassign to Feed_mode_comments
biotraits[14388,14]<-NA
biotraits[which(biotraits$Feed_mode_sec == "Filter feeders and sometimes predators" ), ]
biotraits[13768,13]<-"Filter feeders and sometimes predators" #reassign to Feed_mode_comments
biotraits[13768,14]<-NA
biotraits[which(biotraits$Feed_mode_sec == "Other (specify in comments)" ), ]

biotraits[which(biotraits$Feed_mode_sec!="NA" & biotraits$Feed_mode_sec=="NA")]
biotraits[which(biotraits$Feed_mode_comments=="Algae"),]
biotraits[which(biotraits$Feed_mode_comments=="scraper"),]
biotraits[which(biotraits$Feed_mode_comments=="herbivorous"),]
biotraits$Feed_prim_abbrev[which(biotraits$Feed_mode_comments=="herbivorous")]<-"HB"
biotraits[which(biotraits$Feed_mode_comments=="Consumes diatoms." ),]
biotraits[which(biotraits$Feed_mode_comments=="Feed on diatoms" ),]
biotraits[which(biotraits$Feed_mode_comments=="Algal grazer" ),]
biotraits[which(biotraits$Feed_mode_comments=="Early instars eat diatoms; late instars eat Lemanea (red algae) exclusively."  ),]
biotraits[which(biotraits$Feed_mode_comments=="Feed on algae, periphyton."  ),]
biotraits[which(biotraits$Feed_mode_comments=="Graze algae on rock surfaces."  ),]

unique(biotraits$Female_disp_abbrev)
revalue(biotraits$Female_disp_abbrev[biotraits$Female_disp_abbrev=="low"]<-"Low")
revalue(biotraits$Female_disp_abbrev[biotraits$Female_disp_abbrev=="high"]<-"High")

unique(biotraits$Female_disp_old)
biotraits[which(biotraits$Female_disp_old == "10 km or less" ), ]
biotraits[14535,16]<-"High" #reassign dispersal for Notonecta
biotraits[which(biotraits$Female_disp_old == "10 km or less" ), ]
biotraits[which(biotraits$Genus == "Notonecta" ), ]
biotraits[which(biotraits$Genus == "Buenoa" ), ]
biotraits[which(biotraits$Female_disp_old == "1 km or less" ), ]

unique(biotraits$Female_disp_comments)
biotraits[which(biotraits$Female_disp_comments == "1 km or less" ), ]
biotraits[which(biotraits$Female_disp_comments == "100 km or less" ), ]
biotraits[which(biotraits$Female_disp_comments == "less than 1 km flight before laying eggs" ), ]

unique(biotraits$Max_body_size)
unique(biotraits$Max_body_size_abbrev)
revalue(biotraits$Max_body_size_abbrev[biotraits$Max_body_size_abbrev=="large "]<-"Large")
biotraits$Feed_prim_abbrev[which(biotraits$Max_body_size_abbrev == "CG" )]<-"CG"
biotraits$Max_body_size_abbrev[which(biotraits$Max_body_size_abbrev == "CG" )]<-NA

unique(biotraits$Rheophily_abbrev)
unique(biotraits$Rheophily_comments)
biotraits[which(biotraits$Rheophily_comments == "Standing-slight" ), ]
biotraits[which(biotraits$Rheophily_comments == "Fast" ), ]
biotraits[which(biotraits$Genus == "Cricotopus" ), ]
biotraits[which(biotraits$Rheophily_comments == "Standing" ), ]
biotraits$Volt_comments[which(biotraits$Rheophily_comments == "bi_multivoltune (18+/yr)"  )]<-"bi_multivoltune (18+/yr)"
biotraits$Rheophily_comments[which(biotraits$Rheophily_comments == "bi_multivoltune (18+/yr)"  )]<-NA
biotraits[which(biotraits$Rheophily_comments == "Semivoltine, with variable cohorts"  ), ]
biotraits$Volt_comments[which(biotraits$Rheophily_comments == "Semivoltine, with variable cohorts"  )]<-"Semivoltine, with variable cohorts"
biotraits$Rheophily_comments[which(biotraits$Rheophily_comments == "Semivoltine, with variable cohorts"  )]<-NA

#remove Rheophily entries of de Jong
unique(biotraits$Study_Citation_abbrev)
biotraits$Rheophily_abbrev[which(biotraits$Study_Citation_abbrev=="(De Jong, 2018)")]<-NA
biotraits$Rheophily_abbrev[which(biotraits$Study_Citation_abbrev=="(de Jong, 2018)")]<-NA

##########Remove terrestrial taxa###############
biotraits[which(biotraits$Terrestrial == "yes"  ), ]
unique(biotraits$Terrestrial)
biotraits1<-subset(biotraits, Terrestrial=="no"|is.na(Terrestrial))
unique(biotraits1$Terrestrial)

#create data frame of terrestrial taxa to cross-ref with occurrence database
terrestrial<-subset(biotraits, Terrestrial=="yes")
length(unique(terrestrial$SubjectTaxonomicName)) #105 terrestrial taxa
length(unique(terrestrial$Genus)) #70 terrestrial genera

write.csv(terrestrial, "terrestrial_taxa.csv")

########Explore regions and states##############
#check consistency of state assignment
unique(biotraits1$Study_location_state)
biotraits1[which(biotraits1$Study_location_state == "N. America and Europe" ), ]
biotraits1$Study_location_region[which(biotraits1$Study_location_state == "N. America and Europe" ) ]<-"N. America and Europe"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "N. America and Europe" ) ]<-NA
revalue(biotraits1$Study_location_state[biotraits1$Study_location_state=="Massachuset"]<-"Massachusetts")
biotraits1$Study_location_region[which(biotraits1$Study_location_state == "Mexico" ) ]<-"Mexico"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Mexico" ) ]<-NA
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Montreal" ) ]<-"Quebec"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Northern Quebec" ) ]<-"Quebec"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Southern California" ) ]<-"California"
biotraits1$Study_location_region[which(biotraits1$Study_location_state == "Western N. America" ) ]<-"Western N. America"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Western N. America" ) ]<-NA
biotraits1$Study_location_region[which(biotraits1$Study_location_state == "Eastern United States" ) ]<-"Eastern United States"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Eastern United States" ) ]<-NA
biotraits1$Study_location_region[which(biotraits1$Study_location_state == "Northern U.S. and Canada" ) ]<-"Northern U.S. and Canada"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Northern U.S. and Canada" ) ]<-NA
biotraits1$Study_location_region[which(biotraits1$Study_location_state == "New England" ) ]<-"New England"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "New England" ) ]<-NA
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "British Columbia " ) ]<-"British Columbia"
biotraits1$Study_location_region[which(biotraits1$Study_location_state == "North America" ) ]<-"North America"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "North America" ) ]<-NA
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Quebec " ) ]<-"Quebec"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Ontario " ) ]<-"Ontario"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Saskatchewan  " ) ]<-"Saskatchewan"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Northwest Territories " ) ]<-"Northwest Territories"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Arizona " ) ]<-"Arizona"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Georgia " ) ]<-"Georgia"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Washington " ) ]<-"Washington"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Alberta " ) ]<-"Alberta"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Geogia" ) ]<-"Georgia"
biotraits1$Study_location_region[which(biotraits1$Study_location_state == "Eastern N. America" ) ]<-"Eastern N. America"
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Eastern N. America" ) ]<-NA
biotraits1$Study_location_state[which(biotraits1$Study_location_state == "Florida " ) ]<-"Florida"

#check consistency of region assignment
unique(biotraits1$Study_location_region)
biotraits1$Study_Citation[which(biotraits1$Study_location_region == "Jong, Grant de. Aquatic Insects in the Vicinity of the Black Hills, South Dakota and Wyoming. Lulu.com," ) ]<-"Jong, Grant de. Aquatic Insects in the Vicinity of the Black Hills, South Dakota and Wyoming. Lulu.com,"
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "Jong, Grant de. Aquatic Insects in the Vicinity of the Black Hills, South Dakota and Wyoming. Lulu.com," ) ]<-NA
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "Eastern N. America" ) ]<-"Eastern N. America"
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "Pacific Southwest (coastal)" ) ]<-"Pacific"
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "MIdwest" ) ]<-"Midwest"
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "MidWest" ) ]<-"Midwest"
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "Midwest " ) ]<-"Midwest"
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "New England" ) ]<-"Northeast"
biotraits1$Study_location_state[which(biotraits1$Study_location_region == "Alaska" ) ]<-"Alaska"
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "Alaska" ) ]<-NA
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "Great Basin" ) ]<-"Western N. America"
biotraits1$Study_location_region[which(biotraits1$Study_location_region == "Eastern United States" ) ]<-"Eastern N. America"

########Check reference consistency##############
unique(biotraits1$Study_Citation)

#check consistency of Study_Citation_abbrev
unique(biotraits1$Study_Citation_abbrev)
#Fix obvious mistakes
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(De Jong, 2018) " ) ]<-"De Jong (2018)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(de Jong, 2018)" ) ]<-"De Jong (2018)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Stewart et al., 1988)" ) ]<-"Stewart et al. (1988)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Merritt, et al., 2008)" ) ]<-"Merritt, Cummins, Berg (2008)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "Merritt Cummins Berg (2008)" ) ]<-"Merritt, Cummins, Berg (2008)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Merrit et al., 1996)" ) ]<-"Merritt, Cummins, Berg (2008)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Merritt et al., 1996)" ) ]<-"Merritt, Cummins, Berg (2008)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Poff et al., 2006)" ) ]<-"Poff et al. (2006)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(De Jong, 2018)" ) ]<-"De Jong (2018)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Schriever et al., 2015)" ) ]<-"Schriever et al. (2015)" 
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Beaty, 2015) " ) ]<-"Beaty (2015)" 
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Beaty, 2015)" ) ]<-"Beaty (2015)"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "Schriever et al. 2015") ]<-"Schriever et al. (2015)" 
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Wiggins, 1998" )]<-"Wiggins (1998)" 
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "(Wiggins, 1998)" )]<-"Wiggins (1998)" 

#remove parentheses
biotraits1$Study_Citation_abbrev<-gsub("[()]", "", biotraits1$Study_Citation_abbrev)
#remove commas
biotraits1$Study_Citation_abbrev<-gsub("[,]", "", biotraits1$Study_Citation_abbrev)
#Fix book titles
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "Merritt Cummins Berg 2008" ) ]<-"Merritt, Cummins, Berg 2008"
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "Needham Westfall May 2000" ) ]<-"Needham, Westfall, May 2000"
#Fix special characters- European accents
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "Ca√±edo et al. 2016" ) ]<-"Ca\u00f1edo et al. 2016"

#read in citation list from Ethan to match up full citation with citation_abbrev

id3<-"19vXpbnCDCm0iwNAP9cGYORxYJZqLaDXZ"
citations<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id3), stringsAsFactors = FALSE, na.strings=c(""," ", "NA"), fileEncoding="UTF-8")
citations<-citations[rowSums(is.na(citations)) != ncol(citations),]
#remove parantheses
citations$Study_Citation_abbrev<-gsub("[()]", "", citations$Study_Citation_abbrev)
#remove commas
citations$Study_Citation_abbrev<-gsub("[,]", "", citations$Study_Citation_abbrev)


#Replace problematic values in Study.Citation.x with Study.Citation.Y
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "&erson 1979" ) ]<-"Anderson 1979"
biotraits1$Study_Citation[which(biotraits1$Study_Citation_abbrev == "Anderson 1979" ) ]<-"Anderson, R. The Coleoptera of a Lough Neagh Sandy Shoreline with Recent Records of Stenus Palposus Zetterstedt (Staphylinidae) and Dyschirius Obscurus Gyllenhal (Carabidae). The Irish Naturalists Journal 19, no. 9 (1979): 297-302."
biotraits1[which(biotraits1$Study_Citation_abbrev == "Anderson 1979" ) ,]

biotraits1[which(biotraits1$Study_Citation_abbrev == "J≈†ch 1994" ) ,]
biotraits1$Study_Citation_abbrev[which(biotraits1$Study_Citation_abbrev == "J≈†ch 1994" ) ]<-"J\u00e4ch 1994"
biotraits1$Study_Citation[which(biotraits1$Study_Citation_abbrev == "J\u00e4ch 1994" ) ]<-"J\u00e4ch 1994, M.A. Descriptions of New Species of Hydraenidae (Coleoptera) from Texas, Indiana and Oklahoma, with Faunistic and Taxonomic Notes on the Family in Texas.The Coleopterists Bulletin 48, no. 4 (1994): 301-308."

#merge on Citation_abbrev to preserve newer full citation
biotraits2<-left_join(biotraits1, citations, by="Study_Citation_abbrev")
biotraits2[which(biotraits2$Study_Citation_abbrev == "Mendez & 2008"  ) ,]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Mendez & 2008"  ) ]<-"Mendez 2008"

#replace values in Study_citation.x with study_citation.y
biotraits2$Study_Citation.x <- ifelse(is.na(biotraits2$Study_Citation.y), biotraits2$Study_Citation.x, biotraits2$Study_Citation.y)
biotraits2$Study_Citation.y <- NULL
str(biotraits2)
colnames(biotraits2)[35]<-"Study_Citation"

#check consistency of Study_Citation_abbrev
unique(biotraits2$Study_Citation_abbrev)
biotraits2[which(biotraits2$Study_Citation_abbrev == "Merrit et al. 1992"  ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Merrit et al. 1992"  ) ]<-"Merritt et al. 1992"

biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Merrit & Cummins 1984"  ) ]<-"Merritt & Cummins 1984"

biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Nils & B≈°ttger 2001"  ) ]<-"Nils & B\u00f6ttger 2001"

biotraits2[which(biotraits2$Study_Citation_abbrev == "Westfall & May 2007"  ), ]
biotraits2[which(biotraits2$Study_Citation_abbrev == "Westfall & May 2006"  ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Westfall & May 2007"  )]<-"Westfall & May 2006"
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Westfall & May 2006"  )]<-"Westfall, Minter Jackson, and Michael L. May. Damselflies of North America. Vol. 649. Gainesville: Scientific Publishers, 2006."
biotraits2[which(biotraits2$Study_Citation_abbrev == "Arnet et al. 2001"   ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Arnet et al. 2001"   )]<-"Arnett et al. 2001"
biotraits2[which(biotraits2$Study_Citation_abbrev == "Arnett et al. 2001"   ), ]
biotraits2[which(biotraits2$Study_Citation_abbrev == "Sites & Nicols1993"   ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Sites & Nicols1993"   ) ]<-"Sites & Nichols 1993"
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Sites & Nichols 1993"   ) ]<-"Sites, R.W., and B.J. Nichols. Voltinism, Egg Structure, and Descriptions of Immature Stages of Cryphocricos-Hungerfordi (Hemiptera, Naucoridae).  Annals of the Entomological Society of America 86, no. 1 (January 1993): 80–90. https://doi.org/10.1093/aesa/86.1.80."
biotraits2[which(biotraits2$Study_Citation_abbrev == "Sites & Nicols1990"    ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Sites & Nicols1990"    ) ]<-"Sites & Nichols 1990" 

biotraits2[which(biotraits2$Study_Citation_abbrev == "Wiggins 1998"  ), ]
biotraits2[which(biotraits2$Study_Citation_abbrev == "Wiggins 1989"  ), ]

#check remaining inconsistencies in Study_Citation
unique(biotraits2$Study_Citation)

biotraits2[which(biotraits2$Study_Citation_abbrev == "Bennet et al. 2018"  ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Bennet et al. 2018"  ) ]<-"Bennett et al. 2018"
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Bennett et al. 2018"  ) ]<-"Bennett, J.M. et al. GlobTherm, a global database on thermal tolerances for aquatic and terrestrial organisms. Sci. Data 5:180022 doi: 10.1038/sdata.2018.22 (2018)."

biotraits2[which(biotraits2$Study_Citation_abbrev == "Poff et al. 2006"  ), ]
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Poff et al. 2006"  ) ]<-"Poff, N. LeRoy, Julian D. Olden, Nicole K. M. Vieira, Debra S. Finn, Mark P. Simmons, and Boris C. Kondratieff. Functional Trait Niches of North American Lotic Insects: Traits-Based Ecological Applications in Light of Phylogenetic Relationships. Journal of the North American Benthological Society 25, no. 4 (December 1, 2006): 730–55"
biotraits2[which(biotraits2$Study_Citation_abbrev == "Lenat 1987"  ), ]
biotraits2[which(biotraits2$Study_Citation_abbrev == "Arnet 1985"   ), ]
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Arnet 1985"   ) ]<-"Arnett Jr, Ross H. American insects: a handbook of the insects of America north of Mexico. Van Nostran Reinhold Company Ltd., 1985."
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Arnet 1985"   ) ]<-"Arnett 1985"
biotraits2[which(biotraits2$Study_Citation_abbrev == "Arnett 1985"   ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Arnet & Thomas 2001"   ) ]<-"Arnett & Thomas 2001"

biotraits2[which(biotraits2$Study_Citation_abbrev == "Merritt, Cummins, Berg 2008"   ), ]
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Merritt, Cummins, Berg 2008"   ) ]<-"Merritt, Richard W., et al. 2008. An Introduction to the Aquatic Insects of North America. 4th ed., Kendall/Hunt Publishing Company"
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Schriever et al. 2015"   ) ]<-"Schriever, Tiffany A., Michael T. Bogan, Kate S. Boersma, Miguel Ca\u00f1edo-Arg\u00fcelles, Kristin L. Jaeger, Julian D. Olden, and David A. Lytle. 2015. “Hydrology Shapes Taxonomic and Functional Structure of Desert Stream Invertebrate Communities.” Freshwater Science 34 (2): 399–409. https://doi.org/10.1086/680518."
biotraits2[which(biotraits2$Study_Citation_abbrev == "Schriever et al. 2015"   ), ]
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "De Jong 2018"   ) ]<-"Jong, Grant de. Aquatic Insects in the Vicinity of the Black Hills, South Dakota and Wyoming. Lulu.com"
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev ==  "Needham, Westfall, May 2000"   ) ]<-"Needham, J.G., M.J. Westfall, and M.L. May. 2000. Dragonflies of North America., Rev. Ed., Scientific Publishers, Gainesville, FL"
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation ==  "Paulson, Dennis. Dragonflies and Damselflies of the West. Princeton University Press, 2011."   ) ]<-"Paulson 2011b"
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation ==  "Paulson, Dennis. Dragonflies and Damselflies of the East. Princeton University Press, 2011."   ) ]<-"Paulson 2011a"
biotraits2[which(biotraits2$Study_Citation_abbrev == "Corbet et al. 2006"   ), ]
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Wiggins 1996"   ) ]<-"Wiggins, G.B. 1996. Larvae of the North American caddisfly genera: (trichoptera). (2nd ed.). Toronto: University of Toronto Press."
biotraits2[which(biotraits2$Study_Citation == "(In the USGS Database of Lotic Invertebrate Traits for North America): The genus Phylloicus M√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈æ√É∆í√Ç¬∂√É¬¢√ã‚Ä†√Ö¬°√É∆í√¢‚Ç¨Àú√É¬¢√ã‚Ä†√Ö¬°√É‚Äö√Ç¬•_ller in the United States, with a redescription of Phylloicus ornatus"),]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Arnett & Thomas 2000"   )]<-"Arnett & Thomas 2001"
biotraits2[which(biotraits2$Study_Citation_abbrev == "Frank & Kee-Jeong 2011"   ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation == "Arnett Jr, Ross H.American insects: a handbook of the insects of America north of Mexico. Crc Press, 2000."   ) ]<-"Arnett 2000"
biotraits2[which(biotraits2$Study_Citation_abbrev == "Chaboo & Shepard 2015"   ), ]

#write citations table
biotraits_citations<-biotraits2[,c("Study_Citation_abbrev", "Study_Citation")]
citations2<-unique(biotraits_citations)
citations3<-subset(citations2, Study_Citation_abbrev!="USGS 2006")

#need to choose encoding when opening file using excel, otherwise characters are corrupted
write.csv(citations3, "citations_corrected.csv", fileEncoding = "UTF-8")


########Check naming consistency#################
unique(biotraits2$Order) #ok
unique(biotraits2$Family) #ok
unique(biotraits2$SubjectTaxonomicName, options(max.print=5000))
#remove trailing white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
biotraits2$SubjectTaxonomicName<-trim(biotraits2$SubjectTaxonomicName)
#fix entries with weird characters
biotraits2$SubjectTaxonomicName[which(biotraits2$SubjectTaxonomicName == "AncyronyxâˆšÃ©Â¬Ã¦variegatus") ]<-"Ancronyx variegatus"
biotraits2$SubjectTaxonomicName[which(biotraits2$SubjectTaxonomicName == "PeltodytesâˆšÃ©Â¬Ã¦duodecimpuntatusâˆšÃ©Â¬Ã¦âˆšÃ©Â¬Ã¦âˆšÃ©Â¬Ã¦") ]<-"Peltodytes duodecimpuntatus"
biotraits2$SubjectTaxonomicName[which(biotraits2$SubjectTaxonomicName == "ParalauterborniellaâˆšÃ©Â¬Ã¦nigrohalterale") ]<-"Paralauterborniella nigrohalterale"
biotraits2$SubjectTaxonomicName[which(biotraits2$SubjectTaxonomicName == "ZaitzeviaâˆšÃ©Â¬Ã¦parvulus") ]<-"Zaitzevia parvulus"
biotraits2$SubjectTaxonomicName[which(biotraits2$SubjectTaxonomicName == "HomoleptohyphesÃ‚Â dimorphus") ]<-"Homoleptohyphes dimorphus"
biotraits2$SubjectTaxonomicName[which(biotraits2$SubjectTaxonomicName == "TricorythodesÃ‚Â explicatus" ) ]<-"Tricorythodes explicatus"
  
#this removed punctuation
biotraits2$SubjectTaxonomicName<-gsub("[[:punct:]]", " ", biotraits2$SubjectTaxonomicName)
#this removed special characters
biotraits2$SubjectTaxonomicName<-gsub("[Ã â  Å ÃƒÂ ÃƒÂ Ã Ë Å Ã Â Ã Â Ã Â Ã Ë Å Ã Â ]", " ", biotraits2$SubjectTaxonomicName)
#remove extra white space
biotraits2$SubjectTaxonomicName<-gsub("\\s+"," ",biotraits2$SubjectTaxonomicName)

#remove NA record from SubjectTaxonomicName where genus is also NA
biotraits2[is.na(biotraits2$SubjectTaxonomicName),]

biotraits2[15429,1]<-"Auleutes"
biotraits2[16147,1]<-"Matriella"
biotraits2[16148,1]<-"Penelomax"

biotraits3<-biotraits2[!is.na(biotraits2$SubjectTaxonomicName),]

#####################Check names and TSN in ITIS################################################
library(taxize)
#which entries have NAs for TSN?
  nas<-biotraits3[is.na(biotraits3$TSN),]

#first check name consistency of taxa without NAs for TSN, then check for taxa with NAs using SubjectTaxonomicName. Keep DFs of old names and new names for each to remove old names from occurrence data
ids<-biotraits3[!is.na(biotraits3$TSN),] #create DF of taxa without NA in TSN
ids$TSN<-sub("*$", "", ids$TSN) #remove * on TSN
ids2<-unique(ids$TSN)
ids2<-as.numeric(ids2) #convert to numeric
ids2<-na.omit(ids2) #some NAs introduced- remove

itis_names<-itis_acceptname(searchtsn = ids2) #get a vector of accepted names for each tsn

#subset to entries *without NA* in acceptedname
itis_names_new<-itis_names[!is.na(itis_names$acceptedname),] #check that these are all aquatic insects

#need to match new names and TSNs with entries in dataset, and then create a column for original name and original TSN so that I can match up with records in occurrence dataset
#entries for which TSN has not changed will have their original TSN and name in these new columns
#write list of accepted TSN (no NAs) to csv
write.csv(itis_names, "accepted_TSN.csv")

#read in from file
itis_names<-read.csv("accepted_TSN.csv", stringsAsFactors = FALSE)

#change genus, family, and order columns to match accepted name
#which genera have NA but SubjectTaxonomicName is filled in?
genus.na<-biotraits3[is.na(biotraits3$Genus) & !is.na(biotraits3$SubjectTaxonomicName) & (biotraits3$SubjectTaxonomicName != biotraits3$Family),]
#assign genus entries for taxa that have genus and/or species filled in SubjectTaxonomicName
biotraits3$Genus[which(biotraits3$SubjectTaxonomicName == "Listronotus sparsus"  )]<-"Listronotus"
biotraits3$SubjectTaxonomicName[which(biotraits3$SubjectTaxonomicName == "Chironomini Genus B Pinder Reiss"  )]<-"Chironomini"
biotraits3[which(biotraits3$SubjectTaxonomicName == "Chironomini Genus B Pinder Reiss"  ),]
biotraits3$Genus[which(biotraits3$SubjectTaxonomicName == "Metretopus borealis"  )]<-"Metretopus"
biotraits3$Genus[which(biotraits3$SubjectTaxonomicName == "Ameletus"  )]<-"Ameletus"

#which families have NA but SubjectTaxonomicName is filled in? None
family.na<-biotraits3[is.na(biotraits3$Family) & !is.na(biotraits3$SubjectTaxonomicName) & (biotraits3$SubjectTaxonomicName != biotraits3$Order),]

#which entries *have NAs* for TSN?
nas<-biotraits3[is.na(biotraits3$TSN),]
nas2<-unique(nas$SubjectTaxonomicName)

#check names of taxa *with NA* in TSN
valid_names<-tax_name(query=nas2, get=c("order","family", "genus", "species"), db="itis")
write.csv(valid_names, "valid_names.csv")

#Check names from valid_names that have NA for family, genus, species. Match names of taxa from valid_names with biotraits 3. Match TSN from itis_names with biotraits3. After everyhting lines up, check remaining TSN for taxa with NA in TSN using their accepted name
#create new column for accepted name and accepted tsn
biotraits3$Accepted_name<-NA
biotraits3$Accepted_TSN<-NA

#rename TSN to Original_TSN 
colnames(biotraits3)[42]<-"Original_TSN"

#rename submittedtsn in itis_names to "Original_TSN" and acceptedtsn to "Accepted_TSN" and acceptedname to "Accepted_name"
itis_names2<-as.data.frame(cbind(as.character(itis_names$submittedtsn), as.character(itis_names$acceptedtsn), itis_names$acceptedname))
colnames(itis_names2)[1]<-"Original_TSN"
colnames(itis_names2)[3]<-"Accepted_name"
colnames(itis_names2)[2]<-"Accepted_TSN"

#merge itis_names with biotraits3 on "Original_TSN"
biotraits4<-left_join(biotraits3, itis_names2, by="Original_TSN")
#remove extra columns
biotraits4$Accepted_name.x<-NULL
biotraits4$Accepted_TSN.x<-NULL
colnames(biotraits4)[46]<-"Accepted_TSN"
colnames(biotraits4)[47]<-"Accepted_Name"

#get TSN and accepted name for taxa with NA in accepted TSN
tsn_for_NAs<-get_tsn(nas2, searchtype="scientific", accepted=FALSE, ask=TRUE)
tsn<-cbind(tsn_for_NAs, nas2)
tsn2<-as.data.frame(tsn, stringsAsFactors = FALSE)

#merge biotraits4 with tsn2 on SubjectTaxonomicName
#rename tsn_for_NAs
colnames(tsn2)[1]<-"Accepted_TSN"
colnames(tsn2)[2]<-"SubjectTaxonomicName"
biotraits5<-left_join(biotraits4, tsn2, by="SubjectTaxonomicName")

#for entries with NA in Accepted_TSN.x take entry in Accepted_TSN.y
#change Accepted_TSN.x to character vectors
biotraits5$Accepted_TSN.x <- if_else(is.na(biotraits5$Accepted_TSN.x), biotraits5$Accepted_TSN.y, as.character(biotraits5$Accepted_TSN.x))

#check which entries in Accepted_TSN.x and Accepted_TSN.y do not match
mismatch<-biotraits5[which(biotraits5$Accepted_TSN.x != biotraits5$Accepted_TSN.y),] #81 entries don't match. why?
#accepted.TSN.x was generated from itis_names2, and accepted.TSN.y was generated from tsn2. itis_names2 has valid names, so TSN.x should be valid.
#tsn2 generated tsn from SubjectTaxonomicName for taxa with nA in TSN, so could be wrong. Take TSN.x as final vector of accepted TSN
#remove TSN.y
biotraits5$Accepted_TSN.y<-NULL
colnames(biotraits5)[46]<-"Accepted_TSN"

#Double-check that all TSN and names from Accepted_TSN are correct
ids_new<-biotraits5[!is.na(biotraits5$Accepted_TSN),] #create DF of taxa without NA in TSN
ids_new2<-unique(biotraits5$Accepted_TSN)
ids_new3<-na.omit(as.numeric(ids_new2)) #convert to numeric

itis_names_new<-itis_acceptname(searchtsn = ids_new3) #still a few mismatches to fix, then need to get matching Order, Family, Genus for accepted names

#merge accepted tsn and acceptedname from itis-Names_new with biotraits5 on submitted tsn- do only for those that had a mismatch betweem submitted and accepted tsn
colnames(itis_names_new)[1]<-"Original_TSN"
colnames(itis_names_new)[2]<-"Accepted_name"
colnames(itis_names_new)[3]<-"Accepted_TSN"
itis_names_new2<-as.data.frame(cbind(as.character(itis_names_new$Original_TSN),itis_names_new$Accepted_name, itis_names_new$Accepted_TSN))
colnames(itis_names_new2)[1]<-"Original_TSN"
colnames(itis_names_new2)[2]<-"Accepted_Name"
colnames(itis_names_new2)[3]<-"Accepted_TSN"
biotraits6<-left_join(biotraits5, itis_names_new2, by="Original_TSN")

#Do any accepted TSN not match for itis_names_new2 and biotraits5?
mismatch2<-biotraits6[which(biotraits6$Accepted_TSN.x != biotraits6$Accepted_TSN.y),] #no mistmatches
#drop accepted tsn.y
biotraits6$Accepted_TSN.y<-NULL
colnames(biotraits6)[46]<-"Accepted_TSN"
#drop accepted_name.y and take accepted.name.x
biotraits6$Accepted_Name.y<-NULL
colnames(biotraits6)[47]<-"Accepted_Name"

#For entries in Accepted_name, need to generate Genus, Family, Order names
#are they in valid_names?
accepted_name<-biotraits6[!is.na(biotraits6$Accepted_Name),]
valid_names2<-tax_name(query=unique(accepted_name$Accepted_Name), get=c("order","family", "genus"), db="itis")
valid_names3<-cbind(unique(accepted_name$Accepted_Name), valid_names2)

#left join valid_names3 with biotraits6 to combine genus, family, order columns in biotraits6 with genus, family, order columns from valid_names3
#first rename columns in valid_names3
colnames(valid_names3)[4:6]<-c("Order", "Family", "Genus")
colnames(valid_names3)[1]<-"Accepted_Name"
biotraits7<-left_join(biotraits6, valid_names3, by="Accepted_Name")

#drop unneeded columns from valid_names3
biotraits7$db<-NULL
biotraits7$query<-NULL

#for rows for which Accepted_Name does not equal NA, replace entries in Genus.x with Genus.y, Family.x with Family.y, etc.
biotraits7$Genus.x <- if_else(!is.na(biotraits7$Accepted_Name), biotraits7$Genus.y, biotraits7$Genus.x)
biotraits7$Family.x <- if_else(!is.na(biotraits7$Accepted_Name), biotraits7$Family.y, biotraits7$Family.x)
biotraits7$Order.x <- if_else(!is.na(biotraits7$Accepted_Name), biotraits7$Order.y, biotraits7$Order.x)

#drop extra columns for Family, Genus, Order
biotraits7[48:50]<-NULL
#Rename remaining columns
colnames(biotraits7)[colnames(biotraits7)=="Genus.x"] <- "Genus"
colnames(biotraits7)[colnames(biotraits7)=="Family.x"] <- "Family"
colnames(biotraits7)[colnames(biotraits7)=="Order.x"] <- "Order"

#now combine SubjectTaxonomicName and Accepted_Name columns, for any Accepted_Name equals NA, take value in SubjectTaxonomicName
biotraits7$Accepted_Name<- if_else(is.na(biotraits7$Accepted_Name), as.character(biotraits7$SubjectTaxonomicName), as.character(biotraits7$Accepted_Name))

#how many taxa were reassigned names?
biotraits_new_name<-biotraits7[which(biotraits7$SubjectTaxonomicName != biotraits7$Accepted_Name),]
length(unique(biotraits_new_name$SubjectTaxonomicName)) #336 assigned new names

#how many genera were reassigned names?
biotraits_new_genus<-biotraits7[which(biotraits7$SubjectTaxonomicName!=biotraits7$Accepted_Name & biotraits7$Accepted_Name == biotraits7$Genus),] #14 assigned new names
length(unique(biotraits_new_genus$SubjectTaxonomicName)) #8 unique input genera
length(unique(biotraits_new_genus$Accepted_Name)) #6 unique output genera with reassigned names

#################Merge corrected citations with trait dataset#################################################################
length(unique(biotraits7$Study_Citation_abbrev))
length(unique(citations_new$Study_Citation_abbrev))
#read in data
citations_new<-read.csv("corrected_citations_2_26_EH.csv", header=TRUE, stringsAsFactors = FALSE)
colnames(citations_new)<-c("Study_Citation_abbrev", "Study_Citation")
citations_new<-citations_new[-1, ]
#merge with traits database
biotraits7.1<-merge(biotraits7, citations_new, by="Study_Citation_abbrev", all =TRUE)
#tahe a look at how biotraits7 and citations_new align
cit<-biotraits7.1[, c(1,36 ,48)]
#check entries for which there is NA in Study_Citation.x but not y
biotraits7.1[is.na(biotraits7.1$Study_Citation.x),]
#check entries for which there is NA in Study_Citation.y
biotraits7.1[is.na(biotraits7.1$Study_Citation.y),]
#replace Study_Citation_abbrev for Arnett & Thomas 2001 with Arnett & Thomas 2000 in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Arnett & Thomas 2001"  )]<-"Arnett & Thomas 2000"
#replace Study_Citation_abbrev for Bailey1951 with Bailey 1951 in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Bailey1951"  )]<-"Bailey 1951"
#replace Study_Citation_abbrev for Blackenhorn & Fairbairn with Blackenhorn & Fairbairn 2002 in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Blackenhorn & Fairbairn"  )]<-"Blackenhorn & Fairbairn 2002"
#replace Study_Citation_abbrev for Davis 1995 with Davis 1996 in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Davis 1995"  )]<-"Davis"
#replace Study_Citation_abbrev for Leschen et al.  2003 with Leschen et al. 2003 in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Leschen et al.  2003"  )]<-"Leschen et al. 2003"
#replace Merrit et al. 1992 in citations_new with Merritt et al. 1992
citations_new$Study_Citation_abbrev[which(citations_new$Study_Citation_abbrev == "Merrit et al. 1992")]<-"Merritt et al. 1992"
#replace Study_Citation_abbrev for Merritt, Cummins, Berg 2008 with Merritt, Cummins & Berg 2008 in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Merritt, Cummins, Berg 2008"  )]<-"Merritt, Cummins & Berg 2008"
#replace Study_Citation_abbrev for Moller Pillot 2013 with Moller & Pillot 2013 in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Moller Pillot 2013"  )]<-"Moller & Pillot 2013"
#replace Study_Citation_abbrev for Needham et al. 2014 and Study_Citation Needham, James G., et al.Dragonflies of North America: the Odonata (Anisoptera) fauna of Canada, the continental United States, northern Mexico and the Greater Antilles. Scientific Publishers, 2014. in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Needham et al. 2014"&biotraits7$Study_Citation=="Needham, James G., et al.Dragonflies of North America: the Odonata (Anisoptera) fauna of Canada, the continental United States, northern Mexico and the Greater Antilles. Scientific Publishers, 2014.") ]<-"Needham et al. 2014a"
#replace Study_Citation_abbrev for Needham et al. 2014 and Study_Citation Needham, James G., et al.Damselflies of North America: the Odonatafauna of Canada, the continental United States, northern Mexico and the Greater Antilles. Scientific Publishers, 2014. in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Needham et al. 2014"&biotraits7$Study_Citation=="Needham, James G., et al.Damselflies of North America: the Odonatafauna of Canada, the continental United States, northern Mexico and the Greater Antilles. Scientific Publishers, 2014.") ]<-"Needham et al. 2014b"
#replace Study_Citation_abbrev for Needham et al. 2014 and Study_Citation Needham, James G., et al. Damselflies of North America: the Odonata (Zygoptera) fauna of Canada, the continental United States, northern Mexico and the Greater Antilles. Revised ed., Scientific Publishers, 2014. in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Needham et al. 2014"&biotraits7$Study_Citation=="Needham, James G., et al. Damselflies of North America: the Odonata (Zygoptera) fauna of Canada, the continental United States, northern Mexico and the Greater Antilles. Revised ed., Scientific Publishers, 2014.") ]<-"Needham et al. 2014b"
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Needham et al. 2014"&biotraits7$Study_Citation=="Needham, James G., et al. Damselflies of North America: the Odonatafauna of Canada, the continental United States, northern Mexico and the Greater Antilles. Scientific Publishers, 2014.")]<-"Needham et al. 2014b"
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Needham et al. 2014"&biotraits7$Study_Citation=="Needham, James G., et al.Dragonflies of North America: the Odonata (Anisoptera) fauna of Canada, the continental United States, northern Mexico and the Greater Antilles. Revised ed., Scientific Publishers, 2014.")]<-"Needham et al. 2014a"
#replace Study_Citation_abbrev for Tachet et al. 2000 with Tachet 2000 in biotraits7
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Tachet et al. 2000") ]<-"Tachet 2000"
#replace Study_Citation_abbrev for Teskey 1970 and Study_Citation Teskey, H. J. A REVIEW OF THE GENUS GLUTOPS (DIPTERA: PELECORHYNCHIDAE), WITH DESCRIPTIONS OF FOUR NEW SPECIES.The Canadian Entomologist, vol. 102, no. 9, 1970, pp. 1171–1179., doi:10.4039/Ent1021171-9. in biotraits7 with Teskey 1970a
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Teskey 1970"&biotraits7$Study_Citation=="Teskey, H. J. A REVIEW OF THE GENUS GLUTOPS (DIPTERA: PELECORHYNCHIDAE), WITH DESCRIPTIONS OF FOUR NEW SPECIES.The Canadian Entomologist, vol. 102, no. 9, 1970, pp. 1171–1179., doi:10.4039/Ent1021171-9.") ]<-"Teskey 1970a"
#replace Study_Citation_abbrev for Teskey 1970 and Study_Citation Teskey, H. J. THE IMMATURE STAGES AND PHYLETIC POSITION OF GLUTOPS ROSSI (DIPTERA: PELECORHYNCHIDAE). The Canadian Entomologist 102, no. 9 (September 1970): 1130–35. https://doi.org/10.4039/Ent1021130-9. with Teskey 1970b.
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Teskey 1970"&biotraits7$Study_Citation=="Teskey, H. J. THE IMMATURE STAGES AND PHYLETIC POSITION OF GLUTOPS ROSSI (DIPTERA: PELECORHYNCHIDAE). The Canadian Entomologist 102, no. 9 (September 1970): 1130–35. https://doi.org/10.4039/Ent1021130-9.") ]<-"Teskey 1970b"
#replace Study_Citation_abbrev for Teskey 1970 and Study_Citation Teskey, H. J. DIPTERA LARVAE ASSOCIATED WITH TREES IN NORTH AMERICA. The Memoirs of the Entomological Society of Canada 108, no. S100 (ed 1976): 1–53. https://doi.org/10.4039/entm108100fv. with Teskey 1970c
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Teskey 1970"&biotraits7$Study_Citation=="Teskey, H. J. DIPTERA LARVAE ASSOCIATED WITH TREES IN NORTH AMERICA.  The Memoirs of the Entomological Society of Canada 108, no. S100 (ed 1976): 1–53. https://doi.org/10.4039/entm108100fv.") ]<-"Teskey 1976"
#replace Study_Citation_abbrev for Wissinger 1988 and Study_Citation "Wissinger, Scott A. “Spatial Distribution, Life History and Estimates of Survivorship in a Fourteen-Species Assemblage of Larval Dragonflies (Odonata: Anisoptera).” Freshwater Biology 20, no. 3 (December 1, 1988): 329–40. https://doi.org/10.1111/j.1365-2427.1988.tb00458.x."
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Wissinger 1988"&biotraits7$Study_Citation=="Wissinger, Scott A. “Spatial Distribution, Life History and Estimates of Survivorship in a Fourteen-Species Assemblage of Larval Dragonflies (Odonata: Anisoptera).” Freshwater Biology 20, no. 3 (December 1, 1988): 329–40. https://doi.org/10.1111/j.1365-2427.1988.tb00458.x.") ]<-"Wissinger 1988b"
#replace Study_Citation_abbrev for Wissinger 1988 and Study_Citation "Wissinger, Scott A. “Life History and Size Structure of Larval Dragonfly Populations.” Journal of the North American Benthological Society 7, no. 1 (1988): 13–28. https://doi.org/10.2307/1467827."
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Wissinger 1988"&biotraits7$Study_Citation=="Wissinger, Scott A. “Life History and Size Structure of Larval Dragonfly Populations.” Journal of the North American Benthological Society 7, no. 1 (1988): 13–28. https://doi.org/10.2307/1467827.") ]<-"Wissinger 1988a"
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Wilson 1968"&biotraits7$Study_Citation=="Wilson, Louis F. LIFE HISTORY AND HABITS OF RHABDOPHAGA SP. (DIPTERA: CECIDOMYIIDAE), A GALL MIDGE ATTACKING WILLOW IN MICHIGAN. The Canadian Entomologist 100, no. 2 (February 1968): 184–89. https://doi.org/10.4039/Ent100184-2.") ]<-"Wilson 1968a"

biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Watson & Dallwitz 2003"&biotraits7$Study_Citation=="Watson, L., and Dallwitz, M.J. 2003 onwards. British insects: the families of Diptera. Version: 1st January 2012.http://delta-intkey.com.") ]<-"Watson & Dallwitz 2003a" 
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Watson & Dallwitz 2003"&biotraits7$Study_Citation=="Watson, L., and Dallwitz, M.J. 2003 onwards. British insects: water beetles. Version: 18th September 2012.http://delta-intkey.com") ]<-"Watson & Dallwitz 2003b" 
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Wilson 1968"&biotraits7$Study_Citation=="Wilson, Louis F. LIFE HISTORY AND HABITS OF THE WILLOW BEAKED GALL MIDGE,MAYETIOLA RIGIDAE (DIPTERA: CECIDOMYIIDAE), IN MICHIGAN. The Canadian Entomologist 100, no. 2 (February 1968): 202–6. https://doi.org/10.4039/Ent100202-2.") ]<-"Wilson 1968b"
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Schoen 2018" & biotraits7$Study_Citation=="Schoen, Jerry. \"The River's Calender\". iNaturalist.org. 2017") ]<-"Schoen 2017"
biotraits7$Study_Citation_abbrev[which(biotraits7$Study_Citation_abbrev == "Schoen 2018" & biotraits7$Study_Citation=="Schoen, Jerry. \"The River's Calender\". iNaturalist.org. 2019") ]<-"Schoen 2019"

#overwrite old version of biotraits 7.1 with new Citation_abbrev
biotraits7.1<-merge(biotraits7, citations_new, by="Study_Citation_abbrev", all =TRUE)

#take study_citation.y unless there is an NA in that column, then take study_citation.x
#for entries with NA in Accepted_TSN.x take entry in Accepted_TSN.y
#change Accepted_TSN.x to character vectors
biotraits7.1$Study_Citation <- if_else(is.na(biotraits7.1$Study_Citation.y), biotraits7.1$Study_Citation.x, biotraits7.1$Study_Citation.y)

##########Final raw trait cleaning###########################################################################################
#drop Study_Citation.x and Study_Citation.y
biotraits7.2<-biotraits7.1[,-c(36,48)]

#drop unneeded columns: TraitRecordID, Terrestrial
biotraits7.3<-biotraits7.2[,-c(38,40)]

#revalue "Published" column
unique(biotraits7.3$Published)
revalue(biotraits7.3$Published[biotraits7.3$Published == " Yes"] <- "yes")
revalue(biotraits7.3$Published[biotraits7.3$Published == "Yes"] <- "yes")
revalue(biotraits7.3$Published[biotraits7.3$Published == "No"] <- "no")
published.na<-biotraits7.3[is.na(biotraits7.3$Published),]
#drop any rows that have NA in Study_Citation_abbrev
biotraits7.4<-biotraits7.3[!is.na(biotraits7.3$Study_Citation_abbrev),]
#Published==no for any row with Study_Citation_abbrev==na
revalue(biotraits7.4$Published[biotraits7.4$Study_Citation_abbrev=="Pyne traits"]<-"no")
#remaining citations with NA in published have been published- revalue to "yes"
revalue(biotraits7.4$Published[is.na(biotraits7.4$Published)]<-"yes")

#add column for taxonomic resolution
#genus
#initialize column
biotraits7.4$Taxonomic_resolution<-NA
revalue(biotraits7.4$Taxonomic_resolution[!is.na(biotraits7.4$Genus)]<-"Genus")
#family
revalue(biotraits7.4$Taxonomic_resolution[is.na(biotraits7.4$Genus) & !is.na(biotraits7.4$Family)]<-"Family")

#check the resolution of insects assigned to family and na
family<-biotraits7.4[which(biotraits7.4$Taxonomic_resolution =="Family"),]
unique(family$Family)

na.check<-biotraits7.4[is.na(biotraits7.4$Taxonomic_resolution),] #all are Orders
revalue(biotraits7.4$Taxonomic_resolution[is.na(biotraits7.4$Taxonomic_resolution)]<-"Order")

#translate a few emerge season comments to emerge_season code
#which taxa have an NA in Emerge_season_1 but no NA in Emerge_season_comments?
na.emerge<-biotraits7.4[which(is.na(biotraits7.4$Emerge_season_1)&!is.na(biotraits7.4$Emerge_season_comments)),]
#what are the entries in comments for those taxa?
unique(na.emerge$Emerge_season_comments)
#assign values to Emerge_season_1
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="Fall")]<-"Fall"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: spring")]<-"Spring"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: fall")]<-"Fall"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter")]<-"Winter"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: summer")]<-"Summer"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, spring, summer, fall")]<-"Winter"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, spring, summer, fall")]<-"Fall"

biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, spring, summer")]<-"Winter"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, spring, summer")]<-"Summer"

biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, summer, fall")]<-"Winter"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, summer, fall")]<-"Fall"

biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: spring, summer, fall")]<-"Spring"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: spring, summer, fall")]<-"Fall"

biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, spring, fall")]<-"Winter"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, spring, fall")]<-"Fall"

biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: summer, fall")]<-"Summer"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: summer, fall")]<-"Fall"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, summer")]<-"Winter"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, summer")]<-"Summer"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, spring")]<-"Winter"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, spring")]<-"Spring"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: spring, summer")]<-"Spring"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: spring, summer")]<-"Summer"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, fall")]<-"Winter"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: winter, fall")]<-"Fall"
biotraits7.4$Emerge_season_1[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: spring, fall")]<-"Spring"
biotraits7.4$Emerge_season_2[which(biotraits7.4$Emerge_season_comments=="sexually mature form reported in: spring, fall")]<-"Fall"

#which taxa have NA in Voltinism_abbrev but not in Volt_comments?
volt.na<-biotraits7.4[which(is.na(biotraits7.4$Voltinism_abbrev)&!is.na(biotraits7.4$Volt_comments)),] #ok
volt<-biotraits7.4[which(is.na(biotraits7.4$Voltinism_abbrev)&!is.na(biotraits7.4$Voltinism)),] #ok
#feed mode- need to add values for some taxa based on comments
other<-biotraits7.4[which(!is.na(biotraits7.4$Feed_mode_comments) & is.na(biotraits7.4$Feed_prim_abbrev)),]
biotraits7.4$Feed_prim_abbrev[which(is.na(biotraits7.4$Feed_prim_abbrev)&biotraits7.4$Feed_mode_comments== "Feed on green algae.")]<-"HB"
biotraits7.4$Feed_prim_abbrev[which(is.na(biotraits7.4$Feed_prim_abbrev)&biotraits7.4$Feed_mode_comments== "Feed on algae growing on Eleocharis and Chara.")]<-"HB"
biotraits7.4$Feed_prim_abbrev[which(is.na(biotraits7.4$Feed_prim_abbrev)&biotraits7.4$Feed_mode_comments== "Feed on microflora and periphyton on rocks.")]<-"HB"
biotraits7.4$Feed_prim_abbrev[which(is.na(biotraits7.4$Feed_prim_abbrev)&biotraits7.4$Feed_mode_comments== "Feed on periphyton and vascular plants.")]<-"HB"

#keep feed_mode_comments

#drop female_disp_old
biotraits7.4<-biotraits7.4[,-19]

#are there any instances of Max_body_size_abbrev as NA but no NA in Max_body_size- OK
na.body<-biotraits7.4[which(!is.na(biotraits7.4$Max_body_size) & is.na(biotraits7.4$Max_body_size_abbrev)),]

#drop Max_body_size
biotraits7.4<-biotraits7.4[,-23]

#drop Adult from dataset
biotraits7.4<-biotraits7.4[,-3]

#any instances of na in AdultFlyingStrength_abbrev and not in AdultFlyingStrentgh comments?- OK
na.flight<-biotraits7.4[which(!is.na(biotraits7.4$AdultFlyingStrength_comments) & is.na(biotraits7.4$AdultFlyingStrength_abbrev)),]

#drop AdultFlyingStrengthComments
biotraits7.4<-biotraits7.4[,-5]

#keep Emerge_season_comments

#check match of Emerge_synch and Emerge_synch_abbrev- OK
na.synch<-biotraits7.4[which(!is.na(biotraits7.4$AEmerge_synch) & is.na(biotraits7.4$Emerge_synch_abbrev)),]

#drop Emerge_synch
biotraits7.4<-biotraits7.4[,-8]

#check match of Emerge_synch and Emerge_synch_comments- OK
na.synch<-biotraits7.4[which(!is.na(biotraits7.4$Emerge_synch_comments) & is.na(biotraits7.4$Emerge_synch_abbrev)),]

#drop Emerge_synch_comments
biotraits7.4<-biotraits7.4[,-9]

#check Female_disp_comments and Female_disp_abbrev-ok
na.disp<-biotraits7.4[which(!is.na(biotraits7.4$Female_disp_comments) & is.na(biotraits7.4$Female_disp_abbrev)),]

#remove Female_disp_comments
biotraits7.4<-biotraits7.4[,-14]

#drop notes
biotraits7.4<-biotraits7.4[,-19]

#check habit_prim and habit_comments
na.habit<-biotraits7.4[which(!is.na(biotraits7.4$Habit_comments) & is.na(biotraits7.4$Habit_prim)),]
#fix assignment for Rhithrogena based on comments
biotraits7.4$Habit_prim[which(biotraits7.4$Genus=="Rhithrogena")]<-"Clinger"
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Genus=="Rhithrogena")]<-"eros"
biotraits7.4$Thermal_pref[which(biotraits7.4$Genus=="Rhithrogena")]<-"Cold-cool eurythermal (0-15 C)"

#keep habit comments

#Clean thermal_pref data (some entries for resp included)
unique(biotraits7.4$Thermal_pref)
biotraits7.4$Thermal_pref[which(biotraits7.4$Thermal_pref=="Cool-Warm eurythermal (5-30 C)")]<-"Cool-warm eurythermal (5-30 C)"
biotraits7.4$Thermal_pref[which(biotraits7.4$Thermal_pref=="warm eurythermal (15-30 C)")]<-"Warm eurythermal (15-30 C)"
biotraits7.4$Thermal_pref[which(biotraits7.4$Thermal_pref=="Hot eurythermal (>30 C),")]<-"Hot euthermal (>30 C)"
#which taxa have a resp assignment in Thermal_pref?
biotraits7.4[which(biotraits7.4$Thermal_pref=="Tegument"),]
#assign Tegument to Resp and fix Thermal pref assignment
biotraits7.4$Resp_abbrev[which(biotraits7.4$Thermal_pref=="Tegument")]<-"Tegument"
biotraits7.4$Thermal_pref[which(biotraits7.4$Thermal_pref=="Tegument" & biotraits7.4$SubjectTaxonomicName=="Archilestes grandis" & biotraits7.4$Study_Citation_abbrev=="Schriever et al. 2015")]<-NA
biotraits7.4[which(biotraits7.4$Thermal_pref=="gills"),]
biotraits7.4$Resp_abbrev[which(biotraits7.4$Thermal_pref=="gills")]<-"Gills"
biotraits7.4$Thermal_pref[which(biotraits7.4$Thermal_pref=="gills")]<-NA

#drop Voltinism
biotraits7.4<-biotraits7.4[,-33]

#check rheophily and rheophily comments
na.rheo<-biotraits7.4[which(!is.na(biotraits7.4$Rheophily_comments) & is.na(biotraits7.4$Rheophily_abbrev)),] #need to assign rheophily based on comments
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Rheophily_comments=="Fast"& is.na(biotraits7.4$Rheophily_abbrev))]<-"Eros"
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Rheophily_abbrev=="Eros")]<-"eros"
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Rheophily_comments=="Standing" &is.na(biotraits7.4$Rheophily_abbrev))]<-"depo"
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Rheophily_comments=="Standing-slight" &is.na(biotraits7.4$Rheophily_abbrev))]<-"depo"
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Rheophily_comments=="Standing and flowing" &is.na(biotraits7.4$Rheophily_abbrev))]<-"depo_eros"
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Rheophily_comments=="Slight" &is.na(biotraits7.4$Rheophily_abbrev))]<-"depo_eros"
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Rheophily_comments=="Moderate" &is.na(biotraits7.4$Rheophily_abbrev))]<-"eros"
biotraits7.4$Rheophily_abbrev[which(biotraits7.4$Rheophily_comments=="Moderate-fast" &is.na(biotraits7.4$Rheophily_abbrev))]<-"eros"

#check values for a few taxa
biotraits7.4[which(biotraits7.4$Genus=="Caenis"),] #OK
biotraits7.4[which(biotraits7.4$Genus=="Paraleptophlebia"),] #OK
biotraits7.4[which(biotraits7.4$Genus=="Allocapnia"),] #OK

#drop rheophily comments
biotraits7.4<-biotraits7.4[,-27]

#which taxa have an NA in Order?
biotraits7.4[which(is.na(biotraits7.4$Order)),]
biotraits7.4[2537,19]<-"Coleoptera"
biotraits7.4[2974,19]<-"Odonata"
biotraits7.4[c(3535,3593),19]<-"Odonata"

biotraits7.4$Voltinism_abbrev[which(biotraits7.4$Voltinism_abbrev=="uin")]<-"Univoltine"

#which taxa have no accepted TSN?
biotraits7.4[is.na(biotraits7.4$Accepted_TSN),]
#assign TSN
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Ranatra dispar")]<-1085746
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Erythemis simplicocollis")]<-"Erythemis simplicicollis"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Erythemis simplicicollis")]<-101866
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Drunella doddsiii")]<-"Drunella doddsii"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Drunella doddsii")]<-698494
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Simulium Phosterodoros ")]<-"Simulium"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Simulium")]<-126774
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Chironomini Genus B Pinder Reiss ")]<-"Chironomini"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Chironomini")]<-129229
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Agabus spp ")]<-"Agabus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Agabus")]<-111966
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Gyrinus spp ")]<-"Gyrinus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Gyrinus")]<-112654
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Limnephilus consocius")]<-"Anabolia consocia"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Anabolia consocia")]<-115959
biotraits7.4$Genus[which(biotraits7.4$Accepted_Name=="Anabolia consocia")]<-"Anabolia"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Paraboreochlus stahli")]<-127977
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Plumiperla Haploperla")]<-"Plumiperla"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Plumiperla")]<-103305
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Cladotanytarsus vanderwulpi")]<-"Cladotanytarsus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Cladotanytarsus")]<-129873
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Baetis garcianus")]<-"Baetis"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Baetis")]<-100800
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Cloeodes maculipes")]<-"Cloeodes"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Cloeodes consignatus")]<-"Cloeodes"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Cloeodes")]<-568549
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Simulium venustrum")]<-"Simulium venustum"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Simulium venustum")]<-126892
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Einfeldia paganus")]<-"Einfeldia pagana"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Einfeldia pagana")]<-129465
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Peltodytes duodecimpunctatus")]<-"Peltodytes duodecimpuntatus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Peltodytes duodecimpuntatus")]<-111926
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Labrundinia beckae")]<-"Labrundinia becki"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Labrundinia becki")]<-128174
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Chironomus tenuistylus")]<-"Chironomus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Chironomus")]<-129254
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Parachaetocladius abnobaeus")]<-"Parachaetocladius"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Parachaetocladius")]<-128951
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Ptychoptera lenis")]<-"Ptychoptera"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Ptychoptera")]<-125786
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Rheotanytarsus exiquus")]<-"Rheotanytarsus exiguus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Rheotanytarsus exiguus")]<-129957
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Pyrrhalta aenescens")]<-"Pyrrhalta"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Pyrrhalta maculicollis")]<-"Pyrrhalta"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Pyrrhalta")]<-114546
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Erpetogomphus heterodon")]<-592844
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Stempellinella flavidula")]<-"Stempellinella"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Stempellinella leptocelloides")]<-"Stempellinella"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Stempellinella")]<-129969
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Polypedilum flavum")]<-"Polypedilum"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Polypedilum albicorne")]<-"Polypedilum"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Polypedilum")]<-129657
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Nanocladius iniplenus")]<-"Nanocladius spiniplenus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Nanocladius spiniplenus")]<-128862
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Thienemanniella lobapodema")]<-"Thienemanniella"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Thienemanniella taurocapita")]<-"Thienemanniella"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Thienemanniella")]<-129182
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Alotanypus venustus")]<-"Alotanypus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Alotanypus")]<-206646
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Lispoides guatemala")]<-"Lispoides"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Lispoides")]<-150805
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Triznaka diversa")]<-"Triznaka"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Triznaka")]<-103308
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Capnia limata")]<-"Capnia"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Capnia ligulata")]<-"Capnia"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Capnia")]<-102688
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Taenionema vanduzeeum")]<-"Taenionema"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Taenionema")]<-102519
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Pseudacteon curvatus")]<- "Pseudacteon"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Pseudacteon")]<-139567
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Donacia flavipes")]<- "Donacia"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Donacia")]<-114510
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Centroptilum barri")]<- "Centroptilum"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Centroptilum")]<-100873
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Hydropsyche aenigma")]<- "Hydropsyche"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Hydropsyche racona")]<- "Hydropsyche"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Hydropsyche bicornuta")]<- "Hydropsyche"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Hydropsyche jewetti")]<- "Hydropsyche"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Hydropsyche toschiae")]<- "Hydropsyche"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Hydropsyche vanaca")]<- "Hydropsyche"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Hydropsyche intrica")]<- "Hydropsyche"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Hydropsyche")]<-115453
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Orthotrichia provosti")]<- "Orthotrichia"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Orthotrichia")]<-115828
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Procloeon appalachia")]<- "Procloeon"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Procloeon")]<-206622
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Serratella lawsoni")]<- "Serratella"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Serratella")]<-101395
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Micrasema kluane")]<- "Micrasema"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Micrasema")]<-116958
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Oxyethira leonensis")]<- "Oxyethira"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Oxyethira")]<-115779
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Phylloicus mexicanus")]<- "Phylloicus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Phylloicus")]<-116540
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Baetis moki")]<- "Baetis"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Baetis")]<-100800
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Diplectrona margarita")]<- "Diplectrona"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Diplectrona")]<-115399
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Hydroptila choccolocco")]<- "Hydroptila"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Hydroptila")]<-115641
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Cheumatopsyche flinti")]<- "Cheumatopsyche"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Cheumatopsyche")]<-115408
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Ceraclea enodis")]<- "Ceraclea"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Ceraclea")]<-116684
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Lepidostoma styliferum")]<- "Lepidostoma stylifer"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Lepidostoma stylifer")]<-116843
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Epeorus pleurialis")]<- "Epeorus pleuralis"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Epeorus pleuralis")]<-100642
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Homoplectra sierra")]<- "Homoplectra"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Homoplectra")]<-115618
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Glossosoma penitum")]<- "Glossosoma penitus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Glossosoma penitus")]<-117165
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Mayetiola rigidae")]<- "Mayetiola"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Mayetiola")]<-124080
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Parachironomus demeijera")]<- "Parachironomus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Parachironomus")]<-129564
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Metriocnemus abdomino flavatus")]<- "Metriocnemus"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Metriocnemus")]<-128821

#add column for taxonomic resolution
#genus
#initialize column
biotraits7.4$Taxonomic_resolution<-NA
revalue(biotraits7.4$Taxonomic_resolution[!is.na(biotraits7.4$Genus)]<-"Genus")
#family
revalue(biotraits7.4$Taxonomic_resolution[is.na(biotraits7.4$Genus) & !is.na(biotraits7.4$Family)]<-"Family")

#check the resolution of insects assigned to family and na
family<-biotraits7.4[which(biotraits7.4$Taxonomic_resolution =="Family"),]
unique(family$Family)

na.check<-biotraits7.4[is.na(biotraits7.4$Taxonomic_resolution),] #all are Orders
revalue(biotraits7.4$Taxonomic_resolution[is.na(biotraits7.4$Taxonomic_resolution)]<-"Order")

#fix resolution assignment- need to assign resolution for Species based on accepted name
biotraits7.4$TARGET <- grepl(" ", biotraits7.4$Accepted_Name) #create new column that describes whether there is a space in Accepted_name (TRUE for species)
biotraits7.4$Taxonomic_resolution[which(biotraits7.4$TARGET=="TRUE")]<-"Species" #assign species to Taxonomic_resolution
#drop TARGET column
biotraits7.4<-biotraits7.4[,-37]
#drop Adult_disp
biotraits7.4<-biotraits7.4[,-3]
#drop Resp_adult, Resp_late and Resp_early and add to Traits_comments
biotraits7.4<-biotraits7.4[,-c(21,23,24)]

#final name check on Genus names
sort(unique(biotraits7.4$Genus), decreasing=FALSE)
biotraits7.4[which(biotraits7.4$Genus=="Ancyronx"),]
biotraits7.4$Genus[which(biotraits7.4$Genus=="Ancyronx")]<-"Ancyronyx"
biotraits7.4[which(biotraits7.4$Genus=="Camelobaetidus"),]
biotraits7.4$Genus[which(biotraits7.4$Genus=="Camelobaetidus")]<-"Camelobaetidius"
biotraits7.4[which(biotraits7.4$Genus=="Chyrandra"),]
biotraits7.4$Genus[which(biotraits7.4$Genus=="Chyrandra")]<-"Chyranda"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Chyrandra")]<-"Chyranda"
biotraits7.4$Accepted_Name[which(biotraits7.4$Accepted_Name=="Chyrandra centralis")]<-"Chyranda centralis"
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Chyranda")]<-116017
biotraits7.4$Accepted_TSN[which(biotraits7.4$Accepted_Name=="Chyranda centralis")]<-1053275
biotraits7.4[which(biotraits7.4$Genus=="Camelobaetidius"),]
biotraits7.4$Genus[which(biotraits7.4$Genus=="Dromogomphus spoliatus")]<-"Dromogomphus"
biotraits7.4$Genus[which(biotraits7.4$Genus=="Gelastocoris ")]<-"Gelastocoris"

write.csv(biotraits7.4, "biotraits_wide.csv")

biotraits7.4<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/biotraits_wide.csv")
######script needs cleaning beyond here. separate file written for calculating trait modes and affinities "Calc_trait_affinity_mode.R"

################## Change from wide to long format - need to work on this more for EDI #############################################################################################
keycol=c("Trait_group")
valuecol=c("Trait")
gathercol<-c("AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_synch_abbrev", "Feed_prim_abbrev", "Feed_mode_sec", "Female_disp_abbrev", "Habit_prim", "Habit_sec", "Max_body_size_abbrev", "Resp_abbrev", "Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev")
biotraits.long<- gather_(biotraits7.4, keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments

################### Create Ancillary_Citation table- need to work on this more for EDI #############################################################################################
ancillary_citation<-biotraits7.4[,c(1, 31, 19)]
colnames(ancillary_citation)<-c("Citation_ID", "Citation_full", "Published")
write.csv(ancillary_citation, "ancillary_citation.csv")

#check that all entries are unique

################## Rearrange raw trait table and rename columns for publication and to match US_Trait_Affinities table ##########################
#Remove "Citation_full" and "Published" from Raw_trait_table
raw_trait_table<-biotraits.long[,-c(9,17)]
colnames(raw_trait_table)[1]<-"Citation_ID"

#drop comments columns
raw_trait_table2<-raw_trait_table[, -c(3,5,7,9,13)]

#reorder columns
raw_trait_table3<-raw_trait_table2[,c(11, 2,10, 5, 3, 4, 8,9,6,7,12,13)]

#rename TSN and SubjectTaxonomicName
colnames(raw_trait_table3)[c(2,3, 7)]<-c("Submitted_name","Accepted_name", "Submitted_TSN")

#write raw trait table to csv
write.csv(raw_trait_table3, "raw_trait_table.csv")

##################Create US_trait_affinities table############################################################################################
#assign dominant trait state (mode for each taxon by Genus)- start with biotraits7.4
# Remove anything containing the term "Other"
# For now, use the most common trait value (mode) per taxon. Obviously not ideal.
mode_fn <- function(x) ifelse(any(!is.na(x)), names(which.max(table(x))), as.character(NA))

#summarizing missing traits
#How many missing trait assignments in each category?
na.count<-raw_trait_table3%>%
  group_by(Trait_group, Trait)%>%
  tally %>%
  mutate(Percent = n / sum(n))

#***************assign mode to each genus and assess how many missing trait values- change 'Other' to NA- assign zeros and 1s? Dummy code? Look at Quentin code
#count_mode <- biotraits7.4 %>%
  #mutate_all(function(x) if_else(grepl('Other', x), as.character(NA), x)) %>%
  #filter(!is.na(Genus)) %>%
  #mutate(Genus = trimws(Genus)) %>%
  #group_by(Genus) %>%
  #summarize_all(mode_fn)

#first convert any with "other" in feed_mode to NA
biotraits7.4$Feed_prim_abbrev<-ifelse(biotraits7.4$Feed_prim_abbrev=="Other (specify in comments)", as.character(NA), biotraits7.4$Feed_prim_abbrev)
biotraits7.4$Feed_mode_sec<-ifelse(biotraits7.4$Feed_mode_sec=="Other (specify in comments)", as.character(NA), biotraits7.4$Feed_mode_sec)


count_mode <- biotraits7.4 %>% filter(!is.na(Genus)) %>%group_by(Genus) %>%summarize_all(mode_fn)

#for presentation
count_mode_vis<-count_mode[,-c(2:3)]

#write raw trait mode to csv
write.csv(count_mode, '~/Documents/WaterCube/Ch.3/aquatic_insects/raw_trait_genus_mode.csv')

#change to long format
keycol=c("Trait_group")
valuecol=c("Trait")
gathercol<-c("AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_synch_abbrev", "Feed_prim_abbrev", "Feed_mode_sec", "Female_disp_abbrev", "Habit_prim", "Habit_sec", "Max_body_size_abbrev", "Resp_abbrev", "Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev")
count_mode2<- gather_(count_mode, keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments

#how many missing trait values after assigning mode- ranges from 7 percent missingness (Feed_prim_abbrev) to 82% Feed_mode_sec
count_mode3<-count_mode2%>%
  group_by(Trait_group, Trait)%>%
  tally%>%
  mutate(Percent=n/sum(n))

#how much data is missing total? 34% of data is missing total
count_mode4<-count_mode2%>%
  group_by(Trait)%>%
  tally%>%
  mutate(Percent=n/sum(n))

#calculate an affinity score 
#reshape table into long format
traits.long<- biotraits7.4%>%filter(!is.na(Genus))%>%gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments

affinities<-traits.long%>%
  group_by(Genus, Trait_group, Trait)%>%
  tally %>%
  mutate(Percent = n / sum(n))

#for presentation
affinities1<-affinities[,-4]
names(affinities1)<-c("Genus", "Trait_group", "Trait","Affinity")
str(affinities1)

is.num <- sapply(affinities1, is.numeric)
affinities1[is.num] <- lapply(affinities1[is.num], round, 2)

################### Create trait table of our traits + USEPA traits (uncleaned)- then compare new to old - for EDI/data paper ################################################################
#Have two raw datasets. One for taxa that go with WQP data, and one for all insect taxa (WQP plus EPA database)
#Already have raw trait table (raw_trait_table3) in long and wide format

#old dataset before adding traits- overlaps with our dataset
biotraits10 <-  read.csv('~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/biotraits8_11_30_17.csv', stringsAsFactors = FALSE, na.strings=c("","NA"))
length(unique(biotraits10$Genus)) #869 unique genera- all taxa in WQP (only includes USEPA data of taxa in WQP)

length(unique(raw_trait_table3$Genus)) #980 unique genera- all WQP taxa (869+ what we added. 980-819=111 genera added.)

#read in old dataset that does not overlap with our dataset
epa<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/USEPA_only_insects.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
#how many genera do not overlap with our dataset?

epa_unique<-epa%>%
  anti_join(biotraits7.4, by="SubjectTaxonomicName")
length(unique(epa_unique$Genus)) #174 genera that were in USEPA but not WQP database (we added 111 genera and traits for numerous other genera)
#980 + 174 = total insect genera in our dataset = traits for 1154 insect genera 

#combine to create raw dataset with EPA traits- may want to go back through and do data cleaning on this portion to add to trait imputation- look at overlap between taxa in this dataset and ours

###################Run cluster analysis to produce trait syndromes#####################################################################################################
#run on imputed and unimputed traits

###################Create ancillary trait table###############################################################################################
#################Run at end of day############################################################################################################

save.image("Trait_cleaning.RData")

write.csv(biotraits7.2, "Insect_raw_traits.csv")


#######convert characters to factors############
DF[sapply(DF, is.character)] <- lapply(DF[sapply(DF, is.character)], 
                                       as.factor)

###### Comparing new and old databases-Code to calculate number of genera for each dataset ###############################################
#filtering old dataset to only include certain columns 
#select unique genera for each trait
#make separate figures first
#for now combine datasets with uncleaned nonoverlapping epa traits- will need to revisit after cleaning up nonoverlapping traits
biotraits10.05<-rbind.fill(biotraits10, epa_unique) #combines the two relevant epa datasets (taxa in USEPA that are in and not in wqp)
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

biotraits7.5<-rbind.fill(biotraits7.4, epa_unique) 
biotraits_new<-biotraits7.5%>%
  select(Genus,Voltinism_abbrev,Feed_prim_abbrev,Habit_prim,Resp_abbrev,Max_body_size_abbrev,Rheophily_abbrev,Thermal_pref,Female_disp_abbrev,AdultFlyingStrength_abbrev,Emerge_season_1,Emerge_synch_abbrev)
#changing to long format- new trait dataset
biotraits_new2<-melt(biotraits_new, id="Genus", na.rm=TRUE) 
biotraits_new3<-dcast(biotraits_new2, variable~Genus)

#sum unique genera in each trait 
n_Genus1=rowSums(biotraits_new3 != 0)
biotraits_new5<-cbind(biotraits_new3, n_Genus1) 
colnames(biotraits_new5)[1]<-"Trait"

#arranges the new long format dataset
trait<-arrange(biotraits_new5, Trait, desc(n_Genus1))

library(ggplot2)

#Original ggplot code that I tried to change to get the new plot above ********This code works************
traits<-ggplot(trait,aes(x=Trait,y=n_Genus1, fill=Trait))+
  geom_bar(stat="identity")+
  scale_fill_discrete(guide=FALSE)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "Female_disp_abbrev","AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generations/yr", "Feeding style", "Habit", "Respiration", "Body size", "Flow preference", "Thermal preference","Female dispersal", "Flying strength", "Emergence season", "Emergence synchrony"))
traits2<-traits+theme_classic()+ylab("Number of Genera")+xlab("Trait category")+theme(axis.title.x=element_text(size=25), axis.title.y=element_text(size=25), axis.text.x=element_text(size=20, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits2

pdf('~/Documents/WaterCube/Figures/Genus_traits.pdf', width=8, height=6.811)
traits2
dev.off()

#combine both datasets into one dataset
#initilaize new column for each
trait$Dataset<-"CONUS_traits"
biotraits10.5$Dataset<-"USEPA_traits"

all_traits<-rbind.fill(biotraits10.5, trait)

#creating the new plot with separation of data based on "dataset" column 
traits<-ggplot(all_traits,aes(x=reorder(Trait, -n_Genus1),y=n_Genus1, group=Dataset)) +
  geom_bar(stat="identity", position="dodge",aes(fill = Dataset))+
  scale_fill_manual(breaks = c("CONUS_traits", "USEPA_traits"), labels=c("CONUS","USEPA"),
                         values=c("red", "blue"))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "Female_disp_abbrev","AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generation/yr", "Feeding style", "Habit", "Respiration", "Max. body size", "Flow preference", "Thermal preference", "Female dispersal","Flying strength", "Emergence season", "Emergence synchrony"))
traits2<-traits+theme_classic()+ylab("Number of Genera")+xlab("Trait Category")+theme(axis.title.x=element_text(size=30), axis.title.y=element_text(size=30), axis.text.x=element_text(size=25, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits2+ theme(legend.position="top",legend.text = element_text(size = 15), legend.title = element_text(size=16, face="bold"))
ggsave('~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/all_Genus_traits.png', width=8, height=6.811)
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
#the main problem is trying to sort the data by the "data" column I created on lines 8-9 in ggplot when that column has been converted to rows because we switched to long format