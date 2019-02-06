####This script is for data cleaning of the freshwater insects biological trait database compiled from 11/2017 to 12/7/2018 by LT, EH, MB, and ER
####Authors: Laura Twardochleb and Ethan Hiltner
####Date created:12/10/18

#######Notes/to-dos#######
#Check names in ITIS
#Check name consistency in SubjectTaxonomicName, Genus, Family, Order
#Fix remaining citation problems
#remove terrestrial taxa from occurrence database
#assign dominant trait at species level
#assign dominant trait at genus level (binary coding)
#fuzzy-coding by Genus

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

library(plyr)
library(dplyr)
library(ggplot2)

#setwd("~/Documents/WaterCube/Aquaxterra_code_data/Insect_R_scripts_data/STORET_data/WQP_insect_final")
##########Read-in data from Google Drive####################################################################

#new dataset- combine Ethan, Minali, Erika datasets
#AquaticInsects/aquaticinsect_database/Data_files/Bhatt_biotraits_12_7_18.csv
data.key="11tuv40H55w7w_DYgFRE535Kg5qOkn7eV"
Bhatt <-  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data.key), stringsAsFactors=F, na.strings=c(""," ", "NA"))
str(Bhatt)

id<-"1fIVjA6nL8WnKCTTkdj8CqUQQuUo9opdJ"
Ralston<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), stringsAsFactors=F, na.strings=c(""," ", "NA"))

id2<-"1rjzWZA7oNrEJ4HviqmM-h0hgF7tZ7wna"
Hiltner<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id2), stringsAsFactors=F, na.strings=c(""," ", "NA"))
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
other<-biotraits[which(biotraits$Feed_mode_comments != "NA" & biotraits$Feed_prim_abbrev == "Other (specify in comments)"),]

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
citations<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id3), stringsAsFactors = FALSE, na.strings=c(""," ", "NA"))
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
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation_abbrev == "Arnet 1985"   ) ]<-"Arnett 1985"
biotraits2[which(biotraits2$Study_Citation_abbrev == "Arnett 1985"   ), ]
biotraits2$Study_Citation[which(biotraits2$Study_Citation_abbrev == "Arnet 1985"   ) ]<-"Arnett Jr, Ross H. American insects: a handbook of the insects of America north of Mexico. Van Nostran Reinhold Company Ltd., 1985."

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
biotraits2[which(biotraits2$Study_Citation_abbrev == "Arnett & Thomas 2000"   ), ]
biotraits2[which(biotraits2$Study_Citation_abbrev == "Frank & Kee-Jeong 2011"   ), ]
biotraits2$Study_Citation_abbrev[which(biotraits2$Study_Citation == "Arnett Jr, Ross H.American insects: a handbook of the insects of America north of Mexico. Crc Press, 2000."   ) ]<-"Arnett 2000"
biotraits2[which(biotraits2$Study_Citation_abbrev == "Chaboo & Shepard 2015"   ), ]
########Check naming consistency#################"   ), ]

#write citations table
biotraits_citations<-biotraits2[,c("Study_Citation_abbrev", "Study_Citation")]
citations2<-unique(biotraits_citations)
citations3<-subset(citations2, Study_Citation_abbrev!="USGS 2006")

#characters are corrupted when writing as a csv
write.csv(citations3, "citations_corrected.csv", fileEncoding = "UTF-8")


write.csv(biotraits2, "Insect_raw_traits.csv")
#######convert characters to factors############
DF[sapply(DF, is.character)] <- lapply(DF[sapply(DF, is.character)], 
                                       as.factor)

######Comparing new and old databases- Ethan code###############################################
#old dataset
biotraits6 <-  read.csv('biotraits8_11_30_17.csv', stringsAsFactors = FALSE, na.strings=c("","NA"))

#rename columns to match in two databases
biotraits6$Feed_mode_new<-biotraits6$Feed_prim_abbrev
biotraits6$Volt_new<-biotraits6$Voltinism_abbrev
biotraits6$Habit_new<-biotraits6$Habit_prim

#adding a new column called "data" to signify between the two datasets
biotraits6$data <- "USEPA_Database"
biotraits$data <- "New_Database"

#filtering both datasets to only include certain columns 
biotraits1<-biotraits%>%select(SubjectTaxonomicName, Order, Family,Genus,Voltinism_abbrev,Feed_prim_abbrev,Habit_prim,Resp_abbrev,Max_body_size_abbrev,Rheophily_abbrev,Thermal_pref,AdultFlyingStrength_abbrev,Emerge_season_1,Emerge_synch_abbrev, data, Study_Citation_abbrev)
biotraits7<-biotraits6%>%select(SubjectTaxonomicName, Order, Family, Genus,Voltinism_abbrev,Feed_prim_abbrev,Habit_prim,Resp_abbrev,Max_body_size_abbrev,Rheophily_abbrev,Thermal_pref,AdultFlyingStrength_abbrev,Emerge_season_1,Emerge_synch_abbrev, data, Study_Citation_abbrev)

#combine both datasets into one dataset
df <- rbind(biotraits1, biotraits7)

#changing to long format
biotraits2<-melt(df, id="Genus", na.rm=TRUE) 
biotraits3<-dcast(biotraits2, variable+ value~Genus)

#sum unique genera in each trait state
n_Genus1=rowSums(biotraits3 != 0)
biotraits4<-cbind(biotraits3, n_Genus1) 
colnames(biotraits4)[1]<-"Trait"
colnames(biotraits4)[2]<-"Trait_state"

#arranges the new long format dataset
trait<-arrange(biotraits4, Trait, Trait_state, desc(n_Genus1))

library(ggplot2)

#creating the new plot with seperation of data based on "data" column *****This code does not work******
traits<-ggplot(trait,aes(x=Trait,y=n_Genus, fill=data))+
  geom_bar(position = "dodge")+
  scale_fill_discrete(guide=FALSE)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generations/yr", "Feeding mode", "Habit", "Respiration", "Max. body size", "Flow preference", "Thermal preference", "Flying strength", "Emergence season", "Emergence synchrony"))
traits2<-traits+theme_classic()+ylab("Number of Genera")+theme(axis.title.x=element_text(size=25), axis.title.y=element_text(size=20), axis.text.x=element_text(size=25, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits2

#Original ggplot code that I tried to change to get the new plot above
traits<-ggplot(trait,aes(x=Trait,y=n_Genus, fill=Trait))+
  geom_bar(stat="identity")+
  scale_fill_discrete(guide=FALSE)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks=c("Voltinism_abbrev","Feed_prim_abbrev","Habit_prim", "Resp_abbrev", "Max_body_size_abbrev", "Rheophily_abbrev", "Thermal_pref", "AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_synch_abbrev"),
                   labels=c("Generations/yr", "Feeding mode", "Habit", "Respiration", "Max. body size", "Flow preference", "Thermal preference", "Flying strength", "Emergence season", "Emergence synchrony"))
traits2<-traits+theme_classic()+ylab("Number of Genera")+theme(axis.title.x=element_text(size=25), axis.title.y=element_text(size=20), axis.text.x=element_text(size=25, angle=45, vjust=1, hjust=1),  axis.text.y=element_text(size=25))
traits2


#the main problem is trying to sort the data by the "data" column I created on lines 8-9 in ggplot when that column has been converted to rows because we switched to long format