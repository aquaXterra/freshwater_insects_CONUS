################Script to combine Pyne trait database with BioTraits database
#Author: Laura Twardochleb
#8/17/18

rm(list=ls())#this clears data from the workspace
graphics.off()
load("pyne_traits.RData")

library(dplyr)
library(plyr)
library(ggplot2)
library(taxize)

biotraits<-read.csv("biotraits8_6_15_18.csv")
head(biotraits)
pyne_traits<-read.csv("Pyne2016_genus.csv", header=T,  
                      na.strings=c("","NA", "-"))
colnames(biotraits)

#drop unneeded columns
colnames(pyne_traits)
pyne_traits1<-pyne_traits[,-c(1, 3:5, 7:9,11:13)]

#######rename columns in pynetraits#######
pynetraits2<-rename(pyne_traits1, c("Taxa.ID"="SubjectTaxonomicName", "ITIS.TSN"="TSN", "Volt"="Voltinism_abbrev", "Sync"="Emerge_synch_abbrev", "Disp"="Female_disp_abbrev", "Flgt"="AdultFlyingStrength_abbrev", "Rheo"="Rheophily_abbrev", "Size"="Max_body_size_abbrev", "Habi"="Habit_prim", "Trop"="Feed_prim_abbrev", "Ther"="Thermal_pref", "Resp"="Resp_abbrev"))

#revalue biotraits
## Data Cleaning
#replacing all rows containing Heteroptera as the Order wih Hemiptera
revalue(biotraits$Order[biotraits$Order == "Heteroptera"] <- "Hemiptera")
levels(biotraits$Order)

#######revalue voltinism so that all match in biotraits######
biotraits$Voltinism_abbrev<-revalue(biotraits$Voltinism_abbrev, c("semivoltine"="Semivoltine", "bi_multivoltine"="Bi_multivoltine", "BI_multivoltine"="Bi_multivoltine", "univoltine"="Univoltine", "multivoltine"="Bi_multivoltine"))
levels(biotraits$Voltinism_abbrev)

#######revalue thermal preference so that all match in biotraits#######
levels(biotraits$Thermal_pref)
biotraits$Thermal_pref<-revalue(biotraits$Thermal_pref, c("Warm eurythermal (15-30 C) "="Warm eurythermal (15-30 C)", "Warm eurythermal (15-30 C"="Warm eurythermal (15-30 C)", "warm eurythermal (15-30 C"="Warm eurythermal (15-30 C)", "cold-cool eurythermal (0-15 C)"="Cold-cool eurythermal (0-15 C)"))

#######rename values in pynetraits2 to match values in biotraits#######
#voltinism
levels(pynetraits2$Voltinism_abbrev)
pynetraits2$Voltinism_abbrev<-revalue(pynetraits2$Voltinism_abbrev, c("Volt.MultiVoltine"="Bi_multivoltine", "Volt.SemiVoltine"="Semivoltine", "Volt.UniVoltine"="Univoltine"))

########thermal pref#####- not run- need to ask about thermal preference categories#
levels(biotraits$Thermal_pref)
levels(pynetraits2$Thermal_pref)
pynetraits2$Thermal_pref<-revalue(pynetraits2$Thermal_pref,c("Ther.Cold"="Cold-cool eurythermal (0-15 C)", "Ther.Warm"="Warm eurythermal (15-30 C)", "Ther.CoolWarm"="Cool-warm eurythermal (5-30 C)"))

######revalue emergence synchrony####
levels(pynetraits2$Emerge_synch_abbrev)
levels(biotraits$Emerge_synch_abbrev)
biotraits$Emerge_synch_abbrev<-revalue(biotraits$Emerge_synch_abbrev, c("poorly"="Poorly", "well"="Well"))
pynetraits2$Emerge_synch_abbrev<-revalue(pynetraits2$Emerge_synch_abbrev, c("Sync.Poor"="Poorly", "Sync.Well"="Well"))

########revalue female dispersal#####
levels(biotraits$Female_disp_abbrev)
levels(pynetraits2$Female_disp_abbrev)
pynetraits2$Female_disp_abbrev<-revalue(pynetraits2$Female_disp_abbrev, c("Disp.High"="high", "Disp.Low"="low"))
biotraits$Female_disp_abbrev<-revalue(biotraits$Female_disp_abbrev, c("low "="low"))
#assign values to Female_disp_abbrev based on comments in Female_disp_comments- need to check whether other comments apply to adult dispersal-ask Ethan
levels(biotraits$Female_disp_comments)
#see if any Female_disp_abbrev and Female_disp_comments entries don't match
biotraits_disp<-subset(biotraits, Female_disp_abbrev!="NA" & Female_disp_comments!="NA")
biotraits_disp1<-subset(biotraits_disp, Female_disp_abbrev == "high" & Female_disp_comments == "less than 1 km flight before laying eggs")
biotraits_disp2<-subset(biotraits_disp, Female_disp_abbrev=="NA"& Female_disp_comments!="NA" |  Female_disp_abbrev=="NA"& Female_disp_comments!="Female_disp_comments: low = less than 1 km flight before laying eggs; high = greater than 1 km flight before laying eggs")
#revalue all entries in Female_disp_comments with "Female_disp_comments: low = less than 1 km flight before laying eggs; high = greater than 1 km flight before laying eggs" to "NA"
biotraits$Female_disp_comments<-revalue(biotraits$Female_disp_comments, c("Female_disp_comments: low = less than 1 km flight before laying eggs; high = greater than 1 km flight before laying eggs"="NA"))
#create column of Female_disp_abbrev using values from Female_disp_abbrev and Female_disp_comments
biotraits1<-biotraits%>%
  mutate(Female_disp=case_when(Female_disp_abbrev=="low"~"low",
                               Female_disp_abbrev=="high"~"high",
                               Female_disp_abbrev=="1 km or less"~"1 km or less",
                               Female_disp_abbrev=="10 km or less"~"10 km or less",
                               Female_disp_comments=="1 km or less"~"1 km or less",
                               Female_disp_comments=="100 km or less"~"100 km or less",
                               Female_disp_comments=="less than 1 km flight before laying eggs"~"low"))
#rename female disp columns
colnames(biotraits1)[colnames(biotraits1)=="Female_disp_abbrev"] <- "Female_disp_old"
colnames(biotraits1)[colnames(biotraits1)=="Female_disp"] <- "Female_disp_abbrev"

#####revalue adult flying strength####
levels(biotraits1$AdultFlyingStrength_abbrev)
levels(biotraits1$AdultFlyingStrength_comments)
biotraits1$AdultFlyingStrength_abbrev<-revalue(biotraits1$AdultFlyingStrength_abbrev, c("Strong"="strong", "Weak"="weak"))
levels(pynetraits2$AdultFlyingStrength_abbrev)
pynetraits2$AdultFlyingStrength_abbrev<-revalue(pynetraits2$AdultFlyingStrength_abbrev, c("Flgt.Strong"="strong", "Flgt.Weak"="weak"))

#####revalue Rheophily_abbrev#####
levels(biotraits1$Rheophily_abbrev)
levels(pynetraits2$Rheophily_abbrev)
pynetraits2$Rheophily_abbrev<-revalue(pynetraits2$Rheophily_abbrev, c("Rheo.Both"="depo_eros", "Rheo.Depositional"="depo", "Rheo.Erosional"="eros"))

#####revalue max_body_size_abbrev####
levels(biotraits1$Max_body_size_abbrev)
levels(biotraits1$Max_body_size)
biotraits1$Max_body_size<-revalue(biotraits1$Max_body_size, c("small (length < 9 mm)"="Small (length < 9 mm)", "Small (length < 9 mm)S"="Small (length < 9 mm)"))
biotraits1$Max_body_size_abbrev<-revalue(biotraits1$Max_body_size_abbrev, c("Small"="small", "Medium "="medium", "Medium"="medium", "Large"="large"))
#check to see if values in Max_body_size_abbrev and Max_body_size match- no mismatches
biotraits_body<-filter(biotraits1, Max_body_size_abbrev!="small" & Max_body_size=="Small (length < 9 mm)", Max_body_size_abbrev!="medium" & Max_body_size=="Medium (length 9-16 mm)", Max_body_size_abbrev!="large" & Max_body_size=="Large (length > 16 mm)")
#revalue pynetraits
levels(pynetraits2$Max_body_size_abbrev)
pynetraits2$Max_body_size_abbrev<-revalue(pynetraits2$Max_body_size_abbrev, c("Size.Large"="large", "Size.Medium"="medium", "Size.Small"="small"))

#####revalue Habit_prim#####
levels(biotraits1$Habit_prim)
levels(biotraits1$Habit_comments)
levels(pynetraits2$Habit_prim)
biotraits1$Habit_prim<-revalue(biotraits1$Habit_prim, c("clinger"="Clinger", "Clingers "="Clinger"))
#revalue Pynetraits
pynetraits2$Habit_prim<-revalue(pynetraits2$Habit_prim, c("Habi.Burrow"="Burrower", "Habi.Climb"="Climber", "Habi.Cling"="Clinger", "Habi.Skate"="Skater", "Habi.Sprawl"="Sprawler", "Habi.Swim"="Swimmer"))

#####revalue Feed_prim_abbrev####
levels(biotraits1$Feed_prim_abbrev)
levels(biotraits1$Feed_mode_comments)
feed_mode<-filter(biotraits1, Feed_prim_abbrev=="SF" | Feed_prim_abbrev=="GC")
biotraits1$Feed_prim_abbrev<-revalue(biotraits1$Feed_prim_abbrev, c("SF"= "CF", "GC"="CG"))
#revalue pynetraits
levels(pynetraits2$Feed_prim_abbrev)
pynetraits2$Feed_prim_abbrev<-revalue(pynetraits2$Feed_prim_abbrev, c("Trop.CollectorFilterer"="CF", "Trop.CollectorGatherer"="CG", "Trop.Herbivore"="HB", "Trop.Predator"="PR", "Trop.Shredder"="SH"))

####Revalue Resp_abbrev####
levels(biotraits1$Resp_abbrev)
levels(biotraits1$Resp_comments)
biotraits1$Resp_abbrev<-revalue(biotraits1$Resp_abbrev, c("Gillls"="gills", "Gills"="gills", "Gills "="gills", "Tegument"="tegument", "Tegument "="tegument", "Plastron_spiracle"="plastron_spiracle"))
#revalue pynetraits
levels(pynetraits2$Resp_abbrev)
pynetraits2$Resp_abbrev<-revalue(pynetraits2$Resp_abbrev, c("Resp.Air"="plastron_spiracle", "Resp.Gills"="gills", "Resp.Tegument"="tegument"))

######add column for terrestrial species#######
biotraits1<-biotraits1%>%mutate(Terrestrial=case_when(Genus=="Delia"~"yes"))

######Combine subset of Pyne traits for traits of interest with biotraits1##########
colnames(pynetraits2)
pynetraits3<-pynetraits2[,-c(7, 9,12:17, 19:20)]
colnames(pynetraits3)
#create column indicating Pyne traits
pynetraits3$Study_Citation_abbrev<-"Pyne traits"

#combine pynetraits4 with biotraits1 WQP taxa
#subset biotraits1 to just WQP taxa that Ethan is adding
biotraits2<-biotraits1[1:1084,]
#use semi join- keep all observations in biotraits2, only observations that match SubjectTaxonomicName in pynetraits3
pyne_ethan_traits<-pynetraits3%>%semi_join(biotraits2, by="SubjectTaxonomicName")
#now join pyne_ethan_traits with biotraits2 to get all WQP traits that Ethan and Pyne defined
pyne_ethan_traits2<-merge(pyne_ethan_traits, biotraits2, all=TRUE, sort=FALSE)
#reorder by Order and then Family, Genus, etc.
pyne_ethan_traits3<-arrange(pyne_ethan_traits2, Order, SubjectTaxonomicName)
#write to csv for Ethan to work on
write.csv(pyne_ethan_traits3, "Hiltner_biotraits.csv")

#next merge remaining pynetraits with rest of EPA trait database, and combine the entire dataset
pyne_epa_traits<-pynetraits3%>%anti_join(biotraits2, by="SubjectTaxonomicName")
#subset epa database
biotraits3<-biotraits1[-c(1:1084),]
pyne_epa_traits2<-merge(pyne_epa_traits, biotraits3, all=TRUE, sort=FALSE)
pyne_epa_traits3<-arrange(pyne_epa_traits2, Order, SubjectTaxonomicName)

#write csv files for PA students to work from
Ralston<-pyne_epa_traits3[-c(1:5000), ]
Bhatt<-pyne_epa_traits3[-c(5000:10863),]
write.csv(Ralston, "Ralston_biotraits.csv")
write.csv(Bhatt, "Bhatt_biotraits.csv")

#merge both datasets together
biotraits4<-merge(pyne_ethan_traits3, pyne_epa_traits3,  all=TRUE, sort=FALSE)

#write csv for full trait dataset
write.csv(biotraits4, "biotraits_8_23_18.csv")

#read in new PA data and divide in half
Ralston<-read.csv("Ralston_biotraits_10_15_18.csv")
Bhatt<-read.csv("Bhatt_biotraits_10_15_18.csv")
Bhatt2<-subset(Bhatt, Order=="Carabidae"|Order=="Coleoptera"|Order=="Diptera")
Bhatt2$Order<-revalue(Bhatt2$Order,c("Carabidae"="Coleoptera"))
Bhatt2$Family<-revalue(Bhatt2$Family, c("Coleoptera"="Carabidae"))
write.csv(Bhatt2, "Bhatt._biotraits_10_16_18.csv")
ephemeroptera<-subset(Bhatt, Order=="Ephemeroptera")
Ralston$Body_size_comments<-"NA"
Ralston2<-rbind(ephemeroptera, Ralston)
write.csv(Ralston2, "Ralston_biotraits_10_16_18.csv")

#find species designations that are not up-to-date in all databases and generate list of accepted species for each student
library('taxize')
Bhatt<-read.csv("Bhatt_biotraits_10_17_18.csv", stringsAsFactors = FALSE, na.strings=c("","NA", "-"), header=T)
ids<-na.omit(Bhatt$TSN)
ids2<-as.numeric(ids)
ids3<-na.omit(ids2)
#itis_acceptname(searchtsn=111966)- works to retrieve current name
Bhatt_names<-itis_acceptname(searchtsn=ids3)

#try retrieving TSN from a taxa name- works to get scientific name and TSN for all taxa
names<-itis_terms(query="Enallagma boreale", "scientific")

#generate taxa lists for PAs
#Minali-Coleoptera families
coleoptera<-subset(Bhatt, Order=="Coleoptera")
families<-unique(coleoptera$Family)

trichoptera<-subset(Ralston2, Order=="Trichoptera")
genera<-unique(trichoptera$Genus)
families<-unique(trichoptera$Family)
write.csv(genera, "Ralston_tricvhoptera_genera.csv")

#divide Plecoptera taxa between Erika and Minali
Ralston<-read.csv("Ralston_biotraits_11_16_18.csv")
Bhatt<-read.csv("Bhatt_biotraits_11_16_18.csv")
Plecoptera<-subset(Ralston, Order=="Plecoptera")
Plecoptera1<-Plecoptera[c(1:700), ]
Plecoptera2<-Plecoptera[c(700:1433),]
Ralston2<-subset(Ralston, Order!="Plecoptera")
Ralston3<-rbind(Ralston2, Plecoptera1)
write.csv(Ralston3, "Ralston_biotraits_11_16_18.csv")

Bhatt2<-rbind(Bhatt, Plecoptera2)
write.csv(Bhatt2, "Bhatt_biotraits_11_16_18.csv")

genera1<-unique(Plecoptera1$Genus)
genera2<-unique(Plecoptera2$Genus)
write.csv(genera1, "Ralston_Plecoptera_genera.csv")
write.csv(genera2, "Bhatt_Plecoptera_genera.csv")

save.image("pyne_traits.RData")
