#Laura Twardochleb
#This script calculates trait modal values and affinity scores and creates tables to be published online by EDI and with data paper

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

#Load workspace image
load(file='Calc_trait_affinity_mode.RData')

#read-in data
biotraits7.4<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/biotraits_wide2.csv")
epa<-read.csv("epa_traits.csv", stringsAsFactors = FALSE)
epa_citations<-read.csv("epa_citations.csv")

#load packages
library(tidyverse)
library(reshape2)
library(stringr)

################## Final data cleaning #############################################################
#combine epa and biotraits tables
biotraits7.5<-biotraits7.4[,-c(1,2,21,25)]
names(biotraits7.5)
names(epa)
epa2<-epa %>%dplyr:: select(c("Study_Citation_abbrev", "SubjectTaxonomicName","AdultFlyingStrength_abbrev","Emerge_season_1", "Emerge_season_2", "Emerge_season_comments","Emerge_synch_abbrev","Family","Feed_mode_comments","Feed_mode_sec","Feed_prim_abbrev","Female_disp_abbrev", "Genus", "Habit_comments","Habit_prim_abbrev","Habit_sec","Max_body_size_abbrev", "Order", "Resp_abbrev","Resp_comments","Rheophily_abbrev","Study_location_state","Thermal_pref", "TSN","Volt_comments",  "Voltinism_abbrev","acceptedtsn","Accepted_name", "Study_Citation"))
epa2$Taxonomic_resolution<-NA
epa2$Taxonomic_resolution<-ifelse(as.character(epa2$Accepted_name)==as.character(epa2$Genus),"Genus", NA)
epa2$Taxonomic_resolution<-ifelse(as.character(epa2$Accepted_name)==as.character(epa2$Family),"Family", epa2$Taxonomic_resolution)
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

write.csv(epa2, "epa_cleaned.csv")

colnames(epa2)<-colnames(biotraits7.5)

biotraits7.6<-rbind(epa2, biotraits7.5)

#double-check levels
names(biotraits7.6)
unique(biotraits7.6$Thermal_pref)
biotraits7.6$Female_disp_abbrev[which(biotraits7.6$Female_disp_abbrev=="low")]<-"Low"
biotraits7.6$Female_disp_abbrev[which(biotraits7.6$Female_disp_abbrev=="high")]<-"High"
biotraits7.6$Habit_prim[which(biotraits7.6$Habit_prim=="CN")]<-"Clinger"
biotraits7.6$Habit_prim[which(biotraits7.6$Habit_prim=="SP")]<-"Sprawler"
biotraits7.6$Habit_prim[which(biotraits7.6$Habit_prim=="SW")]<-"Swimmer"
biotraits7.6$Habit_prim[which(biotraits7.6$Habit_prim=="BU")]<-"Burrower"
biotraits7.6$Habit_prim[which(biotraits7.6$Habit_prim=="CB")]<-"Climber"
biotraits7.6$Habit_sec[which(biotraits7.6$Habit_sec=="Miner")]<-NA
biotraits7.6$Habit_sec[which(biotraits7.6$Habit_sec=="Diver")]<-NA
biotraits7.6$Habit_sec[which(biotraits7.6$Habit_sec=="Attached/fixed")]<-NA
biotraits7.6$Thermal_pref[which(biotraits7.6$Thermal_pref=="No strong preference")]<-"Cool-warm eurythermal (5-30 C)"
write.csv(biotraits7.6, "biotraits7.6.csv")

length(unique(biotraits7.6$Genus)) #1072 genera

#check the taxonomic resolution designations- seem ok for most part, in some cases genus and submitted name and accepted name don't match
biotraits7.6[which(biotraits7.6$Taxonomic_resolution=="Species"),]

#first convert any with trait "other" to NA
biotraits7.6$Feed_prim_abbrev[which(biotraits7.6$Feed_prim_abbrev=="Other (specify in comments)")]<-NA
biotraits7.6$Feed_mode_sec[which(biotraits7.6$Feed_mode_sec=="Other (specify in comments)")]<-NA
biotraits7.6$Habit_prim[which(biotraits7.6$Habit_prim=="Other (specify in comments)")]<-NA
biotraits7.6$Habit_sec[which(biotraits7.6$Habit_sec=="Other (specify in comments)")]<-NA

#examine and remove duplicate Pyne traits (dups of Poff et al. 2006) in Biotraits7.6
pyne2<-read.csv("Raw_Traits copy_Pyne.csv", stringsAsFactors = FALSE) #will need to do this in wide format

#convert to wide format
pyne3<-pivot_wider(pyne2, names_from = Trait_group, values_from = Trait)

#join with biotraits7.6
biotraits_revision<-biotraits7.6%>%left_join(pyne3, by=c("SubjectTaxonomicName"="Submitted_name_trait", "Study_Citation_abbrev"="Study_citation_abbrev"))

#assign Poff et al. (2006) as Study_Citation_abbrev for Ametropus discussed with Matt Pyne
biotraits_revision$Study_Citation_abbrev[which(biotraits_revision$SubjectTaxonomicName=="Ametropus"&biotraits_revision$Study_Citation_abbrev=="Pyne traits")]<-"Poff et al. (2006)"
biotraits_revision$Study_Citation[which(biotraits_revision$SubjectTaxonomicName=="Ametropus"&biotraits_revision$Study_Citation_abbrev=="Poff et al. (2006)")]<-"Poff, N.L., J.D. Olden, N.K.M. Vieira, D.S. Finn, M.P. Simmons, and B.C. Kondratieff. 2006. Functional trait niches of North American lotic insects: traits-based ecological applications in light of phylogenetic relationships.  Journal of the North American Benthological Society 25(4):730-755 (Trait Matrix, Appendix)"

#assign Poff et al. (2006) as Study_Citation_abbrev for Centroptilum and Procloeon as discussed with Matt Pyne
biotraits_revision$Study_Citation_abbrev[which(biotraits_revision$SubjectTaxonomicName=="Centroptilum"&biotraits_revision$Study_Citation_abbrev=="Pyne traits")]<-"Poff et al. (2006)"
biotraits_revision$Study_Citation[which(biotraits_revision$SubjectTaxonomicName=="Centroptilum"&biotraits_revision$Study_Citation_abbrev=="Poff et al. (2006)")]<-"Poff, N.L., J.D. Olden, N.K.M. Vieira, D.S. Finn, M.P. Simmons, and B.C. Kondratieff. 2006. Functional trait niches of North American lotic insects: traits-based ecological applications in light of phylogenetic relationships.  Journal of the North American Benthological Society 25(4):730-755 (Trait Matrix, Appendix)"
biotraits_revision$Study_Citation_abbrev[which(biotraits_revision$SubjectTaxonomicName=="Procloeon"&biotraits_revision$Study_Citation_abbrev=="Pyne traits")]<-"Poff et al. (2006)"
biotraits_revision$Study_Citation[which(biotraits_revision$SubjectTaxonomicName=="Procloeon"&biotraits_revision$Study_Citation_abbrev=="Poff et al. (2006)")]<-"Poff, N.L., J.D. Olden, N.K.M. Vieira, D.S. Finn, M.P. Simmons, and B.C. Kondratieff. 2006. Functional trait niches of North American lotic insects: traits-based ecological applications in light of phylogenetic relationships.  Journal of the North American Benthological Society 25(4):730-755 (Trait Matrix, Appendix)"

#drop the Poff et al. 2006 entry for Centroptilum/Procloeon
biotraits_revision<-biotraits_revision[-which(biotraits_revision$SubjectTaxonomicName=="Centroptilum/Procloeon"),]

#drop incomplete Pyne trait entries (no traits included)
biotraits_revision<-biotraits_revision[-which(biotraits_revision$Trait.Source=="Incomplete"),]

#fix assignnment of family-level Chironomidae traits- assign at family level instead of genus
biotraits_revision[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation"),]

#assign Poff et al. 2006 as the Citation
biotraits_revision$Study_Citation_abbrev[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-"Poff et al. (2006)"
biotraits_revision$Study_Citation[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-"Poff, N.L., J.D. Olden, N.K.M. Vieira, D.S. Finn, M.P. Simmons, and B.C. Kondratieff. 2006. Functional trait niches of North American lotic insects: traits-based ecological applications in light of phylogenetic relationships.  Journal of the North American Benthological Society 25(4):730-755 (Trait Matrix, Appendix)"

#take Chironomidae name as the submitted name
biotraits_revision$SubjectTaxonomicName[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-"Chironomidae"
rev<-biotraits_revision[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation"),]

#take submitted tsn from poff 2006 and accepted tsn as chironomid TSN
biotraits_revision$Submitted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-127917
biotraits_revision$Accepted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-127917
biotraits_revision$Original_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-127917

#take Chironomidae as the accepted_name
biotraits_revision$Accepted_Name[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-"Chironomidae"

#assign Family as taxonomic resolution
biotraits_revision$Taxonomic_resolution.x[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-"Family"

#assign NA in Genus
biotraits_revision$Genus[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 Chironomidae subfamily designation")]<-NA

#go through same steps for the other families with "Expanded from Poff 2006 family designation"
rev2<-biotraits_revision[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"),]
#take family name as the submitted name
biotraits_revision$SubjectTaxonomicName[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Empididae")]<-"Empididae"
biotraits_revision$SubjectTaxonomicName[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Simuliidae")]<-"Simuliidae"
biotraits_revision$SubjectTaxonomicName[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Elmidae")]<-"Elmidae"
biotraits_revision$SubjectTaxonomicName[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Ceratopogonidae")]<-"Ceratopogonidae"
biotraits_revision$SubjectTaxonomicName[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Corixidae")]<-"Corixidae"

#take family name as accepted name
biotraits_revision$Accepted_Name[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Empididae")]<-"Empididae"
biotraits_revision$Accepted_Name[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Simuliidae")]<-"Simuliidae"
biotraits_revision$Accepted_Name[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Elmidae")]<-"Elmidae"
biotraits_revision$Accepted_Name[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Ceratopogonidae")]<-"Ceratopogonidae"
biotraits_revision$Accepted_Name[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"& biotraits_revision$Family=="Corixidae")]<-"Corixidae"

#assign Poff et al. 2006 as the Citation
biotraits_revision$Study_Citation_abbrev[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation")]<-"Poff et al. (2006)"
biotraits_revision$Study_Citation[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation")]<-"Poff, N.L., J.D. Olden, N.K.M. Vieira, D.S. Finn, M.P. Simmons, and B.C. Kondratieff. 2006. Functional trait niches of North American lotic insects: traits-based ecological applications in light of phylogenetic relationships.  Journal of the North American Benthological Society 25(4):730-755 (Trait Matrix, Appendix)"

#take submitted tsn from poff 2006 and accepted tsn as family tsn
biotraits_revision$Submitted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Empididae")]<-135830
biotraits_revision$Submitted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Simuliidae")]<-126640
biotraits_revision$Submitted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Elmidae")]<-114093
biotraits_revision$Submitted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Ceratopogonidae")]<-127076
biotraits_revision$Submitted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Corixidae")]<-103364

biotraits_revision$Original_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Empididae")]<-135830
biotraits_revision$Original_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Simuliidae")]<-126640
biotraits_revision$Original_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Elmidae")]<-114093
biotraits_revision$Original_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Ceratopogonidae")]<-127076
biotraits_revision$Original_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Corixidae")]<-103364

biotraits_revision$Accepted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Empididae")]<-135830
biotraits_revision$Accepted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Simuliidae")]<-126640
biotraits_revision$Accepted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Elmidae")]<-114093
biotraits_revision$Accepted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Ceratopogonidae")]<-127076
biotraits_revision$Accepted_TSN[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation"&biotraits_revision$Family=="Corixidae")]<-103364

#assign Family as taxonomic resolution
biotraits_revision$Taxonomic_resolution.x[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation")]<-"Family"

#assign NA in Genus
biotraits_revision$Genus[which(biotraits_revision$Trait.Source=="Expanded from Poff 2006 family designation")]<-NA

#drop Pyne traits entries that are directly from Poff et al. 2006
rev3<-biotraits_revision[which(biotraits_revision$Trait.Source=="Directly from Poff 2006"),]

biotraits_revision2<-biotraits_revision[-which(biotraits_revision$Trait.Source=="Directly from Poff 2006"),]

#change Frisonla to Frisonia in Accepted_name, Genus, TSN, Check family and order
biotraits_revision2[which(biotraits_revision2$SubjectTaxonomicName=="Frisonla"),]
biotraits_revision2$Accepted_Name[which(biotraits_revision2$SubjectTaxonomicName=="Frisonla")]<-"Frisonia"
biotraits_revision2$Genus[which(biotraits_revision2$SubjectTaxonomicName=="Frisonla")]<-"Frisonia"
biotraits_revision2$Accepted_TSN[which(biotraits_revision2$SubjectTaxonomicName=="Frisonla")]<-103171
biotraits_revision2$Original_TSN[which(biotraits_revision2$SubjectTaxonomicName=="Frisonla")]<-103171

#harmonize study citation abbrev
unique(biotraits_revision2$Study_Citation_abbrev)
biotraits_revision2$Study_Citation_abbrev<-(gsub("[()]", "", biotraits_revision2$Study_Citation_abbrev))

#drop unneeded columns from merge
biotraits_revision3<-biotraits_revision2[,c(1:29)]

#drop taxa that have NA in all traits
biotraits_revision4<-biotraits_revision3[-which(is.na(biotraits_revision3$AdultFlyingStrength_abbrev.x)&is.na(biotraits_revision3$Emerge_season_1.x)&is.na(biotraits_revision3$Emerge_season_2.x)&is.na(biotraits_revision3$Emerge_synch_abbrev.x)&is.na(biotraits_revision3$Feed_mode_sec.x)&is.na(biotraits_revision3$Feed_prim_abbrev.x)&is.na(biotraits_revision3$Female_disp_abbrev.x)&is.na(biotraits_revision3$Habit_prim.x)&is.na(biotraits_revision3$Habit_sec.x)&is.na(biotraits_revision3$Max_body_size_abbrev.x)&is.na(biotraits_revision3$Resp_abbrev.x)&is.na(biotraits_revision3$Rheophily_abbrev.x)&is.na(biotraits_revision3$Thermal_pref.x)&is.na(biotraits_revision3$Voltinism_abbrev.x)),]
biotraits_revision4<-unique(biotraits_revision4)

#some final mistakes to correct with names- separate those names with a / (e.g., Bezzia and Palpomyia) and check that they haven't already been corrected by Pyne
#write to csv and do manually in excel
write.csv(biotraits_revision4, "biotraits_revision4.csv", fileEncoding = "UTF-8")

#read in corrected csv file
biotraits_revision4<-read.csv("biotraits_revision4.csv", stringsAsFactors = FALSE)
#reassign higher taxonomic name columns- some are missing data- some are still missing accepted TSN
#check genus name assignments
na.genus<-biotraits_revision4[which(is.na(biotraits_revision4$Genus)),]

################### Prep trait mode table for EDI ##################################################
# Use the most common trait value (mode) per taxon
mode_fn <- function(x) ifelse(any(!is.na(x)), names(which.max(table(x))), as.character(NA))

count_mode <- biotraits_revision4 %>% filter(!is.na(Genus)) %>%group_by(Genus) %>%summarize_all(mode_fn)

#remove comments columns
count_mode2<-count_mode[,-grep(pattern="comments", colnames(count_mode))] 

############## prep ancillary taxonomy table #######################################################
#create ancillary taxonomy table- need to add taxa from occurrence table, also
names(biotraits_revision4)
ancillary_taxonomy<-dplyr::select(biotraits_revision4,c("SubjectTaxonomicName","Accepted_Name", "Accepted_TSN", "Order", "Family", "Genus"))%>%unique(.)
colnames(ancillary_taxonomy)<-c("Submitted_name", "Accepted_name","Accepted_TSN", "Order", "Family", "Genus")
#create species column
attach(ancillary_taxonomy)
ancillary_taxonomy$Species<-NA
ancillary_taxonomy$Species<-if_else(Accepted_name!=Genus, Accepted_name, as.character(NA))
detach(ancillary_taxonomy)
write.csv(ancillary_taxonomy, "ancillary_taxonomy_table.csv") #ancillary taxonomy with just trait taxa

#How many name changes in trait datasets?
no_match<-ancillary_taxonomy[which(ancillary_taxonomy$Submitted_name!=ancillary_taxonomy$Accepted_name),]%>%unique(.) #420 names changed, how many genus name changes?
species.change<-no_match[which(no_match$Accepted_name==no_match$Species),] 
length(unique(species.change$Species)) #261 species name changes
genus.change<-no_match[which(no_match$Accepted_name==no_match$Genus),] 
length(unique(genus.change$Genus)) #60 genus name changes
length(unique(genus.change$Submitted_name)) #78 submitted names- 78-60=18 cases where taxa were combined 

length(unique(ancillary_taxonomy$Submitted_name)) #3802
length(unique(ancillary_taxonomy$Accepted_name)) #3537- 265 name merges

#merge ancillary_taxonomy for traits and occurrence taxa
occurrence_taxa<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/ancillary_taxonomy_occurrence.csv", stringsAsFactors = FALSE)
occurrence_taxa2<-unique(occurrence_taxa[,-1])

#assign missing accepted names to occurrence table- then check & revise in the master and genus occurrence tables
#first change Submitted_name to sentence case
occurrence_taxa3<-occurrence_taxa2 %>% mutate(Submitted_name = str_to_sentence(Submitted_name))

occurrence_taxa3$Accepted_name<-if_else(is.na(occurrence_taxa3$Accepted_name)&occurrence_taxa3$Submitted_name==occurrence_taxa3$Genus, occurrence_taxa3$Genus, occurrence_taxa3$Accepted_name)
occurrence_taxa3$Accepted_name<-if_else(is.na(occurrence_taxa3$Accepted_name)&occurrence_taxa3$Submitted_name==occurrence_taxa3$Species, occurrence_taxa3$Species, occurrence_taxa3$Accepted_name)
occurrence_taxa3$Accepted_name<-if_else(is.na(occurrence_taxa3$Accepted_name)&occurrence_taxa3$Submitted_name==occurrence_taxa3$Family, occurrence_taxa3$Family, occurrence_taxa3$Accepted_name)
occurrence_taxa3$Accepted_name<-if_else(is.na(occurrence_taxa3$Accepted_name)&occurrence_taxa3$Submitted_name==occurrence_taxa3$Order, occurrence_taxa3$Order, occurrence_taxa3$Accepted_name)
occurrence_taxa4<-unique(occurrence_taxa3)

ancillary_taxonomy_master<-full_join(ancillary_taxonomy, occurrence_taxa4, by = c("Submitted_name", "Accepted_name", "Accepted_TSN", "Order", "Family", "Genus", "Species"))

#how many name changes in occurrence taxa?
no_match3<-occurrence_taxa4[which(occurrence_taxa4$Submitted_name!=occurrence_taxa4$Accepted_name),] #704 name changes

no_match4<-subset(no_match3, !is.na(Genus))

#how many are genus names? #177 genus names changes 
genus_no_match<-no_match4 %>% mutate(combined = ifelse(str_detect(Submitted_name, Genus), 
                                "yes", "no"))%>%filter(combined=="no")

#96 repeat name changes- how many combined? 
genus_no_match$dups<-duplicated(genus_no_match$Genus)
genus_dups<-subset(genus_no_match, dups=="TRUE")
length(unique(genus_dups$Submitted_name)) #96 submitted
length(unique(genus_dups$Species)) #72 are species
length(unique(genus_dups$Genus)) #36- so 96 names combined into 36 genera

#write master ancillary taxonomy table
write.csv(ancillary_taxonomy_master, "ancillary_taxonomy_table_master.csv")

#subset mode table to columns we want in final table and change names
names(count_mode2)

count_mode3<-dplyr::select(count_mode2,-c("Study_Citation_abbrev", "SubjectTaxonomicName", "Family", "Order", "Study_location_state.x", "Original_TSN", "Accepted_TSN", "Accepted_Name", "Study_Citation"))
colnames(count_mode3)<-c("Genus","AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_synch_abbrev",  "Feed_mode_sec", "Feed_prim_abbrev","Female_disp_abbrev", "Habit_prim", "Habit_sec", "Max_body_size_abbrev", "Resp_abbrev", "Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev")
write.csv(count_mode3, "~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/trait_mode_table.csv")

#convert trait mode table to long format
keycol=c("Trait_group")
valuecol=c("Trait")
gathercol<-c("AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_synch_abbrev", "Feed_prim_abbrev", "Feed_mode_sec", "Female_disp_abbrev", "Habit_prim", "Habit_sec", "Max_body_size_abbrev", "Resp_abbrev", "Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev")

trait_mode.long<- count_mode3%>%gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments
trait_mode.long2<-trait_mode.long[order(trait_mode.long$Genus),]

#csv of trait mode table for EDI
write.csv(trait_mode.long2, "~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/trait_mode.csv")

length(unique(trait_mode.long2$Genus)) #1015 genera

################## Calculate affinity scores #######################################################
colnames(biotraits_revision4)<-c("Study_citation_abbrev", "Submitted_name_trait", "AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_season_comments","Emerge_synch_abbrev", "Family", "Feed_mode_comments", "Feed_mode_sec", "Feed_prim_abbrev", "Female_disp_abbrev", "Genus", "Habit_comments", "Habit_prim", "Habit_sec", "Max_body_size_abbrev", "Order", "Resp_abbrev", "Resp_comments", "Rheophily_abbrev", "Study_location_state", "Thermal_pref", "Submitted_TSN", "Voltinism_comments", "Voltinism_abbrev", "Accepted_TSN", "Accepted_name", "Study_citation")

str(biotraits_revision4) #all traits are characters
unique(biotraits_revision4$Emerge_season_1) #levels consistent- ignore error in reshape2 function about diff. levels
unique(biotraits_revision4$Emerge_season_2)
unique(biotraits_revision4$Voltinism_abbrev)
unique(biotraits_revision4$Thermal_pref)
unique(biotraits_revision4$Habit_prim)
unique(biotraits_revision4$Habit_sec)
unique(biotraits_revision4$AdultFlyingStrength_abbrev)
unique(biotraits_revision4$Max_body_size_abbrev)
unique(biotraits_revision4$Resp_abbrev)
unique(biotraits_revision4$Rheophily_abbrev)
unique(biotraits_revision4$Emerge_synch_abbrev)
unique(biotraits_revision4$Female_disp_abbrev)

#reshape table into long format
traits.long<-biotraits_revision4%>%filter(!is.na(Genus))%>%gather_(keycol, valuecol, gathercol) #create a column "trait" that holds all trait assignments

affinities<-traits.long%>%filter(!is.na(Trait))%>%
  group_by(Genus, Trait_group, Trait)%>%
  tally%>%
  mutate(Percent = n / sum(n))

#prep for EDI
affinities2<-dplyr::select(affinities, -"n")
colnames(affinities2)<-c("Genus", "Trait_group", "Trait", "Trait_affinity")

#csv of trait affinities table for EDI
write.csv(affinities2, "~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/trait_affinities_table.csv")

################## Prep raw trait table to for hosting by EDI #####################################################
names(biotraits_revision4)

#create taxonomic resolution column
biotraits_revision4$Taxonomic_resolution<-NA
biotraits_revision4$Taxonomic_resolution<-ifelse(as.character(biotraits_revision4$Accepted_name)==as.character(biotraits_revision4$Genus),"Genus", NA)
biotraits_revision4$Taxonomic_resolution<-ifelse(as.character(biotraits_revision4$Accepted_name)==as.character(biotraits_revision4$Family),"Family", biotraits_revision4$Taxonomic_resolution)
biotraits_revision4$Taxonomic_resolution<-ifelse(as.character(biotraits_revision4$Accepted_name)==as.character(biotraits_revision4$Order),"Order", biotraits_revision4$Taxonomic_resolution)
biotraits_revision4$Taxonomic_resolution<-ifelse(is.na(biotraits_revision4$Taxonomic_resolution)&!is.na(biotraits_revision4$Genus), "Species", biotraits_revision4$Taxonomic_resolution)

#fix NAs in taxonomic resolution
no.res<-biotraits_revision4[which(is.na(biotraits_revision4$Taxonomic_resolution)),] #all are subfamily level
biotraits_revision4$Taxonomic_resolution[which(is.na(biotraits_revision4$Taxonomic_resolution))]<-"Subfamily"

#select columns
biotraits7.7<-dplyr::select(biotraits_revision4, -"Accepted_TSN")

#convert to long format- include trait comments
gathercol2<-c("AdultFlyingStrength_abbrev", "Emerge_season_1", "Emerge_season_2", "Emerge_season_comments" ,"Emerge_synch_abbrev", "Feed_prim_abbrev", "Feed_mode_sec", "Feed_mode_comments","Female_disp_abbrev", "Habit_prim", "Habit_sec", "Habit_comments","Max_body_size_abbrev", "Resp_abbrev", "Resp_comments","Rheophily_abbrev", "Thermal_pref", "Voltinism_abbrev", "Voltinism_comments")

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

