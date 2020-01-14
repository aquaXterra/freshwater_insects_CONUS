######################## Cleaning of occurrence data  #########################
##Author: Laura Twardochleb
##5/6/19

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

#Load workspace image
load(file='~/Documents/WaterCube/Ch.3/aquatic_insects/WQP_state_merge_2.RData')

#Load packages
library('tidyr')
library('lubridate')
library(taxize) 
library(rgdal)
library(usmap)
library(ggplot2)
library(maptools)
library(sp)
library('dplyr')
############## read-in datasets #############################################################################################
states<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/state_data.csv", stringsAsFactors = FALSE, na.string=c("", NA, "NA", "#N/A", "NULL"))
states1<-states[,-1]
#read in partially cleaned WQP data
insects3<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/insects2001v3.csv")
head(insects3)

############### to-dos #######################################################################################################
#merge state data with WQP data -complete
#get all dates into same format (states and WQP) -complete
#remove samples from before 2001 - complete
#remove duplicate records between states and WQP - complete
#check names as in trait dataset- assign accepted name and accepted_TSN - complete
#get higher taxonomic designations - complete
#remove non-insect taxa - complete
#remove terrestrial taxa - complete
#lat and long coordinates (map)- complete
#to check: states whose data are included and ensure that sampling method names match those in the state sampling method table- complete

################### Final to-dos for preparing data paper ####################################################################
#clean code- have versions for my ch. 3 analysis and versions for data paper
#check dataset format per EDI instructions
#create raw community_data_table and ancillary_location table

########### merge state and WQP data #########################################################################################
insects4<-subset(insects3, select = c("SubjectTaxonomicName", "MonitoringLocationIdentifier", "OrganizationFormalName.x", "ActivityStartDate", "SampleCollectionMethod.MethodName", "MonitoringLocationName", "ProviderName.x", "LatitudeMeasure", "LongitudeMeasure", "HorizontalCoordinateReferenceSystemDatumName"))
names(insects4)<-c("Submitted_name", "Location_ID", "Monitoring_organization", "Date", "Sample_method", "Location_description", "Provider_name", "Latitude", "Longitude", "HorizontalCoordinateReferenceSystemDatumName")                 
insects4$Sample_ID<-NA
insects5<-insects4[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "HorizontalCoordinateReferenceSystemDatumName", "Monitoring_organization")]
#prepare states data to rbind with WQP
states1$Monitoring_organization<-NA
names(states1)<-c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "HorizontalCoordinateReferenceSystemDatumName", "Monitoring_organization")

############ standardize date format- subset to 2001 and later ##############################################################################################################################################################################################################
unique(insects5$Date)
str(insects5)
insects5$Date<-as.character(insects5$Date)
#something is happening to dates when rbinding       
occurrence<-rbind(states1, insects5)   
str(occurrence)
occurrence$Date1<-parse_date_time(x = occurrence$Date,
                orders = c("y", "Y-m-d H:M:OS","Y","d m y", "m/d/y", "y-m-d", "ymd", "m/d/y H:M", "d-b-y", "d m Y", "m/d/Y", "Y-m-d", "Ymd", "m/d/Y H:M", "d-b-Y"), tz="America/New_York")
########need to figure out why these entries are messed up<----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
na.date<-occurrence[which(is.na(occurrence$Date1)),]

#subset to dates after 01/01/2001                 
occurrence2<-subset(occurrence, Date1>="2001-01-01")           
unique(occurrence2$Date1) 
unique(occurrence2$Provider_name)

########################### identify duplicate records using date, latitude, longitude, and Submitted_name ########################################################################################################################################
occurrence3<-occurrence2 %>%
  distinct(Submitted_name, Latitude, Longitude, Date1, .keep_all = TRUE)

########## remove terrestrial taxa using list from trait data cleaning #################################################################################################################################################################################################
terrestrial<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/terrestrial_taxa.csv", stringsAsFactors = FALSE)
#perform an anti-join to keep records that don't match
occurrence4<-anti_join(occurrence3, terrestrial, by=c("Submitted_name"= "SubjectTaxonomicName"))

########### remove records with NAs ####################################################################################################################################################################################################################
occurrence5<-occurrence4[!is.na(occurrence4$Latitude),]
occurrence6<-occurrence5[!is.na(occurrence5$Longitude),]
occurrence7<-occurrence6[!is.na(occurrence6$Submitted_name),]
occurrence8<-occurrence7[!is.na(occurrence7$Date),]

############ Check Submitted_names as in trait dataset #################################################################################################################################################################################################################  
#get upstream and downstream names from trait dataset
traits<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/code/Data_cleaning_WQP_traits/raw_trait_table.csv", stringsAsFactors = FALSE)
traits1<-traits[,c(2:7,9)]

#get list of unique taxa for Submitted_name in Traits1
traits2<-traits1 %>%
  distinct(Submitted_name, .keep_all = TRUE)

occurrence9<-merge(traits2, occurrence8, by="Submitted_name", all.y=TRUE)

#subset entries with NAs in Accepted name
na.occur<-subset(occurrence9, is.na(occurrence9$Accepted_name))

#####create a list of unique taxa and get records using 'taxize' library######
utaxon<-unique(na.occur$Submitted_name) #3500 with no accepted name after merging with names in trait dataset
valid_names<-tax_name(query=utaxon, get=c("order","family", "genus", "species"), db="itis") #retrieved higher and lower names from ITIS- some searches (~1500) failed to return anything 
write.csv(valid_names, "occurrence_valid_names.csv")
#read file
valid_names<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/occurrence_valid_names.csv")

#get tsn and accepted name for taxa with higher and lower valid names
#subset to taxa without na in Order
valid_names1<-subset(valid_names, !is.na(order)) #1500 names still have an NA for Order (no name info), 2000 (these) don't
#subset to insect taxa- 1171 insect taxa with no NA in Order after retrieving higher and lower names from ITIS
valid_names2<-subset(valid_names1,order=="Diptera"| order== "Odonata"|order== "Ephemeroptera"|order== "Lepidoptera"| order=="Coleoptera"|order=="Trichoptera"|order== "Hemiptera"|order== "Plecoptera"|order== "Megaloptera")
#valid_tsn<-get_tsn(valid_names2$query, searchtype="scientific", accepted=FALSE, ask=TRUE)

#internet search keeps failing- try breaking into smaller chunks
valid_names3<-valid_names2[1:500,]
valid_tsn1<-get_tsn(valid_names3$query, searchtype="scientific", accepted=FALSE, ask=TRUE)
write.csv(valid_tsn1, "occurrence_valid_tsn1.csv")
#read file
valid_tsn1<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/occurrence_valid_tsn1.csv", stringsAsFactors = FALSE)

valid_names4<-valid_names2[501:1171,]
valid_tsn2<-get_tsn(valid_names4$query, searchtype="scientific", accepted=FALSE, ask=TRUE)
write.csv(valid_tsn2, "occurrence_valid_tsn2.csv")
#read file
valid_tsn2<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/occurrence_valid_tsn2.csv", stringsAsFactors = FALSE)

valid_tsn<-rbind(valid_tsn1, valid_tsn2)

names_tsn<-cbind(valid_names2, valid_tsn) # 1171 insect taxa with no NAS in Order and their tsn from ITIS- these could be recombined with trait dataset

#write to csv so that won't have to run code again
write.csv(names_tsn, "occurrence_names_tsn.csv")
#read in file
names_tsn<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/occurrence_names_tsn.csv", stringsAsFactors = FALSE)

#now get accepted names for all taxa based on their tsn
itis_names<-itis_acceptname(searchtsn = names_tsn$valid_tsn) #get a vector of accepted names for each tsn
unique(itis_names$acceptedname) #all 1171 submitted insect names are accepted names 

#which names still don't have order, family, or genus assigned? find upstream and downstream designations, accepted names, and tsn
na.names<-subset(valid_names, is.na(order)) #1522 taxa still have an NA in name from original 3500 that were missing a name
#leave higher levels in to calculate richness using a mixture of taxonomic levels?

#remove all trailing entries like Group, group, gr., Gr., sp., Type 1, 
na.names$query<-gsub("Group|group|gr.|Gr.|sp.|Type 1|SP|SP.", "", na.names$query, fixed=FALSE)

#remove extra spaces between words
na.names$query<-gsub("  ", " ", na.names$query, fixed=TRUE)

#remove trailing spaces
na.names$query<-trimws(na.names$query, which = c("both", "left", "right"))

#remove extra period
na.names$query<-gsub("[.]", "", na.names$query, fixed=FALSE)

#remove entries with a slash in it
na.names1<-grep(paste("/"), na.names$query, value=TRUE,invert=TRUE)

#run ITIS search again
utaxon2<-unique(na.names1)
valid_names5<-tax_name(query=utaxon2, get=c("order","family", "genus", "species"), db="itis") #ran ITIS search again on 1327 taxa of 1522 that have an NA in Order- search worked for some tacxa but still failed for many

write.csv(valid_names5, "~/Documents/WaterCube/Ch.3/aquatic_insects/valid_names5.csv")

#read csv
valid_names5<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/valid_names5.csv", stringsAsFactors = FALSE)

#subset those that don't have an order assigned
na.names5<-subset(valid_names5, is.na(order)) #search failed for 691 taxa- I will manually assign names for these taxa

######### manually check names that can't be searched in ITIS using taxize ####################################################
#assign Order for non-insect orders and other relevant information for insects (other relevant=family, genus, species)
#then recombine with valid_names5 and create submitted_name_new that has name without errors taken from family, genus, or species column
#merge with valid_names by submitted_name_new and check to see if TSN and accepted_name already found
#for those without accepted name, find TSN and accepted name for insect orders
#recombine with occurrence dataset

#initiate accepted_name column
na.names5$accepted_name<-NA

#Faxonius is Orconectes- assign order Decapoda
na.names5$order[grep("Faxonius", na.names5$query)]<-"Decapoda" 

#Cricotopus is genus Cricotopus
na.names5$genus[grep("Cricotopus", na.names5$query)]<-"Cricotopus" 
na.names5$order[grep("Cricotopus", na.names5$query)]<-"Diptera" 
na.names5$family[grep("Cricotopus", na.names5$query)]<-"Chironomidae"

#Ablabesmyia
na.names5$order[grep("Ablabesmyia", na.names5$query)]<-"Diptera" 
na.names5$family[grep("Ablabesmyia", na.names5$query)]<-"Chironomidae" 
na.names5$genus[grep("Ablabesmyia", na.names5$query)]<-"Ablabesmyia"
na.names5$species[grep("Ablabesmyia rhamphe", na.names5$query)]<-"Ablabesmyia rhamphe"

#Acentrella ampla
na.names5$order[grep("Acentrella ampla", na.names5$query)]<-"Ephemeroptera"
na.names5$family[grep("Acentrella ampla", na.names5$query)]<-"Baetidae"
na.names5$genus[grep("Acentrella ampla", na.names5$query)]<-"Heterocloeon"
na.names5$accepted_name[grep("Acentrella ampla", na.names5$query)]<-"Heterocloeon amplum"
na.names5$species[grep("Acentrella ampla", na.names5$query)]<-"Heterocloeon amplum"

#Actinonaias ligamentina
na.names5$order[grep("Actinonaias ligamentina", na.names5$query)]<-"Unionoida"

#Aelosoma
na.names5$order[grep("Aeolosoma", na.names5$query)]<-"Annelida"

#Aeschna
na.names5$order[grep("AESCHN", na.names5$query)]<-"Odonata"
na.names5$family[grep("AESCHN", na.names5$query)]<-"Aeshnidae"
na.names5$genus[grep("AESCHNA", na.names5$query)]<-"Aeshna"

#Agabus
na.names5$order[grep("Agabini", na.names5$query)]<-"Coleoptera"
na.names5$family[grep("Agabini", na.names5$query)]<-"Dytiscidae"
na.names5$order[grep("Agabus", na.names5$query)]<-"Coleoptera"
na.names5$family[grep("Agabus", na.names5$query)]<-"Dytiscidae"
na.names5$genus[grep("Agabus", na.names5$query)]<-"Ilybiosoma"
na.names5$species[grep("Agabus perplexus", na.names5$query)]<-"Ilybiosoma perplexus"
na.names5$accepted_name[grep("Agabus perplexus", na.names5$query)]<-"Ilybiosoma perplexus"
na.names5$species[grep("Agabus regularis", na.names5$query)]<-"Ilybiosoma regulare"
na.names5$accepted_name[grep("Agabus regularis", na.names5$query)]<-"Ilybiosoma regulare"

#Agrionidae
na.names5$order[grep("AGRIONIDAE", na.names5$query)]<-"Odonata"
na.names5$family[grep("AGRIONIDAE", na.names5$query)]<-"Calopterygidae"

#Albertathyas
na.names5$order[grep("Albertathyas", na.names5$query)]<-"water mite"

#	Alotanypus aris
na.names5$order[grep("Alotanypus aris", na.names5$query)]<-"Diptera"
na.names5$family[grep("Alotanypus aris", na.names5$query)]<-"Chironomidae"
na.names5$genus[grep("Alotanypus aris", na.names5$query)]<-"Alotanypus"

#	Amblema plicata
na.names5$order[grep("Amblema plicata", na.names5$query)]<-"Unionoida"

na.names5$order[grep("AMERICAMYSIS AKIRA", na.names5$query)]<-"Mysis"
na.names5$order[grep("Amnicolidae", na.names5$query)]<-"Neotaenioglossa"
na.names5$order[grep("AMYGDALUM", na.names5$query)]<-"Mytiloida"
na.names5$order[grep("Anodonta", na.names5$query)]<-"Unionoida"
na.names5$order[grep("ARABELLIDAE", na.names5$query)]<-"Eunicida"


#Acronyx
na.names5$order[grep("Ancyronyx", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Ancyronyx", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Ancyronyx", na.names5$query, ignore.case = TRUE)]<-"Ancyronyx"

#Archilestes
na.names5$order[grep("Archilestes californica", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Archilestes californica", na.names5$query, ignore.case = TRUE)]<-"Lestidae"
na.names5$genus[grep("Archilestes californica", na.names5$query, ignore.case = TRUE)]<-"Archilestes"
na.names5$species[grep("Archilestes californica", na.names5$query, ignore.case = TRUE)]<-"Archilestes californicus"
na.names5$accepted_name[grep("Archilestes californica", na.names5$query, ignore.case = TRUE)]<-"Archilestes californicus"

#Aricidae
na.names5$order[grep("Aricidea", na.names5$query, ignore.case=TRUE)]<-"Annelida"
na.names5$order[grep("Armandia", na.names5$query, ignore.case=TRUE)]<-"Annelida"
na.names5$order[grep("Asychis", na.names5$query, ignore.case=TRUE)]<-"Annelida"
na.names5$order[grep("Atractides", na.names5$query, ignore.case=TRUE)]<-"Arachnida"

#Atrichopogon
na.names5$order[grep("Atrichopogon", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Atrichopogon", na.names5$query, ignore.case = TRUE)]<-"Ceraptogonidae"
na.names5$genus[grep("Atrichopogon", na.names5$query, ignore.case = TRUE)]<-"Atrichopogon"

#Baetis
na.names5$order[grep("Baetis", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Baetis", na.names5$query, ignore.case = TRUE)]<-"Baetidae"
na.names5$genus[grep("Baetis", na.names5$query, ignore.case = TRUE)]<-"Baetis"
na.names5$genus[grep("Baetis frondalis", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon"
na.names5$species[grep("Baetis frondalis", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon frondale"
na.names5$accepted_name[grep("Baetis frondalis", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon frondale"
na.names5$species[grep("Baetis bicaudatus", na.names5$query, ignore.case = TRUE)]<-"Baetis bicaudatus"
na.names5$accepted_name[grep("Baetis bicaudatus", na.names5$query, ignore.case = TRUE)]<-"Baetis bicaudatus"
na.names5$species[grep("Baetis flavistriga", na.names5$query, ignore.case = TRUE)]<-"Baetis flavistriga"
na.names5$accepted_name[grep("Baetis flavistriga", na.names5$query, ignore.case = TRUE)]<-"Baetis flavistriga"
na.names5$species[grep("Baetis piscatoris", na.names5$query, ignore.case = TRUE)]<-"Baetis piscatoris"
na.names5$accepted_name[grep("Baetis piscatoris", na.names5$query, ignore.case = TRUE)]<-"Baetis piscatoris"
na.names5$genus[grep("Baetis propinquus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon"
na.names5$species[grep("Baetis propinquus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon propinquum"
na.names5$accepted_name[grep("Baetis propinquus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon propinquum"
na.names5$species[grep("Baetis tricaudatus", na.names5$query, ignore.case = TRUE)]<-"Baetis tricaudatus"
na.names5$accepted_name[grep("Baetis tricaudatus", na.names5$query, ignore.case = TRUE)]<-"Baetis tricaudatus"

na.names5$order[grep("BALANOGLOSSUS", na.names5$query, ignore.case = TRUE)]<-"worm"
na.names5$order[grep("Batracobdella", na.names5$query, ignore.case = TRUE)]<-"Annelida"
na.names5$order[grep("BIOMPHALARIA OBSTRUCTUS", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#beardius
na.names5$order[grep("Beardius reissi", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Beardius reissi", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Beardius reissi", na.names5$query, ignore.case = TRUE)]<-"Beardius"

#Berosus
na.names5$order[grep("Berosus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Berosus", na.names5$query, ignore.case = TRUE)]<-"Hydrophilidae"
na.names5$genus[grep("Berosus", na.names5$query, ignore.case = TRUE)]<-"Berosus"
na.names5$species[grep("Berosus styliferus", na.names5$query, ignore.case = TRUE)]<-"Berosus stylifer"
na.names5$accepted_name[grep("Berosus styliferus", na.names5$query, ignore.case = TRUE)]<-"Berosus stylifer"

#Bethbilbekia
na.names5$order[grep("Bethbilbeckia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Bethbilbeckia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Bethbilbeckia", na.names5$query, ignore.case = TRUE)]<-"Bethbilbeckia"
na.names5$species[grep("Bethbilbeckia floridensis", na.names5$query, ignore.case = TRUE)]<-"Bethbilbeckia floridensis"

#Bilyjomyia algens
na.names5$order[grep("Bilyjomyia algens", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Bilyjomyia algens", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Bilyjomyia algens", na.names5$query, ignore.case = TRUE)]<-"Apsectrotanypus"
na.names5$species[grep("Bilyjomyia algens", na.names5$query, ignore.case = TRUE)]<-"Apsectrotanypus algens"
na.names5$accepted_name[grep("Bilyjomyia algens", na.names5$query, ignore.case = TRUE)]<-"Apsectrotanypus algens"

#Blepharicera
na.names5$order[grep("Blepharicera", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Blepharicera", na.names5$query, ignore.case = TRUE)]<-"Blephariceridae"
na.names5$genus[grep("Blepharicera", na.names5$query, ignore.case = TRUE)]<-"Blepharicera"
na.names5$species[grep("Blepharicera", na.names5$query, ignore.case = TRUE)]<-"Blepharicera magna"
na.names5$accepted_name[grep("Blepharicera", na.names5$query, ignore.case = TRUE)]<-"Blepharicera"

#Boreonectes
na.names5$order[grep("Boreonectes", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Boreonectes", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$genus[grep("Boreonectes", na.names5$query, ignore.case = TRUE)]<-"Stictotarsus"
na.names5$species[grep("Boreonectes aequinoctialis", na.names5$query, ignore.case = TRUE)]<-"Stictotarsus aequinoctialis"
na.names5$accepted_name[grep("Boreonectes aequinoctialis", na.names5$query, ignore.case = TRUE)]<-"Stictotarsus aequinoctialis"
na.names5$species[grep("Boreonectes striatellus", na.names5$query, ignore.case = TRUE)]<-"Stictotarsus striatellus"
na.names5$accepted_name[grep("Boreonectes striatellus", na.names5$query, ignore.case = TRUE)]<-"Stictotarsus striatellus"

#Brachycentridae
na.names5$order[grep("Brachycentridae", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Brachycentridae", na.names5$query, ignore.case = TRUE)]<-"Brachycentridae"
na.names5$order[grep("Brachycentrus", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Brachycentrus", na.names5$query, ignore.case = TRUE)]<-"Brachycentridae"
na.names5$genus[grep("Brachycentrus", na.names5$query, ignore.case = TRUE)]<-"Brachycentrus"
na.names5$species[which(na.names5$query=="Brachycentrus (Oligoplectrodes) americanus")]<-"Brachycentrus americanus"
na.names5$accepted_name[which(na.names5$query=="Brachycentrus (Oligoplectrodes) americanus")]<-"Brachycentrus americanus"
na.names5$species[which(na.names5$query=="Brachycentrus (Sphinctogaster) occidentalis")]<-"Brachycentrus occidentalis"
na.names5$accepted_name[which(na.names5$query=="Brachycentrus (Sphinctogaster) occidentalis")]<-"Brachycentrus occidentalis"

#Brachycercus maculatus
na.names5$order[grep("Brachycercus maculatus", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Brachycercus maculatus", na.names5$query, ignore.case = TRUE)]<-"Caenidae"
na.names5$genus[grep("Brachycercus maculatus", na.names5$query, ignore.case = TRUE)]<-"Sparbarus"
na.names5$species[which(na.names5$query=="Brachycercus maculatus")]<-"Sparbarus maculatus"
na.names5$accepted_name[which(na.names5$query=="Brachycercus maculatus")]<-"Sparbarus maculatus"

#Bratislavia 
na.names5$order[grep("Bratislavia", na.names5$query, ignore.case = TRUE)]<-"Oligochaeta"

#Braychura
na.names5$order[grep("Braychura", na.names5$query, ignore.case = TRUE)]<-"crab"

#Caecidotea
na.names5$order[grep("Caecidotea", na.names5$query, ignore.case = TRUE)]<-"Isopoda"

#Caenis
na.names5$order[grep("Caenis", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Caenis", na.names5$query, ignore.case = TRUE)]<-"Caenidae"
na.names5$genus[grep("Caenis", na.names5$query, ignore.case = TRUE)]<-"Caenis"
na.names5$species[which(na.names5$query=="Caenis diminuata")]<-"Caenis diminuta"
na.names5$accepted_name[which(na.names5$query=="Caenis diminuata")]<-"Caenis diminuta"

#CALLIANASSA 
na.names5$order[grep("CALLIANASSA", na.names5$query, ignore.case = TRUE)]<-"Decapoda"

#Cambarus 
na.names5$order[grep("Cambarus", na.names5$query, ignore.case = TRUE)]<-"Decapoda"

#capitella
na.names5$order[grep("Capitell", na.names5$query, ignore.case = TRUE)]<-"worm"

#Caudatella
na.names5$order[grep("Caudatella", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Caudatella", na.names5$query, ignore.case = TRUE)]<-"Ephemerellidae"
na.names5$genus[grep("Caudatella", na.names5$query, ignore.case = TRUE)]<-"Caudatella"
na.names5$species[which(na.names5$query=="Caudatella cascadia")]<-"Caudatella hystrix"
na.names5$accepted_name[which(na.names5$query=="Caudatella cascadia")]<-"Caudatella hystrix"

na.names5$species[which(na.names5$query=="Caudatella columbiella")]<-"Caudatella columbiella"
na.names5$accepted_name[which(na.names5$query=="Caudatella columbiella")]<-"Caudatella"

#Centroptilum viridocularis
na.names5$order[grep("Centroptilum viridocularis", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Centroptilum viridocularis", na.names5$query, ignore.case = TRUE)]<-"Baetidae"
na.names5$genus[grep("Centroptilum viridocularis", na.names5$query, ignore.case = TRUE)]<-"Procloeon"
na.names5$species[which(na.names5$query=="Centroptilum viridocularis")]<-"Procloeon viridoculare"
na.names5$accepted_name[which(na.names5$query=="Centroptilum viridocularis")]<-"Procloeon viridoculare"

#Ceraclea transversus
na.names5$order[grep("Ceraclea transversus", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Ceraclea transversus", na.names5$query, ignore.case = TRUE)]<-"Leptoceridae"
na.names5$genus[grep("Ceraclea transversus", na.names5$query, ignore.case = TRUE)]<-"Ceraclea"
na.names5$species[which(na.names5$query=="Ceraclea transversus")]<-"Ceraclea transversa"
na.names5$accepted_name[which(na.names5$query=="Ceraclea transversus")]<-"Ceraclea transversa"

#CERATOPOGONIDAE
na.names5$order[grep("CERATOPOGONIDAE", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("CERATOPOGONIDAE", na.names5$query, ignore.case = TRUE)]<-"Ceraptogonidae"

#Ceratopsyche
na.names5$order[grep("Ceratopsyche", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Ceratopsyche", na.names5$query, ignore.case = TRUE)]<-"Hydropsychidae"
na.names5$genus[grep("Ceratopsyche", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche"
na.names5$accepted_name[which(na.names5$query=="Ceratopsyche rna")]<-"Ceratopsyche"

na.names5$species[which(na.names5$query=="Ceratopsyche morosa bifida")]<-"Ceratopsyche morosa"
na.names5$accepted_name[which(na.names5$query=="Ceratopsyche morosa bifida")]<-"Ceratopsyche morosa"

#Chaetocladius ligni
na.names5$order[grep("Chaetocladius ligni", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Chaetocladius ligni", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Chaetocladius ligni", na.names5$query, ignore.case = TRUE)]<-"Chaetocladius"
na.names5$accepted_name[which(na.names5$query=="Chaetocladius ligni")]<-"Chaetocladius"

#CHAETOGNATHA
na.names5$order[grep("CHAETOGNATHA", na.names5$query, ignore.case = TRUE)]<-"worm"

#CHIONE CLENCHI
na.names5$order[grep("CHIONE CLENCHI", na.names5$query, ignore.case = TRUE)]<-"Veneroida"

#Chironomidae
na.names5$order[grep("Chironom", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Chironom", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$accepted_name[grep("Chironom", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Choroterpes
na.names5$order[grep("Choroterpes", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Choroterpes", na.names5$query, ignore.case = TRUE)]<-"Leptophlebiidae"
na.names5$genus[grep("Choroterpes", na.names5$query, ignore.case = TRUE)]<-"Choroterpes"
na.names5$species[grep("Choroterpes basilis", na.names5$query, ignore.case = TRUE)]<-"Choroterpes basalis"
na.names5$accepted_name[which(na.names5$query=="Choroterpes basilis")]<-"Choroterpes basalis"
na.names5$species[grep("Choroterpes hubbelli", na.names5$query, ignore.case = TRUE)]<-"Choroterpes basalis"
na.names5$accepted_name[which(na.names5$query=="Choroterpes hubbelli")]<-"Choroterpes basalis"

#CIRROPHORUS
na.names5$order[grep("CIRROPHORUS", na.names5$query, ignore.case = TRUE)]<-"worm"

#	Cladotanytarsus
na.names5$order[grep("Cladotanytarsus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Cladotanytarsus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Cladotanytarsus", na.names5$query, ignore.case = TRUE)]<-"Cladotantytarsus"
na.names5$accepted_name[grep("Cladotanytarsus", na.names5$query, ignore.case = TRUE)]<-"Cladotantytarsus"

#Clavidae
na.names5$order[grep("Clavidae", na.names5$query, ignore.case = TRUE)]<-"hydroid"

#Cleptelmus addenda
na.names5$order[grep("Cleptelmus addenda", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Cleptelmus addenda", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Cleptelmus addenda", na.names5$query, ignore.case = TRUE)]<-"Cleptelmis"
na.names5$species[grep("Cleptelmus addenda", na.names5$query, ignore.case = TRUE)]<-"Cleptelmis addenda"
na.names5$accepted_name[grep("Cleptelmus addenda", na.names5$query, ignore.case = TRUE)]<-"Cleptelmis addenda"

#Climacea areolaris	
na.names5$order[grep("Climacea", na.names5$query, ignore.case = TRUE)]<-"Neuroptera"
na.names5$family[grep("Climacea", na.names5$query, ignore.case = TRUE)]<-"Sisyridae"
na.names5$genus[grep("Climacea", na.names5$query, ignore.case = TRUE)]<-"Climacea"
na.names5$species[grep("Climacea", na.names5$query, ignore.case = TRUE)]<-"Climacea areolarsis"
na.names5$accepted_name[grep("Climacea", na.names5$query, ignore.case = TRUE)]<-"Climacea areolarsis"


na.names5$order[grep("Climacia", na.names5$query, ignore.case = TRUE)]<-"Neuroptera"
na.names5$family[grep("Climacia", na.names5$query, ignore.case = TRUE)]<-"Sisyridae"
na.names5$genus[grep("Climacia", na.names5$query, ignore.case = TRUE)]<-"Climacea"
na.names5$species[grep("Climacia", na.names5$query, ignore.case = TRUE)]<-"Climacea areolarsis"
na.names5$accepted_name[grep("Climacia", na.names5$query, ignore.case = TRUE)]<-"Climacea areolarsis"

#Cloeon cognatum
na.names5$order[grep("Cloeon cognatum", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Cloeon cognatum", na.names5$query, ignore.case = TRUE)]<-"Baetidae"
na.names5$genus[grep("Cloeon cognatum", na.names5$query, ignore.case = TRUE)]<-"Cloeon"
na.names5$species[grep("Cloeon cognatum", na.names5$query, ignore.case = TRUE)]<-"Cloeon dipterum"
na.names5$accepted_name[grep("Cloeon cognatum", na.names5$query, ignore.case = TRUE)]<-"Cloeon dipterum"

#CLYMENELLA TORQUATA
na.names5$order[grep("CLYMENELLA TORQUATA", na.names5$query, ignore.case = TRUE)]<-"annelida"

#Coptotomus lenticus
na.names5$order[grep("Coptotomus lenticus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Coptotomus lenticus", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$genus[grep("Coptotomus lenticus", na.names5$query, ignore.case = TRUE)]<-"Coptotomus"
na.names5$species[grep("Coptotomus lenticus", na.names5$query, ignore.case = TRUE)]<-"Coptotomus longulus lenticus"
na.names5$accepted_name[grep("Coptotomus lenticus", na.names5$query, ignore.case = TRUE)]<-"Coptotomus longulus lenticus"

#corticacarus
na.names5$order[grep("Corticacarus", na.names5$query, ignore.case = TRUE)]<-"mite"

#Corydalus texanus
na.names5$order[grep("Corydalus texanus", na.names5$query, ignore.case = TRUE)]<-"Megaloptera"
na.names5$family[grep("Corydalus texanus", na.names5$query, ignore.case = TRUE)]<-"Corydalidae"
na.names5$genus[grep("Corydalus texanus", na.names5$query, ignore.case = TRUE)]<-"Corydalus"
na.names5$species[grep("Corydalus texanus", na.names5$query, ignore.case = TRUE)]<-"Corydalus cornutus"
na.names5$accepted_name[grep("Corydalus texanus", na.names5$query, ignore.case = TRUE)]<-"Corydalus cornutus"

#Corynoneura
na.names5$order[grep("Corynoneura", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Corynoneura", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Corynoneura", na.names5$query, ignore.case = TRUE)]<-"Coryoneura"
na.names5$accepted_name[grep("Corynoneura", na.names5$query, ignore.case = TRUE)]<-"Coryoneura"

#Cosmopterygidae
na.names5$order[grep("Cosmopterygidae", na.names5$query, ignore.case = TRUE)]<-"Lepidoptera"
na.names5$family[grep("Cosmopterygidae", na.names5$query, ignore.case = TRUE)]<-"Cosmopterigidae"
na.names5$accepted_name[grep("Cosmopterygidae", na.names5$query, ignore.case = TRUE)]<-"Cosmopterigidae"

#COSSURA
na.names5$order[grep("COSSURA", na.names5$query, ignore.case = TRUE)]<-"annelida"

#Craspedacusta sowerbii
na.names5$order[grep("Cradacusta", na.names5$query, ignore.case = TRUE)]<-"medusa"

#	Cricotopus
na.names5$accepted_name[grep("Cricotopus", na.names5$query, ignore.case = TRUE)]<-"Cricotopus"
na.names5$species[which(na.names5$query=="Cricotopus (Cricotopus) bicinctus")]<-"Cricotopus bicinctus"
na.names5$accepted_name[which(na.names5$query=="Cricotopus (Cricotopus) bicinctus")]<-"Cricotopus bicinctus"
na.names5$species[which(na.names5$query=="Cricotopus (Cricotopus) trifascia")]<-"Cricotopus trifascia"
na.names5$accepted_name[which(na.names5$query=="Cricotopus (Cricotopus) trifascia")]<-"Cricotopus trifascia"
na.names5$species[which(na.names5$query=="Cricotopus (Nostoc) nostocicola")]<-"Cricotopus nostocicola"
na.names5$accepted_name[which(na.names5$query=="Cricotopus (Nostoc) nostocicola")]<-"Cricotopus nostocicola"
na.names5$species[which(na.names5$query=="Cricotopus (Nostococladius) nostocicola")]<-"Cricotopus nostocicola"
na.names5$accepted_name[which(na.names5$query=="Cricotopus (Nostococladius) nostocicola")]<-"Cricotopus nostocicola"
na.names5$species[which(na.names5$query=="Cricotopus bicinctus")]<-"Cricotopus bicinctus"
na.names5$accepted_name[which(na.names5$query=="Cricotopus bicinctus")]<-"Cricotopus bicinctus"
na.names5$species[which(na.names5$query=="Cricotopus lebetis")]<-"Cricotopus tricinctus"
na.names5$accepted_name[which(na.names5$query=="Cricotopus lebetis")]<-"Cricotopus tricinctus"
na.names5$species[which(na.names5$query=="Cricotopus patens")]<-"Cricotopus patens"
na.names5$accepted_name[which(na.names5$query=="Cricotopus patens")]<-"Cricotopus"
na.names5$species[which(na.names5$query=="Cricotopus reversus  epler")]<-"Cricotopus reversus"
na.names5$accepted_name[which(na.names5$query=="Cricotopus reversus  epler")]<-"Cricotopus reversus"
na.names5$species[which(na.names5$query=="Cricotopus santa fe epler")]<-"Cricotopus santa fe"
na.names5$accepted_name[which(na.names5$query=="Cricotopus santa fe epler")]<-"Cricotopus"
na.names5$species[which(na.names5$query=="Cricotopus silvestris ")]<-"Cricotopus sylvestris"
na.names5$accepted_name[which(na.names5$query=="Cricotopus silvestris ")]<-"Cricotopus sylvestris"
na.names5$species[which(na.names5$query=="Cricotopus sylvestris ")]<-"Cricotopus sylvestris"
na.names5$accepted_name[which(na.names5$query=="Cricotopus sylvestris ")]<-"Cricotopus sylvestris"

#Culoptila plummerensis
na.names5$order[grep("Culoptila plummerensis", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Culoptila plummerensis", na.names5$query, ignore.case = TRUE)]<-"Glossosomatidae"
na.names5$genus[grep("Culoptila plummerensis", na.names5$query, ignore.case = TRUE)]<-"Culoptila"
na.names5$species[grep("Culoptila plummerensis", na.names5$query, ignore.case = TRUE)]<-"Culoptila plummerensis"
na.names5$accepted_name[grep("Culoptila plummerensis", na.names5$query, ignore.case = TRUE)]<-"Culoptila"

#	Cyphon
na.names5$order[grep("Cyphon", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Cyphon", na.names5$query, ignore.case = TRUE)]<-"Scirtidae"
na.names5$genus[grep("Cyphon", na.names5$query, ignore.case = TRUE)]<-"Elodes"
na.names5$accepted_name[grep("Cyphon", na.names5$query, ignore.case = TRUE)]<-"Elodes"

#	Cypridae
na.names5$order[grep("Cypridae", na.names5$query, ignore.case = TRUE)]<-"Ostracoda"

#Cyrtobagous salviniae
na.names5$order[grep("Cyrtobagous salviniae", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Cyrtobagous salviniae", na.names5$query, ignore.case = TRUE)]<-"Erirhinidae"
na.names5$genus[grep("Cyrtobagous salviniae", na.names5$query, ignore.case = TRUE)]<-"Cyrtobagous"
na.names5$species[grep("Cyrtobagous salviniae", na.names5$query, ignore.case = TRUE)]<-"Cyrtobagous salviniae"
na.names5$accepted_name[grep("Cyrtobagous salviniae", na.names5$query, ignore.case = TRUE)]<-"Cyrtobagous"

#Dasyhelea
na.names5$order[grep("Dasyhele", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Dasyhele", na.names5$query, ignore.case = TRUE)]<-"Ceratopogonidae"
na.names5$genus[grep("Dasyhelea", na.names5$query, ignore.case = TRUE)]<-"Dashyhelea"
na.names5$accepted_name[grep("Dasyhelea", na.names5$query, ignore.case = TRUE)]<-"Dashyhelea"

na.names5$order[grep("Dashyeleinae", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Dashyeleinae", na.names5$query, ignore.case = TRUE)]<-"Ceratopogonidae"
na.names5$accepted_name[grep("Dashyeleinae", na.names5$query, ignore.case = TRUE)]<-"Ceratopogonidae"

#Demicryptochironomus
na.names5$genus[grep("Demicryptochironomus", na.names5$query, ignore.case = TRUE)]<-"Demicryptochironomus"
na.names5$accepted_name[grep("Demicryptochironomus", na.names5$query, ignore.case = TRUE)]<-"Demicryptochironomus"

#Dero
na.names5$order[grep("Dero", na.names5$query, ignore.case = TRUE)]<-"oligochaete"

#	Derotanypus
na.names5$order[grep("Derotanypus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Derotanypus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Derotanypus", na.names5$query, ignore.case = TRUE)]<-"Derotanypus"

#Dicrotendipes
na.names5$order[grep("Dicrotendipes", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Dicrotendipes", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Dicrotendipes", na.names5$query, ignore.case = TRUE)]<-"Dicrotendipes"
na.names5$accepted_name[grep("Dicrotendipes", na.names5$query, ignore.case = TRUE)]<-"Dicrotendipes"

#Dineutus
na.names5$order[grep("Dineutus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Dineutus", na.names5$query, ignore.case = TRUE)]<-"Gyrinidae"
na.names5$genus[grep("Dineutus", na.names5$query, ignore.case = TRUE)]<-"Dineutus"
na.names5$accepted_name[grep("Dineutus", na.names5$query, ignore.case = TRUE)]<-"Dineutus"

#Diplectrona metaqui
na.names5$order[grep("Diplectrona metaqui", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Diplectrona metaqui", na.names5$query, ignore.case = TRUE)]<-"Hydropsychidae"
na.names5$genus[grep("Diplectrona metaqui", na.names5$query, ignore.case = TRUE)]<-"Diplectrona"
na.names5$species[grep("Diplectrona metaqui", na.names5$query, ignore.case = TRUE)]<-"Diplectrona metaqui"
na.names5$accepted_name[grep("Diplectrona metaqui", na.names5$query, ignore.case = TRUE)]<-"Diplectrona metaqui"

#DIPLOTHYRA SMYTHI
na.names5$order[grep("DIPLOTHYRA SMYTHI", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Dipolydora
na.names5$order[grep("Dipolydora", na.names5$query, ignore.case = TRUE)]<-"worm"

#Discolomidae
na.names5$order[grep("Discolomidae", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Discolomidae", na.names5$query, ignore.case = TRUE)]<-"Discolomatidae"
na.names5$accepted_name[grep("Discolomidae", na.names5$query, ignore.case = TRUE)]<-"Discolomatidae"

#	Djalmabatista pulchra
na.names5$order[grep("Djalmabatista pulchra", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Djalmabatista pulchra", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Djalmabatista pulchra", na.names5$query, ignore.case = TRUE)]<-"Djalmabatista"
na.names5$species[grep("Djalmabatista pulchra", na.names5$query, ignore.case = TRUE)]<-"Djalmabatista pulcher"
na.names5$accepted_name[grep("Djalmabatista pulchra", na.names5$query, ignore.case = TRUE)]<-"Djalmabatista pulcher"

#Drunella doddsi
na.names5$order[grep("Drunella doddsi", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Drunella doddsi", na.names5$query, ignore.case = TRUE)]<-"Ephemerellidae"
na.names5$genus[grep("Drunella doddsi", na.names5$query, ignore.case = TRUE)]<-"Drunella"
na.names5$species[grep("Drunella doddsi", na.names5$query, ignore.case = TRUE)]<-"Drunella doddsii"
na.names5$accepted_name[grep("Drunella doddsi", na.names5$query, ignore.case = TRUE)]<-"Drunella doddsii"

#	Dubiraphia
na.names5$order[grep("Dubiraphia", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Dubiraphia", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Dubiraphia", na.names5$query, ignore.case = TRUE)]<-"Dubiraphia"
na.names5$accepted_name[grep("Dubiraphia", na.names5$query, ignore.case = TRUE)]<-"Dubiraphia"

#Dugesia tina
na.names5$order[grep("Dugesia tina", na.names5$query, ignore.case = TRUE)]<-"Neoophora"

#	Dytiscidae
na.names5$order[grep("Dytiscidae", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Dytiscidae", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$accepted_name[grep("Dytiscidae", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"

#Ectopria thoracica
na.names5$order[grep("Ectopria thoracica", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Ectopria thoracica", na.names5$query, ignore.case = TRUE)]<-"Psephenidae"
na.names5$genus[grep("Ectopria thoracica", na.names5$query, ignore.case = TRUE)]<-"Ectopria"
na.names5$species[grep("Ectopria thoracica", na.names5$query, ignore.case = TRUE)]<-"Ectopria nervosa"
na.names5$accepted_name[grep("Ectopria thoracica", na.names5$query, ignore.case = TRUE)]<-"Ectopria nervosa"

#	Ectoprocta
na.names5$order[grep("Ectoprocta", na.names5$query, ignore.case = TRUE)]<-"bryozoan"

#	EDOTEA
na.names5$order[grep("EDOTEA", na.names5$query, ignore.case = TRUE)]<-"Crustacea"

#Einfeldia
na.names5$order[grep("Einfeldia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Einfeldia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Einfeldia", na.names5$query, ignore.case = TRUE)]<-"Einfeldia"
na.names5$accepted_name[grep("Einfeldia", na.names5$query, ignore.case = TRUE)]<-"Einfeldia"

#Elimia
na.names5$order[grep("Elimia", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Ellipteroides
na.names5$order[grep("Ellipteroides", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Ellipteroides", na.names5$query, ignore.case = TRUE)]<-"Limoniidae"
na.names5$genus[grep("Ellipteroides", na.names5$query, ignore.case = TRUE)]<-"Ellipteroides"

#	Empididae
na.names5$order[grep("Empididae", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Empididae", na.names5$query, ignore.case = TRUE)]<-"Empididae"
na.names5$accepted_name[grep("Empididae", na.names5$query, ignore.case = TRUE)]<-"Empididae"

#Enallagma 
na.names5$order[grep("Enallagma", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Enallagma", na.names5$query, ignore.case = TRUE)]<-"Coenagrionidae"
na.names5$genus[grep("Enallagma", na.names5$query, ignore.case = TRUE)]<-"Enallagma"
na.names5$species[grep("Enallagma cardenium", na.names5$query, ignore.case = TRUE)]<-"Enallagma coecum"
na.names5$accepted_name[grep("Enallagma cardenium", na.names5$query, ignore.case = TRUE)]<-"Enallagma coecum"

#Enallagma daekii
na.names5$species[grep("Enallagma daekii", na.names5$query, ignore.case = TRUE)]<-"Enallagma daeckii"
na.names5$accepted_name[grep("Enallagma daekii", na.names5$query, ignore.case = TRUE)]<-"Enallagma daeckii"

#	Enchytraeidae
na.names5$order[grep("Enchytra", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#	Endochironomus
na.names5$genus[grep("Endochironomus", na.names5$query, ignore.case = TRUE)]<-"Endochironomus"
na.names5$accepted_name[grep("Endochironomus", na.names5$query, ignore.case = TRUE)]<-"Endochironomus"

#Enochrus collinus
na.names5$order[grep("Enochrus collinus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Enochrus collinus", na.names5$query, ignore.case = TRUE)]<-"Hydrophilidae"
na.names5$genus[grep("Enochrus collinus", na.names5$query, ignore.case = TRUE)]<-"Enochrus"
na.names5$species[grep("Enochrus collinus", na.names5$query, ignore.case = TRUE)]<-"Enochrus hamiltoni"
na.names5$accepted_name[grep("Enochrus collinus", na.names5$query, ignore.case = TRUE)]<-"Enochrus hamiltoni"

#Epeorus
na.names5$order[grep("Epeorus", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Epeorus", na.names5$query, ignore.case = TRUE)]<-"Heptageniidae"
na.names5$genus[grep("Epeorus", na.names5$query, ignore.case = TRUE)]<-"Epeorus"
na.names5$accepted_name[grep("Epeorus", na.names5$query, ignore.case = TRUE)]<-"Epeorus"

#Epicordulia
na.names5$order[grep("Epicordulia", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Epicordulia", na.names5$query, ignore.case = TRUE)]<-"Corduliidae"
na.names5$genus[grep("Epicordulia", na.names5$query, ignore.case = TRUE)]<-"Epitheca"
na.names5$accepted_name[grep("Epicordulia", na.names5$query, ignore.case = TRUE)]<-"Epitheca"

#	Epicordulia princeps
na.names5$species[grep("Epicordulia regina", na.names5$query, ignore.case = TRUE)]<-"Epitheca princeps regina"
na.names5$accepted_name[grep("Epicordulia regina", na.names5$query, ignore.case = TRUE)]<-"Epitheca princeps regina"

#Epicordulia regina
na.names5$species[grep("Epicordulia princeps", na.names5$query, ignore.case = TRUE)]<-"Epitheca princeps"
na.names5$accepted_name[grep("Epicordulia princeps", na.names5$query, ignore.case = TRUE)]<-"Epitheca princeps"

#	Epicordulia princeps regina
na.names5$species[grep("Epicordulia princeps regina", na.names5$query, ignore.case = TRUE)]<-"Epitheca princeps regina"
na.names5$accepted_name[grep("Epicordulia princeps regina", na.names5$query, ignore.case = TRUE)]<-"Epitheca princeps regina"

#Epitheca princeps
na.names5$order[grep("Epitheca princeps", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Epitheca princeps", na.names5$query, ignore.case = TRUE)]<-"Corduliidae"
na.names5$genus[grep("Epitheca princeps", na.names5$query, ignore.case = TRUE)]<-"Epitheca"
na.names5$species[grep("Epitheca princeps", na.names5$query, ignore.case = TRUE)]<-"Epitheca princeps"
na.names5$accepted_name[grep("Epitheca princeps", na.names5$query, ignore.case = TRUE)]<-"Epitheca princeps"

#ERIOCERA
na.names5$order[grep("ERIOCERA", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("ERIOCERA", na.names5$query, ignore.case = TRUE)]<-"Tipulidae"
na.names5$genus[grep("ERIOCERA", na.names5$query, ignore.case = TRUE)]<-"Hexatoma"
na.names5$accepted_name[grep("ERIOCERA", na.names5$query, ignore.case = TRUE)]<-"Hexatoma"

#Erpobdella microstoma
na.names5$order[grep("Erpobdella microstoma", na.names5$query, ignore.case = TRUE)]<-"Hirudinea"

#	Erythemis collacata
na.names5$order[grep("Erythemis collacata", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Erythemis collacata", na.names5$query, ignore.case = TRUE)]<-"Libellulidae"
na.names5$genus[grep("Erythemis collacata", na.names5$query, ignore.case = TRUE)]<-"Erythemis"
na.names5$species[grep("Erythemis collacata", na.names5$query, ignore.case = TRUE)]<-"Erythemis collocata"
na.names5$accepted_name[grep("Erythemis collacata", na.names5$query, ignore.case = TRUE)]<-"Erythemis collocata"

#Eubrianax edwardsi
na.names5$order[grep("Eubrianax edwardsi", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Eubrianax edwardsi", na.names5$query, ignore.case = TRUE)]<-"Psephenidae"
na.names5$genus[grep("Eubrianax edwardsi", na.names5$query, ignore.case = TRUE)]<-"Eubrianax"
na.names5$species[grep("Eubrianax edwardsi", na.names5$query, ignore.case = TRUE)]<-"Eubrianax edwardsii"
na.names5$accepted_name[grep("Eubrianax edwardsi", na.names5$query, ignore.case = TRUE)]<-"Eubrianax edwardsii"

#	EUCLYMENE
na.names5$order[grep("EUCLYMENE", na.names5$query, ignore.case = TRUE)]<-"Annelidaa"

#Eukiefferiella
na.names5$order[grep("Eukiefferiella", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Eukiefferiella", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Eukiefferiella", na.names5$query, ignore.case = TRUE)]<-"Eukiefferiella"

#Eukiefferiella tirolensis
na.names5$species[grep("Eukiefferiella tirolensis", na.names5$query, ignore.case = TRUE)]<-"Eukiefferiella tirolensis"
na.names5$accepted_name[grep("Eukiefferiella tirolensis", na.names5$query, ignore.case = TRUE)]<-"Eukiefferiella"

#	Eukiefferiella devonica 
na.names5$species[grep("Eukiefferiella devonica ", na.names5$query, ignore.case = TRUE)]<-"Eukiefferiella devonica"
na.names5$accepted_name[grep("Eukiefferiella devonica ", na.names5$query, ignore.case = TRUE)]<-"Eukiefferiella devonica"

#FALLCEON QUILLERI
na.names5$order[grep("FALLCEON QUILLERI", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("FALLCEON QUILLERI", na.names5$query, ignore.case = TRUE)]<-"Baetidae"
na.names5$genus[grep("FALLCEON QUILLERI", na.names5$query, ignore.case = TRUE)]<-"Fallceon"
na.names5$species[grep("FALLCEON QUILLERI", na.names5$query, ignore.case = TRUE)]<-"Fallceon quilleri"
na.names5$accepted_name[grep("FALLCEON QUILLERI", na.names5$query, ignore.case = TRUE)]<-"Fallceon quilleri"

#Fissimentum
na.names5$order[grep("Fissimentum", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Fissimentum", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Fissimentum", na.names5$query, ignore.case = TRUE)]<-"Fissimentum"

#Floridobia
na.names5$order[grep("Floridobia", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Fridericia
na.names5$order[grep("Fridericia", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#	Frontipodopsis
na.names5$order[grep("Frontipodopsis", na.names5$query, ignore.case = TRUE)]<-"Arachnida"

#	GERANOMYIA
na.names5$order[grep("GERANOMYIA", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("GERANOMYIA", na.names5$query, ignore.case = TRUE)]<-"Tipulidae"
na.names5$genus[grep("GERANOMYIA", na.names5$query, ignore.case = TRUE)]<-"Limonia"
na.names5$accepted_name[grep("GERANOMYIA", na.names5$query, ignore.case = TRUE)]<-"Limonia"

#Glossiphonia elegans
na.names5$order[grep("Glossiphonia elegans", na.names5$query, ignore.case = TRUE)]<-"Hirudinea"

#Glossosoma intermedia
na.names5$order[grep("Glossosoma intermedia", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Glossosoma intermedia", na.names5$query, ignore.case = TRUE)]<-"Glossosomatidae"
na.names5$genus[grep("Glossosoma intermedia", na.names5$query, ignore.case = TRUE)]<-"Glossosoma"
na.names5$species[grep("Glossosoma intermedia", na.names5$query, ignore.case = TRUE)]<-"Glossosoma intermedium"
na.names5$accepted_name[grep("Glossosoma intermedia", na.names5$query, ignore.case = TRUE)]<-"Glossosoma intermedium"

#Glyptotendipes
na.names5$order[grep("Glyptotendipes", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Glyptotendipes", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Glyptotendipes", na.names5$query, ignore.case = TRUE)]<-"Glyptotendipes"
na.names5$accepted_name[grep("Glyptotendipes", na.names5$query, ignore.case = TRUE)]<-"Glyptotendipes"

#Goeldichironomus cf natans
na.names5$order[grep("Goeldichironomus cf natans", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Goeldichironomus cf natans", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Goeldichironomus cf natans", na.names5$query, ignore.case = TRUE)]<-"Goeldichironomus"
na.names5$species[grep("Goeldichironomus cf natans", na.names5$query, ignore.case = TRUE)]<-"Goeldichironomus natans"
na.names5$accepted_name[grep("Goeldichironomus cf natans", na.names5$query, ignore.case = TRUE)]<-"Goeldichironomus natans"

#Gomphurus
na.names5$order[grep("Gomphurus", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Gomphurus", na.names5$query, ignore.case = TRUE)]<-"Gomphidae"
na.names5$genus[grep("Gomphurus", na.names5$query, ignore.case = TRUE)]<-"Gomphus"
na.names5$accepted_name[grep("Gomphurus", na.names5$query, ignore.case = TRUE)]<-"Gomphus"

#Gomphurus modestus
na.names5$species[grep("Gomphurus modestus", na.names5$query, ignore.case = TRUE)]<-"Gomphus modestus"
na.names5$accepted_name[grep("Gomphurus modestus", na.names5$query, ignore.case = TRUE)]<-"Gomphus modestus"

#Gymnometiocnemus
na.names5$order[grep("Gymnometiocnemus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Gymnometiocnemus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Gymnometiocnemus", na.names5$query, ignore.case = TRUE)]<-"Gymnometiocnemus"

#Haplotaxis
na.names5$order[grep("Haplotaxis", na.names5$query, ignore.case = TRUE)]<-"Ooligochaete"

#Harnischia
na.names5$order[grep("Harnischia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Harnischia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Harnischia", na.names5$query, ignore.case = TRUE)]<-"Harnischia"
na.names5$accepted_name[grep("Harnischia", na.names5$query, ignore.case = TRUE)]<-"Harnischia"

#	Hayesomyia senata
na.names5$order[grep("Hayesomyia senata", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Hayesomyia senata", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Hayesomyia senata", na.names5$query, ignore.case = TRUE)]<-"Thienemanniella"
na.names5$species[grep("Hayesomyia senata", na.names5$query, ignore.case = TRUE)]<-"Thienemanniella senata"
na.names5$accepted_name[grep("Hayesomyia senata", na.names5$query, ignore.case = TRUE)]<-"Thienemanniella senata"

#Hebetancylus excentricus
na.names5$order[grep("Hebetancylus excentricus", na.names5$query, ignore.case = TRUE)]<-"limpet"

#Heleniella ornaticollis
na.names5$order[grep("Heleniella ornaticollis", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Heleniella ornaticollis", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Heleniella ornaticollis", na.names5$query, ignore.case = TRUE)]<-"Heleniella"
na.names5$species[grep("Heleniella ornaticollis", na.names5$query, ignore.case = TRUE)]<-"Heleniella ornaticollis"
na.names5$accepted_name[grep("Heleniella ornaticollis", na.names5$query, ignore.case = TRUE)]<-"Heleniella"

#Helisoma trivolvis
na.names5$order[grep("Helisoma trivolvis", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Helobdella elongata
na.names5$order[grep("Helobdella elongata", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Helodidae
na.names5$order[grep("Helodidae", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Helodidae", na.names5$query, ignore.case = TRUE)]<-"Scirtidae"
na.names5$accepted_name[grep("Helodidae", na.names5$query, ignore.case = TRUE)]<-"Scirtidae"

#Helopelopia
na.names5$order[grep("Helopelopia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Helopelopia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Helopelopia", na.names5$query, ignore.case = TRUE)]<-"Conchapelopia"
na.names5$accepted_name[grep("Helopelopia", na.names5$query, ignore.case = TRUE)]<-"Conchapelopia"

#Hemerodromia
na.names5$order[grep("Hemerodromia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Hemerodromia", na.names5$query, ignore.case = TRUE)]<-"Empididae"
na.names5$genus[grep("Hemerodromia", na.names5$query, ignore.case = TRUE)]<-"Hemerodromia"
na.names5$accepted_name[grep("Hemerodromia", na.names5$query, ignore.case = TRUE)]<-"Hemerodromia"

#Henlea
na.names5$order[grep("Henlea", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Heterocloeon
na.names5$order[grep("Heterocloeon", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Heterocloeon", na.names5$query, ignore.case = TRUE)]<-"Baetidae"
na.names5$genus[grep("Heterocloeon", na.names5$query, ignore.case = TRUE)]<-"Heterocloeon"
na.names5$accepted_name[grep("Heterocloeon", na.names5$query, ignore.case = TRUE)]<-"Heterocloeon"

#Heterocloeon anoka
na.names5$order[grep("Heterocloeon anoka", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Heterocloeon anoka", na.names5$query, ignore.case = TRUE)]<-"Baetidae"
na.names5$genus[grep("Heterocloeon anoka", na.names5$query, ignore.case = TRUE)]<-"Iswaeon"
na.names5$species[grep("Heterocloeon anoka", na.names5$query, ignore.case = TRUE)]<-"Iswaeon anoka"
na.names5$accepted_name[grep("Heterocloeon anoka", na.names5$query, ignore.case = TRUE)]<-"Iswaeon anoka"

#HETEROMASTUS
na.names5$order[grep("HETEROMASTUS", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Heterotrissocladius
na.names5$order[grep("Heterotrissocladius", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Heterotrissocladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Heterotrissocladius", na.names5$query, ignore.case = TRUE)]<-"Heterotrissocladius"
na.names5$accepted_name[grep("Heterotrissocladius", na.names5$query, ignore.case = TRUE)]<-"Heterotrissocladius"

#	Heterotrissocladius subpilosa
na.names5$species[grep("Heterotrissocladius subpilosa", na.names5$query, ignore.case = TRUE)]<-"Heterotrissocladius subpilosus"
na.names5$accepted_name[grep("Heterotrissocladius subpilosa", na.names5$query, ignore.case = TRUE)]<-"Heterotrissocladius subpilosus"

#HOBSONIA FLORIDA
na.names5$order[grep("HOBSONIA FLORIDA", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Holorusia hera
na.names5$order[grep("Holorusia hera", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Holorusia hera", na.names5$query, ignore.case = TRUE)]<-"Tipulidae"
na.names5$genus[grep("Holorusia hera", na.names5$query, ignore.case = TRUE)]<-"Holorusia"
na.names5$species[grep("Holorusia hera", na.names5$query, ignore.case = TRUE)]<-"Holorusia hera"
na.names5$accepted_name[grep("Holorusia hera", na.names5$query, ignore.case = TRUE)]<-"Holorusia"

#HOLOTHUROIDA
na.names5$order[grep("HOLOTHUROIDA", na.names5$query, ignore.case = TRUE)]<-"Sea cucumber"

#Hydracarina
na.names5$order[grep("Hydracarina", na.names5$query, ignore.case = TRUE)]<-"water mite"

#Hydroporus
na.names5$order[grep("Hydroporus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Hydroporus", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$genus[grep("Hydroporus", na.names5$query, ignore.case = TRUE)]<-"Hydroporus"
na.names5$accepted_name[grep("Hydroporus", na.names5$query, ignore.case = TRUE)]<-"Hydroporus"

#Hydroporus dichorous
na.names5$species[grep("Hydroporus dichorous", na.names5$query, ignore.case = TRUE)]<-"Hydroporus dichrous"
na.names5$accepted_name[grep("Hydroporus dichorous", na.names5$query, ignore.case = TRUE)]<-"Hydroporus dichrous"

#Hydropsyche alhedra
na.names5$order[grep("Hydropsyche alhedra", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Hydropsyche alhedra", na.names5$query, ignore.case = TRUE)]<-"Hydropsychidae"
na.names5$genus[grep("Hydropsyche alhedra", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche"
na.names5$species[grep("Hydropsyche alhedra", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche alhedra"
na.names5$accepted_name[grep("Hydropsyche alhedra", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche alhedra"

#Hydropsyche
na.names5$order[grep("Hydropsyche", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Hydropsyche", na.names5$query, ignore.case = TRUE)]<-"Hydropsychidae"
na.names5$genus[grep("Hydropsyche", na.names5$query, ignore.case = TRUE)]<-"Hydropsyche"

#Hydropsyche bifida
na.names5$genus[grep("Hydropsyche bifida", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche"
na.names5$species[grep("Hydropsyche bifida", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche morosa"
na.names5$accepted_name[grep("Hydropsyche bifida", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche morosa"

#Hydropsyche macleodi
na.names5$genus[grep("Hydropsyche macleodi", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche"
na.names5$species[grep("Hydropsyche macleodi", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche macleodi"
na.names5$accepted_name[grep("Hydropsyche macleodi", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche macleodi"

#Hydropsyche morosa
na.names5$genus[grep("Hydropsyche morosa", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche"
na.names5$species[grep("Hydropsyche morosa", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche morosa"
na.names5$accepted_name[grep("Hydropsyche morosa", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche morosa"

#Hydropsyche slossonae
na.names5$genus[grep("Hydropsyche slossonae", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche"
na.names5$species[grep("Hydropsyche slossonae", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche slossonae"
na.names5$accepted_name[grep("Hydropsyche slossonae", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche slossonae"

#	Hydropsyche ventura
na.names5$genus[grep("Hydropsyche ventura", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche"
na.names5$species[grep("Hydropsyche ventura", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche ventura"
na.names5$accepted_name[grep("Hydropsyche ventura", na.names5$query, ignore.case = TRUE)]<-"Ceratopsyche ventura"

#Hydrosmittia
na.names5$order[grep("Hydrosmittia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Hydrosmittia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Hydrosmittia", na.names5$query, ignore.case = TRUE)]<-"Hydrosmittia"

#Hylogomphus geminatus
na.names5$order[grep("Hylogomphus geminatus", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Hylogomphus geminatus", na.names5$query, ignore.case = TRUE)]<-"Gomphidae"
na.names5$genus[grep("Hylogomphus geminatus", na.names5$query, ignore.case = TRUE)]<-"Gomphus"
na.names5$species[grep("Hylogomphus geminatus", na.names5$query, ignore.case = TRUE)]<-"Gomphus geminatus"
na.names5$accepted_name[grep("Hylogomphus geminatus", na.names5$query, ignore.case = TRUE)]<-"Gomphus geminatus"

#	Isonychia sicca
na.names5$order[grep("Isonychia sicca", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Isonychia sicca", na.names5$query, ignore.case = TRUE)]<-"Isonychiidae"
na.names5$genus[grep("Isonychia sicca", na.names5$query, ignore.case = TRUE)]<-"Isonychia"
na.names5$species[grep("Isonychia sicca", na.names5$query, ignore.case = TRUE)]<-"Isonychia sicca"
na.names5$accepted_name[grep("Isonychia sicca", na.names5$query, ignore.case = TRUE)]<-"Isonychia sicca"

#Kloosia dorsenna
na.names5$order[grep("Kloosia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Kloosia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Kloosia", na.names5$query, ignore.case = TRUE)]<-"Kloosia"
na.names5$species[grep("Kloosia dorsenna", na.names5$query, ignore.case = TRUE)]<-"Kloosia dorsenna"

#Kribiodorum
na.names5$order[grep("Kribiodorum", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Kribiodorum", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Kribiodorum", na.names5$query, ignore.case = TRUE)]<-"Kribiodorum"

#Labiobaetis dardanus
na.names5$genus[grep("Labiobaetis dardanus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon"
na.names5$species[grep("Labiobaetis dardanus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon dardanum"
na.names5$accepted_name[grep("Labiobaetis dardanus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon dardanum"

#Labiobaetis ephippiatus
na.names5$genus[grep("Labiobaetis", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon"
na.names5$species[grep("Labiobaetis ephippiatus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon ephippiatum"
na.names5$accepted_name[grep("Labiobaetis ephippiatus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon ephippiatum"

#	Labiobaetis longipalpus
na.names5$species[grep("Labiobaetis longipalpus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon longipalpus"
na.names5$accepted_name[grep("Labiobaetis longipalpus", na.names5$query, ignore.case = TRUE)]<-"Pseudocloeon longipalpus"

#	Labrundinia
na.names5$order[grep("Labrundinia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Labrundinia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Labrundinia", na.names5$query, ignore.case = TRUE)]<-"Labrudinia"
na.names5$species[grep("Labrundinia virescens", na.names5$query, ignore.case = TRUE)]<-"Labrundinia virescens"
na.names5$accepted_name[grep("Labrundinia virescens", na.names5$query, ignore.case = TRUE)]<-"Labrundinia virescens"
na.names5$accepted_name[grep("Labrundinia", na.names5$query, ignore.case = TRUE)]<-"Labrudinia"

#Laccophilus
na.names5$order[grep("Laccophilus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Laccophilus", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$genus[grep("Laccophilus", na.names5$query, ignore.case = TRUE)]<-"Laccophilus"
na.names5$accepted_name[grep("Laccophilus", na.names5$query, ignore.case = TRUE)]<-"Laccophilus"

#Lampsilis subangulata
na.names5$order[grep("Lampsilis subangulata", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#	Lara
na.names5$order[grep("Lara", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Lara", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Lara", na.names5$query, ignore.case = TRUE)]<-"Lara"
na.names5$accepted_name[grep("Lara", na.names5$query, ignore.case = TRUE)]<-"Lara"

#Larsia
na.names5$order[grep("Larsia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Larsia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Larsia", na.names5$query, ignore.case = TRUE)]<-"Larsia"
na.names5$accepted_name[grep("Larsia", na.names5$query, ignore.case = TRUE)]<-"Larsia"

#LEITOSCOLOPLOS
na.names5$order[grep("LEITOSCOLOPLOS", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Lepidostoma
na.names5$order[grep("Lepidostoma", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Lepidostoma", na.names5$query, ignore.case = TRUE)]<-"Lepidostomatidae"
na.names5$genus[grep("Lepidostoma", na.names5$query, ignore.case = TRUE)]<-"Lepidostoma"
na.names5$accepted_name[grep("Lepidostoma", na.names5$query, ignore.case = TRUE)]<-"Lepidostoma"

#LEPTOHYPHES PACKERI
na.names5$order[grep("LEPTOHYPHES PACKERI", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("LEPTOHYPHES PACKERI", na.names5$query, ignore.case = TRUE)]<-"	Leptohyphidae"
na.names5$genus[grep("LEPTOHYPHES PACKERI", na.names5$query, ignore.case = TRUE)]<-"Vacupernius"
na.names5$species[grep("LEPTOHYPHES PACKERI", na.names5$query, ignore.case = TRUE)]<-"Vacupernius packeri"
na.names5$accepted_name[grep("LEPTOHYPHES PACKERI", na.names5$query, ignore.case = TRUE)]<-"Vacupernius packeri"

#LEUCOTRICHA	
na.names5$order[grep("LEUCOTRICHA", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("LEUCOTRICHA", na.names5$query, ignore.case = TRUE)]<-"Hydroptilidae"
na.names5$genus[grep("LEUCOTRICHA", na.names5$query, ignore.case = TRUE)]<-"Leucotrichia"
na.names5$accepted_name[grep("LEUCOTRICHA", na.names5$query, ignore.case = TRUE)]<-"Leucotrichia"

#Libellulinae
na.names5$order[grep("Libellulinae", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Libellulinae", na.names5$query, ignore.case = TRUE)]<-"Libellulidae"
na.names5$accepted_name[grep("Libellulinae", na.names5$query, ignore.case = TRUE)]<-"Libellulidae"

#Limnodrilus claparedianus
na.names5$order[grep("Limnodrilus claparedianus", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#	Limnophila
na.names5$order[grep("Limnophila", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Limnophila", na.names5$query, ignore.case = TRUE)]<-"Tipulidae"
na.names5$genus[grep("Limnophila", na.names5$query, ignore.case = TRUE)]<-"Limnophila"
na.names5$accepted_name[grep("Limnophila", na.names5$query, ignore.case = TRUE)]<-"Limnophila"

#Liodessus affinis
na.names5$order[grep("Liodessus affinis", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Liodessus affinis", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$genus[grep("Liodessus affinis", na.names5$query, ignore.case = TRUE)]<-"Liodessus"
na.names5$species[grep("Liodessus affinis", na.names5$query, ignore.case = TRUE)]<-"Liodessus affinis"
na.names5$accepted_name[grep("Liodessus affinis", na.names5$query, ignore.case = TRUE)]<-"Liodessus affinis"

#Lipiniella
na.names5$order[grep("Lipiniella", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Lipiniella", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Lipiniella", na.names5$query, ignore.case = TRUE)]<-"Lipiniella"
na.names5$accepted_name[grep("Lipiniella", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#	Lopescladius (Cordiella)
na.names5$order[grep("Lopescladius", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Lopescladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Lopescladius", na.names5$query, ignore.case = TRUE)]<-"Lopescladius"
na.names5$accepted_name[grep("Lopescladius", na.names5$query, ignore.case = TRUE)]<-"Lopescladius"

#	Lumbricina
na.names5$order[grep("Lumbricina", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#LUMBRINERIS VERILLI
na.names5$order[grep("LUMBRINERIS VERILLI", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#	Lutrochus
na.names5$order[grep("Lutrochus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Lutrochus", na.names5$query, ignore.case = TRUE)]<-"Lutrochidae"
na.names5$genus[grep("Lutrochus", na.names5$query, ignore.case = TRUE)]<-"Lutrochus"
na.names5$accepted_name[grep("Lutrochus", na.names5$query, ignore.case = TRUE)]<-"Lutrochus"

#Lymnaecia
na.names5$order[grep("Lymnaecia", na.names5$query, ignore.case = TRUE)]<-"Lepidoptera"
na.names5$family[grep("Lymnaecia", na.names5$query, ignore.case = TRUE)]<-"Cosmopterigidae"
na.names5$genus[grep("Lymnaecia", na.names5$query, ignore.case = TRUE)]<-"Limnaecia"
na.names5$accepted_name[grep("Lymnaecia", na.names5$query, ignore.case = TRUE)]<-"Limnaecia"

#MACRELMIS
na.names5$order[grep("MACRELMIS", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("MACRELMIS", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("MACRELMIS", na.names5$query, ignore.case = TRUE)]<-"Macrelmis"
na.names5$accepted_name[grep("MACRELMIS", na.names5$query, ignore.case = TRUE)]<-"Macrelmis"

#Macromiidae
na.names5$order[grep("Macromiidae", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Macromiidae", na.names5$query, ignore.case = TRUE)]<-"Corduliidae"
na.names5$accepted_name[grep("Macromiidae", na.names5$query, ignore.case = TRUE)]<-"Corduliidae"

#	Macronychus
na.names5$order[grep("Macronychus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Macronychus", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Macronychus", na.names5$query, ignore.case = TRUE)]<-"Macronychus"
na.names5$accepted_name[grep("Macronychus", na.names5$query, ignore.case = TRUE)]<-"Macronychus"

#MALDANIDAE
na.names5$order[grep("MALDANIDAE", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#MALMGRENIELLA 
na.names5$order[grep("MALMGRENIELLA", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Mammersellides
na.names5$order[grep("Mammersellides", na.names5$query, ignore.case = TRUE)]<-"Arachnida"

#	Marionina
na.names5$order[grep("Marionina", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#MEDIOMASTUS
na.names5$order[grep("MEDIOMASTUS", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Melanoides tuberculata
na.names5$order[grep("Melanoides tuberculata", na.names5$query, ignore.case = TRUE)]<-"Molluska"

#Mesenchytraeus
na.names5$order[grep("Mesenchytraeus", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Mesobates
na.names5$order[grep("Mesobates", na.names5$query, ignore.case = TRUE)]<-"water mite"

#Metriocnemus eurynotus
na.names5$order[grep("Metriocnemus eurynotus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Metriocnemus eurynotus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Metriocnemus eurynotus", na.names5$query, ignore.case = TRUE)]<-"Metriocnemus"
na.names5$species[grep("Metriocnemus eurynotus", na.names5$query, ignore.case = TRUE)]<-"Metriocnemus eurynotus"
na.names5$accepted_name[grep("Metriocnemus eurynotus", na.names5$query, ignore.case = TRUE)]<-"Metriocnemus"

#Micrasema
na.names5$order[grep("Micrasema", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Micrasema", na.names5$query, ignore.case = TRUE)]<-"Brachycentridae"
na.names5$genus[grep("Micrasema", na.names5$query, ignore.case = TRUE)]<-"Micrasema"
na.names5$accepted_name[grep("Micrasema", na.names5$query, ignore.case = TRUE)]<-"Micrasema"

#Micromenetus
na.names5$order[grep("Micromenetus", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Micropsectra
na.names5$order[grep("Micropsectra", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Micropsectra", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Micropsectra", na.names5$query, ignore.case = TRUE)]<-"Micropsectra"
na.names5$accepted_name[grep("Micropsectra", na.names5$query, ignore.case = TRUE)]<-"Micropsectra"

#Microtendipes
na.names5$order[grep("Microtendipes", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Microtendipes", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Microtendipes", na.names5$query, ignore.case = TRUE)]<-"Microtendipes"
na.names5$species[grep("Microtendipes pedellus", na.names5$query, ignore.case = TRUE)]<-"Microtendipes pedellus"
na.names5$accepted_name[grep("Microtendipes pedellus", na.names5$query, ignore.case = TRUE)]<-"Microtendipes pedellus"
na.names5$species[grep("Microtendipes rydalensis", na.names5$query, ignore.case = TRUE)]<-"Microtendipes rydalensis"
na.names5$accepted_name[grep("Microtendipes rydalensis", na.names5$query, ignore.case = TRUE)]<-"Microtendipes rydalensis"

#	Mooreobdella
na.names5$order[grep("Mooreobdella", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#	Mystacides alafimbriata
na.names5$order[grep("Mystacides alafimbriata", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Mystacides alafimbriata", na.names5$query, ignore.case = TRUE)]<-"Leptoceridae"
na.names5$genus[grep("Mystacides alafimbriata", na.names5$query, ignore.case = TRUE)]<-"Mystacides"
na.names5$species[grep("Mystacides alafimbriata", na.names5$query, ignore.case = TRUE)]<-"Mystacides alafimbriatus"
na.names5$accepted_name[grep("Mystacides alafimbriata", na.names5$query, ignore.case = TRUE)]<-"Mystacides alafimbriatus"

#MYTILOPSIS LEUCOPHAETA
na.names5$order[grep("MYTILOPSIS LEUCOPHAETA", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#	Nais
na.names5$order[grep("Nais", na.names5$query, ignore.case = TRUE)]<-"Oligochaete"

#	Nanocladius
na.names5$order[grep("Nanocladius", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Nanocladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Nanocladius", na.names5$query, ignore.case = TRUE)]<-"Nanocladius"
na.names5$accepted_name[grep("Nanocladius", na.names5$query, ignore.case = TRUE)]<-"Nanocladius"

#	Natarsia
na.names5$order[grep("Natarsia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Natarsia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Natarsia", na.names5$query, ignore.case = TRUE)]<-"Natarsia"
na.names5$accepted_name[grep("Natarsia", na.names5$query, ignore.case = TRUE)]<-"Natarsia"

#NEANTHES MICROMMA
na.names5$order[grep("NEANTHES MICROMMA", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Nematomorpha
na.names5$order[grep("Nemato", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Nemertea
na.names5$order[grep("Nemertea", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Neocloeon
na.names5$order[grep("Neocloeon", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Neocloeon", na.names5$query, ignore.case = TRUE)]<-"Baetidae"
na.names5$genus[grep("Neocloeon", na.names5$query, ignore.case = TRUE)]<-"Neocloeon"
na.names5$accepted_name[grep("Neocloeon", na.names5$query, ignore.case = TRUE)]<-"Neocloeon"

#Neoleptophlebia
na.names5$order[grep("Neoleptophlebia", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Neoleptophlebia", na.names5$query, ignore.case = TRUE)]<-"Leptophlebiidae"
na.names5$genus[grep("Neoleptophlebia", na.names5$query, ignore.case = TRUE)]<-"Neoleptophlebia"
na.names5$accepted_name[grep("Neoleptophlebia", na.names5$query, ignore.case = TRUE)]<-"Leptophlebiidae"

#Neophylax lewisae
na.names5$order[grep("Neophylax lewisae", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Neophylax lewisae", na.names5$query, ignore.case = TRUE)]<-"Uenoidae"
na.names5$genus[grep("Neophylax lewisae", na.names5$query, ignore.case = TRUE)]<-"Neophylax"
na.names5$species[grep("Neophylax lewisae", na.names5$query, ignore.case = TRUE)]<-"Neophylax lewisae"
na.names5$accepted_name[grep("Neophylax lewisae", na.names5$query, ignore.case = TRUE)]<-"Neophylax"

#	Neostempellina
na.names5$order[grep("Neostempellina", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Neostempellina", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Neostempellina", na.names5$query, ignore.case = TRUE)]<-"Neostempellina"
na.names5$species[grep("Neostempellina reissi", na.names5$query, ignore.case = TRUE)]<-"Neostempellina reissi"
na.names5$accepted_name[grep("Neostempellina", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#NEREIDAE	
na.names5$order[grep("NEREIDAE", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#NOTOMASTUS LOBATUS
na.names5$order[grep("NOTOMASTUS LOBATUS", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Odontomyia
na.names5$order[grep("Odontomyia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Odontomyia", na.names5$query, ignore.case = TRUE)]<-"Stratiomyidae"
na.names5$genus[grep("Odontomyia", na.names5$query, ignore.case = TRUE)]<-"Odontomyia"
na.names5$accepted_name[grep("Odontomyia", na.names5$query, ignore.case = TRUE)]<-"Odontomyia"

#ODOSTOMIA IMPRESSA
na.names5$order[grep("ODOSTOMIA IMPRESSA", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Oecetis
na.names5$order[grep("Oecetis", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Oecetis", na.names5$query, ignore.case = TRUE)]<-"Leptoceridae"
na.names5$genus[grep("Oecetis", na.names5$query, ignore.case = TRUE)]<-"Oecetis"
na.names5$accepted_name[grep("Oecetis", na.names5$query, ignore.case = TRUE)]<-"Oecetis"

#Oecetis morsei
na.names5$species[grep("Oecetis morsei", na.names5$query, ignore.case = TRUE)]<-"Oecetis morsei"
na.names5$accepted_name[grep("Oecetis morsei", na.names5$query, ignore.case = TRUE)]<-"Oecetis morsei"

#Oliveiriella
na.names5$order[grep("Oliveiriella", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Oliveiriella", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Oliveiriella", na.names5$query, ignore.case = TRUE)]<-"Oliveiriella"
na.names5$accepted_name[grep("Oliveiriella", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#OPHELIIDAE
na.names5$order[grep("OPHELIIDAE", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#OPHIUROIDEA
na.names5$order[grep("OPHIUROIDEA", na.names5$query, ignore.case = TRUE)]<-"Asterozoa"

#	Optioservus
na.names5$order[grep("Optioservus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Optioservus", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Optioservus", na.names5$query, ignore.case = TRUE)]<-"Optioservus"
na.names5$accepted_name[grep("Optioservus", na.names5$query, ignore.case = TRUE)]<-"Optioservus"

#	ORBINIIDAE
na.names5$order[grep("ORBINIIDAE", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Oreodytes
na.names5$order[grep("Oreodytes", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Oreodytes", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$genus[grep("Oreodytes", na.names5$query, ignore.case = TRUE)]<-"Oreodytes"
na.names5$accepted_name[grep("Oreodytes", na.names5$query, ignore.case = TRUE)]<-"Oreodytes"

#Oreoleptis torrenticola
na.names5$order[grep("Oreoleptis torrenticola", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Oreoleptis torrenticola", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Oreoleptis torrenticola", na.names5$query, ignore.case = TRUE)]<-"Oreoleptis"
na.names5$species[grep("Oreoleptis torrenticola", na.names5$query, ignore.case = TRUE)]<-"Oreoleptis torrenticola"
na.names5$accepted_name[grep("Oreoleptis torrenticola", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#	Oribatei
na.names5$order[grep("Oribat", na.names5$query, ignore.case = TRUE)]<-"Arachnida"

#Ormosia
na.names5$order[grep("Ormosia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Ormosia", na.names5$query, ignore.case = TRUE)]<-"Tipulidae"
na.names5$genus[grep("Ormosia", na.names5$query, ignore.case = TRUE)]<-"Ormosia"
na.names5$accepted_name[grep("Ormosia", na.names5$query, ignore.case = TRUE)]<-"Ormosia"

#Orthocladiinae
na.names5$order[grep("Orthoclad", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Orthoclad", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$accepted_name[grep("Orthoclad", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Orthocladius
na.names5$genus[grep("Orthocladius", na.names5$query, ignore.case = TRUE)]<-"Orthocladius"
na.names5$accepted_name[grep("Orthocladius", na.names5$query, ignore.case = TRUE)]<-"Orthocladius"

#Orthocladius rivicola
na.names5$species[grep("rivicola", na.names5$query, ignore.case = TRUE)]<-"Orthocladius rivicola"

#Orthocladius rivulorum
na.names5$species[grep("rivulorum", na.names5$query, ignore.case = TRUE)]<-"Orthocladius rivulorum"

#Orthocladius lignicola
na.names5$species[grep("lignicola", na.names5$query, ignore.case = TRUE)]<-"Orthocladius lignicola"

#Orthocladius luteipes
na.names5$species[grep("luteipes", na.names5$query, ignore.case = TRUE)]<-"Orthocladius luteipes"

#Orthocladius rubicundus
na.names5$species[grep("rubicundus", na.names5$query, ignore.case = TRUE)]<-"Orthocladius rubicundus"

#Ostrocera	
na.names5$order[grep("Ostrocera", na.names5$query, ignore.case = TRUE)]<-"Plecoptera"
na.names5$family[grep("Ostrocera", na.names5$query, ignore.case = TRUE)]<-"Nemouridae"
na.names5$genus[grep("Ostrocera", na.names5$query, ignore.case = TRUE)]<-"Ostrocera"
na.names5$accepted_name[grep("Ostrocera", na.names5$query, ignore.case = TRUE)]<-"Ostrocera"

#	Paederus
na.names5$order[grep("Paederus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Paederus", na.names5$query, ignore.case = TRUE)]<-"Staphylinidae"
na.names5$genus[grep("Paederus", na.names5$query, ignore.case = TRUE)]<-"Paederus"
na.names5$accepted_name[grep("Paederus", na.names5$query, ignore.case = TRUE)]<-"Staphylinidae"

#Parachironomus
na.names5$order[grep("Parachironomus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Parachironomus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Parachironomus", na.names5$query, ignore.case = TRUE)]<-"Parachironomus"
na.names5$accepted_name[grep("Parachironomus", na.names5$query, ignore.case = TRUE)]<-"Parachironomus"

#Parachironomus chaetoalus complex
na.names5$species[grep("Parachironomus chaetoalus complex", na.names5$query, ignore.case = TRUE)]<-"Parachironomus chaetaolus"
na.names5$accepted_name[grep("Parachironomus chaetoalus complex", na.names5$query, ignore.case = TRUE)]<-"Parachironomus chaetaolus"

#Parachironomus longistilus
na.names5$species[grep("Parachironomus longistilus", na.names5$query, ignore.case = TRUE)]<-"Parachironomus longistilus"

#	Parachironomus supparilis
na.names5$species[grep("Parachironomus supparilis", na.names5$query, ignore.case = TRUE)]<-"Parachironomus supparilis"

#Parachironomus tenuicaudatus complex
na.names5$species[grep("Parachironomus tenuicaudatus complex", na.names5$query, ignore.case = TRUE)]<-"Parachironomus tenuicaudatus"

#	Parakiefferiella
na.names5$order[grep("Parakiefferiella", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Parakiefferiella", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Parakiefferiella", na.names5$query, ignore.case = TRUE)]<-"Parakiefferiella"
na.names5$accepted_name[grep("Parakiefferiella", na.names5$query, ignore.case = TRUE)]<-"Parakiefferiella"

#Paralauterborniella nihalteralis
na.names5$order[grep("Paralauterborniella nihalter", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Paralauterborniella nihalter", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Paralauterborniella nihalter", na.names5$query, ignore.case = TRUE)]<-"Paralauterborniella"
na.names5$species[grep("Paralauterborniella nihalter", na.names5$query, ignore.case = TRUE)]<-"Paralauterborniella nigrohalterale"
na.names5$accepted_name[grep("Paralauterborniella nihalter", na.names5$query, ignore.case = TRUE)]<-"Paralauterborniella nigrohalterale"

#	Paraleptophlebia temperalis
na.names5$order[grep("Paraleptophlebia temperalis", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Paraleptophlebia temperalis", na.names5$query, ignore.case = TRUE)]<-"Leptophlebiidae"
na.names5$genus[grep("Paraleptophlebia temperalis", na.names5$query, ignore.case = TRUE)]<-"Paraleptophlebia"
na.names5$species[grep("Paraleptophlebia temperalis", na.names5$query, ignore.case = TRUE)]<-"Paraleptophlebia temporalis"
na.names5$accepted_name[grep("Paraleptophlebia temperalis", na.names5$query, ignore.case = TRUE)]<-"Paraleptophlebia temporalis"

#Parametriocnemus
na.names5$order[grep("Parametriocnemus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Parametriocnemus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Parametriocnemus", na.names5$query, ignore.case = TRUE)]<-"Parametriocnemus"
na.names5$accepted_name[grep("Parametriocnemus", na.names5$query, ignore.case = TRUE)]<-"Parametriocnemus"

#PARAMPHINOME
na.names5$order[grep("PARAMPHINOME", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#PARAONIDAE
na.names5$order[grep("PARAONIDAE", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Paraphaenocladius
na.names5$order[grep("Paraphaenocladius", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Paraphaenocladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Paraphaenocladius", na.names5$query, ignore.case = TRUE)]<-"Paraphaenocladius"
na.names5$accepted_name[grep("Paraphaenocladius", na.names5$query, ignore.case = TRUE)]<-"Paraphaenocladius"

#Paratanytarsus
na.names5$order[grep("Paratanytarsus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Paratanytarsus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Paratanytarsus", na.names5$query, ignore.case = TRUE)]<-"Paratanytarsus"
na.names5$accepted_name[grep("Paratanytarsus", na.names5$query, ignore.case = TRUE)]<-"Paratanytarsus"

#PECTINARIA GOULDI
na.names5$order[grep("PECTINARIA GOULDI", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Pelecypoda
na.names5$order[grep("Pelecypoda", na.names5$query, ignore.case = TRUE)]<-"Bivalvia"

#Peloscolex ferox
na.names5$order[grep("Peloscolex ferox", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Peltodytes 
na.names5$order[grep("Peltodytes", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Peltodytes", na.names5$query, ignore.case = TRUE)]<-"Haliplidae"
na.names5$genus[grep("Peltodytes", na.names5$query, ignore.case = TRUE)]<-"Peltodytes"
na.names5$accepted_name[grep("Peltodytes", na.names5$query, ignore.case = TRUE)]<-"Peltodytes"

#PENAEUs
na.names5$order[grep("PENAEUs", na.names5$query, ignore.case = TRUE)]<-"shrimp"

#Perithemis
na.names5$order[grep("Perithemis", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Perithemis", na.names5$query, ignore.case = TRUE)]<-"Libellulidae"
na.names5$genus[grep("Perithemis", na.names5$query, ignore.case = TRUE)]<-"Perithemis"

#Perithemis seminole
na.names5$species[grep("Perithemis", na.names5$query, ignore.case = TRUE)]<-"Perithemis tenera"
na.names5$accepted_name[grep("Perithemis", na.names5$query, ignore.case = TRUE)]<-"Perithemis tenera"

#Perlesta
na.names5$order[grep("Perlesta", na.names5$query, ignore.case = TRUE)]<-"Plecoptera"
na.names5$family[grep("Perlesta", na.names5$query, ignore.case = TRUE)]<-"Perlidae"
na.names5$genus[grep("Perlesta", na.names5$query, ignore.case = TRUE)]<-"Perlesta"

#	Perlesta ephelida
na.names5$species[grep("Perlesta ephelida", na.names5$query, ignore.case = TRUE)]<-"Perlesta ephelida"
na.names5$accepted_name[grep("Perlesta ephelida", na.names5$query, ignore.case = TRUE)]<-"Perlesta"

#Perlesta placida complex
na.names5$species[grep("Perlesta placida", na.names5$query, ignore.case = TRUE)]<-"Perlesta placida"
na.names5$accepted_name[grep("Perlesta placida", na.names5$query, ignore.case = TRUE)]<-"Perlesta placida"

#Petrophilia
na.names5$order[grep("Petrophilia", na.names5$query, ignore.case = TRUE)]<-"Lepidoptera"
na.names5$family[grep("Petrophilia", na.names5$query, ignore.case = TRUE)]<-"Crambidae"
na.names5$genus[grep("Petrophilia", na.names5$query, ignore.case = TRUE)]<-"Petrophila"
na.names5$accepted_name[grep("Petrophilia", na.names5$query, ignore.case = TRUE)]<-"Petrophila"

#Phaenopsectra obediens 
na.names5$order[grep("Phaenopsectra", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Phaenopsectra", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Phaenopsectra", na.names5$query, ignore.case = TRUE)]<-"Phaenopsectra"
na.names5$species[grep("Phaenopsectra obediens", na.names5$query, ignore.case = TRUE)]<-"Phaenopsectra obediens"
na.names5$accepted_name[grep("Phaenopsectra obediens", na.names5$query, ignore.case = TRUE)]<-"Phaenopsectra obediens"

#Phaenopsectra punctipes 
na.names5$species[grep("Phaenopsectra punctipes", na.names5$query, ignore.case = TRUE)]<-"Phaenopsectra punctipes"
na.names5$accepted_name[grep("Phaenopsectra punctipes", na.names5$query, ignore.case = TRUE)]<-"Phaenopsectra punctipes"

#PHASCOLION STROMBI
na.names5$order[grep("PHASCOLION STROMBI", na.names5$query, ignore.case = TRUE)]<-"worm"

#	Phasganophora 
na.names5$order[grep("Phasganophora", na.names5$query, ignore.case = TRUE)]<-"Plecoptera"
na.names5$family[grep("Phasganophora", na.names5$query, ignore.case = TRUE)]<-"Perlidae"
na.names5$genus[grep("Phasganophora", na.names5$query, ignore.case = TRUE)]<-"Agnetina"
na.names5$accepted_name[grep("Phasganophora", na.names5$query, ignore.case = TRUE)]<-"Agnetina"

#PHORONIS ARCHITECTA
na.names5$order[grep("PHORONIS ARCHITECTA", na.names5$query, ignore.case = TRUE)]<-"worm"

#	Physa
na.names5$order[grep("Physa", na.names5$query, ignore.case = TRUE)]<-"mollusk"

#	Phytotelmatocladius
na.names5$order[grep("Phytotelmatocladius", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Phytotelmatocladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Phytotelmatocladius", na.names5$query, ignore.case = TRUE)]<-"Phytotelmatocladius"
na.names5$accepted_name[grep("Phytotelmatocladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#PILARGIS BERKELEYAE
na.names5$order[grep("PILARGIS BERKELEYAE", na.names5$query, ignore.case = TRUE)]<-"worm"

#Placobdella pediculata
na.names5$order[grep("Placobdella pediculata", na.names5$query, ignore.case = TRUE)]<-"hirudinea"

#	Pleurocera
na.names5$order[grep("Pleurocera", na.names5$query, ignore.case = TRUE)]<-"Mollusk"

#	PODARKEOPSIS BREVIPALPA
na.names5$order[grep("PODARKEOPSIS BREVIPALPA", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Polypedilum
na.names5$order[grep("Polypedilum", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Polypedilum", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Polypedilum", na.names5$query, ignore.case = TRUE)]<-"Polypedilum"

#	Polypedilum flavum
na.names5$species[grep("Polypedilum flavum", na.names5$query, ignore.case = TRUE)]<-"Polypedilum flavum"
na.names5$accepted_name[grep("Polypedilum flavum", na.names5$query, ignore.case = TRUE)]<-"Polypedilum"

#Polypedilum beckae
na.names5$species[grep("Polypedilum beckae", na.names5$query, ignore.case = TRUE)]<-"Polypedilum beckae"
na.names5$accepted_name[grep("Polypedilum beckae", na.names5$query, ignore.case = TRUE)]<-"Polypedilum"

#Polypedilum convictum 
na.names5$species[grep("Polypedilum convictum", na.names5$query, ignore.case = TRUE)]<-"Polypedilum convictum"
na.names5$accepted_name[grep("Polypedilum convictum", na.names5$query, ignore.case = TRUE)]<-"Polypedilum convictum"

#	Polypedilum halterale 
na.names5$species[grep("Polypedilum halterale", na.names5$query, ignore.case = TRUE)]<-"Polypedilum halterale"
na.names5$accepted_name[grep("Polypedilum halterale", na.names5$query, ignore.case = TRUE)]<-"Polypedilum halterale"

#Polypedilum illinoense 
na.names5$species[grep("Polypedilum illinoense", na.names5$query, ignore.case = TRUE)]<-"Polypedilum illinoense"
na.names5$accepted_name[grep("Polypedilum illinoense", na.names5$query, ignore.case = TRUE)]<-"Polypedilum illinoense"

#Polypedilum scalaenum 
na.names5$species[grep("Polypedilum scalaenum", na.names5$query, ignore.case = TRUE)]<-"Polypedilum scalaenum"
na.names5$accepted_name[grep("Polypedilum scalaenum", na.names5$query, ignore.case = TRUE)]<-"Polypedilum scalaenum"

#Polypedilum trigonus
na.names5$species[grep("Polypedilum trigonus", na.names5$query, ignore.case = TRUE)]<-"Polypedilum trigonum"
na.names5$accepted_name[grep("Polypedilum trigonus", na.names5$query, ignore.case = TRUE)]<-"Polypedilum trigonum"

#Potthastia Longimanus
na.names5$order[grep("Potthastia Longimanus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Potthastia Longimanus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Potthastia Longimanus", na.names5$query, ignore.case = TRUE)]<-"Potthastia"
na.names5$species[grep("Potthastia Longimanus", na.names5$query, ignore.case = TRUE)]<-"Potthastia longimana"
na.names5$accepted_name[grep("Potthastia Longimanus", na.names5$query, ignore.case = TRUE)]<-"Potthastia longimana"

#PRIONOIO CIRRIFERA
na.names5$order[grep("PRIONOIO CIRRIFERA", na.names5$query, ignore.case = TRUE)]<-"worm"

#Pristinella
na.names5$order[grep("Pristinella", na.names5$query, ignore.case = TRUE)]<-"worm"

#Procladius
na.names5$order[grep("Procladius", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Procladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Procladius", na.names5$query, ignore.case = TRUE)]<-"Procladius"

#	Procladius bellus
na.names5$species[grep("Procladius bellus", na.names5$query, ignore.case = TRUE)]<-"Procladius bellus"
na.names5$accepted_name[grep("Procladius bellus", na.names5$query, ignore.case = TRUE)]<-"Procladius bellus"

#	Procloeon hobbsi
na.names5$order[grep("Procloeon", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Procloeon", na.names5$query, ignore.case = TRUE)]<-"Baetidae"
na.names5$genus[grep("Procloeon", na.names5$query, ignore.case = TRUE)]<-"Procloeon"
na.names5$species[grep("Procloeon hobbsi", na.names5$query, ignore.case = TRUE)]<-"Procloeon rufostrigatum"
na.names5$accepted_name[grep("Procloeon hobbsi", na.names5$query, ignore.case = TRUE)]<-"Procloeon rufostrigatum"

#Procloeon viridocularis
na.names5$species[grep("Procloeon viridocularis", na.names5$query, ignore.case = TRUE)]<-"Procloeon viridoculare"
na.names5$accepted_name[grep("Procloeon viridocularis", na.names5$query, ignore.case = TRUE)]<-"Procloeon viridoculare"

#	Promoresia tardella
na.names5$order[grep("Promoresia tardella", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Promoresia tardella", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Promoresia tardella", na.names5$query, ignore.case = TRUE)]<-"Optioservus"
na.names5$species[grep("Promoresia tardella", na.names5$query, ignore.case = TRUE)]<-"Optioservus tardellus"
na.names5$accepted_name[grep("Promoresia tardella", na.names5$query, ignore.case = TRUE)]<-"Optioservus tardellus"

#Prostoma rubrum
na.names5$order[grep("Prostoma rubrum", na.names5$query, ignore.case = TRUE)]<-"worm"

#	Psectrocladius psilopterus 
na.names5$order[grep("Psectrocladius psilopterus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Psectrocladius psilopterus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Psectrocladius psilopterus", na.names5$query, ignore.case = TRUE)]<-"Psectrocladius"
na.names5$species[grep("Psectrocladius psilopterus", na.names5$query, ignore.case = TRUE)]<-"Psectrocladius psilopterus"
na.names5$accepted_name[grep("Psectrocladius psilopterus", na.names5$query, ignore.case = TRUE)]<-"Psectrocladius psilopterus"

#Psephenus
na.names5$order[grep("Psephenus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Psephenus", na.names5$query, ignore.case = TRUE)]<-"Psephenidae"
na.names5$genus[grep("Psephenus", na.names5$query, ignore.case = TRUE)]<-"Psephenus"
na.names5$accepted_name[grep("Psephenus", na.names5$query, ignore.case = TRUE)]<-"Psephenus"

#PSEUDESUCCINEA COLUMELLA
na.names5$order[grep("PSEUDESUCCINEA COLUMELLA", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Psychomiidae
na.names5$order[grep("Psychomiidae", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Psychomiidae", na.names5$query, ignore.case = TRUE)]<-"Psychomyiidae"
na.names5$accepted_name[grep("Psychomiidae", na.names5$query, ignore.case = TRUE)]<-"Psychomyiidae"

#	Pyrogophorus platyrachis
na.names5$order[grep("Pyrogophorus platyrachis", na.names5$query, ignore.case = TRUE)]<-"Mollusk"

#Quadrula
na.names5$order[grep("Quadrula", na.names5$query, ignore.case = TRUE)]<-"Mollusk"

#Quistradrilus
na.names5$order[grep("Quistradrilus", na.names5$query, ignore.case = TRUE)]<-"Oligochaete"

#Radotanypus
na.names5$order[grep("Radotanypus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Radotanypus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Radotanypus", na.names5$query, ignore.case = TRUE)]<-"Radotanypus"
na.names5$accepted_name[grep("Radotanypus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Reomyia	
na.names5$order[grep("Reomyia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Reomyia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Reomyia", na.names5$query, ignore.case = TRUE)]<-"Reomyia"
na.names5$accepted_name[grep("Reomyia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Rhabdomastix tricophora
na.names5$order[grep("Rhabdomastix tricophora", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Rhabdomastix tricophora", na.names5$query, ignore.case = TRUE)]<-"Tipulidae"
na.names5$genus[grep("Rhabdomastix tricophora", na.names5$query, ignore.case = TRUE)]<-"Rhabdomastix"
na.names5$species[grep("Rhabdomastix tricophora", na.names5$query, ignore.case = TRUE)]<-"Rhabdomastix trichophora"
na.names5$accepted_name[grep("Rhabdomastix tricophora", na.names5$query, ignore.case = TRUE)]<-"Rhabdomastix trichophora"

#Rheocricotopus unidentatus	
na.names5$order[grep("Rheocricotopus unidentatus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Rheocricotopus unidentatus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Rheocricotopus unidentatus", na.names5$query, ignore.case = TRUE)]<-"Rheocricotopus"
na.names5$species[grep("Rheocricotopus unidentatus", na.names5$query, ignore.case = TRUE)]<-"Rheocricotopus unidentatus"
na.names5$accepted_name[grep("Rheocricotopus unidentatus", na.names5$query, ignore.case = TRUE)]<-"Rheocricotopus"

#Rheosmittia arcuata
na.names5$order[grep("Rheosmittia arcuata", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Rheosmittia arcuata", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Rheosmittia arcuata", na.names5$query, ignore.case = TRUE)]<-"Rheosmittia"
na.names5$species[grep("Rheosmittia arcuata", na.names5$query, ignore.case = TRUE)]<-"Rheosmittia arcuata"
na.names5$accepted_name[grep("Rheosmittia arcuata", na.names5$query, ignore.case = TRUE)]<-"Rheosmittia"

#Rheotanytarsus distinctissimus
na.names5$order[grep("Rheotanytarsus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Rheotanytarsus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Rheotanytarsus", na.names5$query, ignore.case = TRUE)]<-"Rheotanytarsus"
na.names5$species[grep("Rheotanytarsus distinctissimus", na.names5$query, ignore.case = TRUE)]<-"Rheotanytarsus distinctissimus"
na.names5$accepted_name[grep("Rheotanytarsus distinctissimus", na.names5$query, ignore.case = TRUE)]<-"Rheotanytarsus distinctissimus"

#Rheotanytarsus exiguus 	
na.names5$species[grep("Rheotanytarsus exiguus", na.names5$query, ignore.case = TRUE)]<-"Rheotanytarsus exiguus"
na.names5$accepted_name[grep("Rheotanytarsus exiguus", na.names5$query, ignore.case = TRUE)]<-"Rheotanytarsus exiguus"

#Rhionaeshna californica
na.names5$order[grep("Rhionaeshna", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Rhionaeshna", na.names5$query, ignore.case = TRUE)]<-"Aeschnidae"
na.names5$genus[grep("Rhionaeshna", na.names5$query, ignore.case = TRUE)]<-"Rhionaeschna"
na.names5$species[grep("Rhionaeshna californica", na.names5$query, ignore.case = TRUE)]<-"Rhionaeschna californica"
na.names5$accepted_name[grep("Rhionaeshna californica", na.names5$query, ignore.case = TRUE)]<-"Rhionaeschna californica"

#Rhyacophila
na.names5$order[grep("Rhyacophila", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Rhyacophila", na.names5$query, ignore.case = TRUE)]<-"Rhyacophilidae"
na.names5$genus[grep("Rhyacophila", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila"
na.names5$accepted_name[grep("Rhyacophila", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila"

#Rhyacophila atrata
na.names5$species[grep("Rhyacophila atrata", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila atrata"
na.names5$accepted_name[grep("Rhyacophila atrata", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila atrata"

#Rhyacophila Ecosa
na.names5$species[grep("Rhyacophila Ecosa", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila ecosa"
na.names5$accepted_name[grep("Rhyacophila Ecosa", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila ecosa"

#Rhyacophila Iranda
na.names5$species[grep("Rhyacophila Iranda", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila iranda"
na.names5$accepted_name[grep("Rhyacophila Iranda", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila iranda"

#Rhyacophila siberica	
na.names5$species[grep("Rhyacophila siberica", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila sibirica"
na.names5$accepted_name[grep("Rhyacophila siberica", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila sibirica"

#Rhyacophila vata
na.names5$species[grep("Rhyacophila vata", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila vata"
na.names5$accepted_name[grep("Rhyacophila vata", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila"

#Rhyacophila vetina
na.names5$species[grep("Rhyacophila vetina", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila vetina"
na.names5$accepted_name[grep("Rhyacophila vetina", na.names5$query, ignore.case = TRUE)]<-"Rhyacophila vetina"

#Rissooidea
na.names5$order[grep("Rissooidea", na.names5$query, ignore.case = TRUE)]<-"Mollusk"

#Rithrogena
na.names5$order[grep("Rithrogena", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Rithrogena", na.names5$query, ignore.case = TRUE)]<-"Heptageniidae"
na.names5$genus[grep("Rithrogena", na.names5$query, ignore.case = TRUE)]<-"Rithrogena"
na.names5$accepted_name[grep("Rithrogena", na.names5$query, ignore.case = TRUE)]<-"Rithrogena"

#	Sanfillipodytes
na.names5$order[grep("Sanfillipodytes", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Sanfillipodytes", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$genus[grep("Sanfillipodytes", na.names5$query, ignore.case = TRUE)]<-"Sanfilippodytes"
na.names5$accepted_name[grep("Sanfillipodytes", na.names5$query, ignore.case = TRUE)]<-"Sanfilippodytes"

#	Sasquaperla hoopa
na.names5$order[grep("Sasquaperla hoopa", na.names5$query, ignore.case = TRUE)]<-"Plecoptera"
na.names5$family[grep("Sasquaperla hoopa", na.names5$query, ignore.case = TRUE)]<-"Chloroperlidae"
na.names5$genus[grep("Sasquaperla hoopa", na.names5$query, ignore.case = TRUE)]<-"Sasquaperla"
na.names5$species[grep("Sasquaperla hoopa", na.names5$query, ignore.case = TRUE)]<-"Sasquaperla hoopa"
na.names5$accepted_name[grep("Sasquaperla hoopa", na.names5$query, ignore.case = TRUE)]<-"Chloroperlidae"

#SCOLOPLOS
na.names5$order[grep("SCOLOPLOS", na.names5$query, ignore.case = TRUE)]<-"Annelida"

#Seratella
na.names5$order[grep("Seratella", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Seratella", na.names5$query, ignore.case = TRUE)]<-"Ephemerellidae"
na.names5$genus[grep("Seratella", na.names5$query, ignore.case = TRUE)]<-"Serratella"
na.names5$accepted_name[grep("Seratella", na.names5$query, ignore.case = TRUE)]<-"Serratella"

#	Serratella deficiens
na.names5$order[grep("Serratella deficiens", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Serratella deficiens", na.names5$query, ignore.case = TRUE)]<-"Ephemerellidae"
na.names5$genus[grep("Serratella deficiens", na.names5$query, ignore.case = TRUE)]<-"Teloganopsis"
na.names5$species[grep("Serratella deficiens", na.names5$query, ignore.case = TRUE)]<-"Teloganopsis deficiens"
na.names5$accepted_name[grep("Serratella deficiens", na.names5$query, ignore.case = TRUE)]<-"Teloganopsis deficiens"

#	Serratella velmae
na.names5$order[grep("Serratella velmae", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Serratella velmae", na.names5$query, ignore.case = TRUE)]<-"Ephemerellidae"
na.names5$genus[grep("Serratella velmae", na.names5$query, ignore.case = TRUE)]<-"Ephemerella"
na.names5$species[grep("Serratella velmae", na.names5$query, ignore.case = TRUE)]<-"Ephemerella velmae"
na.names5$accepted_name[grep("Serratella velmae", na.names5$query, ignore.case = TRUE)]<-"Ephemerella velmae"

#Simuliidae
na.names5$order[grep("Simuliidae", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Simuliidae", na.names5$query, ignore.case = TRUE)]<-"Simuliidae"
na.names5$accepted_name[grep("Simuliidae", na.names5$query, ignore.case = TRUE)]<-"Simuliidae"

#Simulium
na.names5$order[grep("Simulium", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Simulium", na.names5$query, ignore.case = TRUE)]<-"Simulidae"
na.names5$genus[grep("Simulium", na.names5$query, ignore.case = TRUE)]<-"Simulium"
na.names5$accepted_name[grep("Simulium", na.names5$query, ignore.case = TRUE)]<-"Simulium"

#	Simulium carbunculum
na.names5$species[grep("Simulium carbunculum", na.names5$query, ignore.case = TRUE)]<-"Simulium carbunculum"
na.names5$accepted_name[grep("Simulium carbunculum", na.names5$query, ignore.case = TRUE)]<-"Simulium"

#	Simulium clarum

na.names5$genus[grep("Simulium clarum", na.names5$query, ignore.case = TRUE)]<-"Simulium"
na.names5$accepted_name[grep("Simulium clarum", na.names5$query, ignore.case = TRUE)]<-"Simulium bivittatum"
na.names5$species[grep("Simulium clarum", na.names5$query, ignore.case = TRUE)]<-"Simulium bivittatum"

#Simulium donovani
na.names5$species[grep("Simulium donovani", na.names5$query, ignore.case = TRUE)]<-"Simulium donovani"

#Simulium hippovorum
na.names5$species[grep("Simulium hippovorum", na.names5$query, ignore.case = TRUE)]<-"Simulium hippovorum"

#Simulium modicum	
na.names5$species[grep("Simulium modicum", na.names5$query, ignore.case = TRUE)]<-"Simulium modicum"

#	Simulium tuberosum
na.names5$accepted_name[grep("Simulium tuberosum", na.names5$query, ignore.case = TRUE)]<-"Simulium tuberosum"
na.names5$species[grep("Simulium tuberosum", na.names5$query, ignore.case = TRUE)]<-"Simulium tuberosum"

#SIPUNCULA	
na.names5$order[grep("SIPUNCULA", na.names5$query, ignore.case = TRUE)]<-"worm"

#Sperchonidae
na.names5$order[grep("Sperchonidae", na.names5$query, ignore.case = TRUE)]<-"Arachnida"

#Sperchopsis tessellatus
na.names5$order[grep("Sperchopsis tessellatus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Sperchopsis tessellatus", na.names5$query, ignore.case = TRUE)]<-"Hydrophilidae"
na.names5$genus[grep("Sperchopsis tessellatus", na.names5$query, ignore.case = TRUE)]<-"Sperchopsis"
na.names5$species[grep("Sperchopsis tessellatus", na.names5$query, ignore.case = TRUE)]<-"Sperchopsis tessellata"
na.names5$accepted_name[grep("Sperchopsis tessellatus", na.names5$query, ignore.case = TRUE)]<-"Sperchopsis tessellata"

#Sphaeriidae
na.names5$order[grep("Sphaeriidae", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#IOCHAETOPTERUS OCULATUS
na.names5$order[grep("OCHAETOPTERUS OCULATUS", na.names5$query, ignore.case = TRUE)]<-"Polychaete"

#	STACTOBIELLA
na.names5$order[grep("STACTOBIELLA", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("STACTOBIELLA", na.names5$query, ignore.case = TRUE)]<-"Hydroptilidae"
na.names5$genus[grep("STACTOBIELLA", na.names5$query, ignore.case = TRUE)]<-"Stactobiella"
na.names5$accepted_name[grep("STACTOBIELLA", na.names5$query, ignore.case = TRUE)]<-"Stactobiella"

#Stempellina
na.names5$order[grep("Stempellina", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Stempellina", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Stempellina", na.names5$query, ignore.case = TRUE)]<-"Stempellina"
na.names5$accepted_name[grep("Stempellina", na.names5$query, ignore.case = TRUE)]<-"Stempellina"

#	Stempellinella
na.names5$order[grep("Stempellinella", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Stempellinella", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Stempellinella", na.names5$query, ignore.case = TRUE)]<-"Stempellinella"
na.names5$accepted_name[grep("Stempellinella", na.names5$query, ignore.case = TRUE)]<-"Stempellinella"

#Stempellinella cf leptocelloides
na.names5$species[grep("Stempellinella cf leptocelloides", na.names5$query, ignore.case = TRUE)]<-"Stempellinella leptocelloides"

#	Stempellinella fimbriata
na.names5$species[grep("Stempellinella fimbriata", na.names5$query, ignore.case = TRUE)]<-"Stempellinella fimbriata"

#Stenelmis
na.names5$order[grep("Stenelmis", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Stenelmis", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Stenelmis", na.names5$query, ignore.case = TRUE)]<-"Stenelmis"
na.names5$accepted_name[grep("Stenelmis", na.names5$query, ignore.case = TRUE)]<-"Stenelmis"

#Stenonema vicarium
na.names5$order[grep("Stenonema vicarium", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Stenonema vicarium", na.names5$query, ignore.case = TRUE)]<-"Heptageniidae"
na.names5$genus[grep("Stenonema vicarium", na.names5$query, ignore.case = TRUE)]<-"Maccaffertium"
na.names5$species[grep("Stenonema vicarium", na.names5$query, ignore.case = TRUE)]<-"Maccaffertium vicarium"
na.names5$accepted_name[grep("Stenonema vicarium", na.names5$query, ignore.case = TRUE)]<-"Maccaffertium vicarium"

#	Stenonema
na.names5$order[grep("Stenonema", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Stenonema", na.names5$query, ignore.case = TRUE)]<-"Heptageniidae"
na.names5$genus[grep("Stenonema", na.names5$query, ignore.case = TRUE)]<-"Stenonema"
na.names5$accepted_name[grep("Stenonema", na.names5$query, ignore.case = TRUE)]<-"Stenonema"

#Stictochironomus caffrarius 
na.names5$order[grep("Stictochironomus caffrarius", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Stictochironomus caffrarius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Stictochironomus caffrarius", na.names5$query, ignore.case = TRUE)]<-"Stictochironomus"
na.names5$species[grep("Stictochironomus caffrarius", na.names5$query, ignore.case = TRUE)]<-"Stictochironomus caffrarius"
na.names5$accepted_name[grep("Stictochironomus caffrarius", na.names5$query, ignore.case = TRUE)]<-"Stictochironomus"

#Stictocladius
na.names5$order[grep("Stictocladius", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Stictocladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Stictocladius", na.names5$query, ignore.case = TRUE)]<-"Stictocladius"
na.names5$accepted_name[grep("Stictocladius", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Stigmella
na.names5$order[grep("Stigmella", na.names5$query, ignore.case = TRUE)]<-"Lepidoptera"
na.names5$family[grep("Stigmella", na.names5$query, ignore.case = TRUE)]<-"Nepticulidae"
na.names5$genus[grep("Stigmella", na.names5$query, ignore.case = TRUE)]<-"Nepticula"
na.names5$accepted_name[grep("Stigmella", na.names5$query, ignore.case = TRUE)]<-"Nepticula"

#Strophitus undulatus undulatus
na.names5$order[grep("Strophitus undulatus undulatus", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Stygothrombium
na.names5$order[grep("Stygothrombium", na.names5$query, ignore.case = TRUE)]<-"water mite"

#Stylurus
na.names5$order[grep("Stylurus", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Stylurus", na.names5$query, ignore.case = TRUE)]<-"Gomphidae"
na.names5$genus[grep("Stylurus", na.names5$query, ignore.case = TRUE)]<-"Stylurus"
na.names5$accepted_name[grep("Stylurus", na.names5$query, ignore.case = TRUE)]<-"Stylurus"

#Suwalliini
na.names5$order[grep("Suwalliini", na.names5$query, ignore.case = TRUE)]<-"Plecoptera"
na.names5$family[grep("Suwalliini", na.names5$query, ignore.case = TRUE)]<-"Chloroperlidae"
na.names5$accepted_name[grep("Suwalliini", na.names5$query, ignore.case = TRUE)]<-"Chloroperlidae"

#Syrphidae
na.names5$order[grep("Syrphidae", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Syrphidae", na.names5$query, ignore.case = TRUE)]<-"Syrphidae"
na.names5$accepted_name[grep("Syrphidae", na.names5$query, ignore.case = TRUE)]<-"Syrphidae"

#Tanytarsini
na.names5$order[grep("Tanytarsini", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Tanytarsini", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$accepted_name[grep("Tanytarsini", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Tanytarsus
na.names5$order[grep("Tanytarsus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Tanytarsus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Tanytarsus", na.names5$query, ignore.case = TRUE)]<-"Tanytarsus"
na.names5$accepted_name[grep("Tanytarsus", na.names5$query, ignore.case = TRUE)]<-"Tanytarsus"

#TAPHROMYSIS BOWMANII
na.names5$order[grep("TAPHROMYSIS BOWMANII", na.names5$query, ignore.case = TRUE)]<-"Mysida"

#Telaganopsis
na.names5$order[grep("Telaganopsis", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Telaganopsis", na.names5$query, ignore.case = TRUE)]<-"Ephemerellidae"
na.names5$genus[grep("Telaganopsis", na.names5$query, ignore.case = TRUE)]<-"Teloganopsis"
na.names5$accepted_name[grep("Telaganopsis", na.names5$query, ignore.case = TRUE)]<-"Teloganopsis"

#Telmatopelopia
na.names5$order[grep("Telmatopelopia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Telmatopelopia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Telmatopelopia", na.names5$query, ignore.case = TRUE)]<-"Telmatopelopia"
na.names5$accepted_name[grep("Telmatopelopia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Tempisquitoneura merrillorum	
na.names5$order[grep("Tempisquitoneura merrillorum", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Tempisquitoneura merrillorum", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Tempisquitoneura merrillorum", na.names5$query, ignore.case = TRUE)]<-"Tempisquitoneura"
na.names5$species[grep("Tempisquitoneura merrillorum", na.names5$query, ignore.case = TRUE)]<-"Tempisquitoneura merrillorum"
na.names5$accepted_name[grep("Tempisquitoneura merrillorum", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Tetragoneuria
na.names5$order[grep("Tetragoneuria", na.names5$query, ignore.case = TRUE)]<-"Odonata"
na.names5$family[grep("Tetragoneuria", na.names5$query, ignore.case = TRUE)]<-"Corduliidae"
na.names5$genus[grep("Tetragoneuria", na.names5$query, ignore.case = TRUE)]<-"Epitheca"
na.names5$accepted_name[grep("Tetragoneuria", na.names5$query, ignore.case = TRUE)]<-"Epitheca"

#Thermonectus basilaris
na.names5$order[grep("Thermonectus basilaris", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Thermonectus basilaris", na.names5$query, ignore.case = TRUE)]<-"Dytiscidae"
na.names5$genus[grep("Thermonectus basilaris", na.names5$query, ignore.case = TRUE)]<-"Thermonectus"
na.names5$species[grep("Thermonectus basilaris", na.names5$query, ignore.case = TRUE)]<-"Thermonectus basilaris"
na.names5$accepted_name[grep("Thermonectus basilaris", na.names5$query, ignore.case = TRUE)]<-"Thermonectus basilaris"

#THIARA (MELANOIDES) TUBERCULATA
na.names5$order[grep("THIARA", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Thienemanniella
na.names5$order[grep("Thienemanniella", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Thienemanniella", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Thienemanniella", na.names5$query, ignore.case = TRUE)]<-"Thienemanniella"
na.names5$accepted_name[grep("Thienemanniella", na.names5$query, ignore.case = TRUE)]<-"Thienemanniella"

#Thienemannimyia
na.names5$order[grep("Thienemannimyia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Thienemannimyia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Thienemannimyia", na.names5$query, ignore.case = TRUE)]<-"Thienemannimyia"
na.names5$accepted_name[grep("Thienemannimyia", na.names5$query, ignore.case = TRUE)]<-"Thienemannimyia"

#Thoracica
na.names5$order[grep("Thoracica", na.names5$query, ignore.case = TRUE)]<-"Barnacle"

#Thremmatidae
na.names5$order[grep("Thremmatidae", na.names5$query, ignore.case = TRUE)]<-"Bivalve"

#Thyas
na.names5$order[grep("Thyas", na.names5$query, ignore.case = TRUE)]<-"Arachnida"

#Thyopsoides
na.names5$order[grep("Thyopsoides", na.names5$query, ignore.case = TRUE)]<-"Arachnida"

#Timpanoginae
na.names5$order[grep("Timpanoginae", na.names5$query, ignore.case = TRUE)]<-"Ephemeroptera"
na.names5$family[grep("Timpanoginae", na.names5$query, ignore.case = TRUE)]<-"Ephemerellidae"
na.names5$accepted_name[grep("Timpanoginae", na.names5$query, ignore.case = TRUE)]<-"Ephemerellidae"

#Tipula
na.names5$order[grep("Tipula", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Tipula", na.names5$query, ignore.case = TRUE)]<-"Tipulidae"
na.names5$genus[grep("Tipula", na.names5$query, ignore.case = TRUE)]<-"Tipula"
na.names5$accepted_name[grep("Tipula", na.names5$query, ignore.case = TRUE)]<-"Tipula"

#	Toxolasma parvus
na.names5$order[grep("Toxolasma parvus", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Triaenodes
na.names5$order[grep("Triaenodes", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Triaenodes", na.names5$query, ignore.case = TRUE)]<-"Leptoceridae"
na.names5$genus[grep("Triaenodes", na.names5$query, ignore.case = TRUE)]<-"Triaenodes"
na.names5$accepted_name[grep("Triaenodes", na.names5$query, ignore.case = TRUE)]<-"Triaenodes"

#Tribelos fuscic
na.names5$order[grep("Tribelos fuscic", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Tribelos fuscic", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Tribelos fuscic", na.names5$query, ignore.case = TRUE)]<-"Tribelos"
na.names5$species[grep("Tribelos fuscic", na.names5$query, ignore.case = TRUE)]<-"Tribelos fuscic"
na.names5$accepted_name[grep("Tribelos fuscic", na.names5$query, ignore.case = TRUE)]<-"Tribelos fuscic"

#Tribelos jucundum
na.names5$order[grep("Tribelos jucundum", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Tribelos jucundum", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Tribelos jucundum", na.names5$query, ignore.case = TRUE)]<-"Tribelos"
na.names5$species[grep("Tribelos jucundum", na.names5$query, ignore.case = TRUE)]<-"Tribelos jucundus"
na.names5$accepted_name[grep("Tribelos jucundum", na.names5$query, ignore.case = TRUE)]<-"Tribelos jucundus"

#Tropisternus
na.names5$order[grep("Tropisternus", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Tropisternus", na.names5$query, ignore.case = TRUE)]<-"Hydrophilidae"
na.names5$genus[grep("Tropisternus", na.names5$query, ignore.case = TRUE)]<-"Tropisternus"
na.names5$accepted_name[grep("Tropisternus", na.names5$query, ignore.case = TRUE)]<-"Tropisternus"

#Truncatelloidea
na.names5$order[grep("Truncatelloidea", na.names5$query, ignore.case = TRUE)]<-"Mollusca"

#Tvetenia discoloripes
na.names5$order[grep("Tvetenia discoloripes", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Tvetenia discoloripes", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Tvetenia discoloripes", na.names5$query, ignore.case = TRUE)]<-"Eukiefferiella"
na.names5$species[grep("Tvetenia discoloripes", na.names5$query, ignore.case = TRUE)]<-"Eukiefferiella discoloripes"
na.names5$accepted_name[grep("Tvetenia discoloripes", na.names5$query, ignore.case = TRUE)]<-"Eukiefferiella discoloripes"

#Tvetenia tshernovskii
na.names5$order[grep("Tvetenia tshernovskii", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Tvetenia tshernovskii", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Tvetenia tshernovskii", na.names5$query, ignore.case = TRUE)]<-"Tvetenia"
na.names5$species[grep("Tvetenia tshernovskii", na.names5$query, ignore.case = TRUE)]<-"Tvetenia vitracies"
na.names5$accepted_name[grep("Tvetenia tshernovskii", na.names5$query, ignore.case = TRUE)]<-"Tvetenia vitracies"

#Virgatanytarsus	
na.names5$order[grep("Virgatanytarsus", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Virgatanytarsus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Virgatanytarsus", na.names5$query, ignore.case = TRUE)]<-"Virgatanytarsus"
na.names5$accepted_name[grep("Virgatanytarsus", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"

#Zaitzevia milleri
na.names5$order[grep("Zaitzevia milleri", na.names5$query, ignore.case = TRUE)]<-"Coleoptera"
na.names5$family[grep("Zaitzevia milleri", na.names5$query, ignore.case = TRUE)]<-"Elmidae"
na.names5$genus[grep("Zaitzevia milleri", na.names5$query, ignore.case = TRUE)]<-"Zaitzevia"
na.names5$species[grep("Zaitzevia milleri", na.names5$query, ignore.case = TRUE)]<-"Zaitzevia posthonia"
na.names5$accepted_name[grep("Zaitzevia milleri", na.names5$query, ignore.case = TRUE)]<-"Zaitzevia posthonia"

#Zaitzeva
na.names5$order[grep("Zaitzeva", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Zaitzeva", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Zaitzeva", na.names5$query, ignore.case = TRUE)]<-"Zaitzevia"
na.names5$accepted_name[grep("Zaitzeva", na.names5$query, ignore.case = TRUE)]<-"Zaitzevia"

#	Zavrelimyia
na.names5$order[grep("Zavrelimyia", na.names5$query, ignore.case = TRUE)]<-"Diptera"
na.names5$family[grep("Zavrelimyia", na.names5$query, ignore.case = TRUE)]<-"Chironomidae"
na.names5$genus[grep("Zavrelimyia", na.names5$query, ignore.case = TRUE)]<-"Zavrelimyia"
na.names5$accepted_name[grep("Zavrelimyia", na.names5$query, ignore.case = TRUE)]<-"Zavrelimyia"

#Zavrelimyia thryptica complex
na.names5$species[grep("Zavrelimyia thryptica complex", na.names5$query, ignore.case = TRUE)]<-"Zavrelimyia thryptica"
na.names5$accepted_name[grep("Zavrelimyia thryptica complex", na.names5$query, ignore.case = TRUE)]<-"Zavrelimyia thryptica"

#Zumatrichia notosa	
na.names5$order[grep("Zumatrichia notosa", na.names5$query, ignore.case = TRUE)]<-"Trichoptera"
na.names5$family[grep("Zumatrichia notosa", na.names5$query, ignore.case = TRUE)]<-"Hydroptilidae"
na.names5$genus[grep("Zumatrichia notosa", na.names5$query, ignore.case = TRUE)]<-"Leucotrichia"
na.names5$species[grep("Zumatrichia notosa", na.names5$query, ignore.case = TRUE)]<-"Leucotrichia notosa"
na.names5$accepted_name[grep("Zumatrichia notosa", na.names5$query, ignore.case = TRUE)]<-"Leucotrichia notosa"

###################### Get accepted_name and tsn for all taxa in na.names5 ################################
#####create a list of unique taxa and get records using 'taxize' library
#add back into valid_names5 
valid_names6<-merge(valid_names5, na.names5, by.x="query", by.y="query",all=TRUE)
#combine columns
valid_names6$Order<-if_else(is.na(valid_names6$order.y),valid_names6$order.x, valid_names6$order.y)
valid_names6$Family<-if_else(is.na(valid_names6$family.y),valid_names6$family.x, valid_names6$family.y)
valid_names6$Genus<-if_else(is.na(valid_names6$genus.y),valid_names6$genus.x, valid_names6$genus.y)
valid_names6$Species<-if_else(is.na(valid_names6$species.y),valid_names6$species.x, valid_names6$species.y)

#Create new Submitted_name without strange characters
#initialize column
valid_names6$Submitted_name<-NA
valid_names6$Submitted_name <- if_else(is.na(valid_names6$accepted_name),valid_names6$Species, valid_names6$accepted_name)
valid_names6$Submitted_name <- if_else(is.na(valid_names6$Submitted_name),valid_names6$Genus, valid_names6$Submitted_name)
valid_names6$Submitted_name <- if_else(is.na(valid_names6$Submitted_name),valid_names6$Family, valid_names6$Submitted_name)
valid_names6$Submitted_name <- if_else(is.na(valid_names6$Submitted_name),valid_names6$Order, valid_names6$Submitted_name)

#drop entries with NA in submitted name and non-insect orders
na.names6<-subset(valid_names6, !is.na(valid_names6$Submitted_name))
na.names7<-subset(na.names6, Order=="Diptera" | Order=="Coleoptera" | Order=="Megaloptera" | Order=="Lepidoptera" | Order=="Ephemeroptera" | Order=="Plecoptera"| Order=="Hemiptera"| Order=="Trichoptera"| Order=="Odonata" | Order=="Neuroptera")
utaxon.5<-unique(na.names7$Submitted_name)

#get tsn and accepted name for taxa with higher and lower valid names
library(taxize)
valid_tsn5<-get_tsn(utaxon.5, searchtype="scientific", accepted=FALSE, ask=TRUE)
utaxon.6<-as.data.frame(cbind(utaxon.5, as.character(valid_tsn5)), stringsAsFactors = FALSE) #619 taxa to which I manually assigned names and their TSN from ITIS, for most the Submitted name is the Accepted name
#write file to csv
write.csv(utaxon.6, "~/Documents/WaterCube/Ch.3/aquatic_insects/utaxon.6.csv")
#read file
utaxon.6<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/utaxon.6.csv", stringsAsFactors = FALSE)

#assign TSN to entries in utaxon.6 that still have NA in TSN
utaxon.6[which(is.na(utaxon.6$V2)),] 
utaxon.6$V2[which(utaxon.5=="Bethbilbeckia floridensis")]<-NA
utaxon.6$V2[which(utaxon.5=="Ceraptogonidae")]<-127076
utaxon.6$accepted_name<-NA
utaxon.6$accepted_name[which(utaxon.5=="Ceraptogonidae")]<-"Ceraptopogonidae"
utaxon.6$accepted_name[which(utaxon.5=="Climacea areolarsis")]<-"Climacia areolaris"
utaxon.6$V2[which(utaxon.5=="Climacea areolarsis")]<-115087
utaxon.6$accepted_name[which(utaxon.5=="Coryoneura")]<-"Corynoneura"
utaxon.6$V2[which(utaxon.5=="Coryoneura")]<-128563
utaxon.6$accepted_name[which(utaxon.5=="Dashyhelea")]<-"Dasyhelea"
utaxon.6$V2[which(utaxon.5=="Dashyhelea")]<-127278
utaxon.6$V2[which(utaxon.5=="Lara")]<-114137
utaxon.6$accepted_name[which(utaxon.5=="Thermonectus basilaris")]<-"Thermonectus basillaris"
utaxon.6$V2[which(utaxon.5=="Thermonectus basilaris")]<-112113
utaxon.6$accepted_name[which(utaxon.5=="Ellipteroides")]<-"Ellipteroides"
utaxon.6$accepted_name[which(utaxon.5=="Hydrosmittia")]<-"Pseudosmittia"
utaxon.6$V2[which(utaxon.5=="Hydrosmittia")]<-129071
utaxon.6$accepted_name[which(utaxon.5=="Rithrogena")]<-"Rhithrogena"
utaxon.6$V2[which(utaxon.5=="Rithrogena")]<-100572
#several taxa with NA in TSN cannot be verified through ITIS, will remain with NA

#merge utaxon.6 and na.names6- keep all columns
na.names8<-merge(na.names6, utaxon.6, by.x="Submitted_name", by.y="utaxon.5", all=TRUE) #691 taxa + manually assigned TSNs

#combine accepted name columns
na.names8$accepted_name<-if_else(is.na(na.names8$accepted_name.y), na.names8$accepted_name.x, na.names8$accepted_name.y)
#drop unneeded columns that resulted from merge
na.names9<-select(na.names8, c("Submitted_name", "query", "Order", "Family", "Genus", "Species", "V2", "accepted_name"))

#check remaining accepted names using TSN
tsn<-na.names9$V2[which(!is.na(na.names9$V2))]
accepted_names9<-itis_acceptname(searchtsn = tsn)

#merge with na.names9
na.names10<-merge(na.names9, unique(accepted_names9), by.x="V2", by.y="submittedtsn", all=TRUE)

#combine accepted_name and TSN columns
na.names10$Accepted_name<-if_else(is.na(na.names10$acceptedname), na.names10$accepted_name, na.names10$acceptedname)
na.names10$Accepted_TSN<-if_else(is.na(na.names10$acceptedtsn), na.names10$V2, na.names10$acceptedtsn)

#drop old accepted name columns
na.names11<-select(na.names10, -c(V2, accepted_name,acceptedname, author))

#correct mismatches between species and accepted_name columns- some accepted_names give genus when species has an accepted_name according to ITIS
mismatch<-na.names11[which(na.names11$Accepted_name !=na.names11$Species),]
#search tsn and accepted_name for species in na.names12
accepted_names10<-get_tsn(mismatch$Species, searchtype="scientific", accepted=FALSE, ask=TRUE)
mismatch1<-as.data.frame(cbind(mismatch, as.character(accepted_names10)), stringsAsFactors = FALSE)
#now get real accepted name using tsn
mismatch_tsn<-na.omit(mismatch1$`as.character(accepted_names10)`)
mismatch_accepted_names<-itis_acceptname(searchtsn = mismatch_tsn)
#merge mismatch1 and mismatch_accepted_names
new_names<-as.data.frame(merge(mismatch1, mismatch_accepted_names, by.x="as.character(accepted_names10)", by.y="submittedtsn", all=TRUE), stringsAsFactors = FALSE)
#combine species column with acceptedname- any NA in acceptedname is the species name
new_names$acceptedname<-if_else(is.na(new_names$acceptedname), new_names$Species, new_names$acceptedname)

#drop unneeded columns, take acceptedname as Accepted_name, acceptedtsn.y as Accepted_TSN, query from na.names12 is original Submitted_name 
new_names1<-unique(select(new_names, c(Accepted_TSN, acceptedname, acceptedtsn.y, Species)))

na.names12<-as.data.frame(merge(na.names11, new_names1, by=intersect(names(na.names11), names(new_names1)), all=TRUE), stringsAsFactors = FALSE)

#drop unneeded columns from merge
na.names13<-select(na.names12, Species, query, Order, Family, Genus, acceptedtsn, Accepted_name, acceptedname, acceptedtsn.y)
#combine accepted names and accepted tsns
na.names13$Accepted_TSN<-if_else(is.na(na.names13$acceptedtsn.y), na.names13$acceptedtsn, as.character(na.names13$acceptedtsn.y))
na.names13$Accepted_name_new<-if_else(is.na(na.names13$acceptedname), na.names13$Accepted_name, na.names13$acceptedname)
na.names14<-select(na.names13, Species, query, Order, Family, Genus, Accepted_TSN, Accepted_name_new)

#assign Genus or Family as Accepted_name for taxa not ID'd to Species
na.names14$Accepted_name_new<-if_else(is.na(na.names14$Accepted_name_new), na.names14$Species, na.names14$Accepted_name_new)
na.names14$Accepted_name_new<-if_else(is.na(na.names14$Accepted_name_new), na.names14$Genus, na.names14$Accepted_name_new)
na.names14$Accepted_name_new<-if_else(is.na(na.names14$Accepted_name_new), na.names14$Family, na.names14$Accepted_name_new)
na.names14$Accepted_name_new<-if_else(is.na(na.names14$Accepted_name_new), na.names14$Order, na.names14$Accepted_name_new)

#get upstream names using Accepted_name
itis_hierarchy<-tax_name(query=unique(na.names14$Accepted_name), get=c("order","family", "genus", "species"), db="itis")
#save file
write.csv(itis_hierarchy, "~/Documents/WaterCube/Ch.3/aquatic_insects/itis_hierarchy.csv")

na.names15<-merge(na.names14, itis_hierarchy, by.x="Accepted_name_new", by.y="query", all=TRUE)

#merge with manually created hierarchy for taxa ID'd to genus or species, but without accepted genus or species designation in ITIS
na.names15$Order<-if_else(is.na(na.names15$order), na.names15$Order, na.names15$order)
na.names15$Family<-if_else(is.na(na.names15$family), na.names15$Family, na.names15$family)
na.names15$Genus<-if_else(is.na(na.names15$genus), na.names15$Genus, na.names15$genus)
na.names15$Species<-if_else(is.na(na.names15$species), na.names15$Species, na.names15$species)
na.names16<-select(na.names15, -c(species, genus, family, order, db))

############ Merge na.names and occurrence9 for corrected name list ####################################################################
#merging rule: if Submitted_name in occurrence9 contains part of entry in 'query' in na.names5 then merge the records
#remove all punctuation in "query" and "Submitted_names"
na.names16$query<-gsub("[[:punct:]]", "", na.names16$query)
occurrence9$Submitted_name<-gsub("[[:punct:]]", "", occurrence9$Submitted_name)
#remove other extra characters from Submitted_name in occurrence9
occurrence9$Submitted_name<-gsub("Group|group|gr.|Gr.|sp.|Type 1|SP|SP.|sp|Gr|gr", "", occurrence9$Submitted_name, fixed=FALSE)

#remove extra spaces between words
occurrence9$Submitted_name<-gsub("  ", " ", occurrence9$Submitted_name, fixed=TRUE)

#remove trailing spaces
occurrence9$Submitted_name<-trimws(occurrence9$Submitted_name, which = c("both", "left", "right"))

#remove extra period
occurrence9$Submitted_name<-gsub("[.]", "", occurrence9$Submitted_name, fixed=FALSE)

#remove entries with a slash in it
occurrence9$Submitted_name<-grep(paste("/"), occurrence9$Submitted_name, value=TRUE,invert=TRUE)
na.names17<-unique(na.names16)

#initialize species column in occurrence records
occurrence9$Species<-NA

#merge na.names17 with occurrence data 
occurrence10<-merge(occurrence9, na.names17, by.x="Submitted_name", by.y="query", all.x=TRUE)
#combine Order, Family, Genus, Species columns
occurrence10$Order<-if_else(is.na(occurrence10$Order.x), occurrence10$Order.y, occurrence10$Order.x)
occurrence10$Family<-if_else(is.na(occurrence10$Family.x), occurrence10$Family.y, occurrence10$Family.x)
occurrence10$Genus<-if_else(is.na(occurrence10$Genus.x), occurrence10$Genus.y, occurrence10$Genus.x)
occurrence10$Accepted_TSN<-if_else(is.na(occurrence10$Accepted_TSN.x), occurrence10$Accepted_TSN.y, as.character(occurrence10$Accepted_TSN.x))
occurrence10$Species<-if_else(is.na(occurrence10$Species.x), as.character(occurrence10$Species.y), as.character(occurrence10$Species.x))

#return to accepted name and species after second merge 
occurrence10$Accepted_name<-if_else(is.na(occurrence10$Accepted_name), occurrence10$Accepted_name_new, occurrence10$Accepted_name)
occurrence11<-select(occurrence10, "Submitted_name", "Taxonomic_resolution","Accepted_name", "Location_ID","Sample_ID", "Date","Latitude" ,"Longitude","Provider_name","Sample_method","Location_description" ,"HorizontalCoordinateReferenceSystemDatumName", "Monitoring_organization","Date1", "Order" ,"Family", "Genus","Species","Accepted_TSN" )

#now merge with names_tsn
#all names are accepted_names per previous search 
names_tsn$query<-gsub("[[:punct:]]", "", names_tsn$query)
names_tsn$query<-gsub("Group|group|gr.|Gr.|sp.|Type 1|SP|SP.|sp|Gr|gr", "", names_tsn$query, fixed=FALSE)

#remove extra spaces between words
names_tsn$query<-gsub("  ", " ", names_tsn$query, fixed=TRUE)

#remove trailing spaces
names_tsn$query<-trimws(names_tsn$query, which = c("both", "left", "right"))

#remove extra period
names_tsn$query<-gsub("[.]", "", names_tsn$query, fixed=FALSE)

#remove entries with a slash in it
names_tsn$query<-grep(paste("/"), names_tsn$query, value=TRUE,invert=TRUE)

occurrence12<-merge(occurrence11, names_tsn, by.x="Submitted_name", by.y="query", all.x=TRUE)

#combine columns from merge
occurrence12$Order<-if_else(is.na(occurrence12$Order), occurrence12$order, occurrence12$Order)
occurrence12$Family<-if_else(is.na(occurrence12$Family), occurrence12$family, occurrence12$Family)
occurrence12$Genus<-if_else(is.na(occurrence12$Genus), occurrence12$genus, occurrence12$Genus)
occurrence12$Species<-if_else(is.na(occurrence12$Species), occurrence12$species, occurrence12$Species)
occurrence12$Accepted_TSN<-if_else(is.na(occurrence12$Accepted_TSN), as.character(occurrence12$valid_tsn), as.character(occurrence12$Accepted_TSN))

#drop unneeded columns from merge
occurrence12<-select(occurrence12, c("Submitted_name", "Taxonomic_resolution","Accepted_name", "Location_ID", "Sample_ID", "Date", "Latitude", "Longitude","Provider_name","Sample_method","Location_description","HorizontalCoordinateReferenceSystemDatumName","Monitoring_organization","Date1","Order","Family","Genus","Species","Accepted_TSN"))

#how many NAs in Order remain
na.order<-subset(occurrence12, is.na(Order))
na.orders<-unique(na.order$Submitted_name)

#run ITIS search on remaining taxa
valid_names_new<-tax_name(query=na.orders, get=c("order","family", "genus", "species"), db="itis") #ran ITIS search again on 971 taxa that have an NA in Order- search worked for some taxa but still failed for many
#write to file
write.csv(valid_names_new, "~/Documents/WaterCube/Ch.3/aquatic_insects/valid_names_new.csv")

valid_names_na<-subset(valid_names_new, is.na(order))
  
#manually recheck remaining names
#	Ablabesmyia karelia
valid_names_na$order[grep("Ablabesmyia karelia", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Ablabesmyia karelia", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Ablabesmyia karelia", valid_names_na$query, ignore.case = TRUE)]<-"Ablabesmyia"
valid_names_na$accepted_name<-NA
valid_names_na$accepted_name[which(valid_names_na$query == "Ablabesmyia karelia")]<-"Ablabesmyia" 

#Acari
valid_names_na$order[grep("Acari", valid_names_na$query, ignore.case = TRUE)]<-"mite"

#Acariformes
valid_names_na$order[grep("Acariformes", valid_names_na$query, ignore.case = TRUE)]<-"mite"

#	AcentrellaPlauditus
valid_names_na$order[grep("AcentrellaPlauditus", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("AcentrellaPlauditus", valid_names_na$query, ignore.case = TRUE)]<-"Baetidae"
valid_names_na$genus[grep("AcentrellaPlauditus", valid_names_na$query, ignore.case = TRUE)]<-NA
valid_names_na$accepted_name[grep("AcentrellaPlauditus", valid_names_na$query, ignore.case = TRUE)]<-"Baetidae"

#Acroneuria arenosaevoluta
valid_names_na$order[grep("Acroneuria arenosaevoluta", valid_names_na$query, ignore.case = TRUE)]<-"Plecoptera"
valid_names_na$family[grep("Acroneuria arenosaevoluta", valid_names_na$query, ignore.case = TRUE)]<-"Perlidae"
valid_names_na$genus[grep("Acroneuria arenosaevoluta", valid_names_na$query, ignore.case = TRUE)]<-"Acroneuria"
valid_names_na$species[grep("Acroneuria arenosaevoluta", valid_names_na$query, ignore.case = TRUE)]<-NA
valid_names_na$accepted_name[grep("Acroneuria arenosaevoluta", valid_names_na$query, ignore.case = TRUE)]<-"Acroneuria"

#Aeshna
valid_names_na$order[grep("Aeshna", valid_names_na$query, ignore.case = TRUE)]<-"Odonata"
valid_names_na$family[grep("Aeshna", valid_names_na$query, ignore.case = TRUE)]<-"Aeshnidae"
valid_names_na$accepted_name[grep("Aeshna", valid_names_na$query, ignore.case = TRUE)]<-"Aeshnidae"

#Archtopsyche
valid_names_na$order[grep("Archtopsyche", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Archtopsyche", valid_names_na$query, ignore.case = TRUE)]<-"Hydropsychidae"
valid_names_na$accepted_name[grep("Archtopsyche", valid_names_na$query, ignore.case = TRUE)]<-"Hydropsychidae"

#AgabusIlybius
valid_names_na$order[grep("Agabus", valid_names_na$query, ignore.case = TRUE)]<-"Coleoptera"
valid_names_na$family[grep("Agabus", valid_names_na$query, ignore.case = TRUE)]<-"Dytiscidae"
valid_names_na$genus[grep("Agabus", valid_names_na$query, ignore.case = TRUE)]<-NA
valid_names_na$accepted_name[grep("Agabus", valid_names_na$query, ignore.case = TRUE)]<-"Dytiscidae"

#Agnetina
valid_names_na$order[grep("Agnetina", valid_names_na$query, ignore.case = TRUE)]<-"Plecoptera"
valid_names_na$family[grep("Agnetina", valid_names_na$query, ignore.case = TRUE)]<-"Perlidae"
valid_names_na$accepted_name[grep("Agnetina", valid_names_na$query, ignore.case = TRUE)]<-"Perlidae"

#Allocladius
valid_names_na$order[grep("Allocladius", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Allocladius", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Allocladius", valid_names_na$query, ignore.case = TRUE)]<-"Allocladius"
valid_names_na$accepted_name[grep("Allocladius", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#	Anafroptilum
valid_names_na$order[grep("Anafroptilum", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("Anafroptilum", valid_names_na$query, ignore.case = TRUE)]<-"Baetidae"
valid_names_na$genus[grep("Anafroptilum", valid_names_na$query, ignore.case = TRUE)]<-"Anafroptilum"
valid_names_na$accepted_name[grep("Anafroptilum", valid_names_na$query, ignore.case = TRUE)]<-"Baetidae"

#Anopholes
valid_names_na$order[grep("Anopholes", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Anopholes", valid_names_na$query, ignore.case = TRUE)]<-"Culicidae"
valid_names_na$genus[grep("Anopholes", valid_names_na$query, ignore.case = TRUE)]<-"Anopheles"
valid_names_na$accepted_name[grep("Anopholes", valid_names_na$query, ignore.case = TRUE)]<-"Anopheles"

#Arctopsyche
valid_names_na$order[grep("Arctopsyche", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Arctopsyche", valid_names_na$query, ignore.case = TRUE)]<-"Hydropsychidae"
valid_names_na$accepted_name[grep("Arctopsyche", valid_names_na$query, ignore.case = TRUE)]<-"Hydropsychidae"

#ATRICHOPOGON UNIDA
valid_names_na$order[grep("ATRICHOPOGON UNID A", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("ATRICHOPOGON UNID A", valid_names_na$query, ignore.case = TRUE)]<-"Ceratopogonidae"
valid_names_na$genus[grep("ATRICHOPOGON UNID A", valid_names_na$query, ignore.case = TRUE)]<-"Atrichopogon"
valid_names_na$accepted_name[grep("ATRICHOPOGON UNID A", valid_names_na$query, ignore.case = TRUE)]<-"Atrichopogon"

#Atylotus
valid_names_na$order[grep("Atylotus", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Atylotus", valid_names_na$query, ignore.case = TRUE)]<-"Hydropsychidae"
valid_names_na$accepted_name[grep("Atylotus", valid_names_na$query, ignore.case = TRUE)]<-"Hydropsychidae"

#Baetis aliusmoqui
valid_names_na$order[grep("Baetis aliusmoqui", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("Baetis aliusmoqui", valid_names_na$query, ignore.case = TRUE)]<-"Baetidae"
valid_names_na$genus[grep("Baetis aliusmoqui", valid_names_na$query, ignore.case = TRUE)]<-"Baetis"
valid_names_na$accepted_name[grep("Baetis aliusmoqui", valid_names_na$query, ignore.case = TRUE)]<-"Baetis"

#Bezzia
valid_names_na$order[grep("Bezzia", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Bezzia", valid_names_na$query, ignore.case = TRUE)]<-"Ceratopogonidae"
valid_names_na$accepted_name[grep("Bezzia", valid_names_na$query, ignore.case = TRUE)]<-"Ceratopogonidae"

#Caloparyphus
valid_names_na$order[grep("Caloparyphus", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Caloparyphus", valid_names_na$query, ignore.case = TRUE)]<-"Stratiomyidae"
valid_names_na$accepted_name[grep("Caloparyphus", valid_names_na$query, ignore.case = TRUE)]<-"Stratiomyidae"

#	Canacidae
valid_names_na$order[grep("Canacidae", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Canacidae", valid_names_na$query, ignore.case = TRUE)]<-"Canacidae"
valid_names_na$genus[grep("Canacidae", valid_names_na$query, ignore.case = TRUE)]<-NA
valid_names_na$accepted_name[grep("Canacidae", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"

#Centroptilum
valid_names_na$order[grep("Centroptilum", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("Centroptilum", valid_names_na$query, ignore.case = TRUE)]<-"Baetidae"
valid_names_na$accepted_name[grep("Centroptilum", valid_names_na$query, ignore.case = TRUE)]<-"Baetidae"

#Cernotina
valid_names_na$order[grep("Cernotina", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Cernotina", valid_names_na$query, ignore.case = TRUE)]<-"Polycentropodidae"
valid_names_na$accepted_name[grep("Cernotina", valid_names_na$query, ignore.case = TRUE)]<-"Polycentropodidae"

#Conchapelopia
valid_names_na$order[grep("Conchapelopia", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Conchapelopia", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Conchapelopia", valid_names_na$query, ignore.case = TRUE)]<-"Conchapelopia"
valid_names_na$accepted_name[grep("Conchapelopia", valid_names_na$query, ignore.case = TRUE)]<-"Conchapelopia"

#	Cricotopus reversus epler
valid_names_na$order[grep("Cricotopus reversus epler", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Cricotopus reversus epler", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Cricotopus reversus epler", valid_names_na$query, ignore.case = TRUE)]<-"Cricotopus"
valid_names_na$species[grep("Cricotopus reversus epler", valid_names_na$query, ignore.case = TRUE)]<-"Cricotopus reversus"
valid_names_na$accepted_name[grep("Cricotopus reversus epler", valid_names_na$query, ignore.case = TRUE)]<-"Cricotopus reversus"

#Cricotopus silvestris
valid_names_na$order[grep("Cricotopus silvestris", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Cricotopus silvestris", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Cricotopus silvestris", valid_names_na$query, ignore.case = TRUE)]<-"Cricotopus"
valid_names_na$species[grep("Cricotopus silvestris", valid_names_na$query, ignore.case = TRUE)]<-"Cricotopus sylvestris"
valid_names_na$accepted_name[grep("Cricotopus silvestris", valid_names_na$query, ignore.case = TRUE)]<-"Cricotopus sylvestris"

#Orthocladius
valid_names_na$order[grep("Orthocladius", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Orthocladius", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Orthocladius", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Demicryptotendipes
valid_names_na$order[grep("Demicryptotendipes", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Demicryptotendipes", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Demicryptotendipes", valid_names_na$query, ignore.case = TRUE)]<-"Demicryptotendipes"
valid_names_na$accepted_name[grep("Demicryptotendipes", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#	Diptera P
valid_names_na$order[grep("Diptera P", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$accepted_name[grep("Diptera P", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"

#Djalmabatista
valid_names_na$order[grep("Djalmabatista", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Djalmabatista", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Djalmabatista", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Dolophilodes
valid_names_na$order[grep("Dolophilodes", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Dolophilodes", valid_names_na$query, ignore.case = TRUE)]<-"Philopotamidae"
valid_names_na$genus[grep("Dolophilodes", valid_names_na$query, ignore.case = TRUE)]<-"Dolophilodes"
valid_names_na$accepted_name[grep("Dolophilodes", valid_names_na$query, ignore.case = TRUE)]<-"Dolophilodes"

#Drunella coloradensisflavilinea
valid_names_na$order[grep("Drunella", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("Drunella", valid_names_na$query, ignore.case = TRUE)]<-"Ephemerellidae"
valid_names_na$genus[grep("Drunella", valid_names_na$query, ignore.case = TRUE)]<-"Drunella"
valid_names_na$accepted_name[grep("Drunella", valid_names_na$query, ignore.case = TRUE)]<-"Drunella"

#Dytiscidae
valid_names_na$order[grep("Dytiscidae", valid_names_na$query, ignore.case = TRUE)]<-"Coleoptera"
valid_names_na$family[grep("Dytiscidae", valid_names_na$query, ignore.case = TRUE)]<-"Dytiscidae"
valid_names_na$accepted_name[grep("Dytiscidae", valid_names_na$query, ignore.case = TRUE)]<-"Dytiscidae"

#Empididae
valid_names_na$order[grep("Empididae", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Empididae", valid_names_na$query, ignore.case = TRUE)]<-"Empididae"
valid_names_na$accepted_name[grep("Empididae", valid_names_na$query, ignore.case = TRUE)]<-"Empididae"

#EnallagmaIschnura
valid_names_na$order[grep("EnallagmaIschnura", valid_names_na$query, ignore.case = TRUE)]<-"Odonata"
valid_names_na$family[grep("EnallagmaIschnura", valid_names_na$query, ignore.case = TRUE)]<-"Coenagrionidae"
valid_names_na$accepted_name[grep("EnallagmaIschnura", valid_names_na$query, ignore.case = TRUE)]<-"Coenagrionidae"

#Endochironomus
valid_names_na$order[grep("Endochironomus", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Endochironomus", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Endochironomus", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#	Epeorus deceptivusherus
valid_names_na$order[grep("Epeorus", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("Epeorus", valid_names_na$query, ignore.case = TRUE)]<-"Heptageniidae"
valid_names_na$genus[grep("Epeorus", valid_names_na$query, ignore.case = TRUE)]<-"Epeorus"
valid_names_na$species[grep("Epeorus deceptivusherus", valid_names_na$query, ignore.case = TRUE)]<-"Epeorus deceptivus"
valid_names_na$accepted_name[grep("Epeorus deceptivusherus", valid_names_na$query, ignore.case = TRUE)]<-"Epeorus deceptivus"

#Epeorus ndirmagnus
valid_names_na$accepted_name[grep("Epeorus ndirmagnus", valid_names_na$query, ignore.case = TRUE)]<-"Epeorus"

#EphemerellaSerratella
valid_names_na$order[grep("EphemerellaSerratella", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("EphemerellaSerratella", valid_names_na$query, ignore.case = TRUE)]<-"Ephemerellidae"
valid_names_na$genus[grep("EphemerellaSerratella", valid_names_na$query, ignore.case = TRUE)]<-"Serratella"
valid_names_na$accepted_name[grep("EphemerellaSerratella", valid_names_na$query, ignore.case = TRUE)]<-"Serratella"

#Ephemerella dorotheaexcrucians
valid_names_na$order[grep("Ephemerella dorotheaexcrucians", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("Ephemerella dorotheaexcrucians", valid_names_na$query, ignore.case = TRUE)]<-"Ephemerellidae"
valid_names_na$genus[grep("Ephemerella dorotheaexcrucians", valid_names_na$query, ignore.case = TRUE)]<-"Ephemerella"
valid_names_na$accepted_name[grep("Ephemerella dorotheaexcrucians", valid_names_na$query, ignore.case = TRUE)]<-"Ephemerella"

#Ephemerella inermisinfrequens
valid_names_na$order[grep("Ephemerella inermisinfrequens", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("Ephemerella inermisinfrequens", valid_names_na$query, ignore.case = TRUE)]<-"Ephemerellidae"
valid_names_na$genus[grep("Ephemerella inermisinfrequens", valid_names_na$query, ignore.case = TRUE)]<-"Ephemerella"
valid_names_na$accepted_name[grep("Ephemerella inermisinfrequens", valid_names_na$query, ignore.case = TRUE)]<-"Ephemerella"

#Eukiefferiella
valid_names_na$order[grep("Eukiefferiella", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Eukiefferiella", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Eukiefferiella", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Hedriodiscus
valid_names_na$order[grep("Hedriodiscus", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Hedriodiscus", valid_names_na$query, ignore.case = TRUE)]<-"Stratiomyidae"
valid_names_na$accepted_name[grep("Hedriodiscus", valid_names_na$query, ignore.case = TRUE)]<-"Stratiomyidae"

#Helichus
valid_names_na$order[grep("Helichus", valid_names_na$query, ignore.case = TRUE)]<-"Coleoptera"
valid_names_na$family[grep("Helichus", valid_names_na$query, ignore.case = TRUE)]<-"Dryopidae"
valid_names_na$accepted_name[grep("Helichus", valid_names_na$query, ignore.case = TRUE)]<-"Dryopidae"

#HelodonProsimulium	
valid_names_na$order[grep("HelodonProsimulium", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("HelodonProsimulium", valid_names_na$query, ignore.case = TRUE)]<-"Simuliidae"
valid_names_na$accepted_name[grep("HelodonProsimulium", valid_names_na$query, ignore.case = TRUE)]<-"Simuliidae"

#HeptageniaNixe
valid_names_na$order[grep("HeptageniaNixe", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("HeptageniaNixe", valid_names_na$query, ignore.case = TRUE)]<-"Heptageniidae"
valid_names_na$accepted_name[grep("HeptageniaNixe", valid_names_na$query, ignore.case = TRUE)]<-"Heptageniidae"

#Heterosternuta opposituswickhami
valid_names_na$order[grep("Heterosternuta opposituswickhami", valid_names_na$query, ignore.case = TRUE)]<-"Coleoptera"
valid_names_na$family[grep("Heterosternuta opposituswickhami", valid_names_na$query, ignore.case = TRUE)]<-"Dytiscidae"
valid_names_na$genus[grep("Heterosternuta opposituswickhami", valid_names_na$query, ignore.case = TRUE)]<-"Heterosternuta"
valid_names_na$accepted_name[grep("Heterosternuta opposituswickhami", valid_names_na$query, ignore.case = TRUE)]<-"Heterosternuta"

#Heterotrissocladius
valid_names_na$order[grep("Heterotrissocladius", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Heterotrissocladius", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Heterotrissocladius", valid_names_na$query, ignore.case = TRUE)]<-"Heterotrissocladius"
valid_names_na$accepted_name[grep("Heterotrissocladius", valid_names_na$query, ignore.case = TRUE)]<-"Heterotrissocladius"

#Hybomitra
valid_names_na$order[grep("Hybomitra", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Hybomitra", valid_names_na$query, ignore.case = TRUE)]<-"Tabanidae"
valid_names_na$accepted_name[grep("Hybomitra", valid_names_na$query, ignore.case = TRUE)]<-"Tabanidae"

#Hydatophylax
valid_names_na$order[grep("Hydatophylax", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Hydatophylax", valid_names_na$query, ignore.case = TRUE)]<-"Limnephilidae"
valid_names_na$accepted_name[grep("Hydatophylax", valid_names_na$query, ignore.case = TRUE)]<-"Limnephilidae"

#Kloosia
valid_names_na$order[grep("Kloosia", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Kloosia", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Kloosia", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Kogotus
valid_names_na$order[grep("Kogotus", valid_names_na$query, ignore.case = TRUE)]<-"Plecoptera"
valid_names_na$family[grep("Kogotus", valid_names_na$query, ignore.case = TRUE)]<-"Perlodidae"
valid_names_na$accepted_name[grep("Kogotus", valid_names_na$query, ignore.case = TRUE)]<-"Perlodidae"

#Labrundinia
valid_names_na$order[grep("Labrundinia", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Labrundinia", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Labrundinia", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Lestes
valid_names_na$order[grep("Lestes", valid_names_na$query, ignore.case = TRUE)]<-"Odonata"
valid_names_na$family[grep("Lestes", valid_names_na$query, ignore.case = TRUE)]<-"Lestidae"
valid_names_na$genus[grep("Lestes", valid_names_na$query, ignore.case = TRUE)]<-"Lestes"
valid_names_na$accepted_name[grep("Lestes", valid_names_na$query, ignore.case = TRUE)]<-"Lestes"

#Leucrocuta
valid_names_na$order[grep("Leucrocuta", valid_names_na$query, ignore.case = TRUE)]<-"Ephemeroptera"
valid_names_na$family[grep("Leucrocuta", valid_names_na$query, ignore.case = TRUE)]<-"Heptageniidae"
valid_names_na$accepted_name[grep("Leucrocuta", valid_names_na$query, ignore.case = TRUE)]<-"Heptageniidae"

#Liodessus
valid_names_na$order[grep("Liodessus", valid_names_na$query, ignore.case = TRUE)]<-"Coleoptera"
valid_names_na$family[grep("Liodessus", valid_names_na$query, ignore.case = TRUE)]<-"Dytiscidae"
valid_names_na$genus[grep("Liodessus", valid_names_na$query, ignore.case = TRUE)]<-"Liodessus"
valid_names_na$accepted_name[grep("Liodessus", valid_names_na$query, ignore.case = TRUE)]<-"Liodessus"

#Malirekus
valid_names_na$order[grep("Malirekus", valid_names_na$query, ignore.case = TRUE)]<-"Plecoptera"
valid_names_na$family[grep("Malirekus", valid_names_na$query, ignore.case = TRUE)]<-"Perlodidae"
valid_names_na$accepted_name[grep("Malirekus", valid_names_na$query, ignore.case = TRUE)]<-"Perlodidae"

#Meropelopia
valid_names_na$order[grep("Meropelopia", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Meropelopia", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Meropelopia", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Tanytarsus
valid_names_na$order[grep("Tanytarsus", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Tanytarsus", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Tanytarsus", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Limnephilus
valid_names_na$order[grep("Limnephilus", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Limnephilus", valid_names_na$query, ignore.case = TRUE)]<-"Limnephilidae"
valid_names_na$genus[grep("Limnephilus", valid_names_na$query, ignore.case = TRUE)]<-"Limnephilus"
valid_names_na$accepted_name[grep("Limnephilus", valid_names_na$query, ignore.case = TRUE)]<-"Limnephilus"

#Nanocladius
valid_names_na$order[grep("Nanocladius", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Nanocladius", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Nanocladius", valid_names_na$query, ignore.case = TRUE)]<-"Nanocladius"
valid_names_na$accepted_name[grep("Nanocladius", valid_names_na$query, ignore.case = TRUE)]<-"Nanocladius"

#Oecetis
valid_names_na$order[grep("Oecetis", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Oecetis", valid_names_na$query, ignore.case = TRUE)]<-"Leptoceridae"
valid_names_na$genus[grep("Oecetis", valid_names_na$query, ignore.case = TRUE)]<-"Oecetis"
valid_names_na$accepted_name[grep("Oecetis", valid_names_na$query, ignore.case = TRUE)]<-"Oecetis"

#Oemopteryx
valid_names_na$order[grep("Oemopteryx", valid_names_na$query, ignore.case = TRUE)]<-"Plecoptera"
valid_names_na$family[grep("Oemopteryx", valid_names_na$query, ignore.case = TRUE)]<-"Taeniopterygidae"
valid_names_na$accepted_name[grep("Oemopteryx", valid_names_na$query, ignore.case = TRUE)]<-"Taeniopterygidae"

#Onconeura
valid_names_na$order[grep("Onconeura", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Onconeura", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Onconeura", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Paramerina
valid_names_na$order[grep("Paramerina", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Paramerina", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Paramerina", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Peltodytes
valid_names_na$order[grep("Peltodytes", valid_names_na$query, ignore.case = TRUE)]<-"Coleoptera"
valid_names_na$family[grep("Peltodytes", valid_names_na$query, ignore.case = TRUE)]<-"Haliplidae"
valid_names_na$genus[grep("Peltodytes", valid_names_na$query, ignore.case = TRUE)]<-"Peltodytes"
valid_names_na$accepted_name[grep("Peltodytes", valid_names_na$query, ignore.case = TRUE)]<-"Peltodytes"

#Pericoma
valid_names_na$order[grep("Pericoma", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Pericoma", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Pericoma", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Phaenopsectra
valid_names_na$order[grep("Phaenopsectra", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Phaenopsectra", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Phaenopsectra", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#Rhyacophila
valid_names_na$order[grep("Rhyacophila", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Rhyacophila", valid_names_na$query, ignore.case = TRUE)]<-"Rhyacophilidae"
valid_names_na$genus[grep("Rhyacophila", valid_names_na$query, ignore.case = TRUE)]<-"Rhyacophila"
valid_names_na$accepted_name[grep("Rhyacophila", valid_names_na$query, ignore.case = TRUE)]<-"Rhyacophila"

#Stempellinella
valid_names_na$order[grep("Stempellinella", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Stempellinella", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$accepted_name[grep("Stempellinella", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"

#	Stenelmis decoratassa
valid_names_na$order[grep("Stenelmis decoratassa", valid_names_na$query, ignore.case = TRUE)]<-"Coleoptera"
valid_names_na$family[grep("Stenelmis decoratassa", valid_names_na$query, ignore.case = TRUE)]<-"Elmidae"
valid_names_na$genus[grep("Stenelmis decoratassa", valid_names_na$query, ignore.case = TRUE)]<-"Stenelmis"
valid_names_na$species[grep("Stenelmis decoratassa", valid_names_na$query, ignore.case = TRUE)]<-"Stenelmis decorata"
valid_names_na$accepted_name[grep("Stenelmis decoratassa", valid_names_na$query, ignore.case = TRUE)]<-"Stenelmis decorata"

#Stictochironomus
valid_names_na$order[grep("Stictochironomus", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Stictochironomus", valid_names_na$query, ignore.case = TRUE)]<-"Chironomidae"
valid_names_na$genus[grep("Stictochironomus", valid_names_na$query, ignore.case = TRUE)]<-"Stictochironomus"
valid_names_na$accepted_name[grep("Stictochironomus", valid_names_na$query, ignore.case = TRUE)]<-"Stictochironomus"

#Tabanus
valid_names_na$order[grep("Tabanus", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("Tabanus", valid_names_na$query, ignore.case = TRUE)]<-"Tabanidae"
valid_names_na$accepted_name[grep("Tabanus", valid_names_na$query, ignore.case = TRUE)]<-"Tabanidae"

#Triaenodes pernahelo
valid_names_na$order[grep("Triaenodes pernahelo", valid_names_na$query, ignore.case = TRUE)]<-"Trichoptera"
valid_names_na$family[grep("Triaenodes pernahelo", valid_names_na$query, ignore.case = TRUE)]<-"Leptoceridae"
valid_names_na$genus[grep("Triaenodes pernahelo", valid_names_na$query, ignore.case = TRUE)]<-"Triaenodes"
valid_names_na$accepted_name[grep("Triaenodes pernahelo", valid_names_na$query, ignore.case = TRUE)]<-"Triaenodes"

#TrichoclinoceraClinocera
valid_names_na$order[grep("TrichoclinoceraClinocera", valid_names_na$query, ignore.case = TRUE)]<-"Diptera"
valid_names_na$family[grep("TrichoclinoceraClinocera", valid_names_na$query, ignore.case = TRUE)]<-"Empididae"
valid_names_na$accepted_name[grep("TrichoclinoceraClinocera", valid_names_na$query, ignore.case = TRUE)]<-"Empididae"

#Yugus
valid_names_na$order[grep("Yugus", valid_names_na$query, ignore.case = TRUE)]<-"Plecoptera"
valid_names_na$family[grep("Yugus", valid_names_na$query, ignore.case = TRUE)]<-"Perlodidae"
valid_names_na$genus[grep("Yugus", valid_names_na$query, ignore.case = TRUE)]<-"Yugus"
valid_names_na$accepted_name[grep("Yugus", valid_names_na$query, ignore.case = TRUE)]<-"Yugus"

#combine valid_names_new and valid_names_na, then find TSN and accepted_name for all with na in accepted_name
valid_names_new$accepted_name<-NA
valid_names_new2<-merge(valid_names_new, valid_names_na, by="query",all.x=TRUE)
valid_names_new2$Order<-if_else(is.na(valid_names_new2$order.x), valid_names_new2$order.y, valid_names_new2$order.x)
valid_names_new2$Family<-if_else(is.na(valid_names_new2$family.x), valid_names_new2$family.y, valid_names_new2$family.x)
valid_names_new2$Genus<-if_else(is.na(valid_names_new2$genus.x), valid_names_new2$genus.y, valid_names_new2$genus.x)
valid_names_new2$Species<-if_else(is.na(valid_names_new2$species.x), valid_names_new2$species.y, valid_names_new2$species.x)
valid_names_new2$Accepted_name<-if_else(is.na(valid_names_new2$accepted_name.x), valid_names_new2$accepted_name.y, as.character(valid_names_new2$accepted_name.x))
#drop unneeded columns
valid_names_new2<-select(valid_names_new2, c(query, Order, Family, Genus, Species, Accepted_name))

#create accepted_name from other names and use to search for TSN in ITIS
valid_names_new2$Accepted_name<-if_else(is.na(valid_names_new2$Accepted_name), valid_names_new2$Species, valid_names_new2$Accepted_name)
valid_names_new2$Accepted_name<-if_else(is.na(valid_names_new2$Accepted_name), valid_names_new2$Genus, valid_names_new2$Accepted_name)
valid_names_new2$Accepted_name<-if_else(is.na(valid_names_new2$Accepted_name), valid_names_new2$Family, valid_names_new2$Accepted_name)
valid_names_new2$Accepted_name<-if_else(is.na(valid_names_new2$Accepted_name), valid_names_new2$Order, valid_names_new2$Accepted_name)

utaxon.6<-unique(valid_names_new2$Accepted_name)
#get TSN
final_tsn<-get_tsn(utaxon.6, searchtype="scientific", accepted=FALSE, ask=TRUE)
valid_names_new3<-as.data.frame(cbind(utaxon.6, as.character(final_tsn)), stringsAsFactors = FALSE) #719 taxa that I manually assigned names to and their TSN from ITIS, for most the Submitted name is the Accepted name
#write file
write.csv(valid_names_new3, "~/Documents/WaterCube/Ch.3/aquatic_insects/valid_names_new3.csv")

#final name check for accepted name using TSN
final_accepted_names<-itis_acceptname(searchtsn = na.omit(final_tsn))

valid_names_new4<-merge(valid_names_new3, final_accepted_names, by.x="V2", by.y="submittedtsn", all=TRUE)

#merge with larger list (valid_names_new2)
valid_names_new5<-merge(valid_names_new4, valid_names_new2, by.x="utaxon.6", by.y="Accepted_name", all=TRUE)
#create final column of accepted names
valid_names_new5$Accepted_name<-if_else(is.na(valid_names_new5$acceptedname), valid_names_new5$utaxon.6, valid_names_new5$acceptedname)
valid_names_new6<-select(valid_names_new5, c(acceptedtsn, query, Order, Family, Genus, Species, Accepted_name))

#merge with occurrence12
occurrence13<-merge(occurrence12, valid_names_new6, by.x="Submitted_name", by.y="query", all.x=TRUE)
#combine name and TSN columns
occurrence13$Accepted_name<-if_else(is.na(occurrence13$Accepted_name.x), occurrence13$Accepted_name.y, occurrence13$Accepted_name.x)
occurrence13$Order<-if_else(is.na(occurrence13$Order.x), occurrence13$Order.y, occurrence13$Order.x)
occurrence13$Family<-if_else(is.na(occurrence13$Family.x), occurrence13$Family.y, occurrence13$Family.x)
occurrence13$Genus<-if_else(is.na(occurrence13$Genus.x), occurrence13$Genus.y, occurrence13$Genus.x)
occurrence13$Species<-if_else(is.na(occurrence13$Species.x), occurrence13$Species.y, occurrence13$Species.x)
occurrence13$Accepted_TSN<-if_else(is.na(occurrence13$Accepted_TSN), occurrence13$acceptedtsn, occurrence13$Accepted_TSN)

#how many still have na in Order?
na_final<-subset(occurrence13, is.na(Order))
na.orders_final<-unique(na_final$Submitted_name)
#just a couple of insects have NA in order- will manually assign data

#drop unneeded columns
occurrence13<-select(occurrence13, c(Submitted_name,Taxonomic_resolution, Location_ID, Sample_ID, Date, Latitude, Longitude, Provider_name, Sample_method, Location_description, HorizontalCoordinateReferenceSystemDatumName, Monitoring_organization, Date1, Accepted_TSN, Accepted_name, Order, Family, Genus, Species))

#assign data for insects with NA in Order
occurrence13$Accepted_name[which(occurrence13$Submitted_name == "Coenaon Enallagma")]<-"Coenagrionidae" 
occurrence13$Order[which(occurrence13$Submitted_name == "Coenaon Enallagma")]<-"Odonata" 
occurrence13$Family[which(occurrence13$Submitted_name == "Coenaon Enallagma")]<-"Coenagrionidae" 
occurrence13$Accepted_TSN[which(occurrence13$Submitted_name == "Coenaon Enallagma")]<-"102077" 
occurrence13$Accepted_name[which(occurrence13$Submitted_name == "CoenaonEnallagma")]<-"Coenagrionidae" 
occurrence13$Order[which(occurrence13$Submitted_name == "CoenaonEnallagma")]<-"Odonata" 
occurrence13$Family[which(occurrence13$Submitted_name == "CoenaonEnallagma")]<-"Coenagrionidae" 
occurrence13$Accepted_TSN[which(occurrence13$Submitted_name == "CoenaonEnallagma")]<-"102077" 

#drop taxa with NA or non-insect Order- there are over 2.4 million insect records at this stage
occurrence14<-occurrence13%>%filter(Order=="Diptera" | Order=="Coleoptera" | Order=="Megaloptera" | Order=="Lepidoptera" | Order=="Ephemeroptera" | Order=="Plecoptera"| Order=="Hemiptera"| Order=="Trichoptera"| Order=="Odonata" | Order=="Neuroptera")

library(plyr)
#update taxonomic resolution
revalue(occurrence14$Taxonomic_resolution[!is.na(occurrence14$Genus)]<-"Genus")
#family
revalue(occurrence14$Taxonomic_resolution[is.na(occurrence14$Genus) & !is.na(occurrence14$Family)]<-"Family")

#check the resolution of insects assigned to family and na
family<-occurrence14[which(occurrence14$Taxonomic_resolution =="Family"),]
unique(family$Family)

na.check<-occurrence14[is.na(occurrence14$Taxonomic_resolution),] #all are Orders
revalue(occurrence14$Taxonomic_resolution[is.na(occurrence14$Taxonomic_resolution)]<-"Order")
occurrence14$TARGET <- grepl(" ", occurrence14$Accepted_name) #create new column that describes whether there is a space in Accepted_name (TRUE for species)
occurrence14$Taxonomic_resolution[which(occurrence14$TARGET=="TRUE")]<-"Species" #assign species to Taxonomic_resolution

#assign species name for those that have NA in species but species as taxonomic resolution 
occurrence14$Species<-if_else(is.na(occurrence14$Species)&occurrence14$TARGET=="TRUE", occurrence14$Accepted_name, occurrence14$Species)

#Check Accepted_name column again- assign to those with NA based on values in other name columns: Order, Family, Genus, Species
NA_accepted_name<-occurrence14[is.na(occurrence14$Accepted_name),] #still many with no Accepted_name, but other names have been verified above
occurrence14$Accepted_name<-if_else(is.na(occurrence14$Accepted_name), occurrence14$Species, occurrence14$Accepted_name)
occurrence14$Accepted_name<-if_else(is.na(occurrence14$Accepted_name), occurrence14$Genus, occurrence14$Accepted_name)
occurrence14$Accepted_name<-if_else(is.na(occurrence14$Accepted_name), occurrence14$Family, occurrence14$Accepted_name)
occurrence14$Accepted_name<-if_else(is.na(occurrence14$Accepted_name), occurrence14$Order, occurrence14$Accepted_name)

#drop TARGET and old date column- unload plyr before running, interferes with dplyr
occurrence15<-select(occurrence14, -c(TARGET, Date))

#Fill NAs in Monitoring_organization (all are data from state agency requests) with Provider_Name (all providers are state agencies, other than STORET)
occurrence15$Monitoring_organization<-if_else(is.na(occurrence15$Monitoring_organization), occurrence15$Provider_name, occurrence15$Monitoring_organization)

############ Remove records in lakes ####################################################################################################################################################################################################
#check which Location_description includes lakes, because some were mis-entered in STORET as streams/rivers
lakes<-c("Lake", "lake")
occurrence16<-subset(occurrence15, grepl(paste(lakes,collapse="|"), Location_description)) #which records contain word lake
streams<-c("UNT", "Slough", "slough","meadow", "Meadow","Rio", "R", "Lake park", "lake park","Near","near","nr","Nr","NR","rio","branch","DS", "ds", "ditch", "Ditch","Blw", "blw","Drain", "drain", "Branch","Canal", "canal", "Channel", "channel", "Trib", "trib", "Trib.", "trib.", "Cr", "cr", "Cr.", "cr.", "Creek", "creek", "River", "river", "Stream", "stream", "St.", "st.", "St", "st.", "Str", "str", "Str.", "str.", "Inlet", "inlet", "Outlet", "outlet", "Fork", "fork", "Tributary", "tributary","Brook", "brook")
#now find records that contain Trib, trib, Trib., trib., Cr, cr, Cr., cr., Creek, creek, River, river, Stream, stream, St., st., St, st. Str, str, Str. str., Inlet, inlet, Outlet, outlet, Fork, fork, Tributary, tributary,Brook, brook,
occurrence17<-subset(occurrence16, grepl(paste(streams, collapse = "|"), Location_description))

#now use anti_join to remove records that mention streams from occurrence16
occurrence18<-anti_join(occurrence16, occurrence17)

#then anti_join with occurrence dataset to remove lakes
occurrence19<-anti_join(occurrence15, occurrence18)

################### Cleaning of Lat and Long Coordinates #####################################################################################################################################################################################
#Remove records that are missing horizontal datum type
occurrence20<-subset(occurrence19, !is.na(HorizontalCoordinateReferenceSystemDatumName))

#fix problems with longitude coordinates- append (-) to coords that are greater than zero
occurrence20[which(occurrence20$Longitude>0),] #some stations recorded positive values for longitude
occurrence20.1<-subset(occurrence20, Longitude>0)
occurrence20.1$Longitude<-occurrence20.1$Longitude*(-1)

#recombine with larger occurrence dataset
occurrence20.2<-subset(occurrence20, Longitude<=0)

occurrence21<-rbind(occurrence20.1, occurrence20.2)

#remove extra 9 appended to Longitude coordinates in Arkanasas- Location_description="Petit Jean River"
occurrence21$Longitude[which(occurrence21$Location_description=="Petit Jean River")]<--93.9375
occurrence21[which(occurrence21$Location_description=="Petit Jean River"),] #Ok now

#go back to occurrence dataset to remove problematic coords
occurrence21[which(occurrence21$Latitude>50),] #all at Latitude above 50 are STORET samples from Alaska- will need to drop
occurrence21[which(occurrence21$Latitude<20),] #all at Latitude below 20 are samples from Puerto Rico and samples misredorded at 0 degrees lat and long
occurrence21[which(occurrence21$Longitude>-60),] #many latitude entries given as 0- need to remove

#drop these non-CONUS occurrence records from database
occurrence22<-subset(occurrence21, Latitude<50&Latitude>20&Longitude<(-60))
maine<-occurrence22[which(occurrence22$Latitude>47.16&occurrence22$Longitude>(-68)),]#this is right on the border of Maine and Canada, it just looks outside US borders in map- *ok* to keep

################# Align sampling method data with sampling method table ##########################################################################################################################
#change sampling method columns based on email communications with state agency personnel
occurrence22$Provider_name[which(occurrence22$Provider_name=="Oregon Department of Environmental Protection")]<-"Oregon Department of Environmental Quality"
occurrence22$Sample_method[which(occurrence22$Sample_method=="5tk"&occurrence22$Provider_name=="Arkansas Department of Environmental Quality")]<-"5 minute travelling kick"
occurrence22$Sample_method[which(occurrence22$Sample_method=="5-TK"&occurrence22$Provider_name=="Arkansas Department of Environmental Quality")]<-"5 minute travelling kick"
occurrence22$Sample_method[which(occurrence22$Sample_method=="5 minute"&occurrence22$Provider_name=="Arkansas Department of Environmental Quality")]<-"5 minute travelling kick"
occurrence22$Sample_method[which(occurrence22$Sample_method=="5TK"&occurrence22$Provider_name=="Arkansas Department of Environmental Quality")]<-"5 minute travelling kick"
occurrence22$Sample_method[which(occurrence22$Sample_method=="PROPORTIONAL"&occurrence22$Provider_name=="Arkansas Department of Environmental Quality")]<-"Proportional"
occurrence22$Sample_method[which(occurrence22$Provider_name=="Idaho Department of Environmental Quality")]<-NA
occurrence22$Sample_method[which(occurrence22$Provider_name=="California State Water Resources Control Board"&occurrence22$Sample_method=="MI_Reach-WideBenthos")]<-"BMI_Reach-WideBenthos"
occurrence22$Sample_method[which(occurrence22$Provider_name=="Minnesota Pollution Control Agency")]<-"QMH"
#assign NARS as sample Method for all New Mexico samples for dates after 2010 per conversation with contact from state agency
occurrence22$Sample_method[which(occurrence22$Provider_name=="New Mexico Environment Department"&occurrence22$Date1>=2010)]<-"NARS"

method<-occurrence22[which(occurrence22$Provider_name!="STORET"),]
unique(method$Sample_method) #records all look OK now

#########Check remaining naming problems with occurrence22##########################################################################################################
genus<-unique(sort(occurrence22$Genus))
occurrence22[which(occurrence22$Genus=="Ancyronx"),]
occurrence22$Genus[which(occurrence22$Genus=="Ancyronx")]<-"Ancyronyx"
occurrence22$Genus[which(occurrence22$Genus=="Camelobaetidus")]<-"Camelobaetidius"
occurrence22$Genus[which(occurrence22$Genus=="Chyrandra")]<-"Chyranda"
occurrence22$Accepted_name[which(occurrence22$Accepted_name=="Chyrandra centralis")]<-"Chyranda centralis"
occurrence22$Accepted_TSN[which(occurrence22$Accepted_name=="Chyranda centralis")]<-1053275
occurrence22$Species[which(occurrence22$Species=="Chyrandra centralis")]<-"Chyranda centralis"
occurrence22$Accepted_name[which(occurrence22$Accepted_name=="Chyrandra")]<-"Chyranda"
occurrence22$Accepted_TSN[which(occurrence22$Accepted_name=="Chyranda")]<-116017
occurrence22[which(occurrence22$Genus=="Climacia"),]
occurrence22$Genus[which(occurrence22$Genus=="Climacea")]<-"Climacia"
occurrence22$Accepted_name[which(occurrence22$Accepted_name=="Climacea areolarsis")]<-"Climacia areolaris"
occurrence22$Species[which(occurrence22$Species=="Climacea areolarsis")]<-"Climacia areolaris"
occurrence22$Genus[which(occurrence22$Genus=="Gelastocoris ")]<-"Gelastocoris"
occurrence22[which(occurrence22$Genus=="Gelastocoris"),]
occurrence22$Genus[which(occurrence22$Genus=="Dromogomphus spoliatus")]<-"Dromogomphus"
occurrence22[which(occurrence22$Genus=="Gymnometriocnemus"),]
occurrence22$Genus[which(occurrence22$Genus=="Gymnometiocnemus")]<-"Gymnometriocnemus"
occurrence22$Accepted_name[which(occurrence22$Accepted_name=="Gymnometiocnemus")]<-"Gymnometriocnemus"
occurrence22$Accepted_TSN[which(occurrence22$Accepted_name=="Gymnometriocnemus")]<-128718
occurrence22[which(occurrence22$Genus=="Labrundinia"),]
occurrence22$Genus[which(occurrence22$Genus=="Labrudinia")]<-"Labrundinia"
occurrence22$Accepted_name[which(occurrence22$Accepted_name=="Labrudinia")]<-"Labrundinia"
occurrence22$Accepted_TSN[which(occurrence22$Accepted_name=="Labrundinia")]<-128173
occurrence22[which(occurrence22$Genus=="Lipinella"),]
occurrence22$Genus[which(occurrence22$Genus=="Lipiniella")]<-"Lipinella"
occurrence22$Accepted_name[which(occurrence22$Genus=="Lipinella")]<-"Lipinella"
occurrence22$Accepted_TSN[which(occurrence22$Accepted_name=="Lipinella")]<-129531

######### Additional cleaning of occurrence records: checking for georeferencing errors ###########################################
#look for state outliers- samples taken fromn a state but whose cooordinates lie outside of state bounds
#first create column that describes which state each record is in
#which agencies provided the data?
unique(occurrence22$Provider_name) #first assign based on provder name
occurrence22$State<-NA
occurrence22$State[which(occurrence22$Provider_name=="Nebraska Department of Environmental Quality")]<-"Nebraska"
occurrence22$State[which(occurrence22$Provider_name=="Arkansas Department of Environmental Quality")]<-"Arkansas"
occurrence22$State[which(occurrence22$Provider_name=="New Mexico Environment Department")]<-"New Mexico"
occurrence22$State[which(occurrence22$Provider_name=="Arizona Department of Environmental Quality")]<-"Arizona"
occurrence22$State[which(occurrence22$Provider_name=="California State Water Resources Control Board" )]<-"California"
occurrence22$State[which(occurrence22$Provider_name=="Minnesota Pollution Control Agency")]<-"Minnesota"
occurrence22$State[which(occurrence22$Provider_name=="Missouri Department of Natural Resources")]<-"Missouri"
occurrence22$State[which(occurrence22$Provider_name=="Ohio Environmental Protection Agency")]<-"Ohio"
occurrence22$State[which(occurrence22$Provider_name=="Oregon Department of Environmental Quality")]<-"Oregon"
occurrence22$State[which(occurrence22$Provider_name=="Florida Department of Environmental Protection")]<-"Florida"
occurrence22$State[which(occurrence22$Provider_name=="West Virginia Department of Environmental Protection")]<-"West Virginia"
occurrence22$State[which(occurrence22$Provider_name=="Nevada Department of Environmental Protection")]<-"Nevada"
occurrence22$State[which(occurrence22$Provider_name=="Idaho Department of Environmental Quality")]<-"Idaho"
occurrence22$State[which(occurrence22$Provider_name=="Texas Commission on Environmental Quality")]<-"Texas"
occurrence22$State[which(occurrence22$Provider_name=="Iowa Department of Natural Resources")]<-"Iowa"
occurrence22$State[which(occurrence22$Provider_name=="Pennsylvania Department of Environmental Protection")]<-"Pennsylvania"
occurrence22$State[which(occurrence22$Provider_name=="Washington State Department of Ecology")]<-"Washington"

#now assign based on monitoring organzation
unique(occurrence22$Monitoring_organization)
occurrence22$State[which(occurrence22$Monitoring_organization=="The Stockbridge-Munsee Community")]<-"Wisconsin"
occurrence22$State[grep("Oklahoma", occurrence22$Monitoring_organization)]<-"Oklahoma"
occurrence22$State[grep("Michigan", occurrence22$Monitoring_organization)]<-"Michigan"
occurrence22$State[grep("MI", occurrence22$Monitoring_organization)]<-"Michigan"
occurrence22$State[grep("Montana", occurrence22$Monitoring_organization)]<-"Montana"
occurrence22$State[grep("Kansas", occurrence22$Monitoring_organization)]<-"Kansas"
occurrence22$State[grep("Louisville", occurrence22$Monitoring_organization)]<-NA
occurrence22$State[grep("Arizona", occurrence22$Monitoring_organization, ignore.case = TRUE)]<-"Arizona"
occurrence22$State[grep("Arkansas", occurrence22$Monitoring_organization)]<-"Arkansas"
occurrence22$State[grep("Maryland", occurrence22$Monitoring_organization)]<-"Maryland"
occurrence22$State[grep("Colorado", occurrence22$Monitoring_organization)]<-"Colorado"
occurrence22$State[grep("Pennsylvania", occurrence22$Monitoring_organization)]<-"Pennsylvania"
occurrence22$State[grep("Georgia", occurrence22$Monitoring_organization)]<-"Georgia"
occurrence22$State[grep("Iowa", occurrence22$Monitoring_organization)]<-"Iowa"
occurrence22$State[grep("Pennsylvania", occurrence22$Monitoring_organization)]<-"Pennsylvania"
occurrence22$State[grep("Washington", occurrence22$Monitoring_organization)]<-"Washington"
occurrence22$State[grep("Utah", occurrence22$Monitoring_organization)]<-"Utah"
occurrence22$State[grep("PA", occurrence22$Monitoring_organization)]<-"Pennsylvania"
occurrence22$State[grep("Idaho", occurrence22$Monitoring_organization)]<-"Idaho"
occurrence22$State[grep("New Hampshire", occurrence22$Monitoring_organization, ignore.case = TRUE)]<-"New Hampshire"
occurrence22$State[grep("Delaware", occurrence22$Monitoring_organization)]<-"Delaware"
occurrence22$State[grep("New York", occurrence22$Monitoring_organization)]<-"New York"
occurrence22$State[grep("West Virginia", occurrence22$Monitoring_organization)]<-"West Virginia"
occurrence22$State[grep("Florida", occurrence22$Monitoring_organization)]<-"Florida"
occurrence22$State[grep("Ohio", occurrence22$Monitoring_organization)]<-"Ohio"
occurrence22$State[grep("MT", occurrence22$Monitoring_organization)]<-"Montana"
occurrence22$State[grep("Missouri", occurrence22$Monitoring_organization)]<-"Missouri"
occurrence22$State[grep("NM", occurrence22$Monitoring_organization)]<-"New Mexico"
occurrence22$State[grep("Maine", occurrence22$Monitoring_organization)]<-"Maine"
occurrence22$State[grep("Wisconsin", occurrence22$Monitoring_organization)]<-"Wisconsin"
occurrence22$State[grep("California", occurrence22$Monitoring_organization)]<-"California"
unique(occurrence22$Monitoring_organization[which(is.na(occurrence22$State))])

occurrence22$State[which(occurrence22$Monitoring_organization=="NCDENR-DWQ WQX")]<-"North Carolina"
occurrence22$State[which(occurrence22$Monitoring_organization=="NJDEP Americorps Program")]<-"New Jersey"
occurrence22$State[which(occurrence22$Monitoring_organization=="Stony Brook-Millstone Watershed Association")]<-"New Jersey"
occurrence22$State[which(occurrence22$Monitoring_organization=="Bay Mills Indian Community")]<-"Michigan"
occurrence22$State[which(occurrence22$Monitoring_organization=="Quapaw Tribe Environmental Office")]<-"Oklahoma"
occurrence22$State[which(occurrence22$Monitoring_organization=="Pawnee Nation Dept of Environmental Conservation and Safety")]<-"Oklahoma"
occurrence22$State[which(occurrence22$Monitoring_organization=="Prairie Band Potawatomi Nation")]<-"Kansas"
occurrence22$State[which(occurrence22$Monitoring_organization=="Musconetcong River Watershed Association")]<-"New Jersey"
occurrence22$State[which(occurrence22$Monitoring_organization=="Kaw Nation")]<-"Oklahoma"
occurrence22$State[which(occurrence22$Monitoring_organization=="HANNAHVILLE TRIBAL COMMUNITY")]<-"Michigan"
occurrence22$State[which(occurrence22$Monitoring_organization=="Citizen Potawatomi Nation")]<-"Oklahoma"
occurrence22$State[which(occurrence22$Monitoring_organization=="Gros Ventre and Assiniboine Tribe (Fort Belknap Indian Res)")]<-"Montana"
occurrence22$State[which(occurrence22$Monitoring_organization=="MUSCONETCONG WATERSHED ASSOCIATION")]<-"New Jersey"
occurrence22$State[which(occurrence22$Monitoring_organization=="Confederated Tribes of Siletz Indians")]<-NA
occurrence22$State[which(occurrence22$Monitoring_organization=="Upper Raritan Watershed Association")]<-"New Jersey"
occurrence22$State[which(occurrence22$Monitoring_organization=="South Branch Watershed Assoc")]<-"New Jersey"
occurrence22$State[which(occurrence22$Monitoring_organization=="Eagle River Watershed Council (ERWC)")]<-"Colorado"
occurrence22$State[which(occurrence22$Monitoring_organization=="Great Swamp Watershed Association")]<-"New Jersey"
occurrence22$State[which(occurrence22$Monitoring_organization=="Roaring Fork Conservancy")]<-"Colorado"
occurrence22$State[which(occurrence22$Monitoring_organization=="Grand Portage Reservation")]<-"Minnesota"
occurrence22$State[which(occurrence22$Monitoring_organization=="Susquehanna River Basin Commission")]<-"Pennsylvania"
occurrence22$State[which(occurrence22$Monitoring_organization=="Pueblo of Taos")]<-"New Mexico"
occurrence22$State[which(occurrence22$Monitoring_organization=="NJDEP Bureau of Freshwater and Biological Monitoring")]<-"New Jersey"
occurrence22$State[which(occurrence22$Monitoring_organization=="US Army Corps of Engineers, Nashville District")]<-NA
occurrence22$State[which(occurrence22$Monitoring_organization=="OWRB Streams Monitoring")]<-"Oklahoma"
occurrence22$State[which(occurrence22$Monitoring_organization=="Red Lake DNR")]<-"Minnesota"
occurrence22$State[which(occurrence22$Monitoring_organization=="EMAP-Great Rivers Ecosystems")]<-NA
occurrence22$State[which(occurrence22$Monitoring_organization=="National Park Service Water Resources Division")]<-NA
occurrence22$State[which(occurrence22$Monitoring_organization=="Southern Ute Tribe")]<-"Colorado"
occurrence22$State[which(occurrence22$Monitoring_organization=="EPA National Aquatic Resources Survey (NARS)")]<-NA
occurrence22$State[which(occurrence22$Monitoring_organization=="EPA R7")]<-NA
occurrence22$State[which(occurrence22$Monitoring_organization=="Arizona Department of Environmental Quality")]<-"Arizona"
occurrence22$State[which(occurrence22$Monitoring_organization=="NEW HAMPSHIRE DEPARTMENT OF ENVIRONMENTAL SERVICES")]<-"New Hampshire"
occurrence22$State[which(occurrence22$Monitoring_organization=="PA DEPARTMENT OF ENVIRONMENTAL PROTECTION")]<-"Pennsylvania"
occurrence22$State[which(occurrence22$Monitoring_organization=="ARIZONA DEPARTMENT OF ENVIRONMENTAL QUALITY")]<-"Arizona"

#now look at records for each state and EMAP and NPS sites
#subset records to those in WGS84 and reproject to Albers equal area 
crsaea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
wgs1984.proj <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
crsnad83<-'+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0'
crsnad27<-'+init=epsg:4267 +proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs+nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat'

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
insect.reproject<-rbind(nad27, nad83, wgs84)

#plot each state individually
states_to_plot <- unique(occurrence22$State)

for (i in states_to_plot) {
  state_i <- ggplot(states, aes(x=long,y=lat)) + geom_path(color="black",aes(group=group))+
    geom_point(data = insect.reproject[which(insect.reproject$State==i),], aes(x = Longitude_transformed, y = Latitude_transformed))+
    theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                     panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                     panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 25), legend.title = element_text(size=30, face="bold"))
  ggsave(filename = paste0('~/Documents/WaterCube/Ch.3/aquatic_insects/state_', i, '_map.png'), plot = state_i, height = 6, width = 9, dpi = 400)
  print(i)
}

#examine and clean misclassified/outlier records
Arkansas<-occurrence22[which(occurrence22$State=="Arkansas"),]
unique(Arkansas$Monitoring_organization)
max(Arkansas$Latitude)
#remove site with error
occurrence22<-occurrence22[!(occurrence22$Location_ID=="ADEQ4G-36"),]

Georgia<-occurrence22[which(occurrence22$State=="Georgia"),]
min(Georgia$Longitude) #ok, matches site description

Maryland<-occurrence22[which(occurrence22$State=="Maryland"),]
min(Maryland$Longitude) #ok, matches site description

Missouri<-occurrence22[which(occurrence22$State=="Missouri"),]
min(Missouri$Longitude) #ok, matches site decsription

New_Mexico<-occurrence22[which(occurrence22$State=="New Mexico"),]
unique(New_Mexico$Monitoring_organization)

Oregon<-occurrence22[which(occurrence22$State=="Oregon"),]
unique(Oregon$Monitoring_organization)

Oklahoma<-occurrence22[which(occurrence22$State=="Oklahoma"),]
unique(Oklahoma$Monitoring_organization)
min(Oklahoma$Latitude)

#remove site with error
occurrence22<-occurrence22[!(occurrence22$Location_ID=="KAWNATON_WQX-KNBCR"),]

#check for genus records that are geographic outliers by comparing distributions to GBIF records
#plot each genus
vars_to_plot <- unique(occurrence22$Genus)
for (i in vars_to_plot) {
  map_i <- ggplot(states, aes(x=long,y=lat)) + geom_path(color="black",aes(group=group))+
    geom_point(data = insect.reproject[which(insect.reproject$Genus==i),], aes(x = Longitude_transformed, y = Latitude_transformed))+
    theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                     panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                     panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 25), legend.title = element_text(size=30, face="bold"))
  ggsave(filename = paste0('~/Documents/WaterCube/Ch.3/aquatic_insects/genus_', i, '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
  print(i)
}

############# Prepare tables for EDI #########################################################################################################################
#correct problems with na in the accepted_name when submitted_name is in all-caps
occurrence22<-occurrence22 %>% mutate(Submitted_name = str_to_sentence(Submitted_name))

occurrence22$Accepted_name<-if_else(is.na(occurrence22$Accepted_name)&occurrence22$Submitted_name==occurrence22$Genus, occurrence22$Genus, occurrence22$Accepted_name)
occurrence22$Accepted_name<-if_else(is.na(occurrence22$Accepted_name)&occurrence22$Submitted_name==occurrence22$Species, occurrence22$Species, occurrence22$Accepted_name)
occurrence22$Accepted_name<-if_else(is.na(occurrence22$Accepted_name)&occurrence22$Submitted_name==occurrence22$Family, occurrence22$Family, occurrence22$Accepted_name)
occurrence22$Accepted_name<-if_else(is.na(occurrence22$Accepted_name)&occurrence22$Submitted_name==occurrence22$Order, occurrence22$Order, occurrence22$Accepted_name)

#prepare ancillary_taxonomy table for merging with trait ancillary_taxonomy_table
#create ancillary taxonomy table- need to add taxa from occurrence table, also
names(occurrence22)
ancillary_taxonomy<-select(occurrence22,c("Submitted_name","Accepted_name", "Accepted_TSN", "Order", "Family", "Genus", "Species"))
write.csv(ancillary_taxonomy, "ancillary_taxonomy_occurrence.csv")

#prepare genus_occurrence_table for EDI
occurrence22.1<-occurrence22[!is.na(occurrence22$Genus),]

genus_occurrence_edi<-select(occurrence22.1, c("Genus", "Location_ID", "Latitude", "Longitude", "HorizontalCoordinateReferenceSystemDatumName"))
colnames(genus_occurrence_edi)<-c("Genus", "Location_ID", "Latitude", "Longitude", "Horizontal_datum")
write.csv(genus_occurrence_edi, "genus_occurrence_table_edi.csv")

#write to file- ready for analysis
write.csv(occurrence22, "~/Documents/WaterCube/Ch.3/aquatic_insects/occurrence22.csv")

#write to file- ready for analysis- genus-only records
write.csv(occurrence22.1, "~/Documents/WaterCube/Ch.3/aquatic_insects/occurrence22.1.csv")

save.image("~/Documents/WaterCube/Ch.3/aquatic_insects/WQP_state_merge_2.RData")

#prepare raw community table for EDI
head(occurrence22)

raw_community<-occurrence22%>%select(c("Taxonomic_resolution", "Submitted_name", "Location_ID", "Sample_ID", "Date1", "Latitude", "Longitude", "HorizontalCoordinateReferenceSystemDatumName","Provider_name", "Monitoring_organization", "State","Location_description", "Sample_method"))
colnames(raw_community)<-c("Taxonomic_resolution", "Submitted_name_occurrence", "Location_ID", "Sample_ID", "Date", "Latitude", "Longitude", "Horizontal_datum", "Data_source", "Monitoring_organization", "Study_state", "Location_description", "Sample_method")
write.csv(raw_community,"raw_community_table.csv")

#this completes data cleaning code on occurrence records. all below code is for data exploration and mapping. will eventually be cleaned and separated into a different R script.

######### Reproject spatial coordinates to US Albers Equal Area Projection ##################################################################################
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

#load libraries for mapping
library(maps); library(ggplot2); library(mapproj); library(dplyr); library(ggspatial); library(ggsn)


#################### Plot all sites ############################################################################################################
library(maps)

#reproject state data from map_data
states <- map_data('state')
state_coords <- SpatialPoints(coords = with(states, data.frame(x = long, y = lat)), proj4string = CRS(wgs1984.proj))
state_albers <- spTransform(state_coords, CRSobj = CRS(crsaea))
states$long <- state_albers@coords[,1]
states$lat <- state_albers@coords[,2]

#plot all points
#site_coords_new<-subset(site_coords1, Provider_name!="STORET")

# This plot works and looks good- need to fix axis labels
richness_map<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = genus.rich, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=genus_richness), shape=20)+scale_colour_gradient(name="Genus richness",low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 25), legend.title = element_text(size=30, face="bold"))
richness_map+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/Genus_site_richness.png", height = 8, width = 16, dpi = 300)
richness_map+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
dev.off()

sites_map<-ggplot(states, aes(x=long,y=lat)) 
sites_map2<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = genus.occurrence, aes(x = Longitude_transformed, y = Latitude_transformed), color = 'black', alpha = 1, shape=20, size=1)+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 25), legend.title = element_text(size=30, face="bold"))
sites_map2+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Figures/all_genus_sites2.png", height = 8, width = 12, dpi = 300)
sites_map2+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
dev.off()

#plot with contrast of WQP and state data
site_coords_new<-subset(site_coords1, Provider_name!="STORET")

# This plot works and looks good- need to fix axis labels
sites_map<-ggplot(states, aes(x=long,y=lat)) 
sites_map2<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = genus.occurrence, aes(x = Longitude_transformed, y = Latitude_transformed), color = 'blue', alpha = 1, shape=20)+geom_point(data = all.occurrence, aes(x = Longitude_transformed, y = Latitude_transformed), color = 'black', alpha = 0.5, shape=20)+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/all_sites1.pdf", sites_map2, height = 8, width = 12, dpi = 300)
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/all_sites1.png",sites_map2, height = 8, width = 12, dpi = 300)

################## plot with WQP data only ############################################################################
site_coords2<-subset(site_coords1, Provider_name=="STORET")

wqp_map2<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = site_coords2, aes(x = Longitude_transformed, y = Latitude_transformed), color = 'black', alpha = 1, shape=20)+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/wqp_sites1.pdf", wqp_map2, height = 8, width = 12, dpi = 300)
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/wqp_sites1.png", wqp_map2, height = 8, width = 12, dpi = 300)

########################## Record exploration ##########################################################################################

#subset to just records with insect genera- there are ~1.89 million records of insect genera in streams
genus.occurrence<-subset(insect.occurrence, !is.na(Genus))

#get all unique locations for all insect taxa - 51,892 unique locations with insect records
lat_long_all<-unique(insect.occurrence[c("Latitude", "Longitude")]) 

#get all unique stream locations for just insect genera - 46,568 unique locations with insect genera
lat_long_genera<-unique(genus.occurrence[c("Latitude", "Longitude")])

occurrencewqp<-subset(insect.occurrence, Provider_name=="STORET") #857,538 insect records in WQP data

occurrencewqp.genera<-subset(genus.occurrence, Provider_name=="STORET") #676,722 insect genus stream records in WQP data

lat_long_wqp<-unique(occurrencewqp[c("Latitude", "Longitude")]) #22,781 insect locations

lat_long_wqp_genera<-unique(occurrencewqp.genera[c("Latitude", "Longitude")]) #17,503 insect genera stream locations

ca_occurrence<-unique(subset(genus.occurrence, Provider_name=="California State Water Resources Control Board")) #203,345 for CA from state- need to also count records from WQP

lat_long_ca_genera<-unique(ca_occurrence[c("Latitude", "Longitude")]) #4,419 unique stream locations for CA- again, only from state

############################ Map of raw genus richness ##########################################################################################
#where is genus_occurrence object? need to recover code from previous version of R script

#color-code richness
#use genus_occurrence df
genus.rich<-genus.occurrence%>%
  filter(Latitude_transformed<4000000 & Longitude_transformed<2000000)%>%
  group_by(Longitude_transformed, Latitude_transformed)%>%
  summarise(genus_richness=length(unique(Genus)))

richness_map<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = genus.rich, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=genus_richness), shape=20)+scale_colour_gradient(name="Genus richness",low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 25), legend.title = element_text(size=30, face="bold"))
richness_map+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/Genus_site_richness.png", height = 8, width = 16, dpi = 300)
richness_map+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
dev.off()

#genus richness by state- choose California
unique(genus.occurrence$Provider_name)
cal.rich<-genus.occurrence%>%
  filter(Provider_name=="California State Water Resources Control Board")%>%
  group_by(Longitude_transformed, Latitude_transformed)%>%
  summarise(genus_richness=length(unique(Genus)))

library(ggsn)
ca<-subset(states, region=="california")
ca_map<-ggplot(ca, aes(x=long,y=lat)) 
ca_richness<-ca_map+geom_path(color="black",aes(group=group))+geom_point(data = cal.rich, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=genus_richness), shape=20)+scale_colour_gradient(name="Genus richness",low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 25), legend.title = element_text(size=30, face="bold"))
ca_richness+ggsn::scalebar(ca, dist = 100, dist_unit="km",st.size=3, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(ca, scale=0.1,symbol=1,location="topright")

ggsave('~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/CA_genus_richness.png', width=8, height=6)
ca_richness+ggsn::scalebar(ca, dist = 100, dist_unit="km",st.size=3, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(ca, scale=0.1,symbol=1,location="topright")
dev.off()


insects<-genus.occurrence%>%filter(Latitude_transformed<4000000 & Longitude_transformed<2000000)%>%
  select(Accepted_name, Longitude_transformed, Latitude_transformed)%>%
  mutate(Site_ID = group_indices(., Longitude_transformed, Latitude_transformed))
#need to assign a unique code to each lat_long combination

################### Map distribution of traits using imputed data first ##############################################################################################################################
#read- in imputed mode trait data
trait_mode<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/imputed_trait_genus_mode.csv", stringsAsFactors = FALSE)
#change to long format
library(reshape2)
trait_mode.melt<-melt(trait_mode[,-1], id="Genus", na.rm=TRUE)

insect_traits<-merge(insects, trait_mode, by.x="Accepted_name", by.y="Genus")

insect_cool<-insect_traits%>%filter(Thermal_pref=="Cold-cool eurythermal (0-15 C)")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_cool=length(unique(Accepted_name)))
insect_cold<-insect_traits%>%filter(Thermal_pref=="Cold stenothermal (<5 C)")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_cool=length(unique(Accepted_name)))
insect_warm<-insect_traits%>%filter(Thermal_pref=="Warm eurythermal (15-30 C)")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_cool=length(unique(Accepted_name)))

#map warm eurythermal insect richness
richness_warm<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_warm, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_cool), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_warm

#try just in California
insects_ca<-genus.occurrence%>%filter(Latitude_transformed<4000000 & Longitude_transformed<2000000 &Provider_name=="California State Water Resources Control Board")%>%
  select(Accepted_name, Longitude_transformed, Latitude_transformed)%>%
  mutate(Site_ID = group_indices(., Longitude_transformed, Latitude_transformed))

ca_traits<-merge(insects_ca, trait_mode, by.x="Accepted_name", by.y="Genus")
insect_warm_ca<-ca_traits%>%filter(Thermal_pref=="Warm eurythermal (15-30 C)")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_cool=length(unique(Accepted_name)))
insect_cool_ca<-ca_traits%>%filter(Thermal_pref=="Cold-cool eurythermal (0-15 C)")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_cool=length(unique(Accepted_name)))


richness_warm_ca<-ca_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_warm_ca, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_cool), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_warm_ca

richness_cool_ca<-ca_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_cool_ca, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_cool), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_cool_ca

#cold stenothermal insect richness
richness_cold<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_cold, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_cool), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_cold

#cool eurythermal insect richness
richness_cool<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_cool, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_cool), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_cool

#insects with preference for erosional habitat
insect_erosional<-insect_traits%>%filter(Rheophily_abbrev=="eros")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_cool=length(unique(Accepted_name)))
insect_depositional<-insect_traits%>%filter(Rheophily_abbrev=="depo")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_cool=length(unique(Accepted_name)))

#map erosional obligates
richness_eros<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_erosional, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_cool), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_eros

#map depositional obligates
richness_depo<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_depositional, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_cool), shape=20)+scale_colour_gradient(low = "lightblue", high = "darkred")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_depo

#FFG richness
insect_ffg<-insect_traits%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_ffg=length(unique(Feed_prim_abbrev)))

richness_ffg<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_ffg, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_ffg), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_ffg


#voltinism
insect_semi<-insect_traits%>%filter(Voltinism_abbrev=="Semivoltine")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_genus=length(unique(Accepted_name)))
insect_multi<-insect_traits%>%filter(Voltinism_abbrev=="Bi_multivoltine")%>%group_by(Longitude_transformed, Latitude_transformed)%>%summarise(n_genus=length(unique(Accepted_name)))


richness_semi<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_semi, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_genus), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_semi


richness_multi<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = insect_multi, aes(x = Longitude_transformed, y = Latitude_transformed,  colour=n_genus), shape=20)+scale_colour_gradient(low = "lightblue", high = "red")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_multi

##################### Map proportions with unimputed data #################################################################################################
#read- in unimputed mode trait data
trait_mode_raw<-read.csv("~/Documents/WaterCube/Ch.3/aquatic_insects/raw_trait_genus_mode.csv", stringsAsFactors = FALSE)
traits_mode_raw2<-subset(trait_mode_raw,select=c("Genus", "AdultFlyingStrength_abbrev", "Emerge_season_1","Emerge_season_2" ,"Emerge_synch_abbrev","Feed_mode_sec","Feed_prim_abbrev" ,"Female_disp_abbrev","Habit_prim","Habit_sec","Max_body_size_abbrev","Resp_abbrev","Rheophily_abbrev", "Thermal_pref","Voltinism_abbrev"))

#change to long format
library(reshape2)
trait_mode.melt_raw<-melt(traits_mode_raw2, id="Genus", na.rm=TRUE)

#proportion of genera in each voltinism category- still haven't figured this out
insect_traits_raw<-merge(insects, trait_mode.melt_raw, by.x="Accepted_name", by.y="Genus")

#this worked for thermal preference
traits_percent <- insect_traits_raw %>%
  group_by(Site_ID, variable, value)%>%
  mutate(n_genus = length(unique(Accepted_name)))%>%
  ungroup()%>%
  group_by(Site_ID, variable)%>%
  mutate(n_genus_all_values = length(unique(Accepted_name)))%>%
  mutate(percent=n_genus/n_genus_all_values) %>%
  distinct(Latitude_transformed, Longitude_transformed, variable, value, percent, .keep_all = TRUE)

#plot proportion of cool eurythermal insects
richness_cool<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="Cold-cool eurythermal (0-15 C)"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(name=expression(paste("Proportion \n cool eurythermal (0-15 °C)")),low = "wheat", high = "purple")+
theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                 panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                 panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 20), legend.title = element_text(size=25, face="bold"))
richness_cool+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/proportion_cool_eurythermal.png", height = 8, width = 16, dpi = 300)
richness_cool+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
dev.off()

richness_cool

#plot proportion of cold stentohermal insects
richness_cold<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="Cold stenothermal (<5 C)"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_cold

#plot proportion of warm eurythermal insects
richness_therm<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="Warm eurythermal (15-30 C)"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(name=expression(paste("Proportion \n warm eurythermal (15-30 °C)")),low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2), legend.text = element_text(size = 20), legend.title = element_text(size=25, face="bold"))
richness_therm+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
ggsave("~/Documents/WaterCube/Ch.3/aquatic_insects/Figures/proportion_warm_eurythermal.png", height = 8, width = 16, dpi = 300)
richness_therm+ggsn::scalebar(states, dist = 500, dist_unit="km",st.size=5, height=0.01,transform =FALSE, location="bottomleft")+ggsn::north(states, scale=0.1,symbol=1,location="bottomright")
dev.off()
richness_therm





#plot proportion of multivoltine insects
richness_volt<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="Bi_multivoltine"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_volt

#plot proportion of semivoltine insects
richness_volt_semi<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="Semivoltine"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_volt_semi

#plot proportion of erosional insects
richness_eros<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="eros"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_eros

#plot proportion of gilled insects
richness_gills<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="Gills"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_gills


################### Plot proportion of genera by site with different traits ##########################################################################
#proportion of genera in each trait
insect_traits2<-merge(insects, trait_mode.melt, by.x="Accepted_name", by.y="Genus")

#this worked for thermal preference
traits_percent_impute <- insect_traits2 %>%
  group_by(Site_ID, variable, value)%>%
  mutate(n_genus = length(unique(Accepted_name)))%>%
  ungroup()%>%
  group_by(Site_ID, variable)%>%
  mutate(n_genus_all_values = length(unique(Accepted_name)))%>%
  mutate(percent=n_genus/n_genus_all_values) %>%
  distinct(Latitude_transformed, Longitude_transformed, variable, value, percent, .keep_all = TRUE)

#plot proportion of warm eurythermal insects
richness_therm_impute<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent_impute, value=="Warm eurythermal (15-30 C)"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_therm_impute

#plot proportion of multivoltine insects
richness_volt_impute<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent_impute, value=="Bi_multivoltine"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_volt_impute

#plot proportion of erosional insects
richness_eros<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="depo"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_eros

#plot proportion of gilled insects
richness_gills<-sites_map+geom_path(color="black",aes(group=group))+geom_point(data = subset(traits_percent, value=="Gills"), aes(x = Longitude_transformed, y = Latitude_transformed,  colour=percent), shape=20)+scale_colour_gradient(low = "wheat", high = "purple")+
  theme_bw()+theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   panel.border = element_rect(colour = "black", size=2))
richness_gills


save.image("WQP_state_merge.RData")
  