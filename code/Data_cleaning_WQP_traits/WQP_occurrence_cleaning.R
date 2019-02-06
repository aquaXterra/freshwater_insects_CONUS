###WQP Insect Occurrence Records Cleaning File 
##Author: Laura Twardochleb

##This script combines and explores all "Benthic Macroinvertebrate" and "Invertebrate" data from WQP for the continental US
##This script contains code for initial data cleaning up to removing records with geo-referencing errors
##This script takes cleaned WQP data and assigns taxonomic names for each taxonomic level- from species to order to WQP data by creating a separate column for each taxonomic level
##A dataset that has removed records with geo-referencing errors or missing datum type "WQP_invertebrate_1979_2016_2.csv"

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

setwd("~/Documents/WaterCube/Aquaxterra_code_data/aquatic_insects-master/code/Data_cleaning_WQP_traits")

load(file='WQP_cleaning.RData')
#####Read in all data############################################
#this is the data from the "benthic macroinvertebrate" assemblage search
biodata<-read.csv("macroinvertebrate_biological.csv", header=TRUE) #read in biological count data
sitedata<-read.csv("macroinvertebrate_site_data.csv", header=TRUE) #read in site data that includes lat and lon for each site

#this is the data from the "invertebrate" assemblage search
invert.biodata<-read.csv("invertebrate_biological.csv", header=TRUE)
invert.sitedata<-read.csv("invertebrate_site.csv", header=TRUE)

#before merging the datasets, need to remove tissue sample records (samples for chemical pollutants) from invertebrate assemblage dataset
unique(invert.biodata$BiologicalIntentName)
invert.biodata1<-subset(invert.biodata, (BiologicalIntentName == "Population Census") | (BiologicalIntentName == "Species Density"))

#####Merge WQP databases#########################################
#merge the site-level and biological invertebrate sample data
invertdata<-merge(invert.biodata1, invert.sitedata, by="MonitoringLocationIdentifier", all=TRUE)
macroinvertdata<-merge(biodata, sitedata, by="MonitoringLocationIdentifier", all=TRUE)

#merge the two datasets
wqp<-rbind(invertdata, macroinvertdata) #how many initial records?

######Remove marine samples##############################################################################
#next remove marine samples- estuaries, oceans, superfund sites, other types can be removed later, e.g., springs, land; also remove samples with NAs in the location type
names(wqp)
unique(wqp$MonitoringLocationTypeName)
wqp1<-subset(wqp, (MonitoringLocationTypeName != "<NA>") & (MonitoringLocationTypeName !="Facility Municipal Sewage (POTW)")&(MonitoringLocationTypeName !="Storm Sewer")&(MonitoringLocationTypeName !="Ocean")&(MonitoringLocationTypeName !="Estuary")&(MonitoringLocationTypeName !="Facility Industrial")&(MonitoringLocationTypeName !="Facility Public Water Supply (PWS)")&(MonitoringLocationTypeName !="CERCLA Superfund Site")&(MonitoringLocationTypeName !="Facility Other")&(MonitoringLocationTypeName !="Canal Irrigation")&(MonitoringLocationTypeName !="Waste Sewer")&(MonitoringLocationTypeName !="Land Runoff")&(MonitoringLocationTypeName !="Canal Drainage")&(MonitoringLocationTypeName !="Canal Transport")&(MonitoringLocationTypeName !="Other-Surface Water")&(MonitoringLocationTypeName !="Land"))
unique(wqp1$MonitoringLocationTypeName)

#######Remove non-count or presence/absence records######################################################
#next remove all tissue samples- or any samples that are not population census, population density
unique(wqp1$BiologicalIntentName)
#examine what is in each of these categories
wqptoxicity<-subset(wqp1, BiologicalIntentName == "Toxicity") #this is count data on aquatic inverts- keep in database
wqpfreq<-subset(wqp1, BiologicalIntentName == "Frequency Class")  #this is invertebrate count data- keep in database
wqptissue<-subset(wqp1, BiologicalIntentName == "Targeted Sampling") #this is presence/absence data- need to remove records that have 'N' in ResultMeasureValue- subset by this next
wqpIndividual<-subset(wqp1, BiologicalIntentName == "Individual") #this is invertebrate count data- keep in database
wqpsummary<-subset(wqp1, BiologicalIntentName == "Group Summary")#some are counts and some are lengths and weights- keep in database, after this, will need to check CharacteristicName and ResultMeasure.MeasureUnitCode
wqptiss<-subset(wqp1, BiologicalIntentName == "Tissue") #this is tissue samples from unidentified invertebrates- remove from database
#now remove BiologicalIntentName="Tissue"
wqp2<-subset(wqp1, (BiologicalIntentName != "Tissue"))
#now remove records that have 'N' in ResultMeasureValue or NA
wqp3<-subset(wqp2, (ResultMeasureValue != "N"))
wqp4<-subset(wqp3, (ResultMeasureValue != "NA"))

####################Remove columns that only contain NAs#################################################
wqp5<-wqp4[, colSums(is.na(wqp4)) != nrow(wqp4)] #how many records in WQP5?

######Remove records with NA in lat or long##############################################################
wqp6<-subset(wqp5, wqp5$LatitudeMeasure != "NA")
wqp7<-subset(wqp6, wqp6$LongitudeMeasure != "NA")
dim(wqp7) #2,492,283 occurence records 
names(wqp7)
unique(wqp7$MonitoringLocationIdentifier) # there are 35,428 sampling locations

#Still need to create columns for each type of taxonomic group and remove non-target insect orders. Need to subset by streams/rivers for subsequent analyses
#Possibly need to remove records that have errors in the geodatum- some are coded as 'UNKWN' for the Vertical and Horizontal coordinate reference system datum name

#Exploring data some more
unique(wqp7$MonitoringLocationTypeName)
unique(wqp7$HorizontalCoordinateReferenceSystemDatumName)
unique(wqp7$VerticalCoordinateReferenceSystemDatumName)

#One type of vertical reference is given as SEALV- trying to determine what this datum is 
sealv<-subset(wqp7, VerticalCoordinateReferenceSystemDatumName=="SEALV")
names(sealv)
unique(sealv$OrganizationFormalName.x)
#SEALV is probably mean sea level, likely equivalent to NGVD29

#One type of horizontal reference is given as OTHER- trying to determine what this datum is 
other<-subset(wqp7, HorizontalCoordinateReferenceSystemDatumName=="OTHER")
unknown<-subset(wqp7, HorizontalCoordinateReferenceSystemDatumName=="UNKWN")

###############Dropping all records that do not have horizontal datum defined########################
wqp8<-subset(wqp7, (wqp7$HorizontalCoordinateReferenceSystemDatumName != "OTHER") & (wqp7$HorizontalCoordinateReferenceSystemDatumName !="UNKWN"))
unique(wqp8$HorizontalCoordinateReferenceSystemDatumName) #how many records remain?
write.csv(wqp8, "WQP_invertebrate_1979_2016_2.csv") #comprehensive WQP insect occurrence dataset without geo-referencing errors

###########Assign taxonomic names for each taxonomic level- from species to order to WQP data by creating a separate column for each taxonomic level#####
#####read in the data####
wqp_insect<-read.csv("WQP_invertebrate_1979_2016_2.csv")

#####create a list of unique taxa and get records using 'taxize' library######
utaxon<-unique(wqp_insect$SubjectTaxonomicName)

#install 'taxize' library to extract taxonomic names
install.packages('taxize')
install.packages('jsonlite')
install.packages('data.table')
library(taxize) #can use this package to query phylogenetic databases

#now practice extracting higher taxonomic names for a single species using the ITIS database
classification("Enallagma boreale", db='itis') #that works, now need to extract it for all in the list of WQP taxa

###############extract higher taxonomic names for all taxa from WQP#############################################
specieslist<-classification(utaxon, db='itis')
class(specieslist)
class(specieslist$`Rhyacophila pellisa`)

specieslist$`Rhyacophila pellisa`$rank
specieslist$`Rhyacophila pellisa`$name
specieslist$`Cricotopus (Cricotopus)`

test<-rbind(specieslist$`Rhyacophila pellisa`$name, specieslist$`Cricotopus (Cricotopus)`$name)
#need to rbind all of these rankings and then rename column headers with the rank. Some taxa go to a lower taxonomic rank- e.g., some go to genus and some to species. 
#will need to replace species rankings with 'NA' for taxa that are not ID'd to species in WQP. After we have these, we add these columns for each taxa in the 'utaxon' list in WQP.
#after assigning rankings, we can extract a list of all taxa with species or genus level records in WQP, and download the GBIF data for all of these taxa.

ranks <- do.call("rbind", lapply(specieslist, "[[", 1)) #this extracts all of the "name" elements from each list in specieslist and rbinds them to result in a matrix with one column for
#taxonomic name in WQP, and then a column for each higher rank from ITIS. The resultant matrix can be combined with WQP data
##do.call 'rbinds' or applies any other 'function' separately over each element within the list 

##########Create a 'lookup' table that has the species name from WQP and the taxonomic name for each rank#############################
##########this extracts the names of lists, the first element- 'names', and the second element- 'ranks' from the list of lists and binds them into a dataframe
id<-names(specieslist)
names<-lapply(specieslist, '[[', 1)
ranksnew1<-lapply(specieslist, '[', 2)
ranksnew1
names(ranksnew1) <- NULL
rank.test<-do.call(rbind, Map(data.frame, rank=ranksnew1, name=names, id=id)) #this is sort of what I want, except reshaped so taht each column is a separate rank
#rename id to SubjectTaxonomicName
library(plyr)
rank.test3<-rename(rank.test, c("id"="SubjectTaxonomicName"))
#writing this rank table to a csv file
write.csv(rank.test3, file="taxonomic_ranks.csv")

###########Now, I need to reshape the rank.test3 database from long to wide format, creating a separate column for each taxonomic rank, before I 
#can combine this database with the WQP portal- use 'dcast' function for casting data frames- 'melt' function transforms data frame from wide to long format
library(reshape2)
?dcast
#rank is variable to swing into column names and name is the value, SubjectTaxonomicName is what we want to have its own row
rank.test4<-dcast(rank.test3, SubjectTaxonomicName ~ rank, value.var="name") #this worked! Now can merge this table with the WQP databse

#################Merge taxonomic rank lookup table with WQP database#############################################################
wqp_insect2<-merge(wqp_insect, rank.test4, by="SubjectTaxonomicName", all=TRUE) #need to merge by more than one criterion?
names(wqp_insect2)
wqp_insect2$class

#write resultant database to csv file
write.csv(wqp_insect2, file="WQP_insect_w_ranks.csv")


################Subset records to insects only#####################
insects<-read.csv("WQP_insect_w_ranks.csv", stringsAsFactors = FALSE)

insects2<-subset(insects, class=="Insecta")

#remove unneeded variables
insects3<-insects2[,c(3:131)] #there are 1.9 million insect records
names(insects3)

write.csv(insects3, "insects2001v3.csv") #this is the database subset without extra columns with taxonomic information

###############Subset records to 2001 to present#####################
insects4<-subset(insects3, ActivityStartDate>="2001-01-01") #1.2 million insect records from 2001 to present


save.image(file="WQP_cleaning.RData")
