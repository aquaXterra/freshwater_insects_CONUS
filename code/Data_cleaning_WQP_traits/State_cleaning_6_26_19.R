######################## Data cleaning of state data #########################
##Author: Laura Twardochleb- need to revise this script to remove changes to sampling method-needs to match what is in WQP_state_merge
##4/18/19

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

setwd("~/Documents/WaterCube/Ch.3/aquatic_insects/")

#Load workspace image
load(file='~/Documents/WaterCube/Ch.3/aquatic_insects/state_cleaning.RData')

#Load packages
library('tidyr')
library('dplyr')
library('lubridate')
############## read-in datasets #############################################################################################

#read in state datasets
dataFiles <- lapply(Sys.glob("~/Documents/WaterCube/Ch.3/state_data/csv_files/*.csv"), read.csv, stringsAsFactors=FALSE)
az<-dataFiles[[1]]
ar.sites<-dataFiles[[2]]
ar.taxa<-dataFiles[[3]]
ca<-dataFiles[[4]]
fl.taxa<-dataFiles[[5]]
fl.sites<-dataFiles[[6]]
fl.sites2<-dataFiles[[7]]
ia.taxa<-dataFiles[[8]]
ia.sites<-dataFiles[[9]]
il.sites<-dataFiles[[10]]
id.taxa<-dataFiles[[11]]
id.sites<-dataFiles[[12]]
mn.taxa1<-dataFiles[[13]]
mn.taxa2<-dataFiles[[14]]
mn.sites<-dataFiles[[15]]
mo.taxa1<-dataFiles[[16]]
mo.taxa2<-dataFiles[[17]]
mo.sites<-dataFiles[[18]]
nb.taxa<-dataFiles[[19]]
nb.taxa2<-dataFiles[[20]]
nb.sites<-dataFiles[[21]]
nv.taxa1<-dataFiles[[22]]
nv.taxa2<-dataFiles[[23]]
nv.taxa3<-dataFiles[[24]]
nv.taxa4<-dataFiles[[25]]
nv.taxa5<-dataFiles[[26]]
nv.taxa6<-dataFiles[[27]]
nv.taxa7<-dataFiles[[28]]
nv.sites<-dataFiles[[29]]
nm<-dataFiles[[30]]
or1<-dataFiles[[31]]
or2<-dataFiles[[32]]
pa<-dataFiles[[33]]
tn<-dataFiles[[34]]
tx.taxa1<-dataFiles[[35]]
tx.taxa2<-dataFiles[[36]]
tx.taxa3<-dataFiles[[37]]
tx.taxa4<-dataFiles[[38]]
tx.taxa5<-dataFiles[[39]]
tx.taxa6<-dataFiles[[40]]
tx.params1<-dataFiles[[41]]
tx.params2<-dataFiles[[42]]
tx.params3<-dataFiles[[43]]
tx.params4<-dataFiles[[44]]
tx.params5<-dataFiles[[45]]
tx.params6<-dataFiles[[46]]
tx.sites<-dataFiles[[47]]
va.taxa<-dataFiles[[48]]
va.sites<-dataFiles[[49]]
wi.taxa<-dataFiles[[50]]
wi.sites<-dataFiles[[51]]
wa.taxa<-dataFiles[[52]]
wa.sites<-dataFiles[[53]]
wv.taxa<-dataFiles[[54]]
wv.sites<-dataFiles[[55]]

############To-do list#######################################################################################################################################
#for each state:
#set up raw commmunity table with Location_ID, Sample_ID (from state agencies), Date, Submitted_name, Latitude, Longitude, State, Sample_method, Location_description (textual description of waterbody), note projection

############ Cleaning/create raw data table for each state ####################################################################################################
################################################# Arizona ######################################################################################################################
#NAD83
head(az)
az.sites<-unique(az[c("LATITUDE_SECOND", "LONGTITUDE_DEGREE")])

az1<-subset(az, select=c("STATION_RID", "ACTIVITY_END_DATE", "LATITUDE_MEASURE", "LONGITUDE_MEASURE", "STATION_DESC", "TAXA_NAME", "COLLECTION_METHOD_NAME", "SAMPLE_NO"))
#rename columns
colnames(az1)<-c("Location_ID", "Date", "Latitude", "Longitude", "Location_description", "Submitted_name", "Sample_method", "Sample_ID")
#creat column for state
az1$Provider_name<-"Arizona Department of Environmental Quality"
#change column order
az2<-az1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description")]
az2$Datum<-"NAD83"
#check taxa names
unique(az2$Submitted_name)

################################################ Arkansas #############################################################################################################
#NAD83
#merge site and taxa datasets
head(ar.sites)
head(ar.taxa)
ar<-merge(ar.sites, ar.taxa, by="StationID")
#subset columns
ar1<-subset(ar, select=c("StationID", "StartDateFormatted", "StreamName", "Lat", "Long", "CollMeth", "FinalID"))
#rename columns
colnames(ar1)<-c("Location_ID", "Date", "Location_description", "Latitude", "Longitude", "Sample_method", "Submitted_name")
#initialize state and sample id columns
ar1$Provider_name<-"Arkansas Department of Environmental Quality"
ar1$Sample_ID<-NA
#reorder columns
ar2<-ar1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description")]
ar2$Datum<-"NAD83"
unique(ar2$Submitted_name)

unique(ar2$Date) #records go up to only 2013, requested additional records up to 2018

#read-in new datasets
new_taxa1<-read.csv("~/Documents/WaterCube/Ch.3/state_data/Arkansas/2016_ar_taxa.csv", stringsAsFactors = FALSE)
new_taxa2<-read.csv("~/Documents/WaterCube/Ch.3/state_data/Arkansas/2017_ar_taxa.csv", stringsAsFactors = FALSE)
new_taxa3<-read.csv("~/Documents/WaterCube/Ch.3/state_data/Arkansas/2017_2018_ar_taxa.csv", stringsAsFactors = FALSE)
new_sites<-read.csv("~/Documents/WaterCube/Ch.3/state_data/Arkansas/ar_Site_Data.csv", stringsAsFactors = FALSE)
names(new_taxa1)<-c("SAMPLE_ID","RIFFLE","DATE_COL","SAMPTYPE", "LAB_NAME","LAB_ID","TAXONOMIST","LAB_TAXON","TAXON_NAME","LIFE.STAGE","ABUNDANCE","IMMATURE", "INDETERMINATE","CONDITION","LR_TAXA","DISTINCT" ,"AGGREGATED","LAB_COM", "KINGDOM", "PHYLUM", "SUBPHYLUM" ,"CLASS","SUBCLASS","INFRACLASS","SUPERORDER","ORDER","SUBORDER","INFRAORDER","SUPERFAMILY","FAMILY","SUBFAMILY","TRIBE","SUBTRIBE","GENUS","SUBGENUS","SPECIES","SUBSPECIES","ADDITIONS","SERIAL")
names(new_taxa2)<-c("SAMPLE_ID","RIFFLE","DATE_COL","SAMPTYPE", "LAB_NAME","LAB_ID","TAXONOMIST","LAB_TAXON","TAXON_NAME","LIFE.STAGE","ABUNDANCE","IMMATURE", "INDETERMINATE","CONDITION","LR_TAXA","DISTINCT" ,"AGGREGATED","LAB_COM", "KINGDOM", "PHYLUM", "SUBPHYLUM" ,"CLASS","SUBCLASS","INFRACLASS","SUPERORDER","ORDER","SUBORDER","INFRAORDER","SUPERFAMILY","FAMILY","SUBFAMILY","TRIBE","SUBTRIBE","GENUS","SUBGENUS","SPECIES","SUBSPECIES","ADDITIONS","SERIAL")
names(new_taxa3)<-c("SAMPLE_ID","RIFFLE","DATE_COL","SAMPTYPE", "LAB_NAME","LAB_ID","TAXONOMIST","LAB_TAXON","TAXON_NAME","LIFE.STAGE","ABUNDANCE","IMMATURE", "INDETERMINATE","CONDITION","LR_TAXA","DISTINCT" ,"AGGREGATED","LAB_COM", "KINGDOM", "PHYLUM", "SUBPHYLUM" ,"CLASS","SUBCLASS","INFRACLASS","SUPERORDER","ORDER","SUBORDER","INFRAORDER","SUPERFAMILY","FAMILY","SUBFAMILY","TRIBE","SUBTRIBE","GENUS","SUBGENUS","SPECIES","SUBSPECIES","ADDITIONS","SERIAL")

new_taxa<-rbind(new_taxa1, new_taxa2, new_taxa3)
new<-merge(new_taxa, new_sites, by.x="SAMPLE_ID", by.y="WQMS")
new1<-subset(new, select=c("SAMPLE_ID", "DATE_COL", "TAXON_NAME", "LAB_ID", "Stream.Name", "Lat", "Long"))
names(new1)<-c("Location_ID", "Date", "Submitted_name", "Sample_ID", "Location_description", "Latitude", "Longitude")
new1$Provider_name<-"Arkansas Department of Environmental Quality"
new1$Datum<-"NAD83"
new1$Sample_method<-NA
new2<-new1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude",  "Provider_name", "Sample_method", "Location_description", "Datum")]

#merge two datasets
ar_master<-rbind(ar2, new2)

############################################### California #############################################################################################################
#WGS84
head(ca)
#subset to insect taxa
ca1<-subset(ca, Class=="Insecta")
ca2<-subset(ca1, select=c("StationCode", "TargetLatitude", "TargetLongitude", "SampleDate", "CollectionMethodName", "FinalID", "waterbody_type", "LabSampleID"))
colnames(ca2)<-c("Location_ID", "Latitude", "Longitude", "Date", "Sample_method", "Submitted_name", "Location_description", "Sample_ID")
ca2$Provider_name<-"California State Water Resources Control Board"
ca3<-ca2[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description")]
ca3$Datum<-"WGS84"
ca.sites<-unique(ca[c("TargetLatitude", "TargetLongitude")])
unique(ca3$Submitted_name)
unique(ca3$Location_description)

################################################ Florida ##############################################################################################################
#NAD83
head(fl.taxa)
head(fl.sites) 
head(fl.sites2)
names(fl.sites)<-names(fl.sites2)
fl_sites<-rbind(fl.sites2, fl.sites)

fl<-merge(fl_sites, fl.taxa, by=intersect(names(fl_sites), names(fl.taxa)))
names(fl)
fl2<-subset(fl, select=c("STANICKNAME", "SAMPLE_ID","SAMPLE_DATE", "TAXON_NAME", "LATITUDE", "LONGITUDE", "GEAR",  "STADESC"))
names(fl2)<-c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Sample_method", "Location_description")
fl2$Provider_name<-"Florida Department of Environmental Protection"
fl3<-fl2[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description")]
fl3$Datum<-"NAD83"

############################################## Iowa ###############################################################################################
#WGS84
head(ia.taxa)
#subset to insect taxa
ia.taxa1<-subset(ia.taxa, Class=="Insecta")
head(ia.sites)
ia<-merge(ia.sites, ia.taxa1, by=intersect(names(ia.sites), names(ia.taxa)))
ia2<-subset(ia, select=c("SiteID", "SessionID", "SampleDate", "FinalID", "LatDD", "LongDD", "BugGearShort", "StreamName"))
ia2$Provider_name<-"Iowa Department of Natural Resources"
names(ia2)<-c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Sample_method", "Location_description", "Provider_name")
ia3<-ia2[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description")]
ia3$Datum<-"WGS84"

######################################### Idaho #################################################################################################################
#NAD83
head(id.sites)
head(id.taxa)
names(id.taxa)
names(id.sites)
#these match
unique(id.sites$BURPID)
unique(id.taxa$MLOC_ID)
colnames(id.sites)[4]<-"MLOC_ID"

id<-merge(id.sites, id.taxa, by="MLOC_ID")
#Drop records with NA for year- these are before 2001
id2<-subset(id, !is.na(id$YEAR))
id3<-subset(id2, select=c("MLOC_ID", "YEAR", "TAX_NAME", "LATITUDE", "LONGITUDE", "ACT_ID", "STREAM", "DATUM"))
id3$Provider_name<-"Idaho Department of Environmental Quality"
id3$Sample_ID<-NA
names(id3)<-c("Location_ID", "Date", "Submitted_name", "Latitude", "Longitude",  "Sample_method", "Location_description", "Datum", "Provider_name", "Sample_ID")
id4<-id3[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

########################################### Illinois- need bug data #############################################################################################

########################################### Minnesota ###########################################################################################################
#NAD83
#Protocol column is sampling method
head(mn.sites)
head(mn.taxa1)
head(mn.taxa2)
mn.taxa<-merge(mn.taxa1, mn.taxa2, by=intersect(names(mn.taxa1), names(mn.taxa2)), all =TRUE)
mn<-merge(mn.sites, mn.taxa, by="FieldNum")
mn2<-subset(mn, select=c("FieldNum", "VisitNum", "VisitDate","Name1", "LAT8xDD.x", "LON8xDD.x", "WBName.x"))
mn2$Provider_name<-"Minnesota Pollution Control Agency"
mn2$Sample_method<-"QMH"
mn2$Datum<-"NAD83"
names(mn2)<-c("Location_ID","Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude",   "Location_description",  "Provider_name", "Sample_method", "Datum")
mn3<-mn2[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

########################################### Mississippi #########################################################################################################
#NAD83
MS<-read.csv("~/Documents/WaterCube/Ch.3/state_data/Mississippi/MS_taxa.csv", stringsAsFactors = FALSE)
#subset to insect taxa
MS1<-subset(MS, Class=="Insecta")
ms1<-subset(MS1, select=c("StationID", "WaterbodyName", "Lat_Dec", "Long_Dec", "BenSampID", "CollDate", "FinalID"))
names(ms1)<-c("Location_ID", "Location_description", "Latitude", "Longitude", "Sample_ID", "Date", "Submitted_name")
ms1$Datum<-NA
ms1$Provider_name<-"Mississippi Department of Environmental Quality"
ms1$Sample_method<-NA
ms2<-ms1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

########################################### Missouri ############################################################################################################
#NAD83
head(mo.sites)
head(mo.taxa1)
head(mo.taxa2)

mo.taxa<-merge(mo.taxa1, mo.taxa2, by=intersect(names(mo.taxa1), names(mo.taxa2)), all=TRUE)
unique(mo.sites$HorizontalReferenceDatumName) #NAD83
mo2<-subset(mo.taxa, select=c("Station", "SampleNumber", "DateTime",  "Waterbody", "Name2","FirstOfLatitudeMeasure", "FirstOfLongitudeMeasure", "SamplingRegime"))
mo2$Provider_name<-"Missouri Department of Natural Resources"
mo2$Datum<-"NAD83"
names(mo2)<-c("Location_ID", "Sample_ID", "Date","Location_description", "Submitted_name", "Latitude", "Longitude",  "Sample_method", "Provider_name", "Datum")
mo3<-mo2[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

########################################## Nebraska #######################################################################################################################
#NAD83
head(nb.sites)
head(nb.taxa)
head(nb.taxa2)

nb.taxa3<-merge(nb.taxa, nb.taxa2, by=intersect(names(nb.taxa), names(nb.taxa2)), all=TRUE)
colnames(nb.sites)[3]<-"Station.ID"
colnames(nb.sites)[7]<-"Sample.date"
nb<-merge(nb.taxa3, nb.sites, by=intersect(names(nb.taxa3), names(nb.sites)), all.x =TRUE)
unique(nb$Station.ID)

#read-in historical state data
nb_hist<-read.csv("~/Documents/WaterCube/Ch.3/state_data/Nebraska/nb_sites_historical.csv")
head(nb_hist)
unique(nb_hist$Monitoring.Location.ID)
colnames(nb_hist)[4]<-"Station.ID"
nb2<-merge(nb_hist, nb, by=intersect(names(nb_hist), names(nb)))
unique(nb2$WQX.Monitoring.Location.Type)
nb2[which(nb2$WQX.Monitoring.Location.Type != "River/Stream"),]
nb3<-subset(nb2, select=c("Station.ID", "Waterbody.Name", "Monitoring.Location.Latitude", "Monitoring.Location.Longitude", "Sample.date", "BenSampID", "Sample.gear", "Final.ID"))
names(nb3)<-c("Location_ID", "Location_description", "Latitude", "Longitude", "Date", "Sample_ID", "Sample_method", "Submitted_name")
nb3$Provider_name<-"Nebraska Department of Environmental Quality"
nb3$Datum<-"NAD83"
nb4<-nb3[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

############################### Nevada #################################################################################################################################################
#NAD83
head(nv.sites)
head(nv.taxa1)
head(nv.taxa2)
head(nv.taxa3)
head(nv.taxa4)
head(nv.taxa5)
head(nv.taxa6)
head(nv.taxa7)

#combine these taxa files, rearrange nv.taxa5 to be in long format for sites
nv.taxa1.5<-rbind(nv.taxa1, nv.taxa2)
nv.taxa3.5<-rbind(nv.taxa3, nv.taxa4)
nv.taxa6.5<-rbind(nv.taxa6, nv.taxa7)
unique(nv.taxa1.5$StationID)
unique(nv.taxa3.5$Station)
unique(nv.taxa6.5$StationID_2)
unique(nv.taxa6.5$StationID_3)
unique(nv.sites$StationID)

#rbind all taxa datasets together and then merge with sites dataset
colnames(nv.taxa3.5)[2]<-"StationID"
colnames(nv.taxa6.5)[3]<-"StationID"
colnames(nv.taxa1.5)[9]<-"CollDate"
nv.taxa6.5<-nv.taxa6.5[,-4]
colnames(nv.taxa3.5)[4]<-"X..Sub_Sampled"
nv.taxa7.5<-nv.taxa3.5[c("WaterbodyName", "StationID", "X..Sub_Sampled", "WAA.ID", "FinalID", "Individuals", "TotalInd","Species", "Tribe", "Class", "Phylum", "Order", "Family", "Genus", "Comments", "RefTaxa", "X.RefTaxaTaken", "CollDate" )]
nv.taxa8.5<-nv.taxa6.5[c("WaterbodyName", "StationID", "X..Sub_Sampled", "WAA.ID", "FinalID", "Individuals", "TotalInd","Species", "Tribe", "Class", "Phylum", "Order", "Family", "Genus", "Comments", "RefTaxa", "X.RefTaxaTaken", "CollDate" )]
nv.taxa<-rbind(nv.taxa8.5, nv.taxa7.5)

nv.taxa1<-subset(nv.taxa, select=c("WaterbodyName", "StationID", "WAA.ID", "FinalID", "CollDate"))

nv.taxa2<-subset(nv.taxa1.5, select=c("StationID", "ActivityID", "CollDate", "Characteristic", "Project_ID", "Site_Name",  "ActivityComments"))
names(nv.taxa2)<-c("ActivityID", "StationID", "CollDate", "FinalID", "Project_ID", "WaterbodyName", "Sample_method")
nv.taxa2$WAA.ID<-NA
nv.taxa1$ActivityID<-NA
nv.taxa1$Project_ID<-NA
nv.taxa1$Sample_method<-NA
nv.taxa2<-nv.taxa2[c("WaterbodyName", "StationID", "WAA.ID", "FinalID", "CollDate", "ActivityID", "Project_ID", "Sample_method")]
nv.taxa3<-rbind(nv.taxa1, nv.taxa2)

#combine nv.taxa3 dataset with nv.taxa5

#divide nv.taxa5 dataset by year- starting with 2001
taxa.2001<-nv.taxa5[-1,c(1,31)]
taxa.2001$CollDate<-"2001"

#tidy data for 2001
tidynv.2001<-taxa.2001%>%
  gather(key="StationID", value="Counts",-c("FinalID", "CollDate"))

#which columns contain 2002?
which(apply(nv.taxa5, 2, function(r) any(r %in% "2002")))
taxa.2002<-nv.taxa5[-1,c(1,32:52)]
taxa.2002$CollDate<-"2002"

#tidy data for 2001
tidynv.2002<-taxa.2002%>%
  gather(key="StationID", value="Counts", -c("FinalID", "CollDate"))

#which columns contain 2003?
taxa.2003<-nv.taxa5[-1,c(1, which(apply(nv.taxa5, 2, function(r) any(r %in% "2003"))))]
taxa.2003$CollDate<-"2003"

tidynv.2003<-taxa.2003%>%
  gather(key="StationID", value="Counts", -c("FinalID", "CollDate"))

#loop through 2004 to 2012
listofdfs<-list()
years<-c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012")
for(i in 1:length(years)){
  df<-as.data.frame(nv.taxa5[-1, c(1, which(apply(nv.taxa5, 2, function(r) any(r %in% years))))])
  listofdfs[[i]]<-df
}

#as a function
getyears<-function(years){
  listofdfs<-list()
  for(i in 1:length(years)){
    df<-as.data.frame(nv.taxa5[-1, c(1, which(apply(nv.taxa5, 2, function(r) any(r %in% years))))])
    df$CollDate<-years[[i]]
    listofdfs[[i]]<-df
  }
  return(listofdfs)
}
years<-c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012")
getyears(years=years)

#tidy each year's data frame
listoftidydfs<-list()
for(i in 1:length(listofdfs)){
  df<-listofdfs[[i]]%>%
    gather(key="StationID", value="Counts", -"FinalID")
  listoftidydfs[[i]]<-df
}
tidy2004<-listoftidydfs[[1]]
tidy2004$CollDate<-"2004"
tidy2005<-listoftidydfs[[2]]
tidy2005$CollDate<-"2005"
tidy2006<-listoftidydfs[[3]]
tidy2006$CollDate<-"2006"
tidy2007<-listoftidydfs[[4]]
tidy2007$CollDate<-"2007"
tidy2008<-listoftidydfs[[5]]
tidy2008$CollDate<-"2008"
tidy2009<-listoftidydfs[[6]]
tidy2009$CollDate<-"2009"
tidy2010<-listoftidydfs[[7]]
tidy2010$CollDate<-"2010"
tidy2011<-listoftidydfs[[8]]
tidy2011$CollDate<-"2011"
tidy2012<-listoftidydfs[[9]]
tidy2012$CollDate<-"2012"


#rbind the nv.taxa5 dataframes into one
nv.taxa5.tidy<-rbind(tidynv.2001, tidynv.2002, tidynv.2003, tidy2004, tidy2005, tidy2006, tidy2007, tidy2008, tidy2009, tidy2010, tidy2011, tidy2012)

#now remove all rows with a 0 in Counts
nv.tidy<-subset(nv.taxa5.tidy, Counts>=1)

#now combine with nv.taxa3
nv.tidy2<-nv.tidy[,-4]
nv.tidy2$WaterbodyName<-NA
nv.tidy2$WAA.ID<-NA
nv.tidy2$ActivityID<-NA
nv.tidy2$Project_ID<-NA
nv.tidy2$Sample_method<-NA
nv.tidy3<-nv.tidy2[c("WaterbodyName", "StationID", "WAA.ID", "FinalID", "CollDate", "ActivityID", "Project_ID", "Sample_method")]
nv.taxa.tidy<-rbind(nv.tidy3, nv.taxa3)

#some sites are coded with .1 and some with -1, wich is why the sites are not matching between datasets
nv.taxa.tidy$StationID<-gsub("[.]", "-", nv.taxa.tidy$StationID)
nv.sites$StationID<-gsub("[.]", "-", nv.sites$StationID)

#now remove trailing numbers from StationID in nv.taxa.tidy- need to use pattern matching in stringr
library(stringr)
#First, find all instances of an extra number appended to end of StationID and then remove
pattern<-".-.-.$"
# station<-as.factor(nv.taxa.tidy$StationID)
nv.taxa.tidy1<-nv.taxa.tidy$StationID%>% 
  str_extract(pattern)%>%
  str_replace("..$", "")

nv.taxa.tid2<-cbind(nv.taxa.tidy, nv.taxa.tidy1)
  
nv.taxa.tid2$StationID2<-ifelse(is.na(nv.taxa.tid2$nv.taxa.tidy1), nv.taxa.tid2$StationID, gsub("..$", "", nv.taxa.tid2$StationID))
#drop extra columns and rename StationID2 to StationID
nv.taxa.tidy2<-subset(nv.taxa.tid2, select=c("WaterbodyName", "WAA.ID", "FinalID", "CollDate", "Project_ID", "StationID2", "Sample_method"))
nv.taxa.tidy2$Datum<-"NAD83"
nv.taxa.tidy2$Provider_name<-"Nevada Department of Environmental Protection"
names(nv.taxa.tidy2)<-c("WaterbodyName", "WAA.ID", "FinalID", "CollDate", "Project_ID", "StationID",  "Sample_method","Datum", "Provider_name")

#read in additional sites data for stations beginning with  634S
nv.sites2<-read.csv("~/Documents/WaterCube/Ch.3/state_data/Nevada/nv_sites2.csv")
nv.sites3<-subset(nv.sites2, select=c("StationCode", "Coordinate", "Coordinate.1", "Name"))
names(nv.sites3)<-c("StationID", "Lat_Dec", "Long_Dec", "WaterbodyName")
nv.sites3$Location<-NA
nv.sites3$BasinID<-NA
nv.sites3$BasinSegID<-NA
nv.sites3$Ecoregion<-NA
nv.sites3$Elev_m<-NA

nv.sites4<-rbind(nv.sites, nv.sites3)

#try merging with sites data
nv<-merge(nv.taxa.tidy2, nv.sites4, by="StationID") #finally more matches
nv1<-subset(nv, select=c("StationID", "FinalID", "CollDate", "Sample_method","Datum", "Provider_name", "WaterbodyName.y", "Lat_Dec", "Long_Dec"))
names(nv1)<-c("Location_ID", "Submitted_name", "Date", "Sample_method","Datum", "Provider_name", "Location_description", "Latitude", "Longitude")
nv1$Sample_ID<-NA
nv2<-nv1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

############################################## New Mexico ##################################################################################
#NAD83
head(nm)
nm_methods<-read.csv("~/Documents/WaterCube/Ch.3/state_data/New_Mexico/Benthic_metrics.csv", stringsAsFactors = FALSE)
head(nm_methods)
#spread column one into three columns
#create key
nm_methods$key<-NA
nm_methods$key[grep("/", nm_methods$Date.method)]<- "Date"
nm_methods$key[grep("Hess", nm_methods$Date.method)]<- "Sample_method"
nm_methods$key[grep("EMAP", nm_methods$Date.method)]<- "Sample_method"
nm_methods$key[grep("KickNet", nm_methods$Date.method)]<- "Sample_method"
nm_methods$key[grep("Surber", nm_methods$Date.method)]<- "Sample_method"

nm_methods2<-nm_methods %>% 
  group_by_at(vars(-Date.method)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=key, value=Date.method) %>%    # spread
  select(-row_id)  # drop the index

nm_methods3<-nm_methods2%>%mutate(Date.new = if_else(!is.na(Date),  Date, lag(Date)))
#transform date formats
nm_methods3$Date.new<-parse_date_time(x = nm_methods3$Date.new,orders = "%m/%d/%y", tz="America/New_York")
names(nm_methods3)<-c("Location_description", "Date.old", "Sample_method","NA", "Date")

nm2<-subset(nm, select = c("MLOC_NAME", "MLOC_LATITUDE", "MLOC_LONGITUDE", "ACT_START_DATE", "TAX_NAME", "MLOC_UID",  "SE_UID"))
head(nm2)
nm2$Date.new<-parse_date_time(x = nm2$ACT_START_DATE,
                              orders = "Y-m-d HMS", tz="America/New_York")
names(nm2)<-c("Location_description", "Latitude", "Longitude", "Date.old", "Submitted_name", "Location_ID", "Sample_ID", "Date")
nm_methods3$Location_description<-as.character(nm_methods3$Location_description)

#now merge nm_methods2 with nm
nm3<-merge(nm2,nm_methods3, by.x=c("Location_description", "Date"),by.y=c("Location_description", "Date"), all.x = TRUE)

nm3$Datum<-"NAD83"
nm3$Provider_name<-"New Mexico Environment Department"

nm4<-nm3[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

#assign Sample_method of NARS to all years after 2010 per email conversation with state agency contact
Date1<-as.Date("2011-01-01")  
nm4$Sample_method[which(nm4$Date>=Date1)]<-"NARS"

############################################## Ohio ########################################################################################
#NAD83
#read-in data
oh1<-read.csv('~/Documents/WaterCube/Ch.3/state_data/OH_Biomon_Data/OEPA_bug_data1.csv', stringsAsFactors = FALSE)
oh2<-read.csv('~/Documents/WaterCube/Ch.3/state_data/OH_Biomon_Data/OEPA_bug_data2.csv', stringsAsFactors = FALSE)
oh<-rbind(oh1, oh2)
oh1<-oh[,-c(2,7)]
names(oh1)<-c("Submitted_name", "Date", "Sample_method", "Location_ID", "Location_description", "Latitude", "Longitude")
oh1$Datum<-"NAD83"
oh1$Provider_name<-"Ohio Environmental Protection Agency"
oh1$Sample_ID<-NA
oh2<-oh1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

############################################## Oregon ################################################################################################
#WGS84
head(or1)
head(or2)
or<-rbind(or1, or2)
#subset to insect taxa
or.1<-subset(or, Class=="Insecta")
or1<-subset(or.1, select=c("STATION_KEY", "LAT_field", "LONG_field", "SITE_NAME", "Date", "Taxon"))
names(or1)<-c("Location_ID", "Latitude", "Longitude", "Location_description", "Date", "Submitted_name")
or1$Provider_name<-"Oregon Department of Environmental Protection"
or1$Datum<-"WGS84"
or1$Sample_ID<-NA
or1$Sample_method<-NA
or2<-or1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

############################################# Pennsylvania ############################################################################################
#NAD83
head(pa)
pa1<-subset(pa, select=c("STR_STATIO", "INT_STATION_GEN_ID", "LAT", "LNG", "STR_METHOD_DESC", "STR_ID_LEVEL"))
#need to split first column into date and name
pa2<-pa1%>%
  separate(STR_STATIO, into=c("Date", "Time", "Name"), sep="-")
pa3<-pa2[,-c(2,3)]

names(pa3)<-c("Date", "Location_ID", "Latitude", "Longitude", "Sample_method", "Submitted_name")
pa3$Provider_name<-"Pennsylvania Department of Environmental Protection"
pa3$Datum<-"NAD83"
pa3$Sample_ID<-NA
pa3$Location_description<-NA
pa4<-pa3[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]
unique(pa4$Date)

############################################## Tennessee ###################################################################################################
#NAD83
head(tn)
tn1<-subset(tn, select=c("StationID", "StreamName", "Latitude", "Longitude", "BenSampID", "CollMeth", "CollDate", "FinalID"))
names(tn1)<-c("Location_ID", "Location_description", "Latitude", "Longitude", "Sample_ID", "Sample_method", "Date", "Submitted_name")
tn1$Provider_name<-"Tennessee Department of Environment and Conservation"
tn1$Datum<-NA
tn2<-tn1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

############################################## Texas #######################################################################################################
#create a separate table with Sample method for each parameter/taxa set and merge with master dataset<------------------------

#can't use separate with list. how to loop through?
params1<-separate(tx.params1, "X............LIST.OF.UNIQUE.PARAMETERS...............................", c("Parameter.Code", "Value"), sep=" ", extra="merge")
params2<-separate(tx.params2, "X............LIST.OF.UNIQUE.PARAMETERS...............................", c("Parameter.Code", "Value"), sep=" ", extra="merge")
params3<-separate(tx.params3, "X............LIST.OF.UNIQUE.PARAMETERS...............................", c("Parameter.Code", "Value"), sep=" ", extra="merge")
params4<-separate(tx.params4, "X............LIST.OF.UNIQUE.PARAMETERS...............................", c("Parameter.Code", "Value"), sep=" ", extra="merge")
params5<-separate(tx.params5, "X............LIST.OF.UNIQUE.PARAMETERS...............................", c("Parameter.Code", "Value"), sep=" ", extra="merge")
params6<-separate(tx.params6, "X............LIST.OF.UNIQUE.PARAMETERS...............................", c("Parameter.Code", "Value"), sep=" ", extra="merge")

#need to drop some parameters from each paramatere set, then merge the subsetted paramter lists with the taxa datasets and keep only matches
params1.1<-params1[c(238:400),] #want rows 194, 195,214, for methods and 238 to 400. #first three alwyas for methods
params2.1<-params2[c(442:916),] #want rows 375,376, 398, 442-916,
params3.1<-params3[c(461:1000),]#391,392,414,461-1000
params4.1<-params4[c(492:814),]#431.432,454, 492-814
params5.1<-params5[c(384:754),]#309,310,339, 384-754
params6.1<-params6[c(384:754),] #309,310,339, 384-754

tx.1<-merge(params1.1, tx.taxa1, by="Parameter.Code") 
tx.2<-merge(params2.1, tx.taxa2, by="Parameter.Code")
tx.3<-merge(params3.1, tx.taxa3, by="Parameter.Code") 
tx.4<-merge(params4.1, tx.taxa4, by="Parameter.Code")
tx.5<-merge(params5.1, tx.taxa5, by="Parameter.Code") 
tx.6<-merge(params6.1, tx.taxa6, by="Parameter.Code")
tx<-rbind(tx.1, tx.2, tx.3, tx.4, tx.5, tx.6)

tx1<-subset(tx, Value.x!="RAPID BIOASSESSMENT PROTOCOLS BENTHIC MACROINVERTEBRATE IBI SCORE")

#merge with sites data
tx2<-merge(tx1, tx.sites, by.x="Station.ID", by.y="MONITORING_STATION_ID")

tx3<-subset(tx2, select=c("Station.ID","Value.x", "ms_long_desc_txt", "latitude_num", "longitude_num", "Start.Date"))
names(tx3)<-c("Location_ID", "Submitted_name", "Location_description", "Latitude", "Longitude", "Date")
tx3$Provider_name<-"Texas Commission on Environmental Quality"
tx3$Sample_method<-NA
tx3$Datum<-NA
tx3$Sample_ID<-NA

tx4<-tx3[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

#read in geo datum data
datum.dat<-read.csv("~/Documents/WaterCube/Ch.3/state_data/Texas/Texas_geo_datum.csv", stringsAsFactors = FALSE)
dat<-subset(datum.dat, select=c("MONITORING_STATION_ID", "latitude_num", "longitude_num", "horizontal_datum"))
names(dat)<-c("Location_ID", "Latitude", "Longitude", "Datum")

tx5<-merge(tx4, dat, by="Location_ID")
#drop extra columns and rename columns
tx6<-subset(tx5, select=c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude.x", "Longitude.x", "Provider_name", "Sample_method", "Location_description", "Datum.y"))
names(tx6)<-c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")

############################################## Virginia ####################################################################################################
#no Datum given
head(va.taxa)
head(va.sites)
va.taxa2<-read.csv("/Volumes/KINGSTON/WaterCube/Ch.3/state_data/Virginia/Virginia_taxa2.csv")
head(va.taxa2)

va.taxa3<-merge(va.taxa, va.taxa2, by=intersect(names(va.taxa), names(va.taxa2)))

va<-merge(va.taxa3, va.sites, by=intersect(names(va.taxa3), names(va.sites)))
va1<-subset(va, select=c("StationID", "BenSampID", "FinalID", "CollDate", "CollMeth", "Lat", "Long", "StreamName"))
names(va1)<-c("Location_ID", "Sample_ID", "Submitted_name", "Date", "Sample_method", "Latitude", "Longitude", "Location_description")
va1$Datum<-NA
va1$Provider_name<-"Virginia"
va2<-va1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

############################################### Washington #################################################################################################
#NAD83
head(wa.sites)
head(wa.taxa)
wa<-merge(wa.sites, wa.taxa, by="Location_ID")
wa1<-subset(wa, select=c("Location_ID", "Location_Description", "Calculated_Latitude_Decimal_Degrees_NAD83HARN.x","Calculated_Longitude_Decimal_Degrees_NAD83HARN.x", "Sample_Taxon_Name", "Field_Collection_Start_Date", "Sample_ID", "Sample_Collection_Method_Description"))
names(wa1)<-c("Location_ID", "Location_description", "Latitude", "Longitude", "Submitted_name", "Date", "Sample_ID", "Sample_method")
wa1$Datum<-"NAD83"
wa1$Provider_name<-"Washington State Department of Ecology"
wa2<-wa1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]

############################################### West Virginia ##############################################################################################
#read in new data
wv<-read.csv("~/Documents/WaterCube/Ch.3/state_data/WV Biomon Data/Aquatic_Insect_Taxa.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
#create column with taxon using Family, Genus, Species, Columns

#first create new species column
wv$taxon<-if_else(is.na(wv$SPECIES), wv$GENUS, paste(wv$GENUS, "", wv$SPECIES))
wv$taxon<-if_else(is.na(wv$taxon), wv$FAMILY, wv$taxon)
wv$taxon<-if_else(is.na(wv$taxon), wv$ORDER_1, wv$taxon)

wv1<-subset(wv, select = c("SAMPLE_ID", "STATION_ID", "SAMPLE_DATE", "STREAM_NAME", "taxon", "POR_LON_DD", "POR_LAT_DD"))
wv1$Datum<-"NAD83"
wv1$Provider_name<-"West Virginia Department of Environmental Protection"
wv1$Sample_method<-NA

names(wv1)<-c("Sample_ID", "Location_ID", "Date", "Location_description", "Submitted_name", "Longitude", "Latitude", "Datum", "Provider_name", "Sample_method")
wv2<-wv1[c("Location_ID", "Sample_ID", "Date", "Submitted_name", "Latitude", "Longitude", "Provider_name", "Sample_method", "Location_description", "Datum")]
############################################### Wisconsin ##################################################################################################
#Need Datum- data requested
head(wi.taxa)
head(wi.sites)
wi<-merge(wi.taxa, wi.sites, by=intersect(names(wi.taxa), names(wi.sites)))
head(wi)
#data not very useable in current format- need to spread data in DNR_parameter_description
wi2<-subset(wi, select=c("MONIT_STATION_SEQ_NO", "OFFICIAL_WATERBODY_NAME", "CALC_LL_LAT_DD_AMT", "CALC_LL_LONG_DD_AMT", "START_DATE_TIME", "PRIMARY_LAB_SAMPLE_ID", "RESULT_VALUE_NO", "DNR_PARAMETER_DESCRIPTION", "PROJECT_DESC"))


#remove non-taxa values?
unique(wi2$DNR_PARAMETER_DESCRIPTION)

wi.long <- wi2 %>% 
  tibble::rowid_to_column() %>% 
  spread(key="DNR_PARAMETER_DESCRIPTION", value="RESULT_VALUE_NO")

wi3<-spread(wi2, key="DNR_PARAMETER_DESCRIPTION", value="RESULT_VALUE_NO")


##########################################Combine states with sufficient data into one data frame to merge with WQP data###########################################################################################################################
states<-rbind(az2, ar_master, ca3, fl3, ia3, id4, mn3, ms2, mo3, nb4, nv2, nm4, oh2, or2, pa4, tn2, tx6, wa2, wv2)
write.csv(states, "state_data.csv")

save.image(file="state_cleaning.RData")



       