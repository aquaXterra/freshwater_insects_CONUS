######################## Summarize insect occurrence records by HUC  #########################
##Author: Laura Twardochleb
##Date created: 10/8/19
##Date last updated:10/24/19
##This script summarizes insect occurrence records by HUC12 watersheds 

# -----------------------------
# LIST OF NECESSARY INPUT FILES
# -----------------------------

#Insect occurrence data
#HUC12 shape files

#Load packages 
library(sp)
library(maptools)
library(dplyr)
library(rgdal)
library(FD)
library(iNEXT)
library(tidyr)
library(readr)
library(purrr)
library(reshape2)
library(ggplot2)
library(ggsn)

#Define map projection
crsaea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
crswgs <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
crsnad83<-'+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0'
crsnad27<-'+init=epsg:4267 +proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs+nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat'

#load insect occurrence data
insect_occurrence<-read.csv("/mnt/research/aquaxterra/DATA/Insects/occurrence22.1.csv", stringsAsFactors = FALSE)

############### Assign occurrence records to HUC IDS-using LAGOS HUCs ####################################################################################
#subset records to those in WGS84 and reproject to Albers equal area 
recordsWGS84<-subset(insect_occurrence, HorizontalCoordinateReferenceSystemDatumName=="WGS84")
wgs84spatial<-with(recordsWGS84, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(crswgs)))
sptransformwgs84<-spTransform(wgs84spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.wgs <- as.data.frame(sptransformwgs84@coords) 
names(site_coords.wgs)<-c("Longitude_transformed", "Latitude_transformed")
wgs84<-cbind(recordsWGS84, site_coords.wgs)

#reproject those in NAD83
recordsnad83<-subset(insect_occurrence, HorizontalCoordinateReferenceSystemDatumName=="NAD83")
nad83spatial<-with(recordsnad83, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(crsnad83)))
sptransformnad83<-spTransform(nad83spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.nad83 <- as.data.frame(sptransformnad83@coords) 
names(site_coords.nad83)<-c("Longitude_transformed", "Latitude_transformed")
nad83<-cbind(recordsnad83, site_coords.nad83)

#reproject those in NAD27
recordsnad27<-subset(insect_occurrence, HorizontalCoordinateReferenceSystemDatumName=="NAD27")
nad27spatial<-with(recordsnad27, SpatialPoints(coords=cbind(Longitude, Latitude), proj4string = CRS(crsnad27)))
sptransformnad27<-spTransform(nad27spatial, CRSobj=CRS(crsaea)) #reprojected to albers equal area projection
site_coords.nad27 <- as.data.frame(sptransformnad27@coords) 
names(site_coords.nad27)<-c("Longitude_transformed", "Latitude_transformed")
nad27<-cbind(recordsnad27, site_coords.nad27)

sp_insects<-rbind(sptransformnad27,sptransformnad83,sptransformwgs84)

# Load the HUC12 polygons- need to change to filepath for polygons in aquaxterra folder of HPC
x <- readOGR(dsn='~/Documents/WaterCube/Ch.3/aquatic_insects/HUCs_PZarnetske/hu12.shp')
xp <- as(x, 'SpatialPolygons')

# For each polygon, get which insect points are in it
ranges <- lapply(1:length(xp), function(i) !is.na(over(sp_insects, xp[i])))

# For each row, get which HUC12 it is in.
ranges_df <- do.call(cbind, ranges)
table(apply(ranges_df, 1, sum)) # check to see if we got unique polygon for each.
# Most are in a single polygon, some are in no polygon but that's okay.
which_huc <- unlist(apply(ranges_df, 1, function(x) {
  xw <- which(x)
  ifelse(length(xw) == 0, NA, xw)
}))

# Get the huc12 ID's
which_huc_id <- x@data$hu12_huc12[which_huc]
insect_occurrence_transformed$HUC12_ID <- which_huc_id

write.csv(insect_occurrence_transformed,"insect_occurrence_w_HUC12s.csv")


