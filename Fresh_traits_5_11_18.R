## TITLE:         Freshwater Traits Database: UURAF Poster  
## AUTHOR:        Phoebe Zarnetske
## COLLABORATORS: E. Hiltner, L. Twardochleb 
## DATA:          Freshwater invertebrate datanase as of April 10, 2018
## PROJECT:       "An ecological trait database of North American freshwater 
##                  invertebrates for the assessment of climate change effects on streams" 
## FIGURES:       creates *Figure 3* (number of species per thermal tolerance x voltinism)
## DATE:          April 10, 2018

#setwd("/Volumes/plz-lab/Toolik/final_data/species")
setwd("~/Desktop/AIR")
rm(list=ls()) #this clears data from the workspace
graphics.off()


library(dplyr)
library(plyr)
library(ggplot2)

####** DATA IMPORT **####
#########################
#load("Fresh_traits.RData")
biotraits <- read.csv("biotraits8_5_9_18 .csv", stringsAsFactors = FALSE)
biotraits

# take a look at the fields in this dataset
str(biotraits)
head(biotraits)

## Data Cleaning
#replacing all rows containing Heteroptera as the Order wih Hemiptera
revalue(biotraits$Order[biotraits$Order == "Heteroptera"] <- "Hemiptera")
revalue(biotraits$Volt_new[biotraits$Volt_new == "semivoltine"] <- "Semivoltine")
revalue(biotraits$Volt_new[biotraits$Volt_new == "bi_multivoltine"] <- "BI_multivoltine")
revalue(biotraits$Volt_new[biotraits$Volt_new == "Bi_multivoltine"] <- "BI_multivoltine")
revalue(biotraits$Volt_new[biotraits$Volt_new == "univoltine"] <- "Univoltine")

#Over-write the levels so they are current
levels(biotraits$Volt_new)<-levels(biotraits$Volt_new)

## creating counts of the number of unique variables for each genus within Orders of interest

#Filtering for Order Odonata
Odonata <- filter(biotraits, Order == "Odonata")

#grouping by Genus and getting counts for unique variables in columns of interest
## PLZ Modified below to reflect new field names
Counts_Odonata<-Odonata %>% group_by(Genus) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))
           
#Filtering for Order Diptera
Diptera <- filter(biotraits, Order == "Diptera")

#grouping by Genus and getting counts for unique variables in columns of interest
Counts_Diptera<-Diptera %>% group_by(Genus) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

#Filtering for Order Coleoptera
Coleoptera <- filter(biotraits, Order == "Coleoptera")

#grouping by Genus and getting counts for unique variables in columns of interest
Counts_Coleoptera<-Coleoptera %>% group_by(Genus) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

#Filtering for Order Ephemeroptera
Ephemeroptera <- filter(biotraits, Order == "Ephemeroptera")

#grouping by Genus and getting counts for unique variables in columns of interest
Counts_Ephemeroptera<-Ephemeroptera %>% group_by(Genus) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

#Filtering for Order Heteroptera
Hemiptera <- filter(biotraits, Order ==  "Hemiptera")

#grouping by Genus and getting counts for unique variables in columns of interest
Counts_Hemiptera<-Hemiptera %>% group_by(Genus) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

#Filtering for Order Megaloptera
Megaloptera <- filter(biotraits, Order == "Megaloptera")

#grouping by Genus and getting counts for unique variables in columns of interest
Counts_Megaloptera<-Megaloptera %>% group_by(Genus) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

#Filtering for Order Plecoptera
Plecoptera <- filter(biotraits, Order == "Plecoptera")

#grouping by Genus and getting counts for unique variables in columns of interest
Counts_Plecoptera<-Plecoptera %>% group_by(Genus) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

#Filtering for Order Trichoptera
Trichoptera <- filter(biotraits, Order == "Trichoptera")

#grouping by Genus and getting counts for unique variables in columns of interest
Counts_Trichoptera<-Trichoptera %>% group_by(Genus) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

#saving the data tables as csv files
write.csv(Counts_Coleoptera, file = "../R_code/final_data/Counts_Coleoptera.csv")
write.csv(Counts_Diptera, file = "../R_code/final_data/Counts_Diptera.csv")
write.csv(Counts_Ephemeroptera, file = "../R_code/final_data/Counts_Ephemeroptera.csv")
write.csv(Counts_Hemiptera, file = "../R_code/final_data/Counts_Hemiptera.csv")
write.csv(Counts_Megaloptera, file = "../R_code/final_data/Counts_Megaloptera.csv")
write.csv(Counts_Odonata, file = "../R_code/final_data/Counts_Odonata.csv")
write.csv(Counts_Plecoptera, file = "../R_code/final_data/Counts_Plecoptera.csv")
write.csv(Counts_Trichoptera, file = "../R_code/final_data/Counts_Trichoptera.csv")

#Looking at all the orders in the database
unique(biotraits$Order)


#creating counts of the number of unique variables for each family within Orders of interest

#grouping by Family and getting counts for unique variables in columns of interest
Counts_Odonata_Family<-Odonata %>% group_by(Family) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

Counts_Diptera_Family<-Diptera %>% group_by(Family) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

Counts_Coleoptera_Family<-Coleoptera %>% group_by(Family) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

Counts_Ephemeroptera_Family<-Ephemeroptera %>% group_by(Family) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

Counts_Hemiptera_Family<-Hemiptera %>% group_by(Family) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

Counts_Megaloptera_Family<-Megaloptera %>% group_by(Family) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

Counts_Plecoptera_Family<-Plecoptera %>% group_by(Family) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

Counts_Trichoptera_Family<-Trichoptera %>% group_by(Family) %>%  
  summarize(Feed_mode_new=n_distinct(Feed_mode_new, na.rm = TRUE),
            Female_disp_abbrev=n_distinct(Female_disp_abbrev, na.rm = TRUE),
            Max_body_size=n_distinct(Max_body_size, na.rm = TRUE), 
            Emerge_season_1=n_distinct(Emerge_season_1, na.rm = TRUE),
            Emerge_season_2=n_distinct(Emerge_season_2, na.rm = TRUE),
            AdultFlyingStrength_abbrev=n_distinct(AdultFlyingStrength_abbrev, na.rm = TRUE),
            Volt_new=n_distinct(Volt_new, na.rm = TRUE),
            Thermal_pref=n_distinct(Thermal_pref, na.rm = TRUE))

#saving the data tables as csv files
write.csv(Counts_Coleoptera_Family, file = "../R_code/final_data/Counts_Coleoptera_Family.csv")
write.csv(Counts_Diptera_Family, file = "../R_code/final_data/Counts_Diptera_Family.csv")
write.csv(Counts_Ephemeroptera_Family, file = "../R_code/final_data/Counts_Ephemeroptera_Family.csv")
write.csv(Counts_Hemiptera_Family, file = "../R_code/final_data/Counts_Hemiptera_Family.csv")
write.csv(Counts_Megaloptera_Family, file = "../R_code/final_data/Counts_Megaloptera_Family.csv")
write.csv(Counts_Odonata_Family, file = "../R_code/final_data/Counts_Odonata_Family.csv")
write.csv(Counts_Plecoptera_Family, file = "../R_code/final_data/Counts_Plecoptera_Family.csv")
write.csv(Counts_Trichoptera_Family, file = "../R_code/final_data/Counts_Trichoptera_Family.csv")

#looking at the number of each unique variable for columns of interest
table(biotraits$Max_body_size)
table(biotraits$Emerge_season_1)
table(biotraits$Emerge_season_2)
table(biotraits$Feed_mode_prim)
table(biotraits$Female_disp_abbrev)
table(biotraits$AdultFlyingStrength_abbrev)
table(biotraits$Voltinism_abbrev)
table(biotraits$Voltinism)
table(biotraits$Thermal_pref)
table(biotraits$Rheophily_abbrev)
table(biotraits$Habit_prim_abbrev)
table(biotraits$Resp_abbrev)

# Plot Figure 3: Barplot showing 
#   thermal temperature range (X) vs. number of species (Y)
#   color-coded by Voltinism

traits1<-subset(biotraits,select=c("Genus","Family","Order",
                                   "SubjectTaxonomicName","Thermal_pref",
                                   "Volt_new","Feed_mode_new","Habit_new","Resp_abbrev","Max_body_size_abbrev"))

revalue(traits1$Thermal_pref[traits1$Thermal_pref == "Warm eurythermal (15-30 C)"] <- "15-30 C")
revalue(traits1$Thermal_pref[traits1$Thermal_pref == "Cold stenothermal (<5 C)"] <- "<5 C")
revalue(traits1$Thermal_pref[traits1$Thermal_pref == "cold-cool eurythermal (0-15 C)"] <- "0-15 C")
revalue(traits1$Thermal_pref[traits1$Thermal_pref == "Cold-cool eurythermal (0-15 C)"] <- "0-15 C")
revalue(traits1$Thermal_pref[traits1$Thermal_pref == "Hot euthermal (>30 C)"] <- ">30 C")

Genera <- count(traits1$Genus)
## Figure 3: x-axis = thermal preference; y-axis = number of species; coded by voltinism

 #new plot


library(ggplot2)

Genera <- traits1 %>% group_by(Genus) %>%  
summarize(Genus=n_distinct(Genus, na.rm = TRUE))

  ggplot(traits1, aes(x = Thermal_pref, y = Genera, fill = Max_body_size_abbrev)) + 
  geom_bar(stat = "identity")

  
  
traits2<-subset(biotraits,select=c("Genus","Family","Order","SubjectTaxonomicName","Thermal_pref","Resp_early","Resp_abbrev"))



# Code from May 10 --------------------------------------------------------

library(dplyr)
library(ggplot2)

# Convert all uppercase to lowercase
biotraits <- biotraits %>%
  mutate(Thermal_pref = trimws(tolower(Thermal_pref)), Resp_abbrev = trimws(tolower(Resp_abbrev)))

biotraits$Thermal_pref[grep("15-30", biotraits$Thermal_pref)] <- '15-30 C'
biotraits$Thermal_pref[grep("<5", biotraits$Thermal_pref)] <- '<5 C'
biotraits$Thermal_pref[grep("0-15", biotraits$Thermal_pref)] <- '0-15 C'
biotraits$Thermal_pref[grep(">30", biotraits$Thermal_pref)] <- '>30 C'



# Group by genus and get most common respiration and thermal preference value from each one
resp_therm_by_genus <- biotraits %>%
  filter(!is.na(Resp_abbrev), !is.na(Thermal_pref)) %>%
  group_by(Order, Family, Genus) %>%
  summarize(mode_resp = names(sort(table(Resp_abbrev), decreasing = TRUE))[1],
            mode_therm = names(sort(table(Thermal_pref), decreasing = TRUE))[1])


# Plot with only beetles and flies, separated
resp_therm_by_genus %>%
  filter(Order %in% c('Coleoptera', 'Diptera')) %>%
  ggplot(aes(x = mode_therm, fill = mode_resp, group = mode_resp)) +
  geom_bar(position = 'stack') +
  facet_wrap(~ Order) +
  theme_bw()


# All regardless of order 
resp_therm_by_genus %>%
  ggplot(aes(x = mode_therm, fill = mode_resp, group = mode_resp)) +
  geom_bar(position = 'stack') +
  theme_bw() +
  scale_y_continuous(name = 'Number of Genera', expand = c(0,0), limits=c(0,150))


# new plot (max immature body size vs. thermal preference)

# Convert all uppercase to lowercase
biotraits <- biotraits %>%
  mutate(Max_body_size_abbrev = trimws(tolower(Max_body_size_abbrev)))

# Group by genus and get most common max immature body size and thermal preference value from each one
size_therm_by_genus <- biotraits %>%
  filter(!is.na(Max_body_size_abbrev), !is.na(Thermal_pref)) %>%
  group_by(Order, Family, Genus) %>%
  summarize(max_size = names(sort(table(Max_body_size_abbrev), decreasing = TRUE))[1],
            mode_therm = names(sort(table(Thermal_pref), decreasing = TRUE))[1])

# Plot of All genera regardless of order 
size_therm_by_genus %>%
  ggplot(aes(x = mode_therm, fill = max_size, group = max_size)) +
  geom_bar(position = 'stack') +
  theme_bw() +
  scale_y_continuous(name = 'Number of Genera', expand = c(0,0), limits=c(0,150))



# new plot (max immature body size vs. flight ability)

# Convert all uppercase to lowercase
biotraits <- biotraits %>%
  mutate(AdultFlyingStrength_abbrev = trimws(tolower(AdultFlyingStrength_abbrev)))

# Group by genus and get most common max immature body size and flying strength value from each one
size_flight_by_genus <- biotraits %>%
  filter(!is.na(Max_body_size_abbrev), !is.na(AdultFlyingStrength_abbrev)) %>%
  group_by(Order, Family, Genus) %>%
  summarize(max_size = names(sort(table(Max_body_size_abbrev), decreasing = TRUE))[1],
            flying_strength = names(sort(table(AdultFlyingStrength_abbrev), decreasing = TRUE))[1])

# Plot of All genera regardless of order 
size_flight_by_genus %>%
  ggplot(aes(x = max_size, fill = flying_strength, group = flying_strength)) +
  geom_bar(position = 'stack') +
  theme_bw() +
  scale_y_continuous(name = 'Number of Genera', expand = c(0,0), limits=c(0,150))



# new plot (Voltinism vs. thermal preference)

# Convert all uppercase to lowercase
biotraits <- biotraits %>%
  mutate(Volt_new = trimws(tolower(Volt_new)))

# Group by genus and get most common max immature body size and thermal preference value from each one
volt_therm_by_genus <- biotraits %>%
  filter(!is.na(Volt_new), !is.na(Thermal_pref)) %>%
  group_by(Order, Family, Genus) %>%
  summarize(voltinism = names(sort(table(Volt_new), decreasing = TRUE))[1],
            mode_therm = names(sort(table(Thermal_pref), decreasing = TRUE))[1])

# Plot of All genera regardless of order 
volt_therm_by_genus %>%
  ggplot(aes(x = mode_therm, fill = voltinism, group = voltinism)) +
  geom_bar(position = 'stack') +
  theme_bw() +
  scale_y_continuous(name = 'Number of Genera', expand = c(0,0), limits=c(0,150))



# new plot (Voltinism vs. max immature body size)

# Convert all uppercase to lowercase
biotraits <- biotraits %>%
  mutate(Volt_new = trimws(tolower(Volt_new)))

# Group by genus and get most common max immature body size and thermal preference value from each one
volt_size_by_genus <- biotraits %>%
  filter(!is.na(Volt_new), !is.na(Max_body_size_abbrev)) %>%
  group_by(Order, Family, Genus) %>%
  summarize(voltinism = names(sort(table(Volt_new), decreasing = TRUE))[1],
            max_size = names(sort(table(Max_body_size_abbrev), decreasing = TRUE))[1])

# Plot of All genera regardless of order 
volt_size_by_genus %>%
  ggplot(aes(x = max_size, fill = voltinism, group = voltinism)) +
  geom_bar(position = 'stack') +
  theme_bw() +
  scale_y_continuous(name = 'Number of Genera', expand = c(0,0), limits=c(0,200))




# new plot (habit vs. Rheophily)

# Convert all uppercase to lowercase
biotraits <- biotraits %>%
  mutate(Habit_new = trimws(tolower(Habit_new)))

biotraits$Habit_new[grep("clinger", biotraits$Habit_new)] <- 'clinger'
biotraits$Habit_new[grep("attached", biotraits$Habit_new)] <- 'clinger'
biotraits$Habit_new[grep("planktonic", biotraits$Habit_new)] <- 'other (specify in comments)'

# Group by genus and get most common max immature body size and thermal preference value from each one
habit_rheo_by_genus <- biotraits %>%
  filter(!is.na(Habit_new), !is.na(Rheophily_abbrev)) %>%
  group_by(Order, Family, Genus) %>%
  summarize(habit = names(sort(table(Habit_new), decreasing = TRUE))[1],
            rheophily = names(sort(table(Rheophily_abbrev), decreasing = TRUE))[1])

# Plot of All genera regardless of order 
habit_rheo_by_genus %>%
  ggplot(aes(x = habit, fill = rheophily, group = rheophily)) +
  geom_bar(position = 'stack') +
  theme_bw() +
  scale_y_continuous(name = 'Number of Genera', expand = c(0,0), limits=c(0,200))


save.image(file="Fresh_traits.RData")