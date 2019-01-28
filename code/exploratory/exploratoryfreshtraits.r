# Exploratory analysis
# Freshwater traits
# Started by QDR 24 Jan 2019


# Very crude data cleaning ------------------------------------------------

library(tidyverse) # for data cleaning
library(psych) # for dummy coding
library(cluster) # for Gower distance
library(Rtsne) # for the plot

traits <- read.csv('~/Documents/R/Raw_trait_table.csv', stringsAsFactors = FALSE)

# For now, use these columns.
use_cols <- c("Adult_disp", "AdultFlyingStrength_abbrev", 
              "Emerge_season_1", "Emerge_season_2", 
              "Emerge_synch_abbrev", 
              "Feed_mode_sec", 
              "Feed_prim_abbrev", "Female_disp_abbrev", 
              "Habit_prim", "Habit_sec", 
              "Max_body_size_abbrev", 
              "Resp_abbrev", 
              "Rheophily_abbrev", 
              "Thermal_pref", 
              "Voltinism_abbrev")

traits_use <- traits[,c('SubjectTaxonomicName', use_cols)]

# Remove anything containing the term "Other"
# For now, use the most common trait value (mode) per taxon. Obviously not ideal.
mode_fn <- function(x) ifelse(any(!is.na(x)), names(which.max(table(x))), as.character(NA))

traits_grp <- traits_use %>%
  mutate_all(function(x) if_else(grepl('Other', x), as.character(NA), x)) %>%
  filter(!is.na(SubjectTaxonomicName)) %>%
  mutate(SubjectTaxonomicName = trimws(SubjectTaxonomicName)) %>%
  group_by(SubjectTaxonomicName) %>%
  summarize_all(mode_fn)

table(apply(traits_grp[,-1], 1, function(x) sum(!is.na(x)))) # How many non-missing values are in each row.

# Get rid of anything with less than 6 traits because it's not going to show a syndrome anyway
# Again this can be done better later with imputation and other methods

traits_grp <- traits_grp %>%
  filter(apply(traits_grp[,-1], 1, function(x) sum(!is.na(x))) >= 6)

# Dummy code to convert all variables to several binary columns.
trait_dummy_list <- imap(traits_grp[,-1], function(x, y) {
  res <- dummy.code(x) 
  dimnames(res)[[2]] <- paste(y, dimnames(res)[[2]], sep = '_')
  res
})

traits_dummy <- data.frame(taxon = traits_grp$SubjectTaxonomicName, do.call(cbind, trait_dummy_list), stringsAsFactors = FALSE)

# Exploratory cluster analysis --------------------------------------------

# See https://www.r-bloggers.com/clustering-mixed-data-types-in-r/ for source of this analysis

# Compute Gower distance of all the dummy coded variables
traits_gower <- daisy(traits_dummy[,-1], metric = 'gower')
traits_gower_mat <- as.matrix(traits_gower)

# Even though Gower deals with some missing data, there are still some NA values in the dissimilarity matrix which is not allowed. 
# We need to get rid of them.
which(is.na(traits_gower_mat), arr.ind = TRUE)

getrid <- c(600, 760) # These two taxa are responsible for all the bad values so get rid of them.
traits_dummy <- traits_dummy[-getrid, ]

# Recalculate Gower
traits_gower <- daisy(traits_dummy[,-1], metric = 'gower')
traits_gower_mat <- as.matrix(traits_gower)
which(is.na(traits_gower_mat), arr.ind = TRUE) # No missing values.

# Sanity check: look at the most similar and most dissimilar pairs of taxa in the analysis
# What is the most similar pair?
mostsimilarpair <- traits_dummy[which(traits_gower_mat == min(traits_gower_mat[traits_gower_mat > 0], na.rm = TRUE), arr.ind = TRUE)[1,], ]
mostsimilarpair$taxon # Peltodytes and Haliplus -- they actually do seem similar so that's cool

# What is the most dissimilar pair?
mostdiffpair <- traits_dummy[which(traits_gower_mat == max(traits_gower_mat[traits_gower_mat < max(traits_gower_mat, na.rm = TRUE)], na.rm = TRUE), arr.ind = TRUE)[1,], ]
mostdiffpair$taxon # A dragonfly and a small minnow mayfly. Different I guess???

# Use silhouette width to determine the optimal number of clusters with PAM algorithm (partitioning around medoids)
# It is sort of like an R2
# Try everything from 2 to 10
set.seed(555)
test_n_clusters <- 2:10
pam_fits <- test_n_clusters %>% map(~ pam(traits_gower, diss = TRUE, k = .x))
sil_widths <- map_dbl(pam_fits, ~ .x$silinfo$avg.width)

plot(2:10, sil_widths) # 2 appears to be the optimal number of clusters.

final_fit <- pam_fits[[1]] # The one with 2 clusters

traits_dummy[final_fit$medoids, ] # These are the two species at the medoid of each cluster.


# Make a t-SNE plot to show 2-D version of the clusters (an ordination technique)
traits_tsne <- Rtsne(traits_gower, is_distance = TRUE)
tsne_data <- traits_tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(final_fit$clustering),
         taxon = traits_dummy$taxon)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
# Clearly this is not a good situation. We need to refine things more.