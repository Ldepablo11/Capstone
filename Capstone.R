setwd('~/Downloads')

# Loading all the necessary packages.
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(Synth)
source('Helper_Functions (1).R')

# Loading the data-frame with Chicago census tracts, turning the GEOID into a
# character variable.
chi_df <- read.csv('CensusTractsTIGER2010.csv')
chi_df$GEOID10 <- as.character(chi_df$GEOID10)

# Loading the economic characteristics data from U.S Census Bureau from 2011
# to 2022 of all Cook County census tracts. Joining each data-frame vertically.
econ_df <- filepath_join('~/Downloads/Economic Characteristics')

# Removing all of the columns containing margins of error.
cols_to_remove <- sapply(econ_df, function(column) any(grepl('Margin of Error', column)))
econ_df <- econ_df[, !cols_to_remove]

# Turning the year variable numeric.
econ_df$YEAR <- as.numeric(str_extract(rownames(econ_df), '\\d{4}'))

# Removing the first row with variable descriptions.
rows_to_remove <- apply(econ_df, 1, function(row) any(grepl('Geography', row)))
econ_df <- econ_df[!rows_to_remove, ]

# Re-making the GEOID variable to only include the Census Tract numbers and
# match the GEOID variables in the Chicago data frame.
econ_df$GEO_ID <- str_extract(econ_df$GEO_ID, '(?<=US)\\d+')

# Filtering the Cook County dataframe to only include observations from Chicago
# Census Tracts.
econ_df <- econ_df[econ_df$GEO_ID %in% chi_df$GEOID10, ]

# Creating a vector with only the variables of interest in the project, and
# filtering .
kept_vars <- c('DP03_0004PE', 'DP03_0007PE', 'DP03_0009PE', 'DP03_0021PE',
               'DP03_0027PE', 'DP03_0028PE', 'DP03_0029PE', 'DP03_0030PE',
               'DP03_0031PE', 'DP03_0034PE', 'DP03_0035PE', 'DP03_0036PE',
               'DP03_0037PE', 'DP03_0038PE', 'DP03_0039PE', 'DP03_0040PE',
               'DP03_0041PE', 'DP03_0042PE', 'DP03_0043PE', 'DP03_0052PE',
               'DP03_0053PE', 'DP03_0054PE', 'DP03_0055PE', 'DP03_0056PE',
               'DP03_0057PE', 'DP03_0058PE', 'DP03_0059PE', 'DP03_0060PE',
               'DP03_0061PE', 'DP03_0062E', 'DP03_0119PE', 'GEO_ID', 'NAME',
               'YEAR')

econ_df <- econ_df[, names(econ_df) %in% kept_vars]

# Creating a vector with the names of the census tracts surrounding the
# train station (i.e treatment). Changing the NAME variable in the main
# data-frame. Creating the TREATMENT variable.
treat_obs <- c('8424', '4402.01', '8343', '4402.02', '4401.02', '7302.02',
               '8340', '4909.02', '4401.01', '5002', '4403', '4407', '4406',
               '4408', '4409', '4701', '4803', '7108', '4802', '4910', '4911',
               '7109', '7110', '7114', '4903', '4904', '4905', '4906', '4907',
               '4908', '7304', '7305', '7306', '7307', '7115', '7301', '5001',
               '7302.01', '6913', '6912', '6914', '6915', '6813', '7101')

econ_df$NAME <- gsub('[^0-9.]', '', econ_df$NAME)

econ_df$TREATMENT <- as.numeric(econ_df$NAME %in% treat_obs)

# Converting all analysis variables into numeric variables.
econ_df[kept_vars] <- lapply(econ_df[kept_vars], as.numeric)

# Preparing data for clustering.
econ_df <- econ_df[complete.cases(econ_df),]

cluster_vars <- setdiff(kept_vars, c('GEO_ID', 'NAME', 'YEAR'))
cluster_data <- econ_df[cluster_vars]

# Setting the number of clusters.
k <- 3

# Running k-means clustering.
set.seed(123)
clusters <- kmeans(cluster_data, centers = k)
econ_df$clusters <- clusters$cluster

# Calculating the variable means for the treatment group
treatment_means <- econ_df %>%
  filter(TREATMENT == 1) %>%
  summarise(across(all_of(cluster_vars), ~ mean(., na.rm = TRUE)))

# Calculating the distance of each cluster's centroid to the treatment means.
centroid_dist <- sapply(1:k, function(i) {
  sum((clusters$centers[i,] - unlist(treatment_means))^2)
})

# Finding clusters closest to the treatment group.
closest_cluster <- which.min(centroid_dist)

# Creating data-frame with observations in the closest cluster
clc_df <- econ_df %>%
  filter(clusters == closest_cluster)

# Filtering the large data set to only include closest cluster variables.
sample_df <- econ_df %>%
  filter(NAME %in% clc_df$NAME)

# Creating the after & before variable.
sample_df <- sample_df %>%
  mutate(AFTER = ifelse(YEAR >= 2019, 1, 0))
sample_df$BEFORE <- 1 - sample_df$AFTER

# Bundling income variables
sample_df <- sample_df %>%
  mutate(UNDER_25K = DP03_0052PE + DP03_0053PE + DP03_0054PE)

sample_df <- sample_df %>%
  mutate(BTWN_25K_AND_50K = DP03_0055PE + DP03_0056PE)

sample_df <- sample_df %>%
  mutate(ABOVE_50K = DP03_0057PE + DP03_0058PE + DP03_0059PE + DP03_0060PE +
           DP03_0061PE)

# Plotting
ggplot(sample_df, aes(x = YEAR, y = DP03_0021PE)) +
  geom_point() +
  labs(title = 'Public transport use across time', x = 'Year',
       y = '% of Commuters using Public Transport use per Census Tract')

ggplot(sample_df, aes(x = YEAR, y = DP03_0004PE)) +
  geom_point() +
  labs(title = 'Employment in the labor force', x = 'Year',
       y = '% of Civilians in Labor Force who are Employed use per Census Tract')

# Initial Difference-in-Differences model
model1 <- lm(DP03_0009PE ~ AFTER*TREATMENT + AFTER + TREATMENT + UNDER_25K +
               BTWN_25K_AND_50K + ABOVE_50K + DP03_0007PE + BEFORE + YEAR^2 +
               YEAR^3, 
             data = sample_df)
summary(model1)

model2 <- lm(DP03_0004PE ~ AFTER*TREATMENT + AFTER + TREATMENT + UNDER_25K +
               BTWN_25K_AND_50K + ABOVE_50K + DP03_0007PE + BEFORE + YEAR^2 +
               YEAR^3, 
             data = sample_df)
summary(model2)

model3 <- lm(DP03_0031PE ~ AFTER*TREATMENT + AFTER + TREATMENT + UNDER_25K +
               BTWN_25K_AND_50K + ABOVE_50K + DP03_0007PE + BEFORE + YEAR^2 +
               YEAR^3, 
             data = sample_df)
summary(model3)

model4 <- lm(DP03_0027PE ~ AFTER*TREATMENT + AFTER + TREATMENT + UNDER_25K +
               BTWN_25K_AND_50K + ABOVE_50K + DP03_0007PE + BEFORE + YEAR^2 +
               YEAR^3, 
             data = sample_df)
summary(model4)

model5 <- lm(DP03_0028PE ~ AFTER*TREATMENT + AFTER + TREATMENT + UNDER_25K +
               BTWN_25K_AND_50K + ABOVE_50K + DP03_0007PE + BEFORE + YEAR^2 +
               YEAR^3, 
             data = sample_df)
summary(model5)

# Adding the UNIX time column


# Exporting the CSV for GIS
econ_df$GEO_ID <- as.double(econ_df$GEO_ID)
write.csv(econ_df, 'GIS_Data.csv', row.names = FALSE)
