# What: Software Carpentry Workshop
# When: October 18th, 2016
# Who: Yu-Chen Wang
# Where: Hatcher Library, UM

#######################################################

### This is a script for the EEB/SNRE workshop

# Clear working space
rm(list = ls())

# Package Information
# RSQLite, dplyr, ggplot2, tidyr

# Load packages
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)

# Set working directory
#setwd("C:/Users/Yu-Chen/Documents/R_Scripts/Data")

### Example of join using SQLite

# Build connection
#conn <- dbConnect(SQLite(), dbname = "survey.sqlite")

#tables <- dbListTables(conn)
#tables

#class(tables)

# Use JOIN and ON to join a table
# Use . and = to specify the column name in a table
#surveys <- dbGetQuery(conn, "SELECT * FROM surveys
#JOIN species ON surveys.species_id = species.species_id
#JOIN plots ON surveys.plot_id = plots.plot_id")

#names(surveys)
#head(surveys)
#summary(surveys)

### Disconnect to the database
#dbDisconnect(conn)
#rm(conn)



# Read the ecology csv file
surveys <- read.csv("C:/Users/Yu-Chen/Desktop/SWC_R/ecology.csv", 
                    header = TRUE, stringsAsFactors = FALSE)

# Subset data frame
class(surveys["year"])   # Return a data frame
class(surveys[, "year"]) # Return a vector
class(surveys[, 4])      # Return a vector
class(surveys[["year"]]) # Return a vector

# Change the sex column to factor
surveys$sex <- factor(surveys$sex, levels = c("F", "M"))

levels(surveys$sex)

nlevels(surveys$sex)

### order factor example in R
spice1 <- factor(c("low", "medium", "low", "high"), 
                levels = c("low", "medium", "high"))
levels(spice1)
max(spice1)
# Error in Summary.factor(c(1L, 2L, 1L, 3L), na.rm = FALSE) : 
#  ‘max’ not meaningful for factors

spice2 <- factor(c("low", "medium", "low", "high"), 
                levels = c("low", "medium", "high"),
                ordered = TRUE)
levels(spice2)
max(spice2)
# [1] high
# Levels: low < medium < high

# Ordered the factor
spice3 <- ordered(spice1, levels = c("high", "medium", "low"))
max(spice3)

### the table function
tabulation <- table(surveys$taxa)
tabulation
barplot(tabulation)

### Reorder the taxa by abundance

# Get the abundance and rank them
rank_taxa <- names(sort(tabulation, decreasing = TRUE))

# Create facotr based on the order levels
surveys$taxa <- factor(surveys$taxa, levels = rank_taxa, ordered = TRUE) 

# Create a new table
tabulation_new <- table(surveys$taxa)
barplot(tabulation_new)

### Cross-tabulation
table(surveys$year, surveys$taxa)

with(surveys, table(year, taxa))

# Question: Waht was the median weight of each rodent species between 1980 and 1990?
surveys$taxa == "Rodent"
length(surveys$taxa == "Rodent")
dim(surveys)

surveys[surveys$taxa == "Rodent", "taxa"]

# Filter data when year is between 1980 to 1990 and taxa is "Rodent"
rodent_80_90 <- surveys[surveys$year >= 1980 & surveys$year <= 1990 & surveys$taxa == "Rodent", ]

### Calculate the median using tapply
tapply(rodent_80_90$weight, INDEX = rodent_80_90$species, FUN = median, na.rm = TRUE)

### dplyr package
output <- select(surveys, year, taxa, weight)
head(output)

filter(surveys, taxa == "Rodent")

filter(select(surveys, year, taxa, weight), taxa == "Rodent")

rodent_surveys <- surveys %>%
  filter(taxa == "Rodent") %>%
  select(year, taxa, weight)

### select records of rodents from 1980 to 1990
rodent_80_90_dplyr <- surveys %>%
  filter(taxa == "Rodent", year %in% seq(1980, 1990, 1)) %>%
  select(year, taxa, weight)

surveys %>%
  mutate(weight_kg = weight/1000) %>%
  tail()

# Split, Apply, Combine
surveys %>%
  filter(!is.na(weight), year %in% 1980:1990, taxa == "Rodent") %>%
  group_by(species_id) %>%
  summarise(med_weight = median(weight)) %>%
  print(n = 18)

surveys_complete <- surveys %>%
  filter(!is.na(weight), species_id != "", 
         !is.na(hindfoot_length), sex != "", taxa == "Rodent")

common_species <- surveys_complete %>%
  group_by(species_id) %>%
  tally() %>%
  filter(n >= 50) %>%
  select(species_id)

common_surveys <- surveys_complete %>%
  filter(species_id %in% common_species$species_id) 

write.csv(common_surveys, file = "surveys_complete.csv", row.names = FALSE)

### Package ggplot2
ggplot(data = common_surveys, aes(x = weight, 
                                  y = hindfoot_length, 
                                  color = species_id)) +
  geom_point()
