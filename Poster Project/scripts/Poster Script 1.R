# Author: Devan Arnold
# Date: 10/10/23
# Updated: 10/12/23
#
# Created to assist with the creation of economic analysis for a poster presentation
# Project as part of ECO 530 offered at the University of Maine. 

# Libraries
library(modelsummary)
library(tidyverse)
library(lmtest)
library(sandwich)
library(estimatr)
library(vtable)
library(kableExtra)
library(ggplot2)
library(tidycensus)
library(sjmisc)

# File paths
generalpath <- "F:/Users/Devan/Documents/Education/ECO530/eco530/Poster Project"
datapath <- "F:/Users/Devan/Documents/Education/ECO530/eco530/Poster Project/data"
scriptpath <- "F:/Users/Devan/Documents/Education/ECO530/eco530/Poster Project/scripts"
tablesfigurespath <- "F:/Users/Devan/Documents/Education/ECO530/eco530/Poster Project/tables and figures"

apikeyfilepath <- "F:/Users/Devan/Documents/Education/ECO530/2 - Poster Project/Part 1/data/censusAPIKey.txt"

# Creates color palettes for plots or graphics
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalettet <- c("#99999950", "#E69F0050", "#56B4E950", "#009E7350", "#F0E44250", "#0072B250", "#D55E0050", "#CC79A750")


################################################################################
# Set up
################################################################################
# Sets working directory to project folder (stored in variable 'generalpath')
setwd(generalpath)

# Get api Key for TidyCensus
censusApi <- readChar(apikeyfilepath,file.info(apikeyfilepath)$size)

# Applies the API to allow census data querying
census_api_key(censusApi)

census.data <- data.frame()
census.usable <- data.frame(matrix(data = NA,nrow=0,ncol=5))

YEAR <- (2010:2019)
SCHL <- c("bb",as.character(1:24))
COW <- c("b",as.character(1:9))

################################################################################
# Legacy Method to get census data from Census API
################################################################################
# Starts a for loop to iterate over desired years of data
#for(i in YEAR){
# Uses TodyCensus package to query data from us census bureau and adds the year to the 
# resulting data frame
#  census.data <- get_pums(variables = c("ST","COW","SCHL","WAGP"),year=i,state = "all",survey = "acs1")
#  census.data$year <- i
#  
# Takes state identifiers from census data frame and stores as a vector
#  STATE <- unique(census.data$ST)
# Creates variable for assignment to census.usable data frame later
#  dis_year <- i
#
# Loops over YEAR, STATE, SCHL, and COW values to create all possible categories 
# from the ACS 1 year survey for each year of data
#  for(j in 1:length(STATE)){
# Similar to dis_year but for state identifiers
#    dis_state <- STATE[j]
#     
#     for(k in 1:length(SCHL)){
# Similar to dis_year but for years of schooling
#       dis_schl <- SCHL[k]
#       
#       for(l in 1:length(COW)){
# Similar to dis_year but for class of worker
#         dis_COW <- COW[l]
#         
#         Creates a temporary data frame containing values for a certain year and 
#         STATE/SCHL/COW combimation, then transforms the WAGP term into an integer
#         temp.data <- filter(census.data,COW == dis_COW & SCHL == dis_schl & ST == dis_state)
#         temp.data$WAGP <- as.integer(temp.data$WAGP)
#         
#         Creates a mean WAGP value for the category
#         dis_wagp <- round(mean(temp.data$WAGP),digits=0)
#         
#         Handles the NaN case
#         if(is.nan(dis_wagp)){
#           dis_wagp <- NA
#         }
#         
#         Creates a row vector for binding to census.usable data frame
#         newor <- c(dis_year,dis_state,dis_schl,dis_COW,dis_wagp)
#         Applies the new row to the census.usbale data frame
#         census.usable <- rbind(census.usable,newor)
#       }
#     }
#   }
#   
#   Names the census.usable data frame columns
#   colnames(census.usable) <- c("YEAR", "STATE", "SCHL","COW","WAGP")
#   Saves the census.usbale frame and the census data for a certain year
#   save(census.usable,file = "censususable.Rda")
#   save(census.data,file = paste("censusdata(",i,").Rda",sep = ""))
# }
################################################################################
# It worked... whew lol

################################################################################
# Manipulation of census.usbale to allow for analysis
################################################################################
# Creates a new census.usable to allow for more mutation of the raw census data
for(i in YEAR){
# Gets a raw census data frame from the above census query process for the year of
# interest
 target_file <- paste(datapath,"/censusdata(",i,").Rda",sep="")
 load(target_file)
 # Loads a vector with the state identifiers in the raw census data
 STATE <- unique(census.data$ST)
 # Loops through all STATE, SCHL, and COW values for a given year to create all
 # possible categories (combination of above variables)
 for(j in STATE){
   for(k in SCHL){
     for(l in COW){
       # Creates a vector of YEAR, STATE, SCHL, COW variables to create a category
       new_combo <- c(i,j,k,l)
       # Assigns new category to census.usable data frame
       census.usable <- rbind(census.usable,new_combo)
     }
   }
 }
}
# Applies appropriate labels to the census.usable data frame
colnames(census.usable) <- c("YEAR", "STATE", "SCHL","COW")

# Creates calculated WAGP values from raw census data to apply to census.usable
for(i in 1:nrow(census.usable)){
  # Primes the target year for later use
  tar_year <- census.usable$YEAR[i]
  # Determines if the currently loaded raw census data is for the desired year
  if(census.data$year[1]==tar_year){
    # No action taken if true
  } else {
    # If false, loads the census data for the appropriate year
    target_file <- paste(datapath,"/censusdata(",tar_year,").Rda",sep="")
    load(target_file)
  }
  # Creates variables for filtering of raw census data from the category of current iteration
  tar_cow <- census.usable$COW[i]
  tar_schl <- census.usable$SCHL[i]
  tar_state <- census.usable$STATE[i]
  # Creates a temporary data frame of all raw census values for a certain STATE, SCHL, COW variables
  temp.data <- filter(census.data,COW == tar_cow & SCHL == tar_schl & ST == tar_state)
  # Coverts WAGP to an integer value for mathematical manipulation
  temp.data$WAGP <- as.integer(temp.data$WAGP)
  
  # Creates a sum total, count of observations, and average value for WAGP in temp.data
  cat_sum <- sum(temp.data$WAGP)
  cat_count <- nrow(temp.data)
  cat_mean <- round(mean(temp.data$WAGP),digits = 0)
  
  # Adds the calculated values to the census.usable data frame
  census.usable$WAGP <- cat_mean
  census.usable$WAGP.SUM <- cat_sum
  census.usable$CATEGORY.COUNT <- cat_count
}

# Saves the census.usable data frame 
save(census.usable,file = "censususable.Rda")