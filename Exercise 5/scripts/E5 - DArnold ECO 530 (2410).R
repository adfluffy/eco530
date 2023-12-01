# Author: Devan Arnold
# Exercise 5: Replication of Results
#    Do Voters Affect or Elect Policies? Evidence from the US House
#    David S Lee, Enrico Moretti, Matthew J Butler (2004)
# ECO 530, Fall 2023
# Professor Jonathan Malacarne

################################################################################
# Set Up
################################################################################

# Prepares relevant libraries
library(modelsummary)
library(tidyverse)
library(lmtest)
library(sandwich)
library(estimatr)
library(vtable)
library(car)
library(multcomp)


# Prepares file path variables
exercisepath <- "//OPENMEDIAVAULT/MyCloudHome/Education/UMaine Files/ECO 530/eco530/Exercise 5"
datapath <- "//OPENMEDIAVAULT/MyCloudHome/Education/UMaine Files/ECO 530/eco530/Exercise 5"
scriptpath <- "//OPENMEDIAVAULT/MyCloudHome/Education/UMaine Files/ECO 530/eco530/Exercise 5/scripts"
tablesfigurespath <- "//OPENMEDIAVAULT/MyCloudHome/Education/UMaine Files/ECO 530/eco530/Exercise 5/tables and figures"

# Prepares graph color palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalettet <- c("#99999950", "#E69F0050", "#56B4E950", "#009E7350", "#F0E44250", "#0072B250", "#D55E0050", "#CC79A750")

################################################################################
# Task 2
################################################################################

setwd(exercisepath)



