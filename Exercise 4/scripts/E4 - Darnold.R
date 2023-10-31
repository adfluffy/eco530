# Author: Devan Arnold
# Exercise 4
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
datafolder <- "~/Documents/GitHub/eco530/Exercise 4/data"
scriptsfolder <- "~/Documents/GitHub/eco530/Exercise 4/scripts"
tablesfigures <- "~/Documents/GitHub/eco530/Exercise 4/tables and figures"

# Prepares graph color palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalettet <- c("#99999950", "#E69F0050", "#56B4E950", "#009E7350", "#F0E44250", "#0072B250", "#D55E0050", "#CC79A750")

################################################################################
# Question 1
################################################################################
# Note: Food expenditure (food.expend) is the outcome (dependent) variable for 
# the following works

# Loads the provided nica.rbd data frame into the global environment
load(paste(datafolder,"nicaRBD.RData",sep = "/"))

# Create new data frame that has:
#   1) No missing values (N/A or otherwise)
#   2) Contains:
#     a) food.expend
#     b) land
#     c) hadloan
#     d) crop
temp.data <- nica.rbd %>% dplyr::select(food.expend,land,hadloan,crop)
# Ensures that there are no NA values in the new e4.data data frame
e4.data <- na.omit(temp.data)
# I also ran the summary table command on the e4.data data set to ensure that the
# hadloan, land, and food.expend variables did not contain any obviously coded
# 'blank' values (such as 999). No such values were observed, so I conclude that
# the set must be complete per the outline of objectives in Question 1
st(e4.data)

# Creates a variable to store the final suary table output as a kable object
e4.data.table <- sumtable(e4.data,out = "kable") %>% kable_classic()
# Exports to the viewer
e4.data.table



################################################################################
# Question 2
################################################################################
# Creates model for the relationship of land and food.expend using linear regression
# with heteroskedastic robust standard errors
model.q2 <- lm_robust(food.expend~land,data = e4.data,se_type = "HC1")

# Creates variables for the slope and intercept for model.q2
slope.q2 <- model.q2$coefficients[2]
intercept.q2 <- model.q2$coefficients[1]
# Creates a plot of land-food.expend using values from e4.data and includes the
# linear regression relationship estimated in model.1 as a line
plot.q2 <- ggplot(e4.data,aes(x=land,y=food.expend)) +
            geom_point() + geom_abline(slope = slope.q2,
                                       intercept = intercept.q2)
# Publishes plot
plot.q2



################################################################################
# Question 3
################################################################################
# Creates a linear regression model of the relationship between food.expend and 
# land, this time conditioned on whether the individual had taken a loan (hadloan)
model.q3 <- lm_robust(food.expend~land+hadloan,data = e4.data,se_type = "HC1")

# Creates the slope values for the conditions hadloan = 1 (slope.q3.1) and 
# hadloan = 0 (slope.q3.2)
slope.q3 <- model.q3$coefficients[2]
# Creates a variable for the intercept for model.q3. The intercepts are varied
# depending on whether hadloan = 1 (intercept.q3.1) or hadloan = 0 (intercept.q3.2)
intercept.q3.1 <- model.q3$coefficients[1] + model.q3$coefficients[3]
intercept.q3.2 <- model.q3$coefficients[1]
# Creates 2 plots for the new linear estimate - one that contains the data 
# points from e4.data (plot.q3.1) and one that is only the estimated relationship
# of land and food.expend conditioned on hadloan (plot.q3.2)
plot.q3 <- ggplot(e4.data,aes(x=land,y=food.expend)) + 
  geom_abline(aes(slope = slope.q3 ,intercept = intercept.q3.1,color=1)) +
  geom_abline(aes(slope = slope.q3,intercept = intercept.q3.2,color=0)) + 
  ylim(0,3000) + xlim(0,300) +
  annotate(geom = "text",x=100,y=1500,label="hadloan = 1", color=1) +
  annotate(geom = "text",x=100,y=2100,label="hadloan = 0")

# Publishes plots
plot.q3



################################################################################
# Question 4
################################################################################