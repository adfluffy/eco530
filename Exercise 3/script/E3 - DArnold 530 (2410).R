# Imports necessary libraries for the assignment
library(modelsummary)
library(tidyverse)
library(lmtest)
library(sandwich)
library(estimatr)
library(vtable)
library(kableExtra)
library(ggplot2)

# Creates color palettes for plots or graphics
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalettet <- c("#99999950", "#E69F0050", "#56B4E950", "#009E7350", "#F0E44250", "#0072B250", "#D55E0050", "#CC79A750")

# Sets file paths for various file types
datapath <- "F:/Users/Devan/Documents/Education/ECO530/eco530/Exercise 3/data"
scriptpath <- "F:/Users/Devan/Documents/Education/ECO530/eco530/Exercise 3/scripts"
tablesfigurespath <- "F:/Users/Devan/Documents/Education/ECO530/eco530/Exercise 3/tables and figures"


################################################################################
#  Question 1
################################################################################
# Sets the working directory to data folder and loads the nicaRBD.RData file into
# R
data_load <- paste(datapath,"/nicaRBD.RData",sep = "")
load(data_load)


# Creates a data frame for use in the assignment that filters out NA values in the
# four variables of interest from the nica.rbd data frame
e3_data <- filter(nica.rbd,!is.na(te_maize) & !is.na(food.expend) & !is.na(couldgetloan) & !is.na(writtentitle))

# Creates a table of summary statistics for te_maize and food.expend as a kable table then 
# displays in viewer
maize_food_sum <- st(e3_data,vars = c("te_maize","food.expend"),out = "kable",digits=2) %>% kable_classic()
maize_food_sum

# Creates a scatter plot with te_maize on the x axis and food.expend on the y
maize_food_plot <- ggplot(data=e3_data, aes(x=te_maize,y=food.expend)) + geom_point() +
                      xlim(0,0.9) + ylim(0,7500)
maize_food_plot



################################################################################
#  Question 2
################################################################################
# Performs a linear regression of te_mmaize on food.expend
reg_1 <- lm_robust(food.expend~te_maize,data = e3_data)
# Adds the model to a list that can be called later in the exercise
e3_fe_models <- list("Model 1"=reg_1)

# Updates the model table with the model summary from e3_models as a kable table
# including only the number of observations as additional information to the 
# model
fe_model_table <- modelsummary(e3_fe_models,output = "kableExtra",gof_map = "nobs",title = "Food Expenditures") %>% kable_classic()
# Reports the model_table object
fe_model_table

# Creates a coefficient plot for the reg_1 model and reports it to the user
reg_1_plot <- modelplot(reg_1,coef_omit = "Intercept") + labs(title="Coefficients for Model\nTE Maize on Food Expendature")
reg_1_plot



################################################################################
#  Question 3
################################################################################
# Changes the values in the writtentitle variable to 1 for true and 0 for false to 
# allow for inclusion into our model. I use a for loop here so that I can run the
# code multiple times and still have the working data in e3_data correct, as the values
# reference a data frame that has not been manipulated. 
e3_data$writtentitle <- as.integer(e3_data$writtentitle) - 1


# Runs regression models for the dependent variable food.expend conditioned on 
# couldgetloan in reg_2 and writtentitle in reg_3
reg_2 <- lm_robust(food.expend~te_maize+couldgetloan,data = e3_data)
reg_3 <- lm_robust(food.expend~te_maize+writtentitle,data = e3_data)

# Adds the above models to the e3_models list
e3_fe_models <- list("Model 1"=reg_1,"Model 2"=reg_2,"Model 3"=reg_3)

# Updates the model table with the model summary from e3_models as a kable table
# including only the number of observations as additional information to the 
# model
fe_model_table <- modelsummary(e3_fe_models,output = "kableExtra",gof_map = "nobs",title = "Food Expenditures") %>% kable_classic()
# Reports the model_table object
model_table



################################################################################
#  Question 4
################################################################################
# Below code regresses couldgetloan on te_maize. The variable couldgetloan is used
# here instead of hadloan we are looking to understand the association between 
# loan availability for individual i and the technical efficiency of their maize
# crops. If instead we wanted to know what the association between having a loan
# and the technical efficiency of the maize crops was then we would employ the
# hadloan variable. 
reg_4 <- lm_robust(te_maize~couldgetloan,data = e3_data)

# Added the te_maize model to a seperate list for reporting
e3_te_models <- list("Model 1"=reg_4)
# Created a table for te_maize as the dependent variable, similar to the tables created
# in prior problem for food.expend. 
te_model_table <- modelsummary(e3_te_models,output = "kableExtra",gof_map = "nobs",title = "Technical Efficiency of Maize") %>% kable_classic()
# Reports table to the user
te_model_table
