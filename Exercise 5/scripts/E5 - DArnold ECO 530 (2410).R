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
library(rdrobust)
library(rddensity)
library(gridExtra)
library(kableExtra)
library(knitr)


# Prepares file path variables
exercisepath <- "//OPENMEDIAVAULT/MyCloudHome/Education/UMaine Files/ECO 530/eco530/Exercise 5"
datapath <- "/data/"
scriptpath <- "/scripts/"
tablesfigurespath <- "/tables and figures/"

# Prepares graph color palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalettet <- c("#99999950", "#E69F0050", "#56B4E950", "#009E7350", "#F0E44250", "#0072B250", "#D55E0050", "#CC79A750")

# Sets the working directory and opens the data file
setwd(exercisepath)
load(paste(exercisepath,datapath,"lb-data.RDATA",sep=""))

# Creates a new data frame for later use
plot.df <- lmb_data
################################################################################
# Task 2
################################################################################
# So my dumb monkey brain can comprehend all of the variables in the data set
sumtable(lmb_data)

# Sets the number of bins to equal the number of observed districts
bin_count.2 <- length(unique(plot.df$district))
# Creates an RDD plot for the relationship
plot2 <- rdplot(y=plot.df$score,x=plot.df$lagdemvoteshare,c=0.5,nbins = bin_count.2,
                title = "Total Effect of Initial Win on Future ADA Scores",
                x.label = "Democratic Vote Share, time t",
                y.label = "ADA Score, time t+1")

# Creates an RDD object and reports the determined gap
task2 <- rdrobust(y=plot.df$score,x=plot.df$lagdemvoteshare,c=0.5)
task2$Estimate[1]


################################################################################
# Task 3
################################################################################
# Creates the predictive model to determine probability of a democrat victory in time
# t+1
probability.D.model <- lm_robust(democrat~lagdemvoteshare + incmbncy + lagdemvoteshare*incmbncy,plot.df,se_type='HC1')
# Adds predicted values to the plot.df data frame
plot.df <- plot.df %>% mutate("prob_D" = predict(probability.D.model,plot.df))

# Sets the number of bins to equal the number of observed districts
bin_count.3 <- length(unique(plot.df$district))
# Creates an RDD plot for the relationship
plot3 <- rdplot(y=plot.df$prob_D,x=plot.df$lagdemvoteshare,c=0.5,nbins = bin_count.3,
                title = "Effect of Initial Win on Winning Next Election",
                x.label = "Democratic Vote Share, time t",
                y.label = "Probability of Democrat Win, time t+1")

# Creates an RDD object and reports the determined gap
task3 <- rdrobust(y=plot.df$prob_D,x=plot.df$lagdemvoteshare,c=0.5)
task3$Estimate[1]


################################################################################
# Task 4
################################################################################
# Changes the bin count to match the number of congresses held during the data period
# instead of the number of districts
bin_count.4 <- length(unique(plot.df$district))

# Creates a variable for the percent of population that is voting eligible
plot.df <- plot.df %>% mutate("eligible_percent"=votingpop/totpop)

# Creates RDD plots for each category of interest from the paper
# Income
plot4.1 <- rdplot(y=plot.df$realincome,x=plot.df$lagdemvoteshare,c=0.5,nbins = bin_count.4,
                  title="",
                  x.label = "Democratic Vote Share, time t",
                  y.label = "Log Income")
# High School Completion percentage
plot4.2 <- rdplot(y=plot.df$pcthighschl,x=plot.df$lagdemvoteshare,c=0.5,nbins = bin_count.4,
                  title="",
                  x.label = "Democratic Vote Share, time t",
                  y.label = "High School")
# Percent of population black
plot4.3 <- rdplot(y=plot.df$pctblack,x=plot.df$lagdemvoteshare,c=0.5,nbins = bin_count.4,
                  title="",
                  x.label = "Democratic Vote Share, time t",
                  y.label = "Black")
# Percent of population eligible voters
plot4.4 <- rdplot(y=plot.df$eligible_percent,x=plot.df$lagdemvoteshare,c=0.5,nbins = bin_count.4,
                  title="",
                  x.label = "Democratic Vote Share, time t",
                  y.label = "Eligible")

# Combines the above RDD plots into one plot object, arranged into a 2x2 grid
plot4.fullplot <- grid.arrange(plot4.1$rdplot,plot4.2$rdplot,plot4.3$rdplot,plot4.4$rdplot,ncol=2)
# Plots the combined plot object
plot(plot4.fullplot)


# Creates the ln(realincome) variable in the plot.df data frame
plot.df <- plot.df %>%  mutate("ln_realincome" = log(realincome))

# Creates linear regression models for the conditions listed in the paper
task4.col1.model <- plot.df %>% lm_robust(formula = democrat~ log(realincome) + pcthighschl + pctblack + eligible_percent + totpop + factor(district) + factor(congress) + factor(district)*factor(congress),se_type = "HC1")
task4.col2.model <- plot.df %>% filter(demvoteshare >= 0.25 & demvoteshare <= 0.75) %>% 
              lm_robust(formula = democrat~ log(realincome) + pcthighschl + pctblack + eligible_percent + factor(district) + totpop + factor(congress) + factor(district)*factor(congress),se_type = "HC1")
task4.col3.model <- plot.df %>% filter(demvoteshare >= 0.40 & demvoteshare <= 0.60) %>%
              lm_robust(formula = democrat~ log(realincome) + pcthighschl + pctblack + eligible_percent + factor(district) + totpop + factor(congress) + factor(district)*factor(congress),se_type = "HC1")
task4.col4.model <- plot.df %>% filter(demvoteshare >= 0.45 & demvoteshare <= 0.55) %>%
              lm_robust(formula = democrat~ log(realincome) + pcthighschl + pctblack + eligible_percent + factor(district) + totpop + factor(congress) + factor(district)*factor(congress),se_type = "HC1")
task4.col5.model <- plot.df %>% filter(demvoteshare >= 0.48 & demvoteshare <= 0.52) %>%
              lm_robust(formula = democrat~ log(realincome) + pcthighschl + pctblack + eligible_percent + factor(district) + totpop + factor(congress) + factor(district)*factor(congress),se_type = "HC1")
# Creates the RDD estimates for each variable of interest
task4.col6.row1 <- rdrobust(y=plot.df$ln_realincome,x=plot.df$lagdemvoteshare,c=0.5,p=4)
task4.col6.row2 <- rdrobust(y=plot.df$pcthighschl,x=plot.df$lagdemvoteshare,c=0.5,p=4)
task4.col6.row3 <- rdrobust(y=plot.df$pctblack,x=plot.df$lagdemvoteshare,c=0.5,p=4)
task4.col6.row4 <- rdrobust(y=plot.df$eligible_percent,x=plot.df$lagdemvoteshare,c=0.5,p=4)
# Creates vectors of the coefficients of interest, as well as the number of observations
task4.col1 <- c(task4.col1.model$coefficients[2],task4.col1.model$coefficients[3],task4.col1.model$coefficients[4],task4.col1.model$coefficients[5],task4.col1.model$nobs)
task4.col2 <- c(task4.col2.model$coefficients[2],task4.col2.model$coefficients[3],task4.col2.model$coefficients[4],task4.col2.model$coefficients[5],task4.col2.model$nobs)
task4.col3 <- c(task4.col3.model$coefficients[2],task4.col3.model$coefficients[3],task4.col3.model$coefficients[4],task4.col3.model$coefficients[5],task4.col3.model$nobs)
task4.col4 <- c(task4.col4.model$coefficients[2],task4.col4.model$coefficients[3],task4.col4.model$coefficients[4],task4.col4.model$coefficients[5],task4.col4.model$nobs)
task4.col5 <- c(task4.col5.model$coefficients[2],task4.col5.model$coefficients[3],task4.col5.model$coefficients[4],task4.col5.model$coefficients[5],task4.col5.model$nobs)
task4.col6 <- c(task4.col6.row1$Estimate[1],task4.col6.row2$Estimate[1],task4.col6.row3$Estimate[1],task4.col6.row4$Estimate[1],task4.col1.model$nobs)


# Creates and stores a data to combine above information
task4.models <- data.frame()
task4.models <- cbind(task4.col1, task4.col2, task4.col3, task4.col4, task4.col5, task4.col6)
# Applies names to rows and columns
colnames(task4.models) <- c("All","+/-0.25","+/-0.10","+/-0.05","+/-0.02","Polynomial")
rownames(task4.models) <- c("Log Income"," Percent HS Grad","Percent Population Black","Percent Eligible Voters","Num. Obs")


# Creates kable object to display the results
task4.table <- kable(task4.models, align = "c", digits = 3)
task4.table <- task4.table %>%  kableExtra::row_spec(row = 5,bold = T,extra_css = "border-top: 1px solid")
kable_classic_2(task4.table)


################################################################################
# Clean Up
################################################################################
setwd(paste(exercisepath,tablesfigurespath,sep=""))
# Save each plot individually using ggsave()
# Plot 2
ggsave("plot2_rdplot.png", plot = plot2$rdplot)
# Plot 3
ggsave("plot3_rdplot.png", plot = plot3$rdplot)
# Plot 4.fullplot
ggsave("plot4_fullplot.png", plot = plot4.fullplot)
# Plot 4.1
ggsave("plot4_1_rdplot.png", plot = plot4.1$rdplot)
# Plot 4.2
ggsave("plot4_2_rdplot.png", plot = plot4.2$rdplot)
# Plot 4.3
ggsave("plot4_3_rdplot.png", plot = plot4.3$rdplot)
# Plot 4.4
ggsave("plot4_4_rdplot.png", plot = plot4.4$rdplot)


# Saves the task4.table object
save_kable(task4.table,file = "task4_table.html")


# Saves the plot.df data frame
setwd(paste(exercisepath,datapath,sep=""))
save(plot.df, file = "plot_df.RDA")
