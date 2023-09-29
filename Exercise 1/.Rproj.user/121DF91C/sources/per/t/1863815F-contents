#-------------------------------------------------------------------------------
# R1
#-------------------------------------------------------------------------------
#(a) Install/Load packages
#install.packages("tidyverse")
library(tidyverse)
#install.packages("vtable")
library(vtable)
# Above code only requires that the install.packages() functions be removed from
# comment status to be run for the first time (user should delete the leading '#')

#(b) Sets the file paths for data, scripts, and tables and figures
datapath <- "F:/Users/Devan/Documents/Education/ECO530/Assignments/Assignment 1/data"
scriptpath <- "F:/Users/Devan/Documents/Education/ECO530/Assignments/Assignment 1/scripts"
tablesfigurespath <- "F:/Users/Devan/Documents/Education/ECO530/Assignments/Assignment 1/tables and figures"

#(c) Sets the working directory to the data folder
setwd(datapath)
# Reads the csv file name 'cars' in the datapath folder and stores the data into a
# new data frame name cars.data
cars.data <- read.csv("cars.csv",header=TRUE)

#(d) Creates a summary table of the cars.data data frame
st(cars.data)


#-------------------------------------------------------------------------------
# R2
#-------------------------------------------------------------------------------
#(a) Creates a summary of the average mpg, weight, and price of cars grouped by
#    their foreign or domestic status. Stores the data in the summary.data data frame
summary.data <- cars.data %>%
  group_by(foreign) %>%
  summarize("Avg. MPG" = round(mean(mpg),digits=1),
            "Avg. Weight" = round(mean(weight),digits=0),
            "Avg. Price" = round(mean(price),digits=2))

#(b) Creates a formatted table from the summary.data data frame
summ.plot <- kable(summary.data)

#(c) Creates a new data frame named domestic.cars with only information from cars.data
#    on domestically manufactured cars.
domestic.cars <- cars.data[cars.data$foreign=="Domestic",]

#(d) Adds a column to the domestic.cars data frame that contains the value of
#    that car's price divided by its mpg rating. Rounds to 1 digit after the decimal.
domestic.cars <- domestic.cars %>% mutate(pricepermpg = round(price/mpg,digits=1))


#-------------------------------------------------------------------------------
# R3
#-------------------------------------------------------------------------------
# Creates var1, var2, and var3. Performs 250 draws from a normal distribution
# based on the mean and standard deviation provided
var1 <- rnorm(250,mean=3,sd=sqrt(1))
var2 <- rnorm(250,mean=-1,sd=sqrt(2))
var3 <- rnorm(250,mean=2,sd=sqrt(3))

# Creates the id variable for later use
id <- 1:250

# Creates the 'random.draws' data frame by combining the vectors var1, var2, and var3
random.draws <- data.frame(id,var1,var2,var3)


#-------------------------------------------------------------------------------
# R4
#-------------------------------------------------------------------------------
# Creates a scatter plot of var1 with dots, chart title, and axis labels
scatter <- ggplot(data=random.draws, aes(x=id,y=var1)) +
  geom_point() +
  labs(x="Draw",
       y= "Var1",
       title ="Scatter Plot of Var1")

# Displays the plot
scatter


#-------------------------------------------------------------------------------
# R5
#-------------------------------------------------------------------------------
# Creates a density plot of var2 with a craaaazy custom color (not default)
dens <- ggplot(data=random.draws, aes(x=var2)) +
  geom_density(fill="#ff38fc",color="#ff38fc") +
  labs(x="Var2",
       title ="Density Plot of Var2")

# Displays the plot
dens


#-------------------------------------------------------------------------------
# R6
#-------------------------------------------------------------------------------
# Creates var4 as a summation of vars 1, 2, & 3 and assigns it to the random.draws data frame
random.draws <- random.draws %>% mutate(var4 = var1 + var2 + var3)

#(a) Since var4 is a combination of vars 1-3, I suspect that the expected value
#    of var4 should be at 4. This is because E[var1]=mean_var1, and since var4 = var1 + var2 + var3,
#    E[var4] = E[var1 + var2 + var3] = E[var1] + E[var2] + E[var3] = 3 + (-1) + 2 = 4

#(b) Creates the var4 density plot with expected value line
dens4 <- ggplot(data=random.draws, aes(x=var4)) +
  geom_density(fill="#1ef1a3",color="#1ef1a3") +
  labs(x="Var4",
       title ="Density Plot of Var4") +
  geom_vline(xintercept = 4, linetype="dashed")

# Displays the plot
dens4


#-------------------------------------------------------------------------------
# R7
#-------------------------------------------------------------------------------
# Creates the var5 column in the random.draws data frame in accordance with the
# instructions for this exercise.
random.draws <- random.draws %>% mutate(var5 = (var3 - 1)/2)

#(a) I expect that the new density plot will be centered around x=0.5. This represents
#    the expected value of var5, which is E[(var3 - 1)/2]=0.5

#(b) Since we are making the values of all terms half their original size I expect
#    that there will be tighter distribution around the mean (variance5 < variance3)

#(c) Creates the density plot as well as a dashed vertical line at the expected value
#    of var5 (0.5)
dens5 <- ggplot(data=random.draws, aes(x=var5)) +
  geom_density(fill="#b7b115",color="#b7b115") +
  labs(x="Var5",
       title ="Density Plot of Var5") +
  geom_vline(xintercept = 0.5, linetype="dashed")

# Displays the plot
dens5
