library(tidyverse)
library(kableExtra)

datapath <- "F:/Users/Devan/Documents/Education/ECO530/Assignments/Exercise 2/data"
scriptpath <- "F:/Users/Devan/Documents/Education/ECO530/Assignments/Exercise 2/scripts"
tablesfigurespath <- "F:/Users/Devan/Documents/Education/ECO530/Assignments/Exercise 2/tables and figures"

#------------------------------------------------------------------------------
## Question 1
#------------------------------------------------------------------------------

# Nothing here thus far

#------------------------------------------------------------------------------
## Question 2
#------------------------------------------------------------------------------
# Part (A)
# Given x~N(0,1), where mean = 0 and var = 1, find Pr(X<=1.64)
meanA <- 0
sdA <- sqrt(1)
pnorm(1.64,mean=meanA,sd=sdA,lower.tail = T)

# Part (B)
# Given X~N(42,8), where mean = 42 and var = 8, find Pr(X>=30)
meanB <- 42
sdB <- sqrt(8)
pnorm(30,mean=meanB,sd=sdB,lower.tail = F)

# Part (C)
# Given X~N(0,1), where mean = 0 and var = 1, find Pr(|X|>=1.64)
meanC <- 0
sdC <- sqrt(1)
upperC <- pnorm(1.64,mean=meanA,sd=sdA,lower.tail = F)
lowerC <- pnorm(-1.64,mean=meanA,sd=sdA,lower.tail = T)
upperC + lowerC

# Part (D)
# Given X~N(0,1), where mean = 0 and var = 1, find Pr(X<=-1.64 U X>=2.5)
meanD <- 0
sdD <- sqrt(1)
upperD <- pnorm(2.5,mean=meanA,sd=sdA,lower.tail = F)
lowerD <- pnorm(-1.64,mean=meanA,sd=sdA,lower.tail = T)
upperD + lowerD


#------------------------------------------------------------------------------
## Question 3
#------------------------------------------------------------------------------
# Part (B)
# Sets working directory to data folder for Exercise 2
setwd(datapath)
# Loads RData file for the assignment
load(file="E2data.RDATA")

# Sets the sample size and mean (mu) for later use
sample.size <- 25
mu <- 22
# Sets a seed for later review
set.seed(seed=8675309)

# Takes a sample of size sample.size from the universe data frame and stores the values in 
# the universe.sample data frame
universe.sample <- sample_n(universe,size = sample.size,replace=F)

# Calculates the values of the y_bar, y_tilde, and y_hat estimators
y_bar <- sum(universe.sample$age)/sample.size
y_tilde <- sum(universe.sample[1:5,1])/5
y_hat <- universe.sample[1,1]

# Determines the distance (in absolute value terms) of each estimator from the population mean
mu_y_bar <- abs(y_bar - mu)
mu_y_tilde <- abs(y_tilde - mu)
mu_y_hat <-  abs(y_hat - mu)

# Determines which of the above differences is least
smallest_difference <- min(c(mu_y_bar,mu_y_tilde,mu_y_hat))

# Reports the estimator that is closest to the population mean and then reports that
# difference in numerical terms. 
if(smallest_difference == mu_y_bar){print("y_bar is the closest estimator to the mean mu = 22")}
if(smallest_difference == mu_y_tilde){print("y_tilde is the closest estimator to the mean mu = 22")}
if(smallest_difference == mu_y_hat){print("y_hat is the closest estimator to the mean mu = 22")}
print(smallest_difference)


#------------------------------------------------------------------------------
## Question 4
#------------------------------------------------------------------------------
# Part (a)
# Declare variables for our estimated beta ('beta_hat'), number of observations 
# (n, stored as 'obs'), and the standard error of our estimate 'se'
beta_hat <- 6.25
obs <- 502
se <- 1.43

# Declares the acceptable bound for one tail of the distribution
bound <- 0.025

# Declare the null hypothesis values for tests 1, 2, and 3
hyp_1 <- 9.25
hyp_2 <- 2.1
hyp_3 <- 3.5

# Determines the proportion of the distribution that resides above each hypothesis
upper_1 <- pnorm(hyp_1,mean=beta_hat,sd=se,lower.tail = F)
upper_2 <- pnorm(hyp_2,mean=beta_hat,sd=se,lower.tail = F)
upper_3 <- pnorm(hyp_3,mean=beta_hat,sd=se,lower.tail = F)
# Determines the proportion of the distribution that resides below each hypothesis
lower_1 <- pnorm(hyp_1,mean=beta_hat,sd=se,lower.tail = T)
lower_2 <- pnorm(hyp_2,mean=beta_hat,sd=se,lower.tail = T)
lower_3 <- pnorm(hyp_3,mean=beta_hat,sd=se,lower.tail = T)

# Creates a boolean value to determine if each hypothesis is within the 5% unallowed
# portion of the distribution. 
hyp_test_1 <- (upper_1 > bound & lower_1 > bound)
hyp_test_2 <- (upper_2 > bound & lower_2 > bound)
hyp_test_3 <- (upper_3 > bound & lower_3 > bound)

# Reports to the console the results of the above tests in plain language
paste("For hypothesis 1 of beta =",hyp_1,"we cannot reject the hypothesis - TRUE or FALSE?",hyp_test_1,sep=" ")
paste("For hypothesis 2 of beta =",hyp_2,"we cannot reject the hypothesis - TRUE or FALSE?",hyp_test_2,sep=" ")
paste("For hypothesis 3 of beta =",hyp_3,"we cannot reject the hypothesis - TRUE or FALSE?",hyp_test_3,sep=" ")


# Part (b)
# Determines the upper bounds for a 95% and 99% CI for mean = beta_hat, standard
# deviation = se (our standard error variable)
upper_95 <- round(qnorm(0.975,mean=beta_hat,sd=se,lower.tail = T),digits = 2)
upper_99 <- round(qnorm(0.995,mean=beta_hat,sd=se,lower.tail = T),digits = 2)

# Determines the lower bounds for a 95% and 99% CI for mean = beta_hat, standard
# deviation = se (our standard error variable)
lower_95 <- round(qnorm(0.025,mean=beta_hat,sd=se,lower.tail = T),digits = 2)
lower_99 <- round(qnorm(0.005,mean=beta_hat,sd=se,lower.tail = T),digits = 2)

# Stores the upper and lower bounds of the 95% CI in the vector 'conf_95' and prints
# the values to the console
conf_95 <- c(lower_95,upper_95)
conf_95

# Stores the upper and lower bounds of the 99% CI in the vector 'conf_99' and prints
# the values to the console
conf_99 <- c(lower_99,upper_99)
conf_99


# Part (c)
# Declares the hypothesis that beta = 4 for testing. Stores value in 'hyp_c'
hyp_c <- 4

# Determines the proportion of the distribution above and below the hyp_c value
below_c <- round(pnorm(hyp_c,mean=beta_hat,sd=se,lower.tail = T),digits = 2)
above_c <- round(pnorm(hyp_c,mean=beta_hat,sd=se,lower.tail = F),digits = 2)

# Determines the lesser of the above proportions and takes that as the one-tail alpha.
# Then multiplies that value by two for a two tail test for reporting to the user. 
alpha_c <- 2*min(c(below_c,above_c))

# Displays results in console using natural language.
paste("For the hypothesis beta =",hyp_c," we would reject the hypothesis if using an alpha value of",alpha_c,sep=" ")



#------------------------------------------------------------------------------
## Question 5
#------------------------------------------------------------------------------
# Part (a)
# This bit of code just proves that there are no values above 1 or below 0 for the
# study.hard column of the universe data frame. Understanding this aspect of the data
# will be important for the approach I take later. 
max(universe$study.hard)
min(universe$study.hard)

# Takes the mean of GPAs in the full sample where case (0) is study.hard = 0 and
# where case (1) is study.hard = 1. Stores the conditional means in y0_bar for case (0)
# and y1_bar for case (1)
y0_bar <- mean(universe$gpa[universe$study.hard==0])
y1_bar <- mean(universe$gpa[universe$study.hard==1])

# Takes the sum of all GPA's divided by themselves to create a count of observations
# then subtracts the count of observations where study.hard =/= 0
n0 <- sum(universe$gpa/universe$gpa)-sum(universe$study.hard)
# Sums all of the values of study.hard where study.hard =/= 0 
n1 <- sum(universe$study.hard)

# Calculates the sample variance for case (0) (stored as S0_2) and case (1) (stored
# as s1_2).
s0_2 <- var(universe$gpa[universe$study.hard==0])
s1_2 <- var(universe$gpa[universe$study.hard==1])

# Calculates gamma_hat based on the values of y0_bar and y1_bar and reports to the console
gamma_hat <- y1_bar - y0_bar
gamma_hat


# Part (b)
# Using the above calculated s0_2, s1_2, n0, and n1 we can determine the standard
# error for gamma_hat
se <- sqrt((s1_2/n1)+(s0_2/n0))
se


# Part (c)
# In order to determine if there is a GPA effect due to studying hard, we will assume
# that there is NO GPA effect due to studying hard. If this is true, than we would expect
# that, on average, gamma_hat = 0. We will test to see if our observed gamma_hat is
# outside of values of alpha associated with a 95% CI. This means that an upper_gamma
# value less than 0.025 or a lower value less than 0.025 would cause us to reject
# the hypothesis HO: gamma_hat = 0. 
alpha = 0.025
# Determine the proportion of the distribution that is higher than our observed
# gamma_hat
upper_gamma <- pnorm(gamma_hat,mean = 0, sd=se,lower.tail = F)
upper_gamma
# Determine the proportion of the distribution that is lower than our observed
# gamma_hat
lower_gamma <- pnorm(gamma_hat,mean = 0, sd=se, lower.tail=T)
lower_gamma

# Since the value of upper_gamma is 0, we can reject the hypothesis that gamma_hat = 0
gamma_hyp_test <- ((upper_gamma < alpha)|(lower_gamma < alpha))
paste("For hypothesis of gamma_hat = 0, we reject the hypothesis - TRUE or FALSE?",gamma_hyp_test,sep=" ")


# Part (d)
# First, we will create the upper and lower bounds of the 95% CI using the qnorm
# function measured from the lower tail of the distribution
gamma_95u <- round(qnorm(0.975,mean = gamma_hat,sd=se,lower.tail = T),digits=2)
gamma_95l <- round(qnorm(0.025,mean = gamma_hat,sd=se,lower.tail=T),digits=2)

# We place the lower and upper bounds of the 95% CI a vector named 'gamma_95CI' for
# reporting to the console
gamma_95CI <- c(gamma_95l,gamma_95u)
gamma_95CI

# This confidence interval represents the values of gamma for which we would reject
# or fail to reject a hypothesis given an alpha of 0.05



#------------------------------------------------------------------------------
## Question 6
#------------------------------------------------------------------------------
# For the sake of consistency, I will label the parts of question 6 as (a), (b),
# and (d) to create a 1-1 analogy to question 5
#
# Part (a)
# This code is reused from (5a), but with a term added to condition the estimators
# on students knowing where the library is (library = 1)
max(universe$library)
min(universe$library)

# Takes the mean of GPAs in the full sample where case (0) is study.hard = 0 and
# where case (1) is study.hard = 1. Stores the conditional means in y0_bar for case (0)
# and y1_bar for case (1)
y0_bar6 <- mean(universe$gpa[universe$study.hard==0 & universe$library==1])
y1_bar6 <- mean(universe$gpa[universe$study.hard==1 & universe$library==1])

# Takes the sum of all GPA's divided by themselves to create a count of observations
# then subtracts the count of observations where study.hard =/= 0
n06 <- sum(universe$gpa[universe$library==1]/universe$gpa[universe$library==1])-sum(universe$study.hard[universe$library==1])
# Sums all of the values of study.hard where study.hard =/= 0 
n16 <- sum(universe$study.hard[universe$library==1])

# Calculates the sample variance for case (0) (stored as S0_2) and case (1) (stored
# as s1_2).
s0_26 <- var(universe$gpa[universe$study.hard==0 & universe$library==1])
s1_26 <- var(universe$gpa[universe$study.hard==1 & universe$library==1])

# Calculates gamma_hat based on the values of y0_bar and y1_bar and reports to the console
gamma_hat6 <- y1_bar6 - y0_bar6
gamma_hat6


# Part (b)
# Again, code is reused from (5b)
# Using the above calculated s0_2, s1_2, n0, and n1 we can determine the standard
# error for gamma_hat
se6 <- sqrt((s1_26/n16)+(s0_26/n06))
se6


# Part (d)
# Again, code is reused from (5d)
# First, we will create the upper and lower bounds of the 95% CI using the qnorm
# function measured from the lower tail of the distribution
gamma_95u_6 <- round(qnorm(0.975,mean = gamma_hat6,sd=se6,lower.tail = T),digits=2)
gamma_95l_6 <- round(qnorm(0.025,mean = gamma_hat6,sd=se6,lower.tail=T),digits=2)

# We place the lower and upper bounds of the 95% CI a vector named 'gamma_95CI' for
# reporting to the console
gamma_95CI_6 <- c(gamma_95l_6,gamma_95u_6)
gamma_95CI_6

# This confidence interval represents the values of gamma for which we would reject
# or fail to reject a hypothesis given an alpha of 0.05