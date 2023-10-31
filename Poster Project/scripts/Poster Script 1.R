# Author: Devan Arnold
# Date: 10/10/23
# Updated: 10/15/23
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
library(scales)

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
SCHL <- c(as.character(0:24))
COW <- c(as.character(0:9))

################################################################################
# Get Raw Census Data and save to machine
################################################################################
# Starts a for loop to iterate over desired years of data
for(i in YEAR){
  # Uses TidyCensus package to query data from us census bureau and adds the year to the 
  # resulting data frame
  census.data <- get_pums(variables = c("ST","COW","SCHL","WAGP"),year=i,state = "all",survey = "acs1")
  census.data$year <- i
  
  #Takes state identifiers from census data frame and stores as a vector
  STATE <- unique(census.data$ST)
  # Creates variable for assignment to census.usable data frame later
  dis_year <- i

  # Loops over YEAR, STATE, SCHL, and COW values to create all possible categories 
  # from the ACS 1 year survey for each year of data
  for(j in 1:length(STATE)){
    # Similar to dis_year but for state identifiers
    dis_state <- STATE[j]
     
     for(k in 1:length(SCHL)){
       # Similar to dis_year but for years of schooling
       dis_schl <- SCHL[k]
       
       for(l in 1:length(COW)){
         # Similar to dis_year but for class of worker
         dis_COW <- COW[l]
         
         # Creates a temporary data frame containing values for a certain year and 
         # STATE/SCHL/COW combination, then transforms the WAGP term into an integer
         temp.data <- filter(census.data,COW == dis_COW & SCHL == dis_schl & ST == dis_state)
         temp.data$WAGP <- as.integer(temp.data$WAGP)
         
         # Creates a mean WAGP value for the category
         dis_wagp <- round(mean(temp.data$WAGP),digits=0)
         
         # Handles the NaN case
         if(is.nan(dis_wagp)){
           dis_wagp <- NA
         }
         
         # Creates a row vector for binding to census.usable data frame
         newor <- c(dis_year,dis_state,dis_schl,dis_COW,dis_wagp)
         # Applies the new row to the census.usable data frame
         census.usable <- rbind(census.usable,newor)
       }
     }
   }
   
   # Names the census.usable data frame columns
   colnames(census.usable) <- c("YEAR", "STATE", "SCHL","COW","WAGP")
   # Saves the census.usable frame and the census data for a certain year
   save(census.usable,file = "censususable.Rda")
   save(census.data,file = paste("censusdata(",i,").Rda",sep = ""))
 }
# It worked... whew lol

################################################################################
# Creation of census.usable to allow for analysis
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

################################################################################
# Manipulation of Raw Census Data for uniformity
################################################################################
for(i in YEAR){
  target_file <- paste(datapath,"/censusdata(",i,").Rda",sep="")
  load(target_file)
  # Converts 2017 and later ACS survey results to the numerical responses consistent
  # With 2016 and prior data files
  if(i >= 2017){
    # Replaces bb and b values in SCHL and COW, respectively, with 0
    census.data$SCHL[census.data$SCHL=="bb"] <- "0"
    census.data$COW[census.data$COW=="b"] <- "0"
    # Converts to integer and then back to character to remove leading 0's
    census.data$SCHL <- as.character(as.integer(census.data$SCHL))
    census.data$COW <- as.character(as.integer(census.data$COW))
  }
  save(census.data,file = paste("censusdata(",i,").Rda",sep = ""))
}

################################################################################
# Populate the census.usable data frame from raw census data
################################################################################
# Creates calculated WAGP values from raw census data to apply to census.usable
bad_cow <- c("")
bad_schl <- c("")
bad_state <- c("")

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
  
  # Tests for mismatches in expected data values and actual values
  test_cow <- tar_cow %in% census.data$COW
  if (!test_cow){bad_cow <- append(bad_cow,paste(tar_cow,tar_year,sep=" "))}
  test_schl <- tar_schl %in% census.data$SCHL
  if (!test_schl){bad_schl <- append(bad_schl,paste(tar_schl,tar_year,sep=" "))}
  test_state <- tar_state %in% census.data$ST
  if (!test_state){bad_state <- append(bad_state,paste(tar_state,tar_year,sep=" "))}
  
  # Creates a temporary data frame of all raw census values for a certain STATE, SCHL, COW variables
  temp.data <- filter(census.data,COW == tar_cow & SCHL == tar_schl & ST == tar_state)
  # Coverts WAGP to an integer value for mathematical manipulation
  temp.data$WAGP <- as.integer(temp.data$WAGP)

  
  # Creates a sum total, count of observations, and average value for WAGP in temp.data
  cat_sum <- sum(temp.data$WAGP)
  cat_count <- nrow(temp.data)
  cat_mean <- round(mean(temp.data$WAGP),digits = 0)
  
  # Adds the calculated values to the census.usable data frame for the specific row
  census.usable$WAGP[i] <- cat_mean
  census.usable$WAGP.SUM[i] <- cat_sum
  census.usable$CATEGORY.COUNT[i] <- cat_count
}

# Saves the census.usable data frame 
census.usable$STATE <- as.character(as.integer(census.usable$STATE))
census.usable[is.nan(census.usable$WAGP),"WAGP"] <- 0
save(census.usable,file = paste(datapath,"/censususable.Rda",sep = ""))

################################################################################
# Creates data frame for first relationship of interest -
# HS Graduate average earnings across all worker classes and per pupil primary
# and secondary education spending, indexed by year and state
################################################################################
# Imports the CCD survey data for state primary and secondary education expenses (2010-2019)
load(paste(datapath,"/censususable.Rda",sep = ""))
education.expense <- read.csv(file = paste(datapath,"/Education Expendatures by State (2010-2019).csv",sep = ""),header = T)
# Loads table of monetary conversion rates to 2010 dollars
dollar.discounts <- read.csv(file=paste(datapath,"/Dollar Discounts.CSV",sep = ""),header = T)
# Primes the list of state codes
STATE <- unique(education.expense$State.Code)
# Creates data frame to hold data of interest
wages.edexpense <- data.frame()
# Identifies the education level of interest (16 = HS diploma)
ed_level <- c(16:24)
#ed_level <- as.character(ed_level)
worker_class <- c(1:9)
#worker_class <- as.character(worker_class)

# Iterates over all state/year combinations and adds the average wage (WAGP) and the
# per pupil education expense for the specific state/year combination from the education.expense
# data frame
for(i in STATE){
  for(j in YEAR){
    # Gets the discount factor to convert to 2010 dollars
    discount <- dollar.discounts$Discount.Factor[dollar.discounts$Year==j]
    # Stores the WAGP and CATEGORY.COUNT variables as vectors to allow for calculation of
    # an average that is weighted on worker counts. The alternative would be to average
    # the wages across each COW, but each COW has inconsistent observbation counts
    # across state and year. As such, a weighted average on the number of observations
    # of SCHL = 16 provides a standard not based on COW. 
    wage_vect <- census.usable$WAGP[census.usable$YEAR==j & 
                                      census.usable$STATE==i & 
                                      census.usable$SCHL %in% ed_level & 
                                      census.usable$COW %in% worker_class]
    count_vect <- census.usable$CATEGORY.COUNT[census.usable$YEAR==j & 
                                                 census.usable$STATE==i & 
                                                 census.usable$SCHL %in% ed_level & 
                                                 census.usable$COW %in% worker_class]
    # Calculates the weighted average 
    total_wage <- sum(wage_vect * count_vect) / sum(count_vect)
    # Converts to 2010 dollars
    total_wage <- total_wage * discount
    # Rounds to nearest whole number  
    total_wage <- round(total_wage,digits = 0)
    # Pulls the per pupil state funding from the education.expense data frame
    state_expend <- education.expense[education.expense$State.Code==i, paste("Year",j,sep=".")]
    # Converts to 2010 dollars and rounds to nearest whole number
    state_expend <- state_expend * discount
    state_expend <- round(state_expend,digits = 0)
    # Adds a new row to the expend_schl16 data frame with the index information and
    # the calculated values of interest
    wages.edexpense <- rbind(wages.edexpense,c(i,j,total_wage,state_expend))
  }
}
# Applies appropriate names to the expend_schl16 data frame and saves the file for
# later use
colnames(wages.edexpense) <- c("STATE","YEAR","WAGP","EXPENDITURE")
save(wages.edexpense, file = paste(datapath,"/expenditures for education lvl 16.Rda",sep = ""))

# NOTE: Above code has be replaced in usage by the earningByEducation function

################################################################################
# Below section is for the creation of graphics based on previously prepared data
################################################################################
# Uses the earningsByEducation function to create a data frame containing wages
# by state and year for the specific year range (default 2010-2019) and for the 
# desired COW and SCHL codes. Below code is run with min SCHL of 16 for HS diploma
# and min COW of 1 to exclude persons that have never worked due to age. 

# Plot 1
wages.edexpense <- data.frame()
wages.edexpense <- earningsByEducation(min_cow = 1)

wages.edexpense$ST.CODE <- as.integer(wages.edexpense$ST.CODE)
wages.edexpense$WAGP <- as.integer(wages.edexpense$WAGP)
wages.edexpense$EXPENDITURE <- as.integer(wages.edexpense$EXPENDITURE)



x_max <- ceiling(max(wages.edexpense$EXPENDITURE)/1000)*1000
x_min <- floor(min(wages.edexpense$EXPENDITURE)/1000)*1000

x_step <- (x_max - x_min)/8
x_scale <- seq(x_min,x_max,x_step)

y_max <- ceiling(max(wages.edexpense$WAGP)/1000)*1000
y_min <- floor(min(wages.edexpense$WAGP)/1000)*1000

y_step <- (y_max - y_min)/10
y_scale <- seq(y_min,y_max,y_step)

plot1 <- ggplot(data = wages.edexpense,aes(x=EXPENDITURE,y=WAGP)) + 
  geom_point() + 
  labs(title = "Figure 1:\nWages and Primary/Secondary Education Expenditures by State (2010-2019)",
       subtitle = "All Education Levels") +
  scale_x_continuous(name = "Education Expenditure per Pupil",
                     labels = dollar_format(),
                     limits = c(x_scale[1],x_scale[length(x_scale)]),
                     breaks = x_scale) +
  scale_y_continuous(name = "Average Annual Wages",
                     labels = dollar_format(),
                     limits = c(y_scale[1],y_scale[length(y_scale)]),
                     breaks = y_scale)

plot1

# Plot 2
wages.edexpense <- data.frame()
wages.edexpense <- earningsByEducation(min_education = 16,min_cow = 1)

wages.edexpense$ST.CODE <- as.integer(wages.edexpense$ST.CODE)
wages.edexpense$WAGP <- as.integer(wages.edexpense$WAGP)
wages.edexpense$EXPENDITURE <- as.integer(wages.edexpense$EXPENDITURE)


y_max <- ceiling(max(wages.edexpense$WAGP)/1000)*1000
y_min <- floor(min(wages.edexpense$WAGP)/1000)*1000

y_step <- (y_max - y_min)/10
y_scale <- seq(y_min,y_max,y_step)

plot2 <- ggplot(data = wages.edexpense,aes(x=EXPENDITURE,y=WAGP)) + 
            geom_point() + 
            labs(title="Figure 2:\nWages and Primary/Secondary Education Expenditures by State (2010-2019)",
                 subtitle = "High School Diploma holders and higher Educational Attainment") +
            scale_x_continuous(name = "Education Expenditure per Pupil",
                               labels = dollar_format(),
                               limits = c(x_scale[1],x_scale[length(x_scale)]),
                               breaks = x_scale) +
            scale_y_continuous(name = "Average Annual Wages",
                               labels = dollar_format(),
                               limits = c(y_scale[1],y_scale[length(y_scale)]),
                               breaks = y_scale)

plot2

# Plot 3
wages.edexpense <- data.frame()
wages.edexpense <- earningsByEducation(min_education = 21,min_cow = 1)

wages.edexpense$ST.CODE <- as.integer(wages.edexpense$ST.CODE)
wages.edexpense$WAGP <- as.integer(wages.edexpense$WAGP)
wages.edexpense$EXPENDITURE <- as.integer(wages.edexpense$EXPENDITURE)


y_max <- ceiling(max(wages.edexpense$WAGP)/1000)*1000
y_min <- floor(min(wages.edexpense$WAGP)/1000)*1000

y_step <- (y_max - y_min)/10
y_scale <- seq(y_min,y_max,y_step)

plot3 <- ggplot(data = wages.edexpense,aes(x=EXPENDITURE,y=WAGP)) + 
  geom_point() + 
  labs(title="Figure 3:\nWages and Primary/Secondary Education Expenditures by State (2010-2019)",
       subtitle = "Bachelor's Degree Recipients and higher Educational Attainment") +
  scale_x_continuous(name = "Education Expenditure per Pupil",
                     labels = dollar_format(),
                     limits = c(x_scale[1],x_scale[length(x_scale)]),
                     breaks = x_scale) +
  scale_y_continuous(name = "Average Annual Wages",
                     labels = dollar_format(),
                     limits = c(y_scale[1],y_scale[length(y_scale)]),
                     breaks = y_scale)

plot3



# Tables
wages.edexpense <- data.frame()
wages.edexpense <- earningsByEducation(min_cow = 1)

wages.edexpense$ST.CODE <- as.integer(wages.edexpense$ST.CODE)
wages.edexpense$WAGP <- as.integer(wages.edexpense$WAGP)
wages.edexpense$EXPENDITURE <- as.integer(wages.edexpense$EXPENDITURE)


table1 <- tapply(X=wages.edexpense$WAGP,INDEX = list(wages.edexpense$ST.NAME,wages.edexpense$YEAR),FUN = function(x) x)
table1 <- as.table(table1) 
kable1 <- table1 %>% kable(align = "c",
                           caption = "Table 1: Average Annual Wages by State",
                           format.args = list(big.mark = ","),
                           digits = 0) 
kable1 <- footnote(kable1,general = "Values reported in 2010 dollars\nValues are weighted mean income for workers that have entered the work force and have at least a High School Diploma or equivalent") 

kable_classic_2(kable1)

table2 <- tapply(X=wages.edexpense$EXPENDITURE,INDEX = list(wages.edexpense$ST.NAME,wages.edexpense$YEAR),FUN = function(x) x)
table2 <- as.table(table2) 
kable2 <- table2 %>% kable(align = "c",
                           caption = "Table 2: Education Expense by State",
                           format.args = list(big.mark = ","),
                           digits = 0) 
kable2 <- footnote(kable2,general = "Values reported in 2010 dollars\nValues are on a per pupil basis") 

kable_classic_2(kable2)

table3 <- tapply(X=wages.edexpense$WAGP,INDEX = wages.edexpense$YEAR,FUN = mean)
table4 <- tapply(X=wages.edexpense$EXPENDITURE,INDEX = wages.edexpense$YEAR,FUN = mean)


table5 <- cbind(table3,table4)
table5 <- as.table(table5)

kable3 <- table5 %>% kable(align = "c",
                           caption = "Table 1: Average Wages and Educational Expense by Year",
                           format.args = list(big.mark = ","),
                           digits = 0,
                           col.names = c("Average Annual Wage","Average Education Expenditure (per pupil)")) 
kable3 <- footnote(kable3,general = "All values reported in 2010 dollars\nAnnual wages are non-weighted and by state\nEducational exspenses are a per pupil average across states") 

kable_classic_2(kable3)
