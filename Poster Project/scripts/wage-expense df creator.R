earningsByEducation <- function(min_education = 0,max_education = 24,min_cow = 0,max_cow = 9,min_year = 2010, max_year = 2019){
  
  load(paste(datapath,"/censususable.Rda",sep = ""))
  education.expense <- read.csv(file = paste(datapath,"/Education Expendatures by State (2010-2019).csv",sep = ""),header = T)
  # Loads table of monetary conversion rates to 2010 dollars
  dollar.discounts <- read.csv(file=paste(datapath,"/Dollar Discounts.CSV",sep = ""),header = T)
  # Primes the list of state codes
  STATE <- unique(education.expense$State.Code)
  # Creates data frame to hold data of interest
  wage.edexpend <- data.frame()
  
  YEAR <- c(min_year:max_year)
  worker_class <- as.character(c(min_cow:max_cow))
  ed_level <- as.character(c(min_education:max_education))
  
  for(i in STATE){
    for(j in YEAR){
      # Gets the discount factor to convert to 2010 dollars
      discount <- dollar.discounts$Discount.Factor[dollar.discounts$Year==j]
      # Stores the WAGP and CATEGORY.COUNT variables as vectors to allow for calculation of
      # an average that is weighted on worker counts. The alternative would be to average
      # the wages across each COW, but each COW has inconsistent observbation counts
      # across state and year. As such, a weighted average on the number of observations
      # of SCHL = 16 provides a standard not based on COW. 
      wage_vect <- census.usable$WAGP[(census.usable$YEAR==j) & 
                                        (census.usable$STATE==i) & 
                                        (census.usable$SCHL %in% ed_level) & 
                                        (census.usable$COW %in% worker_class)]
      count_vect <- census.usable$CATEGORY.COUNT[(census.usable$YEAR==j) & 
                                         (census.usable$STATE==i) & 
                                         (census.usable$SCHL %in% ed_level) & 
                                         (census.usable$COW %in% worker_class)]
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
      # Adds the State name to the reported data frame 
      state_name <- education.expense$State.Name[education.expense$State.Code==i]
      # Adds a new row to the wage.edexpend data frame with the index information and
      # the calculated values of interest
      wage.edexpend <- rbind(wage.edexpend,c(state_name,i,j,total_wage,state_expend))
    }
  }
  # Applies appropriate names to the wage.edexpend data frame and saves the file for
  # later use
  colnames(wage.edexpend) <- c("ST.NAME","ST.CODE","YEAR","WAGP","EXPENDITURE")
  return(wage.edexpend)
}