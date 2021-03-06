#Week 3 Homework Part 6
# A function to rank hospitals by outcome in an state
# arguments:
#   state: 2 char abbrev state name
#   outcome: "heart attack", "heart failure", "pneumonia"
#   num: ranking of a hospital in that state for an outcome
#        takes "best", "worst", or an integer indicating rank (1 is best)
# input data: outcome-of-care-measures.csv
# output: a character vector with the name of the hospital with num rank
# if number given by num is > number of hospitals in the state, then return NA
# exclude hospitals with no data on a particular outcome when 
#  deciding rankings
# resolve ties in ranking by alphabetical order of the hospital names
# invalid state argument throws error via stop(): "invalid state"
# invalid outcome throws error via stop(): "invalid outcome"

#############################
# Variable and data set list:
# outcome.dat - data set for outcome of care measures
# state - fn. argument - state of interest
# outcome - fn. argument - outcome of interest
# num - fn. argument - ranking of interest (incl. "best", "worst")
# out.set - vector of relevant outcomes
# state.abb - built in data set of 2 letter state abbreviations
# out.col - col. numbers for outcomes of interest (11, 17, 23)
# out.stat - data for only the state of interest
# out.min - min. value for outcome of interest and state of interest (best)
# out.max - max. value for outcome of interest and state of interest (worst)
# out.hosp - data for state, hospitals, outcomes of interest


# Function template:
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome.dat <- read.csv("outcome-of-care-measures.csv",
                          colClasses = "character")
  ## define valid outcomes
  out.set <- c("heart failure", "heart attack", "pneumonia")
  
  ## Check that state and outcome are valid
  if(state %in% state.abb) {
    if(outcome %in% out.set) {
    
      # pull out data for relevant state and variables
      out.state <- outcome.dat[which(outcome.dat$State==state), 
                               c(2,7,11,17,23)]
    
      # rename the relevant variables to shorter names
      library(reshape)
      out.state <- rename(out.state, 
       c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"=
           "Heart.Attack"))
      out.state <- rename(out.state, 
       c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"=
                              "Heart.Failure"))
      out.state <- rename(out.state, 
       c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"=
                              "Pneumonia"))
      
      # turn relevant variables into numeric
      out.state$Heart.Attack <- as.numeric(out.state$Heart.Attack)
      out.state$Heart.Failure <- as.numeric(out.state$Heart.Failure)
      out.state$Pneumonia <- as.numeric(out.state$Pneumonia)
      
        
      #  sort by outcome and then hospital; the na.last option removes na
      ha.dat <- out.state[order(out.state$Heart.Attack, 
                                 out.state$Hospital.Name, na.last=NA),]
      hf.dat <- out.state[order(out.state$Heart.Failure, 
                                out.state$Hospital.Name, na.last=NA),]
      pn.dat <- out.state[order(out.state$Pneumonia, 
                                out.state$Hospital.Name, na.last=NA),]
      
      
      # create new variable with the rank of each outcomes 
      ha.dat$ha.rank <- rank(ha.dat$Heart.Attack, ties.method = "first")
      hf.dat$hf.rank <- rank(hf.dat$Heart.Failure, ties.method = "first")
      pn.dat$pn.rank <- rank(pn.dat$Pneumonia, ties.method = "first")
   
      # get the best and worst
      # best is rank 1, 1st hospital
      # worst is last rank without an NA, 1st hospital
     ifelse(num=="best", num <- 1, num <- num)
     ifelse(num=="worst", num <- max(hf.dat$hf.rank), num <- num)
      
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
      #testing
      result <- hf.dat[hf.dat$hf.rank==num,]
      print(result$Hospital.Name)
      
      
  } else {
    stop("invalid state")
    } # end if(outcome)...else stop
       } else {
       stop("invalid outcome")
    }  # end if(state)...else stop
}  # end function

#other code for testing
state <- "TX"
outcome <- "heart failure"
num <- "worst"
  3
  "best"
head(outcome.dat)
names(out.state)
summary(out.state)
str(out.state)
summary(Heart.Failure)
summary(Pneumonia)
head(ha.rank)

#test ordering
test.order <- out.state[order(out.state$Hospital.Name),]

#this works but eliminates the NA values so will not merge back into the data
ha.rank <- rank(out.state$Heart.Attack, na.last=NA, 
                          ties.method = "first")

#!This didn't work because it doesn't order the hospitals
# create new variables to rank each of the outcomes, keeping NA
#  for now; NA values will go at the end of the rankings
out.state$ha.rank <- rank(out.state$Heart.Attack, na.last=TRUE, 
                          ties.method = "first")
out.state$hf.rank <- rank(out.state$Heart.Failure, na.last=TRUE, 
                          ties.method = "first")
out.state$pn.rank <- rank(out.state$Pneumonia, na.last=TRUE, 
                          ties.method = "first")