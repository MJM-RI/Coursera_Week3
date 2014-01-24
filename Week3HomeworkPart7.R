#Week 3 Homework Part 7
# A function to select the hospital in each state with a given rank 
# for an outcome 
# 2 arguments:
#   outcome: "heart attack", "heart failure", "pneumonia"
#   num: ranking of a hospital in that state for an outcome
#        takes "best", "worst", or an integer indicating rank (1 is best)
# input data: outcome-of-care-measures.csv
# output: a 2-col data frame with the name of the hospital and the state
#         1st col named "hospital", 2nd col named "state"
# exclude hospitals with no data on a particular outcome when 
#  deciding rankings
# resolve ties in ranking by alphabetical order of the hospital names
# invalid outcome throws error via stop(): "invalid outcome"

###############################################################################
# Variables, parameters, data sets:
# outcome.dat - data set for outcome of care measures
# state - fn. argument - state of interest
# outcome - fn. argument - outcome of interest
# num - fn. argument - ranking of interest (incl. "best", "worst")
# out.set - vector of relevant outcomes
# state.abb - built in data set of 2 letter state abbreviations
# Note: col. numbers for outcomes of interest are 11, 17, 23
# out.state - data for only the state of interest
# ha.dat, hf.dat, pn.dat - data for each outcome of interest
# ha.rank, etc. - rank variables
###############################################################################

# Function:
rankall <- function(outcome, num = "best") {  
  
  ## Read outcome data
  outcome.dat <- read.csv("outcome-of-care-measures.csv",
                          colClasses = "character")
  
  ## define valid outcomes
  out.set <- c("heart failure", "heart attack", "pneumonia")
  
  ## Check that outcome is valid
  if(outcome %in% out.set) {
      
  
   # rename the relevant variables to shorter names
   library(reshape)
   outcome.dat <- rename(outcome.dat, 
              c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"=
                              "Heart.Attack"))
   outcome.dat <- rename(outcome.dat, 
              c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"=
                              "Heart.Failure"))
   outcome.dat <- rename(outcome.dat, 
              c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"=
                              "Pneumonia"))
      
   # turn relevant variables into numeric
   outcome.dat$Heart.Attack <- as.numeric(outcome.dat$Heart.Attack)
   outcome.dat$Heart.Failure <- as.numeric(outcome.dat$Heart.Failure)
   outcome.dat$Pneumonia <- as.numeric(outcome.dat$Pneumonia)
  
   # for each outcome, create rankings by state
   #TESTING
   try.dat <- outcome.dat[, c("State", "Heart.Attack", "Hospital.Name")]
   # add a rank col by state
   try.dat <- cbind(try.dat, rank=ave(try.dat$Heart.Attack, 
                                      try.dat$State, FUN=rank)) 
   #  sort by outcome and then hospital; the na.last option removes na
   sort.dat <- try.dat[order(try.dat$State, try.dat$rank, 
                             try.dat$Hospital.Name, na.last=NA),]
   
   
   
    #  sort by outcome and then hospital; the na.last option removes na
    ha.dat <- out.state[order(out.state$Heart.Attack, 
                                out.state$Hospital.Name, na.last=NA),]
    hf.dat <- out.state[order(out.state$Heart.Failure, 
                                out.state$Hospital.Name, na.last=NA),]
    pn.dat <- out.state[order(out.state$Pneumonia, 
                                out.state$Hospital.Name, na.last=NA),]
      
      
      # create new variable with the rank of each outcome 
      ha.dat$ha.rank <- rank(ha.dat$Heart.Attack, ties.method = "first")
      hf.dat$hf.rank <- rank(hf.dat$Heart.Failure, ties.method = "first")
      pn.dat$pn.rank <- rank(pn.dat$Pneumonia, ties.method = "first")
      
      # assign "best" and "worst" to ranks
      # best is rank 1, worst is highest rank
      # need to do this for each outcome
      if(outcome=="heart attack") {
        ifelse(num=="best", num <- 1, num <- num)
        ifelse(num=="worst", num <- max(ha.dat$ha.rank), num <- num)
      } else { 
        if(outcome=="heart failure") {
          ifelse(num=="best", num <- 1, num <- num)
          ifelse(num=="worst", num <- max(hf.dat$hf.rank), num <- num)  
        } else { # the last option, which is pneumonia
          ifelse(num=="best", num <- 1, num <- num)
          ifelse(num=="worst", num <- max(pn.dat$pn.rank), num <- num)  
        }
      }
      
      ## Return hospital name in that state with the given rank
      ## using if...else for outcomes
      if(outcome=="heart attack") {
        result <- ha.dat[ha.dat$ha.rank==num,]
      } else { 
        if(outcome=="heart failure") {
          result <- hf.dat[hf.dat$hf.rank==num,]  
        } else { # the last option, which is pneumonia
          result <- pn.dat[pn.dat$pn.rank==num,]
        }
      }
      
      ## Return the result    
      ##  Return NA if num is too large  
      ## need to do this for each outcome
      if(outcome=="heart attack") {
        ifelse(num > max(ha.dat$ha.rank), return(NA), return(result$Hospital.Name))
      } else { 
        if(outcome=="heart failure") {
          ifelse(num > max(hf.dat$hf.rank), return(NA), return(result$Hospital.Name))
        } else { # the last option, which is pneumonia
          ifelse(num > max(pn.dat$pn.rank), return(NA), return(result$Hospital.Name))
        }
      }   
      
      
    }  # end if(outcome)
    
    else { stop("invalid outcome") }    
  
}  # end rankhospital
