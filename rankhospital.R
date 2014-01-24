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
rankhospital <- function(state, outcome, num = "best") {  
  
  ## Read outcome data
  outcome.dat <- read.csv("outcome-of-care-measures.csv",
                          colClasses = "character")
  
  ## define valid outcomes
  out.set <- c("heart failure", "heart attack", "pneumonia")
  
  
  ## Check that state and outcome are valid
 #! if(state %in% state.abb) {
  #!   if(outcome %in% out.set) {
    
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
      
      
 #!  }  # end if(outcome)
    
  #!    } else { stop("invalid outcome") }

  #!  }  # end if(state)
  
  #!  else { stop("invalid state") }        

}  # end rankhospital

#  Tests of function
rankhospital("TX", "heart failure", 4)
#works

rankhospital("MD", "heart attack", "worst")
#works

rankhospital("MN", "heart attack", 5000)
# works

