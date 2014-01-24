#  5 Finding the best hospital in a state
#  Write a function called best that take two arguments: the 2-character abbreviated name of a state and anoutcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
 
 
#  Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c",                                                                                      and "f" are tied for best, then hospital "b" should be returned).
 
#  The function should use the following template.

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

best <- function(state, outcome) {

  ## Read outcome data
  outcome.dat <- read.csv("outcome-of-care-measures.csv",
                          colClasses = "character")
  
  ## define the outcome set
   out.set <- c("heart attack", "heart failure", "pneumonia")
   
   ## Check that state is valid
   if(state %in% state.abb) {
     
     ## Check that outcome is valid
     if(outcome %in% out.set) {
       
       ## pull out data for the relevant state and variables
       out.state <- outcome.dat[which(outcome.dat$State==state),
                                c(2,7,11,17,23)]
       
       # so now we have 5 variables: # 1-Hospital.Name; 2-State; 
       # 3-"Heart Attack"; 4-"Heart Failure"; 5-"Pneumonia
       
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
       
       ## Return hospital name for best in that state 
       ## using if...else for outcomes
       if(outcome=="heart attack") {
         result <- ha.dat[ha.dat$Heart.Attack==min(ha.dat$Heart.Attack),] 
         #return(result)
         return(result$Hospital.Name)
       } else { 
         if(outcome=="heart failure") {
           result <- hf.dat[hf.dat$Heart.Failure==min(hf.dat$Heart.Failure),] 
           #return(result)
           return(result$Hospital.Name)
         } else { # the last option, which is pneumonia
           result <- pn.dat[pn.dat$Pneumonia==min(pn.dat$Pneumonia),] 
           #return(result)
           return(result$Hospital.Name)
         }
       }
              
     } #end if(outcome)  
     else { stop('invalid outcome') }
     
     
   } #end if(state)
   else { stop('invalid state') }
      
 } #end best
 
