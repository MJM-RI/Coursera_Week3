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
         result <- min(ha.dat$Heart.Attack) 
         return(result$Hospital.Name)
       } else { 
         if(outcome=="heart failure") {
           result <- min(ha.dat$Heart.Attack) 
           return(result$Hospital.Name)
         } else { # the last option, which is pneumonia
           result <- min(ha.dat$Heart.Attack) 
           return(result$Hospital.Name)
         }
       }
              
     } #end if(outcome)  
     else { stop('invalid outcome') }
     
     
   } #end if(state)
   else { stop('invalid state') }
      
 } #end best
 

 #tests
 best("TX", "heart attack") #works
 
 best("TX", "heart failure") #!doesn't work - 4 outcomes but none are correct
 
 best("MD", "heart attack") #works
 
 best("MD", "pneumonia") #!doesn't work - 4 outcomes, none correct
 
 best("BB", "heart attack") #works
 
 best("NY", "hert attack") #works
 
 
 
 
 
 
 
     
     
     ## Calculate best
     
     outcome  <- "heart attack" #col 11
     outcome  <- "heart failure" #col 17
     outcome <- "pneumonia" #col 23
     
      ## Select the hospital(s) that minimize the outcome, for each outcome     
       if(outcome=="heart attack") 
        outcome.var  <- as.numeric(outcome.dat[,11])
        #na.omit(ha.outcome)   
       #order(ha.outcome) 
       #summary(ha.outcome)
     
     best.outcome <- min(outcome.var, na.rm=TRUE)
     
     #keep observations(s) with lowest outcomes
     
     best.outcome.dat <- subset(outcome.dat, best.outcome==TRUE)
     
     ,
                                select=c("outcome", Hospital.Name))
     
     best.hospital <- 
       
     
     else { stop('invalid outcome') }
         
     else { stop('invalid state') }
   
   
   ## Return hospital name in that state with lowest 30-day death
   ## rate
   
 
 
 }
 
   names(outcome.dat)
   
   
   #min rate for
   # heart attack: 10.1
   # heart failure: 6.7
   # pneumonia: 6.8
   
   
   
   hospital  <-  which.min(ha.outcome)
   hospital <- outcome[which.min(outcome.dat[ ,11])]
   
 
 ## Check that state and outcome are valid
 state  <- "RI"
 
 ifelse(state %in% state.abb, print("correct"), print("invalid state"))
 
 
 #tips from the discussion boards:
 options(warn=-1)
 "suppressWarnings"
 
 If(outcome %in% "heart attack") {  
   hospital <- outcome2[which.min(outcome2[ ,11]),2] }
 else { stop('invalid outcome') }
 
 using the order() funciton. This is my output.
  > State.Ord[,1]
 
 
 
 
#  The function should check the validity of its arguments. If an invalid state value is passed to best, the function should throw an error via the stop function with the exact message "invalid state". If an invalid outcome value is passed to best, the function should throw an error via the stop function with the exact message "invalid outcome".
 
 #Figure out the state piece:
 str("State")
 summary("State")
 
 names(outcome)
 head(outcome)
 
 state.abb
 
 ##########################################
 ## write pieces of code using 1 case:
 
 #################
 # Var List:
 # outcome.dat - data set for outcome of care measures
 # state - parameter - defines the state of interest
 # outcome - parameter - defines the outcome of interest
 # out.set - a vector of relevant outcomes (N=3)
 # state.abb - a built in data set of 2 letter state abbreviations
 # out.col - the column number for each of the outcomes (11, 17, 23)
 # out.state - only data for the state of interest
 # out.min - the minimum value of the outcome of interest
 # out.hosp - only the observations for state, hospital(s) and outcome of interest
 
 ## Read outcome data
 outcome.dat <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character") 
 
 ## test for valid state:
 state  <- "TX"
 state <- "BB"
 if(state %in% state.abb) {
   print("correct")   
 } else {
   print("invalid state")
 } 
 
 ## test for valid outcome:
#define outcomes
 out.set <- c("heart attack", "heart failure", "pneumonia")
 
 outcome <- "heart failure"
 outcome <- "cough"
 if(outcome %in% out.set) {
   print("correct")   
 } else {
   print("invalid outcome")
 }
 
 
 ## define the correct column for each outcome
 if(outcome=="heart attack") out.col <- 11
 if(outcome=="heart failure") out.col <- 17
 if(outcome=="pneumonia") out.col <- 23
 
 ## pull out data for the relevant state and variables
 out.state <- outcome.dat[which(outcome.dat$State==state), c(2,7,11,17,23)]
 head(out.state)
 
 # so now I have 5 variables: # 1-Hospital.Name; 2-State; 
  # 3-"Heart Attack"; 4-"Heart Failure"; 5-"Pneumonia"
 
 ## pick out the column with the needed data and get the minimum
 out.min <- (min(out.state[,4]))
 
  ## pick the hospital(s) with that minimum
 # first subset the data to keep the obs. for state and min
 out.hosp <- out.state[which(out.state[,4]==out.min),]
 if(nrow(out.hosp)>1) order(out.state[,1])
 print(out.hosp$Hospital.Name[1,])
 
 #!not working for more than one hospital
 #!not working for Texasl heart failure
 
 ##############################################################
 #test:
 #texas data only
 ## Read outcome data
 outcome.dat <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character") 
 
 ## test for valid state:
 state  <- "TX"
 out.state <- outcome.dat[which(outcome.dat$State==state), c(2,7,11,17,23)]
 #out.tex <- outcome.dat[which(outcome.dat$State=="TX"),]
#  head(out.tex)
#  heart.att.dth <- as.numeric(out.tex[, 11])
#  heart.fail.dth <- as.numeric(out.tex[, 17])
#  pneum.dth <- as.numeric(out.tex[, 23])
 
 summary(heart.att.dth)
 summary(heart.fail.dth)
 summary(pneum.dth)
 
 as.numeric(out.state[,3])
 
 out.ha.tex <- out.state[which(out.state[,3]==12),]
 out.hf.tex <- out.tex[which(out.tex[,4]==8.1),]
 out.pn.tex <- out.tex[which(out.tex[,5]==7.3),]
 
 ##############################################################
 outcome.keep <- outcome.dat[ which(outcome.dat$State=="RI" 
                                   & outcome.dat[,out.col]==out.min), ]
 
 
 outcome.keep <- outcome.dat[ which(outcome.dat$State=="RI"),] 
 outcome.keep[,out.col]
 
 for(outcome.dat[,out.col] %in% out.min) {
   print("Hospital.Name")
 }
 
 
 
 
 
 
 
 
 
 
 
 
 

 
 