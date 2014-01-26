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
###############################################################################
#####################################################################  
# Tests
rankall("heart attack", 20)
rankall("pneumonia", worst)
rankall("heart failure", 10)

outcome <- "heart attack"
num <- 20
#####################################################################  


# Function:
rankall <- function(outcome, num = "best") {  
  
  ## Read outcome data
  outcome.dat <- read.csv("outcome-of-care-measures.csv",
                          colClasses = "character")
  
  ## define valid outcomes
  out.set <- c("heart failure", "heart attack", "pneumonia")
  
  # get the state names frome the data set
  outcome.dat$State <- as.factor(outcome.dat$State)
  statenames <- levels(outcome.dat$State)
  
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
  
   # keep just the needed variables
   outcome.dat <- outcome.dat[, c("State", "Hospital.Name", "Heart.Attack", 
                                  "Heart.Failure", "Pneumonia" )]
   
   
   # set things up to work with any of the 3 outcomes 
    if(outcome=="heart attack") {
       outcome.dat <- rename(outcome.dat, c("Heart.Attack" = "outcome"))
       
     } else { 
       if(outcome=="heart failure") {
         outcome.dat <- rename(outcome.dat, c("Heart.Failure" = "outcome"))
         
       } else { # the last option, which is pneumonia
         
         outcome.dat <- rename(outcome.dat, c("Pneumonia" = "outcome"))
       }
     }
     
  # Order by state, outcome, hospital, removing NA for the outcome var.
  outcome2.dat <- outcome.dat[order(outcome.dat$State, outcome.dat$outcome, 
                                outcome.dat$Hospital.Name , na.last=NA), ]
     
  # then rank for each state  
   # first add a numerical ordering to the whole data set
   outcome2.dat$order<-seq(1:nrow(outcome2.dat))
   # then get the minimum order value for each state (in another data frame)
   st.order.dat <- aggregate(x=outcome2.dat$order, 
                             by=list(outcome2.dat$State), FUN='min')
   names(st.order.dat)<-c('State','minOrder')
   #  subtract 1 from the min for each state   
   st.order.dat$minOrder<-st.order.dat$minOrder-1
   
   #  merge back into the main data set
   outcome2.dat <- merge(st.order.dat, outcome2.dat, by="State", all=T)
   
   #  The rank for each state and hospital is the order - the state's min. order
   outcome2.dat$rank <- outcome2.dat$order - outcome2.dat$minOrder
   
 # create a logical variable for "worst"
  # first get the "worst" value for each state
   st.max.dat <- aggregate(x=outcome2.dat$rank, 
                             by=list(outcome2.dat$State), FUN='max')
   names(st.max.dat)<-c('State','worst.rank')
   # then merge this back into the data set
   outcome2.dat <- merge(st.max.dat, outcome2.dat, by="State", all=T)
 
   # create a vector the same no. of rows as outcome2.dat
   worst.yes <- rep(NA, nrow(outcome2.dat))
   #join it to outdata2
   outcome2.dat <- cbind(outcome2.dat, worst.yes)
 # make a new logical variable for the worst rank for each state 
  for (i in 1:nrow(outcome2.dat)) {
    ifelse(outcome2.dat[i,]$worst.rank==outcome2.dat[i,]$rank, 
         outcome2.dat[i,]$worst.yes <- TRUE, 
           outcome2.dat[i,]$worst.yes <-FALSE)
      }
  
  # Create a variable to flag wanted results 
  # create a vector the same no. of rows as outcome2.dat
   result <- rep(NA, nrow(outcome2.dat))
   #join it to outcome2.dat
   outcome2.dat <- cbind(outcome2.dat, result)
  # set "result" to FALSE to start
  outcome2.dat$result <- FALSE   

      
  # set "result" = true in outcome2.dat to keep the correct obs.
  # This picks obs where num=rank, including num="best"
   ifelse(num=="best", num <- 1, num <- num)
    
   for (i in 1:nrow(outcome2.dat)) {
      ifelse(num==outcome2.dat[i,]$rank | 
              (num>outcome2.dat[i,]$worst.rank 
               & outcome2.dat[i,]$worst.yes==TRUE) , 
             outcome2.dat[i,]$result <- TRUE , 
             outcome2.dat[i,]$result <- FALSE)
   }
   
   for (i in 1:nrow(outcome2.dat)) {
      # rename hospital name to NA for cases where rank>max(rank)
      if(num > outcome2.dat[i,]$worst.rank & outcome2.dat[i,]$result==TRUE) {
        outcome2.dat[i,]$Hospital.Name <- "NA"
      } #end if
   } # end for
   
  # where num = "worst"
     for (i in 1:nrow(outcome2.dat)) {
     if((num=="worst" &  outcome2.dat[i,]$worst.rank==outcome2.dat[i,]$rank)) 
              outcome2.dat[i,]$result <- TRUE
             }
  
  # select the rows with result = TRUE
   result.dat <- subset(outcome2.dat,
                        subset = result==TRUE,
                        select = c(Hospital.Name, State)
     )
  
   # rename the columns
   names(result.dat)<-c('hospital','state')
   
   
   ## Return the result    
   return(result.dat)
   
  }  # end if(outcome)
  
  else { stop("invalid outcome") }    
  
}  # end rankall

   
   
#####################################################################  
# Tests
rankall("heart attack", 20)
rankall("pneumonia", worst)
rankall("heart failure", 10)

outcome <- "heart attack"
num <- 20

     
   #####################################################################  
     
     
     select = c(Hospital.Name, State, rank, )
     subset(outcome2.dat,
            subset = rank==worst.rank,
            select = c(Hospital.Name, State))
  # This picks obs where num=rank, including num="best"
     
     
     test.dat <- outcome2.dat$worst.yes
     
  num <- "worst"
     num <- "best"
   num <- 100  
     
     ###############################################################
   #skip best and worst for now
   # assign the best and worst values to num
   
   ifelse(num=="worst", num <- outcome2.dat$worst.rank, num <- num)
   ###############################################################
   
   
   # pick the rows with the rank
   for (i in 1:nrow(outcome2.dat)) {
     ifelse(outcome2.dat[i,]$rank==outcome2.dat[i,]$rank, 
            outcome2.dat[i,]$worst.yes <- TRUE, 
            outcome2.dat[i,]$worst.yes <-FALSE)
   }
   
   
   
   
   num <- 100
   
   # create a 2x54 dataframe for the output
   result.dat <- data.frame(hospital=rep(NA, 54), state=rep(NA, 54))  
   
   # output just the obs. we want 
   for(j in statenames) {
   result.dat <- subset(outcome2.dat,
                        subset = rank==num,
                        select = c(Hospital.Name, State))
        }

   
   names(result.dat)<-c('hospital','state')
   #this doesn't keep all the states
   
   result2.dat  <- data.frame(hospital=rep(NA, 54), state=rep(NA, 54))
   
   result2.dat <- merge(result.dat, result2.dat, by="State", all=T)
   
   
   
   
  
   
  
   
   
   
   
   
 
      
   keep.dat <- outcome2.dat[outcome2.dat$rank] 
   
   
   # loop over states to get the outcome for each
   For(i in 1:50) {
     
     
     
     # put data into the data frame "outfile"
     result[i,] <- c(outcome2.dat$Hospital[i,].Name, outcome2.dat[i,]$State)
     
   }
   
   #############################################################################   
   
  
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




   
   
   
   
   
   
 
 ##############################################################################  
  
   #NOT SURE WHY WE DID THIS 
   ha.dat.ord <- ha.dat.ord[,-c(2,5)] 
   if(outcome=="heart attack") keep="Heart.Attack"
   a<-outcome.dat[,c('State','Hospital.Name',keep)]
   
   
   
   summary(outcome2.dat)
   summary(outcome.dat$Heart.Attack)
   
  
   
   # run through the list of states and find rank "num"
   # 1st define a function to select the relevant row for each state
   num <- 1
   row.select <- function(state.abb){
     state.res <- sec.try.dat[sec.try.dat$State=="state.abb",][num,]
   }
   result <- sapply(state.abb, row.select)
   
  ##############################################################################  
   
  
  #####################
  # For testing
  outcome<-"heart attack"
  outcome<-"heart failure"
  outcome<-"pneumonia"
  outcome<-"HEART ATTACK"



############################################################################## 
num <- "best"

# deal with num="best"
ifelse(num=="best", num <- 1, num <- num)


#Try for single states
State



# create a 2x54 dataframe for the output
result.dat <- data.frame(hospital=rep(NA, 54), state=rep(NA, 54)) 

for(j in statenames) {  # loop over states
  for(k in 1:worst.rank) { 
    if(num=="worst") {
      worst.hosp <- subset(outcome2.dat,
                           subset = rank==worst.rank,
                           select = c(Hospital.Name, State))
      result.dat[j,] <- worst.hosp
      
    } else { 
      if(num>worst.rank) {
        result.dat[j,] <- "NA"
        
      } else {
        
        result.dat[j,] <- subset(outcome2.dat,
                                 subset = rank==num,
                                 select = c(Hospital.Name, State))
        
      } # end loop of one state
      
    } # end loop of all states
    
    
    #NOT WORKING
    