# 1 Plot the 30-day mortality rates for heart attack
# Read the outcome data into R via the read.csv function and look at the firrst few rows.
#NOTE: the data are read in as character, so need to change to numeric if necessary

 outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 head(outcome)

#  There are many columns in this dataset. You can see how many by typing 
 ncol(outcome) 
# there are 46  
#  (you can see the number of rows with the nrow function). 
 nrow(outcome)
#there are 4706
 
#  In addition, you can see the names of each column by typing
names(outcome) 
#  (the names are also in the PDF document.

#  To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset),run
 outcome[, 11] <- as.numeric(outcome[, 11])
 ## You may get a warning about NAs being introduced; that is okay
 hist(outcome[, 11])

#  Because we originally read the data in as character (by specifying colClasses = "character" we need to coerce the column to be numeric. You may get a warning about NAs being introduced but that is okay. This code creates a histogram of the death rates but could benefit from some better labelling.

# 1. Add a label to the x-axis that says "30-day Death Rate"
# 2. Add a title for the histogram that says "Heart Attack 30-day Death Rate"

?plot
 
 title(xlab="30-day Death Rate", main="Heart Attack 30-day Death Rate" )
 #this overwrote what was there; need to do it over
 
 hist(outcome[, 11], main="Heart Attack 30-day Death Rate", xlab="30-day Death Rate" )
 
###############################################################################
#2 Plot the 30-day mortality rates for heart attack, heart failure, and pneumonia
#  If you haven't already, read in the outcome-of-care-measures.csv dataset using the code specified above.

 # 1. Identify which columns of the data frame contain the 30-day death rate from heart attack, heart failure, and pneumonia. [11, 17, 23]
# 2. Coerce these columns to be numeric using the as.numeric function as above. You may receive warnings about NAs but that is okay.

 heart.att.dth <- as.numeric(outcome[, 11])
 heart.fail.dth <- as.numeric(outcome[, 17])
 pneum.dth <- as.numeric(outcome[, 23])
 
 #Also can do:
 for(col in c(11, 17, 23)) outcome[,col] <- as.numeric(outcome[,col])
 
#  3. Make histograms of the death rates for each outcome and put the histograms on the same plot window.
# This can be done by running 
 par(mfrow = c(3, 1)) 
#  before calling hist. This sets the plot window to have 3 rows and 1 column.
 hist(heart.att.dth, main="Heart Attack 30-day Death Rate", xlab="30-day Death Rate" )
 hist(heart.fail.dth, main="Heart Failure 30-day Death Rate", xlab="30-day Death Rate" )
 hist(pneum.dth, main="Pneumonia 30-day Death Rate", xlab="30-day Death Rate" )
 
 #  4. For each plot (there should be three plots, one for each outcome) make sure the x-axis label is "30-day Death Rate".
#  5. For each plot, set the title of the plot to be the outcome (i.e. heart attack, heart failure, or pneumonia).
 
 
# 6. Each time you call hist, a new plot is constructed using the data to be plotted. However, this makes it difficult to compare histograms across outcomes. Set all of the histograms to have the same numerical range on the x-axis by using the xlim argument. You can calculate the range of a vector of numbers by using the range function.
 #!##################################
 #Couldn't get range function to work
 #!##################################
 
 summary(heart.att.dth) #range is 10.1-21.9
 summary(heart.fail.dth) #range is 6.7-18.1
 summary(pneum.dth) #range is 6.8-21.2

 par(mfrow = c(3, 1)) 
 hist(heart.att.dth, main="Heart Attack 30-day Death Rate", xlab="30-day Death Rate", xlim=c(6,22))
 hist(heart.fail.dth, main="Heart Failure 30-day Death Rate", xlab="30-day Death Rate", xlim=c(6,22))
 hist(pneum.dth, main="Pneumonia 30-day Death Rate", xlab="30-day Death Rate", xlim=c(6,22) )
 
###########################
 #Another way to do it, from a TA
 
 common.range <- range(outcome[,c(11,17,23)],na.rm=TRUE)
 ###########################
 
 
 
#  Try the following variations on this plot:
#    1. Instead of plotting the histograms on top of each other, plot them all in a row, side by side.
 par(mfrow = c(1, 3)) 
 hist(heart.att.dth, main="Heart Attack 30-day Death Rate", xlab="30-day Death Rate", xlim=c(6,22))
 hist(heart.fail.dth, main="Heart Failure 30-day Death Rate", xlab="30-day Death Rate", xlim=c(6,22))
 hist(pneum.dth, main="Pneumonia 30-day Death Rate", xlab="30-day Death Rate", xlim=c(6,22) )
 
 
#  2. Using the median and the abline function, draw a vertical line on each histogram at the location of the median for that outcome.
 par(mfrow = c(3, 1)) 
  hist(heart.att.dth, main="Heart Attack 30-day Death Rate", 
       xlab="30-day Death Rate", xlim=c(6,22))
 abline(v=15.4)
 hist(heart.fail.dth, main="Heart Failure 30-day Death Rate", 
      xlab="30-day Death Rate", xlim=c(6,22))
 abline(v=11.6)
  hist(pneum.dth, main="Pneumonia 30-day Death Rate", xlab="30-day Death Rate", xlim=c(6,22) )
 abline(v=11.9)
 
###############################################################################
 #!This part isn't done
 #  3. In the title of each histogram, put in parentheses the mean death rate by adding (¯X =??) where ?? is the actual mean for that outcome. Consult the help page for plotmath to see how to get the ¯X to appear on the plot.
 par(mfrow = c(3, 1)) 
 
 hist(heart.att.dth, 
      main = expression ("bar(x)=10") 
 )
      
      xlab="30-day Death Rate", xlim=c(6,22))
      
      main=substitute("Heart Attack 30-day Death Rate""*bar(x)=*"==k, 
                      list(k=mean(heart.att.dth, na.rm))  
      , xlab="30-day Death Rate", xlim=c(6,22))
 
      
      abline(v=15.4)
 hist(heart.fail.dth, main="Heart Failure 30-day Death Rate", 
      xlab="30-day Death Rate", xlim=c(6,22))
 abline(v=11.6)
 hist(pneum.dth, main="Pneumonia 30-day Death Rate", xlab="30-day Death Rate", xlim=c(6,22) )
 abline(v=11.9)
 
 
 #Try the examples from the lecture
 plot(0,0, main = expression(theta == 0),
      ylab = expression(hat(gamma) == 0),
      xlab = expression(sum(x[i] * y[i], i==1, n)))
 
x <- rnorm(100) 
hist(x,
     xlab=expression("The mean (" * bar(x) * ") is " *
                       sum(x[i]/n, i==1,n))
     )
 
 
 
 
#  4. Add a smooth density estimate on top of the histogram. To do this you need to use the density function and you need to set prob=TRUE when calling hist.
 ###############################################################################


 
 
 
 
#  5 Finding the best hospital in a state
#  Write a function called best that take two arguments: the 2-character abbreviated name of a state and anoutcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
 
 
#  Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c",                                                                                      and "f" are tied for best, then hospital "b" should be returned).
 
#  The function should use the following template.
 best <- function(state, outcome) {
   
   ## define the outcome set
   out.set <- c("heart attack", "heart failure", "pneumonia")
   
   ## Check that state is valid
   if(state %in% state.abb) {
     
     ## Check that outcome is valid
     if(outcome %in% out.set) {
       
       ## Read outcome data
       outcome.dat <- read.csv("outcome-of-care-measures.csv", 
                               colClasses = "character") 
       
       ## pull out data for the relevant state and variables
       out.state <- outcome.dat[which(outcome.dat$State==state),
                                c(2,7,11,17,23)]
       
       # so now we have 5 variables: # 1-Hospital.Name; 2-State; 
       # 3-"Heart Attack"; 4-"Heart Failure"; 5-"Pneumonia
       ## set column numbers
       if(outcome=="heart attack") out.col <- 3
       if(outcome=="heart failure") out.col <- 4
       if(outcome=="pneumonia") out.col <- 5
       
       ## pick out the column with the needed data and get the minimum
       out.min <- (min(out.state[,out.col]))
       
       ## pick the hospital(s) with that minimum
       # first subset the data to keep the obs. for state and min
       out.hosp <- out.state[which(out.state[,out.col]==out.min),]
       if(nrow(out.hosp)>1) order(out.state[,1])
       print(out.hosp$Hospital.Name)
       
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
 
 
 
 
 
 
 
 
 
 
 
 
 

 
 