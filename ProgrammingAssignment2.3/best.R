## Returns hospital name with the best given outcome

best <- function(state, outcome) {

        ## Reading in outcome data
        out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid, otherwise stop()
        ##   any()    returns TRUE if there is at least one match

        if (! any( out$State == state ) ) {
           stop("invalid state")
        }

        ## This function is called with "outcome" which is a shorthand for a longer
        ## variable name in the data file. These "if" statements work through each
        ## case and remember the longer variable names. If there isn't a match, then
        ## stop().

        if ( outcome == "heart attack" ) {
             outcomevar <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
             outcomevar <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
             outcomevar <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
             stop("invalid outcome")
        }

        ## Return hospital name in that state with lowest 30-day death rate

        ## filter by State, retaining the Hospital.Name and "outcomevar" columns
        out2 <- out[out$State == state,c("Hospital.Name",outcomevar)]

        ## this converts the outcomevar columm to a number, suppressing the
        ## inevitable warnings about coercing NAs

        out2[,outcomevar] <- suppressWarnings(as.numeric(out2[,outcomevar]))

        ## Remove NA in outcomevar
        out3<-out2[!is.na(out2[,outcomevar]),]

        ## get minimum value of correct outcome column
        out3min <- min(out3[,outcomevar])

        ## filter by minimum value in correct outcome column
        out4 <- out3[out3[,outcomevar] == out3min,]

        ## return the first result by alphabetic
        min(out4$Hospital.Name)  # first by alaphabetic

}
