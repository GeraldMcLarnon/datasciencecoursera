## Returns hospital name for a given outcome and ranking

rankhospital <- function(state, outcome, num="best") {

        ## Reading in outcome data form the csv file, forcing columns to be characters

        out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state is valid, otherwise stop(). Uses any() which returns
        ## TRUE if at least one of the logical tests is true.

        if (! any( out$State == state ) ) {
           stop("invalid state")
        }

        ## Checks that outcome is valid, otherwise stop(). Along the way, sets
        ## outcomevar to be name of the column we'll need to lookup.

        if ( outcome == "heart attack" ) {
             outcomevar <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
             outcomevar <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
             outcomevar <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
             stop("invalid outcome")
        }


        ## Filter by State, retaining the columns "Hospital.Name" and outcomevar
        ## Then rename columns to something easier for later!

        out2 <- out[out$State == state,c("Hospital.Name",outcomevar)]
        colnames(out2)<-c("Name","Outcome")

        ## this converts the Outcome columm to a number, suppressing the
        ## inevitable warnings about coercing NAs

        out2$Outcome <- suppressWarnings(as.numeric(out2$Outcome))

        ## Remove NA in outcomevar

        out3<-out2[!is.na(out2$Outcome),]

        ## order on Outcome then Name

        attach(out3)
        out4<-out3[order(Outcome,Name),]
        detach(out3)


        ## Assume that "num" was a number, suppressing warning messages

        mynum <- suppressWarnings(as.numeric(num))

        ## But reset if num was "best" or "worst"

        if ( num == "best" ) {
           mynum <- 1
        } else if ( num == "worst" ) {
           mynum <- nrow(out4)
        } 

        ## return the Name, which is in column 1
        out4[mynum,1]

}
