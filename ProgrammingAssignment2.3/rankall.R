## For a given outcome and rank, returns data frame with result for each state

rankall <- function(outcome, num="best") {

        ## Reading in outcome data form the csv file, forcing columns to be characters

        out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
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


        states <- unique( out$State )

        ## Create a data.frame to hold the State and Hospital.Name
        result <- matrix(nrow = length(states), ncol = 2)
        colnames(result) <- c("hospital","state")
        count <- 0

        for (i in states) {
             count <- count + 1

             ## working variable to hold hospital.name and outcomevar

             out2 <- out[out$State == i,c("Hospital.Name",outcomevar)]
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

             ## set the appropriate part of the data.frame

             #cat("RUNNING",i,mynum,out4[mynum,1],"\n")

             result[count,1] <- out4[mynum,1]
             result[count,2] <- i
        }

        ## return the result as a data frame, after setting row names

        frame <- data.frame(result)
        row.names(frame) <- frame$state

        attach(frame)
        frame <- frame[order(state),]
        detach(frame)
        frame

}
