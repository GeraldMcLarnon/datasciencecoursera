complete <- function(directory, id=1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the locaiton of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## returns a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

        ## filenames will be a vector of character to check, the attribute "names"
        ## filenames <- sapply(id, function(x){ paste(directory,"/",formatC(x,width=3,format="d",flag="0"),".csv",sep="") } )

        ## data will be a vector with the count of complete cases in each file
        ## data <- sapply(filenames,function(x){ d <- read.csv(file=x,header=T); sum(complete.cases(d)) } )

        data <- data.frame(id = id, nobs = sapply(id, function(x){
                               filename <- paste(directory,"/",formatC(x,width=3,format="d",flag="0"),".csv",sep="")
                               d <- read.csv(file=filename,header=T)
                               sum(complete.cases(d))
                           } ) )
        data
   
}      
