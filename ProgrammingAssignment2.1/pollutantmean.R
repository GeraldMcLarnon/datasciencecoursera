pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV file

        ## 'pollutant' us a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate"

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollountant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result

        ## filenames will be a list of files to load
        filenames <- sapply( formatC(id,width=3,format="d",flag="0"), function(x){ paste(directory,"/",x,".csv",sep="") } )

        ## data will be a list of the relevant pollution column loaded from each files
        data <- lapply( filenames,function(x){ d <- read.csv(file=x,header=T); d[[pollutant]] } )

        ## data now one big vector
        appenddata <- Reduce( function(x,y){ c(x,y) }, data )

        ## calculate the mean, ignoring NA values
        mean( appenddata, na.rm=TRUE )
}


