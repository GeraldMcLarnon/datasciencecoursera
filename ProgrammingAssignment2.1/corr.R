corr <- function(directory = "specdata", threshold = 0, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all variables)
        ## required to compute the correlation between
        ## nitrate and sulface; the default is 0

        ## returns a numeric vector of correlations
        ## NOTE: Do not round the result!

        result = double(length=0)

        for ( i in id ) {
              Filename <- paste(directory,"/",formatC(i,width=3,format="d",flag="0"),".csv",sep="")
              d <- read.csv(file=Filename,header=T)
              Nobs <- sum(complete.cases(d))
              if ( Nobs > threshold ) {
                   x <- d["sulfate"]
                   y <- d["nitrate"]
                   Corr <- cor( x, y , use="na.or.complete")
                   # Corr <- 1
                   result = c(result, Corr)
              } else {
                   # print(c("skipped case ",i))
              }
        }

        result
}

