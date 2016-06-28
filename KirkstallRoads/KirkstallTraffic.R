# Process files from Council to undertand rat run
#
# 2016194.xlsl - Data for Church Street
#    Sheets are called "13 Jun" .. "19 Jun"
#    WestBound data in A52:O77 and EastBound data in A90:O115
#    Both data sets have first two columns as headers
#
# 2016254.xlsl - Data for Victoria Road
#    Sheets are called "13 Jun" .. "19 Jun"
#    NorthBound data in A52:O77 and SouthBound data in A90:O115
#    Both data sets have first two columns as headers

library(dtplyr)  # data.table plus plyr
library(tidyr)
library(xlsx)

filenames <- c("2016194.xlsx", "2016254.xlsx")
dates <- c("13 Jun", "14 Jun", "15 Jun", "16 Jun", "17 Jun", "18 Jun", "19 Jun")

FTmp <- data.frame(Road=character(),                               # Temporary variable to which all
                   Direction=character(),                          # spreadsheet data will be rbind()
                   Date=character(),
                   Time=character(),
                   Speed=numeric(),
                   Count=integer())


for ( file in filenames ) {
   for ( date in dates ) {
      RoadName  <- as.character(
                             read.xlsx(file,sheetName=date,
                             header=FALSE,colIndex=2:2,rowIndex=5:5)[1,1] )
      Data1     <- read.xlsx(file,sheetName=date,
                             header=TRUE,colIndex=1:15,rowIndex=52:77) 
      Dir1      <- as.character(
                             read.xlsx(file,sheetName=date,
                             header=FALSE,colIndex=1:1,rowIndex=51:51)[1,1] )
      Data2     <- read.xlsx(file,sheetName=date,
                             header=TRUE,colIndex=1:15,rowIndex=90:115) 
      Dir2      <- as.character(
                             read.xlsx(file,sheetName=date,
                             header=FALSE,colIndex=1:1,rowIndex=89:89)[1,1] )

      colnames(Data1)[1] <- "Time"                                     # Add Column Name to first column
      colnames(Data2)[1] <- "Time"
      
      Tmp1 <- Data1 %>%  select(-Average.Flow)                    %>%  # Remove "Average.Flow" column
                         filter(!is.na(Time))                     %>%  # Remove first row, which is blank
                         gather(Speed,Count,-Time)                %>%  # Convert column headings into a single "speed" column
                         mutate(Date=date,Road=RoadName,
                                Direction=Dir1)                   %>%  # Add Date, RoadName and Direction
                         mutate(Speed=gsub("\\.0\\.","-",Speed))  %>%  # Convert Speed to a numeric
                         mutate(Speed=gsub("X","",Speed))         %>%  # using a number of regrex
                         mutate(Speed=gsub("\\.0mph","",Speed))   %>%  
                         mutate(Speed=gsub("\\.5","0-5",Speed))   %>% 
                         mutate(Speed=gsub("\\.60","60-",Speed))  %>%
                         select(Road,Direction,Date,Time,      
                                Speed,Count)                           # Re-order columns

      Tmp2 <- Data2 %>%  select(-Average.Flow)                    %>%  # Remove "Average.Flow" column
                         filter(!is.na(Time))                     %>%  # Remove first row, which is blank
                         gather(Speed,Count,-Time)                %>%  # Convert column headings into a single "speed" column
                         mutate(Date=date,Road=RoadName,
                                Direction=Dir2)                   %>%  # Add Date, RoadName and Direction
                         mutate(Speed=gsub("\\.0\\.","-",Speed))  %>%  # Convert Speed to a numeric
                         mutate(Speed=gsub("X","",Speed))         %>%  # using a number of regrex
                         mutate(Speed=gsub("\\.0mph","",Speed))   %>%   
                         mutate(Speed=gsub("\\.5","0-5",Speed))   %>%
                         mutate(Speed=gsub("\\.60","60-",Speed))  %>%
                         select(Road,Direction,Date,Time,      
                                Speed,Count)                           # Re-order columns

      
       FTmp <- rbind(FTmp,Tmp1,Tmp2)                                   # Append Tmp1 and Tmp2 to FTmp
   }
}


Full <- tbl_df(FTmp)
rm(Data1,Data2,Dir1,Dir2,Tmp1,Tmp2,FTmp,filenames,dates)

# Example analysis
# Count of cars by Road/Duration/Date, ignoring Time and Speed

Tmp1 <- Full %>% group_by(Road,Direction,Date) %>% summarise(sum(Count))

 
