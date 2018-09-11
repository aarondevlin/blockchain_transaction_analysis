
## Transaction Summarization Metrics: Per Day, Month, & Year
##
## tx is transaction
## columns to keep: transaction count (txCount), and use adjustedTransaction Amount
## daily blockchain transaction volume (by count & by amount), monthly, & yearly (some coins dont have but one year.. take this into account)
##    this is daily tx count & tx amount, for all of the coins in one bucket
## ONLY BY COUNT IS DONE, SINCE SOME CSVs DO NOT CONTAIN adjustedTransaction Amount

## FUTURE WORK
## daily blockchain database growth in MB
## daily crypto traffic bandwidth



## ************************* INGEST DATA FROM MULTIPLE CSV FILES ************************* 
setwd("/Users/aaronfl1p/Desktop/boxy/coinMetrics")

## Obtain strings of the full file path for each .csv in the 'coinData08122018' folder
coinDataFiles <- list.files(path="/Users/aaronfl1p/Desktop/boxy/coinMetrics/coinData08122018", pattern="*.csv", full.names=TRUE, recursive=FALSE)

# Initialize the allcoindata df
#allCoinData <- data.frame(file = "", date = "", txVolume.USD. = "", txCount = "")
allCoinData <- data.frame(file = "", date = "", txCount = "")
allCoinData <- allCoinData[-1,]

## Loop through files in the 'coinData08122018' folder and put all data into one df called 'allCoinData'
## Open each file, make a df, then only choose the 'date', 'txCount'
for(i in 1:length(coinDataFiles)){                                              # For each csv filename
  currentFileName <- coinDataFiles[i]                                           #   Grab the ith filename
  currentCoinDF <- read.csv(currentFileName, header = F, stringsAsFactors = F)  #   Turn the ith file into a df
  colnames(currentCoinDF) <- currentCoinDF[1,]                                  #   Add column names as the first row of the .csv
  currentCoinDF <- currentCoinDF[-1,]                                           #   Remove the first row, since they are colnames now
  #currentCoinDF <- currentCoinDF[, c("date", "txVolume(USD)", "txCount")]       #   Choose only these three columns
  currentCoinDF <- currentCoinDF[, c("date", "txCount")]       #   Choose only these two columns
  currentCoinDF$fileIndex <- i                                                  #   Add the file number (1 through 75) to the df, for future reference
  allCoinData <- rbind(allCoinData, currentCoinDF)                              #   Concatenate the df with the df of the previous loop iteration
}

allCoinData$date <- as.factor(allCoinData$date)
allCoinData$txCount <- as.numeric(allCoinData$txCount)

## Create MonthYear Column, for Per Month Analysis
allCoinData$MonthYear <- substr(allCoinData$date, 1, 7)

## Create Year Column, for Per Year Analysis
allCoinData$Year <- substr(allCoinData$date, 1, 4)



## ************************* CALCULATE DAILY/MONTHLY/YEARLY TRANSACTION SUMS ************************* 
library(plyr)

allCoinData <- allCoinData[complete.cases(allCoinData),]  # Remove rows with NA's

## Transaction Count Per Day
dailySums <- ddply(allCoinData, .(date), summarize, dailySum = sum(txCount, na.rm=T))

## Transaction Count Per MonthYear
monthlySums <- ddply(allCoinData, .(MonthYear), summarize, monthlySum = sum(txCount, na.rm=T))

## Transaction Count Per Year
yearlySums <- ddply(allCoinData, .(Year), summarize, yearlySum = sum(txCount, na.rm=T))

write.csv(allCoinData, "allCoinData.csv", row.names = F)



## ************************* PLOT DAILY/MONTHLY/YEARLY TRANSACTION SUMS *************************
library(ggplot2)
library(scales)

plot(x=yearlySums$Year, y=yearlySums$yearlySum) ## works


## Plot of Yearly Transaction Count
ggplot(data=yearlySums, aes(x=Year, y=yearlySum)) +
  geom_point() + 
  ylab("Transaction Count") +
  scale_y_continuous(labels = comma, breaks = seq(0, 500000000, by=50000000))

## Plot of Monthly Transaction Count
ggplot(data=monthlySums, aes(x=MonthYear, y=monthlySum)) +
  geom_point() + 
  ylab("Transaction Count") + xlab("Month") +
  scale_y_continuous(labels = comma, breaks = seq(0, 150000000, by=10000000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6))

## Plot of Daily Transaction Count
ggplot(data=dailySums, aes(x=date, y=dailySum)) +
  geom_point() + 
  ylab("Transaction Count") + xlab("Day") +
  scale_y_continuous(labels = comma, breaks = seq(0, 10000000, by=500000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 3))


# daily again... this one looks better
dailySums$date <- as.Date(dailySums$date)

ggplot(dailySums, aes(date, dailySum)) + geom_line() +
  scale_y_continuous(labels = comma, breaks = seq(0, 10000000, by=500000)) +
  xlab("") + ylab("Daily Transaction Count")
