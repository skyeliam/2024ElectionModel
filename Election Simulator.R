#Election Simulator
library(shiny)
library(tidyverse)

#create districts
districts <- c(state.abb,"ME-2", "NE-2")

#pull 538's data if it's been more than 2 hours, otherwise try to load local data, if error then pull 538 data
if(as.numeric(now()-as.numeric(file.info("timestamp.txt")[4])) > 7200){
  FTEPolls <- data.frame(read.csv(url("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv")))
  write.csv(FTEPolls, "FTEPolls.csv")
  write.table(now(),"timestamp.txt")
} else {
  FTEPolls <- tryCatch({data.frame(read.csv("FTEPolls.csv"))}, error = function(e) {
    FTEPolls <- data.frame(read.csv(url("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv")))
  })
  write.csv(FTEPolls, "FTEPolls.csv")
}
#create a data frame that will pull all the 538 data where Harris is a candidate
pollingDF <- data.frame(unique(filter(FTEPolls,answer=="Harris")$poll_id))
colnames(pollingDF) <- "PollID"
pollingDF$PollName <- FTEPolls$pollster[match(pollingDF$PollID,FTEPolls$poll_id)]
pollingDF$StartDate <- as.Date(FTEPolls$start_date[match(pollingDF$PollID,FTEPolls$poll_id)],"%m/%d/%y")
pollingDF$EndDate <- as.Date(FTEPolls$end_date[match(pollingDF$PollID,FTEPolls$poll_id)],"%m/%d/%y")
pollingDF$District <- FTEPolls$state[match(pollingDF$PollID,FTEPolls$poll_id)]
pollingDF$PollType <- vector(mode = "character",length = length(pollingDF$PollID))
pollingDF$SampleSize <- vector(mode = "integer",length = length(pollingDF$PollID))
pollingDF$Harris <- vector(mode = "numeric",length = length(pollingDF$PollID))
pollingDF$Trump <- vector(mode = "numeric",length = length(pollingDF$PollID))

#sample only polls since harris dropped
pollingDF <- filter(pollingDF,pollingDF$StartDate > as.Date("7/25/24","%m/%d/%y"))

#pull in data, preferring likely voter responses to registered and registered to adults
for (id in pollingDF$PollID){
  #try to pull likely voter results
  pollResults <- filter(FTEPolls, poll_id == id & population == "lv")
  if(nrow(pollResults) >0){
    pollingDF$PollType[match(id,pollingDF$PollID)] <- "LV"
  }
  #otherwise try to pull registered voter results
  else{
    pollResults <- filter(FTEPolls, poll_id == id & population == "rv")
    if(nrow(pollResults) >0){
      pollingDF$PollType[match(id,pollingDF$PollID)] <- "RV"
    }
    #just pull adults otherwise
    else{
      pollResults <- filter(FTEPolls, poll_id == id)
      pollingDF$PollType[match(id,pollingDF$PollID)] <- "A"
    }
  }
  #populate the data frame
  pollingDF$Harris[match(id,pollingDF$PollID)] <- min(filter(pollResults,answer=="Harris")$pct)
  pollingDF$Trump[match(id,pollingDF$PollID)] <- min(filter(pollResults,answer=="Trump")$pct)
  pollingDF$SampleSize[match(id,pollingDF$PollID)] <- min(pollResults$sample_size)
}

#throw out polls without a reported sample size
pollingDF <- filter(pollingDF,!is.na(SampleSize))

#replace blanks with U.S.
pollingDF$District[pollingDF$District==""]<-"US"
#calculate standard error
pollingDF$HarrisVar <- sqrt(pollingDF$Harris*(100-pollingDF$Harris))
pollingDF$TrumpVar <- sqrt(pollingDF$Trump*(100-pollingDF$Trump))
#calculate polling margin
pollingDF$margin <- pollingDF$Harris - pollingDF$Trump
#calculate a time discount
pollingDF$TimeDiscount <- 1/(1+as.numeric(floor((today()-pollingDF$EndDate)/7)))

#creating a dataframe that aggregates the polling data for each state / relevant CDs / US
stateData <- data.frame(unique(pollingDF$District))
names(stateData) <- c("State")
stateData$HarrisAvg <- vector(mode = "numeric",length = length(stateData$State))
stateData$TrumpAvg <- vector(mode = "numeric",length = length(stateData$State))
stateData$WeightedSampleSize <- vector(mode = "numeric",length = length(stateData$State))

#creating a dataframe where each state is a column with total poll results beneath it
getStateData <- function(state){
  filter(pollingDF,District==state)
}
getStateData = Vectorize(getStateData)
stateByPoll <- data.frame(getStateData(stateData$State))

#function for accessing specific variables within said dataframe
getDataItem <- function(state,variable){
  stateByPoll[[str_replace(str_replace(state," ","."),"-",".")]][[variable]]
}

getDataItem <- Vectorize(getDataItem)

#itereate through each state and calculate a weighted average based on the sqrt of the sample size and inverse of time since poll was released
for (state in stateData$State){
  ##TODO CLEAN UP AVERAGE CALCULATIONS
  stateData$HarrisAvg[match(state,stateData$State)] <- 
    weighted.mean(filter(pollingDF,District == state)$Harris,sqrt(filter(pollingDF,District == state)$SampleSize*filter(pollingDF,District == state)$TimeDiscount))
  stateData$TrumpAvg[match(state,stateData$State)] <- 
    weighted.mean(filter(pollingDF,District == state)$Trump,sqrt(filter(pollingDF,District == state)$SampleSize*filter(pollingDF,District == state)$TimeDiscount))
  stateData$WeightedSampleSize[match(state,stateData$State)] <- sum(getDataItem(state,"SampleSize")*getDataItem(state,"TimeDiscount"))
}
stateData$WeightedSampleSize[stateData$WeightedSampleSize<400] <- 400

#calculate margin, undecided, standard error for each candidate
stateData$Margin <- stateData$HarrisAvg - stateData$TrumpAvg
stateData$Undecided <- 100 - stateData$HarrisAvg - stateData$TrumpAvg
stateData$HarrisError <- sqrt(unlist(lapply(getDataItem(stateData$State,"HarrisVar"),"mean"))/100/stateData$WeightedSampleSize)*100
stateData$TrumpError <- sqrt(unlist(lapply(getDataItem(stateData$State,"TrumpVar"),"mean"))/100/stateData$WeightedSampleSize)*100

#generating a raw win probability to eyeball
stateData$HarrisRawWin <- sprintf("%.2f%%",pnorm(stateData$Margin/(stateData$TrumpError + stateData$HarrisError))*100)

#ask user if they want to write a state data file
writeStateData <- function(write){
  if(tolower(substr(write,1,1)) == "y"){
  write.csv(stateData,file = paste("State_Polling_Data",format(now(),"%Y-%b-%d_%H-%M"),".csv",sep=""))
  }
}
writeStateData(readline("Write state polling data file? (Y/N)"))