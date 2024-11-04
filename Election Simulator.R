#Election Simulator
library(shiny)
library(tidyverse)
library(DescTools)
setwd("~/Documents/GitHub/2024ElectionModel")
#create districts
districts <- c(state.name,"District of Columbia", "Maine CD-2", "Maine CD-1", "Nebraska CD-2")
midwest <- list("Illinois","Indiana","Iowa","Kansas","Michigan","Minnesota","Missouri","Nebraska","North Dakota",
                "Ohio","Pennsylvania","South Dakota","West Virginia","Wisconsin","Nebraska CD-2")
southeast <- list("Alabama","Arkansas","Florida","Georgia","Kentucky","Louisiana","Mississippi","North Carolina",
                  "Oklahoma","South Carolina","Tennessee","Texas","Virginia")
southwest <- list("Arizona","Colorado","Nevada","New Mexico","Utah")

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
pollingDF$Harris <- Winsorize(pollingDF$Harris)
pollingDF$Trump <- Winsorize(pollingDF$Trump)

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

#throw out polls without a reported sample size and dupe(ish) polls
pollingDF <- filter(pollingDF,!is.na(SampleSize))
pollingDF <- pollingDF[-which(mutate(pollingDF,
                                     cond = ifelse(PollName == lead(PollName) & District == lead(District),TRUE,FALSE) )$cond),]

#replace blanks with U.S.
pollingDF$District[pollingDF$District==""]<-"US"
#calculate standard error
pollingDF$HarrisVar <- sqrt(pollingDF$Harris*(100-pollingDF$Harris))
pollingDF$TrumpVar <- sqrt(pollingDF$Trump*(100-pollingDF$Trump))
#calculate polling margin
pollingDF$margin <- pollingDF$Harris - pollingDF$Trump
#calculate a time discount
pollingDF$TimeDiscount <- 1/(1+as.numeric(floor((today()-pollingDF$EndDate)/7)))

for(poll in pollingDF$PollID){
  subSample <- filter(pollingDF,PollName == pollingDF[pollingDF$PollID==poll,]$PollName&
                        District == pollingDF[pollingDF$PollID==poll,]$District&
                        abs(EndDate-pollingDF[pollingDF$PollID==poll,]$EndDate)<=7&
                        PollType==pollingDF[pollingDF$PollID==poll,]$PollType)
  subLen <- length(subSample[,1])
  pollingDF$SampleSize[pollingDF$PollID == poll] <- pollingDF$SampleSize[pollingDF$PollID == poll]/subLen
}

#creating a dataframe that aggregates the polling data for each state / relevant CDs / US
stateData <- data.frame(unique(pollingDF$District))
names(stateData) <- c("State")
stateData$HarrisAvg <- vector(mode = "numeric",length = length(stateData$State))
stateData$TrumpAvg <- vector(mode = "numeric",length = length(stateData$State))
stateData$WeightedSampleSize <- vector(mode = "numeric",length = length(stateData$State))
stateData$TimeDiscountedPollCount <- vector(mode = "numeric",length = length(stateData$State))

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
  stateData$HarrisAvg[stateData$State == state] <- 
    weighted.mean(filter(pollingDF,District == state)$Harris,sqrt(filter(pollingDF,District == state)$SampleSize*filter(pollingDF,District == state)$TimeDiscount))
  stateData$TrumpAvg[stateData$State == state] <- 
    weighted.mean(filter(pollingDF,District == state)$Trump,sqrt(filter(pollingDF,District == state)$SampleSize*filter(pollingDF,District == state)$TimeDiscount))
  stateData$WeightedSampleSize[stateData$State == state] <- sum(getDataItem(state,"SampleSize")*getDataItem(state,"TimeDiscount"))
  stateData$TimeDiscountedPollCount[stateData$State == state] <- sum(pollingDF$TimeDiscount[pollingDF$District == state])
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
  write.csv(ElectionSims,file = paste("Election Simulation Data",format(now(),"%Y-%b-%d_%H-%M"),".csv",sep=""))
  write.csv(stateData,file = paste("Latest_State_Polling_Data",".csv",sep=""))
  write.csv(ElectionSims,file = paste("Latest_Election Simulation Data",".csv",sep=""))
  }
}

DemoData <- read.csv("StateDems.csv",colClasses=c("NULL",NA,NA,NA))

#generate 4000x4 regional variables for seeding the model
ElectionSims <- data.frame(USVar = rnorm(4000))
ElectionSims$midwestVar <- (2*ElectionSims$USVar - rnorm(4000))/sqrt(8)
ElectionSims$southwestVar <- (2*ElectionSims$USVar - rnorm(4000))/sqrt(8)
ElectionSims$southeastVar <- (2*ElectionSims$USVar - rnorm(4000))/sqrt(8)
sigma <- 2.5

#generate 2-way popular vote
ElectionSims$HarrisPopVote <- ElectionSims$USVar * stateData$HarrisError[match("US",stateData$State)] + 
  stateData$HarrisAvg[match("US",stateData$State)]+pnorm(ElectionSims$USVar/sqrt(sigma)) *
  stateData$Undecided[match("US",stateData$State)]
ElectionSims$TrumpPopVote <- -1* ElectionSims$USVar * stateData$TrumpError[match("US",stateData$State)] + 
  stateData$TrumpAvg[match("US",stateData$State)]+(1-pnorm(ElectionSims$USVar/sqrt(sigma))) *
  stateData$Undecided[match("US",stateData$State)]
ElectionSims$HarrisMargin <- ElectionSims$HarrisPopVote - ElectionSims$TrumpPopVote

#expects a state name, a vector of the popular vote, and a variable for calculating error
calculateStateMargin <- function(state,popVoteMargin,regionVar){
  #pull all demo data and polling data needed to make calculations
  PVI <- DemoData$PVI[DemoData$District == state]
  HarrisStateAvg <- stateData$HarrisAvg[stateData$State == state]
  TrumpStateAvg <- stateData$TrumpAvg[stateData$State == state]
  UndecidedStateAvg <- stateData$Undecided[stateData$State == state]
  HarrisStateErr <- stateData$HarrisError[stateData$State == state]
  TrumpStateErr <- stateData$TrumpError[stateData$State == state]
  PollCount <- stateData$TimeDiscountedPollCount[stateData$State == state]
  noise <- (4*regionVar - rnorm(4000))/sqrt(16)
  impliedByPVI <- popVoteMargin + PVI
  HarrisResult <- noise * HarrisStateErr + HarrisStateAvg + (pnorm(noise/sqrt(sigma))) * UndecidedStateAvg
  TrumpResult <- -1 * noise * TrumpStateErr + TrumpStateAvg + (1-pnorm(noise/sqrt(sigma))) * UndecidedStateAvg
  return(((HarrisResult - TrumpResult) * PollCount + impliedByPVI)/(1+PollCount))
}

for(state in districts){
  regionVar <- ElectionSims$USVar
  if(state %in% midwest){regionVar <- ElectionSims$midwestVar}else if(state %in% southeast){
    regionVar <- ElectionSims$southeastVar}else if(state%in% southwest){
      regionVar <- ElectionSims$southwestVar
    }
  if(state %in% stateData$State){
    ElectionSims[,state] <- calculateStateMargin(state,ElectionSims$HarrisMargin,regionVar)
  }else{
    ElectionSims[,state] <- regionVar * stateData$HarrisError[stateData$State == "US"] + (2*pnorm(ElectionSims$USVar/sqrt(sigma))-1) *
      stateData$Undecided[stateData$State == "US"] + DemoData$PVI[DemoData$District == state]
  }
}

ElectionSims$ElectoralVotes <- rowSums(sweep((ElectionSims[,districts]>0),MARGIN = 2,unlist(data.table::transpose(list(DemoData$EVs[DemoData$District == districts]))),`*`))

#code to find the tipping point state
findTippingPoint <- function(){
  tippingPoint <- c()
  for(sim in rownames(ElectionSims)){
    winner <- ElectionSims[sim,"ElectoralVotes"] >269
    findTipping <- data.frame(sort(unlist(transpose(ElectionSims[sim,districts])),decreasing = winner))
    votes <- 0
    for(state in rownames(findTipping)){
      if (winner){
        votes <- votes + as.numeric(findTipping[state,]>0)*DemoData$EVs[DemoData$District==state]
      } else{
        votes <- votes + as.numeric(findTipping[state,]<0)*DemoData$EVs[DemoData$District==state]
      }
      if(votes > 269){
        tippingPoint <- c(tippingPoint,state)
        break
      }
    }
    if(votes == 269){
      tippingPoint <- c(tippingPoint,NA)
    }
  }
  tippingPoint
}

ElectionSims$TippingPointState <- findTippingPoint()

writeStateData(readline("Write data files? (Y/N)"))