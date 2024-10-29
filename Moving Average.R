historicalData <- data.frame(runDate=file.info(dir()[str_detect(dir(),"Election Simulation Data2024")])$ctime)
#historicalData$runDate<-file.info(dir()[str_detect(dir(),"Election Simulation Data2024")])$ctime
harrisevData <- c()
trumpevData <- c()
harrispopData <- c()
trumppopData <- c()
for(i in dir()[str_detect(dir(),"Election Simulation Data2024")]){
  temp <- read.csv(i)
  harrisevData <- c(harrisevData,sum(temp$ElectoralVotes>269)/40)
  trumpevData <- c(trumpevData,sum(temp$ElectoralVotes<269)/40)
  harrispopData <-c(harrispopData,mean(temp$HarrisPopVote))
  trumppopData <-c(trumppopData,mean(temp$TrumpPopVote))
}
historicalData$HarrisWinProb <- harrisevData
historicalData$TrumpWinProb <- trumpevData
historicalData$HarrisPopVote <- harrispopData
historicalData$TrumpPopVote <- trumppopData
historicalData$margin <- harrispopData - trumppopData

ggplot(historicalData)+geom_point(aes(x=runDate,y=HarrisWinProb),color="dodgerblue4")+
  stat_smooth(aes(x=runDate,y=HarrisWinProb),se=FALSE,color="dodgerblue")+
  geom_point(aes(x=runDate,y=TrumpWinProb),color="firebrick")+
  stat_smooth(aes(x=runDate,y=TrumpWinProb),se=FALSE,color="firebrick1")

ggplot(historicalData)+geom_point(aes(x=runDate,y=HarrisPopVote),color="dodgerblue4")+
  stat_smooth(aes(x=runDate,y=HarrisPopVote),se=FALSE,color="dodgerblue")+
  geom_point(aes(x=runDate,y=TrumpPopVote),color="firebrick")+
  stat_smooth(aes(x=runDate,y=TrumpPopVote),se=FALSE,color="firebrick1")

ggplot(historicalData)+geom_point(aes(x=runDate,y=margin),color="dodgerblue4")+
  stat_smooth(aes(x=runDate,y=margin),se=FALSE,color="dodgerblue")