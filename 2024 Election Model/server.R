#pull the latest simulation data
latestSimData <- read.csv(url("https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/Latest_Election%20Simulation%20Data.csv"))
colnames(latestSimData) <- str_replace_all(colnames(latestSimData),"\\."," ")
timeStamp <- StrExtractBetween(read_file(url(
  "https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/timestamp.txt"))," ","\n")

#function for generating histogram of all EV outcomes
histGenerator <- function(){
  breaktest <- c(105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,210,215,220,225,230,235,240,
                 245,250,255,260,265,268.5,269,269.5,275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,
                 360,365,370,375,380,385,390,395,400,405,410,415,420,425,430,435,440,445,450,455,460)
  gram <- hist(latestSimData$ElectoralVotes,breaks = breaktest,freq=TRUE)
  colorbreaks <- rep("firebrick1",length(gram$breaks))
  colorbreaks[gram$breaks > 268] <- "grey"
  colorbreaks[gram$breaks > 269] <- "dodgerblue"
  hist(latestSimData$ElectoralVotes, col = colorbreaks, breaks = breaktest,freq=TRUE,
       xlab = "Harris Electoral Votes",main = NULL)
}

#function for generating a dot plot of EV outcomes vs popular vote
dotplotGenerator <- function(){
  latestSimData %>% mutate(quadrant = case_when(latestSimData$HarrisMargin>0 & latestSimData$ElectoralVotes >= 270 ~"Harris wins PV and EC",
                                                latestSimData$HarrisMargin<0 & latestSimData$ElectoralVotes >= 270 ~"Harris loses PV and wins EC",
                                                latestSimData$HarrisMargin>0 & latestSimData$ElectoralVotes < 269 ~"Trump loses PV and wins EC",
                                                latestSimData$HarrisMargin<0 & latestSimData$ElectoralVotes < 269 ~"Trump wins PV and wins EC",
                                                TRUE ~ "Electoral College tie")) %>%
    ggplot(aes(x=latestSimData$HarrisMargin,y=latestSimData$ElectoralVotes-270,color = quadrant)) + geom_jitter() + 
    scale_color_manual(values = c(`Harris wins PV and EC` = 'dodgerblue4', `Harris loses PV and wins EC` = 'dodgerblue', 
                                  `Trump loses PV and wins EC` = 'firebrick1', 
                                  `Trump wins PV and wins EC` = 'firebrick',`Electoral College tie` = "darkgrey")) +
    xlab("Harris popular vote margin (%)") +
    ylab("Harris Electoral College vote margin")
}

#function for generating the range of states as a forestplot
stateRangeGenerator <- function(){
  districts <- c(state.name,"District of Columbia","Maine CD 1","Maine CD 2","Nebraska CD 2")
  mean  <- unlist(lapply(latestSimData[,districts],mean))
  lower <- unlist(lapply(latestSimData[,districts],quantile,probs=1/20))
  upper <- unlist(lapply(latestSimData[,districts],quantile,probs=19/20))
  zeropoint <- ifelse(lower<0 & upper>0,0,NA)
  lineColor <- ifelse(upper<0,"firebrick1",ifelse(lower>0,"dodgerblue1",NA))
  
  df <- data.frame(districts, mean, lower, upper,zeropoint)
  
  #flip the axes
  df$districts <- factor(df$districts, levels=rev(df$districts))
  
  dotcolors <- ifelse(mean>0,"dodgerblue4","firebrick")
  #generate the forest plot
  ggplot(data=df) + geom_segment(aes(x=lower,xend=zeropoint,y=districts,yend=districts),colour="firebrick1") + 
    geom_segment(aes(x=zeropoint,xend=upper,y=districts,yend=districts),colour="dodgerblue1") + 
    geom_segment(aes(x=lower,xend=upper,y=districts,yend=districts),colour=lineColor) +
    geom_point(aes(x = mean, y = districts),color=dotcolors) + xlab("Forecasted Margin (%)") + ylab("State/District") + theme_bw()
}

singleStateLine <- function(state){
  mean  <- mean(latestSimData[,state])
  lower <- quantile(latestSimData[,state],probs=1/20)
  upper <- quantile(latestSimData[,state],probs=19/20)
  zeropoint <- ifelse(lower<0 & upper>0,0,NA)
  lineColor <- ifelse(upper<0,"firebrick1",ifelse(lower>0,"dodgerblue1",NA))
  dotcolors <- ifelse(mean>0,"dodgerblue4","firebrick")
  df <- data.frame(state, mean, lower, upper,zeropoint)
  
  TrumpOrHarris <- function(val){
    ifelse(val<0,paste0("Trump \n+",sprintf("%.2f%%",abs(val))),paste0("Harris \n+",sprintf("%.2f%%",val)))
  }
  if(!is.na(zeropoint)){
    ggplot(data=df) + geom_segment(aes(x=lower,xend=zeropoint,y=state,yend=state),colour="firebrick1",size=3) + 
      geom_segment(aes(x=zeropoint,xend=upper,y=state,yend=state),colour="dodgerblue1",size=3) + 
      geom_segment(aes(x=lower,xend=upper,y=state,yend=state),colour=lineColor,size=3) +
      geom_point(aes(x = mean, y = state),color=dotcolors,size = 3) + xlab(paste("95th Percentile Range in",state)) +
      geom_text(aes(x = mean, y = state, label = TrumpOrHarris(mean),vjust=2)) + 
      geom_text(aes(x = lower, y = state, label = TrumpOrHarris(lower),vjust=-1)) +
      geom_text(aes(x = upper, y = state, label = TrumpOrHarris(upper),vjust=-1)) +
      ylab("")+ theme_bw() + theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.ticks.x=element_blank()) + 
      xlim(lower -5,upper +5)
  }else{
    ggplot(data=df) + 
      geom_segment(aes(x=lower,xend=upper,y=state,yend=state),colour=lineColor,size=3) +
      geom_point(aes(x = mean, y = state),color=dotcolors,size = 3) + xlab(paste("95th Percentile Range in",state)) +
      geom_text(aes(x = mean, y = state, label = TrumpOrHarris(mean),vjust=2)) + 
      geom_text(aes(x = lower, y = state, label = TrumpOrHarris(lower),vjust=-1)) +
      geom_text(aes(x = upper, y = state, label = TrumpOrHarris(upper),vjust=-1)) +
      ylab("")+ theme_bw() + theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.ticks.x=element_blank())+
      xlim(ifelse(lower>0,-5,lower-5),ifelse(upper<0,5,upper+5))
  }
}



server <- function(input, output, session) {
  output$header <- renderText({
    paste0("<h1>Welcome to Liam's 2024 Presidential Election Nowcast</h1><p1>Data last updated on ",
           paste(format(as.Date(timeStamp),format="%B %d, %Y"),"at",StrExtractBetween(timeStamp," ","\\.")),
           "</p>")
  })
  state <- reactive({
    latestSimData[,input$statePick]
  })
  output$ECHist <- renderPlot({
    histGenerator()
  }, res = 96)
  output$PopVsEC <- renderPlot({
    dotplotGenerator()
  }, res = 96)
  output$forestPlot <- renderPlot({
    stateRangeGenerator()
  }, res = 96)
  output$stateForest <- renderPlot({
    singleStateLine(input$statePick)
  }, res = 96)
  output$summary <- renderText({
    winProb <- sum(state()>0)/4000
    outputString <- paste0("Harris has a ",sprintf("%.2f%%",winProb*100)," chance of winning ",input$statePick)
    outputString <- paste0(outputString, "<p>","Her average margin in ",input$statePick, " is ", sprintf("%.2f%%",mean(state())),"</p>")
    outputString
  })
  output$footer <- renderText({
    paste0("<footer>Built by Liam C. Stewart, www.liamstewart.com <br>
    Polling data from 538. State partisan index data from Cook Political Report.<br>
           Built in R using Shiny. Deployed via shinyapps.io.</footer>")
  })
}
