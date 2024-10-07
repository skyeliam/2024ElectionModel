latestSimData <- read.csv(url("https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/Latest_Election%20Simulation%20Data.csv"))

histGenerator <- function(){
breaktest <- c(105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,210,215,220,225,230,235,240,
               245,250,255,260,265,268.5,269,269.5,275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,
               360,365,370,375,380,385,390,395,400,405,410,415,420,425,430,435,440,445,450,455,460)
gram <- hist(latestSimData$ElectoralVotes,breaks = breaktest,freq=TRUE)
colorbreaks <- rep("firebrick1",length(gram$breaks))
colorbreaks[gram$breaks > 268] <- "grey"
colorbreaks[gram$breaks > 269] <- "dodgerblue"
hist(latestSimData$ElectoralVotes, col = colorbreaks, breaks = breaktest,freq=TRUE,
     xlab = "Harris Electoral Votes",
     main = paste("Harris wins",
                  sprintf("%.1f%%",sum(latestSimData$ElectoralVotes>=270)/40),"of simulations"))
}
histGenerator()

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
  ggtitle("Popular vote margin vs. Electoral College margin") + xlab("Harris popular vote margin (%)") +
  ylab("Harris Electoral College vote margin")
}
dotplotGenerator()

stateRangeGenerator <- function(){
  districts <- c(state.name,"District of Columbia","Maine CD-1","Maine CD-2","Nebraska CD-2")
  districts <- str_replace_all(districts," ",".")
  districts <- str_replace_all(districts,"-",".")
  mean  <- unlist(lapply(latestSimData[,districts],mean))
  lower <- unlist(lapply(latestSimData[,districts],quantile,probs=1/20))
  upper <- unlist(lapply(latestSimData[,districts],quantile,probs=19/20))
  zeropoint <- ifelse(lower<0 & upper>0,0,NA)
  lineColor <- ifelse(upper<0,"firebrick1",ifelse(lower>0,"dodgerblue1",NA))
  
  df <- data.frame(districts, mean, lower, upper,zeropoint)
  
  # reverses the factor level ordering for labels after coord_flip()
  df$districts <- factor(df$districts, levels=rev(df$districts))
  
  library(ggplot2)
  fp <- ggplot(data=df, aes(x=districts, y=mean, ymin=lower, ymax=upper)) +
    geom_pointrange() + 
    geom_hline(yintercept=0, lty=2) +  
    coord_flip() +
    xlab("State") + ylab("Mean (95% CI)") +
    theme_bw()  # use a white background
  
  #trying a multicolor range
  ggplot(data=df) + geom_segment(aes(x=lower,xend=zeropoint,y=districts,yend=districts),colour="firebrick1") + 
    geom_segment(aes(x=zeropoint,xend=upper,y=districts,yend=districts),colour="dodgerblue1") + 
    geom_segment(aes(x=lower,xend=upper,y=districts,yend=districts),colour=lineColor) +
    geom_point(x = mean, y = districts) + xlab("Forecasted Margin (%)") + ylab("State/District") + theme_bw()
  }