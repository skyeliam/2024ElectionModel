histGenerator <- function(){
breaktest <- c(105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,210,215,220,225,230,235,240,
               245,250,255,260,265,268.5,269,269.5,275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,
               360,365,370,375,380,385,390,395,400,405,410,415,420,425,430,435,440,445,450,455,460)
gram <- hist(ElectionSims$ElectoralVotes,breaks = breaktest,freq=TRUE)
colorbreaks <- rep("firebrick1",length(gram$breaks))
colorbreaks[gram$breaks > 268] <- "grey"
colorbreaks[gram$breaks > 269] <- "dodgerblue"
hist(ElectionSims$ElectoralVotes, col = colorbreaks, breaks = breaktest,freq=TRUE,
     xlab = "Harris Electoral Votes",
     main = paste("Harris wins",
                  sprintf("%.1f%%",sum(ElectionSims$ElectoralVotes>=270)/40),"of simulations"))
}
histGenerator()

ElectionSims %>% mutate(quadrant = case_when(ElectionSims$HarrisMargin>0 & ElectionSims$ElectoralVotes > 270 ~"Harris wins PV and EC",
                                             ElectionSims$HarrisMargin<0 & ElectionSims$ElectoralVotes > 270 ~"Harris loses PV and wins EC",
                                             ElectionSims$HarrisMargin>0 & ElectionSims$ElectoralVotes < 270 ~"Trump loses PV and wins EC",
                                             ElectionSims$HarrisMargin<0 & ElectionSims$ElectoralVotes < 270 ~"Trump wins PV and wins EC",
                                             TRUE ~ "Electoral College tie")) %>%
  ggplot(aes(x=ElectionSims$HarrisMargin,y=ElectionSims$ElectoralVotes-270,color = quadrant)) + geom_jitter() + 
  scale_color_manual(values = c(`Harris wins PV and EC` = 'dodgerblue4', `Harris loses PV and wins EC` = 'dodgerblue', 
                                `Trump loses PV and wins EC` = 'firebrick1', 
                                `Trump wins PV and wins EC` = 'firebrick',`Electoral College tie` = "darkgrey")) +
  ggtitle("Popular vote margin vs. Electoral College margin") + xlab("Harris popular vote margin (%)") +
  ylab("Harris Electoral College vote margin")
  
