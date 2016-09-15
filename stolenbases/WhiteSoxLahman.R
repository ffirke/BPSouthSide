require(readr)
require(dplyr)
require(magrittr)

lahdir <- "C:/Users/Frank/Documents/Blog/Baseball/Lahman/baseballdatabank-master/core/"

batting <- read_csv(paste0(lahdir,"Batting.csv"))
teams <- read_csv(paste0(lahdir,"Teams.csv"))

batnumbers <- batting %>% select(playerID,yearID,stint,teamID,SB,HR) %>% 
  left_join(teams %>% select(yearID,teamID,franchID),by=c("yearID","teamID"))

batnumbers %>% filter(franchID == 'CHW') %>% group_by(yearID) %>% 
  mutate(sbRK = rank(-SB,ties.method='first'),hrRK = rank(-HR,ties.method='first')) %>%
  filter(sbRK == 1 & hrRK == 1)

batnumbers %>% filter(franchID == 'CHW' & HR >= 30 & SB >= 10) %>% write.table("clipboard",sep = '\t',row.names=F)