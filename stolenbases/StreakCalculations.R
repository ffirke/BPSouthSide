require(XML)
require(dplyr)
require(Lahman)

a <- Batting

# Pulls SB leaders by year to put this year in context

a %>% filter(teamID == 'CHA' & !is.na(SB)) %>% group_by(yearID) %>% mutate(SBM = max(SB)) %>% ungroup() %>% 
  filter(SB==SBM) %>%
  arrange(-yearID) -> sbleaders

b <- Fielding

# Looks for 3B leaders in the past

b %>% group_by(playerID,yearID,POS) %>% summarize(G = sum(G)) %>% 
  group_by(playerID,yearID) %>% mutate(maxG = max(G)) %>% filter(G == maxG) -> innings

sbleaders %>% left_join(innings,by=c("yearID",'playerID')) %>% filter(POS == '3B') 

## Pulling streaks below average

# Downloading data

asd<- readHTMLTable("http://www.baseballprospectus.com/sortable/index.php?cid=1962963")

y <- asd[['TTdata']]

y %>% arrange(TEAM,YEAR) %>% mutate(above = as.numeric(as.character(SBR)) > 0) %>% group_by(TEAM) %>% 
  mutate(absum = cumsum(above)) -> cumul

# Longest streaks without being above 0
# Doesn't account for franchise name changes, but those don't affect the top of the list as well

cumul %>% ungroup() %>% group_by(TEAM,absum) %>% tally() %>% arrange(-n) %>% View()