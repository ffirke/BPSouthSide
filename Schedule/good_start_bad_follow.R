dir <- "C:/Users/Frank/Documents/Blog/Retrosheet/GL"

require(readr)
require(dplyr)
require(magrittr)

asd <- read.csv("https://raw.githubusercontent.com/maxtoki/baseball_R/master/data/game_log_header.csv")

for (i in 1871:2015) {
  temp <- read_csv(paste0(dir,i,".txt"))
  names(temp) <- names(asd)
  temp %<>% select(Date,DoubleHeader,VisitingTeam,HomeTeam,VisitorRunsScored,HomeRunsScore)
  if (i == 1871) {
    stack <- temp
  } else {
    stack <- rbind(stack,temp)
  }
  if (mod(i,10) == 0) {
    print(i)
  }
}

stack %>% filter(VisitorRunsScored != HomeRunsScore) %>% 
  mutate(win = ifelse(VisitorRunsScored > HomeRunsScore,1,0),
                 loss = ifelse(VisitorRunsScored > HomeRunsScore,0,1)) %>% 
  select(Date,DoubleHeader,VisitingTeam,win,loss) %>% rename(team=VisitingTeam) -> vis

stack %>% filter(VisitorRunsScored != HomeRunsScore) %>% mutate(win = ifelse(VisitorRunsScored > HomeRunsScore,0,1),
                 loss = ifelse(VisitorRunsScored > HomeRunsScore,1,0)) %>% 
  select(Date,DoubleHeader,HomeTeam,win,loss) %>% rename(team=HomeTeam) -> home

res <- rbind(home,vis) %>% mutate(year_id = floor(Date/10000)) %>% filter(year_id >= 1876) %>% 
  rename(game_dt=Date,game_ct=DoubleHeader)

res %>% group_by(team,year_id) %>% arrange(game_dt,game_ct) %>% 
  mutate(wins=cumsum(win),losses=cumsum(loss),games=wins+losses) %>% ungroup() -> games

games %>% filter(wins-losses == 13 ) -> thirteenover

games %>% filter(wins < losses) %>% inner_join(thirteenover,by=c("team","year_id")) %>%
  filter(games.x>games.y) %>% group_by(team,year_id,games.y) %>% summarize(gap = min(games.x-games.y)) %>% 
  ungroup() %>% arrange(gap) %>% View()

games %>% filter((wins >= 23 & wins+losses==33) | (wins+losses == 59)) %>% 
  group_by(team,year_id) %>% mutate(rel=n()) %>% filter(rel > 1) %>% ungroup() -> tohist

tohist %>% filter(wins+losses == 59) %>% arrange(wins) -> lossesat59 

games %>% filter(wins < losses & games >= 33) %>% select(team,year_id) %>% distinct() -> everunder
games %>% filter((wins >= 23 & wins+losses==33)) %>% select(team,year_id) %>% distinct() -> hotstart

hotstart %>% anti_join(everunder) %>% nrow()
hotstart %>% inner_join(everunder) %>% nrow()


lossesat59 %>% View()

yy <- read_csv("https://github.com/chadwickbureau/retrosheet/blob/master/gamelog/GL1871.TXT")

