library(RMySQL)
library(dplyr)

foo<-dbConnect(MySQL(), host="localhost", dbname="retrosheet",user="root",port=3306)
res<-dbSendQuery(foo, "select year_id, game_dt,game_ct, 
                 case when away_score_ct < home_score_ct then away_team_id else home_team_id end as team,
                  1 as loss,0 as win
                 from games;")
g1 <- dbFetch(res,n=-1)                 
res2<-dbSendQuery(foo,                 
                 "select year_id, game_dt,game_ct, 
                 case when away_score_ct > home_score_ct then away_team_id else home_team_id end as team,
                0 as loss,1 as win
                 from games;")


g2 <- dbFetch(res2,n=-1)
dbDisconnect(foo)

games <- rbind(g1,g2) 
games %<>% group_by(team,year_id) %>% arrange(game_dt,game_ct) %>% 
  mutate(wins=cumsum(win),losses=cumsum(loss),games=wins+losses) %>% ungroup()

games %>% filter(wins-losses == 13 ) -> thirteenover

games %>% filter(wins < losses) %>% inner_join(thirteenover,by=c("team","year_id")) %>%
  filter(games.x>games.y) %>% group_by(team,year_id) %>% summarize(gap = min(games.x-games.y)) %>% View()

games %>% filter((wins >= 23 & wins+losses==33) | (wins+losses == 59)) %>% 
  group_by(team,year_id) %>% mutate(rel=n()) %>% filter(rel > 1) %>% ungroup() -> tohist

tohist %>% filter(wins+losses == 59) %>% arrange(wins) %>% View()

require(ggplot2)

tohist %>% filter(wins+losses == 59) %>% ggplot(aes(x=wins)) + geom_histogram()

tohist %>% filter(wins+losses == 59) %>% group_by(wins) %>% tally() %>% ungroup() %>%
  ggplot(aes(x=wins,y=n)) + geom_point()