require(readr)
require(dplyr)
require(magrittr)

# Raw Retrosheet Transactions Data

# http://www.retrosheet.org/transactions/

transactions <- read_csv("C:/Users/Frank/Documents/Blog/Baseball/Transactions/tran.txt",col_names = c("date","time","approx","date2","approx2","tid","player","type","fteam","fleague",
                                                                "tteam","tleague","dtype","dround","pick","info"))


# http://www.seanlahman.com/baseball-archive/statistics/

lahdir <- "C:/Users/Frank/Documents/Blog/Baseball/Lahman/baseballdatabank-master/core/"

master <- read_csv(paste0(lahdir,"Master.csv"))

mas <- master %>% select(playerID,retroID,nameFirst,nameLast,birthYear,birthMonth) %>% 
  mutate(Name = paste(nameFirst,nameLast)) %>% select(-nameFirst,-nameLast)

batting <- read_csv(paste0(lahdir,"Batting.csv"))
pitching <- read_csv(paste0(lahdir,"Pitching.csv"))
teams <- read_csv(paste0(lahdir,"Teams.csv"))

batgames <- batting %>% select(playerID,yearID,stint,teamID,G) %>% left_join(teams %>% select(yearID,teamID,franchID),by=c("yearID","teamID"))
pitgames <- pitching %>% select(playerID,yearID,stint,teamID,G,GS) %>% mutate(G = 2.5*(G + GS)) %>% select(-GS) %>% 
  left_join(teams %>% select(yearID,teamID,franchID),by=c("yearID","teamID"))

games <- rbind_list(batgames,pitgames) %>% group_by(playerID,yearID,stint,teamID,franchID) %>% summarize(G = min(162,max(G))) %>% ungroup()

gamescum <- games %>% group_by(playerID) %>% arrange(yearID,stint) %>% 
  mutate(dummy = (franchID != lag(franchID) | is.na(lag(franchID))  ) ,cumteams = cumsum(dummy)) %>% ungroup() %>%
  group_by(playerID,cumteams) %>% mutate(cumG = cumsum(G)) %>% ungroup() %>% select(playerID,yearID,teamID,franchID,cumG)

# Dates for seasons
# Retrosheet Database

library(RMySQL)
library(dplyr)

foo<-dbConnect(MySQL(), host="localhost", dbname="retrosheet",user="root",port=3306)
res<-dbSendQuery(foo, "select year_id, cast(min(substr(game_id,4,8)) as unsigned) as min_date, 
  cast(max(substr(game_id,4,8)) as unsigned) as max_date
                 from games group by year_id;")

startend <- dbFetch(res,n=-1)
dbDisconnect(foo)

released <- transactions %>% filter(type %in% c("R","W")) %>% mutate(year_id = as.numeric(substr(as.character(date),1,4)))
midyear <- released %>% inner_join(startend,by="year_id") %>% filter(date >= min_date & date <= max_date) %>%
  left_join(mas,by=c("player"="retroID")) %>%
  mutate(fteam = ifelse(fteam == "ANA" & year_id >= 2005,"LAA",fteam)) %>% filter(year_id >= 1980)

midyeargames <- midyear %>% left_join(games,by=c("playerID"="playerID","year_id"="yearID","fteam"="teamID")) %>% 
  mutate(G = ifelse(is.na(G),0,G)) %>% left_join(gamescum,by=c("playerID"="playerID","year_id"="yearID","fteam"="teamID"))

require(ggplot2)

midyeargames %>% filter(cumG > 0) %>% ggplot(aes(x=cumG)) + 
  geom_histogram() + geom_vline(xintercept=246*5,color="red") + 
  ggtitle("Distribution of Adjusted Games Played During Current Stint with Franchise By Players Released Midseason\nAt Least 1 MLB G, Since 1980") +
  labs(x="Adjusted Games Played",y="Number of Players")

midyeargames %>% 
  mutate(Age = ifelse(birthMonth>6,year_id-birthYear-1,year_id-birthYear),
         strdate = as.character(date),Date=paste(substr(strdate,1,4),substr(strdate,5,6),substr(strdate,7,8),sep="-")) %>% 
  select(Name,fteam,Date,Age,cumG) %>% arrange(-cumG) %>% rename(`Adjusted Games` = cumG,Team = fteam) %>% head(19) -> totable

totable %>% write.table("clipboard",sep = '\t',row.names=F)