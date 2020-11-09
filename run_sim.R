### sim logic ###
options(dplyr.summarise.inform = FALSE)

library(dqrng)
home_team = subset(players, school_name== "Minnesota")
away_team = subset(players, school_name =="Iowa")
game_players= subset(players, school_name == "Minnesota" | school_name == "Iowa")
home_team = subset(home_team, mins >99)
away_team = subset(away_team, mins >99)
home_team = write.csv(home_team, file = "home_hold.csv")
away_team = write.csv(away_team, file = "away_hold.csv")

## get which players are on the court. 
player_storing_events <- data.frame(matrix(ncol = 2, nrow = 0))
ragrat <- c("player","outcome")
colnames(player_storing_events) <- ragrat

all_holding_events <- data.frame(matrix(ncol = 13, nrow = 0))
ragrat <- c("player","3pas", "3pms","2pas","2pms", "ftas","fpms","orebs","drebs","asts","stls","tos","blks","game_num")
colnames(all_holding_events) <- ragrat



player_storing_events = data.frame(player = character(0), outcome = character(0))
#all_holding_events = data.frame(player = character(0), outcome = character(0), game_number = numeric(0))
home_team$chance_on_court = home_team$mins / sum(home_team$mins)
away_team$chance_on_court = away_team$mins / sum(away_team$mins)
home_holder = home_team
away_holder = away_team







home_players = as.character(home_team$player)
away_players = as.character(away_team$player)

home_team_chance_on_court = as.numeric(home_team$mins / sum(home_team$mins))
away_team_chance_on_court = as.numeric(away_team$mins / sum(away_team$mins))

m = rep(NA, 1)
p = rep(NA, 1)
n = rep(NA, 1)

hold = 1

exis_player = rep(NA, hold)
exis_3pa = rep(NA, hold)
exis_3pm = rep(NA, hold)
exis_2pa = rep(NA, hold)
exis_2pm = rep(NA, hold)
exis_fta = rep(NA, hold)
exis_ftm = rep(NA, hold)
exis_orebs = rep(NA, hold)
exis_drebs = rep(NA, hold)
exis_asts = rep(NA, hold)
exis_stls = rep(NA, hold)
exis_tos = rep(NA, hold)
exis_blks = rep(NA, hold)
exis_gamenum = rep(NA, hold)

game_number = 1
x = 0

start_time = Sys.time()
g = run_sim(1001)


#stat = g[[1]]
#player = g[[2]]
#game_num = g[[3]]

player = g[[1]]
`3PA` = g[[2]]
`3PM` = g[[3]]
`2PA` = g[[4]]
`2PM` = g[[5]]
`FTA` = g[[6]]
`FTM` = g[[7]]
`OREB` = g[[8]]
`DREB` = g[[9]]
`AST` = g[[10]]
`STL` = g[[11]]
`TO` = g[[12]]
`BLK` = g[[13]]
`G#` = g[[14]]

final = data.frame(player, `3PA`, `3PM`,`2PA`,`2PM`,`FTA`,`FTM`,`OREB`,`DREB`,`AST`,`STL`,`TO`,`BLK`,`G#`)
final = subset(final, !is.na(final$player))
final = left_join(final, game_players[c("player","school_name")], by = c("player"="player"))

scores = final %>% group_by(school_name, `G.`)%>%
  summarise(score = sum(`X3PM`)*3 + sum(`X2PM`)*2 + sum(FTM))

team1 = subset(scores, school_name == "Minnesota")
team2 = subset(scores, school_name == "Iowa")

scores = left_join(team1, team2, by = c("G."="G."))


end_time = Sys.time()
end_time - start_time

oturu = subset(final, player == "D. Oturu" )
oturu$fpts = oturu$X3PM*3.5 + oturu$X2PM*2 + oturu$FTM + oturu$OREB * 1.25 + oturu$DREB*1.25 + oturu$AST * 1.5 + oturu$STL * 2 + oturu$BLK * 2 + oturu$TO*-0.5 

garza = subset(final, player == "L. Garza" )
garza$fpts = garza$X3PM*3.5 + garza$X2PM*2 + garza$FTM + garza$OREB * 1.25 + garza$DREB*1.25 + garza$AST * 1.5 + garza$STL * 2 + garza$BLK * 2 + garza$TO*-0.5 


#### orebs too high for oturu and garza, fta's too high for oturu and garza



