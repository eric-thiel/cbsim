library(ncaahoopR)
library(dplyr)
library(devtools)
#install_github("lbenz730/ncaahoopR")

ids = ncaahoopR::ids

game_ids = get_game_ids("Iowa","2019-20")
roster_iowa = get_roster("Iowa", "2019-20")

ids_new = as.data.frame(game_ids)
pull = ids_new[1,]

g = get_boxscore(pull)
home = g[[1]]
away = g[[2]]
names(g)->list_name
home$school_name = list_name[[1]]
away$school_name = list_name[[2]]
home$game_id = pull$game_ids
away$game_id = pull$game_ids
final = rbind(home, away)
finito = final[0,]

#ids_new = head(ids_new, 2)


for(i in ids_new$game_ids){
  pull = subset(ids_new, game_ids == i)
  g = get_boxscore(pull)
  Sys.sleep(runif(1,2,10)*runif(1,1,2))
  home = g[[1]]
  away = g[[2]]
  names(g)->list_name
  home$school_name = list_name[[1]]
  away$school_name = list_name[[2]]
  home$game_id = pull$game_ids
  away$game_id = pull$game_ids
  final = rbind(home, away)
  finito = rbind(finito, final)
  print(pull)
}


game_ids = get_game_ids("Minnesota","2019-20")
roster_minny = get_roster("Minnesota", "2019-20")

ids_new = as.data.frame(game_ids)



for(i in ids_new$game_ids){
  pull = subset(ids_new, game_ids == i)
  g = get_boxscore(pull)
  Sys.sleep(runif(1,2,10)*runif(1,1,2))
  home = g[[1]]
  away = g[[2]]
  names(g)->list_name
  home$school_name = list_name[[1]]
  away$school_name = list_name[[2]]
  home$game_id = pull$game_ids
  away$game_id = pull$game_ids
  final = rbind(home, away)
  finito = rbind(finito, final)
  print(pull)
}


#dates = get_master_schedule("2019-11-05")


#g = get_boxscore(head(dates$game_id,1))



troubling = finito

troubling$poss = ifelse(troubling$player == "TEAM", troubling$FGA + (0.475*troubling$FTA)- troubling$OREB + troubling$TO, NA)

teams_possessions = troubling %>% group_by(school_name)%>%
  summarise(sum_poss = sum(poss, na.rm = TRUE), mean_poss = mean(poss, na.rm = TRUE), sum_mins = sum(MIN, na.rm = TRUE))

players = troubling %>% group_by(player)%>%
  summarise(mins = sum(MIN), fgm = sum(FGM), fga = sum(FGA), `3ptm` = sum(`3PTM`), `3pta` = sum(`3PTA`), ftm = sum(FTM), fta = sum(FTA),
            oreb = sum(OREB), dreb = sum(DREB), stl = sum(STL), blk = sum(BLK), to = sum(TO), pf = sum(PF), pts = sum(PTS), school_name = first(school_name))
players =subset(players, players$player !="TEAM")


players$`2ptm` = players$fgm - players$`3ptm`
players$`2pta` = players$fga - players$`3pta`

players = left_join(players, teams_possessions[c("school_name","sum_poss","sum_mins")], by = c("school_name"="school_name"))
players$estimated_poss_on_court = round(players$mins / (players$sum_mins / 5 ) * players$sum_poss,0)


players$`3pta_poss` = players$`3pta` / players$estimated_poss_on_court
players$`2pta_poss` = players$`2pta` / players$estimated_poss_on_court
players$`fta_poss` = players$`fta` / players$estimated_poss_on_court
players$`oreb_poss` = players$`oreb` / players$estimated_poss_on_court
players$`dreb_poss` = players$`dreb` / players$estimated_poss_on_court
players$`stl_poss` = players$`stl` / players$estimated_poss_on_court
players$`blk_poss` = players$`blk` / players$estimated_poss_on_court
players$`to_poss` = players$`to` / players$estimated_poss_on_court
players$`pf_poss` = players$`pf` / players$estimated_poss_on_court

players$`3p%` = players$`3ptm` / players$`3pta`
players$`2p%` = players$`2ptm` / players$`2pta`
players$`ft%` = players$`ftm` / players$`fta`




## adjust player minutes to get estimated possessions on the floor.



## and ones?
## average was like 3% in nba in 2005 lol that is 3% of fgm also had an and one opp.
## back of napkin math to adjust for players shooting more 2s than 3s, more likely to get fouled for an and one. 






#just use percentage of minutes for now






