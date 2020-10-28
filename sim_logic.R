### sim logic ###
options(dplyr.summarise.inform = FALSE)

home_team = subset(players, school_name== "Minnesota")
away_team = subset(players, school_name =="Iowa")
home_team = subset(home_team, mins >99)
away_team = subset(away_team, mins >99)
## get which players are on the court. 
player_storing_events <- data.frame(matrix(ncol = 2, nrow = 0))
ragrat <- c("player","outcome")
colnames(player_storing_events) <- ragrat

all_holding_events <- data.frame(matrix(ncol = 13, nrow = 0))
ragrat <- c("player","3pas", "3pms","2pas","2pms", "ftas","fpms","orebs","drebs","stls","tos","blks","game_num")
colnames(all_holding_events) <- ragrat



#player_storing_events = data.frame(player = character(0), outcome = character(0))
#all_holding_events = data.frame(player = character(0), outcome = character(0), game_number = numeric(0))
home_team$chance_on_court = home_team$mins / sum(home_team$mins)
away_team$chance_on_court = away_team$mins / sum(away_team$mins)
home_holder = home_team
away_holder = away_team
game_number = 1
x = 0
 run_sim <- function(nsims){
repeat{

possessions = 0
npossessions = round(runif(1, 130,145),0)

repeat{
  if (dim(player_storing_events)[1] == 0){
      home_team = home_team
      away_team = away_team
  }
  
  if(last(player_storing_events$outcome != "OREB") & possessions !=0){
    home_holder = home_team
    away_holder = away_team
    home_team = away_holder
    away_team = home_holder
  }
  
home_on_court = sample_n(home_team, 5, replace = FALSE, weight = home_team$chance_on_court)
away_on_court = sample_n(away_team, 5, replace = FALSE, weight = away_team$chance_on_court)


`3pa_home` = sum(home_on_court$`3pta_poss`)
`2pa_home` = sum(home_on_court$`2pta_poss`)
`fta_home` = sum(home_on_court$`fta_poss`)
`to_home` = sum(home_on_court$to_poss)

threepa_home = `3pa_home` / (`2pa_home` + `fta_home` + to_home + `3pa_home`)
twopa_home = `2pa_home` / (`2pa_home` + `fta_home` + to_home + `3pa_home`)
freethrowa_home = `fta_home` / (`2pa_home` + `fta_home` + to_home + `3pa_home`)
turnover_home = `to_home` / (`2pa_home` + `fta_home` + to_home + `3pa_home`)

df = data.frame(threepa_home, twopa_home,freethrowa_home, turnover_home)
df$twopa_home = df$threepa_home + df$twopa_home
df$freethrowa_home = df$twopa_home + df$freethrowa_home
df$turnover_home = df$turnover_home + df$freethrowa_home

rand = runif(1,0,1)
df$which_outcome_home = ifelse(rand < df$threepa_home, "3pa", 
                               ifelse(rand < df$twopa_home, "2pa",
                                      ifelse(rand < df$freethrowa_home, "fta",
                                             ifelse(rand <= df$turnover_home, "to","fuck youi"))))


home_outcome = df$which_outcome_home
#print(home_outcome)

if(home_outcome == "3pa"){
  player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`3pta_poss`)
  player_outcome = player_outcome %>% dplyr::select(player, `3p%`)
  rand = runif(1, 0, 1)
  player_outcome$outcome = ifelse(player_outcome$`3p%` > rand, "3P", "3P Miss") ##normally 1 is rand
  player_outcome$`3p%`= NULL
  player_storing_events = rbind(player_storing_events, player_outcome)
  ## on 3 point make, is there a foul? if not, start possession of other team
  ## assign block on a miss if needed.

  if(player_outcome$outcome == "3P Miss"){
    home_oreb = sum(home_on_court$oreb_poss)*0.9
    away_dreb = sum(away_on_court$dreb_poss)
    choose_rebound = c(home_oreb, away_dreb)
    choose_rebound = as.data.frame(choose_rebound)
    options = c("home_oreb", "away_dreb")
    options = as.data.frame(options)
    choose_rebound = cbind(choose_rebound, options)
    choose_rebound$choose_rebound_1 = 1/sum(choose_rebound$choose_rebound)*choose_rebound$choose_rebound
    choose_rebound$choose_rebound = choose_rebound$choose_rebound_1
    choose_rebound$choose_rebound_1 = NULL
    home_oreb_perc = subset(choose_rebound, options == "home_oreb")
    choice = ifelse(home_oreb_perc$choose_rebound < runif(1,0,1), "away_dreb","home_oreb")
    
    
    rand = runif(1,0,1)
    block_3_away = sum(away_on_court$blk_poss)*0.25
    
    if(block_3_away > rand){
      player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$`blk_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "BLK"
      ##start the "possession" over after storing player outcome 0 and 1.
      player_storing_events = rbind(player_storing_events, player_outcome)
    }
    
    if(choice == "home_oreb"){
      player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`oreb_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "OREB"
      ##start the "possession" over after storing player outcome 0 and 1.
      player_storing_events = rbind(player_storing_events, player_outcome)
      
    }
    
    if(choice == "away_dreb"){
      player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$`dreb_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "DREB"
      player_storing_events = rbind(player_storing_events, player_outcome)
      ## end the home possession, start away possession
    }
  }
  
}

if(home_outcome == "2pa"){
  player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`2pta_poss`)
  player_outcome = player_outcome %>% dplyr::select(player, `2p%`)
  rand = runif(1, 0, 1)
  player_outcome$outcome = ifelse(player_outcome$`2p%` > rand, "2P", "2P Miss")
  player_outcome$`2p%`= NULL
  player_storing_events = rbind(player_storing_events, player_outcome)

  ## on a 2 point miss, who gets the rebound? oreb takes us back to the start, dreb begins other teams possession
  ## assign block on miss if needed.
  

  
  if(player_outcome$outcome == "2P Miss"){
    
    home_oreb = sum(home_on_court$oreb_poss)*1.1
    away_dreb = sum(away_on_court$dreb_poss)
    choose_rebound = c(home_oreb, away_dreb)
    choose_rebound = as.data.frame(choose_rebound)
    options = c("home_oreb", "away_dreb")
    options = as.data.frame(options)
    choose_rebound = cbind(choose_rebound, options)
    choose_rebound$choose_rebound_1 = 1/sum(choose_rebound$choose_rebound)*choose_rebound$choose_rebound
    choose_rebound$choose_rebound = choose_rebound$choose_rebound_1
    choose_rebound$choose_rebound_1 = NULL
    home_oreb_perc = subset(choose_rebound, options == "home_oreb")
    choice = ifelse(home_oreb_perc$choose_rebound < runif(1,0,1), "away_dreb","home_oreb")
    
    rand = runif(1,0,1)
    block_2_away = sum(away_on_court$blk_poss)*1.75
    
    if(block_2_away > rand){
      player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$`blk_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "BLK"
      ##start the "possession" over after storing player outcome 0 and 1.
      player_storing_events = rbind(player_storing_events, player_outcome)
    }
    
    if(choice == "home_oreb"){
      player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`oreb_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "OREB"
      ##start the "possession" over after storing player outcome 0 and 1.
      player_storing_events = rbind(player_storing_events, player_outcome)
      
    }
    
    if(choice == "away_dreb"){
      player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$`dreb_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "DREB"
      player_storing_events = rbind(player_storing_events, player_outcome)
      ## end the home possession, start away possession
    }
  }
  
}

if(home_outcome == "fta"){
  player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`fta_poss`)
  player_outcome = player_outcome %>% dplyr::select(player, `ft%`)
  rand = runif(1, 0, 1)
  player_outcome$outcome = ifelse(player_outcome$`ft%`*player_outcome$`ft%` < rand, "2FT",
                                  ifelse(player_outcome$`ft%` > rand, "1FT", "FT Miss"))
  player_outcome$`ft%`= NULL
  player_storing_events = rbind(player_storing_events, player_outcome)
  
  
  if(player_outcome$outcome == "FT Miss"){
    home_oreb = sum(home_on_court$oreb_poss)
    away_dreb = sum(away_on_court$dreb_poss)
    choose_rebound = c(home_oreb, away_dreb)
    choose_rebound = as.data.frame(choose_rebound)
    options = c("home_oreb", "away_dreb")
    options = as.data.frame(options)
    choose_rebound = cbind(choose_rebound, options)
    choose_rebound$choose_rebound_1 = 1/sum(choose_rebound$choose_rebound)*choose_rebound$choose_rebound
    choose_rebound$choose_rebound = choose_rebound$choose_rebound_1
    choose_rebound$choose_rebound_1 = NULL
    home_oreb_perc = subset(choose_rebound, options == "home_oreb")
    choice = ifelse(home_oreb_perc$choose_rebound < runif(1,0,1), "away_dreb","home_oreb")
    
    
    if(choice == "home_oreb"){
      player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`oreb_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "OREB"
      ##start the "possession" over after storing player outcome 0 and 1.
      player_storing_events = rbind(player_storing_events, player_outcome)
      
    }
    
    if(choice == "away_dreb"){
      player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$`dreb_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "DREB"
      player_storing_events = rbind(player_storing_events, player_outcome)
      ## end the home possession, start away possession
    }
  }
  
  
  if(player_outcome$outcome == "1FT"){
    rand = runif(1,0,1)
    if(rand > 0.45){
      if(player_outcome$outcome == "1FT"){
        home_oreb = sum(home_on_court$oreb_poss)*0.45
        away_dreb = sum(away_on_court$dreb_poss)
        choose_rebound = c(home_oreb, away_dreb)
        choose_rebound = as.data.frame(choose_rebound)
        options = c("home_oreb", "away_dreb")
        options = as.data.frame(options)
        choose_rebound = cbind(choose_rebound, options)
        choose_rebound$choose_rebound_1 = 1/sum(choose_rebound$choose_rebound)*choose_rebound$choose_rebound
        choose_rebound$choose_rebound = choose_rebound$choose_rebound_1
        choose_rebound$choose_rebound_1 = NULL
        home_oreb_perc = subset(choose_rebound, options == "home_oreb")
        choice = ifelse(home_oreb_perc$choose_rebound < runif(1,0,1), "away_dreb","home_oreb")
        
        if(choice == "home_oreb"){
          player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`oreb_poss`)
          player_outcome = player_outcome %>% dplyr::select(player)
          player_outcome$outcome = "OREB"
          ##start the "possession" over after storing player outcome 0 and 1.
          player_storing_events = rbind(player_storing_events, player_outcome)
          
        }
        
        if(choice == "away_dreb"){
          player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$`dreb_poss`)
          player_outcome = player_outcome %>% dplyr::select(player)
          player_outcome$outcome = "DREB"
          player_storing_events = rbind(player_storing_events, player_outcome)
          ## end the home possession, start away possession
        }
      }
    }
  }
  
  ## on 2ft, start other teams possession (also add 3ft lol), on 1ft, what are chances that 2nd one is missed (45%), on ft miss, dreb starts other team
  ## possession, oreb goes back to the start
  
  
}

if(home_outcome == "to"){
  player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`to_poss`)
  player_outcome = player_outcome %>% dplyr::select(player)
  player_outcome$outcome = "TO"
  player_storing_events = rbind(player_storing_events, player_outcome)
  
  
  ## start other team possession, assign steal
  rand = runif(1,0,1)
  if(rand > 0.5){ ## 50% of turnovers are steals (CHECK THIS)
    player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$stl_poss)
    player_outcome = player_outcome %>% dplyr::select(player)
    player_outcome$outcome = "STL"
    player_storing_events = rbind(player_storing_events, player_outcome)
  }
  
}




possessions = possessions+1
if(player_outcome$outcome == "OREB"){
  possessions = possessions -1
}




if(possessions >= npossessions){
  
    player_storing_events$`3PA` = ifelse(player_storing_events$outcome == "3P Miss" | player_storing_events$outcome == "3P",1,0)
    player_storing_events$`3PM` = ifelse(player_storing_events$outcome == "3P",1,0)
    player_storing_events$`2PA` = ifelse(player_storing_events$outcome == "2P Miss" | player_storing_events$outcome == "2P",1,0)
    player_storing_events$`2PM` = ifelse(player_storing_events$outcome == "2P",1,0)
    
    player_storing_events$`FTA` = ifelse(player_storing_events$outcome == "FT Miss" | player_storing_events$outcome == "1FT" | player_storing_events$outcome == "2FT",2,0)
    player_storing_events$`FTM` = ifelse(player_storing_events$outcome == "2FT",2,ifelse(player_storing_events$outcome == "1FT",1,0))
    
    player_storing_events$OREB = ifelse(player_storing_events$outcome == "OREB", 1, 0)
    player_storing_events$DREB = ifelse(player_storing_events$outcome == "DREB", 1, 0)
    player_storing_events$BLK = ifelse(player_storing_events$outcome == "BLK", 1, 0)
    player_storing_events$STL = ifelse(player_storing_events$outcome == "STL", 1, 0)
    player_storing_events$TO = ifelse(player_storing_events$outcome == "TO", 1, 0)
    
    player_stats = player_storing_events %>% group_by(player)%>% ## issues with this, j. hamiltons, FAK, add id to player.
      summarise(`3pas` = sum(`3PA`), `3pms` = sum(`3PM`),`2pas` = sum(`2PA`),`2pms` = sum(`2PM`),
                `ftas` = sum(`FTA`),`ftms` = sum(`FTM`),
                `orebs` = sum(`OREB`),`drebs` = sum(`DREB`),`stls` = sum(`STL`),`tos` = sum(`TO`),`blks` = sum(`BLK`))
    
    
    player_stats$game_num = game_number
    all_holding_events = rbind(all_holding_events, player_stats)
    player_storing_events = player_storing_events[0,]
    print(game_number)
    game_number = game_number+1
    break
}
}

if(game_number >= nsims){
  break
}


}
return(all_holding_events)
}

start_time = Sys.time()
g = run_sim(11)
end_time = Sys.time()
end_time - start_time


# all_holding_events = run_sim(2)
# g = all_holding_events[[1]]
#sum(ifelse(g$outcome == "2P", 2,
#                    ifelse(g$outcome == "3P",3, 
#                           ifelse(g$outcome == "1FT",1, 
#                                  ifelse(g$outcome == "2FT",2,0)))))


## oh shit we have not assigned assists.












