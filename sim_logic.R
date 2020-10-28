### sim logic ###

home_team = subset(players, school_name== "Minnesota")
away_team = subset(players, school_name =="Iowa")
home_team = subset(home_team, mins >99)
away_team = subset(away_team, mins >99)
## get which players are on the court. 

player_storing_events = data.frame(player = character(0), outcome = character(0))



home_team$chance_on_court = home_team$mins / sum(home_team$mins)
away_team$chance_on_court = away_team$mins / sum(away_team$mins)








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

rm(`3pa_home`)
rm(`2pa_home`)
rm(`fta_home`)
rm(`to_home`)

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
print(home_outcome)

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
    home_oreb = sum(home_on_court$oreb_poss)
    away_dreb = sum(away_on_court$dreb_poss)
    choose_rebound = c(home_oreb, away_dreb)
    choose_rebound = as.data.frame(choose_rebound)
    options = c("home_oreb", "away_dreb")
    options = as.data.frame(options)
    choose_rebound = cbind(choose_rebound, options)
    choice = sample_n(choose_rebound, 1, replace = FALSE, weights = choose_rebound$choose_rebound)
    
    rand = runif(1,0,1)
    block_3_away = sum(away_on_court$blk_poss)*0.25
    
    if(block_3_away < rand){
      player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$`blk_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "BLK"
      ##start the "possession" over after storing player outcome 0 and 1.
      player_storing_events = rbind(player_storing_events, player_outcome)
    }
    
    if(choice$options == "home_oreb"){
      player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`oreb_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "OREB"
      ##start the "possession" over after storing player outcome 0 and 1.
      player_storing_events = rbind(player_storing_events, player_outcome)
      
    }
    
    if(choice$options == "away_dreb"){
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
  
  rand = runif(1,0,1)
  block_2_away = sum(away_on_court$blk_poss)*0.75
  
  if(block_2_away < rand){
    player_outcome = sample_n(away_on_court, 1, replace = FALSE, weight = away_on_court$`blk_poss`)
    player_outcome = player_outcome %>% dplyr::select(player)
    player_outcome$outcome = "BLK"
    ##start the "possession" over after storing player outcome 0 and 1.
    player_storing_events = rbind(player_storing_events, player_outcome)
  }
  
  if(player_outcome$outcome == "2P Miss"){
    home_oreb = sum(home_on_court$oreb_poss)
    away_dreb = sum(away_on_court$dreb_poss)
    choose_rebound = c(home_oreb, away_dreb)
    choose_rebound = as.data.frame(choose_rebound)
    options = c("home_oreb", "away_dreb")
    options = as.data.frame(options)
    choose_rebound = cbind(choose_rebound, options)
    choice = sample_n(choose_rebound, 1, replace = FALSE, weights = choose_rebound$choose_rebound)
    
    if(choice$options == "home_oreb"){
      player_outcome = sample_n(home_on_court, 1, replace = FALSE, weight = home_on_court$`oreb_poss`)
      player_outcome = player_outcome %>% dplyr::select(player)
      player_outcome$outcome = "OREB"
      ##start the "possession" over after storing player outcome 0 and 1.
      player_storing_events = rbind(player_storing_events, player_outcome)
      
    }
    
    if(choice$options == "away_dreb"){
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
  player_outcome$outcome = ifelse(player_outcome$`ft%`*player_outcome$`ft%` > rand, "2FT",
                                  ifelse(player_outcome$`ft%` > rand, "1FT", "FT Miss"))
  player_outcome$`ft%`= NULL
  
  
  
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

print(paste(player_outcome$player, player_outcome$outcome))


