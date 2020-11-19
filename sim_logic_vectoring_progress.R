### sim logic vectoring progress is the main sim.

### things to do:

### add 3 point foul possibility. difficult.  ray has 3% chance of a 3 point foul.
### add possession variability based on data
### add defensive stats. % of 2pts, % of 3pts, etc. 3pt%, 2pt%, to%. Use kenpom percentages https://kenpom.com/blog/offense-vs-defense-the-summary/



### to add them. 
library(dplyr)

'%ni%' <- Negate('%in%')

run_sim <- function(nsims){
  repeat{
    
    possessions = 0
    npossessions = round(dqrunif(1, 130,145),0)
    
    while(possessions < npossessions){
      
      if (possessions == 0){
        home_team = home_team
        away_team = away_team
      }
      
      if(outcome != "OREB" & possessions !=0){
        home_holder = home_team
        away_holder = away_team
        home_team = away_holder
        away_team = home_holder
        home_players = as.character(home_team$player)
        away_players = as.character(away_team$player)
        
        home_team_chance_on_court = as.numeric(home_team$mins / sum(home_team$mins))
        away_team_chance_on_court = as.numeric(away_team$mins / sum(away_team$mins)) ## maybe should functionize this, 
      }
      
      
      home_on_court = replicate(1, sample(home_players, 5,prob= home_team_chance_on_court, replace = FALSE))
      away_on_court = replicate(1, sample(away_players, 5,prob= away_team_chance_on_court, replace = FALSE))
      
      home_on_court = sort(home_on_court)
      away_on_court = sort(away_on_court)
      
     
      `3pa_home` = home_team[home_team$player %in% home_on_court,] ### these DO work lol
      `3pa_home` = `3pa_home`[,23]
      #  `3pa_home` = `3pa_home`[,6]
      
      `2pa_home` = home_team[home_team$player %in% home_on_court,]
      `2pa_home` = `2pa_home`[,24]
     # `2pa_home` = `2pa_home`[,19]
      
      `fta_home` = home_team[home_team$player %in% home_on_court,]
      `fta_home` = `fta_home`[,25]
    #  `fta_home` = `fta_home`[,8]
      
      `to_home` = home_team[home_team$player %in% home_on_court,]
      `to_home` = `to_home`[,31]
      # `to_home` = `to_home`[,13]
      
      choose_path_start = c(sum(`3pa_home`), sum(`2pa_home`),sum(`fta_home`)*0.5,sum(`to_home`))
      choose_path_start_words = c("3pa", "2pa","fta","to")
      
      home_outcome = replicate(1, sample(choose_path_start_words, 1,prob= choose_path_start, replace = FALSE))
    

      if(home_outcome == "3pa"){
       `3pa_home` =  as.numeric(unlist(`3pa_home`))
        player_outcome = replicate(1, sample(home_on_court, 1,prob=`3pa_home`, replace = FALSE))
        

        
        player_stat = home_team[home_team$player %in% player_outcome,]
        player_stat = `player_stat`[,33]
        rand = runif(1, 0, 1)
        outcome = ifelse(player_stat > rand, "3P", "3P Miss") ##normally 1 is rand
        
        m = c(outcome,m) ## m is the outcome vector. See below.
        p = c(player_outcome, p) ## p is the player vector. This assigns the outcome above the previous outcome. Wanted or not?
        n = c(game_number, n)
        
        if(outcome ==  "3P"){
          home_ast = home_team[home_team$player %in% home_on_court,] ### these DO work lol
          home_ast = home_ast[home_ast$player %ni% player_outcome,]
          home_possible_asters = as.vector(home_ast$player)
          #home_asts_indiv = home_team[home_team$player %in% home_on_court,]
          home_asts_indiv = as.vector(home_ast[,28])
          
          home_asts_raw = sum(home_ast[,16]) / sum(home_ast[,3])
          rand = runif(1, 0, 1)
          shit = ifelse(home_asts_raw > rand, "AST", "UA")
          if(shit == "AST"){
            `home_asts_indiv` =  as.numeric(unlist(`home_asts_indiv`))
            player_outcome = replicate(1, sample(home_possible_asters, 1,prob= home_asts_indiv, replace = FALSE))
            outcome = "AST"
            m = c(outcome,m) ## m is the outcome vector. See below.
            p = c(player_outcome, p) ## p is the player vector. This assigns the outcome above the previous outcome. Wanted or not?
            n = c(game_number, n)
            
            
          }
        }
        

        if(outcome == "3P Miss"){

          home_oreb = home_team[home_team$player %in% home_on_court,] ### these DO work lol
          home_oreb = home_oreb[,26]
          

          away_dreb = away_team[away_team$player %in% away_on_court,] ### these DO work lol
          away_dreb = away_dreb[,27]
          
          choose_rebound = c(sum(home_oreb)*0.8, sum(away_dreb))

          options = c("home_oreb", "away_dreb")
   
          
          choice = replicate(1, sample(options, 1,prob= choose_rebound, replace = FALSE))
          
          rand = dqrunif(1,0,1)
          
          block_3_away = away_team[away_team$player %in% away_on_court,] ### these DO work lol
          block_3_away = block_3_away[,30]
          block_3_away_sum = c(sum(block_3_away))*0.7

          if(block_3_away_sum > rand){
            `block_3_away` =  as.numeric(unlist(`block_3_away`))
            player_outcome = replicate( 1, sample(away_on_court, 1, prob = block_3_away, replace = FALSE ))
            
            outcome = "BLK"
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)


          }
          
          if(choice == "home_oreb"){
            `home_oreb` =  as.numeric(unlist(`home_oreb`))
            
            player_outcome = replicate( 1, sample(home_on_court, 1, prob = home_oreb, replace = FALSE ))
            
            outcome = "OREB"
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)
            
     
          }
          
          if(choice == "away_dreb"){
            away_dreb = as.numeric(unlist(away_dreb))
            player_outcome = replicate(1, sample(away_on_court, 1, prob = away_dreb, replace = FALSE))
            outcome = "DREB"
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)
            
   
          }
        }
        
      }
      
      if(home_outcome == "2pa"){

        
        `2pa_home` =  as.numeric(unlist(`2pa_home`))
        player_outcome = replicate(1, sample(home_on_court, 1,prob=`2pa_home`, replace = FALSE))
        

        
        player_stat = home_team[home_team$player %in% player_outcome,]
        player_stat = `player_stat`[,34]
        rand = runif(1, 0, 1)
        outcome = ifelse(player_stat > rand, "2P", "2P Miss") ##normally 1 is rand
        m = c(outcome,m) ## m is the outcome vector. See below.
        p = c(player_outcome, p) ## p is the player vector. This assigns the outcome above the previous outcome. Wanted or not?
        n = c(game_number, n)
        
        
        if(outcome ==  "2P"){
          home_ast = home_team[home_team$player %in% home_on_court,] ### these DO work lol
          home_ast = home_ast[home_ast$player %ni% player_outcome,]
          home_possible_asters = as.vector(home_ast$player)
          home_asts_indiv = as.vector(home_ast[,28])
          
          home_asts_raw = sum(home_ast[,16]) / sum(home_ast[,3])
          rand = runif(1, 0, 1)
          shit = ifelse(home_asts_raw > rand, "AST", "UA")
          rand = runif(1,0,1)
          if(shit == "AST"){
            `home_asts_indiv` =  as.numeric(unlist(`home_asts_indiv`))
            player_outcome = replicate(1, sample(home_possible_asters, 1,prob= home_asts_indiv, replace = FALSE))
            outcome = "AST"
            m = c(outcome,m) ## m is the outcome vector. See below.
            p = c(player_outcome, p) ## p is the player vector. This assigns the outcome above the previous outcome. Wanted or not?
            n = c(game_number, n)
            rand = runif(1,0,1)
            
          }
          
          ## add in 1 ft.
          if(runif(1,0,1) < 0.05){ ### change this to runif?
            
            `fta_home` =  as.numeric(unlist(`fta_home`))
            player_outcome = replicate(1, sample(home_on_court, 1,prob=`fta_home`, replace = FALSE))
            player_stat = home_team[home_team$player %in% player_outcome,]
            player_stat = `player_stat`[,35]
            rand = runif(1, 0, 1)
            
            
            choose_ft_outcome = c(player_stat,(1-player_stat))
            choose_ft_words = c("AND ONE","MISS AND ONE")
            
            outcome = replicate(1, sample(choose_ft_words, 1,prob= choose_ft_outcome, replace = FALSE))
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)
            
            if(outcome == "MISS AND ONE"){
              
              
              home_oreb = home_team[home_team$player %in% home_on_court,] ### these DO work lol
              home_oreb = home_oreb[,26]
              
              
              away_dreb = away_team[away_team$player %in% away_on_court,] ### these DO work lol
              away_dreb = away_dreb[,27]
              
              choose_rebound = c(sum(home_oreb), sum(away_dreb)*1.8)
              
              options = c("home_oreb", "away_dreb")
              
              
              choice = replicate(1, sample(options, 1,prob= choose_rebound, replace = FALSE))
              
              
              
              if(choice == "home_oreb"){
                
                `home_oreb` =  as.numeric(unlist(`home_oreb`))
                
                player_outcome = replicate( 1, sample(home_on_court, 1, prob = home_oreb, replace = FALSE ))
                
                outcome = "OREB"
                m = c(outcome,m) ## m is the outcome vector
                p = c(player_outcome, p) ## p is the player vector
                n = c(game_number, n)
                
                
              }
              
              if(choice == "away_dreb"){
                
                away_dreb = as.numeric(unlist(away_dreb))
                player_outcome = replicate(1, sample(away_on_court, 1, prob = away_dreb, replace = FALSE))
                outcome = "DREB"
                m = c(outcome,m) ## m is the outcome vector
                p = c(player_outcome, p) ## p is the player vector
                n = c(game_number, n)
                
              }
            }
          }
        }
        
        if(outcome == "2P Miss"){
         
           home_oreb = home_team[home_team$player %in% home_on_court,] ### these DO work lol
          home_oreb = home_oreb[,26]
          

          away_dreb = away_team[away_team$player %in% away_on_court,] ### these DO work lol
          away_dreb = away_dreb[,27]
          
          choose_rebound = c(sum(home_oreb)*1.2, sum(away_dreb))
          #choose_rebound = as.data.frame(choose_rebound)
          
          options = c("home_oreb", "away_dreb")
 
          
          choice = replicate(1, sample(options, 1,prob= choose_rebound, replace = FALSE))
 
          rand = dqrunif(1,0,1)
          
          block_2_away = away_team[away_team$player %in% away_on_court,] ### these DO work lol
          block_2_away = block_2_away[,30]
          block_2_away_sum = c(sum(block_2_away))*2.8
          
          
          
          if(block_2_away_sum > rand){
    
            `block_2_away` =  as.numeric(unlist(`block_2_away`))
            player_outcome = replicate( 1, sample(away_on_court, 1, prob = block_2_away, replace = FALSE ))
            
            outcome = "BLK"
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)
            
          }
          
          if(choice == "home_oreb"){
    
            `home_oreb` =  as.numeric(unlist(`home_oreb`))
            
            player_outcome = replicate( 1, sample(home_on_court, 1, prob = home_oreb, replace = FALSE ))
            
            outcome = "OREB"
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)
            

          }
          
          if(choice == "away_dreb"){
     
            away_dreb = as.numeric(unlist(away_dreb))
            player_outcome = replicate(1, sample(away_on_court, 1, prob = away_dreb, replace = FALSE))
            outcome = "DREB"
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)
            
          }
        }
        
      }
      
      if(home_outcome == "fta"){
        `fta_home` =  as.numeric(unlist(`fta_home`))
        player_outcome = replicate(1, sample(home_on_court, 1,prob=`fta_home`, replace = FALSE))
        player_stat = home_team[home_team$player %in% player_outcome,]
        player_stat = `player_stat`[,35]
        rand = dqrunif(1, 0, 1)
        

        choose_ft_outcome = c((player_stat*player_stat), (player_stat*(1-player_stat)),(player_stat*(1-player_stat)),((1-player_stat)*(1-player_stat)))
        choose_ft_words = c("2FT", "1FT","1FT","FT Miss")
        
        outcome = replicate(1, sample(choose_ft_words, 1,prob= choose_ft_outcome, replace = FALSE))
        m = c(outcome,m) ## m is the outcome vector
        p = c(player_outcome, p) ## p is the player vector
        n = c(game_number, n)

        
        if(outcome == "FT Miss"){

          
          home_oreb = home_team[home_team$player %in% home_on_court,] ### these DO work lol
          home_oreb = home_oreb[,26]
          

          away_dreb = away_team[away_team$player %in% away_on_court,] ### these DO work lol
          away_dreb = away_dreb[,27]
          
          choose_rebound = c(sum(home_oreb), sum(away_dreb)*1.8)

          options = c("home_oreb", "away_dreb")
        
          
          choice = replicate(1, sample(options, 1,prob= choose_rebound, replace = FALSE))
          
          
          
          if(choice == "home_oreb"){
    
            `home_oreb` =  as.numeric(unlist(`home_oreb`))
            
            player_outcome = replicate( 1, sample(home_on_court, 1, prob = home_oreb, replace = FALSE ))
            
            outcome = "OREB"
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)
            
            
          }
          
          if(choice == "away_dreb"){
    
            away_dreb = as.numeric(unlist(away_dreb))
            player_outcome = replicate(1, sample(away_on_court, 1, prob = away_dreb, replace = FALSE))
            outcome = "DREB"
            m = c(outcome,m) ## m is the outcome vector
            p = c(player_outcome, p) ## p is the player vector
            n = c(game_number, n)
            
          }
        }
        
        
        if(outcome == "1FT"){
          rand = runif(1,0,1)
          if(rand > 0.45){
            if(outcome == "1FT"){

              home_oreb = home_team[home_team$player %in% home_on_court,] ### these DO work lol
              home_oreb = home_oreb[,26]
              

              away_dreb = away_team[away_team$player %in% away_on_court,] ### these DO work lol
              away_dreb = away_dreb[,27]
              
              choose_rebound = c(sum(home_oreb), sum(away_dreb)*1.8)

              options = c("home_oreb", "away_dreb")
        
              
              choice = replicate(1, sample(options, 1,prob= choose_rebound, replace = FALSE))
              
              if(choice == "home_oreb"){
         
                `home_oreb` =  as.numeric(unlist(`home_oreb`))
                
                player_outcome = replicate( 1, sample(home_on_court, 1, prob = home_oreb, replace = FALSE ))
                
                outcome = "OREB"
                m = c(outcome,m) ## m is the outcome vector
                p = c(player_outcome, p) ## p is the player vector
                n = c(game_number, n)
                
                
              }
              
              if(choice == "away_dreb"){
     
                away_dreb = as.numeric(unlist(away_dreb))
                player_outcome = replicate(1, sample(away_on_court, 1, prob = away_dreb, replace = FALSE))
                outcome = "DREB"
                m = c(outcome,m) ## m is the outcome vector
                p = c(player_outcome, p) ## p is the player vector
                n = c(game_number, n)
                
              }
            }
          }
        }
        
        ## on 2ft, start other teams possession (also add 3ft lol), on 1ft, what are chances that 2nd one is missed (45%), on ft miss, dreb starts other team
        ## possession, oreb goes back to the start
        
        
      }
      
      if(home_outcome == "to"){
        `to_home` =  as.numeric(unlist(`to_home`))
        player_outcome = replicate(1, sample(home_on_court, 1,prob=`to_home`, replace = FALSE))
        outcome = "TO"
        m = c(outcome,m) ## m is the outcome vector
        p = c(player_outcome, p) ## p is the player vector
        n = c(game_number, n)
        
        
        

        
        ## start other team possession, assign steal
        rand = dqrunif(1,0,1)
        if(rand > 0.5){ ## 50% of turnovers are steals (CHECK THIS)
          steal_away = away_team[away_team$player %in% away_on_court,] ### these DO work lol
          steal_away = steal_away[,29]
          `steal_away` =  as.numeric(unlist(`steal_away`))
          
          player_outcome = replicate( 1, sample(away_on_court, 1, prob = steal_away, replace = FALSE ))
          
          outcome = "STL"
          m = c(outcome,m) ## m is the outcome vector
          p = c(player_outcome, p) ## p is the player vector

        }
        
      }
    # print(outcome)
      
      
      
      possessions = possessions+1
      if(outcome == "OREB"){ ### fix this.
        possessions = possessions -1
      }
      
      
      
      
      if(possessions >= npossessions){
        
        
        
        final = data.frame(p, m)
        final = final %>% rename("player"="p","stat"="m")
        final = subset(final, !is.na(final$player))
        
        final$`3PA` = ifelse(final$stat == "3P Miss" | final$stat == "3P",1,0)
        final$`3PM` = ifelse(final$stat == "3P",1,0)
        final$`2PA` = ifelse(final$stat == "2P Miss" | final$stat == "2P",1,0)
        final$`2PM` = ifelse(final$stat == "2P",1,0)
        
        final$`FTA` = ifelse(final$stat == "FT Miss" | final$stat == "1FT" | final$stat == "2FT",2,ifelse(
          final$stat == "AND ONE" | final$stat == "MISS AND ONE",1,0
        ))
        final$`FTM` = ifelse(final$stat == "2FT",2,ifelse(final$stat == "1FT",1,ifelse(
          final$stat == "AND ONE",1,0
        )))
        
        final$OREB = ifelse(final$stat == "OREB", 1, 0)
        final$DREB = ifelse(final$stat == "DREB", 1, 0)
        final$AST = ifelse(final$stat == "AST",1,0)
        final$BLK = ifelse(final$stat == "BLK", 1, 0)
        final$STL = ifelse(final$stat == "STL", 1, 0)
        final$TO = ifelse(final$stat == "TO", 1, 0)
        
        player_stats = final %>% group_by(player)%>% ## issues with this, j. hamiltons, FAK, add id to player.
          summarise(`3pas` = sum(`3PA`), `3pms` = sum(`3PM`),`2pas` = sum(`2PA`),`2pms` = sum(`2PM`),
                    `ftas` = sum(`FTA`),`ftms` = sum(`FTM`),
                    `orebs` = sum(`OREB`),`drebs` = sum(`DREB`),`asts` = sum(`AST`),`stls` = sum(`STL`),`tos` = sum(`TO`),`blks` = sum(`BLK`))
        
        p_end = as.character(player_stats$player)
        `3pa_end` = as.numeric(player_stats$`3pas`)
        `3pm_end` = as.numeric(player_stats$`3pms`)
        `2pa_end` = as.numeric(player_stats$`2pas`)
        `2pm_end` = as.numeric(player_stats$`2pms`)
        `fta_end` = as.numeric(player_stats$`ftas`)
        `ftm_end` = as.numeric(player_stats$`ftms`)
        `orebs_end` = as.numeric(player_stats$`orebs`)
        `drebs_end` = as.numeric(player_stats$`drebs`)
        `asts_end` = as.numeric(player_stats$`asts`)
        `stls_end` = as.numeric(player_stats$`stls`)
        `tos_end` = as.numeric(player_stats$`tos`)
        `blks_end` = as.numeric(player_stats$`blks`)
        gamenum_end = rep(game_number, length(p_end))
        
        ## join to existing
        exis_player = c(`p_end`, exis_player)
        exis_3pa = c(`3pa_end`, exis_3pa)
        exis_3pm =c(`3pm_end`, exis_3pm)
        exis_2pa = c( `2pa_end`, exis_2pa)
        exis_2pm = c( `2pm_end`, exis_2pm)
        exis_fta = c( `fta_end`, exis_fta)
        exis_ftm = c( `ftm_end`, exis_ftm)
        exis_orebs = c(`orebs_end`, exis_orebs)
        exis_drebs = c( `drebs_end`, exis_drebs)
        exis_asts = c( `asts_end`, exis_asts)
        exis_stls = c( `stls_end`, exis_stls)
        exis_tos = c( `tos_end`, exis_tos)
        exis_blks = c( `blks_end`, exis_blks)
        exis_gamenum = c( `gamenum_end`, exis_gamenum)
        
        
       # exis_player[i] = p_end
        
        print(game_number)
        game_number = game_number+1
        m = rep(NA, 5)
        p = rep(NA, 5)
        n = rep(NA, 5)
        
        break
        
        
        
        
        
        
        
      }
    }
    
    if(game_number >= nsims){
      break
    }
    
    
  }
 # newlist = list(m, p, n)
  newlist = list(exis_player, exis_3pa,exis_3pm,exis_2pa,exis_2pm,exis_fta,exis_ftm,exis_orebs,exis_drebs,exis_asts,exis_stls,exis_tos, exis_blks, exis_gamenum)
  return(newlist)
}

## oh shit we have not assigned assists.

