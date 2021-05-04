#select data to import
df_2019 = read.csv("D:/Downloads/2019_LoL_esports_match_data_from_OraclesElixir_20201202.csv", header = TRUE)
df_necessary_2019 = subset(df_2019, select = c('league', 'split', 'playoffs', 'team', 'player', 'position', 'earned.gpm'))
lcs_2019 = df_necessary_2019[df_necessary_2019$player!='' & df_necessary_2019$split == 'Summer' , ]

df_2020 = read.csv("D:/Downloads/2020_LoL_esports_match_data_from_OraclesElixir_20201124.csv", header = TRUE)
df_necessary_2020 = subset(df_2020, select = c('league', 'split', 'playoffs', 'team', 'player', 'position', 'earned.gpm'))
lcs_2020 = df_necessary_2020[((df_necessary_2020$league == 'LCS' | df_necessary_2020$league == 'LCS.A') & df_necessary_2020$player!=''),]
lcs_training_2020 = lcs_2020[lcs_2020$split == 'Spring', ]
lcs_a = lcs_2020[lcs_2020$league == 'LCS.A' & lcs_2020$split == 'Summer', ]

lcs_training = rbind(lcs_2019, lcs_training_2020,lcs_a)
players = unique(lcs_training$player)
n_players = length(players)

#function to assign earned gold per minute data to players
#returns vector of earned gpm
create_gold_arr = function(lcs_training){
  all_gold = list()
  eligible_players = list()
  for(i in c(1:n_players)){
    player = players[i]
    #print(player)
    gold = lcs_training[lcs_training$player == player,]$earned.gpm
    if (length(gold) > 5){
      all_gold[[i]] = gold
      eligible_players[[i]] = player
    }
  }
  names(all_gold) = eligible_players
  all_gold = all_gold[!sapply(all_gold,is.null)]
  return(all_gold)
}
all_gold = create_gold_arr(lcs_training)

#obtain a list of all players using Summer 2020 data
col = c('team','player','position')
lcs_test = lcs_2020[lcs_2020$league == 'LCS' & lcs_2020$split == 'Summer',]
roster_info = (subset(lcs_test, select = col))
games_played = aggregate(roster_info$player, by=list(roster_info$team,roster_info$player,roster_info$position), FUN=length)
names(games_played) = c(col, 'count')
games_played = games_played[order(games_played$team, games_played$position, -games_played$count),]
main_roster_info = games_played[!duplicated(games_played[c('team','position')]),]

#create latest rosters 
teams = unique(main_roster_info$team)
n_teams = length(teams)
rosters = list()
for(i in c(1:n_teams)){
  team = teams[i]
  name = main_roster_info[main_roster_info$team == team,]$player
  role = main_roster_info[main_roster_info$team == team,]$position
  rosters[[i]] = name
}
names(rosters) = teams

# function to generate bootstrapped data
# returns the bootstrapped data set
bootstrap = function(data, num_games){
  set.seed(42)
  #get mean and length of stomach vector
  n = length(data)
  mu = mean(data)
  #perform bootstrap n times
  bootstraps = list()
  for(i in c(1:num_games)){
    bootstraps[[i]] = sample(data, n, replace = TRUE)
  }
  #take the mean of each bootstrap
  mu_j = unlist(lapply(bootstraps,mean))
  return(mu_j)
}

# function to calculate the normal kernel estimation with a provided xi,
# the dataset, and bandwidth
norm_kernel_est = function(x, data, h){
  sigma = sd(data)
  n = length(data)
  z = (x-data)/h
  k = exp((-z^2)/2)/sqrt(2*pi)
  f = sum(k)/(n*h)
  return(f)
}

# function used to calculate the player KDE based on the provided data
# using silverman's rule as the bandwidth
calc_player_kernels = function(name, all_gold){
  x = all_gold[[name]]
  n = length(x)
  sigma = sd(x)
  #set x-values to estimate
  s = seq(min(x),max(x),1)
  
  # calculate bandwidth via silvermans rule
  h_silverman = ((4*sigma^5)/(3*n))^0.2
  f_silver = rep(NA,length(s))
  # generate estimates
  for(i in c(1:length(s))){
    f_silver[[i]] = norm_kernel_est(s[[i]], x, h_silverman)
  }
  return(f_silver)
}

# function to create all kernel density estimates for the players
# bootstrap when the number of matches is under the minimum amount
# returns the updated all_gold vector and a vector with the calculated
# kernel density estimates
create_kde = function(all_gold){
  player_kernels = list()
  for(i in c(1:length(all_gold))){
    name = names(all_gold)[i]
    player_data = all_gold[[name]]
    n = length(player_data)
    if(n < 36){
      mu_j = bootstrap(player_data, 36)
      all_gold[[name]] = mu_j
    }
    player_kernels[[i]] = calc_player_kernels(name, all_gold)
  }
  names(player_kernels) = names(all_gold)
  return_me = list('all_gold' = all_gold, 'player_kernels' = player_kernels) 
  return(return_me)
}

p = 'WADID'
hist(all_gold[[p]], main = 'Non-Bootstrapped')
kde_results = create_kde(all_gold)
all_gold = kde_results$all_gold
player_kernels = kde_results$player_kernels
hist(all_gold[[p]], main = 'Bootstrapped')

hist(all_gold[[p]], main = 'Earned.GPM')
x = seq(75, 100, length.out = length(player_kernels[[p]]))
plot(x,player_kernels[[p]], type = 'l', main = 'KDE')

########### Simulating Round Robin Split ##############
# function to simulate a match by drawing from the KDE's 
# returns the total earn GPM of both teams
simulate_match = function(team1, team2){
  team1_gold = 0
  team2_gold = 0
  for(player in c(1:5)){
    matchup1 = rosters[[team1]][player]
    matchup2 = rosters[[team2]][player]
    s1 = seq(min(all_gold[[matchup1]]),max( all_gold[[matchup1]]),1)
    s2 = seq(min(all_gold[[matchup2]]),max( all_gold[[matchup2]]),1)
    team1_gold = team1_gold + sample(s1, 1, replace = TRUE, prob=player_kernels[[matchup1]])
    team2_gold = team2_gold + sample(s2, 1, replace = TRUE, prob=player_kernels[[matchup2]])
  }
  return(c('team1' = team1_gold, 'team2' = team2_gold))
}

#simulate 1 round robin split
#returns the number of team wins
simulate_split = function(teams){
  team_wins = list()
  for(name in teams){
    team_wins[name] = 0
  }
  
  for(i in c(1:length(teams))){
    for(j in c(1:length(teams))){
      if(i != j){
        team1 = teams[i]
        team2 = teams[j]
        match_results = simulate_match(team1, team2)
        team1_gold = match_results[['team1']]
        team2_gold = match_results[['team2']]
        if(team1_gold > team2_gold){ 
          team_wins[[team1]] = team_wins[[team1]] + 1
        } 
        else{ 
          team_wins[[team2]] = team_wins[[team2]] + 1
        }
      }
    }
  }
  return(team_wins)
}
#store round robin results
results = data.frame(matrix(ncol = 10, nrow = 0))
results_wins = data.frame(matrix(ncol = 10, nrow = 0))
colnames(results) = teams
colnames(results_wins) = teams

#simulate the round robin split 1000 times
for(i in c(1:1000)){
  team_wins = simulate_split(teams)
  bind_wins = sapply(team_wins, cbind)
  team_wins_row = sort(bind_wins, decreasing = TRUE)
  placement = names(team_wins_row)
  
  simulation_results_wins = data.frame(t(bind_wins))
  results_wins[nrow(results_wins) + 1,] = simulation_results_wins
  
  simulation_results = data.frame(t(c(1:10)))
  colnames(simulation_results) = placement
  
  simulation_results = simulation_results[ , order(names(simulation_results))]
  results[nrow(results) + 1,] = simulation_results
}
#calculate and print finalized simulation win percentages for each team 
simulated_win_perc = sort(colSums(results_wins)/18000, decreasing = TRUE)
print(simulated_win_perc)

########### Simulating Playoffs ##############
#Use actual seeding instead of seeding generated from simulations
seeding = c('Team Liquid', 'Cloud9', 'FlyQuest', 'Team SoloMid', 
            'Golden Guardians', 'Evil Geniuses', '100 Thieves', 'Dignitas')

#update dataset with the actual summer split data
lcs_new_split = lcs_2020[lcs_2020$split == 'Summer' & lcs_2020$playoffs == 0, ]
lcs_training = rbind(lcs_training_2020,lcs_new_split)

# create updated earned gold per minute data set and recalculate KDEs
all_gold = create_gold_arr(lcs_training)
kde_results = create_kde(all_gold)
all_gold = kde_results$all_gold
player_kernels = kde_results$player_kernels

#function to simulate a best of 5 playoff match
#returns winner and loser along with probability of winning a playoff match
simulate_playoff_match = function(match_pair){
  team1_simulation_win = 0
  team2_simulation_win = 0
  for(i in c(1:1000)){
    team1_win = 0
    team2_win = 0
    team1 = match_pair[1]
    team2 = match_pair[2]
    while(team1_win < 3 & team2_win < 3){
      match_results = simulate_match(team1, team2)
      team1_gold = match_results[['team1']]
      team2_gold = match_results[['team2']]
      if(team1_gold > team2_gold){ 
        team1_win = team1_win + 1
      } 
      else{ 
        team2_win = team2_win + 1
      }
    }
    if(team1_win > team2_win){
      team1_simulation_win = team1_simulation_win + 1 
    }
    else{
      team2_simulation_win = team2_simulation_win + 1
    }
  }
  cat(team1, ":", team1_simulation_win/1000, "\n") 
  cat(team2, ":", team2_simulation_win/1000, "\n")
  if(team1_simulation_win > team2_simulation_win){
    return(c('winner' = team1, 'loser' = team2))
  }
  else{
    return(c('winner' = team2, 'loser' = team1))
  }
}

#create empty array to hold playoff rankings as rounds are simulated
bracket_placement = rep(NA,8)

#Following simulations are created based on the format of the playoffs,
#which runs a double-elimination bracket.
############### Simulate Round 1 ###############
winners_bracket_round1_match1 = c(seeding[3], seeding[6])
print('A1 Results:')
winners_bracket_round1_match1_results = simulate_playoff_match(winners_bracket_round1_match1)
print(winners_bracket_round1_match1_results)

winners_bracket_round1_match2 = c(seeding[4], seeding[5])
print('A2 Results:')
winners_bracket_round1_match2_results = simulate_playoff_match(winners_bracket_round1_match2)
print(winners_bracket_round1_match2_results)

losers_bracket_round1_match1 = c(seeding[8], winners_bracket_round1_match2_results[['loser']])
print('W1 Results:')
losers_bracket_round1_match1_results = simulate_playoff_match(losers_bracket_round1_match1)
print(losers_bracket_round1_match1_results)

losers_bracket_round1_match2 = c(seeding[7], winners_bracket_round1_match1_results[['loser']])
print('W2 Results:')
losers_bracket_round1_match2_results = simulate_playoff_match(losers_bracket_round1_match2)
print(losers_bracket_round1_match2_results)

bracket_placement[8] = losers_bracket_round1_match1_results[['loser']]
bracket_placement[7] = losers_bracket_round1_match2_results[['loser']]

############### Simulate Round 2 ###############
winners_bracket_round2_match1 = c(seeding[1], winners_bracket_round1_match2_results[['winner']])
print('B1 Results:')
winners_bracket_round2_match1_results = simulate_playoff_match(winners_bracket_round2_match1)
print(winners_bracket_round2_match1_results)

winners_bracket_round2_match2 = c(seeding[2], winners_bracket_round1_match1_results[['winner']])
print('B2 Results:')
winners_bracket_round2_match2_results = simulate_playoff_match(winners_bracket_round2_match2)
print(winners_bracket_round2_match2_results)

losers_bracket_round2_match1 = c(losers_bracket_round1_match1_results[['winner']], winners_bracket_round2_match1_results[['loser']])
print('X1 Results:')
losers_bracket_round2_match1_results = simulate_playoff_match(losers_bracket_round2_match1)
print(losers_bracket_round2_match1_results)

losers_bracket_round2_match2 = c(losers_bracket_round1_match2_results[['winner']], winners_bracket_round2_match2_results[['loser']])
print('X2 Results:')
losers_bracket_round2_match2_results = simulate_playoff_match(losers_bracket_round2_match2)
print(losers_bracket_round2_match2_results)

bracket_placement[6] = losers_bracket_round2_match1_results[['loser']]
bracket_placement[5] = losers_bracket_round2_match2_results[['loser']]

############### Simulate Round 3 ###############
winners_bracket_round3 = c(winners_bracket_round2_match1_results[['winner']], winners_bracket_round2_match2_results[['winner']])
print('C1 Results:')
winners_bracket_round3_results = simulate_playoff_match(winners_bracket_round3)
print(winners_bracket_round3_results)

losers_bracket_round3 = c(losers_bracket_round2_match1_results[['winner']], losers_bracket_round2_match2_results[['winner']])
print('Y1 Results:')
losers_bracket_round3_results = simulate_playoff_match(losers_bracket_round3)
print(losers_bracket_round3_results)

bracket_placement[4] = losers_bracket_round3_results[['loser']]

############### Simulate Round 4 ###############
losers_bracket_round4 = c(winners_bracket_round3_results[['loser']], losers_bracket_round3_results[['winner']])
print('Z1 Results:')
losers_bracket_round4_results = simulate_playoff_match(losers_bracket_round4)
print(losers_bracket_round4_results)

bracket_placement[3] = losers_bracket_round4_results[['loser']]

############### Simulate Finals ###############
finals = c(winners_bracket_round3_results[['winner']], losers_bracket_round4_results[['winner']])
print('E1 Results:')
finals_results = simulate_playoff_match(finals)
print(finals_results)

bracket_placement[2] = finals_results[['loser']]
bracket_placement[1] = finals_results[['winner']]

#print final brackets
print(bracket_placement)
