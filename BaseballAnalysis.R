setwd("/Users/anishabiswaray/Desktop/LEVEL/xCase-Stattleship")
getwd()
install.packages("devtools")
install.packages("chron")
library(devtools)
library(chron)
devtools::install_github("stattleship/stattleship-r")
library(stattleshipR)
set_token("c347e0089ef7421193ba37f1a77452a2")
league <- "mlb"
sport <- "baseball"
ep <- "players"
q_body <- list()
pls2016 <- ss_get_result(sport = sport, league = league, ep = "players", query = list(season_id = "mlb-2016"), version = 1, walk = TRUE)
gls2016 <- ss_get_result(sport = sport, league = league, ep = "games", query = list(season_id = "mlb=2016"), version = 1, walk = TRUE)
library(dplyr)
library(tidyr)
############ verify Data ###################
class(pls2016)
class(gls2016)
str(pls2016)
class(pls2016[[1]])
str(pls2016[[1]])
names(pls2016[[1]])
head(pls2016[[1]]$leagues)
str(pls2016[[1]]$leagues)
head(pls2016[[1]]$teams)
str(pls2016[[1]]$teams)
dim(pls2016[[1]]$teams)
####################### Move data from lists to data frames ################

players_pls <-  data.frame()
playing_positions_pls <- data.frame()
teams_pls <- data.frame()
leagues_pls <- data.frame()
for( i in 1:length(pls2016)) {
  players_pls <- rbind(players_pls, pls2016[[i]]$players)
  playing_positions_pls <- rbind(playing_positions_pls, pls2016[[i]]$playing_positions)
  teams_pls <- rbind(teams_pls, pls2016[[i]]$teams)
  leagues_pls <- rbind(leagues_pls, pls2016[[i]]$leagues)
}
##################### Move GLS2016 data from list to Data frames #################
games_gls <-  data.frame()
home_teams_gls <- data.frame()
leagues_gls <- data.frame()
away_teams_gls <- data.frame()
winning_teams_gls <-  data.frame()
seasons_gls <- data.frame()
venues_gls <- data.frame()
officials_gls <- data.frame()
players_gls <-  data.frame()
teams_gls <- data.frame()
opponents_gls <- data.frame()
game_logs_gls <- data.frame()
for( i in 1:length(gls2016)) {
  # populating the created dfs by getting data from each page
  games_gls <- rbind(games_gls, gls2016[[i]]$games)
  home_teams_gls <- rbind(home_teams_gls, gls2016[[i]]$home_teams)
  leagues_gls <- rbind(leagues_gls, gls2016[[i]]$leagues)
  away_teams_gls <- rbind(away_teams_gls, gls2016[[i]]$away_teams)
  
  winning_teams_gls <- rbind(winning_teams_gls, gls2016[[i]]$winning_teams)
  seasons_gls <- rbind(seasons_gls, gls2016[[i]]$seasons)
  venues_gls <- rbind(venues_gls, gls2016[[i]]$venues)
  officials_gls <- rbind(officials_gls, gls2016[[i]]$officials)
  
  players_gls <- rbind(players_gls, gls2016[[i]]$players)
  teams_gls <- rbind(teams_gls, gls2016[[i]]$teams)
  opponents_gls <- rbind(opponents_gls, gls2016[[i]]$opponents)
  game_logs_gls <- rbind(game_logs_gls, gls2016[[i]]$game_logs)
}
####################### Removing duplicates and cleaning up the data frames ########################################
players_rbind <- rbind(players_pls,players_gls)

players_rbind %>% select(id, active:birth_date, draft_season:draft_team_name, handedness:height, 
                         unit_of_height,name, position_abbreviation:salary_currency, slug,
                         state:team_id) %>% distinct() -> players_rbind_select_distinct

teams_gls %>% select(id, location:league_id) %>% distinct() -> teams_gls_select_distinct

#opponents_gls %>% select(id, location:league_id) %>% distinct() -> opponents_gls_select_distinct

#home_teams_gls %>% select(id, location:league_id) %>% distinct() -> home_teams_gls_select_distinct

#away_teams_gls %>% select(id, location:league_id) %>% distinct() -> away_teams_gls_select_distinct

#winning_teams_gls %>% select(id, location:league_id) %>% distinct() -> winning_teams_gls_select_distinct

#playing_positions_pls %>% select(id, abbreviation, formation:league_id) %>% distinct() -> 
playing_positions_pls_select_distinct

teams_pls %>% select(id, location:league_id) %>% distinct() -> teams_pls_select_distinct

#venues_gls %>% select(id, capacity:longitude) %>% distinct() -> venues_gls_select_distinct

games_gls %>% select(id, at_neutral_site:clock, duration, home_team_outcome:humidity, name:period, score:started_at,
                     temperature:timestamp, weather_conditions:official_ids) %>% distinct() -> games_gls_select_distinct

game_logs_gls %>% select(-created_at, -updated_at) %>% distinct() -> game_logs_gls_select_distinct


###################################### Modifying the data #################################################

teams_pls_select_distinct %>% unite_("team_name", c("location", "nickname")) -> teams_pls_select_distinct

#creating lookuptables to transfer player names, team names and game names to game_logs

lookup_team <- select(teams_pls_select_distinct, id:team_name)
names(lookup_team) <- c("team_id", "team_name")
game_logs_gls_select_distinct <- inner_join(game_logs_gls_select_distinct, lookup_team, by = "team_id")
colnames(game_logs_gls_select_distinct)

lookup_gamename <- select(games_gls_select_distinct, id, name)
names(lookup_gamename) <- c("game_id", "game_name")
lookup_gamename <- distinct(lookup_gamename) #removes duplicates in the game_id column
game_logs_gls_select_distinct <- inner_join(game_logs_gls_select_distinct, lookup_gamename, by = "game_id")
colnames(game_logs_gls_select_distinct)


lookup_player <- select(players_rbind_select_distinct, id, name,position_name)
lookup_player <- distinct(lookup_player)
names(lookup_player) <- c("player_id", "player_name", "position_name")
colnames(lookup_player)
game_logs_gls_select_distinct <- inner_join(game_logs_gls_select_distinct, lookup_player, by = "player_id")
colnames(game_logs_gls_select_distinct)

lookup_salary <- select(players_rbind_select_distinct, id, salary)
lookup_salary <- distinct(lookup_salary)
names(lookup_salary) <- c("player_id", "salary")
colnames(lookup_salary)
game_logs_gls_select_distinct <- inner_join(game_logs_gls_select_distinct, lookup_salary, by = "player_id")
colnames(game_logs_gls_select_distinct)

game_logs_gls_select_distinct <- mutate(game_logs_gls_select_distinct, 
                                        FieldP = (outfield_assists + fly_ball_outs)/(outfield_assists + fly_ball_outs + fielding_errors), 
                                        k_9ip = (strikeouts * 9)/innings_pitched)



###################################### Creating the working data frame #################################################

game_logs_gls_select_distinct %>% select(game_id, game_name, game_played, wins, player_id,
                                         player_name, position_name, team_id, team_name, team_outcome, fielding_errors:outfield_assists, 
                                         innings_pitched, hits, home_runs, hit_by_pitch, walks, at_bats, sacrifice_flys, total_bases, 
                                         on_base_percentage, slugging_percentage:strikeouts, salary,
                                         fly_ball_outs, whip, FieldP:k_9ip) -> Working_gamelog_metrics

################ Calculate team wins and losses ######################
game_logs_gls_select_distinct %>% select(team_name, game_name, team_outcome) %>% distinct() %>% 
  group_by(team_name, team_outcome) %>% tally() -> Teams_outcome

Teams_outcome %>% spread(team_outcome, n) %>% replace_na(list(tie = 0)) %>% 
  mutate(total_games = loss + tie + win) -> Teams_outcome

Teams_outcome$undecided <- NULL

stats <-
  Working_gamelog_metrics %>%
  group_by(team_name) %>%
  summarise(OverallBA = sum(hits,na.rm = TRUE)/sum(at_bats,na.rm=TRUE), 
            OverallOBP = (sum(hits, na.rm = TRUE) + sum(walks,na.rm = TRUE) + sum(hit_by_pitch, na.rm = TRUE))/(sum(at_bats, na.rm = TRUE)
                                                                                                                + sum(walks, na.rm = TRUE) + sum(hit_by_pitch, na.rm = TRUE) + sum(sacrifice_flys, na.rm = TRUE)),
            OverallSLG = sum(total_bases, na.rm = TRUE)/sum(at_bats, na.rm = TRUE), OverallSB = sum(stolen_bases, na.rm = TRUE), 
            OverallFieldP = (sum(outfield_assists, na.rm = TRUE) + sum(fly_ball_outs, na.rm = TRUE))/(sum(outfield_assists, na.rm = TRUE) + 
                                                                                                        sum(fly_ball_outs, na.rm = TRUE) + sum(fielding_errors, na.rm = TRUE)), 
            Overallk_9ip = sum(strikeouts * 9, na.rm = TRUE)/sum(innings_pitched, na.rm = TRUE), 
            OverallWhip = (sum(hits, na.rm = TRUE) + sum(walks, na.rm = TRUE))/ sum(innings_pitched, na.rm = TRUE),
            OverallFIP = (((13 * sum(home_runs, na.rm = TRUE) + 3 * sum(walks, na.rm = TRUE)) - (2 * sum(strikeouts, na.rm = TRUE))) / (sum(innings_pitched, na.rm = TRUE)) + 3.20),
            team_budget = sum(as.numeric(salary), na.rm = TRUE))

Teams_outcome <- mutate(Teams_outcome, wins_prob = win/total_games)

stats <- inner_join(stats, Teams_outcome, by = "team_name")

#Delete unwanted rows

stats$tie <- NULL
stats$win <- NULL
stats$loss <- NULL
stats$total_games <- NULL

###################################### Creating descriptive statistics #################################################

### Histograms ####
par(mfrow = c(1,1))
hist(stats$wins_prob, col = "lightgreen", xlab = "Winning Percentage",main="Distribution of winning percentage", freq = F)
lines(density(stats$wins_prob))

hist(stats$OverallFieldP, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$OverallFieldP))

hist(stats$OverallOBP, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$OverallOBP))

hist(stats$OverallSLG, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$OverallSLG))

hist(stats$OverallSB, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$OverallSB))

hist(stats$Overallk_9ip, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$Overallk_9ip))

hist(stats$OverallWhip, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$OverallWhip))

hist(stats$OverallFIP, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$OverallFIP))

hist(stats$OverallBA, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$OverallBA))

hist(stats$Team_Payroll_Amt, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(stats$Team_Payroll_Amt))

### Plotting Scatter Plots ####

stats$T_N <- c("Ar_D","At_Br","Bal_O","Bos_Red","Chi_C","Chi_Whi", "Cin_Reds","Cle_Ind","Col_Roc","Det_Tig","Hou_Ast","Kan_Roy","Los_Ang","Los_Dod","Mia_Mar","Mil_Bre","Min_Twi","New_Met","New_Yan","Oak_Ath","Phi_Phi","Pit_Pir","SD_Pad","SFO_G","Sea_Mar","St_Car","Tam_Ray","Tex_Ran","Tor_Jays","Was_Nat")

ggplot(stats, aes(OverallBA,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(stats, aes(OverallOBP,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(stats, aes(OverallSLG,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(stats, aes(OverallSB,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(stats, aes(OverallFieldP,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(stats, aes(Overallk_9ip,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(stats, aes(OverallWhip,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(stats, aes(Team_Payroll_Amt,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(stats, aes(OverallFIP,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

####Model 1 

summary(stats)
stats_model_1 <- select(stats, team_name, wins_prob, OverallOBP:OverallWhip, team_budget)
summary(stats_model_1)

RM1 <- lm(formula = wins_prob ~  OverallOBP + OverallSLG + OverallSB + Overallk_9ip +OverallFieldP + OverallWhip + Team_Payroll_Amt ,
          data = stats_model_1)
RM1
# to know summary statistics for the model
summary(RM1)

####Model 2 after including BA and FIP

stats_model_2 <- select(stats, wins_prob, OverallBA:team_budget)

summary(stats_model_2)

RM2 <- lm(formula = wins_prob ~ OverallBA + OverallOBP + OverallSLG +OverallSB + OverallFieldP + Overallk_9ip + OverallWhip OverallFIP+ Team_Payroll_Amt, 
          data = stats_model_2)
RM2
# to know summary statistics for the model
summary(RM2)


######################## Plotting correlation plots ######################
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) 
chart.Correlation(stats_model_1[,2:9])
chart.Correlation(stats_model_2[,2:10])
detach(PerformanceAnalytics)

##############################################################################################
##############################################################################################
##################### Attempt to create a model based on 2015 and 2016 data###################
##############################################################################################

############### Extracting data##########################

pls2015 <- ss_get_result(sport = sport, league = league, ep = "players", query = list(season_id = "mlb-2015"), version = 1, walk = TRUE)
gls2015 <- ss_get_result(sport = sport, league = league, ep = "games", query = list(season_id = "mlb-2015"), version = 1, walk = TRUE)

pls2016 <- ss_get_result(sport = sport, league = league, ep = "players", query = list(season_id = "mlb-2016"), version = 1, walk = TRUE)
gls2016 <- ss_get_result(sport = sport, league = league, ep = "games", query = list(season_id = "mlb-2016"), version = 1, walk = TRUE)

######### Extracting separate data frames and merging 2015-16 data frames ##################

test_players_pls <-  data.frame()
test_playing_positions_pls <- data.frame()
test_teams_pls <- data.frame()
test_leagues_pls <- data.frame()

for( i in 1:length(pls2015)) {
  test_players_pls <- rbind(test_players_pls, pls2015[[i]]$players)
  #test_playing_positions_pls <- rbind(test_playing_positions_pls, pls2015[[i]]$playing_positions)
  test_teams_pls <- rbind(test_teams_pls, pls2015[[i]]$teams)
  #test_leagues_pls <- rbind(test_leagues_pls, pls2015[[i]]$leagues)
}

############## Dissimilar players data frame for 2015 and 2016 #########

# deletes unwanted $captain column

test_players_pls$captain <- NULL

for( i in 1:length(pls2016)) {
  test_players_pls <- rbind(test_players_pls, pls2016[[i]]$players)
  #test_playing_positions_pls <- rbind(test_playing_positions_pls, pls2016[[i]]$playing_positions)
  test_teams_pls <- rbind(test_teams_pls, pls2016[[i]]$teams)
  #test_leagues_pls <- rbind(test_leagues_pls, pls2016[[i]]$leagues)
}

test_games_gls <-  data.frame()
test_home_teams_gls <- data.frame()
test_leagues_gls <- data.frame()
test_away_teams_gls <- data.frame()
test_winning_teams_gls <-  data.frame()
test_seasons_gls <- data.frame()
test_venues_gls <- data.frame()
test_officials_gls <- data.frame()
test_players_gls <-  data.frame()
test_teams_gls <- data.frame()
test_opponents_gls <- data.frame()
test_game_logs_gls <- data.frame()
for( i in 1:length(gls2015)) {
  # populating the created dfs by getting data from each page
  test_games_gls <- rbind(test_games_gls, gls2015[[i]]$games)
  #test_home_teams_gls <- rbind(test_home_teams_gls, gls2016[[i]]$home_teams)
  #test_leagues_gls <- rbind(test_leagues_gls, gls2016[[i]]$leagues)
  #test_away_teams_gls <- rbind(test_away_teams_gls, gls2016[[i]]$away_teams)
  
  #test_winning_teams_gls <- rbind(test_winning_teams_gls, gls2016[[i]]$winning_teams)
  #test_seasons_gls <- rbind(test_seasons_gls, gls2016[[i]]$seasons)
  #test_venues_gls <- rbind(test_venues_gls, gls2016[[i]]$venues)
  #test_officials_gls <- rbind(test_officials_gls, gls2016[[i]]$officials)
  
  test_players_gls <- rbind(test_players_gls, gls2015[[i]]$players)
  test_teams_gls <- rbind(test_teams_gls, gls2015[[i]]$teams)
  
}

#test_players_gls$captain <- NULL

for( i in 1:length(gls2016)) {
  # populating the created dfs by getting data from each page
  test_games_gls <- rbind(test_games_gls, gls2016[[i]]$games)
  #test_home_teams_gls <- rbind(test_home_teams_gls, gls2016[[i]]$home_teams)
  #test_leagues_gls <- rbind(test_leagues_gls, gls2016[[i]]$leagues)
  #test_away_teams_gls <- rbind(test_away_teams_gls, gls2016[[i]]$away_teams)
  
  #test_winning_teams_gls <- rbind(test_winning_teams_gls, gls2016[[i]]$winning_teams)
  #test_seasons_gls <- rbind(test_seasons_gls, gls2016[[i]]$seasons)
  #test_venues_gls <- rbind(test_venues_gls, gls2016[[i]]$venues)
  #test_officials_gls <- rbind(test_officials_gls, gls2016[[i]]$officials)
  
  test_players_gls <- rbind(test_players_gls, gls2016[[i]]$players)
  test_teams_gls <- rbind(test_teams_gls, gls2016[[i]]$teams)
  #test_opponents_gls <- rbind(test_opponents_gls, gls2016[[i]]$opponents)
  #test_game_logs_gls <- rbind(test_game_logs_gls, gls2016[[i]]$game_logs)
}

####################### Removing duplicates and cleaning up the data frames ########################################
test_players_rbind <- rbind(test_players_pls,test_players_gls)

test_players_rbind %>% select(id, active:birth_date, draft_season:draft_team_name, handedness:height, 
                              unit_of_height,name, position_abbreviation:salary_currency, slug,
                              state:team_id) %>% distinct() -> test_players_rbind_select_distinct

test_teams_gls %>% select(id, location:league_id) %>% distinct() -> test_teams_gls_select_distinct

#opponents_gls %>% select(id, location:league_id) %>% distinct() -> opponents_gls_select_distinct

#home_teams_gls %>% select(id, location:league_id) %>% distinct() -> home_teams_gls_select_distinct

#away_teams_gls %>% select(id, location:league_id) %>% distinct() -> away_teams_gls_select_distinct

#winning_teams_gls %>% select(id, location:league_id) %>% distinct() -> winning_teams_gls_select_distinct

#playing_positions_pls %>% select(id, abbreviation, formation:league_id) %>% distinct() -> 
#playing_positions_pls_select_distinct

test_teams_pls %>% select(id, location:league_id) %>% distinct() -> test_teams_pls_select_distinct

#venues_gls %>% select(id, capacity:longitude) %>% distinct() -> venues_gls_select_distinct

test_games_gls %>% select(id, at_neutral_site:clock, duration, home_team_outcome:humidity, name:period, score:started_at,
                          temperature:timestamp, weather_conditions:official_ids) %>% distinct() -> test_games_gls_select_distinct

test_game_logs_gls %>% select(-created_at, -updated_at) %>% distinct() -> test_game_logs_gls_select_distinct


###################################### Modifying the data #################################################

test_teams_pls_select_distinct %>% unite_("team_name", c("location", "nickname")) -> test_teams_pls_select_distinct

#creating lookuptables to transfer player names, team names and game names to game_logs

test_lookup_team <- select(test_teams_pls_select_distinct, id:team_name)
names(test_lookup_team) <- c("team_id", "team_name")
test_game_logs_gls_select_distinct <- inner_join(test_game_logs_gls_select_distinct, test_lookup_team, by = "team_id")
colnames(test_game_logs_gls_select_distinct)

test_lookup_gamename <- select(test_games_gls_select_distinct, id, name)
names(test_lookup_gamename) <- c("game_id", "game_name")
test_lookup_gamename <- distinct(test_lookup_gamename) #removes duplicates in the game_id column
test_game_logs_gls_select_distinct <- inner_join(test_game_logs_gls_select_distinct, test_lookup_gamename, by = "game_id")
colnames(test_game_logs_gls_select_distinct)


test_lookup_player <- select(test_players_rbind_select_distinct, id, name,position_name)
test_lookup_player <- distinct(test_lookup_player)
names(test_lookup_player) <- c("player_id", "player_name", "position_name")
colnames(test_lookup_player)
test_game_logs_gls_select_distinct <- inner_join(test_game_logs_gls_select_distinct, test_lookup_player, by = "player_id")
colnames(test_game_logs_gls_select_distinct)

test_lookup_salary <- select(test_players_rbind_select_distinct, id, salary)
test_lookup_salary <- distinct(test_lookup_salary)
names(test_lookup_salary) <- c("player_id", "salary")
colnames(test_lookup_salary)
test_game_logs_gls_select_distinct <- inner_join(test_game_logs_gls_select_distinct, test_lookup_salary, by = "player_id")
colnames(test_game_logs_gls_select_distinct)

test_game_logs_gls_select_distinct <- mutate(test_game_logs_gls_select_distinct, 
                                             FieldP = (outfield_assists + fly_ball_outs)/(outfield_assists + fly_ball_outs + fielding_errors), 
                                             k_9ip = (strikeouts * 9)/innings_pitched)



###################################### Creating the working data frame #################################################

test_game_logs_gls_select_distinct %>% select(game_id, game_name, game_played, wins, player_id,
                                              player_name, position_name, team_id, team_name, team_outcome, fielding_errors:outfield_assists, 
                                              innings_pitched, hits, home_runs, hit_by_pitch, walks, at_bats, sacrifice_flys, total_bases, 
                                              on_base_percentage, slugging_percentage:strikeouts, salary,
                                              fly_ball_outs, whip, FieldP:k_9ip) -> test_Working_gamelog_metrics

################ Calculate team wins and losses ######################
test_game_logs_gls_select_distinct %>% select(team_name, game_name, team_outcome) %>% distinct() %>% 
  group_by(team_name, team_outcome) %>% tally() -> test_Teams_outcome

test_Teams_outcome %>% spread(team_outcome, n) %>% replace_na(list(tie = 0)) %>% 
  mutate(total_games = loss + tie + win) -> test_Teams_outcome

test_Teams_outcome$undecided <- NULL

test_stats <-
  test_Working_gamelog_metrics %>%
  group_by(team_name) %>%
  summarise(OverallBA = sum(hits,na.rm = TRUE)/sum(at_bats,na.rm=TRUE), 
            OverallOBP = (sum(hits, na.rm = TRUE) + sum(walks,na.rm = TRUE) + sum(hit_by_pitch, na.rm = TRUE))/(sum(at_bats, na.rm = TRUE) + sum(walks, na.rm = TRUE) + sum(hit_by_pitch, na.rm = TRUE) + sum(sacrifice_flys, na.rm = TRUE)),
            OverallSLG = sum(total_bases, na.rm = TRUE)/sum(at_bats, na.rm = TRUE), OverallSB = sum(stolen_bases, na.rm = TRUE), 
            OverallFieldP = (sum(outfield_assists, na.rm = TRUE) + sum(fly_ball_outs, na.rm = TRUE))/(sum(outfield_assists, na.rm = TRUE) + sum(fly_ball_outs, na.rm = TRUE) + sum(fielding_errors, na.rm = TRUE)), 
            Overallk_9ip = sum(strikeouts * 9, na.rm = TRUE)/sum(innings_pitched, na.rm = TRUE), 
            OverallWhip = (sum(hits, na.rm = TRUE) + sum(walks, na.rm = TRUE))/ sum(innings_pitched, na.rm = TRUE),
            OverallFIP = (((13 * sum(home_runs, na.rm = TRUE) + 3 * sum(walks, na.rm = TRUE)) - (2 * sum(strikeouts, na.rm = TRUE))) / (sum(innings_pitched, na.rm = TRUE)) + 3.20),
            team_budget = sum(as.numeric(salary), na.rm = TRUE))

test_Teams_outcome <- mutate(test_Teams_outcome, wins_prob = win/total_games)

test_stats <- inner_join(test_stats, test_Teams_outcome, by = "team_name")
test_stats$tie <- NULL
test_stats$win <- NULL
test_stats$loss <- NULL
test_stats$total_games <- NULL

###################################### Creating descriptive statistics #################################################

### Histograms ####

par(mfrow = c(1,1))
hist(test_stats$wins_prob, col = "lightgreen", xlab = "Winning Percentage",main="Distribution of winning percentage", freq = F)
lines(density(test_stats$wins_prob))

hist(test_stats$OverallFieldP, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$OverallFieldP))

hist(test_stats$OverallOBP, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$OverallOBP))

hist(test_stats$OverallSLG, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$OverallSLG))

hist(test_stats$OverallSB, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$OverallSB))

hist(test_stats$Overallk_9ip, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$Overallk_9ip))

hist(test_stats$OverallWhip, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$OverallWhip))

hist(test_stats$OverallFIP, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$OverallFIP))

hist(test_stats$OverallBA, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$OverallBA))

hist(test_stats$Team_Payroll_Amt, col = "lightgreen", xlab = "Field_P",main="Distribution of Fielding percentage", freq = F)
lines(density(test_stats$Team_Payroll_Amt))

### Plotting Scatter Plots ####

test_stats$T_N <- c("Ar_D","At_Br","Bal_O","Bos_Red","Chi_C","Chi_Whi", "Cin_Reds","Cle_Ind","Col_Roc","Det_Tig","Hou_Ast","Kan_Roy","Los_Ang","Los_Dod","Mia_Mar","Mil_Bre","Min_Twi","New_Met","New_Yan","Oak_Ath","Phi_Phi","Pit_Pir","SD_Pad","SFO_G","Sea_Mar","St_Car","Tam_Ray","Tex_Ran","Tor_Jays","Was_Nat")

ggplot(test_stats, aes(OverallBA,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(test_stats, aes(OverallOBP,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(test_stats, aes(OverallSLG,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(test_stats, aes(OverallSB,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(test_stats, aes(OverallFieldP,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(test_stats, aes(Overallk_9ip,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(test_stats, aes(OverallWhip,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(test_stats, aes(Team_Payroll_Amt,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

ggplot(test_stats, aes(OverallFIP,wins_prob,colour=team_name))+geom_text(aes(label=T_N),hjust=0, vjust=0)+geom_smooth(method = "lm", se = FALSE, colour = "red") + xtheme + geom_point(aes(colour = factor(team_name)), size = 4)

####Model 1 

summary(test_stats)
test_stats_model_1 <- select(test_stats, team_name, wins_prob, OverallOBP:OverallWhip, team_budget)
summary(test_stats_model_1)

test_RM1 <- lm(formula = wins_prob ~  OverallOBP + OverallSLG + OverallSB + Overallk_9ip +OverallFieldP + OverallWhip + Team_Payroll_Amt ,
               data = test_stats_model_1)
test_RM1  #### R squared value of RM1 > test_RM1

# to know summary statistics for the model
summary(test_RM1)

####Model 2 after including BA and FIP

test_stats_model_2 <- select(test_stats, wins_prob, OverallBA:team_budget)

summary(test_stats_model_2)

test_RM2 <- lm(formula = wins_prob ~ OverallBA + OverallOBP + OverallSLG +OverallSB + OverallFieldP + Overallk_9ip + OverallWhip OverallFIP+ Team_Payroll_Amt, 
               data = test_stats_model_2)
test_RM2  ###### R squared value of RM1 > test_RM1


# to know summary statistics for the model
summary(test_RM2)


######################## Plotting correlation plots ######################
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) 
chart.Correlation(test_stats_model_1[,2:9])
chart.Correlation(test_stats_model_2[,2:10])
detach(PerformanceAnalytics)

## Project end ##





