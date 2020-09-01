##### Data Transformation work with dplyr #####

library(tidyverse)

### Basic Transformation: Creating Additional Predictors ###
# Replacing NA values -----------------------------------------------------

## Teams Replacing NA with 0 in numeric cols
teams <- teams %>%
  replace_na(list(OTL = 0, T = 0, SoW = 0, SoL = 0))

team_vs_team<- team_vs_team %>%
  replace_na(list(OTL = 0, T = 0))

# Returns a list with col names from a df as the names and 0 as the value
# Used as a list for replace_na to replace na with zeros
list_colname_pair <- function(df,a,b) {
  vec <- colnames(df)
  tmp <- rep(0, length(df))
  list <- as.list(tmp)
  names(list) <- vec
  list[a:b]
}

na_cols_teams_splits <- list_colname_pair(teams_splits,3,length(teams_splits))
teams_splits <- teams_splits %>%
  replace_na(na_cols_teams_splits)

na_cols_goalies <- list_colname_pair(goalies, 5, length(goalies))
goalies <- goalies %>%
  replace_na(na_cols_goalies)

na_cols_players <- list_colname_pair(players, 5, length(players))
players <- players %>%
  replace_na(na_cols_players)



# Points Calculation ------------------------------------------------------

# Function calculating points (for teams_splits)
# 2 points for a Win, 1 for a Tie, 1 for an OverTime Loss
points <- function(W,L,T,OTL) {
  Pts <- 2*W + 1*T + 1*OTL
}


# Point % Calculation (team and teams_splits) -----------------------------------------------------

# Function calculating PtsPercentage = points / maximum possible points(winning all games)
point_per <- function(G,Pts) {
  PtsPer <- Pts/(2*G)
  signif(PtsPer,2)
}

# PtsPer for teams
teams <- teams %>%
  mutate(PtsPer = point_per(G,Pts))
# Rearraning order of columns to make PtsPer next to Pts
teams <- teams[,c(1:13,length(teams),15:length(teams)-1)]

# Point percentage funciton for teams_splits df (does not contain G,Pts cols)
point_per_splits <- function(W,L,T,OTL) {      
  max_pts <- (2*(W+L+T+OTL))                  
  PtsPer <- (2*W + 1*T + 1*OTL) / max_pts
  # check for NaN in cases where no games were played in that month (i.e. max_pts == 0)
  PtsPer[is.nan(PtsPer)] <- 0
  signif(PtsPer,2)
}

# Adding PtsPer for each split in the teams_split df
teams_splits <- teams_splits %>%
  mutate(hPtsPer = point_per_splits(hW,hL,hT,hOTL))  %>%
  mutate(rPtsPer = point_per_splits(rW,rL,rT,rOTL))  %>%
  mutate(SepPtsPer = point_per_splits(SepW,SepL,SepT,SepOL))  %>%
  mutate(OctPtsPer = point_per_splits(OctW,OctL,OctT,OctOL))  %>%
  mutate(NovPtsPer = point_per_splits(NovW,NovL,NovT,NovOL))  %>%
  mutate(DecPtsPer = point_per_splits(DecW,DecL,DecT,DecOL))  %>%
  mutate(JanPtsPer = point_per_splits(JanW,JanL,JanT,JanOL))  %>%
  mutate(FebPtsPer = point_per_splits(FebW,FebL,FebT,FebOL))  %>%
  mutate(MarPtsPer = point_per_splits(MarW,MarL,MarT,MarOL))  %>%
  mutate(AprPtsPer = point_per_splits(AprW,AprL,AprT,AprOL))  


teams_splits <- teams_splits %>%
  # Point Percentage over the months of March and April (last quarter or less of season)
  mutate(Last_Month_Pts_Per = 
           signif((MarPtsPer*(MarW+MarL+MarT+MarOL) + AprPtsPer*(AprW+AprL+AprT+AprOL)) / 
           (MarW+MarL+MarT+MarOL+AprW+AprL+AprT+AprOL), 2)) %>%
  # Team's Point percentage from January on (roughly 2nd half of season)
  mutate(Second_Half_Pts_Per = 
           signif( (JanPtsPer*(JanW+JanL+JanT+JanOL) + FebPtsPer*(FebW+FebL+FebT+FebOL) +
             MarPtsPer*(MarW+MarL+MarT+MarOL) + AprPtsPer*(AprW+AprL+AprT+AprOL)) /
               (MarW+MarL+MarT+MarOL+AprW+AprL+AprT+AprOL+JanW+JanL+JanT+JanOL+FebW+FebL+FebT+FebOL),
             2))
 # teamstmp[,c("year")]





# series_post work -----------------------------------------------

series_post <- series_post %>%
  # Adding winning percentage (win/total games played) 
  mutate(WinPer = signif((W / (W+L)),2)) %>%
  # Adding goal differential 
  mutate(GD_Winner = GoalsWinner - GoalsLoser) %>%
  # Goals For, Allowed, Differential per game
  mutate(GFperG_Winner = signif(GoalsWinner / (W+L+T),2)) %>%
  mutate(GFperG_Loser = signif(GoalsLoser / (W+L+T),2)) %>%
  mutate(GDperG_Winner = GFperG_Winner - GFperG_Loser) %>%
  rename( W_Winner = W, W_Loser = L, Goals_Winner = GoalsWinner, 
          Goals_Loser = GoalsLoser)

# awards_players work -----------------------------------------------------

# Adding tmID to awards_players
awards_players <- awards_players %>%
  mutate(tmID = vector(mode = "character",length = length(awards_players$playerID)))

## Fills tmID variable in awards_players
# Loops through each observation in awards_players
for (i in seq_along(awards_players$playerID)) {
  tmp_award <- awards_players[i,]
  # Grabbing the matching observation in players
  tmp_pl <- players %>%
    filter(year == tmp_award$year) %>%
    filter(playerID == tmp_award$playerID)
  if(length(tmp_pl$stint != 1)){
    # stint denotes whether a player started the season with a team, or was traded (i.e. His 2nd 'Stint')
    # In the playoffs, a player will play for the most recent team he played for 
    # (i.e. His latest (greatest in value) 'Stint')
    tmp_pl <- tmp_pl %>%
      filter(stint == max(stint))
  }
  # Checks if vector lengths match (should be 1)
  if(length(tmp_pl$stint == 1)){
    tmp_award$tmID <- tmp_pl$tmID
    awards_players[i,] <- tmp_award
  }
}



# goalies work ------------------------------------------------------------

goalies <- goalies %>%
  # Adding Save Percentage for regular and postseason
  mutate(SavePer = ifelse(SA != 0, signif(1 - GA/SA, 3), 0)) %>%
  mutate(PostSavePer = ifelse(PostSA != 0, signif(1 - PostGA/PostSA, 3), 0)) %>%
  # Adding Goals Against Average (i.e. goals allowed per 60 mins)
  mutate(GAA = ifelse(Min != 0, signif(GA/Min * 60, 3), 0)) %>%
  mutate(PostGAA = ifelse(PostMin != 0, signif(PostGA/PostMin * 60, 3), 0))

# teams work --------------------------------------------------------------

teams <- teams %>%
  # Regulation plus overtime wins (i.e. non-shootout victories)
  mutate(ROW = W - SoW) %>%
  mutate(GD = GF - GA) %>%
  mutate(GFperG = signif(GF/G, 3)) %>%
  mutate(GAperG = signif(GA/G, 3)) %>%
  mutate(GDperG = signif(GD/G, 2)) %>%
  # Total Goals per game (for + against)
  mutate(TGperG = signif((GF+GA)/G, 3)) %>%
  # Power Play and Penalty Kill percentage (Goals divided by chances)
  mutate(PPPer = signif(PPG/PPC * 100, 4)) %>%
  mutate(PKPer = signif(100 - PKG/PKC * 100, 4)) %>%
  # Even Strength Goals For, Against, Differential (i.e. 5-on-5 regular play)
  mutate(EVGF = GF - PPG - SHF) %>%
  mutate(EVGA = GA - PKG - SHA) %>%
  mutate(EVGD = EVGF - EVGA) %>%
  # Penalties in Minutes per Game
  mutate(PIMperG = signif(PIM/G, 3)) %>%
  mutate(EVGFperG = signif(EVGF/G, 3)) %>%
  mutate(EVGAperG = signif(EVGA/G, 3)) %>%
  mutate(EVGDperG = signif(EVGD/G, 2)) %>%
  # Luck: number of more games won by shootout than lost
  # This could add Points to a team that didn't neccesarily 'earn' them, since shootouts do not 
  # occur in the playoffs
  mutate(luck = ifelse(SoW + SoL > 0, SoW - SoL, 0))


# teams_post work ---------------------------------------------------------

teams_post <- teams_post %>%
  mutate(GD = GF - GA) %>%
  mutate(GFperG = signif(GF/G, 3)) %>%
  mutate(GAperG = signif(GA/G, 3)) %>%
  mutate(GDperG = signif(GD/G, 2)) %>%
  # Total Goals per game (for + against)
  mutate(TGperG = signif((GF+GA)/G, 3)) %>%
  # Power Play and Penalty Kill percentage (Goals divided by chances)
  mutate(PPPer = signif(PPG/PPC * 100, 4)) %>%
  mutate(PKPer = signif(100 - PKG/PKC * 100, 4)) %>%
  # Even Strength Goals For, Against, Differential (i.e. 5-on-5 regular play)
  mutate(EVGF = GF - PPG - SHF) %>%
  mutate(EVGA = GA - PKG - SHA) %>%
  mutate(EVGD = EVGF - EVGA) %>%
  # Penalties in Minutes per Game
  mutate(PIMperG = signif(PIM/G, 3)) %>%
  mutate(EVGFperG = signif(EVGF/G, 3)) %>%
  mutate(EVGAperG = signif(EVGA/G, 3)) %>%
  mutate(EVGDperG = signif(EVGD/G, 2)) %>%
  # Adding winning percentage (win/total games played)
  mutate(WinPer = signif((W / (W+L)),2)) 


### More Work ###


# teams_adjusted work -----------------------------------------------------

# creating teams_adjusted
# tasked with adding data from awards, goalies, teams_splits
teams_adjusted <- teams

## Awards

# Adding empty vectors 
teams_adjusted <- teams_adjusted %>%
  # Does this team have the MVP on their team? (1: yes, 0: no)
  mutate(MVP = vector(mode = "double",length = length(teams_adjusted$year))) %>%
  # Number of First Team All-Stars playing on the teams
  mutate(FirstTeamAllStars = vector(mode = "double",length = length(teams_adjusted$year))) %>%
  # Number of Second Team All-Stars playing on the teams
  mutate(SecondTeamAllStars = vector(mode = "double",length = length(teams_adjusted$year))) 

# Looping through teams to fill in above columns from awards_players
for (i in seq_along(teams_adjusted$year)) {
  tmp_team <- teams_adjusted[i,]
  # finding if the MVP was on the team
  tmp_MVP <- awards_players %>%
    filter(year == tmp_team$year) %>%
    filter(award == "Art Ross") 
  if(tmp_MVP$tmID == tmp_team$tmID){
    teams_adjusted$MVP[i] = 1
  } else{
    teams_adjusted$MVP[i] = 0
  }
  
  # counting number of 1st team All-stars on the team
  tmp_First_count <- awards_players %>%
    filter(year == tmp_team$year) %>%
    filter(award == "First Team All-Star") %>%
    filter(tmID == tmp_team$tmID) %>%
    summarise(n())
  teams_adjusted$FirstTeamAllStars[i] = as.integer(tmp_First_count)
  
  # counting number of 2nd team All-stars on the team
  tmp_Second_count <- awards_players %>%
    filter(year == tmp_team$year) %>%
    filter(award == "Second Team All-Star") %>%
    filter(tmID == tmp_team$tmID) %>%
    summarise(n())
  teams_adjusted$SecondTeamAllStars[i] = as.integer(tmp_Second_count)
}

# Adding count of Total number of All Stars
teams_adjusted <- teams_adjusted %>%
  mutate(AllStars = FirstTeamAllStars +SecondTeamAllStars)

## goalies
teams_adjusted <- teams_adjusted %>%
  mutate(SavePer = vector(mode = "double",length = length(teams_adjusted$year)))

# calculating each teams Save Percentage based on the average of their goalies' Save Percentage that year
for (i in seq_along(teams_adjusted$year)) {
  tmp_team <- teams_adjusted[i,]
  
  tmp_goalies_SavePer <- goalies %>%
    filter(year == tmp_team$year) %>%
    filter(tmID == tmp_team$tmID) %>%
    summarise(mean(SavePer))
  
  teams_adjusted$SavePer[i] = signif(as.double(tmp_goalies_SavePer), 3)
}

## teams_splits

# Adding two columns from teams_splits
teams_adjusted <- teams_adjusted %>%
  inner_join(teams_splits %>% select(year,tmID,Last_Month_Pts_Per, Second_Half_Pts_Per), 
             by = c("year", "tmID"))

# series_adjusted -------------------------------------------------------------

# Creating Series_adjusted
# Will contain all information relevant to the model building
series_adjusted <- series_post %>%
  select(1:8)

series_adjusted <- series_adjusted %>%
  # randomizer to determine which team will be tm_1
  mutate(randomizer = sample(c(1,0), size = 375, replace = TRUE)) %>%
  # Assigning ID for tm_1 and tm_2 (random so that isnt determined by winners and losers)
  mutate(ID_tm_1 = ifelse(randomizer==1, tmIDWinner, tmIDLoser)) %>%
  mutate(ID_tm_2 = ifelse(randomizer==0, tmIDWinner, tmIDLoser)) %>%
  # Moving wins and losses to be tm_1 and tm_2 instead of winners and loser (to even data out)
  mutate(W_tm_1 = ifelse(randomizer==1, W_Winner, W_Loser)) %>%
  mutate(W_tm_2 = ifelse(randomizer==0, W_Winner, W_Loser)) %>%
  # removing original columns
  select(-c(randomizer,tmIDWinner,tmIDLoser,W_Winner,W_Loser,T))

# empty vector for the outcome of the series for tm_1
series_adjusted <- series_adjusted %>%
  mutate(outcome = vector(mode = "character",length = length(series_adjusted$year)))

# applying the result of the series for tm_1
for (i in seq_along(series_adjusted$year)) {
  tmp_series <- series_adjusted[i,]
  if (tmp_series$W_tm_1 == 0 && tmp_series$W_tm_2 == 4) {
    series_adjusted$outcome[i] = "Lose in 4"
  } else if(tmp_series$W_tm_1 == 1 && tmp_series$W_tm_2 == 4){
    series_adjusted$outcome[i] = "Lose in 5"
  } else if(tmp_series$W_tm_1 == 2 && tmp_series$W_tm_2 == 4){
    series_adjusted$outcome[i] = "Lose in 6"
  } else if(tmp_series$W_tm_1 == 3 && tmp_series$W_tm_2 == 4){
    series_adjusted$outcome[i] = "Lose in 7"
  } else if(tmp_series$W_tm_1 == 4 && tmp_series$W_tm_2 == 3){
    series_adjusted$outcome[i] = "Win in 7"
  } else if(tmp_series$W_tm_1 == 4 && tmp_series$W_tm_2 == 2){
    series_adjusted$outcome[i] = "Win in 6"
  } else if(tmp_series$W_tm_1 == 4 && tmp_series$W_tm_2 == 1){
    series_adjusted$outcome[i] = "Win in 5"
  } else{
    series_adjusted$outcome[i] = "Win in 4"
  }
}

# Making outcome a factor for analysis
series_adjusted$outcome <- factor(series_adjusted$outcome, ordered = TRUE,
                                  levels = c("Lose in 4", "Lose in 5", "Lose in 6", "Lose in 7",
                                             "Win in 7", "Win in 6", "Win in 5", "Win in 4"))

# Joining teams_adjusted to series_adjusted for each team: winner and loser
series_adjusted <- series_adjusted %>%
  inner_join(teams_adjusted, by = c("year", "ID_tm_1" = "tmID"), 
             suffix = c("_Series", "_Reg_tm_1")) %>%
  inner_join(teams_adjusted, by = c("year", "ID_tm_2" = "tmID"), 
             suffix = c("_Reg_tm_1", "_Reg_tm_2"))

series_adjusted <- series_adjusted %>%
  # Winner_Home: Binary variable denoting which team has home field advantage during the series
  # (1: Winner was home team, 0: Loser)
  mutate(Home_tm_1 = ifelse(Pts_Reg_tm_1 > Pts_Reg_tm_2, 1, 0))



         
# data environment removal clean ------------------------------------------

rm(tmp_award,tmp_First_count,tmp_goalies_SavePer,tmp_MVP,tmp_pl,tmp_Second_count,tmp_series,tmp_team)
rm(i,na_cols_goalies,na_cols_players,na_cols_teams_splits)
