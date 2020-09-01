##### Hockey Data Science Project Full Script $$$$$
##### Importing and Basic Tidying #####

library(tidyverse)

# Regular Season Team Data (teams) ----------------------------------------

teams <- read_csv("proj_hockey/data/teams.csv", col_types = cols(
  # Specifying columns types to parse. Guess parsing failed for OTL, SoW, SoL since these columns contained
  # no data until ~year=2000 when the NHL changed the rules of the game
  .default = col_double(),
  lgID = col_character(),
  tmID = col_character(),
  franchID = col_character(),
  confID = col_character(),
  divID = col_character(),
  playoff = col_character(),
  name = col_character() 
))  %>%
  # Filtering for years after 1978. In 1979 the WHA merged with the NHL, and changed the playoff format to
  # include 16 teams, which roughly stayed the same for the rest of the dataset. This allows for comparing
  # apples to apples for playoff teams.  (May take subsets of years from this df for more analysis)
  filter(year > 1985) %>%
  # Removing lgID col, only one league exists during 1998 - 2011 (see comment above)
  select(-lgID) 


# Postseason Team Data (teams_post) ---------------------------------------

teams_post <- read.csv("proj_hockey/data/TeamsPost.csv") %>%
  filter(year > 1985) %>%
  select(-lgID)


# Postseason Series Data (series_post) ------------------------------------

series_post <- read_csv("proj_hockey/data/seriespost.csv") %>%
  filter(year > 1985) %>%
  # Removing lgID, note cols 
  select(-c(lgIDWinner,lgIDLoser,note)) 


# Team Splits Data (teams_splits) -----------------------------------------

teams_splits <- read_csv("proj_hockey/data/TeamSplits.csv", col_types = cols(
  # Specifying col types to parse. Guess parser failed similar to teams df, explained in comment there
  .default = col_double(),
  lgID = col_character(), 
  tmID = col_character()   
)) %>%
  filter(year > 1985) %>%
  select(-lgID)


# Regular Season Team vs Team Data (team_vs_team) -------------------------

team_vs_team <- read_csv("proj_hockey/data/TeamVsTeam.csv", col_types = cols(
  # Specifying col types to parse. Guess parser failed similar to teams df, explained in comment there
  .default = col_double(),     
  lgID = col_character(),
  tmID = col_character(),
  oppID = col_character()
)) %>% 
  filter(year > 1985) %>%
  select(-lgID)

# Player Awards Data (awards_players) -------------------------------------

# Parsing error occurs on data for WHA players who won playoff mvp, i dont use this
# data (old data from 1974) so it is unneccesary for my useage. Error does not effect other observations
awards_players <- read_csv("proj_hockey/data/AwardsPlayers.csv")  %>%  
  filter(year > 1985) %>%  
  select(-lgID) 

# Goalie Data (goalies) ---------------------------------------------------

goalies <- read.csv("proj_hockey/data/Goalies.csv") %>%
  filter(year > 1985) %>%
  select(-lgID)

# Player Scoring Data (players) -------------------------------------------

players <- read_csv("proj_hockey/data/Scoring.csv") %>%
  filter(year > 1985) %>%
  select(-lgID)

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

###### EDA and Modeling Process ######

set.seed(1991)

library(tidyverse)
library(modelr)
library(broom)
library(caret)

## LASSO
# install.packages("glmnet")
library(glmnet)

## Plotting correlations
#install.packages("ggcorrplot")
library(ggcorrplot)

# Partitioning ------------------------------------------------------------

# Partitioning dataset into train and test
# 20% for testing (~75 observations, 5 years worth of data), 80% training (20 years worth of data)
series <- resample_partition(series_adjusted, c(train = 0.8, test = 0.2))
series_train <- as_tibble(series$train)
series_test <- as_tibble(series$test)

# Creating a second series dataset with refernce information, redundant and unlikely variables removed
series_2 <- series_adjusted %>%
  select(-c(1:7,9:14,Pts_Reg_tm_1,SoW_Reg_tm_1,SoL_Reg_tm_1,GF_Reg_tm_1,GA_Reg_tm_1,GD_Reg_tm_1, 
            name_Reg_tm_1, PIM_Reg_tm_1,PPG_Reg_tm_1,SHA_Reg_tm_1,PKG_Reg_tm_1,SHF_Reg_tm_1,
            EVGF_Reg_tm_1,EVGA_Reg_tm_1, EVGD_Reg_tm_1,
            franchID_Reg_tm_2,confID_Reg_tm_2,divID_Reg_tm_2,rank_Reg_tm_2,playoff_Reg_tm_2,
            G_Reg_tm_2,Pts_Reg_tm_2,SoW_Reg_tm_2,SoL_Reg_tm_2,GF_Reg_tm_2,GA_Reg_tm_2,GD_Reg_tm_2,
            name_Reg_tm_2,PIM_Reg_tm_2,PPG_Reg_tm_2,SHA_Reg_tm_2,PKG_Reg_tm_2,SHF_Reg_tm_2,
            EVGF_Reg_tm_2,EVGA_Reg_tm_2,EVGD_Reg_tm_2))
# Partitioning dataset into train and test
series_2 <- resample_partition(series_2, c(train = 0.8, test = 0.2))
series_2_train <- as_tibble(series_2$train)
series_2_test <- as_tibble(series_2$test)
# Adding binary outcome variable to compare with predicitons
series_2_train_tmp <- series_2_train %>%
  mutate(outcome_binary = as.factor(ifelse(outcome >= "Win in 7",1 , 0)))
series_2_test_tmp <- series_2_test %>%
  mutate(outcome_binary = as.factor(ifelse(outcome >= "Win in 7",1 , 0)))



# EDA ---------------------------------------------------------------------

# Developing correlation dataframe with a numeric version of outcome_binary
cor_tmp_df <- series_2_train %>%
  mutate(outcome_binary = ifelse(outcome >= "Win in 7",1 , 0)) %>%
  dplyr::select(-contains("tm_2")) %>%
  dplyr::select(-outcome) 
cor_tmp_df <- cor_tmp_df %>%
  dplyr::select_if(cor(.,cor_tmp_df$outcome_binary) > 0.2 | cor(.,cor_tmp_df$outcome_binary) < -0.2)
# Plotting Correlation matrix of the strongest correlations with outcome_binary
cor_plot_outcome_binary <- ggcorrplot(cor(cor_tmp_df),hc.order = TRUE)

# Plotting All Stars versus PtsPer
boxplot_PtsPer_AllStars <- series_2_train %>%
  ggplot(aes(as.factor(AllStars_Reg_tm_1), PtsPer_Reg_tm_1, group = as.factor(AllStars_Reg_tm_1))) +
  geom_boxplot(aes(fill = as.factor(AllStars_Reg_tm_1))) +
  labs(title = "Points Percentage vs Number of All Stars", x = "All Stars", y = "Points %") + 
  theme(legend.position = "none")

# SavePer versus PtsPer
dotplot_PtsPer_SavePer <- series_2_train %>% 
  filter(SavePer_Reg_tm_1 > 0.85) %>%
  ggplot(aes(SavePer_Reg_tm_1,PtsPer_Reg_tm_1)) +
  geom_point(aes(alpha=0.1)) +
  geom_smooth() +
  theme(legend.position = "none") +
  labs(title = "Pts% vs Save%")

# Density Diff in 2nd half pts grouped by outcome binary
density_second_half <- series_2_train_tmp %>%
  ggplot(aes(Second_Half_Pts_Per_Reg_tm_1 - Second_Half_Pts_Per_Reg_tm_2, group = outcome_binary)) +
  geom_density(aes(fill = outcome_binary)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Difference in 2nd Half Pts by series outcome", 
       x = "Differnce in 2nd Half Pts")

# correlation matrix of luck, PtsPer,GDperG
cor_tmp_2 <- series_2_train %>%
  dplyr::select(c(PtsPer_Reg_tm_1,GDperG_Reg_tm_1,luck_Reg_tm_1)) 

cor_plot_luck <- ggcorrplot(cor(cor_tmp_2),hc.order = TRUE)

# Count of outcomes facet wrapped by home field advantage
# series_2_train_tmp %>%
#   ggplot(aes(x = outcome, fill = outcome_binary)) +
#   geom_bar() +
#   facet_wrap(~ Home_tm_1) 

boxplot_home_outcomes <- series_2_train_tmp %>%
  ggplot(aes(x = outcome, fill = as.factor(Home_tm_1))) +
  geom_bar(position = "dodge") +
  labs(fill = "Home Team") +
  labs(title = "Home vs Away Outcomes")


# MAE CV LASSO (k = 10) ---------------------------------------------------------------

# Running a single LASSO fit
# grouped option keeps coefficients values same for each category 
# glmnet only accepts matrices for the predictors
glmnet_fit <- glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
                     family = 'multinomial', type.multinomial = "grouped")
# Cross validaiton LASSO: using mae as the plot score to minimize 
# Warnings occur, but only to show that more optimizaiton isn't neccesary
glmnet_cv_fit_mae <- cv.glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
                               family = 'multinomial',type.multinomial = "grouped", type.measure = "mae")
glmnet_cv_lamba_vs_mae <- plot(glmnet_cv_fit_mae)


### lambda = glmnet_cv_fit$lambda.min 
pred_glmnet_cv_response_mae_2 <- predict(glmnet_cv_fit_mae,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = glmnet_cv_fit_mae$lambda.min , type = "response")

glmnet_cv_coef_mae_2 <- coef(glmnet_cv_fit_mae, s = glmnet_cv_fit_mae$lambda.min)

pred_glmnet_cv_response_mae_2 <- as_tibble(pred_glmnet_cv_response_mae_2[,1:8,1])
pred_glmnet_cv_response_mae_2 <- pred_glmnet_cv_response_mae_2 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_2$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_2$outcome_pred_binary)

glmnet_cv_train_acc_mae_2 <- confusionMatrix(pred_glmnet_cv_response_mae_2$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]


### lambda = glmnet_cv_fit_mae$lambda.1se (most regularized within 1 SE of min) 
pred_glmnet_cv_response_mae_3 <- predict(glmnet_cv_fit_mae,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = glmnet_cv_fit_mae$lambda.1se , type = "response")

glmnet_cv_coef_mae_3 <- coef(glmnet_cv_fit_mae, s = glmnet_cv_fit_mae$lambda.1se)

resp_tmp <- pred_glmnet_cv_response_mae_3[,1:8,1]
resp_tmp <- as_tibble(resp_tmp)
resp_tmp <- resp_tmp %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
resp_tmp$outcome_pred_binary <- as.factor(resp_tmp$outcome_pred_binary)

glmnet_cv_train_acc_mae_3 <- confusionMatrix(resp_tmp$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]

### Lambda = lambda_mae_last (including nearly every variable) 
lambda_mae_last = glmnet_cv_fit_mae$lambda[length(glmnet_cv_fit_mae$lambda)]

pred_glmnet_cv_response_mae_6 <- predict(glmnet_cv_fit_mae,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = lambda_mae_last , type = "response")

glmnet_cv_coef_mae_6 <- coef(glmnet_cv_fit_mae, s = lambda_mae_last)

pred_glmnet_cv_response_mae_6 <- as_tibble(pred_glmnet_cv_response_mae_6[,1:8,1])
pred_glmnet_cv_response_mae_6 <- pred_glmnet_cv_response_mae_6 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_6$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_6$outcome_pred_binary)

glmnet_cv_train_acc_mae_6 <- confusionMatrix(pred_glmnet_cv_response_mae_6$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]

### Lambda = 0.05418944 (smallest standard deviation on graph)
pred_glmnet_cv_response_mae_7 <- predict(glmnet_cv_fit_mae,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = 0.05418944 , type = "response")

glmnet_cv_coef_mae_7 <- coef(glmnet_cv_fit_mae, s = 0.05418944)

pred_glmnet_cv_response_mae_7 <- as_tibble(pred_glmnet_cv_response_mae_7[,1:8,1])
pred_glmnet_cv_response_mae_7 <- pred_glmnet_cv_response_mae_7 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_7$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_7$outcome_pred_binary)

glmnet_cv_train_acc_mae_7 <- confusionMatrix(pred_glmnet_cv_response_mae_7$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]


# MAE CV LASSO (k = 20) ---------------------------------------------------

# Setting number of folds k = 20
glmnet_cv_fit_mae_2 <- cv.glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
                                 family = 'multinomial',type.multinomial = "grouped", type.measure = "mae",
                                 nfolds = 20)
glmnet_cv_lamba_vs_mae_2 <- plot(glmnet_cv_fit_mae_2)


### Lambda = glmnet_cv_fit$lambda.min, k = 20  
pred_glmnet_cv_response_mae_4 <- predict(glmnet_cv_fit_mae_2,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = glmnet_cv_fit_mae_2$lambda.min , type = "response")

glmnet_cv_coef_mae_4 <- coef(glmnet_cv_fit_mae_2, s = glmnet_cv_fit_mae_2$lambda.min)

pred_glmnet_cv_response_mae_4 <- pred_glmnet_cv_response_mae_4[,1:8,1]
pred_glmnet_cv_response_mae_4 <- as_tibble(pred_glmnet_cv_response_mae_4)
pred_glmnet_cv_response_mae_4 <- pred_glmnet_cv_response_mae_4 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_4$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_4$outcome_pred_binary)

glmnet_cv_train_acc_mae_4 <- confusionMatrix(pred_glmnet_cv_response_mae_4$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]

### lambda = glmnet_cv_fit$lambda.1se, k = 20 
pred_glmnet_cv_response_mae_8 <- predict(glmnet_cv_fit_mae_2,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = glmnet_cv_fit_mae_2$lambda.1se , type = "response")

glmnet_cv_coef_mae_8 <- coef(glmnet_cv_fit_mae_2, s = glmnet_cv_fit_mae_2$lambda.1se)

pred_glmnet_cv_response_mae_8 <- pred_glmnet_cv_response_mae_8[,1:8,1]
pred_glmnet_cv_response_mae_8 <- as_tibble(pred_glmnet_cv_response_mae_8)
pred_glmnet_cv_response_mae_8 <- pred_glmnet_cv_response_mae_8 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_8$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_8$outcome_pred_binary)

glmnet_cv_train_acc_mae_8 <- confusionMatrix(pred_glmnet_cv_response_mae_8$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]



# MAE CV LASSO (k = 60) -----------------------------------------------------

### commented out to avoid unnecessary computation slowing.

# Setting number of folds k = 60
# glmnet_cv_fit_mae_3 <- cv.glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
#                                  family = 'multinomial',type.multinomial = "grouped", type.measure = "mae",
#                                  nfolds = 60)
# glmnet_cv_lamba_vs_mae_3 <- plot(glmnet_cv_fit_mae_3)
# 
# ### Lambda = glmnet_cv_fit$lambda.min, k = 60  (65.2%)
# pred_glmnet_cv_response_mae_5 <- predict(glmnet_cv_fit_mae_3,as.matrix(series_2_train[,2:ncol(series_2_train)]),
#                                          s = glmnet_cv_fit_mae_3$lambda.min , type = "response")
# 
# glmnet_cv_coef_mae_5 <- coef(glmnet_cv_fit_mae_3, s = glmnet_cv_fit_mae_3$lambda.min)
# 
# resp_tmp <- pred_glmnet_cv_response_mae_5[,1:8,1]
# resp_tmp <- as_tibble(resp_tmp)
# resp_tmp <- resp_tmp %>%
#   mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
#   mutate(lose_per = 1 - win_per) %>%
#   mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
# resp_tmp$outcome_pred_binary <- as.factor(resp_tmp$outcome_pred_binary)
# 
# glmnet_cv_train_acc_mae_5 <- confusionMatrix(resp_tmp$outcome_pred_binary,
#                                              series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]
# 

# MSE CV LASSO (k = 10) --------------------------------------------------------------

#### Commentted out for computation speed

# # Cross validaiton LASSO: using mse as the plot score to minimize 
# glmnet_cv_fit_mse <- cv.glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
#                                family = 'multinomial',type.multinomial = "grouped", type.measure = "mse")
# glmnet_cv_lamba_vs_mse <- plot(glmnet_cv_fit_mse)
# 
# 
# ### lambda = glmnet_cv_fit_mse$lambda.min (lambda value with minimum mse) 
# pred_glmnet_cv_response_mse_1 <- predict(glmnet_cv_fit_mse,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
#                                    s = glmnet_cv_fit_mse$lambda.min , type = "response")
# 
# glmnet_cv_coef_mse_1 <- coef(glmnet_cv_fit_mse, s = glmnet_cv_fit_mse$lambda.min)
# 
# resp_tmp_mse <- pred_glmnet_cv_response_mse_1[,1:8,1]
# resp_tmp_mse <- as_tibble(resp_tmp_mse)
# resp_tmp_mse <- resp_tmp_mse %>%
#   mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
#   mutate(lose_per = 1 - win_per) %>%
#   mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
# resp_tmp_mse$outcome_pred_binary <- as.factor(resp_tmp_mse$outcome_pred_binary)
# 
# glmnet_cv_train_acc_mse_1 <- confusionMatrix(resp_tmp_mse$outcome_pred_binary,
#                                              series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]
# 
# 
# ### lambda = lambda_mse_last (including most variables) 
# lambda_mse_last <- glmnet_cv_fit_mse$lambda[length(glmnet_cv_fit_mse$lambda)]
# 
# pred_glmnet_cv_response_mse_2 <- predict(glmnet_cv_fit_mse,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
#                                          s = lambda_mse_last , type = "response")
# 
# glmnet_cv_coef_mse_2 <- coef(glmnet_cv_fit_mse, s = lambda_mse_last)
# 
# resp_tmp_mse <- pred_glmnet_cv_response_mse_2[,1:8,1]
# resp_tmp_mse <- as_tibble(resp_tmp_mse)
# resp_tmp_mse <- resp_tmp_mse %>%
#   mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
#   mutate(lose_per = 1 - win_per) %>%
#   mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
# resp_tmp_mse$outcome_pred_binary <- as.factor(resp_tmp_mse$outcome_pred_binary)
# 
# glmnet_cv_train_acc_mse_2 <- confusionMatrix(resp_tmp_mse$outcome_pred_binary,
#                                              series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]
# 


# trivial prediciton ------------------------------------------------------

# Computing trival predictions (picking team with more points during regular season to win)
pred_trivial_series_2_train <- as.factor(ifelse(
  series_2_train$PtsPer_Reg_tm_1 > series_2_train$PtsPer_Reg_tm_2, 1, 0))

pred_trivial_acc <- confusionMatrix(pred_trivial_series_2_train,series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]
# Testing model and visualizations -----------------------------------------------------------------

# Accuracy Dataframe
acc_df <- tibble(
  model = c("Lambda Min", "Lambda 1SE", "Trivial"), 
  train_acc = c(glmnet_cv_train_acc_mae_4,glmnet_cv_train_acc_mae_8,pred_trivial_acc),
  test_acc = c(0,0,0)
)

# Creating a Training Accuracy Bar Chart
bar_chart_train_acc <- acc_df %>%
  ggplot(aes(reorder(model,train_acc),train_acc)) +
  geom_col(aes(fill = model)) +
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1)) +
  labs(title = "Training Accuracy", y = "Training Accuracy", x = "Model") +
  theme(legend.position = "none")

### lambda = glmnet_cv_fit$lambda.min, k = 20  
pred_glmnet_cv_response_mae_4_test <- predict(glmnet_cv_fit_mae_2,as.matrix(series_2_test[,2:ncol(series_2_test)]), 
                                              s = glmnet_cv_fit_mae_2$lambda.min , type = "response")
pred_glmnet_cv_response_mae_4_test <- pred_glmnet_cv_response_mae_4_test[,1:8,1]
pred_glmnet_cv_response_mae_4_test <- as_tibble(pred_glmnet_cv_response_mae_4_test)
pred_glmnet_cv_response_mae_4_test <- pred_glmnet_cv_response_mae_4_test %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_4_test$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_4_test$outcome_pred_binary)

glmnet_cv_test_acc_mae_4 <- confusionMatrix(pred_glmnet_cv_response_mae_4_test$outcome_pred_binary,
                                            series_2_test_tmp$outcome_binary)$overall[["Accuracy"]]

### lambda = glmnet_cv_fit$lambda.1se, k = 20 
pred_glmnet_cv_response_mae_8_test <- predict(glmnet_cv_fit_mae_2,as.matrix(series_2_test[,2:ncol(series_2_test)]), 
                                              s = glmnet_cv_fit_mae_2$lambda.1se , type = "response")
pred_glmnet_cv_response_mae_8_test <- pred_glmnet_cv_response_mae_8_test[,1:8,1]
pred_glmnet_cv_response_mae_8_test <- as_tibble(pred_glmnet_cv_response_mae_8_test)
pred_glmnet_cv_response_mae_8_test <- pred_glmnet_cv_response_mae_8_test %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_8_test$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_8_test$outcome_pred_binary)

glmnet_cv_test_acc_mae_8 <- confusionMatrix(pred_glmnet_cv_response_mae_8_test$outcome_pred_binary,
                                            series_2_test_tmp$outcome_binary)$overall[["Accuracy"]]

### Trivial model accuracy 
pred_trivial_series_2_test <- as.factor(ifelse(
  series_2_test$PtsPer_Reg_tm_1 > series_2_test$PtsPer_Reg_tm_2, 1, 0))

pred_trivial_acc_test <- confusionMatrix(pred_trivial_series_2_test,
                                         series_2_test_tmp$outcome_binary)$overall[["Accuracy"]]

# Updating accuracy dataframe
acc_df$test_acc = c(glmnet_cv_test_acc_mae_4,glmnet_cv_test_acc_mae_8,pred_trivial_acc_test)
acc_df  

# Bar Chart Test Accuracy
bar_chart_test_acc <- acc_df %>%
  ggplot(aes(reorder(model,train_acc),test_acc)) +
  geom_col(aes(fill = model)) +
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,0.7)) +
  labs(title = "Test Accuracy", y = "Test Accuracy", x = "Model") +
  theme(legend.position = "none")

# coefficients
coef_lambda.1SE_2 <- tibble(
  coefficient = c(row.names(glmnet_cv_coef_mae_8$`Lose in 4`),
                  row.names(glmnet_cv_coef_mae_8$`Lose in 4`)),
  value = c(glmnet_cv_coef_mae_8$`Lose in 4`[1:length(glmnet_cv_coef_mae_8$`Lose in 4`)],
            glmnet_cv_coef_mae_8$`Win in 4`[1:length(glmnet_cv_coef_mae_8$`Win in 4`)]),
  class = rep(c("Losein4","Winin4"), each = length(glmnet_cv_coef_mae_8$`Lose in 4`))
)
coef_lambda.1SE_2 <- coef_lambda.1SE_2 %>%
  filter(value !=0)

dotplot_coef <- coef_lambda.1SE_2 %>%
  ggplot(aes(coefficient,value, color = class)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45))

