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
