library(dplyr)
library(tidyverse)
library(ggplot2)
library(nimble)
library(reshape2)
library(janitor)

shots <- read_csv("my_nba_shotchartdetail_2018-19.csv")
shots
shots1 <- read_csv("nba_shotchartdetail_2018-191.csv")
shots

post_shots <- read_csv("nba_shotchartdetail_2018-19_postseason.csv")

View(shots)


n_players <- shots %>% select(PLAYER_NAME) %>% n_distinct()

ggplot(data=shots, aes(x=SHOT_DISTANCE)) + geom_bar()

summary(shots)

dim(shots)
table(shots$SHOT_ZONE_BASIC)

unique(shots$TEAM_ID)

shots %>% filter(PLAYER_NAME=="James Harden")


n_players


head(shots)
names(shots)
shots$SHOT_MADE_FLAG

# see regions based on SHOT_ZONE_BASIC/SHOT_ZONE_AREA
# basic seems to be the one that's more useful
ggplot(data=sample_n(shots, 10000), aes(x=LOC_X, y=LOC_Y, color=SHOT_ZONE_BASIC)) + geom_point()
ggplot(data=sample_n(shots, 10000), aes(x=LOC_X, y=LOC_Y, color=SHOT_ZONE_AREA)) + geom_point()

list_shots <- shots %>% 
  select(PLAYER_NAME, PLAYER_ID, EVENT_TYPE, SHOT_MADE_FLAG, SHOT_ZONE_BASIC) %>% 
  tabyl(SHOT_ZONE_BASIC, SHOT_MADE_FLAG, PLAYER_ID, show_missing_levels = TRUE)

shots %>% filter(PLAYER_ID==101106) %>% 
  select(PLAYER_ID, EVENT_TYPE, SHOT_MADE_FLAG, SHOT_ZONE_BASIC) %>% 
  tabyl(SHOT_ZONE_BASIC, SHOT_MADE_FLAG, PLAYER_ID, show_missing_levels = TRUE)

  
# function to transform the list items into useful rows
list_item_to_row <- function(lll){
  
  fgm <- lll[,2]
  fga <- lll[,3]
  
  pctg <- fgm / (fgm + fga)
  
  rrr <- data.frame(t(c(fgm, pctg)))
  
  rrr[is.na(rrr)] <- 0
  
  colnames(rrr) <- c("Above Break 3 Made", "BC Made", "Paint (non-RA) Made", 
                     "LC3 Made", "MR Made", "RA Made", "RC3 Made",
                     "Above Break 3 pct", "BC pct", "Paint (non-RA) pct", 
                     "LC3 pct", "MR pct", "RA pct", "RC3 pct")
  
  return(rrr)
}

makes_pcts <-  map_dfr(list_shots, list_item_to_row)

list_item_to_row(list_shots[[1]])
list_shots[[1]]
list_shots[1]
shots$PLAYER_ID[1]
list_shots[2]

names(list_shots) == sort(names(list_shots))


sort(as.character(unique(shots$PLAYER_ID))) == names(list_shots)

player_inf <- shots %>% select(PLAYER_NAME, PLAYER_ID) %>% distinct() %>% arrange(as.character(PLAYER_ID))

wide_shots <- bind_cols(player_inf, makes_pcts)
