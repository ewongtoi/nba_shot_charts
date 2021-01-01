library(dplyr)
library(tidyverse)
library(ggplot2)
library(nimble)


shots <- read_csv("nba_shotchartdetail_2018-19.csv")
shots
shots1 <- read_csv("nba_shotchartdetail_2018-191.csv")
shots

View(shots)


n_players <- shots %>% select(PLAYER_NAME) %>% n_distinct()

ggplot(data=shots, aes(x=SHOT_DISTANCE)) + geom_bar()

summary(shots)

dim(shots)
table(shots$SHOT_ZONE_BASIC)

unique(shots$TEAM_ID)

shots %>% filter(PLAYER_NAME=="James Harden")


n_players
