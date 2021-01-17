library(dplyr)
library(tidyverse)
library(ggplot2)
library(nimble)
library(reshape2)
library(janitor)
library(visdat)
library(useful)
library(splines)
library(multisensi)
library(stringi)

# load data ---------------------------------------------------------------

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


list_shots <- shots %>% 
  select(PLAYER_NAME, PLAYER_ID, EVENT_TYPE, SHOT_MADE_FLAG, SHOT_ZONE_BASIC) %>% 
  tabyl(SHOT_ZONE_BASIC, SHOT_MADE_FLAG, PLAYER_ID, show_missing_levels = TRUE)

shots %>% filter(PLAYER_ID==101106) %>% 
  select(PLAYER_ID, EVENT_TYPE, SHOT_MADE_FLAG, SHOT_ZONE_BASIC) %>% 
  tabyl(SHOT_ZONE_BASIC, SHOT_MADE_FLAG, PLAYER_ID, show_missing_levels = TRUE)


rezoned_shots <- shots %>% 
  mutate(zone = ifelse(SHOT_ZONE_BASIC %in% c("In The Paint (Non-RA)", "Restricted Area"), 
                       SHOT_ZONE_BASIC, 
                       paste(SHOT_ZONE_BASIC, SHOT_ZONE_AREA)))


# visualize zones ---------------------------------------------------------

# see regions based on SHOT_ZONE_BASIC/SHOT_ZONE_AREA
# basic seems to be the one that's more useful
ggplot(data=sample_n(shots, 10000), aes(x=LOC_X, y=LOC_Y, color=SHOT_ZONE_BASIC)) + geom_point()
ggplot(data=sample_n(shots, 10000), aes(x=LOC_X, y=LOC_Y, color=SHOT_ZONE_AREA)) + geom_point()
ggplot(data=sample_n(rezoned_shots, 4000), aes(x=LOC_X, y=LOC_Y, color=zone)) + geom_point()

list_rezoned_shots <- rezoned_shots %>% 
  select(PLAYER_NAME, PLAYER_ID, EVENT_TYPE, SHOT_MADE_FLAG, zone) %>% 
  tabyl(zone, SHOT_MADE_FLAG, PLAYER_ID, show_missing_levels = TRUE)

# function to transform the list items into useful rows
list_item_to_row <- function(lll){
  
  fgmake <- lll[,2]
  fgmiss <- lll[,3]
  
  pctg <- fgmake / (fgmake + fgmiss)
  
  fga <- fgmake + fgmiss
  
  rrr <- data.frame(t(c(fga, pctg)))
  
  rrr[is.na(rrr)] <- 0
  
  zone_names <- c("ab3bc", 
                  "ab3c", "ab3lc", "ab3rc", 
                  "bcbc", 
                  "paint", 
                  "lc3", 
                  "mrc", "mrlc", "mrl", "mrrc", "mrr", 
                  "ra", 
                  "rc3")
  
  cnms <- c(paste(zone_names, "attempt"), paste(zone_names, "pct"))
  
  colnames(rrr) <- cnms
  
  return(rrr)
}

attempts_pcts <-  map_dfr(list_shots, list_item_to_row)
attempts_pcts_rz <-  map_dfr(list_rezoned_shots, list_item_to_row)

names(list_shots) == sort(names(list_shots))


sort(as.character(unique(shots$PLAYER_ID))) == names(list_rezoned_shots)

# player info to be merged with summarized shot data
player_inf <- shots %>% 
  select(PLAYER_NAME, PLAYER_ID) %>% 
  distinct() %>% 
  arrange(as.character(PLAYER_ID))

wide_shots <- bind_cols(player_inf, attempts_pcts)

wide_rezoned_shots <- bind_cols(player_inf, attempts_pcts_rz)

wide_rezoned_shots$total_attempts <- wide_rezoned_shots %>% 
  select(ends_with("attempt")) %>% 
  rowSums() 

wide_rezoned_shots <- wide_rezoned_shots %>% 
  select(!ends_with(c("bc attempt", "bc pct")))

names(wide_shots)      

View(wide_shots)
names(wide_rezoned_shots)
View(wide_rezoned_shots)


wide_rezoned_shots %>% select(ends_with("attempt")) %>% na_if(0) %>% vis_miss()
twoplus_each <- sum(!is.na(wide_rezoned_shots %>%
             select(ends_with("attempt")) %>% 
             na_if(0) %>% 
             na_if(1) %>% na_if(2) %>% 
             rowSums()))


twoplus_each - wide_rezoned_shots %>%
  select(contains("attempt")) %>% 
  na_if(0) %>% 
  na_if(1) %>% na_if(2) %>% 
  filter(total_attempts > 250) %>% 
  select(!total_attempts) %>% 
  rowSums() %>% 
  is.na() %>% 
  sum()

wide_rezoned_shots$total_attempts


# find zone centers -------------------------------------------------------
cart_zone_ctrs <- rezoned_shots %>% 
  group_by(zone) %>% 
  summarize(mean_x = mean(LOC_X), 
            mean_y = mean(LOC_Y))
cart_zone_ctrs$zone
polar_zone_ctrs <- cart2pol(cart_zone_ctrs$mean_x, cart_zone_ctrs$mean_y) %>% 
  bind_cols(cart_zone_ctrs$zone) %>% 
  rename(zone = 5) 


relevant_ctrs <- polar_zone_ctrs %>% 
  filter(!str_detect(zone, "(BC)"))


# bspline -----------------------------------------------------------------

# use bs() function

# attempts_pcts <-  map_dfr(list_shots, list_item_to_row)

relevant_ctrs %>% select(r, theta) %>% basis.bsplines(basis.args = list(knots=3, mdegree=3))

cbind(c(1, 2, 3), c(43, 5, 2)) %>% bs()

wide_rezoned_shots_nobc %>% select(contains("attempt")) %>% names()

wide_rezoned_shots_nobc <- wide_rezoned_shots %>% 
  select(!contains("bc")) %>% 
  filter_at((vars(ends_with("attempt"))), all_vars(. > 4))

head(wide_rezoned_shots_nobc)
class(wide_rezoned_shots_nobc)

"Timothe Luwawu-Cabarrot" %in% wide_rezoned_shots$PLAYER_NAME

