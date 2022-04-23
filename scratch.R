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
library(fda)
library(here)
library(BasketballAnalyzeR)
library(grid)
library(jpeg)

# load data ---------------------------------------------------------------
# check for years
shots <- read_csv(here::here("data/1920/my_nba_shotchartdetail_2019-20.csv"))
shots
shots <- read_csv(here::here("data/1819/nba_shotchartdetail_2018-191.csv"))
shots

post_shots <- read_csv("nba_shotchartdetail_2018-19_postseason.csv")

View(shots)


n_players <- shots %>% dplyr::select(PLAYER_NAME) %>% n_distinct()


list_shots <- shots %>% 
  dplyr::select(PLAYER_NAME, PLAYER_ID, EVENT_TYPE, SHOT_MADE_FLAG, SHOT_ZONE_BASIC) %>% 
  tabyl(SHOT_ZONE_BASIC, SHOT_MADE_FLAG, PLAYER_ID, show_missing_levels = TRUE)



rezoned_shots <- shots %>% 
  mutate(zone = ifelse(SHOT_ZONE_BASIC %in% c("In The Paint (Non-RA)", "Restricted Area"), 
                       SHOT_ZONE_BASIC, 
                       paste(SHOT_ZONE_BASIC, SHOT_ZONE_AREA)))

saveRDS(rezoned_shots, here::here("saved_robjs/1819/rezoned_shots_1819"))

# visualize zones ---------------------------------------------------------
# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))



# see regions based on SHOT_ZONE_BASIC/SHOT_ZONE_AREA
# basic seems to be the one that's more useful
ggplot(data=sample_n(shots, 1000), aes(x=LOC_X, y=LOC_Y, color=SHOT_ZONE_BASIC)) + geom_point()
shotslocs <- ggplot(data=sample_n(shots, 1000), aes(x=LOC_X, y=LOC_Y, color=SHOT_ZONE_AREA)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point()
rz_locs <- ggplot(data=sample_n(rezoned_shots %>% dplyr::filter(LOC_Y < 500), 1000), aes(x=LOC_X, y=LOC_Y, color=zone)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point()



list_rezoned_shots <- rezoned_shots %>% 
  dplyr::select(PLAYER_NAME, PLAYER_ID, EVENT_TYPE, SHOT_MADE_FLAG, zone) %>% 
  tabyl(zone, SHOT_MADE_FLAG, PLAYER_ID, show_missing_levels = TRUE)

# function to transform the list items into useful rows
list_item_to_row <- function(lll){
  
  fgmake <- lll[,3]
  fgmiss <- lll[,2]
  
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

# attempts_pcts <-  map_dfr(list_shots, list_item_to_row)
attempts_pcts_rz <-  map_dfr(list_rezoned_shots, list_item_to_row)

# names(list_shots) == sort(names(list_shots))


# sort(as.character(unique(shots$PLAYER_ID))) == names(list_rezoned_shots)

# player info to be merged with summarized shot data
player_inf <- shots %>% 
  dplyr::select(PLAYER_NAME, PLAYER_ID) %>% 
  distinct() %>% 
  arrange(as.character(PLAYER_ID))

# wide_shots <- bind_cols(player_inf, attempts_pcts)

wide_rezoned_shots <- bind_cols(player_inf, attempts_pcts_rz)

wide_rezoned_shots$total_attempts <- wide_rezoned_shots %>% 
  dplyr::select(ends_with("attempt")) %>% 
  rowSums() 

wide_rezoned_shots <- wide_rezoned_shots %>% 
  dplyr::select(!ends_with(c("bc attempt", "bc pct")))


wide_rezoned_shots %>% dplyr::select(ends_with("attempt")) %>% na_if(0) %>% vis_miss()
twoplus_each <- sum(!is.na(wide_rezoned_shots %>%
             dplyr::select(ends_with("attempt")) %>% 
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


polar_zone_ctrs <- cart2pol(cart_zone_ctrs$mean_x, cart_zone_ctrs$mean_y) %>% 
  bind_cols(cart_zone_ctrs$zone) %>% 
  rename(zone = 5) 


relevant_ctrs <- polar_zone_ctrs %>% 
  filter(!str_detect(zone, "(BC)"))


# bspline -----------------------------------------------------------------

# use bs() function

# attempts_pcts <-  map_dfr(list_shots, list_item_to_row)

relevant_ctrs %>% dplyr::select(r, theta) 

cbind(c(1, 2, 3), c(43, 5, 2)) %>% bs()



wide_rezoned_shots_nobc <- wide_rezoned_shots %>% 
  dplyr::select(!contains("bc")) %>% 
  #filter_at((vars(ends_with("attempt"))), all_vars(. > 3)) 
  filter(total_attempts > 100)


all_rezoned_shots_nobc <- wide_rezoned_shots %>% 
  dplyr::select(!contains("bc"))

dim(wide_rezoned_shots_nobc)

"Giannis Antetokounmpo" %in% wide_rezoned_shots$PLAYER_NAME

str_ht_to_in <- function(str_ht){
  ret_vec <- rep(0, times=length(str_ht))
  
  for(i in 1:length(str_ht)){
    split <- strsplit(str_ht[i], "-")
  
    ft <- as.numeric(split[[1]][1])
    inch <- as.numeric(split[[1]][2])
  
    ret_vec[i] <- 12*ft + inch
  }
  
  return(ret_vec)
}

## CHCEK PATH
joined_inf <- readRDS(here::here("saved_robjs/1819/salary_plus"))


joined_shots <- left_join(wide_rezoned_shots_nobc, joined_inf, 
          by = c("PLAYER_NAME" = "Player")) %>% 
  drop_na() %>% 
  group_by(PLAYER_NAME) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::mutate(Ht_in = str_ht_to_in(Ht)) %>% 
  dplyr::mutate(Wt = as.numeric(Wt)) %>% 
  dplyr::mutate(Exp = as.numeric(Exp))

joined_shots$Wt

joined_shots_full <- left_join(wide_rezoned_shots, joined_inf, 
                          by = c("PLAYER_NAME" = "Player")) %>% 
  drop_na() %>% 
  group_by(PLAYER_NAME) %>% 
  slice(1) %>% 
  ungroup()

all_joined <- left_join(all_rezoned_shots_nobc, joined_inf, 
                        by = c("PLAYER_NAME" = "Player")) %>% 
  drop_na() %>% 
  group_by(PLAYER_NAME) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::mutate(Ht_in = str_ht_to_in(Ht)) %>% 
  dplyr::mutate(Wt = as.numeric(Wt)) %>% 
  dplyr::mutate(Exp = as.numeric(Exp))


curry_atts <- joined_shots %>% 
  filter(PLAYER_NAME == "Seth Curry") %>% 
  dplyr::select(ends_with("pct")) %>% 
  t()

dim(relevant_ctrs %>% dplyr::select(r, theta) )

plot(y=residuals(lm(curry_atts ~ bs(relevant_ctrs$r) + bs(relevant_ctrs$theta) )), 
     x=lm(curry_atts ~ bs(relevant_ctrs$r) + bs(relevant_ctrs$theta) )$fitted.values)
curry_atts

design_shooting <- cbind(1, bs(relevant_ctrs$r), bs(relevant_ctrs$theta))

fit_mu <- function(player_row){
  player_atts <- dplyr::select(player_row, ends_with("attempt")) %>% t()
  
  
  mu <- lm(player_atts ~ bs(relevant_ctrs$r) + bs(relevant_ctrs$theta))$fitted.values
  return(t(mu))
}

fit_eta <- function(player_row){
  player_pcts <- dplyr::select(player_row, ends_with("pct")) %>% t()
  
  
  phi <- lm(player_pcts ~ bs(relevant_ctrs$r) + bs(relevant_ctrs$theta))$fitted.values
  return(t(phi))
}

fit_phi(wide_rezoned_shots_nobc[1,])

mu_ests <- joined_shots %>% 
  rowwise() %>% 
  fit_mu() %>% 
  as_tibble() %>% 
  add_column(PLAYER_NAME = joined_shots$PLAYER_NAME) %>% 
  relocate(PLAYER_NAME)

eta_ests <- joined_shots %>% 
  rowwise() %>% 
  fit_eta() %>% 
  as_tibble() %>% 
  add_column(PLAYER_NAME = joined_shots$PLAYER_NAME) %>% 
  relocate(PLAYER_NAME)

View(eta_ests)
View(mu_ests)
saveRDS(joined_shots, here::here("saved_robjs/1819/joined_shots_100plus_1920"))
saveRDS(design_shooting, here("/saved_robjs/design_shooting"))
saveRDS(wide_rezoned_shots_nobc, here("saved_robjs/1819/wide_rezoned_nobc_100plus_1920"))
#saveRDS(mu_ests, here("/saved_robjs/mu_ests"))
#saveRDS(eta_ests, here("/saved_robjs/eta_ests"))

#write_csv(eta_ests, here("/data/eta_ests.csv"))
#write_csv(mu_ests, here("/data/mu_ests.csv"))
write_csv(joined_shots, here::here("data/1920/joined_shots_1920.csv"))
write_csv(joined_shots_full, here("data/1920/joined_shots_full_1920.csv"))

# wide_rezoned_shots_nobc <- readRDS(here("wide_rezoned_nobc"))
view(joined_shots_full)

# 19-20 -------------------------------------------------------------------
shots <- read_csv("data/my_nba_shotchartdetail_2019-20.csv")


atleast500 <- (table(shots$PLAYER_NAME) %>% sort())[373:527] %>% names()

atleast500_shots <- shots %>% 
  dplyr::filter(PLAYER_NAME %in% atleast500) %>% 
  arrange(PLAYER_NAME) %>% 
  dplyr::select(c(PLAYER_NAME, LOC_X, LOC_Y, EVENT_TYPE))




polar_500 <- cart2pol(atleast500_shots$LOC_X, atleast500_shots$LOC_Y)


write_csv(cbind(atleast500_shots, polar_500[, 1:2]), here("/data/reg1920_500plus.csv"))




names(wide_rezoned_shots)



