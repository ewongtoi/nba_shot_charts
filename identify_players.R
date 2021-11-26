# find three players

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
library(magrittr)

joined_shots <-readRDS("~/Documents/uci/nba_shot_charts/saved_robjs/joined_shots")

starset <-joined_shots %>% dplyr::filter(PLAYER_NAME %in% c("Luka Doncic", "DeMar DeRozan", "Stephen Curry", "LeBron James", "James Harden"))
View(starset)

thirteenclub <- joined_shots %>% 
  dplyr::filter(total_attempts > 1250) %>%
  dplyr::filter(total_attempts < 1351)

View(thirteenclub)


small13 <- thirteenclub %>% dplyr::select(ends_with("attempt"))
View(small13)

as.matrix(dist(small13, "euclidean"))

# probably want to take demar, steph, kat (reasonably distant)
