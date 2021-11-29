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

write.csv(thirteenclub, "~/Documents/uci/nba_shot_charts/data/thirteenclub.csv")

small13 <- thirteenclub %>% dplyr::select(ends_with("attempt") | ends_with("pct"))


as.matrix(dist(small13, "euclidean"))

# probably want to take demar, steph, kat (reasonably distant)
interesting_3 <- c("Stephen Curry", "DeMar DeRozan", "Karl-Anthony Towns")

small13$Player <- thirteenclub$PLAYER_NAME

df_att <- melt(small13 %>% 
            dplyr::filter(Player %in% interesting_3) %>% 
            dplyr::select(ends_with("attempt") | "Player"), 
          shot = colnames(smallthirteen), 
          variable.name="shot")

df_pct <- melt(small13 %>% 
                 dplyr::filter(Player %in% interesting_3) %>% 
                 dplyr::select(ends_with("pct") | "Player"), 
               shot = colnames(smallthirteen), 
               variable.name="shot")


ggplot(df_att, aes(shot, value, fill=Player)) + 
  geom_bar(position="dodge",stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  ggtitle("Shot attempts by region") + 
  ylab("Attempts") + 
  xlab("Court Region")

ggplot(df_pct, aes(shot, value, fill=Player)) + 
  geom_bar(position="dodge",stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  ggtitle("Percentages by region") + 
  ylab("Percentage") + 
  xlab("Court Region")


dft = melt(data.frame(A=c(2, 10), B=c(3, 20), 
                     experiment=c("X", "X & Y")),
          variable.name="metric")
dft
ggplot(dft, aes(experiment, value, fill=metric)) + 
  geom_bar(position="dodge",stat="identity")

