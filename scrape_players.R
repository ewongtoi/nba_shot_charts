# https://www.basketball-reference.com/teams/GSW/2019.html
# https://hoopshype.com/salaries/players/2018-2019/
# https://www.basketball-reference.com/leagues/NBA_2019_shooting.html
library(tidyverse)
library(robotstxt)
library(rvest)
library(here)
library(stringi)


# player info (ht, wt, etc) -----------------------------------------------
nba_reg <- read_csv(here("nba_ref_summary.txt"))

team_abbrv <- nba_reg %>% select(5) %>% unique() %>% slice(c(2:14, 16:32))

paths_allowed("https://www.basketball-reference.com")

player_info <- matrix(0, nrow = 1, ncol = 9)

for(t in 1:30) {

  new_url <- paste0("https://www.basketball-reference.com/teams/", 
                    slice(team_abbrv, t), 
                    "/2019.html")

  team_page <- read_html(new_url)
  
  team_inf_list <- team_page %>% 
    html_nodes("#roster .center , #roster .right , #roster .left") %>% 
    html_text()
  
  team_inf_mat <- team_inf_list %>% 
    matrix(nrow=(length(team_inf_list)/9), ncol=9, byrow=TRUE)
  
  player_info <- rbind(player_info, team_inf_mat)
}


player_tib <- player_info %>% 
  as_tibble() %>% 
  distinct()

colnames(player_tib) <- player_tib %>% slice(2)

player_tib <- player_tib %>% 
  slice(-(1:2)) %>% 
  mutate(Player = stringi::stri_trans_general(Player, "Latin-ASCII")) %>% 
  select("Player", "Pos", "Ht", "Wt", "Exp", "Birth Date") %>% 
  mutate("Exp" = ifelse(Exp == 'R', 0, Exp))
  




# salary ------------------------------------------------------------------
paths_allowed("https://hoopshype.com/salaries/players/2018-2019/")

sal_page <- read_html("https://hoopshype.com/salaries/players/2018-2019/")
sal_page %>% 
  html_nodes(".hh-salaries-sorted , .name") %>% 
  html_text() %>% 
  str_remove("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t") %>% 
  str_remove("\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t")
sal_page %>% 
  html_nodes(".name") %>% 
  html_text()
  
sal_data <- read_delim(here("salary_data_1819.txt"), "\t") %>% 
  select(2:3)
colnames(sal_data) <- c("Player", "Salary")

sal_data <- sal_data %>% 
  mutate(Salary = str_sub(Salary, 2)) %>% 
  mutate(Salary = str_remove_all(Salary, ",")) %>% 
  mutate(Salary = as.numeric(Salary))


# salary and info ---------------------------------------------------------
joined_inf <- left_join(sal_data, player_tib, by=c("Player" = "Player")) %>% 
  group_by(Player) %>% 
  distinct() %>% 
  drop_na()
View(joined_inf)
dim(joined_inf)

saveRDS(joined_inf, here("salary_plus"))

