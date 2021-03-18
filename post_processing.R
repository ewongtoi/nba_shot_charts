library(tidyr)
library(tidyverse)
library(here)
library(plyr)

samples <- readRDS(here("saved_robjs/samps_moran3"))


summary(samples)

param_nm <- rownames(samples$summary$all.chains)

zsamp1 <- samples$samples$chain1[ , grep('z', colnames(samples$samples$chain1))]

alphas1 <- samples$samples$chain1[ , grep('alphas', colnames(samples$samples$chain1))]

names(alphas1[1,])

alphas1[,1]
length(alphas1[2,])

class(alphas1)
length(alphas1)

# NEED TO TEST

reorder_coefs <- function(c_smps, mmbr_list) {
  num_clust <- max(mmbr_list)
  should_swap == FALSE
  
  
  if(num_clust == 2){
    if(c_smps[1] > c_smps[2]){
      temps <- c_smps[c(2, 169, 336, 503, 670)]
      c_smps[c(2, 169, 336, 503, 670)] <- c_smps[c(1, 168, 335, 502, 669)]
      c_smps[c(1, 168, 335, 502, 669)] <- temps
      
      plyr::mapvalues(mmbr_list, from=c(1, 2), to=c(2, 1))
      
      return(c_smps)
    }
    
    
  }
  
  return(NULL)
}


mapvalues(a, from=c(1, 2, 3, 4, 5), to=c(4, 5, 6, 7, 8))
a

mcmcout_median_ab <- as_tibble(mcmc.outiw$summary$all.chains) %>% 
  add_column(param_nm, .before="Mean") %>% 
  dplyr::filter(str_detect(param_nm, "^beta|^alpha")) %>% 
  mutate(player_ind = as.numeric(str_extract(param_nm, "(?<=\\[)(.*?)(?=,)"))) %>% 
  mutate(zone = as.numeric(str_extract(param_nm, "(?<=,)(.*?)(?=\\])"))) %>% 
  mutate(param = str_extract(param_nm, "(.*?)(?=\\[)")) %>%
  arrange(.group_by=player_ind) %>% 
  dplyr::select(c(player_ind,Median, zone, param)) %>% 
  pivot_wider(names_from=c(zone, param), values_from=Median) %>% 
  add_column(player_name, .before="player_ind")
