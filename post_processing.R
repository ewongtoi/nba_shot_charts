library(tidyr)
library(tidyverse)
library(here)
library(plyr)

samples <- readRDS(here("saved_robjs/samps_moran3_diag"))

which(names(alphas1[1,]) %in% c("alphas[1, 1]", "alphas[1, 2]", "alphas[1, 3]", "alphas[1, 4]", "alphas[1, 5]"))
which(names(betas1[1,]) %in% c("betas[1, 1]", "betas[1, 2]", "betas[1, 3]", "betas[1, 4]", "betas[1, 5]"))

reorder_coefs <- function(c_smps,c_smps2, mmbr_list) {
  num_clust <- max(mmbr_list)
  should_swap <- FALSE
  
  
  if(num_clust == 2){
    if(c_smps[1] < c_smps[2]){
      temps <- c_smps[c(2, 169, 336, 503, 670)]
      c_smps[c(2, 169, 336, 503, 670)] <- c_smps[c(1, 168, 335, 502, 669)]
      c_smps[c(1, 168, 335, 502, 669)] <- temps
     
      temps2 <- c_smps2[c(2, 169, 336, 503, 670)]
      c_smps2[c(2, 169, 336, 503, 670)] <- c_smps2[c(1, 168, 335, 502, 669)]
      c_smps2[c(1, 168, 335, 502, 669)] <- temps2
       
      relab <- plyr::mapvalues(mmbr_list, from=c(1, 2), to=c(2, 1))
      
      return(list(c_smps=c_smps, c_smps2=c_smps2, mml=relab))
    }
    
    
  }
  
  return(list(c_smps=c_smps, c_smps2=c_smps2, mml=mmbr_list))
}

impose_order <- function(c_smps_mat, c_smps_mat2, mmbr_mat){
  ordered_coefs <- matrix(0, nrow=nrow(c_smps_mat), ncol=ncol(c_smps_mat))
  ordered_coefs2 <- matrix(0, nrow=nrow(c_smps_mat), ncol=ncol(c_smps_mat))
  relabelled_grps <- matrix(0, nrow=nrow(mmbr_mat), ncol=ncol(mmbr_mat))
  
  print(dim(relabelled_grps))
  
  #print(dim(ordered_coefs))
  nsamps <- nrow(c_smps_mat)
  
  for(s in 1:nsamps) {
    reordered <- reorder_coefs(c_smps_mat[s, ], c_smps_mat2[s, ], mmbr_mat[s,])
    
    #print(dim(reordered$c_smps))
    ordered_coefs[s, ] <- reordered$c_smps
    ordered_coefs2[s, ] <- reordered$c_smps2
    
   # print(length(reordered$mml))
    
    relabelled_grps[s, ] <- reordered$mml
  }
  
  return(list(ord_coefs=ordered_coefs, ord_coefs2=ordered_coefs2, relab_grps=relabelled_grps))
}



summary(samples)

param_nm <- rownames(samples$summary$all.chains)


# gather samples
zsamp1 <- samples$samples$chain1[ , grep('z', colnames(samples$samples$chain1))]
zsamp2 <- samples$samples$chain2[ , grep('z', colnames(samples$samples$chain2))]
alphas1 <- samples$samples$chain1[ , grep('alphas', colnames(samples$samples$chain1))]
alphas2 <- samples$samples$chain2[ , grep('alphas', colnames(samples$samples$chain2))]
betas1 <- samples$samples$chain1[ , grep('betas', colnames(samples$samples$chain1))]
betas2 <- samples$samples$chain2[ , grep('betas', colnames(samples$samples$chain2))]

imposed_samps1 <- impose_order(alphas1[2001:10000,], betas1[2001:10000,], zsamp1[2001:10000,])
imposed_samps2 <- impose_order(alphas2[2001:10000,], betas2[2001:10000,], zsamp2[2001:10000,])

dim(zsamp1)
dim(alphas1)
dim(betas1)



clust_count_ord <- rep(0, times=8000)

for(i in 1:8000){
  curr_mml <- imposed_samps1$relab_grps
  clust_count_ord[i] <- max(curr_mml[i,])
}

clust_count_ord2 <- rep(0, times=8000)

for(i in 1:8000){
  curr_mml <- imposed_samps2$relab_grps
  clust_count_ord2[i] <- max(curr_mml[i,])
}

length(which(clust_count_ord == 2))

ord_alphs1 <- imposed_samps1$ord_coefs
ord_betas1 <- imposed_samps1$ord_coefs2

colMeans(ord_alphs1[which(clust_count_ord == 2),1:2])
apply(ord_alphs1[which(clust_count_ord == 2),1:2], 2, sd)
apply(ord_betas1[which(clust_count_ord == 2),1:2], 2, sd)


hist(clust_count_ord2)

names(alphas1[1,])

alphas1[,1]
length(alphas1[2,])

class(alphas1)
length(alphas1)

# c_smps is a row of the matrix of posterior samples
# mmbr_list is the z vector holding the assignments

mml1 <- zsamp1[200,]
csmps1 <- alphas1[200,]
csmps1


outs <- reorder_coefs(csmps1, mml1)


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
