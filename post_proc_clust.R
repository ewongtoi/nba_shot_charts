library(tidyr)
library(tidyverse)
library(here)
library(plyr)
#library(BasketballAnalyzeR)
library(cowplot)
library(grid)
#library(Rmisc)
#library(ggpubr)
library(jpeg)
library(RCurl)
library(gridExtra)
library(xtable)

# ------------- helper functions

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


reorder_coefs_multi <- function(c_smps,c_smps2, mmbr_list, indices) {
  num_clust <- length(unique(mmbr_list))

  
  derange <- rank(c_smps[unique(mmbr_list)])

  shift_inds <- rep(indices, num_clust) + rep((unique(mmbr_list)-1), each=5)
  old_vals1 <- matrix(c_smps[shift_inds], nrow=num_clust, ncol=5, byrow=TRUE)
  old_vals2 <- matrix(c_smps2[shift_inds], nrow=num_clust, ncol=5, byrow=TRUE)
  
  
  for(cl in 1:num_clust){

    c_smps[indices+derange[cl]-1] <- old_vals1[cl, 1:5]
    c_smps2[indices+derange[cl]-1] <- old_vals2[cl, 1:5]
  }
  
  relab <- plyr::mapvalues(mmbr_list, from=unique(mmbr_list), to=derange)
  

  
  return(list(c_smps=c_smps, c_smps2=c_smps2, mml=relab))
}

impose_order <- function(c_smps_mat, c_smps_mat2, mmbr_mat){
  ordered_coefs <- matrix(0, nrow=nrow(c_smps_mat), ncol=ncol(c_smps_mat))
  ordered_coefs2 <- matrix(0, nrow=nrow(c_smps_mat), ncol=ncol(c_smps_mat))
  relabelled_grps <- matrix(0, nrow=nrow(mmbr_mat), ncol=ncol(mmbr_mat))
  
  changes <- 0
  
  #print(dim(ordered_coefs))
  nsamps <- nrow(c_smps_mat)
  
  for(s in 1:nsamps) {
    reordered <- reorder_coefs_multi(c_smps_mat[s, ], c_smps_mat2[s, ], mmbr_mat[s,])
    
    #print(dim(reordered$c_smps))
    ordered_coefs[s, ] <- reordered$c_smps
    ordered_coefs2[s, ] <- reordered$c_smps2
    
   # print(length(reordered$mml))
    
    relabelled_grps[s, ] <- reordered$mml
    
    if(reordered$mml != mmbr_mat[s, ]){
      changes <- changes + 1
    }
  }
  
  return(list(ord_coefs=ordered_coefs, ord_coefs2=ordered_coefs2, relab_grps=relabelled_grps, changes=changes))
}


impose_order_multi <- function(c_smps_mat, c_smps_mat2, mmbr_mat, indices){
  ordered_coefs <- matrix(0, nrow=nrow(c_smps_mat), ncol=ncol(c_smps_mat))
  ordered_coefs2 <- matrix(0, nrow=nrow(c_smps_mat), ncol=ncol(c_smps_mat))
  relabelled_grps <- matrix(0, nrow=nrow(mmbr_mat), ncol=ncol(mmbr_mat))
  nsamps <- nrow(c_smps_mat)
  
  for(s in 1:nsamps) {
    reordered <- reorder_coefs_multi(c_smps_mat[s, ], c_smps_mat2[s, ], mmbr_mat[s,], indices)
    
    #print(dim(reordered$c_smps))
    ordered_coefs[s, ] <- reordered$c_smps
    ordered_coefs2[s, ] <- reordered$c_smps2
    
    # print(length(reordered$mml))
    
    relabelled_grps[s, ] <- reordered$mml
  }
  
  return(list(ord_coefs=ordered_coefs, ord_coefs2=ordered_coefs2, relab_grps=relabelled_grps))
}



make_adj_mat <- function(member_list){
  n <- length(member_list)
  
  adj_mat <- matrix(0, nrow=n, ncol=n)
  
  for(i in 1:n){
    adj_mat[,i] = (member_list == member_list[i]) #* member_list[i]
  }
  
  #rownames(adj_mat) <- player_names[[1]]
  #colnames(adj_mat) <- player_names[[1]]
  
  
  return(adj_mat)
  
}


make_adj_mat_grp <- function(member_list){
  n <- length(member_list)
  
  adj_mat <- matrix(0, nrow=n, ncol=n)
  
  for(i in 1:n){
    adj_mat[,i] = (member_list == member_list[i]) * member_list[i]
  }
  
  rownames(adj_mat) <- player_names[[1]]
  colnames(adj_mat) <- player_names[[1]]
  
  
  return(adj_mat)
  
}

# -------------- end helper functions

how_many_clusters <- rep(0, times=100)

base_path <- "/home/ewongtoi/nba_shot_charts/saved_robjs/cluster_sims/easy/easy"
print("preloop")
for (loop_ind in 1:100){
  print(loop_ind)
  path <- paste0(base_path, toString(loop_ind))


  samples <- readRDS(path)


  n_players <- 160


  # gather samples
  zsamp1 <- samples$samples$chain1[ , grep('clust', colnames(samples$samples$chain1))]
  zsamp2 <- samples$samples$chain2[ , grep('clust', colnames(samples$samples$chain2))]  
  alphas1 <- samples$samples$chain1[ , grep('alphas', colnames(samples$samples$chain1))]
  alphas2 <- samples$samples$chain2[ , grep('alphas', colnames(samples$samples$chain2))]
  betas1 <- samples$samples$chain1[ , grep('betas', colnames(samples$samples$chain1))]
  betas2 <- samples$samples$chain2[ , grep('betas', colnames(samples$samples$chain2))]

  alphas1[1,which(names(alphas1[1,]) %in% c("alphas[1, 1]", "alphas[1, 2]", "alphas[1, 3]", "alphas[1, 4]", "alphas[1, 5]"))]
  indices <- which(names(betas1[1,]) %in% c("betas[1, 1]", "betas[1, 2]", "betas[1, 3]", "betas[1, 4]", "betas[1, 5]"))



  param_nm <- rownames(samples$summary$all.chains)

  # ordered samps for chains 1/2
  imposed_samps1 <- impose_order_multi(alphas1[5001:20000,], betas1[5001:20000,], zsamp1[5001:20000,], indices)
  imposed_samps2 <- impose_order_multi(alphas2[5001:20000,], betas2[5001:20000,], zsamp2[5001:20000,], indices)


  N <- 15000
  l <- vector("list", N)
  l2 <- vector("list", N)
  sum_mat <- matrix(0, n_players, n_players)
  for(z in 1:N){

    l[[z]] <- make_adj_mat(imposed_samps1$relab_grps[z,])
    l2[[z]] <- make_adj_mat(imposed_samps2$relab_grps[z,])
    sum_mat <- l[[z]] + l2[[z]] + sum_mat
  }


  mean_mat <- sum_mat/(2*N)

  dists <- rep(0, 2*N)
  for(z in 1:N){
  
    diff <- mean_mat - l[[z]]
    ss <- mean(diff^2)
    dists[z] = ss

  
    diff <- mean_mat - l2[[z]]
    ss <- mean(diff^2)
    dists[z + N] = ss
  }



  zsamp1 <- samples$samples$chain1[ , grep('clust', colnames(samples$samples$chain1))]
  zsamp2 <- samples$samples$chain2[ , grep('clust', colnames(samples$samples$chain2))]
  zsamptot <- rbind(zsamp1, zsamp2)
  clust_count <- rep(0, times=30000)
  for(i in 1:15000){
    clust_count[i] <- length(unique(zsamp1[i+5000, ]))
    clust_count[i+ 15000] <- length(unique(zsamp2[i+5000, ]))
  }


  closest_mat <- which.min(dists)
  closest_mat




  if (closest_mat > 15000){
    # is in second chain
    how_many_clusters[loop_ind] = length(table(imposed_samps2$relab_grps[closest_mat - 15000,]))
  } else {
    # is in first chain
    how_many_clusters[loop_ind] = length(table(imposed_samps1$relab_grps[closest_mat+5000,]))
  }
}

hist(how_many_clusters)

save_path <- "/home/ewongtoi/nba_shot_charts/saved_robjs/cluster_sims/easy/clusters"
saveRDS(how_many_clusters, save_path)
ccount <- readRDS("/home/ewongtoi/nba_shot_charts/saved_robjs/cluster_sims/easy/clusters")
hist(ccount)
