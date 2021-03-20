library(tidyr)
library(tidyverse)
library(here)
library(plyr)

samples <- readRDS(here("saved_robjs/samps_moran_randeff_fixedalph1"))

# gather samples
zsamp1 <- samples$samples$chain1[ , grep('clust', colnames(samples$samples$chain1))]
zsamp2 <- samples$samples$chain2[ , grep('clust', colnames(samples$samples$chain2))]
alphas1 <- samples$samples$chain1[ , grep('alphas', colnames(samples$samples$chain1))]
alphas2 <- samples$samples$chain2[ , grep('alphas', colnames(samples$samples$chain2))]
betas1 <- samples$samples$chain1[ , grep('betas', colnames(samples$samples$chain1))]
betas2 <- samples$samples$chain2[ , grep('betas', colnames(samples$samples$chain2))]

alphas1[1,which(names(alphas1[1,]) %in% c("alphas[1, 1]", "alphas[1, 2]", "alphas[1, 3]", "alphas[1, 4]", "alphas[1, 5]"))]
indices <- which(names(betas1[1,]) %in% c("betas[1, 1]", "betas[1, 2]", "betas[1, 3]", "betas[1, 4]", "betas[1, 5]"))

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



summary(samples)

param_nm <- rownames(samples$summary$all.chains)




#imposed_samps1 <- impose_order(alphas1[2001:10000,], betas1[2001:10000,], zsamp1[2001:10000,])
#imposed_samps2 <- impose_order(alphas2[2001:10000,], betas2[2001:10000,], zsamp2[2001:10000,])

# ordered samps for chains 1/2
imposed_samps1 <- impose_order_multi(alphas1[5001:20000,], betas1[5001:20000,], zsamp1[5001:20000,], indices)
imposed_samps2 <- impose_order_multi(alphas2[5001:20000,], betas2[5001:20000,], zsamp2[5001:20000,], indices)


make_adj_mat <- function(member_list){
  n <- length(member_list)
  
  adj_mat <- matrix(0, nrow=n, ncol=n)
  
  for(i in 1:n){
    adj_mat[,i] = (member_list == member_list[i]) #* member_list[i]
  }
  
  rownames(adj_mat) <- player_names[[1]]
  colnames(adj_mat) <- player_names[[1]]
  
  
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

N <- 15000
l <- vector("list", N)
l2 <- vector("list", N)
sum_mat <- matrix(0, n_players, n_players)
for(z in 1:N){
  l[[z]] <- make_adj_mat(imposed_samps1$relab_grps[z,])
  l2[[z]] <- make_adj_mat(imposed_samps2$relab_grps[z,])
  sum_mat <- l[[z]] + sum_mat
}




dists <- rep(0, 2*N)
for(z in 1:N){
  
  diff <- mean_mat - l[[z]]
  ss <- mean(diff^2)
  dists[z] = ss
}
for(z in 1:N){
  
  diff <- mean_mat - l2[[z]]
  ss <- mean(diff^2)
  dists[z] = ss
}

mean_mat <- sum_mat/(2*N)

image(t(mean_mat))
closest_mat <- which.min(dists)
closest_mat
pnms[which(imposed_samps1$relab_grps[closest_mat, ]==7)]

image(make_adj_mat_grp(imposed_samps1$relab_grps[which.min(dists),]))
table(imposed_samps1$relab_grps[which.min(dists),])
image(l[[which.min(dists)]])
sort(dists)

# belwo this is the two cluster stuff -------------------------------------




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

colMeans(ord_alphs1[which(clust_count_ord == 2),c(2, 169, 336, 503, 670)])
apply(ord_alphs1[which(clust_count_ord == 2),c(2, 169, 336, 503, 670)], 2, sd)
apply(ord_betas1[which(clust_count_ord == 2),c(2, 169, 336, 503, 670)], 2, sd)


apply(ord_alphs1[which(clust_count_ord == 2),c(1, 168, 335, 502, 669)], 2, sd)
apply(ord_betas1[which(clust_count_ord == 2),c(1, 168, 335, 502, 669)], 2, sd)


# cluster 1, chain 

coef_inds <- c(1, 168, 335, 502, 669)

par(mfrow=c(5, 4))
for(j in 1:5){
  i <- coef_inds[j]
  hist(ord_alphs1[which(clust_count_ord == 2),i], main=paste("cluster 1: alpha", j), xlab="")
  hist(ord_betas1[which(clust_count_ord == 2),i], main=paste("cluster 1: beta", j), xlab="")
  hist(ord_alphs1[which(clust_count_ord == 2),i+1], main=paste("cluster 2: alpha", j), xlab="")
  hist(ord_betas1[which(clust_count_ord == 2),i+1], main=paste("cluster 2: beta", j), xlab="")
}


for(j in 1:2){
  alphs1_means <- apply(ord_alphs1[which(clust_count_ord == 2),c(1, 168, 335, 502, 669)], 2, mean)
  alphs1_means2 <- apply(ord_alphs1[which(clust_count_ord == 2),(c(1, 168, 335, 502, 669)+1)], 2, mean)
  
  alphs1_sds <- apply(ord_alphs1[which(clust_count_ord == 2),c(1, 168, 335, 502, 669)], 2, sd)
  alphs1_sds2 <- apply(ord_alphs1[which(clust_count_ord == 2),(c(1, 168, 335, 502, 669)+1)], 2, sd)
  
  
  betas1_sds <- apply(ord_betas1[which(clust_count_ord == 2),c(1, 168, 335, 502, 669)], 2, sd)
  betas1_sds2 <- apply(ord_betas1[which(clust_count_ord == 2),(c(1, 168, 335, 502, 669)+1)], 2, sd)
  
  betas1_means <- apply(ord_betas1[which(clust_count_ord == 2),c(1, 168, 335, 502, 669)], 2, mean)
  betas1_means2 <- apply(ord_betas1[which(clust_count_ord == 2),(c(1, 168, 335, 502, 669)+1)], 2, mean)
  
}


table(imposed_samps1$relab_grps[which(clust_count_ord == 2),])/sum(table(imposed_samps1$relab_grps[which(clust_count_ord == 2),]))
table(imposed_samps2$relab_grps[which(clust_count_ord2 == 2),])

par(mfrow=c(1, 1))
hist(colMeans(imposed_samps1$relab_grps[which(clust_count_ord == 2),]))

image(cor(imposed_samps1$relab_grps[which(clust_count_ord == 2),]))


alphas_summary <- rbind(alphs1_means, alphs1_sds, alphs1_means2, alphs1_sds2)
rownames(alphas_summary) <- c("cluster 1 alpha mean", "cluster 1 alpha sd", "cluster 2 alpha mean", "cluster 2 alpha sd")
betas_summary <- rbind(betas1_means, betas1_sds, betas1_means2, betas1_sds2)
rownames(betas_summary) <- c("cluster 1 beta mean", "cluster 1 beta sd", "cluster 2 beta mean", "cluster 2 beta sd")

hist(betas1[,670])
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
