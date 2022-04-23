library(tidyr)
library(tidyverse)
library(here)
library(plyr)
#library(BasketballAnalyzeR)
library(cowplot)
library(grid)
#library(Rmisc)
library(ggpubr)
library(jpeg)
library(RCurl)
library(gridExtra)
library(xtable)
library(openxlsx)

samples <- readRDS(here::here("saved_robjs/1819/samps_moran_randeff_alphapt25sigma2525"))
load_shots <- readRDS(here::here("saved_robjs/1819/joined_shots"))
rezoned_shots <- readRDS(here::here("saved_robjs/1819/rezoned_shots_1819"))


courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

n_players <- dim(load_shots)[1]

# samples <- mcmc.out

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

N <- 15000
l <- vector("list", N)
l2 <- vector("list", N)
sum_mat <- matrix(0, n_players, n_players)
for(z in 1:N){
  if(z/1000 == 0){
    print(z)
  }
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


pnms <- load_shots$PLAYER_NAME
image(t(mean_mat))
closest_mat <- which.min(dists)
closest_mat


table(imposed_samps1$relab_grps[closest_mat,])
num_samps = dim(imposed_samps2$relab_grps)[1]
num_clusts = c()
for (i in 1:num_samps){
  num_clusts[i] = dim(table(imposed_samps1$relab_grps[i,]))
}
for (i in 1:num_samps){
  num_clusts[i+num_samps] = dim(table(imposed_samps2$relab_grps[i,]))
}
hist(num_clusts)
var(num_clusts)

length(imposed_samps1)

load_shots

ts.plot(imposed_samps2$ord_coefs[,67])

clust_shots <- load_shots %>% 
  add_column(cluster = imposed_samps1$relab_grps[closest_mat,]) 

# for each cluster gather the names
cluster_assignments = list()
cluster_stats = list()
for (c in 1:dim(clust_shots %>% dplyr::select(cluster) %>% unique())[1]){
  #cluster_assignements <- append(cluster_assignments, 
  #                              clust_shots %>% 
  #                                filter(cluster==c) %>% 
  #                              dplyr::select(PLAYER_NAME) %>% 
  #                                c())
  cluster_assignments[[paste0("cluster", c)]] = clust_shots %>% 
    filter(cluster==c) %>% 
    dplyr::select(PLAYER_NAME) %>% 
    c()
  cluster_stats[[paste0("cluster", c)]] = clust_shots %>% 
    filter(cluster==c)
  
}
write.xlsx(cluster_assignments, here::here("data/1920/clust_assignments.xls"))
write.xlsx(cluster_stats, here::here("data/1920/clust_summary.xls"))

comp_3pct <- function(all_shots){
  atts <- all_shots %>% 
    dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*3)(?=.*attempt).*$")==TRUE))
  
  pcts <- all_shots %>% 
    dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*3)(?=.*pct).*$")==TRUE))
  
  return(list(sum(atts * pcts)/sum(atts), sum(atts)/nrow(atts)))
}

comp_fgpct <- function(all_shots){
  atts <- all_shots %>% 
    dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*attempt).*$")==TRUE)) %>% 
    dplyr::select(-total_attempts)
  
  pcts <- all_shots %>% 
    dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*pct).*$")==TRUE))
  
  return(list(sum(atts * pcts)/sum(atts), sum(atts)/nrow(atts)))
}


comp_zone_pct <- function(all_shots, raw_shots){
  atts <- all_shots %>% 
    dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*attempt).*$")==TRUE)) %>% 
    dplyr::select(-total_attempts)
  

  pcts <- all_shots %>% 
    dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*pct).*$")==TRUE))
  
  cart_zone_ctrs <- raw_shots %>% 
    dplyr::filter(SHOT_ZONE_BASIC!="Backcourt") %>% 
    group_by(zone) %>% 
    dplyr::summarize(mean_x = mean(LOC_X), 
              mean_y = mean(LOC_Y))
  
  return(list(pcts=colSums(atts*pcts)/colSums(atts), ctr=cart_zone_ctrs))
}

all_shots <- clust_shots
atts <- all_shots %>% 
  dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*attempt).*$")==TRUE)) %>% 
  dplyr::select(-total_attempts) %>% 
  colSums()

atts

three_pct <- rep(0, times = max(clust_shots$cluster))
three_att <- rep(0, times = max(clust_shots$cluster))
total_pct <- rep(0, times = max(clust_shots$cluster))
total_att <- rep(0, times = max(clust_shots$cluster))

for(cl in 1:max(clust_shots$cluster)){
  pct_att3 <- comp_3pct(clust_shots %>% dplyr::filter(cluster==cl))
  
  three_pct[cl] <- pct_att3[[1]]
  three_att[cl] <- pct_att3[[2]]
  
  pct_att <- comp_fgpct(clust_shots %>% dplyr::filter(cluster==cl))
  
  total_pct[cl] <- pct_att[[1]]
  total_att[cl] <- pct_att[[2]]
}

three_pct_pos <- rep(0, times = 5)
three_att_pos <- rep(0, times = 5)
total_pct_pos <- rep(0, times = 5)
total_att_pos <- rep(0, times = 5)

three_pct_posall <- rep(0, times = 5)
three_att_posall <- rep(0, times = 5)
total_pct_posall <- rep(0, times = 5)
total_att_posall <- rep(0, times = 5)
positions <- c("PG", "SG", "SF", "PF", "C")
for(ps in 1:5){
  cl <- positions[ps]
  
  pct_att3 <- comp_3pct(clust_shots %>% dplyr::filter(Pos==cl))
  
  three_pct_pos[ps] <- pct_att3[[1]]
  three_att_pos[ps] <- pct_att3[[2]]
  
  pct_att3 <- comp_3pct(all_joined %>% dplyr::filter(Pos==cl))
  
  three_pct_posall[ps] <- pct_att3[[1]]
  three_att_posall[ps] <- pct_att3[[2]]
  
  
  pct_att <- comp_fgpct(clust_shots %>% dplyr::filter(Pos==cl))
  
  total_pct_pos[ps] <- pct_att[[1]]
  total_att_pos[ps] <- pct_att[[2]]

  pct_att <- comp_fgpct(all_joined %>% dplyr::filter(Pos==cl))
  
  total_pct_posall[ps] <- pct_att[[1]]
  total_att_posall[ps] <- pct_att[[2]]
}  




clust_summary <- clust_shots %>% 
  group_by(cluster) %>% 
  dplyr::summarise(Ht_mn = mean(Ht_in), 
                   Ht_sd = sd(Ht_in),
                   Wt_mn = mean(Wt), 
                   Wt_sd = sd(Wt),
                   clust_size=n(), 
                   sal_mn = mean(Salary/1000000),
                   sal_sd = sd(Salary/1000000),
                   exp_mn = mean(Exp),
                   exp_sd = sd(Exp),
                   mode_position = names(which.max(table(Pos)))) %>% 
  add_column(three_pct = three_pct) %>% 
  add_column(three_att = three_att) %>% 
  add_column(total_pct = total_pct) %>% 
  add_column(total_att = total_att)

clust_summary


pos_summary <- clust_shots %>% 
  group_by(Pos) %>% 
  dplyr::summarise(clust_size=n(), 
                   Ht_mn = mean(Ht_in), 
                   Ht_sd = sd(Ht_in),
                   Wt_mn = mean(Wt), 
                   Wt_sd = sd(Wt),
                   sal_mn = mean(Salary/1000000),
                   sal_sd = sd(Salary/1000000),
                   exp_mn = mean(Exp),
                   exp_sd = sd(Exp),
                   mode_position = names(which.max(table(Pos)))) %>% 
  add_column(three_pct = three_pct_pos) %>% 
  add_column(three_att = three_att_pos) %>% 
  add_column(total_pct = total_pct_pos) %>% 
  add_column(total_att = total_att_pos)

pct_att3 <- comp_3pct(clust_shots)
pct_att <- comp_fgpct(clust_shots)

marg_stats <- c("all", 167, mean(clust_shots$Ht_in), sd(clust_shots$Ht_in), 
  mean(clust_shots$Wt), sd(clust_shots$Wt), 
  mean(clust_shots$Salary/1000000), sd(clust_shots$Salary/1000000),
  mean(clust_shots$Exp), sd(clust_shots$Exp), 
  "",
  pct_att3_all[[1]], pct_att3_all[[2]], pct_att_all[[1]], pct_att_all[[2]])

pos_summ_combo <- rbind(pos_summary, marg_stats)



all_pos_summary <- all_joined %>% 
  group_by(Pos) %>% 
  dplyr::summarise(clust_size=n(), 
                   Ht_mn = mean(Ht_in), 
                   Ht_sd = sd(Ht_in),
                   Wt_mn = mean(Wt), 
                   Wt_sd = sd(Wt),
                   sal_mn = mean(Salary/1000000),
                   sal_sd = sd(Salary/1000000),
                   exp_mn = mean(Exp),
                   exp_sd = sd(Exp),
                   mode_position = names(which.max(table(Pos)))) %>% 
  add_column(three_pct = three_pct_posall) %>% 
  add_column(three_att = three_att_posall) %>% 
  add_column(total_pct = total_pct_posall) %>% 
  add_column(total_att = total_att_posall)

pct_att3all <- comp_3pct(all_joined)
pct_attall <- comp_fgpct(all_joined)

marg_stats <- c("all", 470, mean(all_joined$Ht_in), sd(all_joined$Ht_in), 
                mean(all_joined$Wt), sd(all_joined$Wt), 
                mean(all_joined$Salary/1000000), sd(all_joined$Salary/1000000),
                mean(all_joined$Exp), sd(all_joined$Exp), 
                "",
                pct_att3all[[1]], pct_att3all[[2]], pct_attall[[1]], pct_attall[[2]])

pos_summ_combo_all <- rbind(all_pos_summary, marg_stats)


clust_dem <- clust_summary %>% select(1:10)
clust_sh <- clust_summary %>% select(c(1, 12:15))

write.csv(clust_summary, here::here("/data/FINALcluster_summary.csv"))

write.csv(clust_shots %>% arrange(cluster) %>% dplyr::select(c(PLAYER_NAME, cluster)), here::here("data/FINALplayer_assignments.csv"))

load_shots %>% dplyr::select(which(str_detect(colnames(load_shots), "attempt|pct")==TRUE)) %>% names() 

clust_shots %>% arrange(cluster) %>% dplyr::select(c(PLAYER_NAME, cluster))

image(make_adj_mat_grp(imposed_samps1$relab_grps[which.min(dists),]))
table(imposed_samps1$relab_grps[which.min(dists),])
image(l[[which.min(dists)]])
sort(dists)

pnms[which(imposed_samps1$relab_grps[closest_mat, ]==1)]


clust_to_row <- function(nms){
  temp <- paste(nms, '& ')
  paste(temp, collapse = "")
}
clust_to_row(pnms[which(imposed_samps1$relab_grps[closest_mat, ]==1)])

clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(!(cluster %in% c(1, 4, 5, 10))) %>% 
  group_by(cluster) %>% 
  ggplot(aes(x=LOC_X, y=LOC_Y, color=zone, shape=EVENT_TYPE)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_density_2d() + 
  facet_wrap(~cluster) + 
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size = 12),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

shotslocs <- ggplot(data=sample_n(shots, 1000), aes(x=LOC_X, y=LOC_Y, color=SHOT_ZONE_AREA)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_density_2d()


shotslocs

rezoned_clusts <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster, Pos)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC != "Backcourt")





ggplot(data=dens_data1,aes(x=LOC_X, y=LOC_Y)) +
  annotation_custom(court, -250, 250, -52, 418) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_density_2d()+
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_point(data=data1, aes(x=mean_x, y=mean_y, color=zone)) +
  geom_text(aes(colour = zone, label = labs1), vjust = 1.2, size = 4, show.legend=F) 


# silly ggplot ------------------------------------------------------------

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

region_labs <- c("AB3 Ctr", "AB3 L", "AB3 R", "Paint", "LC3", "MR Ctr", 
                 "MR LCtr", "MR L", "MR RCtr", "MR R", "RA", "RC3")
region_full <- c("Above Break 3 Ctr",
                 "Above Break 3 Left",
                 "Above Break 3 Right",
                 "Left Corner 3",
                 "Mid-Range Ctr",
                 "Mid-Range Left Ctr",
                 "Mid-Range Left",
                 "Mid-Range Right Ctr",
                 "Restricted Area (RA)",
                 "Paint (non-RA)",
                 "Mid-Range Right",
                 "Right Corner 3")

# precovid inds
clust_inds <- c(2, 3, 6, 7, 8, 9, 11, 12, 13)

# covid season inds
clust_inds <- 1:7
dens_color <- "lightsteelblue2"
tsize <- 3
gpsize <- 1
gpshape <- 16

spaceLegend <- 0.75

cl <- clust_inds[1]
test1 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

dens_data1 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

data1 <- data.frame(test1$ctr[2:13,])
data1$zone <- region_full

labs1 <- paste0(round(test1$pcts, digits=4)*100,'%')

p0 <- ggplot(data=data1, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Zones")) +
  #guides(alpha = FALSE, size = FALSE, 
  #       color = guide_legend(override.aes = list(size = gpsize), 
  #                            shape=as.character(1:12))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone), show.legend=T) + 
  scale_color_manual(labels = region_full, name="Zone", values=gg_color_hue(12)) +
  #scale_x_continuous(breaks=c(x1,x2,x3), labels=as.character()) + 
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1))


p1 <- ggplot(data=data1, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) + 
  scale_color_manual(labels = labs1, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data1,aes(x=-LOC_X, y=LOC_Y),color=dens_color, show.legend = F)


cl <- clust_inds[2]

test2 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

data2 <- data.frame(test2$ctr[2:13,])
data2$zone <- region_full

dens_data2 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)



labs2 <- paste0(round(test2$pcts, digits=4)*100,'%')

p2 <- ggplot(data=data2, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs2, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data2,aes(x=-LOC_X, y=LOC_Y),color=dens_color)


cl <- clust_inds[3]
test3 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

data3 <- data.frame(test3$ctr[2:13,])
data3$zone <- region_full

labs3 <- paste0(round(test3$pcts, digits=4)*100,'%')

dens_data3 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p3 <- ggplot(data=data3, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs3, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data3,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[4]
test4 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

data4 <- data.frame(test4$ctr[2:13,])
data4$zone <- region_full

labs4 <- paste0(round(test4$pcts, digits=4)*100,'%')



dens_data4 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p4 <- ggplot(data=data4, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs4, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data4,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[5]
test5 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

data5 <- data.frame(test5$ctr[2:13,])
data5$zone <- region_full
labs5 <- paste0(round(test5$pcts, digits=4)*100,'%')


dens_data5 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p5 <- ggplot(data=data5, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
 # geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs5, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data5,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[6]
test6 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

data6 <- data.frame(test6$ctr[2:13,])
data6$zone <- region_full

labs6 <- paste0(round(test6$pcts, digits=4)*100,'%')


dens_data6 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p6 <- ggplot(data=data6, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs6, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data6,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[7]
test7 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

data7 <- data.frame(test7$ctr[2:13,])
data7$zone <- region_full

labs7 <- paste0(round(test7$pcts, digits=4)*100,'%')

dens_data7 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)


p7 <- ggplot(data=data7, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs7, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data7,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[8]
test8 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

data8 <- data.frame(test8$ctr[2:13,])
data8$zone <- region_full

labs8 <- paste0(round(test8$pcts, digits=4)*100,'%')


dens_data8 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p8 <- ggplot(data=data8, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs8, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data8,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[9]
test9 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                      rezoned_clusts %>% dplyr::filter(cluster==cl))

data9 <- data.frame(test9$ctr[2:13,])
data9$zone <- region_full

labs9 <- paste0(round(test9$pcts, digits=4)*100,'%')



dens_data9 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p9 <- ggplot(data=data9, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs9, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data9,aes(x=-LOC_X, y=LOC_Y), color=dens_color)



# end silly ggplot --------------------------------------------------------


ten_plot <- ggarrange(p0, p1, p2, p3, 
                       p4, p5, p6,
                       p7, p8, p9,
                       nrow=2, ncol=5, common.legend=F, align="h")

nine_plot <- ggarrange(p1, p2, p3, 
          p4, p5, p6,
          p7, p8, p9,
          nrow=3, ncol=3, common.legend=F)
annotate_figure(nine_plot, 
                top = text_grob("Shooting Density and Zone FG%", 
                                color = "black", face = "bold", size = 14))


annotate_figure(ten_plot, 
                top = text_grob("Shooting Density and Zone FG%", 
                                color = "black", face = "bold", size = 14))


seven_plot <- ggarrange(p1, p2, p3, 
                      p4, p5, p6,
                      p7,
                      nrow=4, ncol=2, common.legend=F, align="h")

annotate_figure(seven_plot, 
                top = text_grob("Shooting Density and Zone FG%", 
                                color = "black", face = "bold", size = 14))




# position plots ----------------------------------------------------------

# silly ggplot ------------------------------------------------------------

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

region_labs <- c("AB3 Ctr", "AB3 L", "AB3 R", "Paint", "LC3", "MR Ctr", 
                 "MR LCtr", "MR L", "MR RCtr", "MR R", "RA", "RC3")
region_full <- c("Above Break 3 Ctr",
                 "Above Break 3 Left",
                 "Above Break 3 Right",
                 "Left Corner 3",
                 "Mid-Range Ctr",
                 "Mid-Range Left Ctr",
                 "Mid-Range Left",
                 "Mid-Range Right Ctr",
                 "Restricted Area (RA)",
                 "Paint (non-RA)",
                 "Mid-Range Right",
                 "Right Corner 3")

position_names <- c("PG", "SG", "SF", "PF", "C")

dens_color <- "lightsteelblue2"
tsize <- 3
gpsize <- 1
gpshape <- 16

spaceLegend <- 0.75

test0 <- comp_zone_pct(clust_shots,
                       rezoned_clusts)

dens_data0 <- clust_shots %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(zone != "Above the Break 3 Back Court(BC)") %>% 
  data.frame()

data0 <- data.frame(test0$ctr[2:13,])
data0$zone <- region_full

labs0 <- paste0(round(test0$pcts, digits=4)*100,'%')


p0 <- ggplot(data=dens_data0, aes(x=-LOC_X, y=LOC_Y, color=zone)) +
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point( show.legend=T, shape="." ) + 
  geom_text(data=data0, aes(x=-mean_x, y=-mean_y, label = region_full), 
            vjust = 1.5, size = 5, show.legend=F) +
  ggtitle(paste("All Shots")) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1))# +
 # geom_density_2d(data=dens_data0,aes(x=-LOC_X, y=LOC_Y),color=dens_color, show.legend = F)

p0



region_full <- c("Above Break 3 Ctr",
                 "Above Break 3 Left",
                 "Above Break 3 Right",
                 "Paint (non-RA)",
                 "Left Corner 3",
                 "Mid-Range Ctr",
                 "Mid-Range Left Ctr",
                 "Mid-Range Left",
                 "Mid-Range Right Ctr",
                 "Mid-Range Right",
                 "Restricted Area",
                 "Right Corner 3")



p00 <- ggplot(data=data0, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  ggtitle("All Player Shots") +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  #geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) + 
  #scale_color_manual(labels = labs1, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_point(data=dens_data0,aes(x=-LOC_X, y=LOC_Y, color=zone), show.legend = F, shape='.') +
  geom_text(aes(label = region_full), color = 'grey3', size = 5, show.legend=F)

p00































ps <- position_names[1]
test1 <- comp_zone_pct(clust_shots %>% dplyr::filter(Pos==ps),
                       rezoned_clusts %>% dplyr::filter(Pos==ps))

dens_data1 <- clust_shots %>% 
  arrange(Pos) %>% 
  dplyr::select(c(PLAYER_NAME, Pos)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(Pos==ps)

data1 <- data.frame(test1$ctr[2:13,])
data1$zone <- region_full

labs1 <- paste0(round(test1$pcts, digits=4)*100,'%')


p1 <- ggplot(data=data1, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Position:", ps)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) + 
  scale_color_manual(labels = labs1, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data1,aes(x=-LOC_X, y=LOC_Y),color=dens_color, show.legend = F)

ps <- position_names[2]
test2 <- comp_zone_pct(clust_shots %>% dplyr::filter(Pos==ps),
                       rezoned_clusts %>% dplyr::filter(Pos==ps))

dens_data2 <- clust_shots %>% 
  arrange(Pos) %>% 
  dplyr::select(c(PLAYER_NAME, Pos)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(Pos==ps)

data2 <- data.frame(test2$ctr[2:13,])
data2$zone <- region_full

labs2 <- paste0(round(test2$pcts, digits=4)*100,'%')


p2 <- ggplot(data=data2, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Position:", ps)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) + 
  scale_color_manual(labels = labs2, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data2,aes(x=-LOC_X, y=LOC_Y),color=dens_color, show.legend = F)

ps <- position_names[3]
test3 <- comp_zone_pct(clust_shots %>% dplyr::filter(Pos==ps),
                       rezoned_clusts %>% dplyr::filter(Pos==ps))

dens_data3 <- clust_shots %>% 
  arrange(Pos) %>% 
  dplyr::select(c(PLAYER_NAME, Pos)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(Pos==ps)

data3 <- data.frame(test3$ctr[2:13,])
data3$zone <- region_full

labs3 <- paste0(round(test3$pcts, digits=4)*100,'%')


p3 <- ggplot(data=data3, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Position:", ps)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) + 
  scale_color_manual(labels = labs3, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data3,aes(x=-LOC_X, y=LOC_Y),color=dens_color, show.legend = F)

p3

ps <- position_names[4]
test4 <- comp_zone_pct(clust_shots %>% dplyr::filter(Pos==ps),
                       rezoned_clusts %>% dplyr::filter(Pos==ps))

dens_data4 <- clust_shots %>% 
  arrange(Pos) %>% 
  dplyr::select(c(PLAYER_NAME, Pos)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(Pos==ps)

data4 <- data.frame(test4$ctr[2:13,])
data4$zone <- region_full

labs4 <- paste0(round(test4$pcts, digits=4)*100,'%')


p4 <- ggplot(data=data4, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Position:", ps)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) + 
  scale_color_manual(labels = labs4, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data4,aes(x=-LOC_X, y=LOC_Y),color=dens_color, show.legend = F)


p4

ggarrange(p00, p4)

gg_color_hue(12)

ps <- position_names[5]
test5 <- comp_zone_pct(clust_shots %>% dplyr::filter(Pos==ps),
                       rezoned_clusts %>% dplyr::filter(Pos==ps))

dens_data5 <- clust_shots %>% 
  arrange(Pos) %>% 
  dplyr::select(c(PLAYER_NAME, Pos)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(Pos==ps)

data5 <- data.frame(test5$ctr[2:13,])
data5$zone <- region_full

labs5 <- paste0(round(test5$pcts, digits=4)*100,'%')


p5 <- ggplot(data=data5, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Position:", ps)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) + 
  scale_color_manual(labels = labs5, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data5,aes(x=-LOC_X, y=LOC_Y),color=dens_color, show.legend = F)

p5

# end silly ggplot --------------------------------------------------------



posplot <- ggarrange( p1, p2, p3, p4, nrow=2, ncol=2)

annotate_figure(posplot, 
                top = text_grob("Shooting Density and Zone FG%", 
                                color = "black", face = "bold", size = 14))



# end position plots ------------------------------------------------------


zsamp1[1,]

clust_count <- rep(0, times=30000)
for(i in 1:15000){
  clust_count[i] <- length(unique(zsamp1[i+5000,]))
  clust_count[i+15000] <- length(unique(zsamp2[i+5000,]))
}



ggplot(data = data.frame(clust_count), aes(x=clust_count) ) + 
  geom_histogram() +
  ggtitle("Histogram of Number of Clusters in Each Sample") +
  xlab("Clusters in Sample") +
  ylab("Count")+
  theme_bw()

mean(clust_count)
sd(clust_count)




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
