library(nimble)
library(here)
library(dplyr)
library(bayesplot)
library(parallel)
library(tibble)
library(stringr)
library(tidyr)
library(robustHD)
library(igraph)


load_shots <- readRDS(here::here("/saved_robjs/joined_shots"))
design_shooting <- readRDS(here("/saved_robjs/design_shooting"))

player_mat <- load_shots %>% 
  select("Exp", "Salary") %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(Salary = Salary/1000000) %>% 
  mutate(Exp2 = Exp^2, Salary2 = Salary^2) %>% 
  as.matrix()


player_names <- load_shots %>% dplyr::select(PLAYER_NAME) %>% c()

M <- 30
n_players <- load_shots %>% nrow()
n_zones <- 12
set.seed(225)
mu <- matrix(rnorm(n_players * n_zones), nrow = n_players, ncol = n_zones)
eta <- matrix(rnorm(n_players * n_zones), nrow = n_players, ncol = n_zones)


alphas <- matrix(rnorm(5*M), nrow = M, ncol = 5)
betas <- matrix(rnorm(5*M), nrow = M, ncol = 5)
alpha <- rgamma(1, 2, 1)

clust <- sample(1:10, n_players, replace=TRUE)

rand_eff <- matrix(0, nrow=n_players, ncol=n_zones)
scale_eff <- rep(0, times=n_players)




Y <- load_shots %>% dplyr::select(ends_with("attempt"))
Z <- load_shots %>% dplyr::select(ends_with("pct"))



for(r in 1:nrow(Z)){
  for(c in 1:ncol(Z)){
    if(Z[r, c] == 0){
      Z[r, c] <-  0.001
    } else if (Z[r, c] ==1 ){
      Z[r,c] <- .999
    }
  }
}

logit_Z <- log(Z/(1-Z))

id_5 = diag(10, 5)
id_nz = diag(10, n_zones)
rand_Sig <- id_nz

zero_5 <- rep(0, times=5)
zero_nz <- rep(0, times=12)


MoransI.Basis<-function(X,r,A){
  
  
  n = dim(X)[1]
  
  PsiOper = (diag(n) - X%*%solve(t(X)%*%X)%*%t(X))%*%A%*%(diag(n) - X%*%solve(t(X)%*%X)%*%t(X))
  output2<-eigen(PsiOper)
  Psi = output2$vectors[,1:r]
  
  return(Psi)
}







new_basis <- matrix(0, nrow=12, ncol=12)
ab3c  <- c(1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0)
ab3lc <- c(1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
ab3rc <- c(1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1)
paint <- c(0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0)
lc3   <- c(0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0)
mrc   <- c(1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0)
mrlc  <- c(1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0)
mrl   <- c(0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0)
mrrc  <- c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1)
mrr   <- c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1)
ra    <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0)
rc3   <- c(0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1)

regions <- c("above break 3 center", 
             "above break 3 left center",
             "above break 3 right center",
             "paint",
             "left corner 3",
             "midrange center",
             "midrange left center",
             "midrange left",
             "midrange right center",
             "midrange right",
             "restricted area",
             "right corner 3")

new_basis <- rbind(ab3c, ab3lc, ab3rc, paint, lc3, mrc, mrlc, mrl, mrrc, mrr, ra, rc3)
colnames(new_basis) = rownames(new_basis)
colnames(new_basis) <- regions
rownames(new_basis) <- regions
new_basis == t(new_basis)

new_basis1 <- new_basis - diag(12)

write.csv(data.frame(new_basis1), here("adjacency_basis1.csv"))

#intercept only model
X = matrix(1,12,1)
r = 5
#first nearest neighbor Moran's I
Psi = MoransI.Basis(X,5,new_basis1)

design_shooting1 <- Psi

for(i in 1:5){
  design_shooting1[, i] <- (Psi[, i] - mean(Psi[, i]))/sd(Psi[,i])
}

write.csv(data.frame(design_shooting1), here("moranbasis.csv"))

shots_code <- nimbleCode({

  
  #alpha ~ dgamma(1, 1)
  clust[1:n_players] ~ dCRP(0.1, size=n_players)
  
  for(i in 1:M) {
    betas[i, 1:5] ~ dmnorm(mean=zero_5[1:5], cov=id_5[1:5,1:5])
    alphas[i, 1:5] ~ dmnorm(mean=zero_5[1:5], cov=id_5[1:5,1:5])
  }
  
  for(i in 1:n_zones){
    sigma[i] ~ dinvgamma(.01, .01)
  }
  
  for(player in 1:n_players){
    # prior
    rand_Sig[1:n_zones, 1:n_zones] ~ dinvwish(id_nz[1:n_zones, 1:n_zones], df=15)
    rand_eff[player, 1:n_zones] ~ dmnorm(zero_nz[1:n_zones], rand_Sig[1:n_zones, 1:n_zones])
    scale_eff[player] ~ dnorm(0, sd=10)
  
    mu[player, 1:n_zones] <- design_shooting[1:n_zones, 1:5] %*% betas[clust[player], 1:5] + rand_eff[player, 1:n_zones]
    #mu[player, 1:n_zones] ~ dmnorm(mean=m_mean[player, 1:n_zones], cov=id_nz[1:n_zones, 1:n_zones])
    
    
    for(shot in 1:n_zones){
    
      
      
      y_rate[player, shot] <- exp(mu[player, shot]) 
      Y[player, shot] ~ dpois(y_rate[player, shot])   

    }

    eta[player, 1:n_zones] <- design_shooting[1:n_zones, 1:5] %*% alphas[clust[player], 1:5] + rand_eff[player, 1:n_zones] * scale_eff[player]
    #eta[player, 1:n_zones] ~ dmnorm(e_mean[player,  1:n_zones], cov=id_nz[1:n_zones, 1:n_zones])

    
    for(pct in 1:n_zones){
      lz_mean[player, pct] <- (eta[player, pct])
      logit_Z[player, pct] ~ dnorm(lz_mean[player, pct], var=sigma[pct])
    }
  }
  
})
 

constants <- list(n_players = n_players,
                  n_zones = n_zones,
                  design_shooting = design_shooting1,
                  id_5 = id_5,
                  id_nz = id_nz,
                  zero_5 = zero_5,
                  zero_nz = zero_nz,
                  M=M)


 
data <- list(Y = Y,
             logit_Z = logit_Z)
 
inits <- list(betas = betas,
              alphas = alphas,
              alpha = alpha,
              eta = eta,
              mu = mu,
              sigma = rep(1, times=n_zones),
              clust=clust,
              rand_eff = rand_eff,
              rand_Sig = rand_Sig,
              scale_eff = scale_eff)
 
shots_model <- nimbleModel(shots_code, 
                           constants = constants, 
                           data = data, 
                           inits = inits, 
                           debug=FALSE)
 
shots_mcmc <- buildMCMC(shots_model)
set.seed(226)
mcmc.out <- nimbleMCMC(code = shots_code, constants = constants,
                       data = data, inits = inits,
                       nchains = 2, niter = 20000,
                       summary = TRUE, WAIC = FALSE,
                       monitors = c('alphas','betas',
                                     'mu', 'eta', 'clust',
                                    'scale_eff', 'rand_eff'))#,
                                   # 'player_alph', 'player_beta'))
# 
# 

saveRDS(mcmc.out, here("/saved_robjs/samps_moran_randeff_fixedalphpt1"))

player_name <- load_shots$PLAYER_NAME
param_nm <- rownames(mcmc.outiw$summary$all.chains)
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


mcmcout_median_ab <- as_tibble(mcmc.outiw$summary$all.chains) %>% 
  add_column(param_nm, .before="Mean") %>% 
  filter(str_detect(param_nm, "^beta|^alpha|^player")) %>% 
  mutate(param = str_extract(param_nm, "(.*?)(?=\\[)"))

view(mcmcout_tib)
View(mcmcout_median_ab)
mcmcout_tib
as.numeric(str_extract("[23,", "(?<=\\[)(.*?)(?=,)"))

plot(mcmc.outiw$samples$chain1[1:10000, 180])

write.csv(mcmcout_median_ab2, "mcmc_medians_ab2.csv")
# unsure if this works (paralllel) ----------------------------------------


shots_const <- constants
shots_inits <- inits

this_cluster <- makeCluster(4)


run_MCMC_allcode <- function(seed, data) {
  library(nimble)
  
  myModel <- nimbleModel(code = shots_code,
                         data = data$data,
                         constants = data$const,
                         inits = data$inits)
  
  CmyModel <- compileNimble(myModel)
  
  myMCMC <- buildMCMC(CmyModel)
  CmyMCMC <- compileNimble(myMCMC)
  
  results <- runMCMC(CmyMCMC, niter = 10000, setSeed = seed)
  
  return(results)
}
chain_output <- parLapply(cl = this_cluster, X = 1:4, 
                          fun = run_MCMC_allcode, 
                          MoreArgs = list(data=data, const=constants, inits=inits))
stopCluster(this_cluster)
par(mfrow = c(2,2))
for (i in 1:4) {
  this_output <- chain_output[[i]]
  plot(this_output[,"b"], type = "l", ylab = 'b')
}





plot(mcmc.out10$samples$chain1[,3000:1000])
view(mcmc.out10$samples$chain1)
View(mcmc.out$summary$all.chains)


# 
# 
# stan_data <- list(
#   n_players = n_players,
#   n_zones = n_zones,
#   Y = Y,
#   Z = Z,
#   id_7 = id_7/10,
#   id_nz = id_nz/10,
#   design_shooting = design_shooting,
#   zero_7 = zero_7,
#   zero_12 = zero_12
# )
# 
# fit <- stan(
#   file = "shots_model_stan.stan",
#   data = stan_data,
#   warmup = 1000,
#   iter = 4000,
#   chains = 1,
#   seed = 225,
#   # control the sampler's behavior
#   control = list(adapt_delta = 0.9,
#                  max_treedepth = 12)
# )



mcmc.outiw$samples$chain1[ , grep('z', colnames(mcmc.outiw$samples$chain1))]


dim(mcmc.out$samples$chain2[ , grep('z', colnames(mcmc.out$samples$chain1))])
zsamp1 <- mcmc.out$samples$chain1[ , grep('clust', colnames(mcmc.out$samples$chain1))]
zsamp2 <- mcmc.out$samples$chain2[ , grep('clust', colnames(mcmc.out$samples$chain2))]
zsamptot <- rbind(zsamp1, zsamp2)
clust_count <- rep(0, times=30000)
for(i in 1:15000){
  clust_count[i] <- length(unique(zsamp1[i+5000, ]))
  clust_count[i+ 15000] <- length(unique(zsamp2[i+5000, ]))
}
hist(clust_count[8001:16000])
table(clust_count[1:8000])

hist(clust_count[15001:30000])

mean(clust_count)
unique(clust_count)
table(clust_count)
c(table(zsamp1))
c(table(zsamp2))



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

table(zsamp1[6395,])

image(make_adj_mat_grp(zsamp1[6395,]))
max(zsamp1[6395,])
make_adj_mat_grp(zsamp1[6395,])

sum(zsamp1[6395,]==1)


test_mat <- make_adj_mat(zsamp1[1000,])




test_graph <- graph_from_adjacency_matrix(l[13060])
plot(test_graph)
max(zsamp1[1000,])


image(l[[13149]])

l[[1302]][40:50, 40:50]

mean(l[[1302]])


mean(clust_count)

N <- 16000
l <- vector("list", N)

sum_mat <- matrix(0, n_players, n_players)
for(z in 1:8000){
  l[[z]] <- make_adj_mat(zsamp1[z+2000, ])
  l[[z + 8000]] <- make_adj_mat(zsamp2[z+2000, ])
  
  sum_mat <- l[[z]] + l[[z + 8000]] + sum_mat
}


mean_mat <- sum_mat/16000

mean_mat[1:10, 1:10]

image(mean_mat)

close_mat <- l[[1]]

min_dist <- 10000
dists <- rep(0, 16000)
for(z in 1:16000){
  
  diff <- mean_mat - l[[z]]
  ss <- mean(diff^2)
  dists[z] = ss
}


# closest is at 13060; converts to 13060-8000+2000
mean(zsamp2[7060,] == l[[13060]])

image(l[[13060]])
sum(l[[13060]])/(167)

dim(test_mat)
library(ggplot2)
ggplot(data.frame(test_mat), aes(x = from, y = to, fill = group)) +
  geom_raster() +
  theme_bw() +
  # Because we need the x and y axis to display every node,
  # not just the nodes that have connections to each other,
  # make sure that ggplot does not drop unused factor levels
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1,
    # Hide the legend (optional)
    legend.position = "none")

write.csv(mean_mat, here("mean_adj_mat.csv"))
