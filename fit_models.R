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


load_shots <- readRDS(here("/saved_robjs/joined_shots"))
design_shooting <- readRDS(here("/saved_robjs/design_shooting"))

player_mat <- load_shots %>% 
  select("Exp", "Salary") %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(Salary = Salary/1000000) %>% 
  mutate(Exp2 = Exp^2, Salary2 = Salary^2) %>% 
  as.matrix()


player_names <- load_shots %>% select(PLAYER_NAME) %>% c()

M <- 167
n_players <- load_shots %>% nrow()
n_zones <- 12
set.seed(225)
mu <- matrix(rnorm(n_players * n_zones), nrow = n_players, ncol = n_zones)
eta <- matrix(rnorm(n_players * n_zones), nrow = n_players, ncol = n_zones)


alphas <- matrix(rnorm(5*M), nrow = M, ncol = 5)
betas <- matrix(rnorm(5*M), nrow = M, ncol = 5)
alpha <- rgamma(1, 3, 1)


z <- rCRP(1, alpha, n_players)


Y <- load_shots %>% dplyr::select(ends_with("attempt"))
Z <- load_shots %>% dplyr::select(ends_with("pct"))

id_5 = diag(10, 5)
id_nz = diag(10, n_zones)
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

new_basis <- rbind(ab3c, ab3lc, ab3rc, paint, lc3, mrc, mrlc, mrl, mrrc, mrr, ra, rc3)
colnames(new_basis) = rownames(new_basis)
new_basis == t(new_basis)

#intercept only model
X = matrix(1,12,1)
r = 5
#first nearest neighbor Moran's I
Psi = MoransI.Basis(X,5,new_basis)

design_shooting1 <- Psi

for(i in 1:5){
  design_shooting1[, i] <- (Psi[, i] - mean(Psi[, i]))/sd(Psi[,i])
}


shots_code <- nimbleCode({

  
  alpha ~ dgamma(3, 1)
  z[1:n_players] ~ dCRP(alpha, size=n_players)
  
  for(i in 1:M) {
    betas[i, 1:5] ~ dmnorm(mean=zero_5[1:5], cov=id_5[1:5,1:5])
    alphas[i, 1:5] ~ dmnorm(mean=zero_5[1:5], cov=id_5[1:5,1:5])
  }
  
  for(player in 1:n_players){
    # prior
    
  
    m_mean[player, 1:n_zones] <- design_shooting[1:n_zones, 1:5] %*% betas[z[player], 1:5] 
    mu[player, 1:n_zones] ~ dmnorm(mean=m_mean[player, 1:n_zones], cov=id_nz[1:n_zones, 1:n_zones])
    
    
    for(shot in 1:n_zones){
    
      
      
      y_rate <- exp(mu[player, shot]) 
      Y[player, shot] ~ dpois(y_rate)   

    }

    e_mean[player, 1:n_zones] <- design_shooting[1:n_zones, 1:5] %*% alphas[z[player], 1:5] 
    eta[player, 1:n_zones] ~ dmnorm(e_mean[player,  1:n_zones], cov=id_nz[1:n_zones, 1:n_zones])

    
    for(pct in 1:n_zones){
      lz_mean <- (eta[player, pct])
      logit(Z[player, pct]) ~ dnorm(lz_mean, sd=10)
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
             Z = Z)
 
inits <- list(betas = betas,
              alphas = alphas,
              alpha = alpha,
              eta = eta,
              mu = mu,
              z=z)
 
shots_model <- nimbleModel(shots_code, 
                           constants = constants, 
                           data = data, 
                           inits = inits, 
                           debug=FALSE)
 
shots_mcmc <- buildMCMC(shots_model)
set.seed(226)
mcmc.out <- nimbleMCMC(code = shots_code, constants = constants,
                       data = data, inits = inits,
                       nchains = 2, niter = 10000,
                       summary = TRUE, WAIC = TRUE,
                       monitors = c('alphas','betas',
                                     'mu', 'eta', 'z'))#,
                                   # 'player_alph', 'player_beta'))
# 
# 

saveRDS(mcmc.out, here("/saved_robjs/samps_moran3"))

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
zsamp1 <- mcmc.out$samples$chain1[ , grep('z', colnames(mcmc.out$samples$chain1))]
zsamp2 <- mcmc.out$samples$chain2[ , grep('z', colnames(mcmc.out$samples$chain2))]
zsamptot <- rbind(zsamp1, zsamp2)
clust_count <- rep(0, times=16000)
for(i in 1:8000){
  clust_count[i] <- max(zsamp1[i+2000, ])
  clust_count[i+ 8000] <- max(zsamp2[i+2000, ])
}
hist(clust_count)


mean(clust_count)
unique(clust_count)
table(clust_count)

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
