library(nimble)
library(here)
library(dplyr)
library(rstan)
library(bayesplot)
library(parallel)
library(tibble)
library(stringr)
library(tidyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load_shots <- readRDS(here("/saved_robjs/joined_shots"))
design_shooting <- readRDS(here("/saved_robjs/design_shooting"))

player_mat <- load_shots %>% 
  select("Exp", "Salary") %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(Salary = Salary/1000000) %>% 
  mutate(Exp2 = Exp^2, Salary2 = Salary^2) %>% 
  as.matrix()

n_players <- load_shots %>% nrow()
n_zones <- 12

mu <- matrix(0, nrow = n_players, ncol = n_zones)
eta <- matrix(0, nrow = n_players, ncol = n_zones)
scale_eff <- matrix(0, nrow = n_players)

omega <- matrix(0, nrow = n_players, ncol = n_zones)

alphas <- matrix(0, nrow = n_players, ncol = 7)
betas <- matrix(0, nrow = n_players, ncol = 7)

player_alph <- rep(0, times=4)
player_beta <- rep(0, times=4)


Y <- load_shots %>% dplyr::select(ends_with("attempt"))
Z <- load_shots %>% dplyr::select(ends_with("pct"))

id_7 = diag(100, 7)
id_nz = diag(100, n_zones)
zero_7 <- rep(0, times=7)
zero_12 <- rep(0, times=12)

id_7 <- diag(7)
id_nz <- diag(n_zones)
zero_7 <- rep(0, times=7)
zero_nz <- rep(0, times=n_zones)

shots_code <- nimbleCode({

  player_alph[1:4] ~ dmnorm(zero_7[1:4], cov=id_7[1:4,1:4])
  player_beta[1:4] ~ dmnorm(zero_7[1:4], cov=id_7[1:4,1:4])
  
  for(player in 1:n_players){
    # prior
    betas[player, 1:7] ~ dmnorm(zero_7[1:7], cov=id_7[1:7,1:7])
    alphas[player, 1:7] ~ dmnorm(zero_7[1:7], cov=id_7[1:7,1:7])
    scale_eff[player, 1] ~ dnorm(0, 100)
    
    
    omega[player, 1:n_zones] ~ dmnorm(zero_nz[1:n_zones], cov=id_nz[1:n_zones,1:n_zones])

    
  
    m_mean[1:n_zones, player] <- design_shooting[1:n_zones, 1:7] %*% betas[player, 1:7] 
    mu[player, 1:n_zones] ~ dmnorm(m_mean[1:n_zones, 1], id_nz[1:n_zones, 1:n_zones])
    
    rate_adj <- (player_mat[player, 1:4]%*%player_beta[1:4])[1,1]  
    
    for(shot in 1:n_zones){
    
      
      
      y_rate <- exp(mu[player, shot] + rate_adj + scale_eff[player, 1] * omega[player, shot])
      Y[player, shot] ~ dpois(y_rate)   

    }

    e_mean[1:n_zones, player] <- design_shooting[1:n_zones, 1:7] %*% alphas[player, 1:7] 
    eta[player, 1:n_zones] ~ dmnorm(e_mean[1:n_zones, 1], id_nz[1:n_zones, 1:n_zones])
    mean_adj <- (player_mat[player, 1:4]%*%player_alph[1:4])[1,1]  
    
    for(pct in 1:n_zones){
      
      lz_mean <- (eta[player, pct] + mean_adj + omega[player, pct])
      logit(Z[player, pct]) ~ dnorm(lz_mean, 10)
    }
  }

})
 


constants <- list(n_players = n_players,
                  n_zones = n_zones,
                  design_shooting = design_shooting,
                  id_7 = id_7,
                  id_nz = id_nz,
                  zero_7 = zero_7,
                  zero_nz = zero_nz,
                  player_mat = player_mat)


 
data <- list(Y = Y * 10,
             Z = Z)
 
inits <- list(betas = betas,
              alphas = alphas,
              eta = eta,
              mu = mu,
              scale_eff = scale_eff,
              player_beta = player_beta,
              player_alph = player_alph)
 
shots_model <- nimbleModel(shots_code, 
                           constants = constants, 
                           data = data, 
                           inits = inits, 
                           debug=FALSE)
 
shots_mcmc <- buildMCMC(shots_model)
mcmc.out10 <- nimbleMCMC(code = shots_code, constants = constants,
                       data = data, inits = inits,
                       nchains = 2, niter = 10000,
                       summary = TRUE, WAIC = TRUE,
                       monitors = c('alphas','betas',
                                    'scale_eff', 'mu', 'eta', 
                                    'player_alph', 'player_beta'))
# 
# 

player_name <- load_shots$PLAYER_NAME
param_nm <- rownames(mcmc.out$summary$all.chains)
mcmcout_median_ab <- as_tibble(mcmc.out$summary$all.chains) %>% 
  add_column(param_nm, .before="Mean") %>% 
  dplyr::filter(str_detect(param_nm, "^beta|^alpha")) %>% 
  mutate(player_ind = as.numeric(str_extract(param_nm, "(?<=\\[)(.*?)(?=,)"))) %>% 
  mutate(zone = as.numeric(str_extract(param_nm, "(?<=,)(.*?)(?=\\])"))) %>% 
  mutate(param = str_extract(param_nm, "(.*?)(?=\\[)")) %>%
  arrange(.group_by=player_ind) %>% 
  select(c(player_ind,Median, zone, param)) %>% 
  pivot_wider(names_from=c(zone, param), values_from=Median) %>% 
  add_column(player_name, .before="player_ind")


mcmcout_median_ab <- as_tibble(mcmc.out$summary$all.chains) %>% 
  add_column(param_nm, .before="Mean") %>% 
  filter(str_detect(param_nm, "^beta|^alpha|^player")) %>% 
  mutate(param = str_extract(param_nm, "(.*?)(?=\\[)"))

view(mcmcout_tib)
View(mcmcout_median_ab)
mcmcout_tib
as.numeric(str_extract("[23,", "(?<=\\[)(.*?)(?=,)"))

write.csv(mcmcout_median_ab, "mcmc_medians_ab.csv")
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
