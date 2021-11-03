library(here)
library(MASS)
library(LaplacesDemon)
library(invgamma)
library(nimble)
library(stringr)

args = commandArgs(trailingOnly=TRUE)

iter = strtoi(args)

num_clusts <- 8
n_zones <- 12


# how many players are in each cluster (this sets ten in each of the clusters)
player_assignments <- rep(1:num_clusts, each=20)
n_players <- length(player_assignments)

mu_list <- rep((1:8)*3, each=20)

saveRDS(num_clusts, "/pub/ewongtoi/testsave")

# set up basis
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

new_basis1 <- new_basis - diag(12)

#write.csv(data.frame(new_basis1), here("adjacency_basis1.csv"))

#intercept only model
X = matrix(1,12,1)
r = 5
#first nearest neighbor Moran's I
Psi = MoransI.Basis(X,5,new_basis1)

design_shooting1 <- Psi


# remember: i is player, j is zone, theta_i is the cluster of player i (1, 2, ..., num_clusts (unknown))
for(i in 1:5){
  design_shooting1[, i] <- (Psi[, i] - mean(Psi[, i]))/sd(Psi[,i])
}



shots_code <- nimbleCode({

  
  alpha ~ dgamma(.25, 1)
  clust[1:n_players] ~ dCRP(alpha, size=n_players)
  
  for(i in 1:M) {
    betas[i, 1:5] ~ dmnorm(mean=zero_5[1:5], cov=id_5[1:5,1:5])
    alphas[i, 1:5] ~ dmnorm(mean=zero_5[1:5], cov=id_5[1:5,1:5])
  }
  
  for(i in 1:n_zones){
    #sigma[i] ~ dinvgamma(.01, .01)
    sigma[i] ~ dinvgamma(.25, .25)
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
 


    # this file generates the simulated dataset
    M <- 30

    set.seed(10 * iter)

    init_mu <- matrix(rnorm(n_players * n_zones), nrow = n_players, ncol = n_zones)
    init_eta <- matrix(rnorm(n_players * n_zones), nrow = n_players, ncol = n_zones)


    init_alphas <- matrix(rnorm(5*M), nrow = M, ncol = 5)
    init_betas <- matrix(rnorm(5*M), nrow = M, ncol = 5)
    init_alpha <- rgamma(1, 2, 1)

    init_clust <- sample(1:10, n_players, replace=TRUE)

    init_rand_eff <- matrix(0, nrow=n_players, ncol=n_zones)
    init_scale_eff <- rep(0, times=n_players)


    id_5 = diag(10, 5)
    id_nz = diag(10, n_zones)
    init_rand_Sig <- id_nz

    zero_5 <- rep(0, times=5)
    zero_nz <- rep(0, times=12)


    set.seed(10 * iter)

    sim_alphas <- t(mvrnorm(n=5, mu = 1:num_clusts * 50, Sigma = diag(8) * .5))
    sim_betas <- t(mvrnorm(n=5, mu = 1:num_clusts * 50, Sigma = diag(8) * .5))
    #sim_alphas <- mvrnorm(n=num_clusts, mu = rep(0, 5), Sigma = diag(5))
    #sim_betas <- mvrnorm(n=num_clusts, mu = rep(0, 5), Sigma = diag(5))

    sim_mus <- design_shooting1 %*% t(sim_alphas)
    sim_etas <- design_shooting1 %*% t(sim_betas)

    rand_sigma <- rinvwishart(nu=15, S = diag(12)*.1)
    rand_effs <- mvrnorm(n=n_players, mu = rep(0, n_zones), Sigma = rand_sigma)
    scale_eff <- rnorm(n=n_players, mean = 0, sd = 1)


    obs_y <- matrix(rep(0, n_players * n_zones), nrow=n_players, ncol=n_zones)
    obs_logit_z <- matrix(rep(0, n_players * n_zones), nrow=n_players, ncol=n_zones)

    zone_sd <- rinvgamma(n= n_zones, shape=0.25, scale=0.25)



    for (pl in 1:n_players) {
        lam_vec <- sim_mus[,player_assignments[pl]] + rand_effs[pl,]
        obs_y[pl, ] <- rpois(n = n_zones, lambda = abs(lam_vec))

        mean_vec <- sim_etas[, player_assignments[pl]] + rand_effs[pl, ]
        obs_logit_z[pl, ] <- mvrnorm(n = 1, mean_vec, diag(zone_sd^2))
    }



    constants <- list(n_players = n_players,
                      n_zones = n_zones,
                      design_shooting = design_shooting1,
                      id_5 = id_5,
                      id_nz = id_nz,
                      zero_5 = zero_5,
                      zero_nz = zero_nz,
                      M=M)


 
    data <- list(Y = obs_y,
                 logit_Z = obs_logit_z)
 
    inits <- list(betas = init_betas,
                  alphas = init_alphas,
                  alpha = init_alpha,
                  eta = init_eta,
                  mu = init_mu,
                  sigma = rep(1, times=n_zones),
                 clust=init_clust,
                  rand_eff = init_rand_eff,
                  rand_Sig = init_rand_Sig,
                 scale_eff = init_scale_eff)
 
    shots_model <- nimbleModel(shots_code, 
                               constants = constants, 
                               data = data, 
                               inits = inits, 
                               debug=FALSE)
 
    #shots_mcmc <- buildMCMC(shots_model)
    set.seed(226 * iter)
    mcmc.out <- nimbleMCMC(code = shots_code, constants = constants,
                           data = data, inits = inits,
                           nchains = 2, niter = 20000, 
                           summary = TRUE, WAIC = FALSE,
                           monitors = c('alphas','betas', 'alpha',
                                         'mu', 'eta', 'clust',
                                        'scale_eff', 'rand_eff'))#,
                                       # 'player_alph', 'player_beta'))



    file_iter <- str_pad(iter, 3, pad = '0')
    save_path = "/pub/ewongtoi/cluster_sims/easy/"
    
    mcmc_save_path <- paste0(save_path, "mcmc/mcmc_easy-", toString(file_iter))

    z_save_path <- paste0(save_path, "obs_logit_z/obs_logit_z_easy-", toString(file_iter))
    y_save_path <- paste0(save_path, "obs_y/obs_y_easy-", toString(file_iter))

    saveRDS(mcmc.out, mcmc_save_path)
    saveRDS(obs_logit_z, z_save_path)
    saveRDS(obs_y, y_save_path)

