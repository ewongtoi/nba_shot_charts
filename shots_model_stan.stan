//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n_players;
  int<lower=0> n_zones;
  
  int Y[n_players, n_zones];
  real Z[n_players, n_zones];
  
  cov_matrix[7] id_7;
  cov_matrix[n_zones] id_nz;
  
  matrix[n_zones, 7] design_shooting;
  
  vector[7] zero_7;
  vector[12] zero_12;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[n_zones] mu[n_players];
  vector[n_zones] eta[n_players];
  
  vector[7] betas[n_players];
  vector[7] alphas[n_players];
  
  real scale_eff[n_players];
  
  vector[n_zones] omega[n_players];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  for (player in 1:n_players){
    scale_eff[player] ~ normal(0, 10);
    betas[player] ~ multi_normal(zero_7, id_7);
    alphas[player] ~ multi_normal(zero_7, id_7);
    
    omega[player] ~ multi_normal(zero_12, id_nz);
    
    mu[player] ~ multi_normal(design_shooting * betas[player], id_nz);
    
    
    for (shot in 1:n_zones){
      Y[player, shot] ~ poisson(exp(mu[player, shot] + scale_eff[player] * omega[player, shot]));
    }
    
    
    eta[player] ~ multi_normal(design_shooting * alphas[player], id_nz);
    
    for (pct in 1:n_zones){
      Z[player, pct] ~ normal(eta[player, pct] + omega[player, pct], 10);
    }
    
  }
  
}

