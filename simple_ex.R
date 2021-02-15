code <- nimbleCode({
  beta0 ~ dnorm(0, sd = 10000)
  beta1 ~ dnorm(0, sd = 10000)
  sigma_RE ~ dunif(0, 1000)
  for (i in 1:N) {
    beta2[i] ~ dnorm(0, sd = sigma_RE)
    logit(p[i]) <- beta0 + beta1 * x[i] + beta2[i]
    r[i] ~ dbin(p[i], n[i])
  }
})

## constants, data, and initial values
constants <- list(N = 10)

data <- list(
  r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46),
  n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79),
  x = c(0,  0,  0,  0,  0,  1, 1,  1,  1,  1)
)

inits <- list(beta0 = 0, beta1 = 0, sigma_RE = 1)

## create the model object
glmmModel <- nimbleModel(code = code, constants = constants, data = data, 
                         inits = inits, check = FALSE)
glmmMCMC <- buildMCMC(glmmModel)
CglmmModel <- compileNimble(glmmModel)
CglmmMCMC <- compileNimble(glmmMCMC, project = glmmModel)
samples <- runMCMC(CglmmMCMC, niter = 10000)


samples





























code <- nimbleCode({
  mu ~ dunif(-100, 100)
  sigma ~ dunif(0, 100)
  rho ~ dunif(20, 100)
  muVec[1:N] <- mu * onesVector[1:N]
  Cov[1:N, 1:N] <- diag(N)
  g[1:N] ~ dmnorm(muVec[1:N], cov = Cov[1:N, 1:N])
  for (i in 1:N) {
    y[i] ~ dpois(exp(g[i]))
  }
})

constants <- list(N = 148, 
                  onesVector = rep(1, 148), 
                  dist = matrix(1, nrow=148, ncol=148))
y = rpois(148, 1) # vector of 148 observations (counts)
data <- list(y=y)

inits <- list(mu = 0, 
              sigma = 5, 
              rho = 60, 
              g = rep(0, 148))
m <- nimbleModel(code = code, constants = constants, data = data, inits = inits)
my_mcmc <- function(code, constants, data, inits, n_iter=1e5, thin = 1e2){  
  Rmodel <- nimbleModel(code = code, constants = constants, data = data, inits = inits)
  Cmodel <- compileNimble(Rmodel)
  mcmcspec <- configureMCMC(Rmodel, print=FALSE,thin=thin)
  Rmcmc <- buildMCMC(mcmcspec)
  Cmcmc <- compileNimble(Rmcmc, project = Cmodel)
  Cmcmc$run(n_iter)
  samples <- as.data.frame(as.matrix(Cmcmc$mvSamples))
  #samples <- samples[,1:(length(inits)-1)]
  gather(samples)
}

df <- my_mcmc(code = code, constants = constants, data = data, inits = inits, n = 1e6)


summarise(group_by(df, key), mean=mean(value), std=sqrt(var(value)))
