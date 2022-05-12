model{
  for(i in 1:nsite){
    # Latent state, b is the smoothing term, beta_occ is the effect of forest cover.
    logit(psi[i]) <- inprod(b,X[i,]) + inprod(beta_occ,covs[i,])
    z[i] ~ dbern(psi[i])
    # Detection
    logit(det_prob[i]) <- beta_det
    y[i] ~ dbin(det_prob[i] * z[i], nsurvey)
  }
  # the priors
  beta_occ ~ dnorm(0, 0.75)
  beta_det ~ dnorm(0, 0.75)
  ## Parametric effect priors 
  b[1] ~ dnorm(0,0.75)
  ## prior for s(x,y)... 
  K1 <- S1[1:14,1:14] * lambda[1] 
  b[2:15] ~ dmnorm(zero[2:15],K1) 
  ## smoothing parameter priors...
  lambda ~ dgamma(.05,.005)
  rho <- log(lambda)
}