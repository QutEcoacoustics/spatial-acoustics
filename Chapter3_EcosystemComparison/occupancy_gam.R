model{
  for(i in 1:nsite){
    # Latent state, b is the smoothing term, beta_occ is the effect of vegetation group.
    logit(psi[i]) <- inprod(b,X[i,]) + inprod(beta_occ,covs[i,])
    z[i] ~ dbern(psi[i])
    # Detection
    logit(det_prob[i]) <- beta_det
    y[i] ~ dbin(det_prob[i] * z[i], nsurvey)
  }
  # the priors
  beta_occ ~ dnorm(0,0.75)
  beta_det ~ dnorm(0,0.75)
  ## Parametric effects prior
  b[1] ~ dnorm(0,0.0083)
  ## prior for s(x,y)... 
  K1 <- S1[1:8,1:8] * lambda[1] 
  b[2:9] ~ dmnorm(zero[2:9],K1) 
  ## smoothing parameter priors CHECK...
    lambda ~ dgamma(.05,.005)
    rho <- log(lambda)
}