rm(list=ls())
# Load the libraries 
library(mgcv)    # For GAMs
library(runjags) # For fitting a model in JAGs
library(raster)  # For simulating some spatial data
library(mvtnorm) # For simulating some spatial data.

#  Generate some covariates. The most complicated part of this
#    is the spatial autocorrelation. The function below generates a 
#    does this for us.

# gen_mvn: 
#  Generate multivariate normal data. This functions simulate covariate
#    values over cells in a raster.

gen_mvn <- function(
    rast = NULL, mu = NULL,
    sigma = NULL, rho = NULL
){
  # ARGUMENTS
  # ---------
  # rast = A raster object
  #
  # mu = numeric vector of length 2. Each mu is proportionally 
  #  where you want the mean value to be. (0,0) is the bottom left,
  #  (1,1) is top right.
  # 
  # sigma = Variance of covariate on x and y axes
  #
  # rho = Correlation of covariate on x and y axes
  # ---------
  # error checking
  if(length(mu) != 2 | !is.numeric(mu) | any(mu > 1) | any(mu < 0)){
    stop("mu must be a numeric vector of length 2.")
  }
  if(length(sigma) != 2 | !is.numeric(sigma) | any(sigma < 0)){
    stop("Sigma must be a non-negative numeric vector of length 2.")
  }
  if(length(rho) != 1 | !is.numeric(rho)| rho > 1 |rho < -1){
    stop("rho must be a numeric scalar between -1 and 1.")
  }
  
  # get bounds of raster
  bounds <- raster::extent(rast)
  
  # input a proportion of where you want mu to be on x and y
  mu_loc <- c(
    bounds@xmin + mu[1] * (bounds@xmax - bounds@xmin),
    bounds@ymin + mu[2] * (bounds@ymax - bounds@ymin)
  )
  
  # Make variance terms
  Sigma <- diag(
    c(
      sigma[1] * abs(bounds@xmax - bounds@xmin),
      sigma[2] * abs(bounds@ymax - bounds@ymin)
    )
  )
  # fill the off diagonal terms
  Sigma[2:3] <- rep(rho * prod(diag(Sigma)))
  
  response <- mvtnorm::dmvnorm(
    raster::xyFromCell(
      rast, 1:raster::ncell(rast)
    ), 
    mean=mu_loc, 
    sigma=Sigma
  )
  return(response)
}

# Generate our "landscape"
#   bounds of the plane we are sampling within
plane_xmin <- -1
plane_xmax <-  1
plane_ymin <- -1
plane_ymax <-  1

# number of pixels in space. 
npixels_x <- 100
npixels_y <- 100

# create a raster, currently has no data. We also make a 'blank' raster
#  so that we can easily add 1 other layer.
plane <- blank <- raster(
  ncol = npixels_x,
  nrow = npixels_y,
  xmn = plane_xmin,
  xmx = plane_xmax,
  ymn=plane_ymin,
  ymx=plane_ymax
)

# the x y coordinates of each pixel in the plane
plane_coord <- xyFromCell(
  plane,
  1:ncell(plane)
)
# set seed for simulation
set.seed(312)

# generate a spatial covariate, this will create a circular pattern

x_cov <- 0.7 * gen_mvn(plane, c(0.5,0.5), c(0.2,0.1), 0.1) + 
  0.3 * gen_mvn(plane, c(0.2,0.5), c(0.5,0.7), 0.4)

values(plane) <- as.numeric(scale(x_cov))
names(plane) <- c("spatial", "something")

# We made this blank raster above.
values(blank) <- rnorm(
  raster::ncell(plane)
)
names(blank) <- "forest"
plane <- raster::addLayer(
  plane,
  blank
)

# The number of survey sites.
nsite <- 250

# very rough and somewhat even spacing of sites on landscape
my_sites <- floor(
  seq(
    1, raster::ncell(plane), length.out = nsite
  )
)

# move half the sites a little bit so they don't end up in a straight line
jiggle_sites <- sort(
  sample(
    1:nsite, floor(nsite/2), replace = FALSE
  )
)

# add the jiggled sites back into my_sites
my_sites[jiggle_sites] <- floor(
  my_sites[jiggle_sites] + median(diff(my_sites)/2)
)

# This shuffling process could mean we have a value >
#   the number of cells in the raster. Fix it if this
#   is the case.
if(any(my_sites > raster::ncell(plane))){
  my_sites[my_sites > raster::ncell(plane)] <- raster::ncell(plane)
  my_sites <- unique(my_sites)
}

xy <- xyFromCell(plane, 
  my_sites)

plot(plane$spatial)
points(xy, pch = 21, bg = "gray50")

my_covars <- values(plane)[my_sites,]

# We are going to multiply the spatial term by -1 here so the 
#  species is more likely to be outside the green circle (
#  (which are positive values). Additionally, let's say the
#  species is more likely to occur in areas of high forest cover.

# The occupancy parameters
sim_params <- c(-1, 0.5)

logit_prob <- my_covars %*% sim_params

species_prob <- plogis(logit_prob)

# Simulate occupancy on landscape

z <- rbinom(nsite, 1, species_prob)

my_sites2 <- cbind(my_sites, xy, z)

# Plot it out
plot(plane$spatial)
points(xy[my_sites2[z == 1],], pch = 21, bg = "blue")
points(xy[my_sites2[z == 1],], pch = 21, bg = "grey50")
legend(
  "bottomleft",
  pt.bg = c("blue", "grey50"),
  legend =c("present", "absent"),
  pch = 21,
  bty = "n"
)

# Detection probability
det_prob <- 0.4
# The number of repeat surveys
n_survey <- 4

# The simulated data we would have 'in hand' at the end of the study
y <- rbinom(
  nsite,
  n_survey,
  det_prob * z
)



# Get site coordinates
site_coords <- xy

# The temporary GAM we will take apart.
tmp_jags <- mgcv::jagam(
  response ~ s(
    x, y, k = 15, bs = "ds", m = c(1,0.5)
  ),
  data = data.frame(
    response = rep(1, nsite),
    x = site_coords[,1],
    y = site_coords[,2]
  ),
  family = "binomial",
  file = "tmp.jags"
  
)

# data to include
data_list <- list(
  y = y,
  X = tmp_jags$jags.data$X,
  S1 = tmp_jags$jags.data$S1,
  covs = matrix(my_covars[,2], ncol = 1, nrow = nsite),
  nsite = nsite,
  nsurvey = n_survey,
  zero = tmp_jags$jags.data$zero
)

# initial values function
my_inits <- function(chain){
  gen_list <- function(chain = chain){
    list(
      z = rep(1, nsite),
      beta_occ = rnorm(1),
      beta_det = rnorm(1),
      b = rnorm(
        length(tmp_jags$jags.ini$b),
        tmp_jags$jags.ini$b,
        0.2
      ),
      lambda = rgamma(1,1,1),
      .RNG.name = switch(chain,
                         "1" = "base::Wichmann-Hill",
                         "2" = "base::Wichmann-Hill",
                         "3" = "base::Super-Duper",
                         "4" = "base::Mersenne-Twister",
                         "5" = "base::Wichmann-Hill",
                         "6" = "base::Marsaglia-Multicarry",
                         "7" = "base::Super-Duper",
                         "8" = "base::Mersenne-Twister"
      ),
      .RNG.seed = sample(1:1e+06, 1)
    )
  }
  return(switch(chain,
                "1" = gen_list(chain),
                "2" = gen_list(chain),
                "3" = gen_list(chain),
                "4" = gen_list(chain),
                "5" = gen_list(chain),
                "6" = gen_list(chain),
                "7" = gen_list(chain),
                "8" = gen_list(chain)
  )
  )
}

# Long model run to increase effective sample size
my_mod <- runjags::run.jags(
  model = "occupancy_gam_example.R",
  monitor = c("b", "beta_occ", "beta_det", "rho"),
  inits = my_inits,
  data = data_list,
  n.chains = 3,
  adapt = 1000,
  burnin = 100000,
  sample = 20000,
  thin = 20,
  method = "parallel"
)

# summarize model
my_sum <- summary(my_mod)
plot(my_mod)

# Combine the three mcmc chains
my_mcmc <- do.call('rbind', my_mod$mcmc)

# Make the spatial prediction across all 250 sites for each
#  step in the MCMC chain, then calculate 95% credible interval.
spatial_est <- data_list$X %*% t(my_mcmc[,1:15])
spatial_est <- apply(
  spatial_est, 1, quantile, probs = c(0.025,0.5,0.975)
)

# Combine these estimates with the true values
#  Note: Multiplying the true values by -1 because that is how
#  we simualted the data.

spatial_est <- cbind(
  t(spatial_est), my_covars[,1] * -1
)
# order by the true values
spatial_est <- spatial_est[order(spatial_est[,4]),]

# Just a quick check to determine if true value in within
#   95% credible interval at all sites. It's TRUE.
sum(
  spatial_est[,4] > spatial_est[,1] & 
    spatial_est[,4] < spatial_est[,3]
) == nsite

# Plot this out.
plot(
  spatial_est[,2] ~ c(1:nsite), type = "n",
  bty = "l",
  xlab = "Sites",
  ylab = "Spatial value at each site"
)
for(i in 1:nsite){
  # Add 95% credible intervals
  lines(
    y = spatial_est[i,c(1,3)], x = rep(i,2)
  )
}
# Add the true values as well
points(spatial_est[,4], pch = 21, bg = "grey50")


# get the median estimate of the spatial smoothing term
#  which are the first 15 values in the model summary, column 2.
median_est <- data_list$X %*% my_sum[1:15,2]

# We already have the site coordinates, so we just need to 
#  generate the colors we want to plot out.

# This is roughly the range of the model estimates
cuts <- seq(-2.1, 2.6, length.out = 255)

# Classify the median estimate into a color category
median_est_colors <- as.numeric(
  cut(median_est, cuts)
)

# And the colors (similar to other plots)
my_terrain <- terrain.colors(255)
plot(
  x = my_sites2[,2],
  y = my_sites2[,3],
  pch = 21,
  bg = my_terrain[median_est_colors],
  cex = 2,
  xlab = "x",
  ylab = "y",
  bty = "l"
)
