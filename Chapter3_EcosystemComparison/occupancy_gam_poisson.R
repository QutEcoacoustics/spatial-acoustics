model {
+   for (i in 1:N) {
+      Y[i] ~ dpois(lambda[i])
+      log(lambda[i]) <- beta_occ + beta_det*covs[i]
+   }
+   beta_occ ~ dnorm(0,1.0E-06)
+   beta_det ~ dnorm(0,1.0E-06)
+ } 