model_string_Mt_faster <- "
model {
  # Time-varying baseline detection
  for(k in 1:K) {
    g0[k] ~ dunif(0, 1)
  }

  sigma ~ dunif(1, 2000)
  psi ~ dunif(0, 1)

  for(i in 1:M) {
    s[i,1] ~ dunif(xlim[1], xlim[2])
    s[i,2] ~ dunif(ylim[1], ylim[2])

    z[i] ~ dbern(psi)

    for(j in 1:J) {
      dist[i,j] <- sqrt(
        pow(s[i,1] - x[j,1], 2) +
        pow(s[i,2] - x[j,2], 2)
      )

      for(k in 1:K) {
        p[i,j,k] <- g0[k] *
          exp(-pow(dist[i,j], 2) / (2 * pow(sigma, 2)))
      }
    }
  }

  # Likelihood for observed bears
  for(i in 1:n) {
    for(j in 1:J) {
      for(k in 1:K) {
        y[i,j,k] ~ dbern(p[i,j,k])
      }
    }
  }

  # Probability of no captures at each occasion
  for(i in (n+1):M) {
    for(k in 1:K) {
      prodTrap[i,1,k] <- 1 - p[i,1,k]

      for(j in 2:J) {
        prodTrap[i,j,k] <-
          prodTrap[i,j-1,k] * (1 - p[i,j,k])
      }

      PrNoCapOcc[i,k] <- prodTrap[i,J,k]
    }

    PrNoCap[i,1] <- PrNoCapOcc[i,1]

    for(k in 2:K) {
      PrNoCap[i,k] <-
        PrNoCap[i,k-1] * PrNoCapOcc[i,k]
    }

    PrAtLeastOneCap[i] <- 1 - PrNoCap[i,K]

    zero[i] ~ dbern(PrAtLeastOneCap[i] * z[i])
  }

  EN <- M * psi
  N <- sum(z)
}
"

M <- 200
J <- dim(caps.2016)[2]
K <- dim(caps.2016)[3]
n <- dim(caps.2016)[1]

xlim <- range(traps[, 1]) + c(-1000, 1000)
ylim <- range(traps[, 2]) + c(-1000, 1000)

jags_data_Mt_faster <- list(
  y = caps.2016,
  n = n,
  M = M,
  J = J,
  K = K,
  zero = rep(0, M),
  x = traps,
  xlim = xlim,
  ylim = ylim
)


jags_inits_Mt_faster <- function() {
  si <- cbind(
    runif(M, xlim[1], xlim[2]),
    runif(M, ylim[1], ylim[2])
  )
  
  for (i in seq_len(n)) {
    detected_traps <- which(
      rowSums(caps.2016[i, , ] > 0) > 0
    )
    
    if (length(detected_traps) > 0) {
      si[i, ] <- colMeans(traps[detected_traps, , drop = FALSE])
    }
  }
  
  list(
    z = c(rep(NA, n), rep(0, M-n)),
    psi = runif(1, 0.5, 0.9),
    s = si,
    g0 = runif(K, 0.1, 0.9),
    sigma = runif(1, 100, 500)
  )
}

writeLines(
  model_string_Mt_faster,
  "SCR-Mt-faster.jag"
)

jags_pars_Mt_faster <- c(
  "g0", "sigma", "psi", "EN", "N"
)

jags_post_Mt_faster <- jags.basic(
  data = jags_data_Mt_faster,
  inits = jags_inits_Mt_faster,
  parameters.to.save = jags_pars_Mt_faster,
  model.file = "SCR-Mt-faster.jag",
  n.chains = 3,
  n.adapt = 100,
  n.burnin = 0,
  n.iter = 2000,
  parallel = TRUE,
  verbose = TRUE
)


plot(jags_post_Mt_faster)
