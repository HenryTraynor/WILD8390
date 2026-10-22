# Generate landscape
roost_info <- create_roosts(
  num_roosts = 25,
  num_clusters = 5,
  sd = 0.08,
  prop_modern = 0.5
)

# Initialize bat populations
roost_info <- initialize_roost_populations(
  roost_info
)

# Check the initialized populations
head(roost_info)
colSums(roost_info[, c("S", "I", "R", "N")])

# Set up spatial movement
spatial_params <- create_spatial_params(nx = 50, ny = 50)

movement_params <- setup_movement(
  roost_info,
  spatial_params
)

# Set up epidemic parameters
epidemic_params <- create_epidemic_params(
  beta = 0.5,
  gamma = 0.03,
  model = "SIR"
)

# Run 120 days
sim <- run_simulation(
  roost_info = roost_info,
  spatial_params = spatial_params,
  movement_params = movement_params,
  epidemic_params = epidemic_params,
  n_days = 120
)

daily_prevalence <- vapply(sim, function(day) {
  roosts <- day$roosts
  total_N <- sum(roosts$N)
  
  if (total_N > 0) {
    sum(roosts$I) / total_N
  } else {
    NA_real_
  }
}, numeric(1))

plot(
  seq_along(daily_prevalence) - 1,
  daily_prevalence,
  type = "l",
  xlab = "Day",
  ylab = "Prevalence",
  main = "Epidemic trajectory",
  ylim = c(0, 1)
)

# ------------------------------

n_simulations <- nrow(parameter_grid)

results <- vector("list", n_simulations)

pb <- progress_bar$new(
  format = "  Simulations [:bar] :current/:total (:percent) eta: :eta",
  total = n_simulations,
  clear = FALSE,
  width = 60
)

for (i in seq_len(n_simulations)) {
  
  p <- parameter_grid[i, ]
  
  results[[i]] <- tryCatch(
    run_one_simulation(
      prop_modern = p$prop_modern,
      spatial_setting = p$spatial_setting,
      replicate = p$replicate,
      spatial_settings = spatial_settings,
      num_roosts = num_roosts,
      nx = nx,
      ny = ny,
      n_days = n_days,
      beta = beta,
      gamma = gamma
    ),
    error = function(e) {
      warning(
        sprintf(
          "Simulation %d failed (modern=%.2f, setting=%d, replicate=%d): %s",
          i, p$prop_modern, p$spatial_setting,
          p$replicate, conditionMessage(e)
        ),
        call. = FALSE
      )
      
      NULL
    }
  )
  
  pb$tick()
}

simulation_results <- bind_rows(results)

dim(simulation_results)
head(simulation_results)