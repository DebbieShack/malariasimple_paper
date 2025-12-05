##Functions Source File
# Required packages:
# malariasimple
# malariasimulation
# microbenchmark

#Testing runtime of malariasimulation run
malsim_basic_timings <- function(test_pops, n_sims, init_EIR, n_years){
  n_years <- n_years
  n_days <- n_years * 365

  g0 = 0.28
  g = c(-0.3, -0.03, 0.17)
  h = c(-0.35, 0.32, -0.07)

  malsim_runtimes <- data.frame()
  for(pop in test_pops){
    malsim_params <- malariasimulation::get_parameters(
      overrides = list(
        model_seasonality = TRUE,
        g0 = g0,
        g = g,
        h = h,
        clinical_incidence_rendering_min_ages = 0,
        clinical_incidence_rendering_max_ages = 100 * 365,
        prevalence_rendering_min_ages = c(0, 2 * 365),
        prevalence_rendering_max_ages = c(5*365, 10 * 365),
        human_population = pop
      )
    ) |>
      malariasimulation::set_equilibrium(init_EIR = init_EIR)
    runtime <- microbenchmark::microbenchmark(malariasimulation::run_simulation(timesteps = n_days,
                                                                                parameters = malsim_params),
                                              times = n_sims)
    runtime$pop <- pop
    malsim_runtimes <- rbind(malsim_runtimes, runtime)
  }
  return(malsim_runtimes)
}

#Testing runtime of malariasimple with both SMC and ITN
simple_dual_timings <- function(stochastic, n_sims, init_EIR, n_years){
  n_years <- n_years
  n_days <- n_years * 365

  g0 = 0.28
  g = c(-0.3, -0.03, 0.17)
  h = c(-0.35, 0.32, -0.07)

  peak_cc <- 233
  smc_offsets <- c(-60, -30, 0, 30)
  smc_days <- rep(365 * seq(1, n_years - 1, by = 1), each = length(smc_offsets)) +
    peak_cc +
    rep(smc_offsets, (n_years-1))
  smc_cov <- rep(0.5, length(smc_days))

  itn_days <- seq(1:(n_years - 1))*365
  itn_cov <- rep(0.5, length(itn_days))

  params <- malariasimple::get_parameters(n_days = n_days,
                                          stochastic = stochastic) |>
    malariasimple::set_seasonality(g0=g0,
                                   g=g,
                                   h=h) |>
    malariasimple::set_bednets(days = itn_days,
                               coverages = itn_cov) |>
    malariasimple::set_smc(days = smc_days,
                           coverages = smc_cov) |>
    malariasimple::set_equilibrium(init_EIR = init_EIR)
  runtimes <- microbenchmark::microbenchmark(malariasimple::run_simulation(params = params),
                                             times = n_sims)
  return(runtimes)
}

#Testing runtime of malariasimple with ITNs only
simple_itn_timings <- function(stochastic, n_sims, init_EIR, n_years){
  n_years <- n_years
  n_days <- n_years * 365

  g0 = 0.28
  g = c(-0.3, -0.03, 0.17)
  h = c(-0.35, 0.32, -0.07)

  itn_days <- seq(1:(n_years - 1))*365
  itn_cov <- rep(0.5, length(itn_days))

  params <- malariasimple::get_parameters(n_days = n_days,
                                          stochastic = stochastic) |>
    malariasimple::set_seasonality(g0=g0,
                                   g=g,
                                   h=h) |>
    malariasimple::set_bednets(days = itn_days,
                               coverages = itn_cov) |>
    malariasimple::set_equilibrium(init_EIR = init_EIR)
  runtimes <- microbenchmark::microbenchmark(malariasimple::run_simulation(params = params),
                                             times = n_sims)
  return(runtimes)
}

#Testing runtimes of malariasimple with no interventions
simple_basic_timings <- function(stochastic, n_sims, init_EIR, n_years){
  n_years <- n_years
  n_days <- n_years * 365

  g0 = 0.28
  g = c(-0.3, -0.03, 0.17)
  h = c(-0.35, 0.32, -0.07)

  params <- malariasimple::get_parameters(n_days = n_days,
                                          stochastic = stochastic) |>
    malariasimple::set_seasonality(g0=g0,
                                   g=g,
                                   h=h) |>
    malariasimple::set_equilibrium(init_EIR = init_EIR)
  runtimes <- microbenchmark::microbenchmark(malariasimple::run_simulation(params = params),
                                             times = n_sims)
  return(runtimes)
}

eff_ci <- function(basic_vec, int_vec, probs = c(0.05, 0.95)){
  mean <- 1 - (mean(int_vec) / mean(basic_vec))
  eff <- 1 - (int_vec / basic_vec)
  ci <- quantile(eff, probs = probs)
  ci_df <- data.frame(
    mean = mean,
    lo = ci[1],
    hi = ci[2]
  )
  return(ci_df)
}

