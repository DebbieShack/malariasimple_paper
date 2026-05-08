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

##---------------------------------------------------------------------------------------------
#                             PLOTTING FUNCTIONS FOR FIGURE 3
##---------------------------------------------------------------------------------------------
prev_plt_ribbon <- function(malsim_summ, simple_stoch_summ, simple_det_summ, counter_sim,
                            EIR_set, itn_days, smc_days, alpha = 0.3, lwd = 1,
                            txt_size = 8,
                            lbl_size = 10){
  plt <- ggplot() +
    geom_vline(aes(xintercept = itn_days/365, lty = "ITN"), col = "#619CFF") +
    geom_vline(aes(xintercept = smc_days/365, lty = "SMC"), col = "#00BA38") +
    geom_ribbon(
      data = malsim_summ %>% filter(EIR == EIR_set),
      aes(
        x = timestep/365,
        ymin = prev_05,
        ymax = prev_95,
        fill = "malariasimulation",
        col = "malariasimulation"
      ),
      alpha = alpha,
      lwd = lwd
    ) +
    geom_ribbon(
      data = simple_stoch_summ %>% filter(EIR == EIR_set),
      aes(
        x = time/365,
        ymin = prev_05,
        ymax = prev_95,
        fill = "malariasimple - Stochastic",
        col = "malariasimple - Stochastic"
      ),
      alpha = alpha,
      lwd = lwd
    ) +
    geom_ribbon(
      data = simple_det_summ %>% filter(EIR == EIR_set),
      aes(
        x = time/365,
        ymin = prev_05,
        ymax = prev_95,
        fill = "malariasimple - Deterministic",
        col = "malariasimple - Deterministic"
      ),
      alpha = alpha,
      lwd = lwd
    ) +
    geom_line(
      data = counter_sim %>% filter(EIR == EIR_set),
      aes(
        x = time/365,
        y = n_detect_730_3650 / n_730_3650,
        col = "Counterfactual"),
      lwd = 1,
      lty = 1) +
    scale_color_manual(
      name = "",
      values = c(
        "Counterfactual" = "black",
        "malariasimulation" = malsim_col,
        "malariasimple - Stochastic" = simple_stoch_col,
        "malariasimple - Deterministic" = simple_det_col
      )
    ) +
    scale_linetype_manual(name = "", values = c("ITN" = 2, "SMC" = 2)) +
    scale_fill_manual(
      name = "",
      values = c(
        "Counterfactual" = "black",
        "malariasimulation" = malsim_col,
        "malariasimple - Stochastic" = simple_stoch_col,
        "malariasimple - Deterministic" = simple_det_col
      ),
      guide = "none"
    ) +
    labs(x = "Year", y = expression(italic(Pf) ~ PR[2 - 10])) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text  = element_text(size = txt_size),
          axis.title = element_text(size = lbl_size))

  return(plt)
}

prev_hist <- function(resid_df,
                      EIR_set,
                      txt_size = 8,
                      lbl_size = 10) {
  plt <- ggplot(resid_df %>% filter(EIR == EIR_set), aes(resid)) +
    geom_histogram(aes(y = after_stat(density)), fill = "grey50") +
    geom_vline(aes(xintercept = 0)) +
    labs(x = "Model Difference", y = "Density") +
    theme_bw() +
    theme(axis.text  = element_text(size = txt_size),
          axis.title = element_text(size = lbl_size))
  return(plt)
}

cases_ts <- function(weekly_malsim_summ, weekly_simple_det_summ, weekly_simple_stoch_summ, weekly_inc_counter,
                     EIR_set, itn_days, smc_days, lwd = 1, txt_size = 8, lbl_size = 10){
  plt <- ggplot() +
  geom_vline(aes(xintercept = itn_days / 365, lty = "ITN"), col = "#619CFF") +
  geom_vline(aes(xintercept = smc_days / 365, lty = "SMC"), col = "#00BA38") +
  geom_ribbon(data = weekly_malsim_summ %>% filter(EIR == EIR_set), aes(x= week / 52, ymin = cases_05, ymax = cases_95,
                                             col = "malariasimulation",
                                             fill = "malariasimulation"),
              alpha = 0.3,
              lwd = lwd) +
  geom_ribbon(data = weekly_simple_stoch_summ %>% filter(EIR == EIR_set), aes(x= week / 52, ymin = cases_05, ymax = cases_95,
                                                   col = "Stochastic",
                                                   fill = "Stochastic"),
              alpha = 0.3,
              lwd=lwd) +
  geom_ribbon(data = weekly_simple_det_summ %>% filter(EIR == EIR_set), aes(x= week / 52, ymin = cases_05, ymax = cases_95,
                                                 col = "Deterministic",
                                                 fill = "Deterministic"),
              alpha = 0.3,
              lwd=lwd) +
  geom_line(data = weekly_inc_counter %>% filter(EIR == EIR_set), aes(x=day / 365, y = clin_inc,
                                           col = "Counterfactual"),
            linewidth = 1.) +
  labs(x = "Year", y = "Weekly Cases") +
  scale_color_manual(
    name = "",
    values = c(
      "Counterfactual" = "black",
      "malariasimulation" = malsim_col,
      "Stochastic" = simple_stoch_col,
      "Deterministic" = simple_det_col
    ),
    breaks = c("Counterfactual", "Deterministic", "Stochastic", "malariasimulation"),
    guide = guide_legend(
      order = 1,
      override.aes = list(
        fill     = c("black", simple_det_col,simple_stoch_col, malsim_col),
        alpha    = c(1,             0.4,       0.4, 0.4),
        linetype = c(1,             1,         1, 1),
        linewidth= c(lwd,             lwd,       lwd, 1)
      ))
  ) +
  scale_linetype_manual(name = "", values = c("ITN" = 2, "SMC" = 2),
                        labels = c("ITN" = "ITN Distribution",
                                   "SMC" = "SMC Distribution"),
                        guide = guide_legend(order = 2)) +
  scale_fill_manual(
    name = "",
    values = c(
      "Counterfactual" = "black",
      "malariasimulation" = malsim_col,
      "Stochastic" = simple_stoch_col,
      "Deterministic" = simple_det_col
    ),
    breaks = c("Counterfactual", "Deterministic", "Stochastic", "malariasimulation"),
    guide = "none"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = txt_size),
        axis.text  = element_text(size = txt_size),  
        axis.title = element_text(size = lbl_size))
  return(plt)
}

cases_hist <- function(weekly_cases_resid_df, EIR_set, txt_size = 8, lbl_size = 10) {
  plt <- ggplot(weekly_cases_resid_df %>% filter(EIR == EIR_set)) +
    geom_histogram(aes(x = rel_resid, y = after_stat(density)), fill = "grey50") +
    geom_vline(aes(xintercept = 0)) +
    labs(x = "Model Difference", y = "Density") +
    theme_bw() +
    theme(axis.text  = element_text(size = txt_size),
          axis.title = element_text(size = lbl_size))
  return(plt)
  }
