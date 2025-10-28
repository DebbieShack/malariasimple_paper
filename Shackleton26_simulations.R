##------Install any required packages available on CRAN
# pkgs <- c(
#   "microbenchmark",
#   "dplyr",
#   "ggplot2",
#   "cowplot"
# )
# to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
# if (length(to_install) > 0) {
#   install.packages(to_install)
# }

#--------Install malariasimple
# install.packages("pak")
# pak::pak("mrc-ide/malariasimple")

#--------Install malariasimulation
# install.packages("remotes")
# remotes::install_github('mrc-ide/malariasimulation')

library(malariasimple) #Version 0.1.0
library(malariasimulation) #Version 2.0.2
library(microbenchmark) #Version 1.5.0
library(dplyr) #Version 1.1.4
library(ggplot2) #Version 3.5.2
library(cowplot) #Version 1.2.0
source("C:/Users/Debbie/OneDrive - Imperial College London/malariasimple_stuff/paper/malariasimple_paper_plots_neat_functions.R")

#Set colour scheme
simple_det_col   <- "#CA0020"
simple_stoch_col <- "#984EA3"
malsim_col       <- "grey20"

#Set seed for reproducibility
set.seed(101)

#--------------------------------------------------------------------------------------------
#                             FIGURE 2 - TIMINGS PLOT
#--------------------------------------------------------------------------------------------

#------------------ Run simulations -------------------------
n_sims = 20
n_years = 20
test_pops <- seq(10000, 100000, by = 10000)

# n_sims <- 5
# n_years <- 2
# test_pops <- seq(2000,4000, by = 1000)
malsim_timings_10 <- malsim_basic_timings(
  test_pops = test_pops,
  n_sims = n_sims,
  init_EIR = 10,
  n_years = n_years
)

malsim_timings_50 <- malsim_basic_timings(
  test_pops = test_pops,
  n_sims = n_sims,
  init_EIR = 50,
  n_years = n_years
)

malsim_timings_200 <- malsim_basic_timings(
  test_pops = test_pops,
  n_sims = n_sims,
  init_EIR = 200,
  n_years = n_years
)

simple_basic_det <- simple_basic_timings(
  stochastic = FALSE,
  n_sims = n_sims,
  init_EIR = 50,
  n_years = n_years
)

simple_basic_stoch <- simple_basic_timings(
  stochastic = TRUE,
  n_sims = n_sims,
  init_EIR = 50,
  n_years = n_years
)

simple_itn_det <- simple_itn_timings(
  stochastic = FALSE,
  n_sims = n_sims,
  init_EIR = 50,
  n_years = n_years
)

simple_itn_stoch <- simple_itn_timings(
  stochastic = TRUE,
  n_sims = n_sims,
  init_EIR = 50,
  n_years = n_years
)

simple_dual_det <- simple_dual_timings(
  stochastic = FALSE,
  n_sims = n_sims,
  init_EIR = 50,
  n_years = n_years
)

simple_dual_stoch <- simple_dual_timings(
  stochastic = TRUE,
  n_sims = n_sims,
  init_EIR = 50,
  n_years = n_years
)

#------------------ Prepare malariasimulation plot ------------------
malsim_df <- rbind(
  malsim_timings_10,
  malsim_timings_50,
  malsim_timings_200
) %>%
  dplyr::select(time, pop) %>%
  mutate(eir = rep(c(10,50,200), each = n_sims * length(test_pops))) %>%
  mutate(eir = as.factor(eir)) %>%
  group_by(pop, eir) %>%
  mutate(time = time / 10^9) %>%
  summarise(min_time = min(time),
            max_time = max(time),
            mean_time = mean(time))

malsim_plt <- ggplot(malsim_df) +
  geom_smooth(aes(x=pop, y = mean_time / 10^9, group = eir, col = eir), method = "lm", se = FALSE) +
  geom_point(aes(x=pop, y = mean_time / 10^9, group = eir, col = eir)) +
  geom_errorbar(aes(x=pop, ymin = min_time / 10^9, ymax = max_time / 10^9, group = eir, col = eir), width = median(test_pops) / 10) +
  scale_color_manual(values= c(`10` = "grey70", `50` = "grey40", `200` = "black")) +
  labs(x = "Human Population", y = "Mean Runtime (s)", col = "Baseline EIR") +
  ylim(0,NA) +
  theme_bw() +
  theme(legend.position = "bottom")

#------------------ Prepare malariasimple plot -----------------------------
#Add medium malariasimulation for comparison
med_malsim <- malsim_df %>%
  ungroup() %>%
  filter(pop == 3000, eir == 50) %>%
  dplyr::select(-c(pop, eir)) %>%
  slice(rep(1,3)) %>%
  mutate(sim = "malariasimulation",
         ints = c("basic", "itn", "dual"))


simple_df <- rbind(
  simple_basic_det,
  simple_basic_stoch,
  simple_itn_det,
  simple_itn_stoch,
  simple_dual_det,
  simple_dual_stoch
) %>%
  mutate(ints = rep(c("basic", "itn", "dual"), each = n_sims*2),
         sim = rep(c("Deterministic", "Stochastic"), each = n_sims, times = 3)) %>%
  group_by(ints, sim) %>%
  mutate(time = time / 10^9) %>%
  summarise(mean_time = mean(time),
            min_time = min(time),
            max_time = max(time)) %>%
  rbind(med_malsim) %>%
  mutate(sim = factor(sim, levels = c("Deterministic", "Stochastic", "malariasimulation")),
         ints = factor(ints, levels = c("dual", "itn", "basic")))

#Create inset plot for Figure 2B
simple_plt_inset <- ggplot(simple_df, aes(x = ints, y = mean_time, fill = sim)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, col = "black") +
  geom_errorbar(position = position_dodge(width = 0.8), aes(ymin = min_time, ymax = max_time, group = sim), width = 0.2) +
  labs(x = "", y = "", fill = "") +
  scale_x_discrete(labels = c("2","1","0")) +
  #scale_y_continuous(breaks = c(0,100,200,300), labels = c("0", "100", "200", "300")) +
  scale_fill_manual(values = c("Deterministic" = "#CA0020", "Stochastic" = "#984EA3", "malariasimulation" = "grey40")) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", colour = NA),  # white inside plot
    plot.background = element_rect(fill = NA, colour = NA)         # transparent outside
  )

simple_plt_main <- ggplot(simple_df %>% filter(sim != "malariasimulation"), aes(x = ints, y = mean_time, fill = sim)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, col = "black") +
  geom_errorbar(position = position_dodge(width = 0.8), aes(ymin = min_time, ymax = max_time, group = sim), width = 0.2) +
  labs(x = "No. of Interventions", y = "Mean Runtime (s)", fill = "") +
  scale_x_discrete(labels = c("2","1","0")) +
  scale_fill_manual(values = c("Deterministic" = "#CA0020", "Stochastic" = "#984EA3")) +
  theme_bw() +
  theme(legend.position = "bottom")

simple_plt <- ggdraw() +
  draw_plot(simple_plt_main) +  # main plot
  draw_plot(
    simple_plt_inset,
    x = 0.56,
    y = 0.56,
    width = 0.44,
    height = 0.44
  )

#Figure 2
plot_grid(malsim_plt, simple_plt, labels = c("A", "B"))


##-------------------------------------------------------------------------------------------
#                             FIGURE 3 - DEMO PLOT
##-------------------------------------------------------------------------------------------
#------------------ Define parameters -----------------------
#Standard parameters
n_years <- 3
human_pop <- 50000
n_days <- n_years*365
init_EIR <- 20
n_sims <- 20

##Seasonality parameters
g0 = 0.28
g = c(-0.3, -0.03, 0.17)
h = c(-0.35, 0.32, -0.07)

##Define ITN parameters
itn_days <- c(100)
itn_cov <- c(0.6)
gamman <- 2.64*365
retention <- 5*365

n_dist <- length(itn_days)
dn0 <- rep(0.41, n_dist)
rn <- rep(0.56, n_dist)
rnm <- rep(0.24, n_dist)
gamman_vec <- rep(gamman, n_dist)

##Define SMC parameters
smc_offsets <- c(-60, -30, 0, 30)
peak_cc <- get_peak_cc(g0, g, h)
smc_days <- rep(365 * seq(1, n_years - 1, by = 1), each = length(smc_offsets)) +
  peak_cc +
  rep(smc_offsets, 2)
smc_cov <- seq(0.3,0.6, length.out = length(smc_days))
smc_min_age = 0.25*365
smc_max_age = 5*365

draws <- sample(1:1000, replace = TRUE, size = n_sims)


#------------------ Produce malariasimulation demo runs ------------------------
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
    human_population = human_pop
  )
) |>
  malariasimulation::set_drugs(list(SP_AQ_params)) |>
  malariasimulation::set_smc(
    drug = 1,
    timesteps = smc_days,
    coverages = smc_cov,
    min_ages = rep(smc_min_age, length(smc_days)),
    max_ages = rep(smc_max_age, length(smc_days))
  ) |>
  malariasimulation::set_bednets(
    timesteps = itn_days,
    coverages = itn_cov,
    retention = retention,
    dn0 = matrix(dn0, nrow = n_dist, ncol = 1),
    rn = matrix(rn, nrow = n_dist, ncol = 1),
    rnm = matrix(rnm, nrow = n_dist, ncol = 1),
    gamman = gamman_vec
  )

malsim <- data.frame()
for(i in 1:n_sims){
  malsim_params_i <- malsim_params |>
    set_parameter_draw(draw = draws[i]) |>
    set_equilibrium(init_EIR)
  malsim_i <- malariasimulation::run_simulation(timesteps = n_days,
                                                parameters = malsim_params_i)
  malsim_i$repetition = i
  malsim <- rbind(malsim, malsim_i)
  print(i)
}

malsim_summ <- malsim %>%
  group_by(timestep) %>%
  mutate(prev_2_10 = n_detect_lm_730_3650 / n_age_730_3650) %>%
  summarise(prev_05 = quantile(prev_2_10, 0.05),
            prev_95 = quantile(prev_2_10, 0.95),
            prev_mean = mean(prev_2_10))

#------------------ Produce malariasimple demo runs ------------------------
simple_det <- data.frame()
for(i in 1:n_sims){
  simple_det_params <- malariasimple::get_parameters(
    parameter_draws = draws[i],
    n_days = n_days,
    prevalence_rendering_min_ages = c(0,2 * 365),
    prevalence_rendering_max_ages = c(5*365, 10 * 365),
    human_pop = human_pop
  ) |>
    malariasimple::set_seasonality(
      g0 = g0,
      g = g,
      h = h
    ) |>
    malariasimple::set_bednets(
      days = itn_days,
      coverages = itn_cov,
      gamman = gamman,
      retention = retention,
      distribution_type = "random"
    ) |>
    malariasimple::set_smc(
      min_age = smc_min_age,
      max_age = smc_max_age,
      days = smc_days,
      coverages = smc_cov,
      distribution_type = "random"
    ) |>
    malariasimple::set_equilibrium(init_EIR = init_EIR)
  simple_det_i <- malariasimple::run_simulation(simple_det_params) |> as.data.frame()
  simple_det_i$repetition <- i
  simple_det <- rbind(simple_det, simple_det_i)
}

ggplot(simple_det) +
  geom_line(aes(x=time, y = n_detect_0_1825, group = repetition))

simple_det_summ <- simple_det %>%
  group_by(time) %>%
  mutate(prev_2_10 = n_detect_730_3650 / n_730_3650) %>%
  summarise(prev_05 = quantile(prev_2_10, 0.05),
            prev_95 = quantile(prev_2_10, 0.95),
            prev_mean = mean(prev_2_10))

##Set up stochsatic malariasimple parameters
simple_stoch <- data.frame()
for(i in 1:n_sims){
  simple_stoch_params <- malariasimple::get_parameters(
    parameter_draws = draws[i],
    stochastic = TRUE,
    n_days = n_days,
    prevalence_rendering_min_ages = c(0,2 * 365),
    prevalence_rendering_max_ages = c(5*365, 10 * 365),
    human_pop = human_pop
  ) |>
    malariasimple::set_seasonality(
      g0 = g0,
      g = g,
      h = h
    ) |>
    malariasimple::set_bednets(
      days = itn_days,
      coverages = itn_cov,
      gamman = gamman,
      retention = retention,
      distribution_type = "random"
    ) |>
    malariasimple::set_smc(
      min_age = smc_min_age,
      max_age = smc_max_age,
      days = smc_days,
      coverages = smc_cov,
      distribution_type = "random"
    ) |>
    malariasimple::set_equilibrium(init_EIR = init_EIR)
  simple_stoch_i <- malariasimple::run_simulation(simple_stoch_params) |> as.data.frame()
  simple_stoch_i$repetition <- i
  simple_stoch <- rbind(simple_stoch, simple_stoch_i)
}

simple_stoch_summ <- simple_stoch %>%
  group_by(time) %>%
  mutate(prev_2_10 = n_detect_730_3650 / n_730_3650) %>%
  summarise(prev_05 = quantile(prev_2_10, 0.05),
            prev_95 = quantile(prev_2_10, 0.95),
            prev_mean = mean(prev_2_10))


counter_params <- malariasimple::get_parameters(
  n_days = n_days,
  prevalence_rendering_min_ages = c(0,2 * 365),
  prevalence_rendering_max_ages = c(5*365, 10 * 365),
  human_pop = human_pop
) |>
  malariasimple::set_seasonality(
    g0 = g0,
    g = g,
    h = h
  ) |>
  malariasimple::set_equilibrium(init_EIR = init_EIR)
counter_sim <- malariasimple::run_simulation(counter_params) |> as.data.frame()

#------------------ Produce prevalence time series ----------------------
alpha <- 0.3
lwd <- 1
prev_ts <- ggplot() +
  geom_vline(aes(xintercept = itn_days/365, lty = "ITN"), col = "#619CFF") +
  geom_vline(aes(xintercept = smc_days/365, lty = "SMC"), col = "#00BA38") +
  geom_ribbon(
    data = malsim_summ,
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
    data = simple_stoch_summ,
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
    data = simple_det_summ,
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
    data = counter_sim,
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
  theme(legend.position = "none")
prev_ts
simple_det$simple_prevalence <- simple_det$n_detect_730_3650 / simple_det$n_730_3650

#------------------ Produce prevalence histogram ----------------------
resid_df <- malsim %>%
  mutate(malsim_prevalence = n_detect_lm_730_3650 / n_age_730_3650) %>%
  dplyr::select(malsim_prevalence, timestep, repetition) %>%
  full_join(simple_det[,c("time", "simple_prevalence", "repetition")], by = join_by(timestep == time, repetition == repetition)) %>%
  mutate(resid = malsim_prevalence - simple_prevalence)

prev_hist <- ggplot(resid_df, aes(resid)) +
  geom_histogram(aes(y = after_stat(density)), fill = "grey50") +
  geom_vline(aes(xintercept = 0)) +
  labs(x = "Model Difference", y = "Density") +
  theme_bw()
#------------------ Produce weekly cases time series --------------------
weekly_inc_simple_det <- simple_det %>%
  mutate(week = 1 + (time - time %% 7) / 7) %>%
  group_by(week, repetition) %>%
  summarise(clin_inc = sum(n_clin_inc_0_Inf)) %>%
  mutate(day = week * 7)

weekly_inc_simple_stoch <- simple_stoch %>%
  mutate(week = 1 + (time - time %% 7) / 7) %>%
  group_by(week, repetition) %>%
  summarise(clin_inc = sum(n_clin_inc_0_Inf)) %>%
  mutate(day = week * 7)

weekly_inc_malsim <- malsim %>%
  mutate(week = 1 + (timestep - timestep %% 7) / 7) %>%
  group_by(week, repetition) %>%
  summarise(clin_inc = sum(n_inc_clinical_0_36500)) %>%
  mutate(day = week * 7)

weekly_inc_counter <- counter_sim %>%
  mutate(week = 1 + (time - time %% 7) / 7) %>%
  group_by(week) %>%
  summarise(clin_inc = sum(n_clin_inc_0_Inf)) %>%
  mutate(day = week * 7)

weekly_simple_det_summ <- weekly_inc_simple_det %>%
  group_by(week) %>%
  summarise(cases_05 = quantile(clin_inc, 0.05),
            cases_95 = quantile(clin_inc, 0.95),
            cases_mean = mean(clin_inc))

weekly_simple_stoch_summ <- weekly_inc_simple_stoch %>%
  group_by(week) %>%
  summarise(cases_05 = quantile(clin_inc, 0.05),
            cases_95 = quantile(clin_inc, 0.95),
            cases_mean = mean(clin_inc))

weekly_malsim_summ <- weekly_inc_malsim %>%
  group_by(week) %>%
  summarise(cases_05 = quantile(clin_inc, 0.05),
            cases_95 = quantile(clin_inc, 0.95),
            cases_mean = mean(clin_inc))


#Produce plot
cases_ts <- ggplot() +
  geom_vline(aes(xintercept = itn_days / 365, lty = "ITN"), col = "#619CFF") +
  geom_vline(aes(xintercept = smc_days / 365, lty = "SMC"), col = "#00BA38") +
  geom_ribbon(data = weekly_malsim_summ, aes(x= week / 52, ymin = cases_05, ymax = cases_95,
                                             col = "malariasimulation",
                                             fill = "malariasimulation"),
              alpha = 0.3,
              lwd = lwd) +
  geom_ribbon(data = weekly_simple_stoch_summ, aes(x= week / 52, ymin = cases_05, ymax = cases_95,
                                                   col = "Stochastic",
                                                   fill = "Stochastic"),
              alpha = 0.3,
              lwd=lwd) +
  geom_ribbon(data = weekly_simple_det_summ, aes(x= week / 52, ymin = cases_05, ymax = cases_95,
                                                 col = "Deterministic",
                                                 fill = "Deterministic"),
              alpha = 0.3,
              lwd=lwd) +
  geom_line(data = weekly_inc_counter, aes(x=day / 365, y = clin_inc,
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
      override.aes = list(
        fill     = c("black", simple_det_col,simple_stoch_col, malsim_col),
        alpha    = c(1,             0.4,       0.4, 0.4),
        linetype = c(1,             1,         1, 1),
        linewidth= c(lwd,             lwd,       lwd, 1)
      ))
  ) +
  scale_linetype_manual(name = "", values = c("ITN" = 2, "SMC" = 2),
                        labels = c("ITN" = "ITN Distribution",
                                   "SMC" = "SMC Distribution")) +
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
  theme(legend.position = "bottom")

#------------------ Produce weekly cases histogram --------------------
##Residuals histogram
weekly_cases_resid_df <- weekly_inc_malsim %>%
  full_join(weekly_inc_simple_det, by = c("week", "repetition")) %>%
  mutate(resid = clin_inc.x - clin_inc.y) %>%
  mutate(rel_resid = resid / clin_inc.x)

cases_hist <- ggplot(weekly_cases_resid_df) +
  geom_histogram(aes(x = rel_resid, y = after_stat(density)), fill = "grey50") +
  geom_vline(aes(xintercept = 0)) +
  labs(x = "Model Difference", y = "Density") +
  theme_bw()

#------------------ Combine plots to produce Figure 3 -----------------
cases_ts_no_leg <- cases_ts +
  theme(legend.position = "none")
legend <- get_legend(cases_ts)
empty <- ggplot() + theme_void()
top_panel <- plot_grid(prev_ts, prev_hist, rel_widths = c(2, 1), labels = c("A", "B"))
bottom_panel <- plot_grid(cases_ts_no_leg, cases_hist, rel_widths = c(2, 1), labels = c("C", "D"))
legend_panel <- plot_grid(legend, empty, rel_widths = c(2,1))

plot_grid(
  top_panel,
  bottom_panel,
  legend_panel,
  ncol = 1,
  rel_heights = c(1, 1, 0.15)  # adjust third value for legend spacing
)



##-------------------------------------------------------------------------------------------
#                    FIGURE 4 - COMPARISON OF INTERVENTION EFFECTIVENESS
##-------------------------------------------------------------------------------------------

#------------------ Define parameters -------------------------
n_days <- 3*365 + 173
init_EIRs <- c(10, 50, 200)
human_pop <- 50000
n_sims <- 20

retention <- 5 * 365
gamman <- 2.64*365
days <- c(173)
coverages <- c(0.5)
n_dists <- length(days)

smc_days <- rep(365 * c(0,1,2), each = length(smc_offsets)) +
  peak_cc +
  rep(smc_offsets, 3)
smc_cov <- rep(0.5, length.out = length(smc_days))

stochastic = c(FALSE, TRUE) #Two options of stochasticity in malariasimple

#------------------ Run counterfactual simulations -------------------------
##malariasimple
malsim_basic <- data.frame()
for(EIR in init_EIRs){
  malsim_params_basic <- malariasimulation::get_parameters(
    overrides = list(
      model_seasonality = TRUE,
      g0 = g0,
      g = g,
      h = h,
      clinical_incidence_rendering_min_ages = c(0, 0.25, 0) * 365,
      clinical_incidence_rendering_max_ages = c(5, 5, 100) * 365,
      prevalence_rendering_min_ages = c(0, 0.25, 0) * 365,
      prevalence_rendering_max_ages = c(5, 5, 100) * 365,
      human_population = human_pop
    )
  )
  malsim_basic_EIR <- data.frame()
  for(i in 1:n_sims){
    malsim_params_i <- malsim_params_basic |>
      set_parameter_draw(draw = draws[i]) |>
      set_equilibrium(EIR)
    malsim_i <- malariasimulation::run_simulation(timesteps = n_days,
                                                  parameters = malsim_params_i)
    malsim_i$repetition = i
    malsim_i$init_EIR = EIR
    malsim_basic_EIR <- rbind(malsim_basic_EIR, malsim_i)
  }
  malsim_basic <- rbind(malsim_basic, malsim_basic_EIR)
}

malsim_basic <- malsim_basic %>% filter(timestep <= n_days)

malsim_basic_mean <- malsim_basic %>%
  group_by(timestep, init_EIR) %>%
  summarise(clin_inc_all = mean(n_inc_clinical_0_36500),
            clin_inc_05 = mean(n_inc_clinical_0_1825))

##malariasimple
simple_basic_list <- vector(mode = "list", length = 2)
for (s in 1:2) {
  simple_basic <- data.frame()
  for (EIR in init_EIRs) {
    simple_basic_EIR <- data.frame()
    for (i in 1:n_sims) {
      simple_params_basic <- malariasimple::get_parameters(
        n_days = n_days,
        stochastic = stochastic[s],
        parameter_draws = draws[i],
        prevalence_rendering_min_ages = c(0, 0.25, 0) * 365,
        prevalence_rendering_max_ages = c(5, 5, Inf) * 365,
        clin_inc_rendering_min_ages = c(0, 0.25, 0) * 365,
        clin_inc_rendering_max_ages = c(5, 5, Inf) * 365,
        human_pop = human_pop
      ) |>
        malariasimple::set_seasonality(g0 = g0, g = g, h = h) |>
        malariasimple::set_equilibrium(init_EIR = EIR)
      simple_i <- malariasimple::run_simulation(simple_params_basic) |> as.data.frame()
      simple_i$repetition = i
      simple_i$init_EIR = EIR
      simple_basic_EIR <- rbind(simple_basic_EIR, simple_i)
    }
    simple_basic <- rbind(simple_basic, simple_basic_EIR)
  }
 simple_basic_list[[s]] <- simple_basic
}

#------------------ Run ITN simulations -------------------------
##malariasimulation
malsim_itn <- data.frame()
for(EIR in init_EIRs){
  malsim_params_itn <- malariasimulation::get_parameters(
    overrides = list(
      model_seasonality = TRUE,
      g0 = g0,
      g = g,
      h = h,
      clinical_incidence_rendering_min_ages = c(0, 0.25, 0) * 365,
      clinical_incidence_rendering_max_ages = c(5, 5, 100) * 365,
      prevalence_rendering_min_ages = c(0, 0.25, 0) * 365,
      prevalence_rendering_max_ages = c(5, 5, 100) * 365,
      human_population = human_pop
    )
  ) |>
    malariasimulation::set_bednets(
      timesteps = days,
      coverages = coverages,
      retention = retention,
      dn0 = matrix(rep(0.41,n_dists), nrow = n_dists, ncol = 1),
      rn = matrix(rep(0.56,n_dists), nrow = n_dists, ncol = 1),
      rnm = matrix(rep(0.24, n_dists), nrow = n_dists, ncol = 1),
      gamman = rep(gamman, n_dists)
    )

  malsim_itn_EIR <- data.frame()
  for(i in 1:n_sims){
    malsim_params_i <- malsim_params_itn |>
      set_parameter_draw(draw = draws[i]) |>
      malariasimulation::set_equilibrium(EIR)
    malsim_i <- malariasimulation::run_simulation(timesteps = n_days,
                                                  parameters = malsim_params_i)
    malsim_i$repetition = i
    malsim_i$init_EIR = EIR
    malsim_itn_EIR <- rbind(malsim_itn_EIR, malsim_i)
  }
  malsim_itn <- rbind(malsim_itn, malsim_itn_EIR)
}

##malariasimple
simple_itn_list <- vector(mode = "list", length = 2)
for (s in 1:2) {
  simple_itn <- data.frame()
  for (EIR in init_EIRs) {
    simple_itn_EIR <- data.frame()
    for (i in 1:n_sims) {
      simple_params_itn <- malariasimple::get_parameters(
        n_days = n_days,
        stochastic = stochastic[s],
        parameter_draws = draws[i],
        prevalence_rendering_min_ages = c(0, 0.25, 0) * 365,
        prevalence_rendering_max_ages = c(5, 5, Inf) * 365,
        clin_inc_rendering_min_ages = c(0, 0.25, 0) * 365,
        clin_inc_rendering_max_ages = c(5, 5, Inf) * 365,
        human_pop = human_pop
      ) |>
        malariasimple::set_seasonality(g0 = g0, g = g, h = h) |>
        malariasimple::set_bednets(
          days = days,
          coverages = coverages,
          gamman = gamman,
          retention = retention,
          distribution_type = "random"
        ) |>
        malariasimple::set_equilibrium(init_EIR = EIR)
      simple_i <- malariasimple::run_simulation(simple_params_itn) |> as.data.frame()
      simple_i$repetition = i
      simple_i$init_EIR = EIR
      simple_itn_EIR <- rbind(simple_itn_EIR, simple_i)
    }
    simple_itn <- rbind(simple_itn, simple_itn_EIR)
  }
  simple_itn_list[[s]] <- simple_itn
}
#------------------ Run SMC simulations -------------------------
##malariasimulation
malsim_smc <- data.frame()
for(EIR in init_EIRs){
  malsim_params_smc <- malariasimulation::get_parameters(
    overrides = list(
      model_seasonality = TRUE,
      g0 = g0,
      g = g,
      h = h,
      clinical_incidence_rendering_min_ages = c(0, 0.25, 0) * 365,
      clinical_incidence_rendering_max_ages = c(5, 5, 100) * 365,
      prevalence_rendering_min_ages = c(0, 0.25, 0) * 365,
      prevalence_rendering_max_ages = c(5, 5, 100) * 365,
      human_population = human_pop
    )
  ) |>
    malariasimulation::set_drugs(list(SP_AQ_params)) |>
    malariasimulation::set_smc(
      drug = 1,
      timesteps = smc_days,
      coverages = smc_cov,
      min_ages = rep(smc_min_age, length(smc_days)),
      max_ages = rep(smc_max_age, length(smc_days))
    ) |>
    malariasimulation::set_equilibrium(init_EIR = EIR)
  malsim_smc_EIR <- data.frame()
  for(i in 1:n_sims){
    malsim_params_i <- malsim_params_smc |>
      set_parameter_draw(draw = draws[i]) |>
      malariasimulation::set_equilibrium(EIR)
    malsim_i <- malariasimulation::run_simulation(timesteps = n_days,
                                                  parameters = malsim_params_i)
    malsim_i$repetition = i
    malsim_i$init_EIR = EIR
    malsim_smc_EIR <- rbind(malsim_smc_EIR, malsim_i)
  }
  malsim_smc <- rbind(malsim_smc, malsim_smc_EIR)
}

##malariasimple
simple_smc_list <- vector(mode = "list", length = 2)
for (s in 1:2) {
  simple_smc <- data.frame()
  for (EIR in init_EIRs) {
    simple_smc_EIR <- data.frame()
    for (i in 1:n_sims) {
      simple_params_smc <- malariasimple::get_parameters(
        n_days = n_days,
        stochastic = stochastic[s],
        parameter_draws = draws[i],
        prevalence_rendering_min_ages = c(0, 0.25, 0) * 365,
        prevalence_rendering_max_ages = c(5, 5, Inf) * 365,
        clin_inc_rendering_min_ages = c(0, 0.25, 0) * 365,
        clin_inc_rendering_max_ages = c(5, 5, Inf) * 365,
        human_pop = human_pop
      ) |>
        malariasimple::set_smc(
          days = smc_days,
          coverages = smc_cov,
          min_age = smc_min_age,
          max_age = smc_max_age
        ) |>
        malariasimple::set_seasonality(g0 = g0, g = g, h = h) |>
        malariasimple::set_equilibrium(init_EIR = EIR)
      simple_i <- malariasimple::run_simulation(simple_params_smc) |> as.data.frame()
      simple_i$repetition = i
      simple_i$init_EIR = EIR
      simple_smc_EIR <- rbind(simple_smc_EIR, simple_i)
    }
    simple_smc <- rbind(simple_smc, simple_smc_EIR)
  }
  simple_smc_list[[s]] <- simple_smc
}


#------------------ Prepare plot inputs ------------------------
all_sims <- list(
  malariasimulation = list(basic = malsim_basic, itn = malsim_itn, smc = malsim_smc),
  simple_det = list(basic = simple_basic_list[[1]], itn = simple_itn_list[[1]], smc = simple_smc_list[[1]]),
  simple_stoch = list(basic = simple_basic_list[[2]], itn = simple_itn_list[[2]], smc = simple_smc_list[[2]])
)

clin_inc_names <- c("n_inc_clinical_0_36500", "n_clin_inc_0_Inf", "n_clin_inc_0_Inf")
clin_inc_names_05 <- c("n_inc_clinical_0_1825", "n_clin_inc_0_1825", "n_clin_inc_0_1825")
sim_names = c("basic", "itn", "smc")
time_name <- c("timestep", "time", "time")
model_names <- names(all_sims)

eff_summ <- data.frame()
for(i in 1:length(model_names)){
  df_inc_long <- data.frame()
  model <- model_names[i]
  inc_name <- clin_inc_names[i]
  inc_name_05 <- clin_inc_names_05[i]
  mod <- all_sims[[model]]

  for(sim in sim_names){
    x <- mod[[sim]]
    x <- x[x[,time_name[i]] %in% 173:903,]
    x <- x %>%
      dplyr::group_by(init_EIR, repetition) %>%
      dplyr::summarise(all = sum(.data[[inc_name]] / n()),
                       under_5 = sum(.data[[inc_name_05]] / n()),
                       .groups = "drop") %>%
      tidyr::pivot_longer(cols = c(all, under_5),
                          names_to = "ages",
                          values_to = "mean_inc") %>%
      mutate(sim = sim)
    df_inc_long <- rbind(df_inc_long, x)
  }
  df_inc <- df_inc_long %>%
    tidyr::pivot_wider(
      id_cols = c(init_EIR, repetition, ages),
      names_from = sim,
      values_from = mean_inc)

  eff_summ_sim <- df_inc %>%
    group_by(init_EIR, ages) %>%
    summarise(itn = eff_ci(basic, itn),
              smc = eff_ci(basic, smc)) %>%
    mutate(model = model)
  eff_summ <- rbind(eff_summ, eff_summ_sim)
}

eff_summ_long <- eff_summ %>%
  tidyr::pivot_longer(
    cols = c(itn, smc),
    names_to = c("intervention"),
    values_to = "stats") %>%
  tidyr::unnest_wider(stats) %>%
  mutate(model = recode(model,
                        "simple_det" = "Deterministic",
                        "simple_stoch" = "Stochastic")) %>%
  mutate(model = factor(model, levels = c("malariasimulation", "Stochastic", "Deterministic"))) %>%
  filter(!(ages == "all" & intervention == "smc"),
         !(ages == "under_5" & intervention == "itn"))
#------------------ Produce plot ------------------------
ggplot(eff_summ_long) +
  geom_bar(stat = "identity", aes(x=model, y = mean, fill = model), col = "black") +
  geom_errorbar(aes(x=model, ymin = lo, ymax = hi), col = "black", width = 0.1) +
  geom_text(data = eff_summ_long,
            aes(x=model, y = 0, label = paste0(sprintf("%.3f", mean))),
            vjust = -0.6, size = 3, col = "white") +
  scale_fill_manual(values = c(
    "malariasimulation" = malsim_col,
    "Deterministic" = simple_det_col,
    "Stochastic" = simple_stoch_col)
  ) +
  facet_grid(
    init_EIR ~ intervention,
    labeller = labeller(init_EIR = as_labeller(c(
      `10` = "Low EIR",
      `50` = "Moderate EIR",
      `200` = "High EIR")),
      intervention = as_labeller(c(
        "itn" = "ITN",
        "smc" = "SMC"
      )))) +
  labs(x = "", y = "Proportion Reduction in Cases") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"))


##-------------------------------------------------------------------------------------------
#                    FIGURES 5 & 6 - MCMC runs
##-------------------------------------------------------------------------------------------

#------------------ Define parameters -------------------------

#Based on Kedougou, Senegal from 'site' package
n_days <- 5*365
itn_days <- 365 #Days on which ITN distribution events occur
itn_cov <- 0.6 #Proportion of the population who receive an ITN on each distribution event
gamman <- 2.64*365 #Half-life of insecticide
retention <- 5*365 #People keep their nets for 5 years
human_pop <- 250000
init_EIR <- 11
n_tests <- 200
#Seasonality
g0 <- 95.811
g <- c(-117.2, 4.578, 22.62)
h <- c(-97.24, 73.66, -6.86)

#------------------ Set up synthetic dataset -------------------
params = malariasimple::get_parameters(n_days = n_days,
                                       human_pop = human_pop,
                                       prevalence_rendering_min_ages = 0,
                                       prevalence_rendering_max_ages = 5*365,
                                       stochastic = TRUE) |>
  malariasimple::set_seasonality(g0=g0,
                                 g=g,
                                 h=h) |>
  malariasimple::set_bednets(days = itn_days,
                             coverages = itn_cov,
                             gamman = gamman,
                             retention = retention) |>
  malariasimple::set_equilibrium(init_EIR = init_EIR)

out_full <- malariasimple::run_simulation(params) %>%
  as.data.frame()

out <- out_full %>%
  filter(time %% 365 == 100)

sim_data <- data.frame(
  time = out$time,
  tests = n_tests,
  positive = rbinom(n = nrow(out), size = n_tests, prob = out$n_detect_0_1825 / out$n_0_1825)
) %>%
  mutate(a = 1 + positive, #Analytical Beta conjugate posterior from Beta prior and Binomial likelihood.
         b = 1 + tests - positive) %>%
  mutate(ci05 = qbeta(0.05, a, b),
         ci95 = qbeta(0.95, a, b))

#------------------ Prepare MCMC ---------------------
if (!requireNamespace("monty", quietly = TRUE)) {
  install.packages("monty", repos = c(
    "https://mrc-ide.r-universe.dev",
    "https://cloud.r-project.org"
  ))
}
if (!requireNamespace("dust2", quietly = TRUE)) {
  install.packages(
    "dust2",
    repos = c("https://mrc-ide.r-universe.dev",
              "https://cloud.r-project.org"))
}
library(monty)
library(dust2)

gen <- malariasimple::malariasimple_deterministic()
filter <- dust_unfilter_create(gen, time_start = 0, data = sim_data, dt = 1/4)
packer <- monty_packer(
  scalar = c("init_EIR", "coverage"),
  process = function(x) {
    params = malariasimple::get_parameters(n_days = 5*365,
                                           human_pop = 250000,
                                           prevalence_rendering_min_ages = 0,
                                           prevalence_rendering_max_ages = 5*365) |>
      malariasimple::set_seasonality(g0=g0,
                                     g=g,
                                     h=h) |>
      malariasimple::set_bednets(days = 365,
                                 coverages = x$coverage,
                                 gamman =  2.64*365,
                                 retention = 5*365) |>
      malariasimple::set_equilibrium(init_EIR = x$init_EIR)
  }
)
likelihood <- dust_likelihood_monty(filter, packer)

prior <- monty_dsl({
  init_EIR ~ Gamma(shape = 1.5, rate = 1/5)
  coverage ~ Uniform(0, 1)
})
posterior <- prior + likelihood

vcv <- diag(2) * 0.5 #Variance covariance matrix
vcv[2,2] <- 0.05
sampler <- monty_sampler_adaptive(vcv, initial_vcv_weight = 10)

#------------------ Run MCMC ---------------------
n_steps <- 5000 #Number of MCMC iterations
n_chains <- 4
samples <- monty::monty_sample(posterior,
                               sampler,
                               n_steps = n_steps,
                               initial = c(5,0.5),
                               n_chains = n_chains)
burn_in <- 500
samples_mat <- samples$pars[,burn_in:n_steps,]
dim(samples_mat) <- c(2,ncol(samples_mat)*n_chains)

samples_df_wide <- samples_mat |>
  t() |>
  as.data.frame() |>
  magrittr::set_colnames(c("Baseline EIR", "Coverage"))

samples_df <- samples_df_wide |>
  reshape2::melt()

true_vals <- data.frame(
  variable = c("Baseline EIR", "Coverage"),
  value = c(11, 0.6)
)

ci90 <- samples_df %>%
  group_by(variable) %>%
  summarise(lo = quantile(value, 0.05),
            hi = quantile(value, 0.95),
            med = median(value), .groups = "drop")

#----------------- Produce Figure 5 -----------
eir_dens_plt <- ggplot(samples_df_wide) +
  geom_rect(data = ci90 %>% filter(variable == "Baseline EIR"),
            aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  geom_density(aes(x = `Baseline EIR`)) +
  geom_vline(data = ci90 %>% filter(variable == "Baseline EIR"), aes(xintercept = med, col = "Median"), lty = "dashed") +
  geom_vline(data = true_vals %>% filter(variable == "Baseline EIR"), aes(xintercept = value, col = "'True' value")) +
  scale_color_manual(
    values = c("Median" = "black",
               "'True' value" = "firebrick")
  ) +
  labs(x = "Baseline EIR", y = "Density", col = "") +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside")

cov_dens_plt <- ggplot(samples_df_wide) +
  geom_rect(data = ci90 %>% filter(variable == "Coverage"),
            aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  geom_density(aes(x = Coverage)) +
  geom_vline(data = ci90 %>% filter(variable == "Coverage"), aes(xintercept = med, col = "Median"), lty = "dashed") +
  geom_vline(data = true_vals %>% filter(variable == "Coverage"), aes(xintercept = value, col = "'True' value")) +
  scale_color_manual(
    values = c("Median" = "black",
               "'True' value" = "firebrick")
  ) +
  labs(x = "Coverage", y = "Density", col = "") +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside")

corr_plt <- ggplot(data = samples_df_wide) +
  geom_point(aes(x=`Baseline EIR`, y = Coverage), shape = 21, col = "grey10", fill = "grey10", alpha = 0.5, size = 2.5) +
  theme_bw()

##Figure 5
left_col <- plot_grid(
  eir_dens_plt, cov_dens_plt,
  ncol = 1,
  labels = c("A", "B")   # labels for the stacked plots
)
plot_grid(
  left_col, corr_plt,
  ncol = 2,
  rel_widths = c(2, 1),  # left column is twice as wide
  labels = c("", "C")    # "" because left_col already has A and B
)

#----------------- Produce Figure 6 -----------
posterior_simulation <- data.frame()

for(i in 1:20){
  chain <- sample(1:n_chains,1)
  iter <- sample(burn_in:n_steps, 1)

  init_EIR <- samples$pars[1,iter,chain]
  coverage <- samples$pars[2,iter,chain]

  params = malariasimple::get_parameters(n_days = n_days,
                                         human_pop = human_pop,
                                         prevalence_rendering_min_ages = 0,
                                         prevalence_rendering_max_ages = 5*365) |>
    malariasimple::set_seasonality(g0=g0,
                                   g=g,
                                   h=h) |>
    malariasimple::set_bednets(days = itn_days,
                               coverages = coverage,
                               gamman = gamman,
                               retention = retention) |>
    malariasimple::set_equilibrium(init_EIR = init_EIR)

  sim <- malariasimple::run_simulation(params) |> as.data.frame()
  sim$run <- i
  sim$init_EIR <- init_EIR
  sim$coverage <- coverage
  posterior_simulation <- rbind(posterior_simulation, sim)
}

posterior_summary <- posterior_simulation %>%
  group_by(time) %>%
  summarise(mean = mean(n_detect_0_1825) / n_0_1825,
            q05 = quantile(n_detect_0_1825, 0.05) / n_0_1825,
            q25 = quantile(n_detect_0_1825, 0.25) / n_0_1825,
            q75 = quantile(n_detect_0_1825, 0.75) / n_0_1825,
            q95 = quantile(n_detect_0_1825, 0.95) / n_0_1825,
            .groups = "drop")

ggplot(posterior_summary, aes(x=time/ 365)) +
  geom_vline(aes(xintercept = 1, lty = "ITN"), col = "#619CFF", lty = 2) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill  = "90% CI")) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = "50% CI")) +
  geom_line(aes(y=mean, col = "Posterior mean"), lwd=1, lty = 2) +
  geom_line(data = out_full, aes(x=time / 365, y = n_detect_0_1825 / n_0_1825, col = "'True'"), lwd = 1) +
  geom_point(data = sim_data, aes(x=time / 365, y = positive / tests, col = "Simulated Data")) +
  geom_errorbar(data = sim_data, aes(x=time / 365, ymin = ci05, ymax = ci95, col = "Simulated Data"), width = 0.1) +
  scale_fill_manual(
    name = "Posterior Credible Interval",
    values = c("90% CI" = "#CFE8FF",
               "50% CI" = "#7DB7F2")
  ) +
  scale_color_manual(
    name = "",
    values = c("Posterior mean" = "#08306B",
               "'True'" = "black",
               "Simulated Data" = "red")
  ) +
  labs(x = "Year", y = expression(P~italic(f)~PR[0-5])) +
  theme_bw() +
  theme(legend.position = "none")
