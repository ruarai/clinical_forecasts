

smush_results <- list(
  "NT" = tar_read(n_isolated_NT),
  "NSW" =  tar_read(n_isolated_NSW),
  "QLD" = tar_read(n_isolated_QLD),
  "TAS" = tar_read(n_isolated_TAS),
  "VIC" = tar_read(n_isolated_VIC),
  "WA" = tar_read(n_isolated_WA),
  "SA" = tar_read(n_isolated_SA),
  "ACT" = tar_read(n_isolated_ACT)
)

smush_results <- smush_results[names(smush_results) %>% order()]

source("R/absenteeism/traj_plotting.R")

forecast_dates <- tar_read(forecast_dates)

p_raw <-map(
  1:length(smush_results),
  function(i) plot_trajs(smush_results[[i]], do_sum = TRUE))


p_lims <- c(
  "ACT" = 25000,
  "NSW" = 350000,
  "NT" = 50000,
  "QLD" = 300000,
  "SA" = 75000,
  "TAS" = 90000,
  "VIC" = 600000,
  "WA" = 75000
)

smush_plots <- map(
  1:length(smush_results),
  function(i) p_raw[[i]] +
    geom_vline(xintercept = forecast_dates$forecast_start, alpha = 0.5) +
    theme(legend.position = "none") +
    coord_cartesian(xlim = c(ymd("2021-12-01"), NA), ylim = c(0, p_lims[i])) +
    ggtitle(NULL, names(smush_results)[i]) +
    xlab(NULL) +
    ylab("Count")
)


cowplot::plot_grid(
  plotlist = smush_plots,
  ncol = 2
)

ggsave("results/absenteeism/state_absent_counts.png",
       bg = "white",
       width = 8, height = 10)


smush_results[[1]] + smush_results[[2]]

national_results <- Reduce('+', smush_results)

plot_trajs(national_results, do_sum = TRUE)  +
  geom_vline(xintercept = forecast_dates$forecast_start, alpha = 0.5) +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(ymd("2021-12-01"), NA)) +
  xlab(NULL) +
  ylab("Count")


ggsave("results/absenteeism/state_absent_counts_nat.png",
       bg = "white",
       width = 8, height = 5)
