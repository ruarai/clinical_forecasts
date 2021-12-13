

plot_abm_results <- function(results_all,
                             simulation_options) {
  
  tbl_count <- results_all$tbl_count
  tbl_count_grouped <- results_all$tbl_count_grouped
  tbl_transitions <- results_all$tbl_transitions
  tbl_count_grouped_quants <- results_all$tbl_count_grouped_quants
  
  forecast_date_lines <- list(
    geom_vline(xintercept = simulation_options$dates$last_onset_50, linetype = 'dashed'),
    geom_vline(xintercept = simulation_options$dates$backcast_cutoff, linetype = 'dotted')
  )
  
  source("R/agent_based_model/quant_plots.R")
  
  plot_group_counts(sim_results, simulation_options,
                    forecast_date_lines)
  
  plot_group_transitions(sim_results, simulation_options,
                         forecast_date_lines)
  
  plot_all_transitions(sim_results, simulation_options,
                         forecast_date_lines)
  
  
  plot_ED_capacity(sim_results, simulation_options,
                   forecast_date_lines)
  
  source("R/agent_based_model/reporting_figures.R")
  reporting_comp_quants(sim_results, simulation_options)
  
  source("R/agent_based_model/capacity_figures.R")
  plot_capacity_plots(sim_results, simulation_options)
  
  
  
  source("R/agent_based_model/trace_plots.R")
  
  plot_trace_plots(sim_results, simulation_options,
                   forecast_date_lines)
}
