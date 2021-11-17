


results_wide_age %>%
  mutate(var = shape / rate ^ 2,
         stdev = sqrt(var)) %>% 
  select(compartment, age_class, stdev) %>%
  pivot_wider(names_from = compartment,
              values_from = stdev) %>%
  
  write_csv(paste0(output_dir, "/tbl_stdev.csv"))

results_wide_age %>%
  select(compartment, age_class, mean) %>%
  pivot_wider(names_from = compartment,
              values_from = mean) %>%
  
  write_csv(paste0(output_dir, "/tbl_mean.csv"))


wide_prob_table %>%
  write_csv(paste0(output_dir, "/tbl_prob.csv"))
