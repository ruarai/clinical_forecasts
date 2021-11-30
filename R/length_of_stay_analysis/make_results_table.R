


results_wide_age %>%
  mutate(var = shape / rate ^ 2,
         stdev = sqrt(var),
         result = str_c(round(mean,2), " (", round(stdev,2), ")")) %>% 
  select(compartment, age_class, result) %>%
  filter(compartment == "symptomatic_to_ED") %>%
  pivot_wider(names_from = compartment,
              values_from = result)# %>%
  
  write_csv(paste0(output_dir, "/tbl_result.csv"))
