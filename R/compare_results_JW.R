
make_quants <- function(tbl, sim_prefix = "sim_") {
  data_matrix <- tbl %>%
    select(starts_with(sim_prefix)) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with(sim_prefix))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
  probs <- c(0.5, 0.75, 0.9, 0.95, 0.99)
  
  quant_probs <- c(rev(1 - probs) / 2, 0.5 + probs / 2)
  quant_names <- c(str_c("lower_", rev(probs) * 100), str_c("upper_", probs * 100))
  
  quants <- data_matrix %>%
    matrixStats::rowQuantiles(probs = quant_probs) %>%
    `colnames<-`(quant_names) %>%
    as_tibble() %>%
    bind_cols(id_tbl, .) %>%
    pivot_longer(cols = c(starts_with("lower"), starts_with("upper")),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}


our_results <- read_csv("results/test_1_VIC.csv") %>%
  filter(group %in% c("ward", "ICU")) %>%
  arrange(desc(quant)) %>%
  mutate(bed_type = case_when(group == "ward" ~ "Ward",
                           TRUE ~ group),
         quant = factor(quant, levels = unique(quant)))


# 
# their_results <- read_csv("data/old_forecasts_JW_model/cp_dataframe_VIC_forecast_22-Oct-2021.csv") %>%
#   group_by(date, bed_type) %>%
#   summarise(across(starts_with("occupancy_"), ~ sum(.)))

their_quants <- their_results %>%
  
  ungroup() %>%
  
  make_quants(sim_prefix = "occupancy_") %>%
  mutate(date = dmy(date))


ggplot() +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant, color = quant),
              their_quants %>% filter(quant %in% c("50", "90")),
              alpha = 0.5) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant, color = quant),
              our_results %>% filter(quant %in% c("50", "90")),
              alpha = 0.5) +
  
  facet_wrap(~bed_type,
             scales = "free_y") +
  
  scale_fill_brewer(type = 'seq',
                    palette = 5) +
  
  scale_color_brewer(type = 'seq',
                    palette = 5) +
  
  theme_minimal()

