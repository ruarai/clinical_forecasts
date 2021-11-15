
make_quants <- function(tbl) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
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
    pivot_longer(cols = -all_of(colnames(id_tbl)),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}


ward_admission_by_day <- clinical_linelist %>%
  group_by(date = as_date(dt_hosp_admission, 'days')) %>%
  summarise(n = n()) %>%
  mutate(new_comp = "ward", old_comp = "ED_queue")

ICU_admission_by_day <- clinical_linelist %>%
  drop_na(dt_first_icu) %>%
  group_by(date = as_date(dt_first_icu, 'days')) %>%
  summarise(n = n()) %>%
  mutate(new_comp = "ICU", old_comp = "ward")

clinical_data <- bind_rows(
  ward_admission_by_day,
  ICU_admission_by_day
)


test <- sim_results$tbl_transitions %>%
  pivot_wider(names_from = ix,
              values_from = n,
              names_prefix = "sim_") %>%
  mutate(across(starts_with("sim_"), ~ replace_na(., 0))) %>%
  
  make_quants()


ggplot(test) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  facet_wrap(~ old_comp * new_comp,
             scales = "free_y") +
  
  scale_fill_brewer(type = 'seq',
                    palette = 5)

ggplot(test %>%
         filter(old_comp %in% clinical_data$old_comp,
                new_comp %in% clinical_data$new_comp)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  geom_line(aes(x = date, y = n),
            clinical_data) +
  
  facet_wrap(~ old_comp * new_comp,
             scales = "free_y") +
  
  scale_fill_brewer(type = 'seq',
                    palette = 5)

