

results_aged_quants <- results$age_stratified_grouped_results %>%
  select(-c(transitions)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_grouped() %>%
  mutate(age_group = age_groups[age_group + 1])



linelist_raw_NSW <- readxl::read_excel("~/data_private/NSW/NSW_out_episode_2022_04_26.xlsx", sheet = 2) %>%
  mutate(date_onset = as_date(admit_date - ddays(covid_to_adm))) %>%
  
  group_by(person_id) %>%
  slice(n())


ward_occupancy <- read_csv("~/data_private/NSW_occupancy/Ward_2022-04-26_UNSW.csv") %>%
  select(date = DATE, date_snapshot = SNAPSHOT_DATE,
         
         age_group = AGE_GROUP_10YR, count_PCR = PCR_Ward, count_RAT = RAT_Ward) %>%
  pivot_longer(cols = c(count_PCR, count_RAT),
               names_prefix = "count_",
               names_to = "type", values_to = "count")

ICU_occupancy <- read_csv("~/data_private/NSW_occupancy/ICU_2022-04-26_UNSW.csv") %>%
  select(date = DATE, date_snapshot = SNAPSHOT_DATE,
         
         age_group = AGE_GROUP_10YR, count_PCR = PCR_ICU, count_RAT = RAT_ICU) %>%
  pivot_longer(cols = c(count_PCR, count_RAT),
               names_prefix = "count_",
               names_to = "type", values_to = "count")

align_age_groups <- function(x) {
  x <- x %>% str_remove(" years")
  
  case_when(x == "80-89" ~ "80+", x == "90+" ~ "80+", TRUE ~ x)
} 


all_occupancy <- bind_rows(
  ward_occupancy %>% mutate(group = "ward"),
  ICU_occupancy %>% mutate(group = "ICU")
) %>%
  mutate(age_group = align_age_groups(age_group)) %>%
  group_by(date, age_group, group) %>%
  summarise(count = sum(count))


ggplot(results_aged_quants %>%
         filter(group == "ward")) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              alpha = 0.2, fill = 'purple') +
  
  geom_line(aes(x = date, y = count),
            data = all_occupancy %>% filter(group == "ward")) +
  
  theme_minimal() +
  
  p_common +
  
  facet_wrap(~age_group) 


ggplot(results_aged_quants %>%
           filter(group == "ICU")) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              alpha = 0.2, fill = 'green4') +
  
  geom_line(aes(x = date, y = count),
            data = all_occupancy %>% filter(group == "ICU")) +
  
  theme_minimal() +
  
  facet_wrap(~age_group) 
