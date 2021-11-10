

local_cases <- read_csv("data/input/local_cases_input.csv") %>%
  filter(state == "VIC",
         detection_probability > 0.5) %>%
  mutate(count_adj = round(count / detection_probability))





clinical_linelist <- read_rds("data/processed/clinical_linelist_VIC.rds")


delay_samples <- clinical_linelist %>%
  filter(dt_hosp_admission <= max(dt_hosp_admission) - days(14),
         !is.na(dt_onset)) %>%
  
  mutate(delay = round((dt_hosp_admission - dt_onset) / ddays(1))) %>%
  filter(delay > -5, delay < 31) %>%
  pull(delay)


local_cases_delayed <- local_cases %>%
  filter(date_onset >= max(date_onset) - 60) %>%
  uncount(count_adj) %>%
  mutate(date_admission = date_onset + sample(delay_samples, nrow(.), replace = TRUE)) %>%
  
  group_by(date_admission) %>%
  summarise(n = n())


by_date_admission <- clinical_linelist %>%
  group_by(date_admission = as_date(floor_date(dt_hosp_admission, unit = 'days'))) %>%
  summarise(n = n())

ggplot(by_date_admission) +
  geom_linerange(aes(x = date_admission, ymin = 0, ymax = n)) +
  
  coord_cartesian(xlim = c(max(by_date_admission$date_admission) - 60, NA)) +
  
  geom_point(aes(x = date_admission, y = n * 0.07),
             local_cases_delayed) +
  
  geom_vline(xintercept = max(local_cases$date_onset))
