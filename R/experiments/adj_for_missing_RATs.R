

nindss <- tar_read(nindss_state_NSW) %>%
  mutate(
    age_group = case_when(
      age_group %in% c("0-9", "10-19") ~ "0-19",
      age_group %in% c("70-79", "80+") ~ "70+",
      TRUE ~ age_group
    )
  ) %>%
  filter(date_diagnosis > ymd("2021-12-01"))


fix_age_group <- function(age_group) {
  age_group <- age_group %>%
    str_remove("AgeGroup_")
  case_when(
    age_group == "0-19" ~ "0-19",
    age_group %in% c("20-24", "25-29") ~ "20-29",
    age_group %in% c("30-34", "35-39") ~ "30-39",
    age_group %in% c("40-44", "45-49") ~ "40-49",
    age_group %in% c("50-54", "55-59") ~ "50-59",
    age_group %in% c("60-64", "65-69") ~ "60-69",
    TRUE ~ "70+"
  )
}

cases_nswh <- read_csv(
  "https://data.nsw.gov.au/data/dataset/3dc5dc39-40b4-4ee9-8ec6-2d862a916dcf/resource/4b03bc25-ab4b-46c0-bb3e-0c839c9915c5/download/confirmed_cases_table2_age_group_agg.csv"
  ) %>%
  mutate(age_group = fix_age_group(age_group)) %>%
  
  group_by(notification_date, age_group) %>%
  summarise(count = sum(confirmed_cases_count)) %>%
  ungroup()


cases_nswh_count <- cases_nswh %>%
  filter(notification_date >= ymd("2021-12-01"))

nindss_count <- nindss %>%
  count(date_diagnosis, age_group)

ggplot() + 
  geom_line(aes(x = date_diagnosis, y = n, color = 'nindss'),
            size = 0.5,
            nindss_count) + 
  geom_line(aes(x = notification_date, y = count, color = 'nsw'),
             size = 0.5,
            cases_nswh_count) +
  
  scale_color_manual(
    NULL,
    values = c("nsw" = "black", "nindss" = ggokabeito::palette_okabe_ito(2)),
    labels = c("nsw" = "Data.NSW cases", "nindss" = "NINDSS linelist")
  ) +
  
  facet_wrap(~age_group, scales = "free_y") +
  
  xlab(NULL) + ylab(NULL) +
  
  ggtitle("Incidence") +
  
  theme_minimal() +
  theme(legend.position = "bottom")

combined <- nindss_count %>%
  rename(count_nindss = n) %>%
  left_join(
    cases_nswh_count %>% rename(count_nsw = count) %>% mutate(notification_date),
    by = c("date_diagnosis" = "notification_date", "age_group")
  ) %>%
  
  mutate(pr_in_nindss = count_nindss / count_nsw) %>%
  filter(date_diagnosis <= ymd("2022-02-27"))


ggplot(combined) +
  geom_line(aes(x = date_diagnosis, y = pr_in_nindss, color = age_group)) +
  coord_cartesian(ylim = c(0.2, 1.1)) +
  
  ggokabeito::scale_color_okabe_ito() +
  
  xlab(NULL) + ylab(NULL) +
  ggtitle(NULL, "Proportion of cases in NINDSS (by notification/diagnosis date)") +
  
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  
  theme_minimal()

age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")


morbidity_trajectories <- tar_read(morbidity_trajectories_NSW) %>%
  filter(date >= ymd("2021-12-01")) %>%
  complete(date = seq(ymd("2021-12-01"), max(combined$date_diagnosis), by = 'days'),
           bootstrap = 1:50,
           age_group = age_groups) %>%
  group_by(age_group, bootstrap) %>%
  arrange(date) %>%
  fill(pr_hosp, pr_ICU, pr_age_given_case, .direction = "downup") %>%
  ungroup()

adj_data <- morbidity_trajectories %>%
  mutate(
    age_group = case_when(
      age_group %in% c("0-9", "10-19") ~ "0-19",
      age_group %in% c("70-79", "80+") ~ "70+",
      TRUE ~ age_group
    )
  ) %>%
  group_by(bootstrap, age_group, date) %>%
  summarise(pr_age_given_case = sum(pr_age_given_case),
            pr_hosp = mean(pr_hosp)) %>%
  
  
  left_join(combined %>% select(date = date_diagnosis, pr_in_nindss, age_group)) %>%
  
  drop_na(pr_in_nindss) %>%
  
  mutate(pr_in_nindss = pmin(pr_in_nindss, 1),
         
         pr_age_given_case_adj = pr_age_given_case / pr_in_nindss,
         pr_hosp_adj = pr_hosp * pr_in_nindss) %>%
  
  group_by(bootstrap, date) %>%
  mutate(pr_age_given_case_adj = pr_age_given_case_adj / sum(pr_age_given_case_adj))


ggplot(adj_data) +
  
  geom_line(aes(x = date, y = pr_age_given_case, group = bootstrap, color = 'Unadjusted')) +
  
  geom_line(aes(x = date, y = pr_age_given_case_adj, group = bootstrap, color = 'Adjusted')) +
  
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = 2) +
  
  ggtitle("Probability of age|case") +
  
  
  facet_wrap(~age_group) +
  theme(legend.position = "bottom")

ggplot(adj_data) +
  
  geom_line(aes(x = date, y = pr_hosp, group = bootstrap, color = 'Unadjusted'),
            alpha = 0.25) +
  geom_line(aes(x = date, y = pr_hosp_adj, group = bootstrap, color = 'Adjusted'),
            alpha = 0.25) +
  
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = 7) +
  
  ggtitle("Probability of hospitalised|case") +
  
  facet_wrap(~age_group, scales = "free_y") +
  theme(legend.position = "bottom")

