

NNDSS_linelist <- read_rds("data/processed/clinical_linelist.rds")

NNDSS_counts <- NNDSS_linelist %>%
  group_by(state, date = date_onset) %>%
  summarise(n_ward = sum(status_hospital == 1),
            n_icu = sum(status_ICU == 1))

covid19data <- read_rds("data/covid19data.rds") %>%
  select(-state) %>% rename(state = state_abbrev)


states_of_interest <- c("NSW", "VIC")
filter_for_state <- . %>%
  filter(state %in% states_of_interest)

ggplot() +
  geom_linerange(aes(x = date, ymin = 0, ymax = hosp_cum), 
                 covid19data %>% filter_for_state()) +
  
  geom_point(aes(x = date, y = n_ward),
             NNDSS_counts %>% filter_for_state()) +
  
  facet_wrap(~state,
             scales = "free_y") +
  
  scale_y_continuous(breaks = scales::breaks_extended(7)) +
  
  theme_minimal() +
  
  coord_cartesian(xlim = c(today() - 60, today()))
