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

all_occupancy <- bind_rows(
  ward_occupancy %>% mutate(group = "ward"),
  ICU_occupancy %>% mutate(group = "ICU")
)

plot_data <- all_occupancy %>%
  group_by(date, group, type) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(names_from = "type", values_from = "count")

ggplot(plot_data %>% filter(group == "ward")) +
  geom_linerange(aes(x = date, ymin = 0, ymax = PCR, color = "PCR"),
                 size = 1.2) +
  geom_linerange(aes(x = date, ymin = PCR, ymax = PCR + RAT, color = "RAT"),
                 size = 1.2) +
  paletteer::scale_color_paletteer_d("fishualize::Trimma_lantana", name = "Colour") +
  
  ggtitle("NSW - Daily occupancy by test type", "Ward") +
  
  theme_minimal() +
  
  coord_cartesian(xlim = c(ymd("2022-01-01"), NA))

ggplot(plot_data %>% filter(group == "ICU")) +
  geom_linerange(aes(x = date, ymin = 0, ymax = PCR, color = "PCR"),
                 size = 1.2) +
  geom_linerange(aes(x = date, ymin = PCR, ymax = PCR + RAT, color = "RAT"),
                 size = 1.2) +
  paletteer::scale_color_paletteer_d("fishualize::Trimma_lantana", name = "Colour") +
  
  ggtitle("NSW - Daily occupancy by test type", "ICU") +
  
  theme_minimal() +
  
  coord_cartesian(xlim = c(ymd("2022-01-01"), NA))


plot_data_props <- plot_data %>%
  mutate(both = PCR + RAT,
         PCR = PCR / both,
         RAT = RAT / both)

ggplot(plot_data_props %>% filter(group == "ward")) +
  geom_linerange(aes(x = date, ymin = 0, ymax = PCR, color = "PCR"),
                 size = 1.2) +
  geom_linerange(aes(x = date, ymin = PCR, ymax = PCR + RAT, color = "RAT"),
                 size = 1.2) +
  paletteer::scale_color_paletteer_d("fishualize::Trimma_lantana", name = "Colour") +
  
  ggtitle("NSW - Daily proportion by test type", "Ward") +
  
  theme_minimal() +
  
  coord_cartesian(xlim = c(ymd("2022-01-01"), NA))

ggplot(plot_data_props %>% filter(group == "ICU")) +
  geom_linerange(aes(x = date, ymin = 0, ymax = PCR, color = "PCR"),
                 size = 1.2) +
  geom_linerange(aes(x = date, ymin = PCR, ymax = PCR + RAT, color = "RAT"),
                 size = 1.2) +
  paletteer::scale_color_paletteer_d("fishualize::Trimma_lantana", name = "Colour") +
  
  ggtitle("NSW - Daily proportion by test type", "Ward") +
  
  theme_minimal() +
  
  coord_cartesian(xlim = c(ymd("2022-01-01"), NA))


ggplot(all_occupancy %>% filter(group == "ward") %>%
         group_by(date, age_group) %>%
         mutate(count = count / sum(count))) +
  geom_line(aes(x = date, y = count, color = type)) +
  
  facet_wrap(~age_group, scales = "free_y") +
  
  theme_minimal() +
  
  coord_cartesian(xlim = c(ymd("2022-01-01"), NA))
