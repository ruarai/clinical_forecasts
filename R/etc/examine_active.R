

linelist_raw <- read_csv("/usr/local/forecasting/linelist_data/VIC/20211201_Individual_Stay_Data.csv")


model_data <- linelist_raw %>%
  group_by(RecordID) %>%
  filter(!any(TYPE == "ICU"),
         Status == "Discharged") %>%
  
  filter(TYPE == "HOSP") %>%
  mutate(sep_time = as.numeric(sepdate - Symptom_Onset))


ggplot() +
  geom_col(aes(x = sep_time, y = n),
           model_data %>% count(sep_time)) +
  
  xlab("Time from symptom onset to hospital discharge (days)") +
  ylab("Count") +
  
  scale_x_continuous(breaks = 1:28) +
  
  coord_cartesian(xlim = c(0,28)) +
  
  theme_minimal()

model_data_ICU <- linelist_raw %>%
  group_by(RecordID) %>%
  filter(Status == "Discharged") %>%
  
  filter(TYPE == "ICU") %>%
  mutate(sep_time = as.numeric(sepdate - DiagnosisDate))


ggplot() +
  geom_col(aes(x = sep_time, y = n),
           model_data_ICU %>% count(sep_time)) +
  
  xlab("Time from DiagnosisDate to ICU discharge (days)") +
  ylab("Count") +
  
  scale_x_continuous(breaks = 1:28) +
  
  coord_cartesian(xlim = c(0,28)) +
  
  theme_minimal()


