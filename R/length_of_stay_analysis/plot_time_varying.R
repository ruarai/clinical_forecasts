

mean_ward <- ward_modelling %>%
  group_by(time = floor_date(dt_hosp_admission, unit = 'weeks')) %>%
  
  summarise(mean_ward_LoS = mean(ward_LoS))


ggplot() +
  
  geom_point(aes(x = dt_hosp_admission, y = ward_LoS, color = ward_coding),
             ward_modelling %>% filter(ward_coding != "censored"),
             
             size = 0.5) +
  
  geom_line(aes(x = time, y = mean_ward_LoS),
            color = '#d94801',
            mean_ward)


mean_ICU <- ICU_modelling %>%
  group_by(time = floor_date(dt_hosp_admission, unit = 'weeks')) %>%
  
  summarise(mean_ICU_LoS = mean(ICU_LoS))

ggplot() +
  
  geom_point(aes(x = dt_hosp_admission, y = ICU_LoS, color = ICU_coding),
             ICU_modelling %>% filter(ICU_coding != "censored"),
             
             size = 0.5) +
  
  geom_line(aes(x = time, y = mean_ICU_LoS),
            color = '#d94801',
            mean_ICU)


mean_postICU <- postICU_modelling %>%
  group_by(time = floor_date(dt_hosp_admission, unit = 'weeks')) %>%
  
  summarise(mean_postICU_LoS = mean(postICU_LoS))



ggplot() +
  
  geom_point(aes(x = dt_hosp_admission, y = postICU_LoS, color = postICU_coding),
             postICU_modelling %>% filter(postICU_coding != "censored"),
             
             size = 0.5) +
  
  geom_line(aes(x = time, y = mean_postICU_LoS),
            color = '#d94801',
            mean_postICU)
