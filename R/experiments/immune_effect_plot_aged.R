

library(targets)
library(tidyverse)
library(lubridate)

print("Loading vaccine state")

vaccine_state <- tar_read(vaccination_data)
data_date <- ymd("2022-07-12")



reff_env <- new.env()
suppressPackageStartupMessages(source("R/immunity/functions_new.R", local = reff_env))

date_sequence <- seq.Date(
  from = as.Date("2021-02-22"),
  to = data_date + weeks(16),
  by = "1 week"
)#[63:66]


print("Calculating ve_tables")

# calculate vaccine effects
ve_tables <- tibble(
  date = date_sequence
) %>%
  mutate(
    cohorts = map(
      .x = date,
      .f = reff_env$get_vaccine_cohorts_at_date,
      vaccine_scenarios = vaccine_state
    ),
    coverage = map(
      .x = cohorts,
      .f = reff_env$get_coverage
    ),
    ves = map(
      .x = cohorts,
      .f = reff_env$get_vaccine_efficacies
    )
  )


get_hosp_ves <- function(coverage, ves, ...) {
  ves %>%
    filter(outcome == "hospitalisation") %>%
    
    left_join(coverage, by = c("scenario", "state", "age_band")) %>%
    complete(scenario, omicron_scenario, state, age_band, variant, outcome) %>%
    
    mutate(ve = replace_na(ve, 0),
           coverage = replace_na(coverage, 0),
           m_hosp = 1 - ve * coverage)
}



print("Calculating hosp_effect")


hosp_effect <- ve_tables %>%
  select(date, coverage, ves) %>%
  mutate(
    hosp = pmap(., get_hosp_ves)
  )


age_bands_to_wider <- function(age_group_vacc) {
  case_when(
    age_group_vacc %in% c("0-4") ~ "0-4",
    age_group_vacc %in% c("5-11") ~ "5-11",
    age_group_vacc %in% c("12-15", "16-19") ~ "12-19",
    age_group_vacc %in% c("20-24", "25-29") ~ "20-29",
    age_group_vacc %in% c("30-34", "35-39") ~ "30-39",
    age_group_vacc %in% c("40-44", "45-49") ~ "40-49",
    age_group_vacc %in% c("50-54", "55-59") ~ "50-59",
    age_group_vacc %in% c("60-64", "65-69") ~ "60-69",
    age_group_vacc %in% c("70-74", "75-79") ~ "70-79",
    age_group_vacc %in% c("80+") ~ "80+")
}


print("Plotting...")

plot_data_ves <- hosp_effect %>% 
  select(date, hosp) %>%
  unnest(hosp) %>%
  
  mutate(age_group = age_bands_to_wider(age_band)) %>%
  group_by(variant, date, state, age_group) %>%
  summarise(m_hosp = mean(m_hosp)) %>%
  filter(variant != "Delta") %>%
  mutate(
    data_type = if_else(
      date <= data_date,
      "Actual",
      "Forecast"
    )
  ) %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-11", "12-19", "20-29", "30-39", "40-49", "50-59", 
                                                "60-69", "70-79", "80+")))



plots_common <- list(
  scale_x_date(breaks = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = "3 months"),
               labels = scales::label_date_short(format = c("%Y", "%b")),
               expand = expansion(mult = c(0, 0.05))),
  scale_y_continuous(breaks = scales::extended_breaks(),
                     labels = scales::label_comma(),
                     expand = expansion(mult = c(0, 0.01))),
  theme_minimal(),
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey80", linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(colour = "grey60"),
        axis.ticks.length = unit(5, "pt"),
        axis.line = element_line(colour = "grey40"),
        plot.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        text = element_text(family = "Helvetica"))
)





ggplot(plot_data_ves %>% filter(state == "NSW")) +
  geom_line(aes(x = date, y = m_hosp, linetype = data_type, alpha = variant),
            color = ggokabeito::palette_okabe_ito(5)) +
  
  geom_hline(yintercept = 0, size = 0.8, col = 'grey40')  +
  
  geom_vline(xintercept = ymd("2021-02-22"), size = 0.8, col = 'grey40') +
  coord_cartesian(xlim = c(ymd("2021-02-22"), max(plot_data_ves$date)), ylim = c(0, 1)) +
  
  geom_rug(aes(x = date), tibble(date = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = "month")),
           col = 'grey60') +
  
  facet_wrap(~age_group, ncol = 2) +
  xlab(NULL) + ylab("Change in probability of severe disease") +
  
  scale_linetype_manual("Data type", values = c(1, 2)) +
  scale_alpha_manual("Omicron sub-variant", values = c(0.4, 1))+
  
  plots_common +
  
  theme(legend.position = "bottom")


ggsave(
  filename = "results/extra_figures/vaccine_effect_severe_aged_2022_07_12.png",
  width = 8, height = 8,
  bg = "white"
)
print("Done")
