
library(targets)
library(tidyverse)
library(lubridate)

print("Loading vaccine state")

vaccine_state <- tar_read(vaccination_data, store = "_long/")
data_date <- ymd("2022-07-12")

state_population_aged <- vaccine_state %>%
  filter(scenario == max(scenario)) %>%
  group_by(age_band, state) %>%
  summarise(
    population = sum(num_people, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(state) %>%
  mutate(prop_age = population / sum(population))


state_population <- vaccine_state %>%
  filter(scenario == max(scenario)) %>%
  group_by(state) %>%
  summarise(
    population = sum(num_people, na.rm = TRUE),
    .groups = "drop"
  )

date_sequence <- seq.Date(
  from = as.Date("2021-10-22"),
  #to = as.Date("2021-10-22") + weeks(8),
  to = data_date + weeks(16),
  by = "1 week"
)#[63:66]


vaccine_state <- vaccine_state %>%
  filter(state == "VIC")

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
    filter(outcome == "hospitalisation" | outcome == "acquisition") %>%
    
    pivot_wider(names_from = outcome,
                values_from = ve) %>%
    
    left_join(coverage, by = c("scenario", "state", "age_band")) %>%
    complete(scenario, omicron_scenario, state, age_band, variant) %>%
    
    mutate(ve = replace_na(hospitalisation, 0),
           
           hospitalisation = 1 - (1 - hospitalisation) / (1 - acquisition),
           hospitalisation = replace_na(hospitalisation, 0),
           
           coverage = replace_na(coverage, 0),
           m_hosp = 1 - hospitalisation * coverage,
           m_hosp_old = 1 - ve * coverage,
           ) %>%

    left_join(state_population_aged, by = c("age_band", "state")) %>%

    group_by(scenario, omicron_scenario, variant, state) %>%
    summarise(m_hosp = sum(m_hosp * prop_age),
              m_hosp_old = sum(m_hosp_old * prop_age),
              .groups = "drop")
}


hosp_effect <- ve_tables %>%
  select(date, coverage, ves) %>%
  mutate(
    hosp = pmap(., get_hosp_ves)
  )


plot_data_ves <- hosp_effect %>% 
  select(date, hosp) %>%
  unnest(hosp) %>%
  filter(variant != "Delta") %>%
  mutate(
    data_type = if_else(
      date <= data_date,
      "Actual",
      "Forecast"
    )
  )


plots_common <- list(
  scale_x_date(breaks = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = "3 months"),
               labels = scales::label_date_short(format = c("%Y", "%b")),
               expand = expansion(mult = c(0, 0.05))),
  scale_y_continuous(breaks = scales::extended_breaks(),
                     labels = scales::label_comma(),
                     expand = expansion(mult = c(0, 0))),
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

ggplot(plot_data_ves) +
  geom_line(aes(x = date, y = m_hosp, alpha = variant, linetype = data_type),
            color = ggokabeito::palette_okabe_ito(5)) +
  geom_line(aes(x = date, y = m_hosp_old, alpha = variant, linetype = data_type),
            color = ggokabeito::palette_okabe_ito(3)) +
  
  geom_hline(yintercept = 0, size = 0.8, col = 'grey40')  +
  
  geom_vline(xintercept = ymd("2021-02-22"), size = 0.8, col = 'grey40') +
  coord_cartesian(xlim = c(ymd("2021-02-22"), max(plot_data_ves$date)), ylim = c(0, 1)) +
  
  geom_rug(aes(x = date), tibble(date = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = "month")),
           col = 'grey60') +
  
  facet_wrap(~state, ncol = 2) +
  xlab(NULL) + ylab("Change in probability of severe disease") +
  
  scale_linetype_manual("Data type", values = c(1, 3)) +
  scale_alpha_manual("Omicron sub-variant", values = c(0.4, 1)) +
  
  plots_common +
  
  theme(legend.position = "bottom")

ggsave(
  filename = "results/extra_figures/vaccine_effect_hosp_2022_07_12.png",
  width = 8, height = 8,
  bg = "white"
)



local_cases <- read_csv("~/mfluxunimelb/local_cases_input/local_cases_input_2022-07-13.csv") %>%
  select(
    date = date_onset,
    state,
    cases = count
  ) %>%
  filter(date <= data_date)


ascertainment_rates <- c(0.5, 0.75)

omicron_infections_all <- reff_env$get_omicron_infections(
  local_cases,
  ascertainment_rates,
  state_population
)




states <- unique(state_population$state)

all_ie_tables <- map(
  states,
  function(i_state) {
    
    omicron_infections_state <- omicron_infections_all %>%
      unnest(omicron_infections) %>%
      filter(state == i_state) %>%
      nest(omicron_infections = c(date, state, num_people))
    
    ie_tables <- tibble(
      date = seq.Date(
        from = as.Date("2021-12-07"),
        to = data_date + weeks(16),
        by = "1 week"
      ) - 1
    ) %>%
      expand_grid(omicron_infections_state) %>%
      left_join(
        y = ve_tables,
        by = "date"
      ) %>%
      rename(cohorts_vaccination = cohorts) %>%
      mutate(
        cohorts_infection = map2(
          .x = omicron_infections,
          .y = date,
          .f = reff_env$get_infection_cohorts_at_date
        ),
        coverage_infection = map(
          .x = cohorts_infection,
          .f = reff_env$get_coverage_infection
        ),
        ies = map(
          .x = cohorts_infection,
          .f = reff_env$get_infection_efficacies_infection_only
        ),
        vies = map2(
          .x = cohorts_vaccination,
          .y = cohorts_infection,
          .f = reff_env$get_infection_efficacies_vax
        )
      )
    
    ie_tables
  }
)


rm(list = "ve_tables")


all_ie_tbl <- all_ie_tables %>%
  `names<-`(states) %>%
  bind_rows(.id = "state")




get_hosp_vies <- function(state, coverage, ves, coverage_infection, ies, vies, ...) {
  
  
  coverage %>%
    filter(state == !!state) %>%
    left_join(ves %>% filter(state == !!state, variant != "Delta"), by = c("scenario", "state", "age_band")) %>%
    rename(coverage_vacc = coverage,
           effect_vacc = ve) %>%
    filter(outcome == "hospitalisation") %>%
    complete(scenario, omicron_scenario, state, age_band, variant, outcome) %>%
    
    left_join(coverage_infection %>% rename(coverage_inf = coverage), by = "state") %>%
    left_join(ies %>% rename(effect_inf = ve), by = c("omicron_scenario", "state", "variant", "outcome")) %>%
    left_join(vies %>% rename(effect_both = ve), by = c("scenario", "omicron_scenario", "state", "age_band", "variant", "outcome")) %>%
    mutate(coverage_vacc = replace_na(coverage_vacc, 0),
           effect_vacc = replace_na(effect_vacc, 0),
           effect_both = replace_na(effect_both, 0)) %>%
    
    mutate(p_vacc_only = coverage_vacc * (1 - coverage_inf),
           p_inf_only = coverage_inf * (1 - coverage_vacc),
           p_both = coverage_inf * coverage_vacc) %>%
    
    mutate(m_hosp = 1 - (p_vacc_only * effect_vacc + p_inf_only * effect_inf + p_both * effect_both)) %>%
    
    left_join(state_population_aged, by = c("age_band", "state")) %>%
    
    group_by(scenario, omicron_scenario, variant, state) %>%
    summarise(m_hosp = sum(m_hosp * prop_age), .groups = "drop")
}


all_effects <- all_ie_tbl %>%
  select(date, state, ascertainment, coverage, ves, coverage_infection, ies, vies) %>%
  mutate(effect = pmap(., get_hosp_vies))


plot_data_vies <- all_effects %>%
  select(date, ascertainment, effect) %>%
  unnest(effect) %>%
  filter(date <= data_date)

# 
# plot_data_ves <- read_rds("plot_data_ves.rds")
# plot_data_vies <- read_rds("plot_data_vies.rds")

ggplot() +
  geom_line(aes(x = date, y = m_hosp, linetype = variant, alpha = factor(ascertainment)),
            color = ggokabeito::palette_okabe_ito(5),
            plot_data_vies)  +
  geom_line(aes(x = date, y = m_hosp, linetype = variant),
            color = 'black',
            plot_data_ves %>% filter(date <= data_date,
                                     date <= ymd("2021-12-06")))  +
  
  geom_hline(yintercept = 0, size = 0.8, col = 'grey40')  +
  
  geom_vline(xintercept = ymd("2021-02-22"), size = 0.8, col = 'grey40') +
  coord_cartesian(xlim = c(ymd("2021-02-22"), data_date), ylim = c(0, 1)) +
  
  geom_rug(aes(x = date), tibble(date = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = "month")),
           col = 'grey60') +
  
  facet_wrap(~state, ncol = 2) +
  xlab(NULL) + ylab("Change in probability of severe disease") +
  
  plots_common +
  
  scale_linetype_manual("Omicron sub-variant", values = c(2, 1)) +
  scale_alpha_manual("Ascertainment", values = c(0.5, 1)) +
  theme(legend.position = "bottom")

ggsave(
  filename = "results/extra_figures/combined_immunity_effect_hosp_2022_07_12.png",
  width = 8, height = 8,
  bg = "white"
)




  