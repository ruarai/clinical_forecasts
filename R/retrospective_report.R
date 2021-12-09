
validation_runs <- c(
  "NSW-validation-2021-11-08",
  "NSW-validation-2021-11-16",
  "NSW-prod-2021-11-25",
  "NSW-prod-2021-11-30",
  "NSW-prod-2021-12-07_NNDSS"
)

report_dir <- str_c("results/", "NSW_retro_", today())

dir.create(report_dir,
           showWarnings = FALSE)

sim_dates <- str_c(
  "results/",
  validation_runs,
  "/input/local_cases_input.csv"
) %>% map(read_csv) %>%
  map(function(local_cases) {
    local_cases %>%
      filter(detection_probability >= 0.5) %>%
      pull(date_onset) %>% max()
  }) %>%
  do.call(c, .)


result_files <- str_c(
  "results/",
  validation_runs,
  "/data/sim_results.rds"
)

data_all <- result_files %>% map(read_rds)

counts_all <- 1:length(validation_runs) %>%
  map_dfr(function(i) {
    data_all[[i]] %>%
      pluck("tbl_count_grouped_quants") %>%
      mutate(data_date = ymd(str_extract(validation_runs[i], "\\d{4}-\\d{2}-\\d{2}")),
             forecast_date = sim_dates[i])  
  })





clinical_linelist <- read_rds("results/NSW-prod-2021-12-07/input/clinical_linelist.rds")
clinical_linelist_raw <- readxl::read_excel(
  "/usr/local/forecasting/linelist_data/NSW/NSW_out_episode_2021_12_07.xlsx",
  sheet = 2
  )


days <- seq(min(clinical_linelist$dt_hosp_admission, na.rm = TRUE),
            max(clinical_linelist$dt_hosp_discharge, na.rm = TRUE),
            by ='days') %>% as_date()


linelist_data_counts <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward = clinical_linelist %>%
           filter(dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
                  dt_hosp_admission <= date,
                  
                  is.na(dt_first_icu) | dt_first_icu >= date | dt_last_icu <= date) %>%
           nrow(),
         
         count_ICU = clinical_linelist %>%
           drop_na(dt_first_icu) %>%
           filter(dt_last_icu >= date | is.na(dt_last_icu),
                  dt_first_icu <= date) %>%
           nrow()) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")


linelist_data_counts_raw <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward = clinical_linelist_raw %>%
           filter(discharge_date_dt >= date | is.na(discharge_date_dt),
                  admit_date_dt <= date,
                  
                  is.na(first_icu_date_dt) | first_icu_date_dt >= date | last_icu_date_dt <= date) %>%
           nrow(),
         
         count_ICU = clinical_linelist_raw %>%
           drop_na(first_icu_date_dt) %>%
           filter(last_icu_date_dt >= date | is.na(last_icu_date_dt),
                  first_icu_date_dt <= date) %>%
           nrow()) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")

p_ICU <- ggplot(counts_all %>%
         filter(group == "ICU")) +
  geom_ribbon(aes(x = date,
                  ymin = lower, ymax = upper,
                  group = quant),
              fill = "green4",
              alpha = 0.2) +
  
  geom_vline(aes(xintercept = forecast_date),
             linetype = 'dotted') +
  
  geom_line(aes(x = date, y = count),
            linelist_data_counts_raw %>% filter(group == "ICU")) +
  
  facet_grid(rows = vars(forecast_date)) +
  
  coord_cartesian(xlim = c(ymd("2021-11-01", NA)),
                  ylim = c(0, 120)) +
  
  scale_x_date(breaks = scales::breaks_width("1 month"),
               labels = scales::label_date_short()) +
  
  scale_y_continuous(breaks = scales::breaks_extended()) +
  
  xlab("Date") + ylab("") +
  
  ggtitle("", "ICU") +
  
  theme_minimal() +
  theme(axis.title.y = element_blank())


p_ward <- ggplot(counts_all %>%
         filter(group == "ward")) +
  geom_ribbon(aes(x = date,
                  ymin = lower, ymax = upper,
                  group = quant),
              fill = "darkorchid",
              alpha = 0.2) +
  
  geom_vline(aes(xintercept = forecast_date),
             linetype = 'dotted') +
  
  geom_line(aes(x = date, y = count),
            linelist_data_counts_raw %>% filter(group == "ward")) +
  
  facet_grid(rows = vars(forecast_date)) +
  
  coord_cartesian(xlim = c(ymd("2021-11-01", NA)),
                  ylim = c(0, 400)) +
  
  scale_x_date(breaks = scales::breaks_width("1 month"),
               labels = scales::label_date_short()) +
  
  scale_y_continuous(breaks = scales::breaks_extended()) +
  
  xlab("Date") + ylab("Count") +
  
  ggtitle("Occupancy Counts", "Ward") +
  
  theme_minimal() +
  theme(strip.text = element_blank())

cowplot::plot_grid(p_ward, p_ICU, nrow = 1)


scale <- 6
ggsave(paste0(report_dir, "/occupancy_validation_raw.png"),
       bg = 'white',
       width = 2 * scale, height = 1 * scale,
       dpi = 200)

