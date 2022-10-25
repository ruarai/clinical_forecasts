
get_public_occupancy <- function(date_forecasting) {
  
  anzic_files <- list.files(
    "/home/forecast/mfluxunimelb/anzics-data",
    full.names = TRUE,
    include.dirs = FALSE,
    recursive = TRUE
  )
  
  read_file <- function(file) {
    file %>%
      readxl::read_xlsx(sheet = 1,
                        range = "B12:P15") %>%
      select(c(1, 3, 5, 7, 9, 11, 13, 15)) %>%
      
      slice(3) %>%
      pivot_longer(everything(), names_to = "state") %>%
      mutate(count = as.numeric(str_split_fixed(value, " \\(", 2)[,1]))
  }
  
  anzics_data <- tibble(
    file = anzic_files
  ) %>%
    mutate(date = dmy(str_extract(file, "\\d{2}.{3}\\d{4}"))) %>%
    filter(date >= ymd("2022-08-01")) %>%
    
    rowwise() %>%
    mutate(
      data = list(read_file(file))
    ) %>%
    unnest(data) %>%
    select(date, state, count)
  
  
  
  anzics_interp <- anzics_data %>%
    complete(
      state,
      date = seq(min(date), max(date), by = "days")
    ) %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(ICU = round(zoo::na.approx(count))) %>%
    ungroup() %>%
    
    select(date, state, ICU)
  
  
  
  
  national_data <- readxl::read_excel(
    "data/occupancy/NAT_2022-04-21_Data for Uni of Melbourne.xlsx",
    sheet = 2
  ) %>%
    `colnames<-`(c("date", "hospitalised", "ICU", "state")) %>%
    mutate(date = as_date(date))
  
  
  anzics_and_NAT <- national_data %>%
    left_join(anzics_interp) %>%
    mutate(ward = hospitalised - ICU) %>%
    
    select(date, state, ward, ICU)  %>%
    pivot_longer(c(ward, ICU), values_to = "count", names_to = "group")  %>%
    
    mutate(source = "nat")
  
  
  scrape <- function(states = NULL) {
    require(rvest)
    
    if (is.null(states)) {
      states <- c("act","nsw","nt","qld","sa","tas","vic","wa")
    }
    #to lower incase input are upper
    states <- tolower(states)
    
    urls <- paste0("https://covidlive.com.au/report/daily-hospitalised/",states)
    
    map_dfr(1:length(states), function(state) {
      url <- urls[state]
      state_ll <- url %>%
        read_html() %>%
        html_nodes(
          "table"
        ) %>%
        .[[2]] %>%
        html_table(
          fill = TRUE
        ) %>%
        mutate(
          date = as.Date(DATE, format = "%d %B %y"),
          hosp = as.numeric(gsub(",","",HOSP)),
          hosp = ifelse(is.na(hosp),0,hosp),
          hosp = ifelse(hosp < 0,0,hosp),
          icu = as.numeric(gsub(",","",ICU)),
          icu = ifelse(is.na(icu),0,icu),
          icu = ifelse(icu < 0,0,icu),
          state = toupper(states[state])
        ) %>%
        arrange(
          date
        ) %>%
        mutate(ward = hosp - icu) %>%
        select(state, date, ward, ICU = icu) %>%
        
        pivot_longer(c("ward", "ICU"),
                     names_to = "group",
                     values_to = "count")
    })
  } 
  
  
  occ_counts_c19data <- scrape() %>%
    mutate(source = "covidlive")
  
  
  occ_counts_QLD_direct <- tribble(
    ~date, ~inpatients, ~icu_nonvent, ~icu_vent,
    "2022-09-10", 176, 3, 3,
    "2022-09-11", 176, 3, 3,
    "2022-09-12", 162, 4, 4,
    "2022-09-13", 159, 5, 3,
    "2022-09-14", 154, 4, 3,
    "2022-09-15", 145, 3, 0,
    "2022-09-16", 142, 5, 1,
    "2022-09-17", 142, 5, 1,
    "2022-09-18", 142, 5, 1,
    "2022-09-19", 142, 6, 0,
    "2022-09-20", 147, 5, 2,
    "2022-09-21", 141, 4, 3,
    "2022-09-22", 141, 4, 3,
    "2022-09-23", 137, 3, 3,
    "2022-09-24", 137, 3, 3,
    "2022-09-25", 137, 3, 3,
    "2022-09-26", 132, 1, 2,
    "2022-09-27", 129, 2, 2,
    "2022-09-28", 116, 4, 1,
    "2022-09-29", 100, 3, 1,
    "2022-09-30", 99, 3, 0,
    "2022-10-01", 99, 3, 0,
    "2022-10-02", 99, 3, 0,
    "2022-10-03", 99, 3, 0,
    "2022-10-04", 96, 1, 1,
    "2022-10-05", 97, 1, 0,
    "2022-10-06", 99, 1, 0,
    "2022-10-07", 94, 1, 1,
    "2022-10-08", 94, 1, 1,
    "2022-10-09", 94, 1, 1,
    "2022-10-10", 85, 2, 0,
    "2022-10-11", 97, 1, 0,
    "2022-10-12", 96, 1, 0,
    "2022-10-13", 90, 2, 1,
    "2022-10-14", 82, 2, 0,
    "2022-10-15", 83, 2, 0,
    "2022-10-16", 83, 2, 0,
    "2022-10-17", 107, 1, 0,
    "2022-10-18", 101, 2, 0,
    "2022-10-19", 91, 2, 0
  ) %>%
    mutate(date = ymd(date) - days(1),
           ICU = icu_nonvent + icu_vent,
           state = "QLD",
           source = "direct") %>%
    select(date, state, ward = inpatients, ICU, source) %>%
    pivot_longer(c(ward, ICU), values_to = "count", names_to = "group") 
  
  read_ACT <- .  %>%
    readxl::read_xlsx(sheet = 2) %>%
    mutate(date = as_date(Date),
           state = "ACT",
           source = "direct") %>%
    select(date, state, ward = Ward, ICU, source) %>%
    pivot_longer(c(ward, ICU), values_to = "count", names_to = "group") 
  
  
  ACT_data_direct <- map_dfr(
    c(
      "data/occupancy/ACT_06-10-2022 Hospital Data.xlsx",
      "data/occupancy/ACT_12-10-2022 Hospital Data.xlsx"
    ),
    read_ACT
  )
  
  
  TAS_data_direct <- bind_rows(
    read_csv("data/occupancy/TAS_Daily_Hospitalisations_TAS_REV20221014.csv") %>%
      mutate(date = dmy(current_as_at),
             ward = Hospitalised - ICU,
             state = "TAS",
             source = "direct") %>%
      select(date, state, ward, ICU = ICU, source) %>%
      pivot_longer(c(ward, ICU), values_to = "count", names_to = "group"),
    
    read_csv("data/occupancy/TAS_20221019 Commonwealth Weekly Report Tasmania.csv") %>%
      mutate(date = dmy(current_as_at),
             ward = hospitalised - hospitalised_icu,
             ICU = hospitalised_icu,
             state = "TAS",
             source = "direct") %>%
      select(date, state, ward, ICU = ICU, source) %>%
      pivot_longer(c(ward, ICU), values_to = "count", names_to = "group")
  )
  
  NSW_data_direct <- read_csv("~/source/email_digester/downloads/nsw_summary_counts/nsw_data_20221016.csv") %>%
    mutate(date = dmy(date),
           state = "NSW",
           source = "direct") %>%
    select(date, state, ward, ICU, source) %>%
    pivot_longer(c(ward, ICU), values_to = "count", names_to = "group")  %>%
    drop_na(count)
  
  
  
  drop_count <- function(date, state) {
    case_when(
      state == "VIC" ~ FALSE,
      state == "QLD" & date >= ymd("2022-09-09") ~ TRUE,
      state == "ACT" & date >= ymd("2022-09-01") ~ TRUE,
      state == "TAS" & date >= ymd("2022-09-09") ~ TRUE,
      state == "NSW" & date >= ymd("2021-11-01") ~ TRUE,
      #date >= ymd("2022-09-09") & wday(date) != 6 ~ TRUE,
      TRUE ~ FALSE
    )
  }
  
  
  full_occ <- occ_counts_c19data %>%
    
    mutate(
      count = if_else(
        (date >= ymd("2022-09-09") & wday(date) != 6) | date > today() - days(7),
        NA_real_,
        count
      )
    ) %>%
    drop_na(count) %>%
    
    bind_rows(occ_counts_QLD_direct, ACT_data_direct, TAS_data_direct, NSW_data_direct, anzics_and_NAT) %>%
    
    group_by(state, date) %>%
    mutate(any_direct = any(source == "direct"),
           any_nat = any(source == "nat")) %>%
    
    filter(!(source == "covidlive" & (any_direct | any_nat)),
           !(source == "direct" & any_nat)) %>%
    ungroup()
  
  
  p <- full_occ %>%
    filter(date >= today() - days(60), group == "ward") %>%
    ggplot() +
    geom_point(aes(x = date, y = count, colour = source)) +
    
    facet_wrap(~state, scales = "free_y")
  
  
  write_csv(full_occ, str_c("data/occupancy/compiled/compiled_", today(), ".csv"))
  
  full_occ
}

