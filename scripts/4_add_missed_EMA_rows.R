# use start date/end date and start prompt time/end prompt time for each participant; if an expected entry is missing, add an NA row
# loop through for each participant
# also manages one participant who altered their start prompt time/end prompt time during the study period

n_emas = 16
n_study_days = 10
ema_list_df <- demographics %>%
  filter(id %in% ema_df$id) %>%
  group_by(id)

ema_list <- ema_list_df %>%
  group_split()

names(ema_list) <- ema_list_df %>%
  group_keys() %>%
  pull(id)

# Participant p95 received some EMA prompts before the hour mark
shift_EMA_response = c("p95")

# Participants that set prompts to half hours
half_hour_prompt = c("p101", "p17", "p44", "p61", "p97")

altered_ema <- demographics %>%
  filter(altered_prompt == "yes")

ema_responses_df <- lapply(ema_list, function(x) {

  if(!x$id %in% altered_ema$id) {

    pid = unique(x$id)
    start_date <- as_date(x$start_datetime[x$id == pid])
    start_time <- hour(x$start_datetime[x$id == pid])

    if(start_time >= 9) {

      n_study_days <- n_study_days + 1

    }

    ema_days <- seq(from = start_date, to = start_date + (n_study_days - 1), by = 1)
    ema_hours <- seq(from = start_time, to = start_time + (n_emas - 1), by = 1)

    ema_prompt_df <- crossing(ema_days, ema_hours)
    ema_prompt_df$ema_days[ema_prompt_df$ema_hours >= 24] <- ema_prompt_df$ema_days[ema_prompt_df$ema_hours >= 24] + 1
    ema_prompt_df$ema_hours[ema_prompt_df$ema_hours >= 24] <- ema_prompt_df$ema_hours[ema_prompt_df$ema_hours >= 24] - 24

    if(n_study_days > 10) {

      ema_prompt_df <- ema_prompt_df[1:160, ]

    }

    ema_prompt_df$study_day <- rep(1:10, each = 16)

    if(pid %in% shift_EMA_response) {

      fix_ema <- ema_df %>%
        filter(id == pid) %>%
        drop_na(lapse_event) %>%
        filter(!str_detect(button_press, "button")| is.na(button_press)) %>%
        mutate(EMA_date = as_date(EMA_date_time),
               EMA_hour = hour(EMA_date_time),
               EMA_minute = minute(EMA_date_time))

      fix_ema$EMA_date_time[fix_ema$EMA_minute >= 30] <- round(as_datetime(fix_ema$EMA_date_time[fix_ema$EMA_minute >= 30], tz = "UTC"), units = "hours")

      expanded_ema <- fix_ema %>%
        ungroup() %>%
        mutate(EMA_date = as_date(EMA_date_time),
               EMA_hour = hour(EMA_date_time)) %>%
        right_join(., ema_prompt_df, by = c("EMA_date" = "ema_days", "EMA_hour" = "ema_hours")) %>%
        arrange(EMA_date, EMA_hour) %>%
        mutate(EMA_number = row_number()) %>%
        bind_rows(ema_df %>%
                    filter(id == pid) %>%
                    drop_na(lapse_event) %>%
                    filter(str_detect(button_press, "button")) %>%
                    mutate(EMA_date = as_date(EMA_date_time),
                           EMA_hour = hour(EMA_date_time))) %>%
        arrange(EMA_date, EMA_hour) %>%
        fill(id, .direction = c("updown")) %>%
        relocate(c("EMA_date", "EMA_hour", "study_day"), .after = "id")

      return(expanded_ema)

    } else {

    expanded_ema <- ema_df %>%
      filter(id == pid) %>%
      drop_na(lapse_event) %>%
      filter(!str_detect(button_press, "button")| is.na(button_press)) %>%
      mutate(EMA_date = as_date(EMA_date_time),
             EMA_hour = hour(EMA_date_time)) %>%
      right_join(., ema_prompt_df, by = c("EMA_date" = "ema_days", "EMA_hour" = "ema_hours")) %>%
      arrange(EMA_date, EMA_hour) %>%
      mutate(EMA_number = row_number()) %>%
      bind_rows(ema_df %>%
                  filter(id == paste0("p", pid)) %>%
                  drop_na(lapse_event) %>%
                  filter(str_detect(button_press, "button")) %>%
                  mutate(EMA_date = as_date(EMA_date_time),
                         EMA_hour = hour(EMA_date_time))) %>%
      arrange(EMA_date, EMA_hour) %>%
      fill(id, .direction = c("updown")) %>%
      relocate(c("EMA_date", "EMA_hour", "study_day"), .after = "id")

    return(expanded_ema)

    }

  } else {

    pid = unique(x$id)
    start_date <- as_date(x$start_datetime[x$id == pid])
    start_time <- hour(x$start_datetime[x$id == pid])
    x$altered_prompt_start_time <- with_tz(as_datetime(paste(x$altered_prompt_study_day, hour(x$altered_prompt_start_time)), format = "%Y-%m-%d %H", tz = "Europe/London"), tz = "UTC")
    altered_date <- as_date(x$altered_prompt_study_day[x$id == pid])
    altered_time <- hour(x$altered_prompt_start_time[x$id == pid])

    ema_days <- seq(from = start_date, to = start_date + (n_study_days - 1), by = 1)

    ema_hours <- seq(from = start_time, to = start_time + (n_emas - 1), by = 1)
    altered_ema_hours <- seq(from = altered_time, to = altered_time + (n_emas - 1), by = 1)

    ema_prompt_df <- bind_rows(crossing(ema_days[ema_days < altered_date], ema_hours),
                               crossing(ema_days[ema_days >= altered_date], altered_ema_hours)) %>%
      rename(ema_days_1 = 1, ema_days_2 = 3,
             ema_hours_1 = 2, ema_hours_2 = 4) %>%
      mutate(ema_days = coalesce(ema_days_1, ema_days_2),
             ema_hours = coalesce(ema_hours_1, ema_hours_2)) %>%
      select(ema_days, ema_hours)

    ema_prompt_df$study_day <- rep(1:10, each = 16)

    expanded_ema <- ema_df %>%
      filter(id == pid) %>%
      drop_na(lapse_event) %>%
      filter(!str_detect(button_press, "button")| is.na(button_press)) %>%
      mutate(EMA_date = as_date(EMA_date_time),
             EMA_hour = hour(EMA_date_time)) %>%
      right_join(., ema_prompt_df, by = c("EMA_date" = "ema_days", "EMA_hour" = "ema_hours")) %>%
      arrange(EMA_date, EMA_hour) %>%
      mutate(EMA_number = row_number()) %>%
      bind_rows(ema_df %>%
                  filter(id == paste0("p", pid)) %>%
                  drop_na(lapse_event) %>%
                  filter(str_detect(button_press, "button")) %>%
                  mutate(EMA_date = as_date(EMA_date_time),
                         EMA_hour = hour(EMA_date_time))) %>%
      arrange(EMA_date, EMA_hour) %>%
      fill(id, .direction = c("updown")) %>%
      relocate(c("EMA_date", "EMA_hour", "study_day"), .after = "id")

    return(expanded_ema)
  }

}) %>%
  bind_rows() %>%
  mutate(EMA_sent_datetime = parse_date_time(paste(EMA_date, EMA_hour), "Y-m-d H")) %>%
  mutate(time_of_day = as.factor(case_when(EMA_hour >= 5 & EMA_hour <= 9 ~ "morning",
                                           EMA_hour >= 10 & EMA_hour <= 14 ~ "midday",
                                           EMA_hour >= 15 & EMA_hour <= 19 ~ "afternoon",
                                           EMA_hour >= 20 & EMA_hour <= 24 ~ "evening",
                                           EMA_hour >= 0 & EMA_hour <= 4 ~ "night"))) %>%
  filter(!(id == "p22" & EMA_number == 126)) # Participant had a second response 14 mins after the first, remove this second one

# Renumber p22s EMAs after removing the duplicate
ema_responses_df$EMA_number[ema_responses_df$id == "p22" & ema_responses_df$EMA_number >= 126] <- ema_responses_df$EMA_number[ema_responses_df$id == "p22" & ema_responses_df$EMA_number >= 126] - 1
# Change the sent time for participants that had set EMAs for 30 minute mark
ema_responses_df$EMA_sent_datetime[ema_responses_df$id %in% half_hour_prompt] <- ema_responses_df$EMA_sent_datetime[ema_responses_df$id %in% half_hour_prompt] + minutes(30)


button_press <- ema_df %>%
  filter(str_detect(button_press, "button")) %>%
  mutate(EMA_date = as_date(EMA_date_time),
         EMA_hour = hour(EMA_date_time)) %>%
  arrange(EMA_date, EMA_hour) %>%
  relocate(c("EMA_date", "EMA_hour")) %>%
  group_by(id) %>%
  group_split() %>%
  lapply(., function(x) {
    
    # Produce a dataframe for each participant listing the study date and start and end time of EMA
    daily_ema_period <- ema_responses_df %>%
      filter(id == unique(x$id)) %>%
      select(id, EMA_date, EMA_sent_datetime) %>%
      group_by(id, EMA_date) %>%
      summarise(EMA_start = min(EMA_sent_datetime),
                EMA_end = max(EMA_sent_datetime))
    
    button_list <- x %>%
      group_by(EMA_date_time) %>%
      group_split() %>% # Each button press is an element of a participant specific list
      lapply(., function(y) {
        
        # Check to see if the button press was on a day of the study
        # If it, was check to see if it occurred between the first and last EMA of the study
        # Label button presses that occurred between these timepoints as TRUE or FALSE
        if(y$EMA_date %in% daily_ema_period$EMA_date) {
          
        y %>%
          mutate(include = case_when(EMA_date_time >= daily_ema_period %>%
                                       filter(EMA_date == y$EMA_date) %>%
                                       pull(EMA_start) & EMA_date_time <= daily_ema_period %>%
                                       filter(EMA_date == y$EMA_date) %>%
                                       pull(EMA_end) ~ TRUE,
                                     TRUE ~ FALSE))
        } else {
        # If the button press was outside of the study days it is not included
          y %>% 
            mutate(include = FALSE)
          
        }
        
      }) %>%
      bind_rows() %>% # Bind the button presses for a participant
      filter(include == TRUE) %>% # Only include the ones that fall within the EMA time periods
      select(-include)
  }) %>%
  bind_rows() # Bind the button presses for all participants

# bring button presses into the response df
ema_responses_df <- bind_rows(ema_responses_df,
                              button_press) %>%
  arrange(id, EMA_date, EMA_hour) %>%
  fill(study_day, .direction = "downup") %>%
  relocate(c("id", "EMA_date", "EMA_hour", "EMA_number"))

# write to rds

write_rds(ema_responses_df, here("data", "ema_responses_df.rds"))
