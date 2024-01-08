sensor_window <- ema_responses_df %>%
  filter(is.na(button_press)) %>%
  distinct(id, EMA_date, EMA_hour, study_day) %>%
  group_by(id, study_day, EMA_date) %>%
  reframe(start_window = as_datetime(paste(EMA_date, min(EMA_hour)), format = "%Y-%m-%d %H"),
            end_window = as_datetime(paste(EMA_date, max(EMA_hour)), format = "%Y-%m-%d %H")) %>%
  distinct() %>%
  group_by(id, study_day) %>%
  summarise(start_window = min(start_window) - hours(1),
            end_window = max(end_window) + hours(1)) %>%
  # for participants with a half hour setting for the EMA, we need to modify their windows
  mutate(start_window = case_when(id %in% half_hour_prompt ~ start_window + minutes(30),
                                  TRUE ~ start_window),
         end_window = case_when(id %in% half_hour_prompt ~ end_window + minutes(30),
                                  TRUE ~ end_window))

