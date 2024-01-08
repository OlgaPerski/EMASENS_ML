# identify heart rate files for each participant

heart_rate_files <- list.files(path = here("data"), pattern = "heart_rate", recursive = TRUE)

# subset the heart rate files to match EMA prompt dates for each participant

heart_rate_file_df <- tibble(participant_id = str_split(heart_rate_files, "/", simplify = TRUE)[,1],
                             filename = heart_rate_files,
                             filedate = as.Date(
                               str_remove(
                                 str_remove(
                                   str_split(heart_rate_files, "/", simplify = TRUE)[,2], pattern = "heart_rate-"),
                                 pattern = ".json"))) %>%
  right_join(ppt_start_date, by = c("participant_id", "filedate" = "date")) %>%
  mutate(participant_id = factor(participant_id)) %>%
  drop_na(filename)

# create list of .json files for each participant

heart_json_list <- as.list(heart_rate_file_df$filename)

# read these .json files in and set as a data frame

heart_json_data <- lapply(heart_json_list, function(x) {read_json(path = here("data", x), simplifyVector = TRUE) %>%
    as.data.frame() %>%
    unnest(value) %>%
    select(dateTime, bpm)})

# only retain heart rate data that correspond to participants' EMA dates

heart_rate_df <- heart_json_data %>%
  bind_rows(.id = "file_id") %>%
  left_join(heart_rate_file_df %>%
              mutate(file_id = as.character(row_number())) %>%
              select(participant_id, file_id),
            by = c("file_id")) %>%
  select(participant_id, date_time = dateTime, bpm) %>%
  # current assumption is that this .json data is in UTC format, explicitly set this
  mutate(date_time = parse_date_time(date_time, orders = "%m %d %y %H:%M:%S", tz = "UTC")) %>%
  # round hr data to 5 second intervals
  mutate(rounded_datetime = round_date(as_datetime(date_time), "5 secs"),
         rounded_datetime = as_datetime(rounded_datetime, tz = "UTC")) %>%
  group_by(rounded_datetime, participant_id) %>%
  summarise(bpm  = round(mean(bpm), 1)) %>%
  ungroup()

# expand the data frame for all 5 second intervals during the study period
# 5 second intervals without HR are assigned NAs

hr_df <- heart_rate_df %>%
  group_by(participant_id) %>%
  group_split() %>%
  lapply(., function(x) {

    # produce an interval for the start and end of the window by study day
    ppt_window = sensor_window %>%
      filter(id %in% x$participant_id) %>%
      mutate(window_interval = interval(start_window, end_window))

    # produce 5 second intervals for each of these timepoints
    all_timepoints <- ppt_window %>%
      select(-id, -window_interval) %>%
      group_by(study_day) %>%
      pivot_longer(names_to = "name", cols = c(start_window, end_window), values_to = "expanded_datetime") %>%
      complete(expanded_datetime = seq(min(expanded_datetime), max(expanded_datetime), by = "5 sec")) %>%
      mutate(expanded_datetime = as_datetime(expanded_datetime, tz = "UTC")) %>%
      ungroup() %>%
      select(study_day, expanded_datetime)

    # convert to a list to use in the subsequent filter
    window = as.list(ppt_window$window_interval)

    # filter data to within the intervals and right join to 5 second interval dataset
    x %>%
      filter(rounded_datetime %within% window) %>%
      right_join(all_timepoints, by = c("rounded_datetime" = "expanded_datetime")) %>%
      arrange(rounded_datetime) %>%
      fill(participant_id, .direction = "downup")

  }) %>%
  bind_rows()

hr_df_comp <- hr_df %>%
  group_by(participant_id) %>%
  summarise(bpm_not_na = sum(!is.na(bpm)),
            bpm_is_na = sum(is.na(bpm))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(total_rows = sum(bpm_not_na, bpm_is_na))

# write to rds

write_rds(hr_df, here("data", "heart_rate_df.rds"))
