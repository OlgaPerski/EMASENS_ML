# identify step count files for each participant
step_files <- list.files(path = here("data"), pattern = "steps-", recursive = TRUE)

# convert the step count file names to allow to matching to EMA prompt dates for each participant
step_file_df <- tibble(participant_id = str_split(step_files, "/", simplify = TRUE)[, 1],
                       filename = step_files,
                       filedate = as.Date(
                         str_remove(
                           str_remove(
                             str_split(step_files, "/", simplify = TRUE)[, 2], pattern = "steps-"),
                           pattern = ".json")))

# create list of .json files for each participant
step_json_list <- as.list(step_file_df$filename)

# read these .json files in and set as a dataframe
step_json_data <- lapply(step_json_list, function(x) {read_json(path = here("data", x), simplifyVector = TRUE) %>%
    as.data.frame() %>%
    unnest(value) %>%
    select(dateTime, value)})

# associate the step count data with the participant ID and format date_time
steps <- step_json_data %>%
  bind_rows(.id = "file_id") %>%
  left_join(step_file_df %>%
              mutate(file_id = as.character(row_number())) %>%
              select(participant_id, file_id),
            by = c("file_id")) %>%
  select(participant_id, date_time = dateTime, value) %>%
  # current assumption is that this .json data is in UTC format, explicitly set this
  mutate(date_time = parse_date_time(date_time, orders = "%m %d %y %H:%M:%S", tz = "UTC"),
         date = as.Date(date_time)) %>%
  rename(steps = value)

steps_df <- steps %>%
  group_by(participant_id) %>%
  group_split() %>%
  lapply(., function(x) {

    # produce an interval for the start and end of the window by study day
    ppt_window = sensor_window %>%
      filter(id %in% x$participant_id) %>%
      mutate(window_interval = interval(start_window, end_window))

    # produce 1 minute intervals for each of these timepoints
    all_timepoints <- ppt_window %>%
      select(-id, -window_interval) %>%
      group_by(study_day) %>%
      pivot_longer(names_to = "name", cols = c(start_window, end_window), values_to = "expanded_datetime") %>%
      complete(expanded_datetime = seq(min(expanded_datetime), max(expanded_datetime), by = "1 min")) %>%
      mutate(expanded_datetime = as_datetime(expanded_datetime, tz = "UTC")) %>%
      ungroup() %>%
      select(study_day, expanded_datetime)

    # convert to a list to use in the subsequent filter
    window = as.list(ppt_window$window_interval)

    # filter data to within the intervals and right join to 1 minute interval dataset
    x %>%
      filter(date_time %within% window) %>%
      right_join(all_timepoints, by = c("date_time" = "expanded_datetime")) %>%
      arrange(date_time) %>%
      fill(participant_id, .direction = "downup")

  }) %>%
  bind_rows() %>%
  drop_na(participant_id)

step_df_comp <- steps_df %>%
  group_by(participant_id) %>%
  summarise(steps_not_na = sum(!is.na(steps)),
            steps_is_na = sum(is.na(steps))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(total_rows = sum(steps_not_na, steps_is_na))

# write to rds

write_rds(steps_df, here("data", "steps_df.rds"))
