# remove ppts with too much missingness

include_ids_hr <- hr_df %>%
  group_by(participant_id, study_day) %>%
  summarise(bpm_na = sum(is.na(bpm)),
            bpm_not_na = sum(!is.na(bpm))) %>%
  rowwise() %>%
  mutate(n_expected = sum(bpm_na, bpm_not_na),
         prop_bpm = bpm_not_na/n_expected) %>%
  filter(prop_bpm >= 0.2) %>% # may want to increase this to 0.5
  group_by(participant_id) %>%
  mutate(n_days_wear = n()) %>%
  filter(n_days_wear >= 5) %>%
  distinct(participant_id)

exclude_ids_hr <- unique(demographics$id)[!unique(demographics$id) %in% include_ids_hr$participant_id]

write_rds(hr_df %>%
            group_by(participant_id, study_day) %>%
            summarise(bpm_na = sum(is.na(bpm)),
                      bpm_not_na = sum(!is.na(bpm))) %>%
            rowwise() %>%
            mutate(n_expected = sum(bpm_na, bpm_not_na),
                   prop_bpm = bpm_not_na/n_expected) %>%
            group_by(participant_id) %>%
            mutate(n_days_wear = n(),
                   include = case_when(participant_id %in% exclude_ids_hr ~ "exclude",
                                       TRUE ~ "include")),
          here("data", "hr_with_prop_complete.rds"))

# HR imputation -----------------------------------------------------------
# imputation windows are 5, 10 and 15 minutes of data

if(!file.exists(here("data", "hr_imputed_list.rds"))) {

  impute_within_window_hr <- function(df = hr_df, windows = imputation_windows) {

    df %>%
      filter(participant_id %in% include_ids_hr$participant_id) %>%
      group_by(participant_id) %>%
      mutate(HR_record = row_number(),
             HR_id = paste0(participant_id, "_", HR_record)) %>%
      left_join(windows, by = c("HR_id")) %>%
      drop_na(EMA_id) %>%
      group_by(EMA_id) %>%
      group_split() %>%
      lapply(function(x) {

        if(sum(!is.na(x$bpm)) >= 3)

          x %>%
          select(participant_id, HR_id, EMA_id, rounded_datetime, bpm) %>%
          mutate(bpm_imp = round(na_kalman(bpm), 0),
                 bpm_imp = case_when(bpm_imp <= 40 ~ 40, # setting the lower limit of heart rate to 40, assuming anything below this is a measurement error from the device
                                     bpm_imp >= 220 ~ 220, # setting the upper limit of heart rate to 220
                                       TRUE ~ bpm_imp))

        else

          x %>%
          select(participant_id, HR_id, EMA_id, rounded_datetime, bpm) %>%
          mutate(bpm_imp = NA_integer_)

      }) %>%
      bind_rows()

  }

  hr_imputed_pd_1 <- mapply(function(x, i)

    impute_within_window_hr(df = hr_df, windows = x) %>%
      mutate(time_window = i,
             prediction_distance = "15 minutes"),

    x = dictionary_list$Prediction_distance_1[1:3],
    i = names(dictionary_list$Prediction_distance_1[1:3]),
    SIMPLIFY = FALSE)

  hr_imputed_pd_2 <- mapply(function(x, i)

    impute_within_window_hr(df = hr_df, windows = x) %>%
      mutate(time_window = i,
             prediction_distance = "30 minutes"),

    x = dictionary_list$Prediction_distance_2[1:3],
    i = names(dictionary_list$Prediction_distance_1[1:3]),
    SIMPLIFY = FALSE)

  hr_imputed_pd_3 <- mapply(function(x, i)

    impute_within_window_hr(df = hr_df, windows = x) %>%
      mutate(time_window = i,
             prediction_distance = "45 minutes"),

    x = dictionary_list$Prediction_distance_3[1:3],
    i = names(dictionary_list$Prediction_distance_3[1:3]),
    SIMPLIFY = FALSE)

  # write to rds
  hr_imputed_list <- list(prediction_distance_1 = hr_imputed_pd_1,
                          prediction_distance_2 = hr_imputed_pd_2,
                          prediction_distance_3 = hr_imputed_pd_3)

  write_rds(hr_imputed_list, here("data", "hr_imputed_list.rds"))

} else {
  
  hr_imputed_list <- read_rds(here("data", "hr_imputed_list.rds"))
  
}

# visualise HR imputation

check_imputation <- function(df = pred_dist_wind_period) {
  
  count_imputed <- df %>%
    group_by(participant_id) %>%
    summarise(values = sum(!is.na(bpm)),
              imputed = sum(!is.na(bpm_imp)),
              all_timepoints = n()) %>%
    mutate(n_imputed = imputed-values)
  
  # check the number of timepoints for each particpant
  
  visualise_imputed <- df %>%
    group_by(participant_id) %>%
    group_split() %>%
    lapply(., function(x) x %>%
             ungroup() %>%
             pivot_longer(cols = c("bpm", "bpm_imp"), values_to = "bpm", names_to = "method") %>%
             ggplot() +
             geom_line(aes(x = rounded_datetime, y = bpm, colour = method, alpha = method), lwd = 0.8) +
             scale_colour_manual(values = c("red", "black")) +
             scale_alpha_manual(values = c(1, 0.4)) +
             theme_bw() +
             labs(x = "Date",
                  y = "BPM",
                  title = paste0(unique(x$participant_id), ": PD = ", unique(x$prediction_distance), ", window = ", if(str_detect(unique(x$time_window), "primary")) {
                    "5 min" } else 
                      if(str_detect(unique(x$time_window), "secondary")) {
                      "10 min" } else 
                        "15 min")))
  
  return(list(
    imputation_summary = count_imputed,
    imputation_plots = visualise_imputed)
    )
}

first_pred_window_visualise <- lapply(hr_imputed_list$prediction_distance_1, check_imputation)
second_pred_window_visualise <- lapply(hr_imputed_list$prediction_distance_2, check_imputation)
third_pred_window_visualise <- lapply(hr_imputed_list$prediction_distance_3, check_imputation)

# steps imputation --------------------------------------------------------

steps_df <- read_rds(here("data", "steps_df.rds")) %>%
  mutate(dup_time = date_time,
         steps = as.numeric(steps))

# create a complete time stamp sequence with 1 min intervals and merge with the original data, filling in missing with NA

s_df <- demographics %>%
  select(id, start_datetime, end_datetime) %>%
  mutate(end_datetime = case_when(format(end_datetime, "%H:%M:%S") == "00:00:00" ~ end_datetime + days(1),
                                  format(end_datetime, "%H:%M:%S") == "01:00:00" ~ end_datetime + days(1),
                                  format(end_datetime, "%H:%M:%S") == "05:00:00" ~ end_datetime + days(1),
                                  TRUE ~ end_datetime)) %>%
  pivot_longer(names_to = "name", cols = c(start_datetime, end_datetime), values_to = "study_datetime") %>%
  group_by(id) %>%
  complete(study_datetime = seq(min(study_datetime), max(study_datetime), by = "1 min")) %>%
  mutate(study_datetime = as_datetime(study_datetime, tz = "UTC"),
         dup_time = study_datetime) %>%
  select(id, study_datetime, dup_time) %>%
  ungroup() %>%
  left_join(., steps_df, by = c("id" = "participant_id", "dup_time"))

# repeat for steps

s_df_comp <- s_df %>%
  group_by(id, study_day) %>%
  summarise(steps_not_na = sum(!is.na(steps)),
            steps_na = sum(is.na(steps)),
            prop_steps = steps_not_na/sum(steps_not_na, steps_na))

write_rds(s_df %>%
            group_by(id, study_day) %>%
            summarise(steps_not_na = sum(!is.na(steps)),
                      steps_na = sum(is.na(steps)),
                      prop_steps = steps_not_na/sum(steps_not_na, steps_na)), here("data", "steps_with_prop_complete.rds"))

ggplot(s_df_comp) +
  aes(x = id, y = prop_steps) +
  geom_col(colour = "black") +
  geom_hline(yintercept = 50, colour = "red") +
  geom_hline(yintercept = 60, colour = "orange") +
  geom_hline(yintercept = 70, colour = "green")

# univariate kalman filter

if(!file.exists(here("data", "s_imputed_list.rds"))) {

  impute_within_window_s <- function(df = s_df, windows = imputation_windows) {

    df %>%
      filter(!id %in% exclude_ids_hr) %>% # do not impute step data for those excluded based on HR adherence
      group_by(id) %>%
      select(id, study_datetime, steps) %>%
      mutate(step_record = row_number(),
             step_id = paste0(id, "_", step_record)) %>%
      left_join(windows, by = c("step_id")) %>%
      drop_na(EMA_id) %>%
      group_by(EMA_id) %>%
      group_split() %>%
      lapply(function(x) {

        if(sum(!is.na(x$steps)) >= 3)

          x %>%
          select(id, step_id, EMA_id, study_datetime, steps) %>%
          mutate(steps_imp = round(na_kalman(steps), 0),
                 steps_imp = case_when(steps_imp <= 0 ~ 0,
                                       steps_imp >= 190 ~ 190,
                                       TRUE ~ steps_imp),
                 steps_imp = coalesce(steps_imp, steps))

        else

          x %>%
          select(id, step_id, EMA_id, study_datetime, steps) %>%
          mutate(steps_imp = NA_integer_)

      }) %>%
      bind_rows()

  }

  s_imputed_pd_1 <- mapply(function(x, i)

    impute_within_window_s(df = s_df, windows = x) %>%
      mutate(time_window = i,
             prediction_distance = "15 minutes"),

    x = dictionary_list$Prediction_distance_1[4:6],
    i = names(dictionary_list$Prediction_distance_1[4:6]),
    SIMPLIFY = FALSE)

  s_imputed_pd_2 <- mapply(function(x, i)

    impute_within_window_s(df = s_df, windows = x) %>%
      mutate(time_window = i,
             prediction_distance = "30 minutes"),

    x = dictionary_list$Prediction_distance_2[4:6],
    i = names(dictionary_list$Prediction_distance_2[4:6]),
    SIMPLIFY = FALSE)

  s_imputed_pd_3 <- mapply(function(x, i)

    impute_within_window_s(df = s_df, windows = x) %>%
      mutate(time_window = i,
             prediction_distance = "45 minutes"),

    x = dictionary_list$Prediction_distance_3[4:6],
    i = names(dictionary_list$Prediction_distance_3[4:6]),
    SIMPLIFY = FALSE)

  # write to rds

  s_imputed_list <- list(prediction_distance_1 = s_imputed_pd_1,
                         prediction_distance_2 = s_imputed_pd_2,
                         prediction_distance_3 = s_imputed_pd_3)

  write_rds(s_imputed_list, here("data", "s_imputed_list.rds"))

} else {
  
  s_imputed_list <- read_rds(here("data", "s_imputed_list.rds"))
  
}

# visualise steps imputation

check_steps_imputation <- function(df = pred_dist_wind_period) {
  
  count_imputed <- df %>%
    group_by(id) %>%
    summarise(values = sum(!is.na(steps)),
              imputed = sum(!is.na(steps_imp)),
              all_timepoints = n()) %>%
    mutate(n_imputed = imputed-values)
  
  # check the number of timepoints for each particpant
  
  visualise_imputed <- df %>%
    group_by(id) %>%
    group_split() %>%
    lapply(., function(x) x %>%
             ungroup() %>%
             pivot_longer(cols = c("steps", "steps_imp"), values_to = "steps", names_to = "method") %>%
             ggplot() +
             geom_line(aes(x = study_datetime, y = steps, colour = method, alpha = method), lwd = 0.8) +
             scale_colour_manual(values = c("red", "black")) +
             scale_alpha_manual(values = c(1, 0.4)) +
             theme_bw() +
             labs(x = "Date",
                  y = "Steps",
                  title = paste0(unique(x$id), ": PD = ", unique(x$prediction_distance), ", window = ", if(str_detect(unique(x$time_window), "primary")) {
                    "5 min" } else 
                      if(str_detect(unique(x$time_window), "secondary")) {
                        "10 min" } else 
                          "15 min")))
  
  return(list(
    imputation_summary = count_imputed,
    imputation_plots = visualise_imputed)
  )
}

first_pred_window_visualise_steps <- lapply(s_imputed_list$prediction_distance_1, check_steps_imputation)
second_pred_window_visualise <- lapply(hr_imputed_list$prediction_distance_2, check_imputation)
third_pred_window_visualise <- lapply(hr_imputed_list$prediction_distance_3, check_imputation)

# summarise the number of datapoints, EMAs and button presses for each participant by prediction window and time window

data_summary <- lapply(hr_imputed_list, function(x) {
  
  bind_rows(x) %>%
    group_by(participant_id, time_window, prediction_distance) %>%
    summarise(n_HR = n(),
              n_HR_NA = sum(is.na(bpm_imp)),
              n_EMA_hr = length(unique(EMA_id))) %>%
    mutate(time_window = str_split(time_window, "_", simplify = TRUE)[,1])
  
}) %>%
  bind_rows() %>%
  ungroup() %>%
  left_join(lapply(s_imputed_list, function(x) {
    
    bind_rows(x) %>%
      group_by(id, time_window, prediction_distance) %>%
      summarise(n_s = n(),
                n_s_NA = sum(is.na(steps_imp)),
                n_EMA_s = length(unique(EMA_id))) %>%
      rename(participant_id = id) %>%
      mutate(time_window = str_split(time_window, "_", simplify = TRUE)[,1])
    
  }) %>%
    bind_rows() %>%
    ungroup(),
  by = c("participant_id", "time_window", "prediction_distance")) %>%
  arrange(participant_id, prediction_distance, time_window)

# reshape dataframe

data_summary %>%
  rowwise() %>%
  mutate(ema_button = max(n_EMA_hr - 160, n_EMA_s - 160),
         ema = 160) %>%
  pivot_longer(cols = c("n_HR", "n_s"), values_to = "n_rows", names_to = "sensor") %>%
  mutate(sensor = str_remove(sensor, "n_"),
         `NA_real` = case_when(sensor == "HR" ~ n_HR_NA,
                               sensor == "s" ~ n_s_NA)) %>%
  distinct(prediction_distance, time_window, sensor, n_rows, `NA_real`, ema_button, ema, participant_id) %>%
  arrange(participant_id, prediction_distance, sensor, time_window) %>%
  write_csv(here("data", "imputation_completeness_windows.csv"))
