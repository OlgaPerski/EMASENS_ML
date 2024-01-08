# time stamps for the sensor data are set to participants' Fitbit timezone setting (BST or GMT, depending on time of recruitment) but the data export is adjusted to UTC (BST-1)
# a 1-hour timezone offset is therefore needed to match the EMA prompts for participants recruited during BST

# create EMA dictionary ---------------------------------------------------

ema_imp <- read_rds(here("data", "ema_imputed_df.rds")) %>%
  select(c(id,
           EMA_sent_datetime,
           EMA_date,
           "EMA_response_datetime" = EMA_date_time,
           EMA_hour,
           EMA_minute,
           EMA_number,
           study_day,
           time_of_day,
           "EMA_record" = time,
           excited_imp,
           cigarette_availability_imp,
           calm_imp,
           bored_imp,
           enthusiastic_imp,
           irritable_imp,
           anxious_imp,
           contented_imp,
           lapse_event_imp,
           stressed_imp,
           sad_imp,
           motivation_imp,
           craving_imp,
           caffeine_imp,
           happy_imp,
           confidence_imp,
           pain_imp,
           alcohol_imp,
           nicotine_imp,
           location_home_imp,
           location_school_work_imp,
           location_outside_imp,
           location_restaurant_imp,
           location_public_place_imp,
           location_public_transport_imp,
           location_private_vehicle_imp,
           location_others_home_imp,
           location_other_imp,
           activity_eating_imp,
           activity_tv_imp,
           activity_music_imp,
           activity_reading_imp,
           activity_working_imp,
           activity_walking_imp,
           activity_child_care_imp,
           activity_socialising_imp,
           activity_social_media_imp,
           activity_relaxing_imp,
           activity_chores_imp,
           activity_other_imp,
           social_context_alone_imp,
           social_context_partner_imp,
           social_context_friend_imp,
           social_context_child_imp,
           social_context_relative_imp,
           social_context_colleague_imp,
           social_context_stranger_imp,
           social_context_other_imp)) %>%
  mutate(EMA_id = paste0(id, "_", EMA_record))

EMA_dictionary <- ema_imp %>%
  select(EMA_id, id, EMA_sent_datetime, EMA_response_datetime) %>%
  mutate(window_end = coalesce(EMA_response_datetime, EMA_sent_datetime), # window end refers to the point at which data are available if they responded to the prompt, or the time the prompt was sent if they did not respond
         window_end = with_tz(window_end, tzone = "UTC"), # convert timezone to UTC
         prediction_distance_1 = window_end - minutes(15), # 15 min prior, ignore data between this point and the EMA response for the 1st prediction distance
         primary_timewindow_1 = prediction_distance_1 - minutes(5),  # 5 min between this point and prediction_distance will be used for feature extraction
         secondary_timewindow_1 = prediction_distance_1 - minutes(10), # 10 min prior
         tertiary_timewindow_1 = prediction_distance_1 - minutes(15),
         prediction_distance_2 = window_end - minutes(30),
         primary_timewindow_2 = prediction_distance_2 - minutes(5),
         secondary_timewindow_2 = prediction_distance_2 - minutes(10),
         tertiary_timewindow_2 = prediction_distance_2 - minutes(15),
         prediction_distance_3 = window_end - minutes(45),
         primary_timewindow_3 = prediction_distance_3 - minutes(5),
         secondary_timewindow_3 = prediction_distance_3 - minutes(10),
         tertiary_timewindow_3 = prediction_distance_3 - minutes(15))

# create HR dictionary ----------------------------------------------------

hr_df <- read_rds(here("data", "heart_rate_df.rds"))

HR_dictionary <- hr_df %>%
  group_by(participant_id) %>%
  mutate(HR_record = row_number(),
         HR_id = paste0(participant_id, "_", HR_record)) %>%
  ungroup() %>%
  select(id = participant_id, HR_id, "HR_datetime" = rounded_datetime, "bpm" = bpm)

# primary prediction distance (15 minutes) --------------------------------

prediction_distance_1 <- list()

# primary prediction distance window HR match -----------------------------

prediction_distance_1 <- list(
  primary_HR_match = interval_inner_join(HR_dictionary %>%
                                           select(id, HR_id, HR_datetime), # take the df with HR unique record IDs
                                         EMA_dictionary %>%
                                           select(id, EMA_id, primary_timewindow_1, prediction_distance_1), # take the df with EMA unique record IDs
                                         by = c("HR_datetime" = "primary_timewindow_1", "HR_datetime" = "prediction_distance_1")) %>% # join these by matching the HR record time to within the primary window
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    mutate(EMA_id = fct_inorder(EMA_id)) %>%
    group_by(EMA_id) %>%
    slice_head(n = 60) %>% # time window is 5 minutes so we only want 60 datapoints
    ungroup(),
  # secondary time window HR match
  secondary_HR_match = interval_inner_join(HR_dictionary %>%
                                             select(id, HR_id, HR_datetime),
                                           EMA_dictionary %>%
                                             select(id, EMA_id, secondary_timewindow_1, prediction_distance_1),
                                           by = c("HR_datetime" = "secondary_timewindow_1", "HR_datetime" = "prediction_distance_1")) %>%
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    mutate(EMA_id = fct_inorder(EMA_id)) %>%
    group_by(EMA_id) %>%
    slice_head(n = 120) %>% # time window is 10 minutes so we only want 120 datapoints
    ungroup(),
  # tertiary time window HR match
  tertiary_HR_match = interval_inner_join(HR_dictionary %>%
                                            select(id, HR_id, HR_datetime),
                                          EMA_dictionary %>%
                                            select(id, EMA_id, tertiary_timewindow_1, prediction_distance_1),
                                          by = c("HR_datetime" = "tertiary_timewindow_1", "HR_datetime" = "prediction_distance_1")) %>%
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    mutate(EMA_id = fct_inorder(EMA_id)) %>%
    group_by(EMA_id) %>%
    slice_head(n = 180) %>% # time window is 15 minutes so we only want 180 datapoints
    ungroup())

# secondary prediction distance (30 minutes) --------------------------------

prediction_distance_2 <- list()

# secondary prediction distance window HR match -----------------------------

prediction_distance_2 <- list(
  primary_HR_match = interval_inner_join(HR_dictionary %>%
                                           select(id, HR_id, HR_datetime), # take the df with HR unique record IDs
                                         EMA_dictionary %>%
                                           select(id, EMA_id, primary_timewindow_2, prediction_distance_2), # take the df with EMA unique record IDs
                                         by = c("HR_datetime" = "primary_timewindow_2", "HR_datetime" = "prediction_distance_2")) %>% # join these by matching the HR record time to within the primary window
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    mutate(EMA_id = fct_inorder(EMA_id)) %>%
    group_by(EMA_id) %>%
    slice_head(n = 60) %>% # time window is 5 minutes so we only want 60 datapoints
    ungroup(),
  # secondary time window HR match
  secondary_HR_match = interval_inner_join(HR_dictionary %>%
                                             select(id, HR_id, HR_datetime),
                                           EMA_dictionary %>%
                                             select(id, EMA_id, secondary_timewindow_2, prediction_distance_2),
                                           by = c("HR_datetime" = "secondary_timewindow_2", "HR_datetime" = "prediction_distance_2")) %>%
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    group_by(EMA_id) %>%
    slice_head(n = 120) %>% # time window is 10 minutes so we only want 120 datapoints
    ungroup(),
  # tertiary time window HR match
  tertiary_HR_match = interval_inner_join(HR_dictionary %>%
                                            select(id, HR_id, HR_datetime),
                                          EMA_dictionary %>%
                                            select(id, EMA_id, tertiary_timewindow_2, prediction_distance_2),
                                          by = c("HR_datetime" = "tertiary_timewindow_2", "HR_datetime" = "prediction_distance_2")) %>%
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    mutate(EMA_id = fct_inorder(EMA_id)) %>%
    group_by(EMA_id) %>%
    slice_head(n = 180) %>% # time window is 15 minutes so we only want 180 datapoints
    ungroup())

# tertiary prediction distance (45 minutes) --------------------------------

prediction_distance_3 <- list()

# tertiary prediction distance window HR match -----------------------------

prediction_distance_3 <- list(
  primary_HR_match = interval_inner_join(HR_dictionary %>%
                                           select(id, HR_id, HR_datetime), # take the df with HR unique record IDs
                                         EMA_dictionary %>%
                                           select(id, EMA_id, primary_timewindow_3, prediction_distance_3), # take the df with EMA unique record IDs
                                         by = c("HR_datetime" = "primary_timewindow_3", "HR_datetime" = "prediction_distance_3")) %>% # join these by matching the HR record time to within the primary window
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    mutate(EMA_id = fct_inorder(EMA_id)) %>%
    group_by(EMA_id) %>%
    slice_head(n = 60) %>% # time window is 5 minutes so we only want 60 datapoints
    ungroup(),
  # secondary time window HR match
  secondary_HR_match = interval_inner_join(HR_dictionary %>%
                                             select(id, HR_id, HR_datetime),
                                           EMA_dictionary %>%
                                             select(id, EMA_id, secondary_timewindow_3, prediction_distance_3),
                                           by = c("HR_datetime" = "secondary_timewindow_3", "HR_datetime" = "prediction_distance_3")) %>%
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    group_by(EMA_id) %>%
    slice_head(n = 120) %>% # time window is 10 minutes so we only want 120 datapoints
    ungroup(),
  # tertiary time window HR match
  tertiary_HR_match = interval_inner_join(HR_dictionary %>%
                                            select(id, HR_id, HR_datetime),
                                          EMA_dictionary %>%
                                            select(id, EMA_id, tertiary_timewindow_3, prediction_distance_3),
                                          by = c("HR_datetime" = "tertiary_timewindow_3", "HR_datetime" = "prediction_distance_3")) %>%
    filter(id.x == id.y) %>%
    drop_na(EMA_id) %>%
    select(HR_id, EMA_id) %>%
    group_by(EMA_id) %>%
    slice_head(n = 180) %>% # time window is 15 minutes so we only want 180 datapoints
    ungroup())

# a single EMA record may be associated with multiple HR records as the windows overlap
# this is likely only related to button presses for the small windows

# create steps dictionary -------------------------------------------------

steps_df <- read_rds(here("data", "steps_df.rds"))

steps_dictionary <- steps_df %>%
  group_by(id = participant_id) %>%
  mutate(step_record = row_number(),
         step_id = paste0(id, "_", step_record)) %>%
  ungroup() %>%
  select(id, step_id, "step_datetime" = date_time, "steps" = steps)

# primary prediction distance (15 minutes) --------------------------------

prediction_distance_1$primary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                  select(id, step_id, step_datetime),
                                                                EMA_dictionary %>%
                                                                  select(id, EMA_id, primary_timewindow_1, prediction_distance_1),
                                                                by = c("step_datetime" = "primary_timewindow_1", "step_datetime" = "prediction_distance_1")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 5) %>% # time window is 5 minutes so we only want 5 datapoints
  ungroup()

prediction_distance_1$secondary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                    select(id, step_id, step_datetime),
                                                                  EMA_dictionary %>%
                                                                    select(id, EMA_id, secondary_timewindow_1, prediction_distance_1),
                                                                  by = c("step_datetime" = "secondary_timewindow_1", "step_datetime" = "prediction_distance_1")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 10) %>% # time window is 10 minutes so we only want 10 datapoints
  ungroup()

prediction_distance_1$tertiary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                   select(id, step_id, step_datetime),
                                                                 EMA_dictionary %>%
                                                                   select(id, EMA_id, tertiary_timewindow_1, prediction_distance_1),
                                                                 by = c("step_datetime" = "tertiary_timewindow_1", "step_datetime" = "prediction_distance_1")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 15) %>% # time window is 15 minutes so we only want 15 datapoints
  ungroup()

# secondary prediction distance (30 minutes) --------------------------------

prediction_distance_2$primary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                  select(id, step_id, step_datetime),
                                                                EMA_dictionary %>%
                                                                  select(id, EMA_id, primary_timewindow_2, prediction_distance_2),
                                                                by = c("step_datetime" = "primary_timewindow_2", "step_datetime" = "prediction_distance_2")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 5) %>% # time window is 5 minutes so we only want 5 datapoints
  ungroup()

prediction_distance_2$secondary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                    select(id, step_id, step_datetime),
                                                                  EMA_dictionary %>%
                                                                    select(id, EMA_id, secondary_timewindow_2, prediction_distance_2),
                                                                  by = c("step_datetime" = "secondary_timewindow_2", "step_datetime" = "prediction_distance_2")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 10) %>% # time window is 10 minutes so we only want 10 datapoints
  ungroup()

prediction_distance_2$tertiary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                   select(id, step_id, step_datetime),
                                                                 EMA_dictionary %>%
                                                                   select(id, EMA_id, tertiary_timewindow_2, prediction_distance_2),
                                                                 by = c("step_datetime" = "tertiary_timewindow_2", "step_datetime" = "prediction_distance_2")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 15) %>% # time window is 15 minutes so we only want 15 datapoints
  ungroup()

# tertiary prediction distance (45 minutes) --------------------------------

prediction_distance_3$primary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                  select(id, step_id, step_datetime),
                                                                EMA_dictionary %>%
                                                                  select(id, EMA_id, primary_timewindow_3, prediction_distance_3),
                                                                by = c("step_datetime" = "primary_timewindow_3", "step_datetime" = "prediction_distance_3")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 5) %>% # time window is 5 minutes so we only want 5 datapoints
  ungroup()

prediction_distance_3$secondary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                    select(id, step_id, step_datetime),
                                                                  EMA_dictionary %>%
                                                                    select(id, EMA_id, secondary_timewindow_3, prediction_distance_3),
                                                                  by = c("step_datetime" = "secondary_timewindow_3", "step_datetime" = "prediction_distance_3")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 10) %>% # time window is 10 minutes so we only want 10 datapoints
  ungroup()

prediction_distance_3$tertiary_step_match <- interval_inner_join(steps_dictionary %>%
                                                                   select(id, step_id, step_datetime),
                                                                 EMA_dictionary %>%
                                                                   select(id, EMA_id, tertiary_timewindow_3, prediction_distance_3),
                                                                 by = c("step_datetime" = "tertiary_timewindow_3", "step_datetime" = "prediction_distance_3")) %>%
  filter(id.x == id.y) %>%
  drop_na(EMA_id) %>%
  select(step_id, EMA_id) %>%
  group_by(EMA_id) %>%
  slice_head(n = 15) %>% # time window is 15 minutes so we only want 15 datapoints
  ungroup()

# dictionary lists --------------------------------------------------------

dictionary_list <- list(EMA_dictionary = EMA_dictionary,
                        Prediction_distance_1 = prediction_distance_1,
                        Prediction_distance_2 = prediction_distance_2,
                        Prediction_distance_3 = prediction_distance_3)

write_rds(EMA_dictionary, here("data", "EMA_dictionary.rds"))

write_rds(ema_imp %>%
             select(id, EMA_id, EMA_date, EMA_hour, EMA_number, study_day, EMA_date_time = EMA_response_datetime, any_of(contains("_imp"))),
          here("data", "EMA_join_df.rds"))
