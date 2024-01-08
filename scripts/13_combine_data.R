EMA_dictionary <- read_rds(here("data", "EMA_dictionary.rds"))

ema_df <- read_rds(here("data", "EMA_join_df.rds"))

feature_df <- read_rds(here("data", "feature_df.rds"))

included_ppt_demographics <- read_rds(here("data", "demographics.rds")) %>%
  filter(id %in% unique(ema_df$id))

ema_responses_df <- read_rds(here("data", "ema_responses_df.rds"))

# remove ppts with too much EMA missingness

ema_prop_compl <- ema_responses_df %>%
  filter(is.na(button_press)) %>%
  group_by(id) %>%
  summarise(prop_completed = sum(!is.na(EMA_date_time))/160*100) %>%
  drop_na()

exclude_ids <- ema_prop_compl %>%
  filter(prop_completed < 60)

# remove ppts with too much sensor missingness

hr_df <- read_rds(here("data", "heart_rate_df.rds"))

include_ids_hr <- hr_df %>%
  group_by(participant_id, study_day) %>%
  summarise(bpm_na = sum(is.na(bpm)),
            bpm_not_na = sum(!is.na(bpm))) %>%
  rowwise() %>%
  mutate(n_expected = sum(bpm_na, bpm_not_na),
         prop_bpm = bpm_not_na/n_expected) %>%
  filter(prop_bpm >= 0.2) %>%
  group_by(participant_id) %>%
  mutate(n_days_wear = n()) %>%
  filter(n_days_wear >= 5) %>%
  distinct(participant_id)

exclude_ids_hr <- unique(included_ppt_demographics$id)[!unique(included_ppt_demographics$id) %in% include_ids_hr$participant_id]

# total sample - 46 ppts

total_sample <- left_join(included_ppt_demographics,
                          ema_df, by = c("id")) %>%
  mutate(id = factor(id, levels = paste0("p", sort(unique(as.numeric(str_remove(id, "p")))))),
         lapse_prior = na_replace(lag(lapse_event_imp), 0),
         lapse_lagged = lead(lapse_event_imp),
         lapse_lagged = replace_na(lapse_lagged, 0))

write_rds(total_sample, here("data", "total_sample.rds"))

# analytic sample 1 (demographics and EMAs, no sensor data) - 38 ppts

analytic_sample_demographics <- included_ppt_demographics %>%
  filter(!id %in% exclude_ids$id)

analytic_sample_1 <- left_join(analytic_sample_demographics,
                               ema_df, by = c("id")) %>%
  group_by(id) %>%
  mutate(id = factor(id, levels = paste0("p", sort(unique(as.numeric(str_remove(id, "p")))))),
         lapse_prior = na_replace(lag(lapse_event_imp), 0),
         lapse_lagged = lead(lapse_event_imp),
         lapse_lagged = replace_na(lapse_lagged, 0))

write_rds(analytic_sample_1, here("data", "analytic_sample_1.rds"))

# analytic sample 2 (demographics, EMAs and sensor data) - 30 ppts

sensor_sample_demographics <- analytic_sample_demographics %>%
  filter(!id %in% exclude_ids_hr)

analytic_sample_2 <- left_join(sensor_sample_demographics,
                               ema_df,
                               by = c("id")) %>%
  left_join(feature_df, by = c("id", "EMA_id")) %>%
  group_by(id) %>%
  mutate(id = factor(id, levels = paste0("p", sort(unique(as.numeric(str_remove(id, "p")))))),
         lapse_prior = na_replace(lag(lapse_event_imp), 0),
         lapse_lagged = lead(lapse_event_imp),
         lapse_lagged = replace_na(lapse_lagged, 0))

write_rds(analytic_sample_2, here("data", "analytic_sample_2.rds"))
