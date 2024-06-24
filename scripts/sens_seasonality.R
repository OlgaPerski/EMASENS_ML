# select variables to include ---------------------------------------------

analytic_sample_1 <- read_rds(here("data", "analytic_sample_1.rds")) 

analytic_sample_clean <- analytic_sample_1 %>%
  mutate(sex = as.factor(sex),
         post_16_edu = as.factor(post_16_edu),
         ethnicity = as.factor(ethnicity),
         past_quit_attempt = as.factor(past_quit_attempt),
         job_type = factor(job_type, levels = c("Non-manual", "Manual", "Other (e.g., student, unemployed, retired)")),
         time_to_first_cig = factor(time_to_first_cig, levels = c("Within 5 minutes", "6-30 minutes",
                                                                  "31-60 minutes", "After 60 minutes")),
         motivation_to_stop = case_when(motivation_to_stop == "I want to stop smoking but haven't though about when" ~
                                          "I want to stop smoking but haven't thought about when",
                                        TRUE ~ motivation_to_stop),
         motivation_to_stop = factor(motivation_to_stop, levels = c("I don't want to stop smoking",
                                                                    "I think I should stop smoking but don't really want to",
                                                                    "I want to stop smoking but haven't thought about when",
                                                                    "I really want to stop smoking but don't know when I will",
                                                                    "I want to stop smoking and hope to soon",
                                                                    "I really want to stop smoking and intend to in the next 3 months",
                                                                    "I really want to stop smoking and intend to in the next month")),
         pharma_support = as.factor(case_when(nrt_no_presc == "yes" | nrt_presc == "yes" | zyban == "yes" | varen == "yes" | ecig == "yes" ~ "yes",
                                              TRUE ~ "no")),
         beh_support = as.factor(case_when(group_supp == "yes" | ind_supp == "yes" | helpline == "yes" | book == "yes" | website == "yes" ~ "yes",
                                           TRUE ~ "no")),
         lapse_lagged = as.factor(case_when(lapse_lagged == 0 ~ "no",
                                            TRUE ~ "yes")),
         cigarette_availability_imp = as.factor(case_when(cigarette_availability_imp == 1 ~ "Not available",
                                                          cigarette_availability_imp == 2 ~ "Available with difficulty",
                                                          TRUE ~ "Easily available")),
         caffeine_imp = as.factor(caffeine_imp),
         alcohol_imp = as.factor(alcohol_imp),
         nicotine_imp = as.factor(nicotine_imp),
         location_home_imp = as.factor(location_home_imp),
         location_school_work_imp = as.factor(location_school_work_imp),
         location_outside_imp = as.factor(location_outside_imp),
         location_restaurant_imp = as.factor(location_restaurant_imp),
         location_public_place_imp = as.factor(location_public_place_imp),
         location_public_transport_imp = as.factor(location_public_transport_imp),
         location_private_vehicle_imp = as.factor(location_private_vehicle_imp),
         location_others_home_imp = as.factor(location_others_home_imp),
         location_other_imp = as.factor(location_other_imp),
         activity_eating_imp = as.factor(activity_eating_imp),
         activity_tv_imp = as.factor(activity_tv_imp),
         activity_music_imp = as.factor(activity_music_imp),
         activity_reading_imp = as.factor(activity_reading_imp),
         activity_working_imp = as.factor(activity_working_imp),
         activity_walking_imp = as.factor(activity_walking_imp),
         activity_child_care_imp = as.factor(activity_child_care_imp),
         activity_socialising_imp = as.factor(activity_socialising_imp),
         activity_social_media_imp = as.factor(activity_social_media_imp),
         activity_relaxing_imp = as.factor(activity_relaxing_imp),
         activity_chores_imp = as.factor(activity_chores_imp),
         activity_other_imp = as.factor(activity_other_imp),
         social_context_alone_imp = as.factor(social_context_alone_imp),
         social_context_partner_imp = as.factor(social_context_partner_imp),
         social_context_friend_imp = as.factor(social_context_friend_imp),
         social_context_child_imp = as.factor(social_context_child_imp),
         social_context_relative_imp = as.factor(social_context_relative_imp),
         social_context_colleague_imp = as.factor(social_context_colleague_imp),
         social_context_stranger_imp = as.factor(social_context_stranger_imp),
         social_context_other_imp = as.factor(social_context_other_imp),
         lapse_prior = as.factor(lapse_prior),
         time_of_day = as.factor(case_when(EMA_hour >= 5 & EMA_hour < 11 ~ "morning",
                                           EMA_hour >= 11 & EMA_hour < 17 ~ "midday",
                                           EMA_hour >= 17 & EMA_hour < 22 ~ "evening",
                                           EMA_hour >= 22 & EMA_hour < 0 ~ "night",
                                           EMA_hour >= 0 & EMA_hour < 5 ~ "night",
                                           TRUE ~ "night")),
         season = month(start_date))

exclude <- c("nr", "id", "EMA_id", "EMA_hour", "lapse_event_imp", "nrt_no_presc", "nrt_presc", "zyban", "varen",
             "ecig", "group_supp", "ind_supp", "helpline", "book", "website",
             "app", "none", "other", "mpath_data", "fitbit_data", "start_date",
             "time_first_prompt", "end_date", "time_last_prompt", "participant_specific_variable_1",
             "participant_specific_variable_2", "last_time_smoke_fup", "CO_reading", "CO_reading1",
             "CO_reading2", "altered_prompt", "altered_prompt_start_time", "altered_prompt_study_day",
             "start_datetime", "end_datetime", "smoking_fup", "EMA_date", "EMA_number", "EMA_date_time",
             "location_others_home_imp", "activity_child_care_imp", "social_context_friend_imp", "social_context_child_imp", # remove pred vars with no variability
             "social_context_colleague_imp", "social_context_stranger_imp") 

df <- analytic_sample_clean %>%
  select(-all_of(exclude)) %>%
  drop_na(lapse_lagged)

df$lapse_lagged <- relevel(df$lapse_lagged, "yes")
df$time_of_day <- factor(df$time_of_day, levels = c("morning", "midday", "evening", "night"))
df$season <- as.factor(case_when(df$season == 12 | df$season == 1 | df$season == 2 ~ "winter",
                                 df$season == 3 | df$season == 4 | df$season == 5 ~ "spring",
                                 df$season == 6 | df$season == 7 | df$season == 8 ~ "summer",
                                 df$season == 9 | df$season == 10 | df$season == 11 ~ "autumn",
                                 TRUE ~ "winter"))

# split data into training and testing ------------------------------------

set.seed(555)

data_split <- initial_split(df, prop = .80)
train_data <- training(data_split)
test_data  <- testing(data_split)

train_data %>%
  count(lapse_lagged) %>%
  mutate(prop = n/sum(n)) # check target class balance

# set up recipe -----------------------------------------------------------

rec <- recipe(lapse_lagged ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(age, cpd) %>%
  step_downsample(lapse_lagged, skip = TRUE) # manages class imbalances through downsampling and skips this step during testing

rf_prep <- prep(rec)
rf_juiced <- juice(rf_prep)

rf_juiced %>%
  count(lapse_lagged) %>%
  mutate(prop = n/sum(n)) # check target class balance

# set up tuning grids -----------------------------------------------------

### random forest

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# set up workflow --------------------------------------------------------

### random forest

tune_wf_rf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_spec)

# tune hyperparameters ----------------------------------------------------

folds <- vfold_cv(train_data, v = 10)

### random forest

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% collect_metrics()
rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)

# plot variable feature importance ----------------------------------------------

### random forest

vip_rf <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(lapse_lagged ~ .,
      data = juice(rf_prep)) %>%
  vip()

vip_rf <- vip_rf$data %>%
  mutate(Variable = str_remove(Variable, "_imp"),
         Variable = str_replace(Variable, "_", " "),
         Variable = str_remove(Variable, "_X1"),
         Variable = str_to_sentence(Variable),
         Variable = str_replace(Variable, "Cpd", "CPD"),
         Variable = str_replace(Variable, "Lapse prior", "Prior event lapse"),
         Variable = str_replace(Variable, "Motivation to_stop_i.think.i.should.stop.smoking.but.don.t.really.want.to", 
                                "Motivation to stop - I think I should stop smoking but don't really want to"),
         Variable = str_replace(Variable, "Cigarette availability_not.available", "Cigarette availability - Not available"),
         Variable = str_replace(Variable, "Cigarette availability_easily.available", "Cigarette availability - Easily available"),
         Variable = str_replace(Variable, "Past quit_attempt_yes..in.the.past.year", "Past quit attempt - Yes, in the past year"),
         Variable = str_replace(Variable, "Pharma support_yes", "Pharmacological support - Yes")) %>%
  arrange(-Importance) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  ggplot() +
  geom_col(aes(x = Importance, y = fct_rev(Variable)), fill = "black") +
  labs(y = "Variable") +
  theme_minimal()

if(!file.exists(here("outputs", "group models (no sensor data)", "sens_seasonality.png"))) ggsave(vip_rf, filename = here("outputs", "group models (no sensor data)", "sens_seasonality.png"), 
                                                                                              dpi = 320, height = 4, width = 6)

# fit final model to test data --------------------------------------------

### random forest

final_rf_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

table_rf <- final_rf_res %>%
  collect_metrics() %>%
  mutate(model = "rf")

roc_rf <- final_rf_res %>%
  collect_predictions() %>%
  select(.pred_yes, lapse_lagged)

roc_rf_obj <- roc(roc_rf$lapse_lagged, roc_rf$.pred_yes)
auc(roc_rf_obj)
ci_auc_rf <- ci.auc(roc_rf_obj)

### set up auc comparison

models <- c("Random Forest (no seasonality)", "Random Forest (seasonality)")
auc <- c(0.899, 0.912)
lower_ci <- c(0.871, 0.887)
upper_ci <- c(0.928, 0.937)

# combine data into a data frame
data <- tibble(models, auc, lower_ci, upper_ci) %>%
  mutate(models = fct_inorder(models))

compare_auc <- ggplot(data, aes(x = models, y = auc)) +
  geom_pointrange(aes(x = models, ymin = lower_ci, ymax = upper_ci, colour = models)) +
  scale_color_viridis_d(option = "plasma", end = .6) +
  xlab("Model") +
  ylab("AUC") +
  labs(colour = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(compare_auc, filename = here("outputs", "group models (no sensor data)", "sens_seasonality_compare_auc.png"),
       dpi = 320, height = 6, width = 9)

# group to individual model -----------------------------------------------

# remove participants with 0% or 100% lapses

df_ <- analytic_sample_clean %>%
  group_by(id) %>%
  mutate(lapse_events = n()) %>%
  mutate(prop_lapses = sum(lapse_lagged == "yes", na.rm = T)/lapse_events,
         prop_non_lapses = sum(lapse_lagged == "no", na.rm = T)/lapse_events) %>%
  filter(prop_lapses > 0 & prop_non_lapses > 0) %>%
  ungroup()

df_ %>% 
  distinct(id) # 25 participants available for analysis

# select variables to include

exclude <- c("nr", "EMA_id", "EMA_hour", "lapse_event_imp", "lapse_events", "prop_lapses", "prop_non_lapses",
             "nrt_no_presc", "nrt_presc", "zyban", "varen", "ecig", "group_supp", "ind_supp", "helpline", "book", "website",
             "app", "none", "other", "mpath_data", "fitbit_data", "start_date",
             "time_first_prompt", "end_date", "time_last_prompt", "participant_specific_variable_1",
             "participant_specific_variable_2", "last_time_smoke_fup", "CO_reading", "CO_reading1",
             "CO_reading2", "altered_prompt", "altered_prompt_start_time", "altered_prompt_study_day",
             "start_datetime", "end_datetime", "smoking_fup", "EMA_date", "EMA_number", "EMA_date_time",
             "location_others_home_imp", "activity_child_care_imp", "social_context_friend_imp", "social_context_child_imp", # remove pred vars with no variability
             "social_context_colleague_imp", "social_context_stranger_imp")

df <- df_ %>%
  select(-all_of(exclude)) %>%
  drop_na(lapse_lagged)

df$lapse_lagged <- relevel(df$lapse_lagged, "yes")
df$time_of_day <- factor(df$time_of_day, levels = c("morning", "midday", "evening", "night"))
df$season <- as.factor(case_when(df$season == 12 | df$season == 1 | df$season == 2 ~ "winter",
                                 df$season == 3 | df$season == 4 | df$season == 5 ~ "spring",
                                 df$season == 6 | df$season == 7 | df$season == 8 ~ "summer",
                                 df$season == 9 | df$season == 10 | df$season == 11 ~ "autumn",
                                 TRUE ~ "winter"))

# split into separate lists

df_split <- df %>%
  group_by(id) %>%
  group_split()

# split into training and testing datasets --------------------------------

df_split_training <- list()

for(i in 1:length(df_split)) {
  
  df_split_training[[i]] <- df_split[-i] %>% # leaves participant i out of the training set
    bind_rows() %>%
    mutate(train_split = "training")
  
  df_split_training[[i]] <- df_split_training[[i]] %>% # uses participant i for the testing set
    bind_rows(., df_split[i] %>%
                bind_rows() %>%
                mutate(train_split = "testing"))
  
  df_split_training[[i]] <- df_split_training[[i]] %>%
    select(-c("id"))
}

# fit best-performing group-level model to each individual's data ---------------------------

df_model_fit <- list()

for (i in 1:length(df_split_training)) {
  
  train_data <- df_split_training[[i]] %>%
    filter(train_split == "training") %>%
    select(-train_split)
  
  test_data <- df_split_training[[i]] %>%
    filter(train_split == "testing") %>%
    select(-train_split)
  
  rec <- recipe(lapse_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(cpd, age) %>%
    step_upsample(lapse_lagged, skip = TRUE) # manages class imbalances
  
  rf_prep <- prep(rec)
  rf_juiced <- juice(rf_prep)
  
  rf_spec <- rand_forest(
    mtry = 12,
    trees = 500,
    min_n = 16
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  
  rf_wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(final_rf)
  
  final_rf_workflow_fit <- fit(rf_wf, data = train_data)
  
  test_data$.pred_yes <- predict(final_rf_workflow_fit, new_data = test_data, type = "prob")$.pred_yes
  
  roc_test_error <- tryCatch(roc(test_data$lapse_lagged, test_data$.pred_yes), error = function(e) e)
  
  if(any(class(roc_test_error) == "error")) {
    
    message("Error")
    
  } else {
    
    roc_obj <- roc(test_data$lapse_lagged, test_data$.pred_yes)
    auc <- auc(roc_obj)
    ci.auc_lower <- ci.auc(roc_obj)[1]
    ci.auc_upper <- ci.auc(roc_obj)[3]
    
    df_model_fit[[i]] <- list(auc = auc,
                              ci.auc_lower = ci.auc_lower,
                              ci.auc_upper = ci.auc_upper)
  }
}

# summarise model performance for each individual -------------------------

model_performance <- tibble(id = df %>%
                              group_by(id) %>%
                              distinct(id) %>%
                              pull(id),
                            auc = unlist(map(df_model_fit, "auc")))

median(model_performance$auc)
range(model_performance$auc)
