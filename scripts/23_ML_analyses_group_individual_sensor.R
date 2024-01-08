analytic_sample_clean_sens <- read_rds(here("data", "group models (sensor data)", "analytic_sample_clean_sens.rds"))

# remove participants with 0% or 100% lapses

df_ <- analytic_sample_clean_sens %>%
  group_by(id) %>%
  mutate(lapse_events = n()) %>%
  mutate(prop_lapses = sum(lapse_lagged == "yes", na.rm = T)/lapse_events,
         prop_non_lapses = sum(lapse_lagged == "no", na.rm = T)/lapse_events) %>%
  filter(prop_lapses > 0 & prop_non_lapses > 0) %>%
  ungroup()

df_ %>% 
  summarise(unique(id)) # 20 participants available for analysis

# select variables to include

exclude <- c("nr", "EMA_id", "EMA_hour", "lapse_event_imp", "lapse_events", "prop_lapses", "prop_non_lapses",
             "nrt_no_presc", "nrt_presc", "zyban", "varen", "ecig", "group_supp", "ind_supp", "helpline", "book", "website",
             "app", "none", "other", "mpath_data", "fitbit_data", "start_date",
             "time_first_prompt", "end_date", "time_last_prompt", "participant_specific_variable_1",
             "participant_specific_variable_2", "last_time_smoke_fup", "CO_reading", "CO_reading1",
             "CO_reading2", "altered_prompt", "altered_prompt_start_time", "altered_prompt_study_day",
             "start_datetime", "end_datetime", "smoking_fup", "EMA_date", "EMA_number", "EMA_date_time",
             "location_others_home_imp", "activity_child_care_imp", "social_context_friend_imp", "social_context_child_imp", # remove pred vars with no variability
             "social_context_colleague_imp", "social_context_stranger_imp", "social_context_relative_imp")

df <- df_ %>%
  select(-all_of(exclude)) %>%
  select(-starts_with("pd_2")) %>%
  select(-starts_with("pd_3")) %>%
  select(-starts_with("pd_1_hr_1")) %>%
  select(-starts_with("pd_1_hr_2")) %>%
  select(-starts_with("pd_1_step_1")) %>%
  select(-starts_with("pd_1_step_2")) %>%
  drop_na(lapse_lagged)

df$lapse_lagged <- relevel(df$lapse_lagged, "yes")
df$time_of_day <- factor(df$time_of_day, levels = c("morning", "midday", "evening", "night"))

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

write_rds(df_split_training, here("data", "group to individual models (sensor data)", "df_split_training.rds"))

# fit best-performing group-level model to each individual's data ---------------------------

final_rf_sens <- read_rds(here("data", "group models (sensor data)", "final_rf_sens.rds")) # get specs for best-performing model

df_model_fit <- list()

for (i in 1:length(df_split_training)) {
  
  train_data <- df_split_training[[i]] %>%
    filter(train_split == "training") %>%
    select(-train_split)
  
  test_data <- df_split_training[[i]] %>%
    filter(train_split == "testing") %>%
    select(-train_split)
  
  rf_rec <- recipe(lapse_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(cpd, age) %>%
    step_upsample(lapse_lagged, skip = TRUE) # manages class imbalances
  
  rf_prep <- prep(rf_rec)
  rf_juiced <- juice(rf_prep)
  
  rf_spec <- rand_forest(
    mtry = 7,
    trees = 500,
    min_n = 28
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  
  wf_rf <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec)
  
  final_rf_workflow_fit <- fit(wf_rf, data = train_data)
  
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

write_rds(df_model_fit, here("data", "group to individual models (sensor data)", "df_model_fit.rds"))

# summarise model performance for each individual -------------------------

df_model_fit <- read_rds(here("data", "group to individual models (sensor data)", "df_model_fit.rds"))

model_performance <- tibble(id = df %>%
                              group_by(id) %>%
                              distinct(id) %>%
                              pull(id),
                            auc = unlist(map(df_model_fit, "auc")))

write_rds(model_performance, here("data", "group to individual models (sensor data)", "group_to_individual.rds"))

flextable(model_performance) %>% 
  theme_vanilla() %>% 
  save_as_docx(path = here("outputs", "group to individual models (sensor data)", "model_performance.docx"))

model_performance <- read_rds(here("data", "group to individual models (sensor data)", "group_to_individual.rds"))

median(model_performance$auc)
range(model_performance$auc)
