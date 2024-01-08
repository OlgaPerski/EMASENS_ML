analytic_sample_clean <- read_rds(here("data", "sensitivity analyses", "analytic_sample_clean.rds"))

# remove participants with 0% or 100% lapses

df_ <- analytic_sample_clean %>%
  group_by(id) %>%
  mutate(craving_events = n()) %>%
  mutate(prop_high = sum(cravings_binary_lagged == "high", na.rm = T)/craving_events,
         prop_low = sum(cravings_binary_lagged == "low", na.rm = T)/craving_events) %>%
  filter(prop_high > 0 & prop_low > 0) %>%
  ungroup()

df_ %>% 
  summarise(unique(id)) # 37 participants available for analysis

# select variables to include

exclude <- c("nr", "EMA_id", "EMA_hour", 
             "craving_imp", "cravings_binary",
             "lapse_event_imp", "lapse_lagged",
             "craving_events", "prop_high", "prop_low",
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
  drop_na(cravings_binary_lagged)

df$cravings_binary_lagged <- relevel(df$cravings_binary_lagged, "high")
df$time_of_day <- factor(df$time_of_day, levels = c("morning", "midday", "evening", "night"))

# split into separate lists

df_split <- df %>%
  group_by(id) %>%
  group_split()

# split into training and testing datasets --------------------------------

df_split_training_group_to_ind <- list()

for(i in 1:length(df_split)) {
  
  df_split_training_group_to_ind[[i]] <- df_split[-i] %>% # leaves participant i out of the training set
    bind_rows() %>%
    mutate(train_split = "training")
  
  df_split_training_group_to_ind[[i]] <- df_split_training_group_to_ind[[i]] %>% # uses participant i for the testing set
    bind_rows(., df_split[i] %>%
                bind_rows() %>%
                mutate(train_split = "testing"))
  
  df_split_training_group_to_ind[[i]] <- df_split_training_group_to_ind[[i]] %>%
    select(-c("id"))
}

write_rds(df_split_training_group_to_ind, here("data", "sensitivity analyses", "df_split_training_group_to_ind.rds"))

# fit best-performing group-level model to each individual's data ---------------------------

final_rf <- read_rds(here("data", "sensitivity analyses", "final_rf.rds")) # get specs for best-performing model

df_model_fit <- list()

for (i in 1:length(df_split_training_group_to_ind)) {
  
  train_data <- df_split_training_group_to_ind[[i]] %>%
    filter(train_split == "training") %>%
    select(-train_split)
  
  test_data <- df_split_training_group_to_ind[[i]] %>%
    filter(train_split == "testing") %>%
    select(-train_split)
  
  rf_rec <- recipe(cravings_binary_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(cpd, age) %>%
    step_upsample(cravings_binary_lagged, skip = TRUE) # manages class imbalances
  
  rf_prep <- prep(rf_rec)
  rf_juiced <- juice(rf_prep)
  
  rf_spec <- rand_forest(
    mtry = 53,
    trees = 500,
    min_n = 20
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  
  wf_rf <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec)
  
  final_rf_workflow_fit <- fit(wf_rf, data = train_data)
  
  test_data$.pred_high <- predict(final_rf_workflow_fit, new_data = test_data, type = "prob")$.pred_high
  
  roc_test_error <- tryCatch(roc(test_data$cravings_binary_lagged, test_data$.pred_high), error = function(e) e)
  
  if(any(class(roc_test_error) == "error")) {
    
    message("Error")
    
  } else {
    
    roc_obj <- roc(test_data$cravings_binary_lagged, test_data$.pred_high)
    auc <- auc(roc_obj)
    ci.auc_lower <- ci.auc(roc_obj)[1]
    ci.auc_upper <- ci.auc(roc_obj)[3]
    
    df_model_fit[[i]] <- list(auc = auc,
                              ci.auc_lower = ci.auc_lower,
                              ci.auc_upper = ci.auc_upper)
  }
}

write_rds(df_model_fit, here("data", "sensitivity analyses", "df_model_fit_group_to_ind.rds"))

# summarise model performance for each individual -------------------------

df_model_fit_group_to_ind <- read_rds(here("data", "sensitivity analyses", "df_model_fit_group_to_ind.rds"))

model_performance_group_to_ind <- tibble(id = df %>%
                                           group_by(id) %>%
                                           distinct(id) %>%
                                           pull(id),
                                         auc = unlist(map(df_model_fit_group_to_ind, "auc")))

write_rds(model_performance_group_to_ind, here("data", "sensitivity analyses", "model_performance_group_to_ind.rds"))

flextable(model_performance_group_to_ind) %>% 
  theme_vanilla() %>% 
  save_as_docx(path = here("outputs", "sensitivity analyses", "model_performance_group_to_ind.docx"))

median(model_performance_group_to_ind$auc)
range(model_performance_group_to_ind$auc)
