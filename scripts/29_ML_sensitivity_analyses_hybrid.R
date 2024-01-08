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
             "lapse_event_imp", "craving_events", "prop_high", "prop_low",
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

df_split_training <- list()

for(i in 1:length(df_split)) {
  
  df_split_training[[i]] <- df_split[-i] %>% # leaves participant i out of the training set
    bind_rows() %>%
    mutate(train_split = "training")
  
  df_split_training[[i]] <- df_split_training[[i]] %>% # uses participant i for the testing set
    bind_rows(., df_split[i] %>%
                bind_rows() %>%
                mutate(train_split = "testing"))
  
  df_split_training[[i]] <- df_split_training[[i]] %>% # select random 20% of testing rows and include in training set
    mutate(row_number = row_number())
  
  tmp <- tibble()
  
  tmp <- df_split_training[[i]][sample(which(df_split_training[[i]]$train_split=="testing"),
                                       round(0.2*length(which(df_split_training[[i]]$train_split=="testing")))),]
  tmp_2 <- tibble()
  
  tmp_2 <- anti_join(df_split_training[[i]], tmp, by = "row_number")
  
  tmp_3 <- tibble()
  
  tmp_3 <- tmp %>%
    mutate(train_split = "training")
  
  account_id <- as.character(unique(tmp_3$id))
  
  df_split_training[[i]] <- bind_rows(tmp_2, tmp_3)
  
  df_split_training[[i]] <- df_split_training[[i]] %>%
    select(-c("id", "row_number"))
  
  names(df_split_training)[[i]] <- account_id
  
}

# fit best-performing group-level model to individual data ---------------------------

df_model_fit_hybrid <- list()

for (i in 1:length(df_split_training)) {
  
  train_data <- df_split_training[[i]] %>%
    filter(train_split == "training") %>%
    select(-train_split)
  
  test_data <- df_split_training[[i]] %>%
    filter(train_split == "testing") %>%
    select(-train_split)
  
  rf_rec <- recipe(cravings_binary_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(cpd, age) %>%
    step_downsample(cravings_binary_lagged, skip = TRUE) # manages class imbalances
  
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
    
    df_model_fit_hybrid[[i]] <- list(auc = auc)
    
    names(df_model_fit_hybrid)[[i]] <- names(df_split_training)[[i]]
  }
  
}

write_rds(df_model_fit_hybrid, here("data", "sensitivity analyses", "df_model_fit_hybrid.rds"))

# summarise model performance for each individual -------------------------

df_model_fit_hybrid <- read_rds(here::here("data", "sensitivity analyses", "df_model_fit_hybrid.rds"))

model_performance_hybrid <- tibble(account_id = names(df_model_fit_hybrid[lengths(df_model_fit_hybrid) >= 1]),
                                   auc = unlist(map(df_model_fit_hybrid, "auc")))

median(model_performance_hybrid$auc)
range(model_performance_hybrid$auc)

# compare to group model --------------------------------------------------

group_auc <- read_rds(here("data", "sensitivity analyses", "model_performance_group_to_ind.rds")) %>%
  mutate(account_id = id)

hybrid_to_group <- model_performance_hybrid %>%
  select(account_id, model_auc = auc) %>%
  left_join(group_auc %>%
              rename(group_auc = auc), by = c("account_id")) %>%
  mutate(hybrid_preferred = model_auc >= group_auc)

sum(hybrid_to_group$hybrid_preferred == TRUE)/37*100
sum(hybrid_to_group$hybrid_preferred == FALSE)/37*100

hybrid_to_group_plot <- hybrid_to_group %>%
  ggplot() +
  geom_point(aes(x = group_auc, y = model_auc, colour = hybrid_preferred)) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Group-level algorithm AUC",
       y = "Hybrid algorithm AUC") +
  theme(legend.position = "none")

if(!file.exists(here("outputs", "sensitivity analyses", "hybrid_to_group_plot.png"))) ggsave(hybrid_to_group_plot, 
                                                                                             filename = here("outputs", "sensitivity analyses", "hybrid_to_group_plot.png"),
                                                                                             dpi = 320, height = 8, width = 10)
