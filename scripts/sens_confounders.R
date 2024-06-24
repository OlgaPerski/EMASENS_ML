source(here::here("scripts", "1_load_packages.R"))

# read in clean analytic sample with sensor data

analytic_sample_clean_sens <- read_rds(here("data", "group models (sensor data)", "analytic_sample_clean_sens.rds"))

# explore nr EMAs with reported walking/exercise

table(analytic_sample_clean_sens$activity_walking_imp, analytic_sample_clean_sens$id)

# explore nr EMAs with reported caffeine intake

table(analytic_sample_clean_sens$caffeine_imp, analytic_sample_clean_sens$id)

# explore nr EMAs with reported nicotine use

table(analytic_sample_clean_sens$nicotine_imp, analytic_sample_clean_sens$id)

exclude <- c("nr", "id", "EMA_id", "EMA_hour", "lapse_event_imp", "nrt_no_presc", "nrt_presc", "zyban", "varen",
             "ecig", "group_supp", "ind_supp", "helpline", "book", "website",
             "app", "none", "other", "mpath_data", "fitbit_data", "start_date",
             "time_first_prompt", "end_date", "time_last_prompt", "participant_specific_variable_1",
             "participant_specific_variable_2", "last_time_smoke_fup", "CO_reading", "CO_reading1",
             "CO_reading2", "altered_prompt", "altered_prompt_start_time", "altered_prompt_study_day",
             "start_datetime", "end_datetime", "smoking_fup", "EMA_date", "EMA_number", "EMA_date_time",
             "location_others_home_imp", "activity_child_care_imp", "social_context_friend_imp", "social_context_child_imp", # remove pred vars with no variability
             "social_context_colleague_imp", "social_context_stranger_imp", "social_context_relative_imp")

df_sens <- analytic_sample_clean_sens %>%
  select(-all_of(exclude)) %>%
  drop_na(lapse_lagged)

# produce dataset to use for sensitivity analysis

conf_removed <- df_sens %>%
  filter(activity_walking_imp != 1) %>%
  filter(caffeine_imp != 1) %>%
  filter(nicotine_imp != 1)

# run 22_ML_analyses_group_sensor.R on reduced dataset with EMAs with potential confounders removed

run_models_pd_tw <- function(df = df_sens, prediction_distance = "pd_1", timewindow = "hr_1|step_1") {
  
  prediction_distance_variables <- names(df)[str_detect(names(df), "pd_")]
  prediction_distance_to_include <- prediction_distance_variables[str_detect(prediction_distance_variables, prediction_distance)]
  pd_time_window_to_include <- prediction_distance_to_include[str_detect(prediction_distance_to_include, timewindow)]
  prediction_distance_to_exclude <- prediction_distance_variables[!prediction_distance_variables %in% pd_time_window_to_include]
  
  # split data into training and testing ------------------------------------ 
  
  data_split <- df %>%
    select(-any_of(prediction_distance_to_exclude)) %>%
    initial_split(prop = .80)
  
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
  
  ### support vector machine
  
  svm_spec <- svm_rbf(
    cost = tune(),
    rbf_sigma = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("kernlab")
  
  ### elastic net
  
  elnet_spec <- logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("glmnet")
  
  ### xgboost
  
  xgb_spec <- boost_tree(
    trees = 1000,
    tree_depth = tune(), min_n = tune(),
    loss_reduction = tune(),                     ## first three hyperparameters: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune(),                         ## step size
  ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
  ### set up possible values to try through space-filling design
  
  xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_data),
    learn_rate(),
    size = 30
  )
  
  # set up workflows --------------------------------------------------------
  
  ### random forest
  
  tune_wf_rf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(rf_spec)
  
  ### support vector machine
  
  tune_wf_svm <- workflow() %>%
    add_recipe(rec) %>%
    add_model(svm_spec)
  
  ### elastic net
  
  tune_wf_elnet <- workflow() %>%
    add_recipe(rec) %>%
    add_model(elnet_spec)
  
  ### xgboost
  
  tune_wf_xgb <- workflow() %>%
    add_recipe(rec) %>%
    add_model(xgb_spec)
  
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
  
  ### support vector machine
  
  svm_res <-
    tune_grid(
      tune_wf_svm,
      resamples = folds,
      grid = 10,
      control = control_grid(save_pred = TRUE),
      metrics = metric_set(roc_auc, sens, spec, accuracy)
    )
  
  svm_res %>% collect_metrics()
  svm_res %>% select_best("roc_auc")
  
  best_svm_auc <-
    select_best(svm_res, "roc_auc")
  
  final_svm <- finalize_model(
    svm_spec,
    best_svm_auc
  )
  
  ### elastic net
  
  elnet_res <-
    tune_grid(
      tune_wf_elnet,
      resamples = folds,
      grid = 10,
      control = control_grid(save_pred = TRUE),
      metrics = metric_set(roc_auc, sens, spec, accuracy)
    )
  
  elnet_res %>% collect_metrics()
  elnet_res %>% select_best("roc_auc")
  
  best_elnet_auc <-
    select_best(elnet_res, "roc_auc")
  
  final_elnet <- finalize_model(
    elnet_spec,
    best_elnet_auc
  )
  
  ### xgboost
  
  xgb_res <- tune_grid(
    tune_wf_xgb,
    resamples = folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )
  
  xgb_res %>% collect_metrics()
  xgb_res %>% select_best("roc_auc")
  
  best_xgb_auc <-
    select_best(xgb_res, "roc_auc")
  
  final_xgb <- finalize_model(
    xgb_spec,
    best_xgb_auc
  )
  
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
  
  ### support vector machine
  
  final_svm_wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(final_svm)
  
  final_svm_res <- final_svm_wf %>%
    last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))
  
  table_svm <- final_svm_res %>%
    collect_metrics() %>%
    mutate(model = "svm")
  
  roc_svm <- final_svm_res %>%
    collect_predictions() %>%
    select(.pred_yes, lapse_lagged)
  
  roc_svm_obj <- roc(roc_svm$lapse_lagged, roc_svm$.pred_yes)
  auc(roc_svm_obj)
  ci_auc_svm <- ci.auc(roc_svm_obj)
  
  ### elastic net
  
  final_elnet_wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(final_elnet)
  
  final_elnet_res <- final_elnet_wf %>%
    last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))
  
  table_elnet <- final_elnet_res %>%
    collect_metrics() %>%
    mutate(model = "elnet")
  
  roc_elnet <- final_elnet_res %>%
    collect_predictions() %>%
    select(.pred_yes, lapse_lagged)
  
  roc_elnet_obj <- roc(roc_elnet$lapse_lagged, roc_elnet$.pred_yes)
  auc(roc_elnet_obj)
  ci_auc_elnet <-ci.auc(roc_elnet_obj)
  
  ### xgboost
  
  final_xgb_wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(final_xgb)
  
  final_xgb_res <- final_xgb_wf %>%
    last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))
  
  table_xgb <- final_xgb_res %>%
    collect_metrics() %>%
    mutate(model = "xgb")
  
  roc_xgb <- final_xgb_res %>%
    collect_predictions() %>%
    select(.pred_yes, lapse_lagged)
  
  roc_xgb_obj <- roc(roc_xgb$lapse_lagged, roc_xgb$.pred_yes)
  auc(roc_xgb_obj)
  ci_auc_xgb <- ci.auc(roc_xgb_obj)
  
  table_performance <- bind_rows(table_rf, table_svm, table_elnet, table_xgb) %>%
    mutate(ci_lower = NA,
           ci_upper = NA) 
  
  table_performance[4,6] <- ci_auc_rf[1]
  table_performance[4,7] <- ci_auc_rf[3]
  
  table_performance[8,6] <- ci_auc_svm[1]
  table_performance[8,7] <- ci_auc_svm[3]
  
  table_performance[12,6] <- ci_auc_elnet[1]
  table_performance[12,7] <- ci_auc_elnet[3]
  
  table_performance[16,6] <- ci_auc_xgb[1]
  table_performance[16,7] <- ci_auc_xgb[3]
  
  
  return(list(results = table_performance,
              model_prep = rf_prep,
              svm_specific = best_svm_auc,
              model_specs = list(rf = final_rf,
                                 svm = final_svm,
                                 elnet = final_elnet,
                                 xgb = final_xgb),
              model_res = list(rf = final_rf_res,
                               svm = final_svm_res,
                               elnet = final_elnet_res,
                               xgb = final_xgb_res),
              pd_tw = pd_time_window_to_include))
  
}

# pd1

sens_pd1_tw1 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_1", timewindow = "hr_1|step_1")
sens_pd1_tw2 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_1", timewindow = "hr_2|step_2")
sens_pd1_tw3 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_1", timewindow = "hr_3|step_3")

# pd2

sens_pd2_tw1 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_2", timewindow = "hr_1|step_1")
sens_pd2_tw2 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_2", timewindow = "hr_2|step_2")
sens_pd2_tw3 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_2", timewindow = "hr_3|step_3")

# pd3

sens_pd3_tw1 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_3", timewindow = "hr_1|step_1")
sens_pd3_tw2 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_3", timewindow = "hr_2|step_2")
sens_pd3_tw3 <- run_models_pd_tw(df = conf_removed, prediction_distance = "pd_3", timewindow = "hr_3|step_3")

# summarise model performance ---------------------------------------------

table_performance <- rbind(sens_pd1_tw1$results %>%
                             mutate(df = "pd1_tw1"),
                           sens_pd1_tw2$results %>%
                             mutate(df = "pd1_tw2"),
                           sens_pd1_tw3$results %>%
                             mutate(df = "pd1_tw3"),
                           sens_pd2_tw1$results %>%
                             mutate(df = "pd2_tw1"),
                           sens_pd2_tw2$results %>%
                             mutate(df = "pd2_tw2"),
                           sens_pd2_tw3$results %>%
                             mutate(df = "pd2_tw3"),
                           sens_pd3_tw1$results %>%
                             mutate(df = "pd3_tw1"),
                           sens_pd3_tw2$results %>%
                             mutate(df = "pd3_tw2"),
                           sens_pd3_tw3$results %>%
                             mutate(df = "pd3_tw3"))

# save output of conf_removed models in sensitivty_confounder folder

flextable(table_performance) %>% 
  theme_vanilla() %>% 
  save_as_docx(path = here("outputs", "sensitivity_confounders", "group models (sensor data)", "table_performance.docx"))

### set up auc comparison

models <- c("Random Forest (with sensor data)", "Random Forest (with sensor data, confounders removed)")
auc <- c(0.952, 0.947)
lower_ci <- c(0.933, 0.923)
upper_ci <- c(0.970, 0.972)

# combine data into a data frame
data <- tibble(models, auc, lower_ci, upper_ci) %>%
  mutate(models = fct_inorder(models))

compare_auc_conf <- ggplot(data, aes(x = models, y = auc)) +
  geom_pointrange(aes(x = models, ymin = lower_ci, ymax = upper_ci, colour = models)) +
  scale_color_viridis_d(option = "plasma", end = .6) +
  xlab("Model") +
  ylab("AUC") +
  labs(colour = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(compare_auc_conf, filename = here("outputs", "sensitivity_confounders", "group models (sensor data)", "compare_auc_conf.png"),
       dpi = 320, height = 6, width = 9)

# run 23_ML_analyses_group_individual_sensor.R on reduced dataset with EMAs with potential confounders removed

df_ <- conf_removed %>%
  group_by(id) %>%
  mutate(lapse_events = n()) %>%
  mutate(prop_lapses = sum(lapse_lagged == "yes", na.rm = T)/lapse_events,
         prop_non_lapses = sum(lapse_lagged == "no", na.rm = T)/lapse_events) %>%
  filter(prop_lapses > 0 & prop_non_lapses > 0) %>%
  ungroup()

df_ %>% 
  summarise(unique(id)) # 17 participants available for analysis

# select variables to include

df <- df_ %>%
  select(-starts_with("pd_1")) %>%
  select(-starts_with("pd_2")) %>%
  select(-starts_with("pd_3_hr_2")) %>%
  select(-starts_with("pd_3_hr_3")) %>%
  select(-starts_with("pd_3_step_2")) %>%
  select(-starts_with("pd_3_step_3")) %>%
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

# fit best-performing group-level model to each individual's data ---------------------------

# get specs for best-performing model through looking at table_performance, selecting the model with the greatest AUC, and then getting the specs for that model

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
    mtry = 5,
    trees = 500,
    min_n = 3
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

# summarise model performance for each individual -------------------------

model_performance <- tibble(id = df %>%
                              group_by(id) %>%
                              distinct(id) %>%
                              pull(id),
                            auc = unlist(map(df_model_fit, "auc")))

flextable(model_performance) %>% 
  theme_vanilla() %>% 
  save_as_docx(path = here("outputs", "sensitivity_confounders", "group to individual models (sensor data)", "model_performance.docx"))

median(model_performance$auc)
range(model_performance$auc)

# run 24_ML_analyses_individual_sensor.R on reduced dataset with EMAs with potential confounders removed

# set additional cut-off for inclusion

lapse_count <- df %>%
  group_by(id, lapse_lagged) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = lapse_lagged, values_from = n_lapses) %>%
  filter(no > 5 & yes > 5) %>%
  pull(id)

lapse_count <- as.character(droplevels(lapse_count)) # 9 participants available for analysis

exclude_baseline <- c("age", "sex", "post_16_edu", "ethnicity", "past_quit_attempt", "job_type", # remove vars with zero variance (baseline characteristics)
                      "cpd", "time_to_first_cig", "motivation_to_stop")

rf_sample <- df %>% # does not contain prop_lapse vars above as this stems from df and not df_
  select(-all_of(exclude_baseline)) %>%
  ungroup() %>%
  filter(id %in% lapse_count) %>%
  group_by(id) %>%
  group_split()

rf_sample <- lapply(rf_sample, function(x) 
  x %>% select(-c("id")))

# random forest -----------------------------------------------------------

individual_models_rf <- list()

return_list_rf <- list()

run_rf_models <- function(x) {
  
  data_split <- initial_split(x, prop = .80, strata = lapse_lagged)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  rf_rec <- recipe(lapse_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_upsample(lapse_lagged, skip = TRUE) # manages class imbalances in training sets
  
  rf_prep <- prep(rf_rec)
  rf_juiced <- juice(rf_prep)
  
  rf_spec <- rand_forest(
    mtry = tune(),
    trees = 500,
    min_n = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  
  tune_wf_rf <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec)
  
  folds <- vfold_cv(train_data, v = 10)
  
  rf_res <-
    tune_grid(
      tune_wf_rf,
      resamples = folds,
      grid = 10,
      control = control_grid(save_pred = TRUE),
      metrics = metric_set(roc_auc, sens, spec, accuracy)
    )
  
  if(str_detect(str_c(rf_res$.notes[1]), "Error")) {
    
    return(failure_reason = rf_res$.notes[1])
    
  } else {
    
    rf_res %>% select_best("roc_auc")
    
    best_rf_auc <-
      select_best(rf_res, "roc_auc")
    
    final_rf <- finalize_model(
      rf_spec,
      best_rf_auc
    )
    
    final_rf_vip <- final_rf %>%
      set_engine("ranger", importance = "permutation") %>%
      fit(lapse_lagged ~ .,
          data = juice(rf_prep)) %>%
      vip()
    
    final_rf_wf <- workflow() %>%
      add_recipe(rf_rec) %>%
      add_model(final_rf)
    
    final_rf_res <- final_rf_wf %>%
      last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))
    
    metrics_rf <- final_rf_res %>%
      collect_metrics()
    
    return(tibble(best_rf_auc = list(best_rf_auc),
                  final_rf_vip = list(final_rf_vip$data),
                  metrics_rf = list(metrics_rf %>% mutate(model = "rf"),
                                    training_data = list(train_data),
                                    testing_data = list(test_data))))
  }
}

individual_models_rf <- lapply(rf_sample, function(x) try(run_rf_models(x)))

write_rds(individual_models_rf, here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_models_rf.rds"))

# support vector machine -----------------------------------------------------------

individual_models_svm <- list()

return_list_svm <- list()

run_svm_models <- function(x) {
  data_split <- initial_split(x, prop = .80, strata = lapse_lagged)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  svm_rec <- recipe(lapse_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_upsample(lapse_lagged, skip = TRUE) # manages class imbalances
  
  svm_prep <- prep(svm_rec)
  svm_juiced <- juice(svm_prep)
  
  svm_spec <- svm_rbf(
    cost = tune(),
    rbf_sigma = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("kernlab")
  
  tune_wf_svm <- workflow() %>%
    add_recipe(svm_rec) %>%
    add_model(svm_spec)
  
  folds <- vfold_cv(train_data, v = 10)
  
  svm_res <-
    tune_grid(
      tune_wf_svm,
      resamples = folds,
      grid = 10,
      control = control_grid(save_pred = TRUE),
      metrics = metric_set(roc_auc, sens, spec, accuracy)
    )
  
  if(str_detect(str_c(svm_res$.notes[1]), "Error")) {
    
    return(failure_reason = svm_res$.notes[1])
    
  } else {
    
    svm_res %>% select_best("roc_auc")
    
    best_svm_auc <-
      select_best(svm_res, "roc_auc")
    
    final_svm <- finalize_model(
      svm_spec,
      best_svm_auc
    )
    
    svm_spec_best <- svm_rbf(
      cost = best_svm_auc$cost, #input values from best performing SVM above
      rbf_sigma = best_svm_auc$rbf_sigma
    ) %>%
      set_mode("classification") %>%
      set_engine("kernlab")
    
    svm_fit <- workflow() %>%
      add_model(svm_spec_best) %>%
      add_formula(lapse_lagged ~ .) %>%
      fit(juice(svm_prep)) 
    
    final_svm_vip <- svm_fit %>%
      extract_fit_parsnip() %>%
      vip(method = "permute", 
          target = "lapse_lagged", metric = "auc", reference_class = "yes",
          pred_wrapper = kernlab::predict, train = juice(svm_prep))
    
    final_svm_wf <- workflow() %>%
      add_recipe(svm_rec) %>%
      add_model(final_svm)
    
    final_svm_res <- final_svm_wf %>%
      last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))
    
    metrics_svm <- final_svm_res %>%
      collect_metrics()
    
    return(tibble(best_svm_auc = list(best_svm_auc),
                  final_svm_vip = list(final_svm_vip$data),
                  metrics_svm = list(metrics_svm %>% mutate(model = "svm"))
    ))
    
  }
}

individual_models_svm <- lapply(rf_sample, function(x) try(run_svm_models(x)))

write_rds(individual_models_svm, here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_models_svm.rds"))

# penalised logistic regression -----------------------------------------------------------

individual_models_elnet <- list()

return_list_elnet <- list()

run_elnet_models <- function(x) {
  data_split <- initial_split(x, prop = .80, strata = lapse_lagged)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  elnet_rec <- recipe(lapse_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_upsample(lapse_lagged, skip = TRUE) # manages class imbalances
  
  elnet_prep <- prep(elnet_rec)
  elnet_juiced <- juice(elnet_prep)
  
  elnet_spec <- logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("glmnet")
  
  tune_wf_elnet <- workflow() %>%
    add_recipe(elnet_rec) %>%
    add_model(elnet_spec)
  
  folds <- vfold_cv(train_data, v = 10)
  
  elnet_res <-
    tune_grid(
      tune_wf_elnet,
      resamples = folds,
      grid = 10,
      control = control_grid(save_pred = TRUE),
      metrics = metric_set(roc_auc, sens, spec, accuracy)
    )
  
  if(str_detect(str_c(elnet_res$.notes[1]), "Error")) {
    
    return(failure_reason_elnet = elnet_res$.notes[1])
    
  } else {
    
    elnet_res %>% select_best("roc_auc")
    
    best_elnet_auc <-
      select_best(elnet_res, "roc_auc")
    
    final_elnet <- finalize_model(
      elnet_spec,
      best_elnet_auc
    )
    
    final_elnet_vip <- final_elnet %>%
      set_engine("glmnet", importance = "permutation") %>%
      fit(lapse_lagged ~ .,
          data = juice(elnet_prep)) %>%
      vip()
    
    final_elnet_wf <- workflow() %>%
      add_recipe(elnet_rec) %>%
      add_model(final_elnet)
    
    final_elnet_res <- final_elnet_wf %>%
      last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))
    
    metrics_elnet <- final_elnet_res %>%
      collect_metrics()
    
    return(tibble(best_elnet_auc = list(best_elnet_auc),
                  final_elnet_vip = list(final_elnet_vip$data),
                  metrics_elnet = list(metrics_elnet %>% mutate(model = "elnet"))
    ))
    
  }
}

individual_models_elnet <- lapply(rf_sample, function(x) try(run_elnet_models(x)))

write_rds(individual_models_elnet, here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_models_elnet.rds"))

# xgboost -----------------------------------------------------------

individual_models_xgb <- list()

return_list_xgb <- list()

run_xgb_models <- function(x) {
  data_split <- initial_split(x, prop = .80, strata = lapse_lagged)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  xgb_rec <- recipe(lapse_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_upsample(lapse_lagged, skip = TRUE) # manages class imbalances
  
  xgb_prep <- prep(xgb_rec)
  xgb_juiced <- juice(xgb_prep)
  
  xgb_spec <- boost_tree(
    trees = 1000, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     
    sample_size = tune(), mtry = tune(),         
    learn_rate = tune(),
  ) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")
  
  xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_data),
    learn_rate(),
    size = 30
  )
  
  tune_wf_xgb <- workflow() %>%
    add_recipe(xgb_rec) %>%
    add_model(xgb_spec)
  
  folds <- vfold_cv(train_data, v = 10)
  
  xgb_res <- tune_grid(
    tune_wf_xgb,
    resamples = folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )
  
  if(str_detect(str_c(xgb_res$.notes[1]), "Error")) {
    
    return(failure_reason_xgb = xgb_res$.notes[1])
    
  } else {
    
    xgb_res %>% select_best("roc_auc")
    
    best_xgb_auc <-
      select_best(xgb_res, "roc_auc")
    
    final_xgb <- finalize_model(
      xgb_spec,
      best_xgb_auc
    )
    
    final_xgb_vip <- final_xgb %>%
      set_engine("xgboost") %>%
      fit(lapse_lagged ~ .,
          data = juice(xgb_prep)) %>%
      vip()
    
    final_xgb_wf <- workflow() %>%
      add_recipe(xgb_rec) %>%
      add_model(final_xgb)
    
    final_xgb_res <- final_xgb_wf %>%
      last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))
    
    metrics_xgb <- final_xgb_res %>%
      collect_metrics()
    
    return(tibble(best_xgb_auc = list(best_xgb_auc),
                  final_xgb_vip = list(final_xgb_vip$data),
                  metrics_xgb = list(metrics_xgb %>% mutate(model = "xgb"))
    ))
    
  }
}

individual_models_xgb <- lapply(rf_sample, function(x) try(run_xgb_models(x)))

write_rds(individual_models_xgb, here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_models_xgb.rds"))

# extract model performance metrics ------------------------------

names(individual_models_rf) <- lapse_count
names(individual_models_svm) <- lapse_count
names(individual_models_elnet) <- lapse_count
names(individual_models_xgb) <- lapse_count

l <- list(individual_models_rf, individual_models_svm, individual_models_elnet, individual_models_xgb)
keys <- unique(unlist(lapply(l, names)))
combined <- setNames(do.call(mapply, c(FUN=c, lapply(l, `[`, keys))), keys)

extract_model_characteristics <- function(account_ids = lapse_count, rf_model = individual_models_rf, svm_model = individual_models_svm, elnet_model = individual_models_elnet, xgb_model = individual_models_xgb) {
  
  model_selection <- tibble(account_id = as.character(NA),
                            metric = as.character(NA),
                            estimate = as.numeric(NA),
                            model = as.character(NA))
  
  complete_models <- tibble(account_ids = account_ids,
                            complete_models = lengths(rf_model) >= 3 & lengths(svm_model) >= 3 & lengths(elnet_model) >= 3 & lengths(xgb_model) >= 3) %>%
    filter(complete_models == TRUE)
  
  for(i in 1:nrow(complete_models)) {
    
    n = complete_models$account_ids[i]
    
    model_extraction <- tibble(account_id = rep(n, 16),
                               metric = c(unlist(rf_model[[n]][["metrics_rf"]][[1]][,1]),
                                          unlist(svm_model[[n]][["metrics_svm"]][[1]][,1]),
                                          unlist(elnet_model[[n]][["metrics_elnet"]][[1]][,1]),
                                          unlist(xgb_model[[n]][["metrics_xgb"]][[1]][,1])),
                               estimate = c(unlist(rf_model[[n]][["metrics_rf"]][[1]][,3]),
                                            unlist(svm_model[[n]][["metrics_svm"]][[1]][,3]),
                                            unlist(elnet_model[[n]][["metrics_elnet"]][[1]][,3]),
                                            unlist(xgb_model[[n]][["metrics_xgb"]][[1]][,3])),
                               model = c(unlist(rf_model[[n]][["metrics_rf"]][[1]][,5]),
                                         unlist(svm_model[[n]][["metrics_svm"]][[1]][,5]),
                                         unlist(elnet_model[[n]][["metrics_elnet"]][[1]][,5]),
                                         unlist(xgb_model[[n]][["metrics_xgb"]][[1]][,5])))
    
    model_selection <- bind_rows(model_selection, model_extraction)
  }
  
  return(model_selection %>%
           drop_na(account_id)) 
}

# summarise mean sens, spec, accuracy and AUC for each model type

extract_model_characteristics() %>%
  mutate(model = factor(model, levels = c("xgb", "svm", "elnet", "rf"))) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  group_by(model) %>%
  summarise(mean_sens = mean(sens, na.rm = T),
            sd_sens = sd(sens, na.rm = T),
            median_sens = median(sens, na.rm = T),
            min_sens = min(sens, na.rm = T),
            max_sens = max(sens, na.rm = T),
            mean_spec = mean(spec, na.rm = T),
            sd_spec = sd(spec, na.rm = T),
            median_spec = median(spec, na.rm = T),
            min_spec = min(spec, na.rm = T),
            max_spec = max(spec, na.rm = T),
            mean_acc = mean(accuracy, na.rm = T),
            sd_acc = sd(accuracy, na.rm = T),
            median_acc = median(accuracy, na.rm = T),
            min_acc = min(accuracy, na.rm = T),
            max_acc = max(accuracy, na.rm = T),
            mean_auc = mean(roc_auc, na.rm = T),
            sd_auc = sd(roc_auc, na.rm = T),
            median_auc = median(roc_auc, na.rm = T),
            min_auc = min(roc_auc, na.rm = T),
            max_auc = max(roc_auc, na.rm = T))

# plot all models and summarise mean AUC and accuracy by model type

individual_model_performance_plot_sens <- extract_model_characteristics() %>%
  mutate(model = factor(model, levels = c("xgb", "svm", "elnet", "rf"))) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  group_by(account_id) %>%
  arrange(-roc_auc, -accuracy, -spec, -sens, model) %>%
  mutate(model = str_to_upper(model)) %>%
  ggplot(aes(x = model,
             y = roc_auc,
             fill = model)) +
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .01, 
    alpha = .3) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  scale_fill_viridis_d(alpha = 0.8) +
  theme_minimal() +
  labs(y = "AUC",
       x = "Model",
       fill = element_blank()) +
  theme(legend.position = "none")

if(!file.exists(here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_model_performance_plot_sens.png"))) ggsave(individual_model_performance_plot_sens, 
                                                                                                                          filename = here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_model_performance_plot_sens.png"),
                                                                                                                          dpi = 320, height = 8, width = 10)

# select best performing model for each participant

comparing_models_sens <- extract_model_characteristics() %>%
  mutate(model = factor(model, levels = c("rf", "elnet", "svm", "xgb"))) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  group_by(account_id) %>%
  arrange(-roc_auc, -accuracy, -spec, -sens, model) %>%
  dplyr::slice(1)

# summarise mean sens, spec, accuracy and AUC for participants' best performing models

comparing_models_sens %>%
  tabyl(model)

summary_vals <- comparing_models_sens %>%
  ungroup() %>%
  summarise(mean_sens = mean(sens, na.rm = T),
            sd_sens = sd(sens, na.rm = T),
            median_sens = median(sens, na.rm = T),
            min_sens = min(sens, na.rm = T),
            max_sens = max(sens, na.rm = T),
            mean_spec = mean(spec, na.rm = T),
            sd_spec = sd(spec, na.rm = T),
            median_spec = median(spec, na.rm = T),
            min_spec = min(spec, na.rm = T),
            max_spec = max(spec, na.rm = T),
            mean_accuracy = mean(accuracy, na.rm = T),
            sd_accuracy = sd(accuracy, na.rm = T),
            median_accuracy = median(accuracy, na.rm = T),
            min_accuracy = min(accuracy, na.rm = T),
            max_accuracy = max(accuracy, na.rm = T),
            mean_roc_auc = mean(roc_auc, na.rm = T),
            sd_roc_auc = sd(roc_auc, na.rm = T),
            median_roc_auc = median(roc_auc, na.rm = T),
            min_roc_auc = min(roc_auc, na.rm = T),
            max_roc_auc = max(roc_auc, na.rm = T))

# plot participants' best performing models and summarise mean AUC and accuracy (NOT split by model type)

individual_best_model_performance_data_sens <- comparing_models_sens %>%
  pivot_longer(cols = c("sens", "spec", "accuracy", "roc_auc"), names_to = "metric") %>%
  mutate(metric = case_when(metric == "sens" ~ "Sensitivity",
                            metric == "spec" ~ "Specificity",
                            metric == "accuracy" ~ "Accuracy",
                            metric == "roc_auc" ~ "AUC")) %>%
  group_by(metric) %>%
  mutate(mean = mean(value, na.rm = TRUE))

annotation <- tibble(metric = factor(c("Accuracy", "Sensitivity", "Specificity", "AUC"), levels = c("Accuracy", "Sensitivity", "Specificity", "AUC")),
                     value = c(0.7, 0.7, 0.5, 0.5),
                     median = individual_best_model_performance_data_sens %>%
                       mutate(metric = factor(metric, levels = c("Accuracy", "Sensitivity", "Specificity", "AUC"))) %>%
                       group_by(metric) %>%
                       summarise(median = median(value, na.rm = TRUE)) %>%
                       pull(median))

individual_best_model_performance_plot_sens <- individual_best_model_performance_data_sens %>%
  mutate(metric = factor(metric, levels = c("Accuracy", "Sensitivity", "Specificity", "AUC"))) %>%
  ggplot(aes(x = value, fill = metric)) +
  geom_histogram() +
  geom_rect(data = annotation, aes(xmin = -Inf, xmax = value, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
  geom_vline(data = annotation, aes(xintercept = median), lwd = 0.7) +
  facet_wrap(~ metric, nrow = 4) +
  scale_fill_viridis_d(alpha = 0.8) +
  scale_x_continuous(limits = c(0, 1.02), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_bw() +
  labs(x = "Performance",
       y = "Count",
       fill = element_blank()) +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"))

if(!file.exists(here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_best_model_performance_plot_sens.png"))) ggsave(individual_best_model_performance_plot_sens, 
                                                                                                                               filename = here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_best_model_performance_plot_sens.png"),
                                                                                                                               dpi = 320, height = 8, width = 10)

# plot 10 most important predictor vars (vip) by model type

extract_model_vip <- function(account_ids = lapse_count, rf_model = individual_models_rf, svm_model = individual_models_svm, elnet_model = individual_models_elnet, xgb_model = individual_models_xgb) {
  
  model_vip <- list()
  
  complete_models <- tibble(account_ids = account_ids,
                            complete_models = lengths(rf_model) >= 3 & lengths(svm_model) >= 3 & lengths(elnet_model) >= 3 & lengths(xgb_model) >= 3) %>%
    filter(complete_models == TRUE)
  
  for(i in 1:nrow(complete_models)) {
    
    n = complete_models$account_ids[i]
    
    n_row <- nrow(bind_rows(rf_model[[n]][["final_rf_vip"]][[1]][,1],
                            svm_model[[n]][["final_svm_vip"]][[1]][,1],
                            elnet_model[[n]][["final_elnet_vip"]][[1]][,1],
                            xgb_model[[n]][["final_xgb_vip"]][[1]][,1]))
    
    n_rf <- nrow(rf_model[[n]][["final_rf_vip"]][[1]][,1])
    
    n_svm <- nrow(svm_model[[n]][["final_svm_vip"]][[1]][,1])
    
    n_elnet <- nrow(elnet_model[[n]][["final_elnet_vip"]][[1]][,1])
    
    n_xgb <- nrow(xgb_model[[n]][["final_xgb_vip"]][[1]][,1])
    
    model_vip[[n]] <- tibble(account_id = rep(n, n_row),
                             variable = c(rf_model[[n]][["final_rf_vip"]][[1]][,1] %>%
                                            pull(Variable),
                                          svm_model[[n]][["final_svm_vip"]][[1]][,1] %>%
                                            pull(Variable),
                                          elnet_model[[n]][["final_elnet_vip"]][[1]][,1] %>%
                                            pull(Variable),
                                          xgb_model[[n]][["final_xgb_vip"]][[1]][,1] %>%
                                            pull(Variable)),
                             importance = c(rf_model[[n]][["final_rf_vip"]][[1]][,2] %>%
                                              pull(Importance),
                                            svm_model[[n]][["final_svm_vip"]][[1]][,2] %>%
                                              pull(Importance),
                                            elnet_model[[n]][["final_elnet_vip"]][[1]][,2] %>%
                                              pull(Importance),
                                            xgb_model[[n]][["final_xgb_vip"]][[1]][,2] %>%
                                              pull(Importance)),
                             model = c(rep(unlist(rf_model[[n]][["metrics_rf"]][[1]][1,5]), n_rf),
                                       rep(unlist(svm_model[[n]][["metrics_svm"]][[1]][1,5]), n_svm),
                                       rep(unlist(elnet_model[[n]][["metrics_elnet"]][[1]][1,5]), n_elnet),
                                       rep(unlist(xgb_model[[n]][["metrics_xgb"]][[1]][1,5]), n_xgb)))
    
  }
  
  model_vip <- bind_rows(model_vip)
  
  return(model_vip %>%
           drop_na(account_id)) 
}

n_participants = 9

individual_vip_by_model_type <- extract_model_vip() %>%
  mutate(model = factor(model, levels = c("xgb", "svm", "elnet", "rf"))) %>%
  group_by(variable, model) %>%
  summarise(n = n()) %>%
  mutate(prop_n = round(n/n_participants, 2)) %>%
  ungroup() %>%
  mutate(variable = str_replace_all(variable, "_imp", " "),
         variable = str_replace_all(variable, "_X1", " "),
         variable = str_replace_all(variable, "_", " "),
         variable = str_to_sentence(variable),
         variable = str_replace_all(variable, "Social context alone", "Social context - Alone"),
         variable = str_replace_all(variable, "Time of day evening", "Time of day - Evening"),
         variable = str_replace_all(variable, "Cigarette availability  easily.available", "Cigarette availability - Easily available"),
         variable = str_replace_all(variable, "Cigarette availability  not.available", "Cigarette availability - Not available"),
         variable = str_replace_all(variable, "Lapse prior", "Prior event lapse"),
         variable = str_replace_all(variable, "Location home", "Location - Home"),
         variable = str_replace_all(variable, "Location public transport", "Location - Public transport"),
         variable = str_replace_all(variable, "Location outside", "Location - Outside"),
         variable = str_replace_all(variable, "Time of day midday", "Time of day - Midday"),
         variable = str_replace_all(variable, "Activity eating", "Activity - Eating"),
         variable = str_replace_all(variable, "Activity other", "Activity - Other"),
         variable = str_replace_all(variable, "Activity socialising", "Activity - Socialising"),
         variable = str_replace_all(variable, "Activity tv", "Activity - Watching TV"),
         variable = str_replace_all(variable, "Location restaurant", "Location - Restaurant"),
         variable = str_replace_all(variable, "Social context relative", "Social context - Relative"),
         variable = str_replace_all(variable, "Location school work", "Location - School/work"),
         variable = str_replace_all(variable, "Location public place", "Location - Public place"),
         variable = str_replace_all(variable, "Activity relaxing", "Activity - Relaxing"),
         variable = str_replace_all(variable, "Activity working", "Activity - Working"),
         variable = str_replace_all(variable, "Location other", "Location - Other"),
         variable = str_replace_all(variable, "Location private vehicle", "Location - Private vehicle"),
         variable = str_replace_all(variable, "Social context other", "Social context - Other"),
         variable = str_replace_all(variable, "Social context partner", "Social context - Partner"),
         variable = str_replace_all(variable, "Time of day night", "Time of day - Night"),
         variable = str_replace_all(variable, "Activity chores", "Activity - Chores"),
         variable = str_replace_all(variable, "Activity walking", "Activity - Walking"),
         variable = str_replace_all(variable, "Activity reading", "Activity - Reading"),
         variable = str_replace_all(variable, "Activity social media", "Activity - Social media"),
         variable = str_replace_all(variable, "Activity music", "Activity - Music"),
         variable = str_replace_all(variable, "Pd 1 hr 3 coefficient", "Change in slope - HR"),
         variable = str_replace_all(variable, "Pd 1 hr 3 standard deviation", "Standard deviation - HR"),
         variable = str_replace_all(variable, "Pd 1 hr 3 min hr", "Min - HR"),
         variable = str_replace_all(variable, "Pd 1 hr 3 max hr", "Max - HR"),
         variable = str_replace_all(variable, "Pd 1 hr 3 rate change", "Rate of change - HR"),
         variable = str_replace_all(variable, "Pd 1 step 3 coefficient", "Change in slope - Steps"),
         variable = str_replace_all(variable, "Pd 1 step 3 standard deviation", "Standard deviation - Steps"),
         variable = str_replace_all(variable, "Pd 1 step 3 min step", "Min - Steps"),
         variable = str_replace_all(variable, "Pd 1 step 3 max step", "Max - Steps"),
         variable = str_replace_all(variable, "Pd 1 step 3 rate change", "Rate of change - Steps")) %>%
  group_by(model) %>%
  arrange(model, -prop_n) %>%
  mutate(variable = fct_inorder(variable)) %>%
  ggplot() +
  geom_point(aes(x = fct_rev(variable), y = model, size = prop_n)) +
  scale_color_brewer(palette = "Dark2") +
  coord_flip() +
  labs(colour = element_blank(),
       size = "Proportion of participants",
       x = element_blank(),
       y = element_blank()) +
  theme_minimal()

if(!file.exists(here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_vip_by_model_type.png"))) ggsave(individual_vip_by_model_type, 
                                                                                                                filename = here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_vip_by_model_type.png"), 
                                                                                                                dpi = 320, height = 8, width = 10)

# plot 10 most important predictor vars (vip) in participants' best performing models

individual_vip_best_performing <- comparing_models_sens %>%
  select(account_id, model) %>%
  left_join(., extract_model_vip(), by = c("account_id", "model")) %>%
  group_by(variable) %>%
  summarise(n = n()) %>%
  mutate(prop_n = round(n/n_participants, 2)) %>%
  ungroup() %>%
  arrange(-prop_n) %>%
  mutate(variable = str_replace_all(variable, "_imp", " "),
         variable = str_replace_all(variable, "_X1", " "),
         variable = str_replace_all(variable, "_", " "),
         variable = str_to_sentence(variable),
         variable = str_replace_all(variable, "Social context alone", "Social context - Alone"),
         variable = str_replace_all(variable, "Time of day evening", "Time of day - Evening"),
         variable = str_replace_all(variable, "Cigarette availability  easily.available", "Cigarette availability - Easily available"),
         variable = str_replace_all(variable, "Cigarette availability  not.available", "Cigarette availability - Not available"),
         variable = str_replace_all(variable, "Lapse prior", "Prior event lapse"),
         variable = str_replace_all(variable, "Location home", "Location - Home"),
         variable = str_replace_all(variable, "Location public transport", "Location - Public transport"),
         variable = str_replace_all(variable, "Location outside", "Location - Outside"),
         variable = str_replace_all(variable, "Time of day midday", "Time of day - Midday"),
         variable = str_replace_all(variable, "Activity eating", "Activity - Eating"),
         variable = str_replace_all(variable, "Activity other", "Activity - Other"),
         variable = str_replace_all(variable, "Activity socialising", "Activity - Socialising"),
         variable = str_replace_all(variable, "Activity tv", "Activity - Watching TV"),
         variable = str_replace_all(variable, "Location restaurant", "Location - Restaurant"),
         variable = str_replace_all(variable, "Social context relative", "Social context - Relative"),
         variable = str_replace_all(variable, "Location school work", "Location - School/work"),
         variable = str_replace_all(variable, "Location public place", "Location - Public place"),
         variable = str_replace_all(variable, "Activity relaxing", "Activity - Relaxing"),
         variable = str_replace_all(variable, "Activity working", "Activity - Working"),
         variable = str_replace_all(variable, "Location other", "Location - Other"),
         variable = str_replace_all(variable, "Location private vehicle", "Location - Private vehicle"),
         variable = str_replace_all(variable, "Social context other", "Social context - Other"),
         variable = str_replace_all(variable, "Social context partner", "Social context - Partner"),
         variable = str_replace_all(variable, "Time of day night", "Time of day - Night"),
         variable = str_replace_all(variable, "Activity chores", "Activity - Chores"),
         variable = str_replace_all(variable, "Activity walking", "Activity - Walking"),
         variable = str_replace_all(variable, "Activity reading", "Activity - Reading"),
         variable = str_replace_all(variable, "Activity social media", "Activity - Social media"),
         variable = str_replace_all(variable, "Activity music", "Activity - Music"),
         variable = str_replace_all(variable, "Pd 3 hr 1 coefficient", "Change in slope - HR"),
         variable = str_replace_all(variable, "Pd 3 hr 1 standard deviation", "Standard deviation - HR"),
         variable = str_replace_all(variable, "Pd 3 hr 1 min hr", "Min - HR"),
         variable = str_replace_all(variable, "Pd 3 hr 1 max hr", "Max - HR"),
         variable = str_replace_all(variable, "Pd 3 hr 1 rate change", "Rate of change - HR"),
         variable = str_replace_all(variable, "Pd 3 step 1 coefficient", "Change in slope - Steps"),
         variable = str_replace_all(variable, "Pd 3 step 1 standard deviation", "Standard deviation - Steps"),
         variable = str_replace_all(variable, "Pd 3 step 1 min step", "Min - Steps"),
         variable = str_replace_all(variable, "Pd 3 step 1 max step", "Max - Steps"),
         variable = str_replace_all(variable, "Pd 3 step 1 rate change", "Rate of change - Steps"),
         model = "Best-performing individual-level algorithm") %>%
  arrange(-prop_n) %>%
  mutate(variable = fct_inorder(variable)) %>%
  ggplot() +
  geom_point(aes(x = fct_rev(variable), y = model, size = prop_n)) +
  scale_color_brewer(palette = "Dark2") +
  coord_flip() +
  labs(colour = element_blank(),
       size = "Proportion of participants",
       x = element_blank(),
       y = element_blank()) +
  theme_minimal()

if(!file.exists(here("outputs", "sensitivity_confounders", "individual models (sensor data)", "individual_vip_best_performing.png"))) ggsave(individual_vip_best_performing, 
                                                                                                                  filename = here("outputs", "sensitivity_confounders", "individual models (sensor data)" , "individual_vip_best_performing.png"), 
                                                                                                                  dpi = 320, height = 6, width = 6)

# run 25_ML_analyses_hybrid_sensor.R on reduced dataset with EMAs with potential confounders removed

df_hybrid <- conf_removed %>%
  group_by(id) %>%
  mutate(lapse_events = n()) %>%
  mutate(prop_lapses = sum(lapse_lagged == "yes", na.rm = T)/lapse_events,
         prop_non_lapses = sum(lapse_lagged == "no", na.rm = T)/lapse_events) %>%
  filter(prop_lapses > 0 & prop_non_lapses > 0) %>%
  ungroup()

df_hybrid %>% 
  summarise(unique(id)) # 17 participants available for analysis

# select variables to include

df_h <- df_hybrid %>%
  select(-starts_with("pd_1")) %>%
  select(-starts_with("pd_2")) %>%
  select(-starts_with("pd_3_hr_2")) %>%
  select(-starts_with("pd_3_hr_3")) %>%
  select(-starts_with("pd_3_step_2")) %>%
  select(-starts_with("pd_3_step_3")) %>%
  drop_na(lapse_lagged)

df_h$lapse_lagged <- relevel(df$lapse_lagged, "yes")
df_h$time_of_day <- factor(df$time_of_day, levels = c("morning", "midday", "evening", "night"))

# split into separate lists

df_split <- df_h %>%
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
  
  rf_rec <- recipe(lapse_lagged ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(cpd, age) %>%
    step_downsample(lapse_lagged, skip = TRUE) # manages class imbalances
  
  rf_prep <- prep(rf_rec)
  rf_juiced <- juice(rf_prep)
  
  rf_spec <- rand_forest(
    mtry = 5,
    trees = 500,
    min_n = 3
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
    
    df_model_fit_hybrid[[i]] <- list(auc = auc)
    
    names(df_model_fit_hybrid)[[i]] <- names(df_split_training)[[i]]
  }
  
}

# summarise model performance for each individual -------------------------

model_performance_hybrid <- tibble(account_id = names(df_model_fit_hybrid[lengths(df_model_fit_hybrid) >= 1]),
                                   auc = unlist(map(df_model_fit_hybrid, "auc")))

median(model_performance_hybrid$auc)
range(model_performance_hybrid$auc)
