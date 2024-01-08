analytic_sample_clean <- read_rds(here("data", "group models (no sensor data)", "analytic_sample_clean.rds"))

exclude <- c("nr", "EMA_id", "EMA_hour", 
             "age", "sex", "post_16_edu", "ethnicity", "past_quit_attempt", "job_type", # remove vars with zero variance (baseline characteristics)
             "cpd", "time_to_first_cig", "motivation_to_stop", "pharma_support", "beh_support",
             "lapse_event_imp", "nrt_no_presc", "nrt_presc", "zyban", "varen",
             "ecig", "group_supp", "ind_supp", "helpline", "book", "website",
             "app", "none", "other", "mpath_data", "fitbit_data", "start_date",
             "time_first_prompt", "end_date", "time_last_prompt", "participant_specific_variable_1",
             "participant_specific_variable_2", "last_time_smoke_fup", "CO_reading", "CO_reading1",
             "CO_reading2", "altered_prompt", "altered_prompt_start_time", "altered_prompt_study_day",
             "start_datetime", "end_datetime", "smoking_fup", "EMA_date", "EMA_number", "EMA_date_time",
             "location_others_home_imp", "activity_child_care_imp", # remove pred vars with no or very little variance
             "social_context_friend_imp", "social_context_child_imp", 
             "social_context_colleague_imp", "social_context_stranger_imp")

df <- analytic_sample_clean %>%
  select(-all_of(exclude)) %>%
  drop_na(lapse_lagged)

df$lapse_lagged <- relevel(df$lapse_lagged, "yes")
df$time_of_day <- factor(df$time_of_day, levels = c("morning", "midday", "evening", "night"))

# remove participants with 0% or 100% lapses

df_ <- df %>%
  group_by(id) %>%
  mutate(lapse_events = n()) %>%
  mutate(prop_lapses = sum(lapse_lagged == "yes", na.rm = T)/lapse_events,
         prop_non_lapses = sum(lapse_lagged == "no", na.rm = T)/lapse_events) %>%
  filter(prop_lapses > 0 & prop_non_lapses > 0) %>%
  ungroup()

df_ %>% 
  summarise(unique(id)) # 28 participants

# set additional cut-off for inclusion

lapse_count <- df %>%
  group_by(id, lapse_lagged) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = lapse_lagged, values_from = n_lapses) %>%
  filter(no > 5 & yes > 5) %>%
  pull(id)

lapse_count <- as.character(droplevels(lapse_count)) # 15 participants available for analysis

rf_sample <- df %>% # does not contain prop_lapse vars above as this stems from df and not df_
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

write_rds(individual_models_rf, here("data", "individual models (no sensor data)", "individual_models_rf.rds"))

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

write_rds(individual_models_svm, here("data", "individual models (no sensor data)", "individual_models_svm.rds"))

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

write_rds(individual_models_elnet, here("data", "individual models (no sensor data)", "individual_models_elnet.rds"))

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

write_rds(individual_models_xgb, here("data", "individual models (no sensor data)", "individual_models_xgb.rds"))

# extract model performance metrics ------------------------------

individual_models_rf <- read_rds(here("data", "individual models (no sensor data)", "individual_models_rf.rds"))
individual_models_svm <- read_rds(here("data", "individual models (no sensor data)", "individual_models_svm.rds"))
individual_models_elnet <- read_rds(here("data", "individual models (no sensor data)", "individual_models_elnet.rds"))
individual_models_xgb <- read_rds(here("data", "individual models (no sensor data)", "individual_models_xgb.rds"))

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

individual_model_performance_plot <- extract_model_characteristics() %>%
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

if(!file.exists(here("outputs", "individual models (no sensor data)", "individual_model_performance_plot.png"))) ggsave(individual_model_performance_plot, 
                                                                                  filename = here("outputs", "individual models (no sensor data)", "individual_model_performance_plot.png"),
                                                                                  dpi = 320, height = 8, width = 10)

# select best performing model for each participant

comparing_models <- extract_model_characteristics() %>%
  mutate(model = factor(model, levels = c("rf", "elnet", "svm", "xgb"))) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  group_by(account_id) %>%
  arrange(-roc_auc, -accuracy, -spec, -sens, model) %>%
  dplyr::slice(1)

# summarise mean sens, spec, accuracy and AUC for participants' best performing models

comparing_models %>%
  tabyl(model)

summary_vals <- comparing_models %>%
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

# plot participants' best performing models and summarise mean AUC and accuracy split by model type

individual_best_model_performance_stratified_plot <- comparing_models %>%
  pivot_longer(cols = c("sens", "spec", "accuracy", "roc_auc"), names_to = "metric") %>%
  filter(str_detect(metric, "roc_auc|accuracy")) %>%
  mutate(model = str_to_upper(model),
         metric = case_when(metric == "accuracy" ~ "Accuracy",
                            metric == "roc_auc" ~ "AUC")) %>%
  ggplot(aes(x = value,
             fill = model)) +
  geom_histogram() +
  facet_wrap(~ metric + model, nrow = 2) +
  scale_fill_viridis_d(alpha = 0.8) +
  scale_x_continuous(limits = c(0, 1.1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_bw() +
  labs(x = "Performance",
       y = "Count",
       fill = element_blank()) +
  theme(legend.position = "none")

if(!file.exists(here("outputs", "individual models (no sensor data)", "individual_best_model_performance_stratified_plot.png"))) 
  ggsave(individual_best_model_performance_stratified_plot, 
         filename = 
           here("outputs", "individual models (no sensor data)", "individual_best_model_performance_stratified_plot.png"),
         dpi = 320, height = 8, width = 10)

# plot participants' best performing models and summarise mean AUC and accuracy (NOT split by model type)

individual_best_model_performance_data <- comparing_models %>%
  pivot_longer(cols = c("sens", "spec", "accuracy", "roc_auc"), names_to = "metric") %>%
  mutate(metric = case_when(metric == "sens" ~ "Sensitivity",
                            metric == "spec" ~ "Specificity",
                            metric == "accuracy" ~ "Accuracy",
                            metric == "roc_auc" ~ "AUC")) %>%
  group_by(metric) %>%
  mutate(mean = mean(value, na.rm = TRUE))

annotation <- tibble(metric = factor(c("Accuracy", "Sensitivity", "Specificity", "AUC"), levels = c("Accuracy", "Sensitivity", "Specificity", "AUC")),
                     value = c(0.7, 0.7, 0.5, 0.5),
                     median = individual_best_model_performance_data %>%
                       mutate(metric = factor(metric, levels = c("Accuracy", "Sensitivity", "Specificity", "AUC"))) %>%
                       group_by(metric) %>%
                       summarise(median = median(value, na.rm = TRUE)) %>%
                       pull(median))

individual_best_model_performance_plot <- individual_best_model_performance_data %>%
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

if(!file.exists(here("outputs", "individual models (no sensor data)", "individual_best_model_performance_plot.png"))) 
  ggsave(individual_best_model_performance_plot, 
         filename = here("outputs", "individual models (no sensor data)", "individual_best_model_performance_plot.png"),
         dpi = 320, height = 8, width = 10)

# compare with group model --------------------------------------------------

group_auc <- read_rds(here("data", "group to individual models (no sensor data)", "group_to_individual.rds")) %>%
  mutate(account_id = id)

individual_to_group <- individual_best_model_performance_data %>%
  filter(metric == "AUC") %>%
  ungroup() %>%
  select(account_id, model, model_auc = value) %>%
  left_join(group_auc %>%
              rename(group_auc = auc), by = c("account_id")) %>%
  mutate(individual_preferred = model_auc >= group_auc)

median(individual_to_group$model_auc)
range(individual_to_group$model_auc)

sum(individual_to_group$individual_preferred == TRUE)/15*100
sum(individual_to_group$individual_preferred == FALSE)/15*100

individual_to_group_plot <- individual_to_group %>%
  ggplot() +
  geom_point(aes(x = group_auc, y = model_auc, colour = individual_preferred)) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("blue", "black")) +
  theme_bw() +
  labs(x = "Group-level algorithm AUC",
       y = "Individual-level algorithm AUC") +
  theme(legend.position = "none")

if(!file.exists(here("outputs", "individual models (no sensor data)", "individual_to_group_plot.png"))) 
  ggsave(individual_to_group_plot, 
         filename = here("outputs", "individual models (no sensor data)", "individual_to_group_plot.png"),
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

n_participants = 15

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
         variable = str_replace_all(variable, "Activity music", "Activity - Music")) %>%
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

if(!file.exists(here("outputs", "individual models (no sensor data)", "individual_vip_by_model_type.png"))) 
  ggsave(individual_vip_by_model_type, 
         filename = here("outputs", "individual models (no sensor data)", "individual_vip_by_model_type.png"), 
         dpi = 320, height = 8, width = 10)

# plot 10 most important predictor vars (vip) in participants' best performing models

individual_vip_best_performing <- comparing_models %>%
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

if(!file.exists(here("outputs", "individual models (no sensor data)", "individual_vip_best_performing.png"))) 
  ggsave(individual_vip_best_performing, 
         filename = here("outputs", "individual models (no sensor data)" , "individual_vip_best_performing.png"), 
         dpi = 320, height = 6, width = 6)
