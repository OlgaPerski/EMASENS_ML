# select variables to include ---------------------------------------------

analytic_sample_2 <- read_rds(here("data", "analytic_sample_2.rds")) 

analytic_sample_clean_sens <- analytic_sample_2 %>%
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
                                           TRUE ~ "night")))

analytic_sample_clean_sens <- write_rds(analytic_sample_clean_sens, here("data", "group models (sensor data)", "analytic_sample_clean_sens.rds"))

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

df_sens$lapse_lagged <- relevel(df_sens$lapse_lagged, "yes")
df_sens$time_of_day <- factor(df_sens$time_of_day, levels = c("morning", "midday", "evening", "night"))

# setup 9 different data frames for each of the prediction distances (1-3) and time windows (1-3), for both steps and HR
# tune the different models on these data frames
# once tuning is complete, compare model performance to identify the best prediction distance and time window to take forwards

set.seed(555)

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

sens_pd1_tw1 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_1", timewindow = "hr_1|step_1")
sens_pd1_tw2 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_1", timewindow = "hr_2|step_2")
sens_pd1_tw3 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_1", timewindow = "hr_3|step_3")

# pd2

sens_pd2_tw1 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_2", timewindow = "hr_1|step_1")
sens_pd2_tw2 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_2", timewindow = "hr_2|step_2")
sens_pd2_tw3 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_2", timewindow = "hr_3|step_3")

# pd3

sens_pd3_tw1 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_3", timewindow = "hr_1|step_1")
sens_pd3_tw2 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_3", timewindow = "hr_2|step_2")
sens_pd3_tw3 <- run_models_pd_tw(df = df_sens, prediction_distance = "pd_3", timewindow = "hr_3|step_3")

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

flextable(table_performance) %>% 
  theme_vanilla() %>% 
  save_as_docx(path = here("outputs", "group models (sensor data)", "table_performance.docx"))

# save specs for best-performing model --------------------------------

write_rds(sens_pd1_tw3$model_specs$rf, here("data", "group models (sensor data)", "final_rf_sens.rds"))

# plot variable feature importance for the prediction distance and time window with the highest AUCs across the models

### random forest

vip_rf <- sens_pd1_tw3$model_specs$rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(lapse_lagged ~ .,
      data = juice(sens_pd1_tw3$model_prep)) %>%
  vip()

vip_rf_sens <- vip_rf$data %>%
  mutate(Variable = str_remove(Variable, "_imp"),
         Variable = str_replace(Variable, "_", " "),
         Variable = str_remove(Variable, "_X1"),
         Variable = str_to_sentence(Variable),
         Variable = str_replace(Variable, "Cpd", "CPD"),
         Variable = str_replace(Variable, "Lapse prior", "Prior event lapse"),
         Variable = str_replace(Variable, "Motivation to_stop_i.think.i.should.stop.smoking.but.don.t.really.want.to", 
                                "Motivation to stop - I think I should stop smoking but don't really want to"),
         Variable = str_replace(Variable, "Cigarette availability_not.available", "Cigarette availability - Not available"),
         Variable = str_replace(Variable, "Ethnicity white..any.white.background.", "Ethnicity - White"),
         Variable = str_replace(Variable, "Time to_first_cig_x31.60.minutes", "Time to first cigarette - 31-60 minutes"),
         Variable = str_replace(Variable, "Pharma support_yes", "Pharmacological support")) %>%
  arrange(-Importance) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  ggplot() +
  geom_col(aes(x = Importance, y = fct_rev(Variable)), fill = "black") +
  labs(y = "Variable") +
  theme_minimal()

write_rds(vip_rf_sens, here("data", "group models (sensor data)", "group_vip_rf_sens.rds"))

if(!file.exists(here("outputs", "group models (sensor data)", "group_vip_rf_sens.png"))) ggsave(vip_rf_sens, filename = here("outputs", "group models (sensor data)", "group_vip_rf_sens.png"), 
                                                                                                dpi = 320, height = 4, width = 6)

### support vector machine

svm_spec_best <- svm_rbf(
  cost = sens_pd1_tw3$svm_specific$cost, #input values from best performing SVM above
  rbf_sigma = sens_pd1_tw3$svm_specific$rbf_sigma
) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svm_fit <- workflow() %>%
  add_model(svm_spec_best) %>%
  add_formula(lapse_lagged ~ .) %>%
  fit(juice(sens_pd1_tw3$model_prep)) 

vip_svm <- svm_fit %>%
  extract_fit_parsnip() %>%
  vip(method = "permute", 
      target = "lapse_lagged", metric = "auc", reference_class = "yes",
      pred_wrapper = kernlab::predict, train = juice(sens_pd1_tw3$model_prep))

vip_svm_prep_sens <- vip_svm$data

vip_svm_sens <- vip_svm_prep_sens %>%
  mutate(Variable = str_remove(Variable, "_X1"),
         Variable = str_remove(Variable, "_imp"),
         Variable = str_replace(Variable, "_", " "),
         Variable = str_to_sentence(Variable),
         Variable = str_replace(Variable, "Cigarette availability_not.available", 
                                "Cigarette availability - Not available"),
         Variable = str_replace(Variable, "Time of_day_evening", "Time of day - Evening"),
         Variable = str_replace(Variable, "Cpd", "CPD"),
         Variable = str_replace(Variable, "Pd 1_hr_3_coefficient", "Change in slope - HR"),
         Variable = str_replace(Variable, "Pd 1_step_3_coefficient", "Change in slope - Steps"),
         Variable = str_replace(Variable, "Sex male", "Sex - Male"),
         Variable = str_replace(Variable, "Job type_manual", "Job type - Manual"),
         Variable = str_replace(Variable, "Job type_other..e.g...student..unemployed..retired.", "Job type - Other")) %>%
  arrange(-Importance) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  ggplot() +
  geom_col(aes(x = Importance, y = fct_rev(Variable)), fill = "black") +
  labs(y = "Variable") +
  theme_minimal()

write_rds(vip_svm_sens, here("data", "group models (sensor data)", "group_vip_svm_sens.rds"))

if(!file.exists(here("outputs", "group models (sensor data)", "group_vip_svm_sens.png"))) ggsave(vip_svm_sens, filename = here("outputs", "group models (sensor data)", "group_vip_svm_sens.png"), 
                                                                                               dpi = 320, height = 8, width = 10)

### elastic net

vip_elnet <- sens_pd1_tw3$model_specs$elnet %>%
  set_engine("glmnet", importance = "permutation") %>%
  fit(lapse_lagged ~ .,
      data = juice(sens_pd1_tw3$model_prep)) %>%
  vip()

vip_elnet <- vip_elnet$data 

vip_elnet_sens <- vip_elnet %>%
  mutate(Variable = str_replace(Variable, "ethnicity_Black..Black.British..Caribbean.or.African..any.Black..Black.British.or.Caribbean.background.",
                                "Ethnicity - Black, Black British, Caribbean or African"),
         Variable = str_replace(Variable, "ethnicity_Other.ethnic.group..i.e...Arab.", "Ethnicity - Other ethnic group"),
         Variable = str_replace(Variable, "time_to_first_cig_After.60.minutes", "Time to first cigarette - >60 minutes"),
         Variable = str_replace(Variable, "job_type_Other..e.g...student..unemployed..retired.", 
                                "Job type - Other"),
         Variable = str_replace(Variable, "job_type_Manual", 
                                "Job type - Manual"),
         Variable = str_replace(Variable, "activity_reading_imp_X1", "Activity - Reading"),
         Variable = str_replace(Variable, "past_quit_attempt_Yes..but.not.in.the.past.year", "Past quit attempt - Yes, but not in the past year"),
         Variable = str_replace(Variable, "past_quit_attempt_Yes..in.the.past.year", "Past quit attempt - Yes, in the past year"),
         Variable = str_replace(Variable, "time_to_first_cig_X6.30.minutes", "Time to first cigarette - 6-30 minutes"),
         Variable = str_replace(Variable, "activity_chores_imp_X1", "Activity - Chores"),
         Variable = str_replace(Variable, "ethnicity_White..any.White.background.", "Ethnicity - White"),
         Variable = str_replace(Variable, "ethnicity_Mixed.or.multiple.ethnic.groups..e.g...White.and.Black.African..White.and.Asian.",
                                "Ethnicity - Multiple"),
         Variable = str_replace(Variable, "motivation_to_stop_I.think.I.should.stop.smoking.but.don.t.really.want.to",
                                "Motivation to stop - I think I should stop smoking but don't really want to"),
         Variable = str_replace(Variable, "location_public_transport_imp_X1", "Location - Public transport")) %>%
  arrange(-Importance) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  ggplot() +
  geom_col(aes(x = Importance, y = fct_rev(Variable)), fill = "black") +
  labs(y = "Variable") +
  theme_minimal()

write_rds(vip_elnet_sens, here("data", "group models (sensor data)", "group_vip_elnet_sens.rds"))

if(!file.exists(here("outputs", "group models (sensor data)", "group_vip_elnet_sens.png"))) ggsave(vip_elnet_sens, filename = here("outputs", "group models (sensor data)", "group_vip_elnet_sens.png"), 
                                                                                                 dpi = 320, height = 8, width = 10)

### xgboost

vip_xgb <- sens_pd1_tw3$model_specs$xgb %>%
  set_engine("xgboost") %>%
  fit(lapse_lagged ~ .,
      data = juice(sens_pd1_tw3$model_prep)) %>%
  vip()

vip_xgb_sens <- vip_xgb$data %>%
  mutate(Variable = str_remove(Variable, "_imp"),
         Variable = str_remove(Variable, "_X1"),
         Variable = str_to_sentence(Variable),
         Variable = str_replace(Variable, "Lapse_prior", "Prior event lapse"),
         Variable = str_replace(Variable, "Cigarette_availability_not.available", "Cigarette availability - Not available"),
         Variable = str_replace(Variable, "Cpd", "CPD"),
         Variable = str_replace(Variable, "Study_day", "Study day"),
         Variable = str_replace(Variable, "Motivation_to_stop_i.think.i.should.stop.smoking.but.don.t.really.want.to", "Motivation to stop - I think I should stop smoking but don't really want to"),
         Variable = str_replace(Variable, "Time_to_first_cig_x31.60.minutes", "Time to first cigarette - 31-60 minutes"),
         Variable = str_replace(Variable, "Job_type_manual", "Job type - Manual"),
         Variable = str_replace(Variable, "Ethnicity_white..any.white.background.", "Ethnicity - White"),
         Variable = str_replace(Variable, "Pd_1_hr_3_standard_deviation", "Standard deviation - HR")) %>%
  arrange(-Importance) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  ggplot() +
  geom_col(aes(x = Importance, y = fct_rev(Variable)), fill = "black") +
  labs(y = "Variable") +
  theme_minimal()

write_rds(vip_xgb_sens, here("data", "group models (sensor data)", "group_vip_xgb_sens.rds"))

if(!file.exists(here("outputs", "group models (sensor data)", "group_vip_xgb_sens.png"))) ggsave(vip_xgb_sens, filename = here("outputs", "group models (sensor data)", "group_vip_xgb_sens.png"), 
                                                                                               dpi = 320, height = 8, width = 10)

# save within single grid plot

combined_vip_sens <- plot_grid(vip_rf_sens, vip_svm_sens, vip_elnet_sens, vip_xgb_sens, labels = c("RF", "SVM", "ELNET", "XGBOOST"), 
                          label_size = 10, ncol = 2, nrow = 2)

if(!file.exists(here("outputs", "group models (sensor data)", "group_combined_vip_sens.png"))) save_plot(combined_vip_sens, filename = here("outputs", "group models (sensor data)", "group_combined_vip_sens.png"), 
                                                                                                       base_height = 10, base_width = 12)

# prepare for model comparison --------------------------------------------

### random forest

rf_auc <-
  sens_pd1_tw3$model_res$rf %>%
  collect_predictions() %>%
  roc_curve(lapse_lagged, .pred_yes) %>%
  mutate(model = "Random Forest")

### support vector machine

svm_auc <-
  sens_pd1_tw3$model_res$svm %>%
  collect_predictions() %>%
  roc_curve(lapse_lagged, .pred_yes) %>%
  mutate(model = "Support Vector Machine")

### elastic net

elnet_auc <-
  sens_pd1_tw3$model_res$elnet %>%
  collect_predictions() %>%
  roc_curve(lapse_lagged, .pred_yes) %>%
  mutate(model = "Penalised Logistic Regression")

### xgboost

xgb_auc <-
  sens_pd1_tw3$model_res$xgb %>%
  collect_predictions() %>%
  roc_curve(lapse_lagged, .pred_yes) %>%
  mutate(model = "XGBoost")

### compare model performance

combined_roc_sens <- bind_rows(rf_auc, elnet_auc, svm_auc, xgb_auc) %>%
  mutate(model = factor(model, levels = c("Random Forest", "XGBoost", "Support Vector Machine", "Penalised Logistic Regression"))) %>%
  ggplot() +
  geom_line(aes(x = 1 - specificity, y = sensitivity, colour = model), lwd = 1.2) + # 1-specificity is the false positive rate; sensitivity is the true positive rate
  geom_abline(lty = 3) +
  coord_equal() +
  scale_color_viridis_d(option = "plasma", end = .6) +
  xlab("False Positive Rate (1 - Specificity)") +
  ylab("True Positive Rate (Sensitivity)") +
  labs(col = "Model") +
  theme_minimal()

write_rds(combined_roc_sens, here("data", "group models (sensor data)", "group_combined_roc_sens.rds"))

if(!file.exists(here("outputs", "group models (sensor data)", "group_combined_roc_sens.png"))) ggsave(combined_roc_sens, filename = here("outputs", "group models (sensor data)", "group_combined_roc_sens.png"),
                                                                                                    dpi = 320, height = 8, width = 10)

### set up auc comparison

models <- c("Random Forest", "Penalised Logistic Regression", "XGBoost", "Support Vector Machine")
auc <- c(0.952, 0.944, 0.933, 0.865)
lower_ci <- c(0.933, 0.921, 0.907, 0.822)
upper_ci <- c(0.970, 0.966, 0.959, 0.909)

# combine data into a data frame
data <- tibble(models, auc, lower_ci, upper_ci) %>%
  mutate(models = fct_inorder(models))

compare_auc_sens <- ggplot(data, aes(x = models, y = auc)) +
  geom_pointrange(aes(x = models, ymin = lower_ci, ymax = upper_ci, colour = models)) +
  scale_color_viridis_d(option = "plasma", end = .6) +
  xlab("Model") +
  ylab("AUC") +
  labs(colour = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# add error bars for confidence intervals
segments(1:4, data$lower_ci, 1:4, data$upper_ci, col = "black", lwd = 2)

ggsave(compare_auc_sens, filename = here("outputs", "group models (sensor data)", "compare_auc_sens.png"),
       dpi = 320, height = 6, width = 9)

