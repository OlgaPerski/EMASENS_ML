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
                                           TRUE ~ "night")))

analytic_sample_clean <- write_rds(analytic_sample_clean, here("data", "group models (no sensor data)", "analytic_sample_clean.rds"))

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

write_rds(vip_rf, here("data", "group models (no sensor data)", "group_vip_rf.rds"))

if(!file.exists(here("outputs", "group models (no sensor data)", "group_vip_rf.png"))) ggsave(vip_rf, filename = here("outputs", "group models (no sensor data)", "group_vip_rf.png"), 
                                                                             dpi = 320, height = 4, width = 6)

### support vector machine

svm_spec_best <- svm_rbf(
  cost = best_svm_auc$cost, #input values from best performing SVM above
  rbf_sigma = best_svm_auc$rbf_sigma
) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svm_fit <- workflow() %>%
  add_model(svm_spec_best) %>%
  add_formula(lapse_lagged ~ .) %>%
  fit(juice(rf_prep)) 

vip_svm <- svm_fit %>%
  extract_fit_parsnip() %>%
  vip(method = "permute", 
      target = "lapse_lagged", metric = "auc", reference_class = "yes",
      pred_wrapper = kernlab::predict, train = juice(rf_prep))

vip_svm_prep <- vip_svm$data

vip_svm_plot <- vip_svm_prep %>%
  mutate(Variable = str_remove(Variable, "_X1"),
         Variable = str_remove(Variable, "_imp"),
         Variable = str_replace(Variable, "_", " "),
         Variable = str_to_sentence(Variable),
         Variable = str_replace(Variable, "Motivation to_stop_i.think.i.should.stop.smoking.but.don.t.really.want.to", 
                                "Motivation to stop - I think I should stop but don't really want to"),
         Variable = str_replace(Variable, "Location public_place", "Location - Public place"),
         Variable = str_replace(Variable, "Social context_partner", "Social context - Partner"),
         Variable = str_replace(Variable, "Time to_first_cig_x31.60.minutes", "Time to first cigarette - 31-60 minutes"),
         Variable = str_replace(Variable, "Location public_transport", "Location - Public transport"),
         Variable = str_replace(Variable, "Activity social_media", "Activity - Social media"),
         Variable = str_replace(Variable, "Activity chores", "Activity - Chores")) %>%
  arrange(-Importance) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  ggplot() +
  geom_col(aes(x = Importance, y = fct_rev(Variable)), fill = "black") +
  labs(y = "Variable") +
  theme_minimal()

write_rds(vip_svm_plot, here("data", "group models (no sensor data)", "group_vip_svm_plot.rds"))

if(!file.exists(here("outputs", "group models (no sensor data)", "group_vip_svm.png"))) ggsave(vip_svm_plot, filename = here("outputs", "group models (no sensor data)", "group_vip_svm.png"), 
                                                                              dpi = 320, height = 8, width = 10)

### elastic net

vip_elnet <- final_elnet %>%
  set_engine("glmnet", importance = "permutation") %>%
  fit(lapse_lagged ~ .,
      data = juice(rf_prep)) %>%
  vip()

vip_elnet <- vip_elnet$data %>%
  mutate(Variable = str_replace(Variable, "ethnicity_Black..Black.British..Caribbean.or.African..any.Black..Black.British.or.Caribbean.background.",
                                "Ethnicity - Black, Black British, Caribbean or African"),
         Variable = str_replace(Variable, "ethnicity_Other.ethnic.group..i.e...Arab.", "Ethnicity - Other ethnic group"),
         Variable = str_replace(Variable, "time_to_first_cig_After.60.minutes", "Time to first cigarette - >60 minutes"),
         Variable = str_replace(Variable, "motivation_to_stop_I.really.want.to.stop.smoking.and.intend.to.in.the.next.3.months", 
                                "Motivation to stop - I really want to stop smoking and intend to in the next 3 months"),
         Variable = str_replace(Variable, "location_other_imp_X1", "Location - Other"),
         Variable = str_replace(Variable, "job_type_Other..e.g...student..unemployed..retired.", "Job type - Other"),
         Variable = str_replace(Variable, "time_to_first_cig_X6.30.minutes", "Time to first cigarette - 6-30 minutes"),
         Variable = str_replace(Variable, "activity_chores_imp_X1", "Activity - Chores"),
         Variable = str_replace(Variable, "ethnicity_White..any.White.background.", "Ethnicity - White"),
         Variable = str_replace(Variable, "location_private_vehicle_imp_X1", "Location - Private vehicle")) %>%
  arrange(-Importance) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  ggplot() +
  geom_col(aes(x = Importance, y = fct_rev(Variable)), fill = "black") +
  labs(y = "Variable") +
  theme_minimal()

write_rds(vip_elnet, here("data", "group models (no sensor data)", "group_vip_elnet.rds"))

if(!file.exists(here("outputs", "group models (no sensor data)", "group_vip_elnet.png"))) ggsave(vip_elnet, filename = here("outputs", "group models (no sensor data)", "group_vip_elnet.png"), 
                                                                                dpi = 320, height = 8, width = 10)

### xgboost

vip_xgb <- final_xgb %>%
  set_engine("xgboost") %>%
  fit(lapse_lagged ~ .,
      data = juice(rf_prep)) %>%
  vip()

vip_xgb <- vip_xgb$data %>%
  mutate(Variable = str_remove(Variable, "_imp"),
         Variable = str_remove(Variable, "_X1"),
         Variable = str_to_sentence(Variable),
         Variable = str_replace(Variable, "Lapse_prior", "Prior event lapse"),
         Variable = str_replace(Variable, "Cigarette_availability_not.available", "Cigarette availability - Not available"),
         Variable = str_replace(Variable, "Cpd", "CPD"),
         Variable = str_replace(Variable, "Study_day", "Study day"),
         Variable = str_replace(Variable, "Cigarette_availability_easily.available", "Cigarette availability - Easily available"),
         Variable = str_replace(Variable, "Job_type_manual", "Job type - Manual")) %>%
  arrange(-Importance) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  ggplot() +
  geom_col(aes(x = Importance, y = fct_rev(Variable)), fill = "black") +
  labs(y = "Variable") +
  theme_minimal()

write_rds(vip_xgb, here("data", "group models (no sensor data)", "group_vip_xgb.rds"))

if(!file.exists(here("outputs", "group models (no sensor data)", "group_vip_xgb.png"))) ggsave(vip_xgb, filename = here("outputs", "group models (no sensor data)", "group_vip_xgb.png"), 
                                                                              dpi = 320, height = 8, width = 10)

# save within single grid plot

combined_vip <- plot_grid(vip_rf, vip_svm_plot, vip_elnet, vip_xgb, labels = c("RF", "SVM", "ELNET", "XGBOOST"), 
                          label_size = 10, ncol = 2, nrow = 2)

if(!file.exists(here("outputs", "group models (no sensor data)", "group_combined_vip.png"))) save_plot(combined_vip, filename = here("outputs", "group models (no sensor data)", "group_combined_vip.png"), 
                                                                                      base_height = 10, base_width = 12)

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

# summarise model performance ---------------------------------------------

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

flextable(table_performance) %>% 
  theme_vanilla() %>% 
  save_as_docx(path = here("outputs", "group models (no sensor data)", "table_performance.docx"))

# save specs for best-performing model --------------------------------

# although the SVM had a slightly higher AUC, it had unacceptable sensitivity
# therefore, the RF was selected

write_rds(final_rf, here("data", "group models (no sensor data)", "final_rf.rds"))

# summarise confusion matrices ---------------------

### random forest

collect_predictions(final_rf_res) %>%
  conf_mat(lapse_lagged, .pred_class)

### support vector machine

collect_predictions(final_svm_res) %>%
  conf_mat(lapse_lagged, .pred_class)

### elastic net

collect_predictions(final_elnet_res) %>%
  conf_mat(lapse_lagged, .pred_class)

### xgboost

collect_predictions(final_xgb_res) %>%
  conf_mat(lapse_lagged, .pred_class)

# prepare for model comparison --------------------------------------------

### random forest

rf_auc <-
  final_rf_res %>%
  collect_predictions() %>%
  roc_curve(lapse_lagged, .pred_yes) %>%
  mutate(model = "Random Forest")

### support vector machine

svm_auc <-
  final_svm_res %>%
  collect_predictions() %>%
  roc_curve(lapse_lagged, .pred_yes) %>%
  mutate(model = "Support Vector Machine")

### elastic net

elnet_auc <-
  final_elnet_res %>%
  collect_predictions() %>%
  roc_curve(lapse_lagged, .pred_yes) %>%
  mutate(model = "Penalised Logistic Regression")

### xgboost

xgb_auc <-
  final_xgb_res %>%
  collect_predictions() %>%
  roc_curve(lapse_lagged, .pred_yes) %>%
  mutate(model = "XGBoost")

### compare model performance

combined_roc <- bind_rows(rf_auc, elnet_auc, svm_auc, xgb_auc) %>%
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

write_rds(combined_roc, here("data", "group models (no sensor data)", "group_combined_roc.rds"))

if(!file.exists(here("outputs", "group models (no sensor data)", "group_combined_roc.png"))) ggsave(combined_roc, filename = here("outputs", "group models (no sensor data)", "group_combined_roc.png"),
                                                                                   dpi = 320, height = 8, width = 10)
