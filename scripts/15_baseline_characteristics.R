# summarise baseline characteristics

demographics <- read_rds(here("data", "demographics.rds"))
ema_responses_df <- read_rds(here("data", "ema_responses_df.rds"))

# remove ppts with <60% EMA adherence

ema_prop_compl <- ema_responses_df %>%
  filter(is.na(button_press)) %>%
  group_by(id) %>%
  summarise(prop_completed = sum(!is.na(EMA_date_time))/160*100) %>%
  drop_na()

exclude_ids <- ema_prop_compl %>%
  filter(prop_completed < 60)

# table 1a - EMA data

demographics_compl_ema <- left_join(demographics, ema_prop_compl, by = "id") %>%
  mutate(group = case_when(id %in% exclude_ids$id ~ "Exclude",
                           TRUE ~ "Include"),
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
         pharma_support = case_when(nrt_no_presc == "yes" | nrt_presc == "yes" | zyban == "yes" | varen == "yes" | ecig == "yes" ~ "yes",
                                    TRUE ~ "no"),
         beh_support = case_when(group_supp == "yes" | ind_supp == "yes" | helpline == "yes" | book == "yes" | website == "yes" ~ "yes",
                                 TRUE ~ "no"))

table_1a <- demographics_compl_ema %>%
  select(-c(nr, id, mpath_data, fitbit_data, start_date, time_first_prompt, end_date, time_last_prompt,
            nrt_no_presc, nrt_presc, zyban, varen, ecig, group_supp, ind_supp, helpline, book, website, app, none, other,
            participant_specific_variable_1, participant_specific_variable_2,
            CO_reading, CO_reading1, CO_reading2, altered_prompt, altered_prompt_start_time,
            altered_prompt_study_day, start_datetime, end_datetime, last_time_smoke_fup)) %>%
  rename("Age" = age,
         "Gender" = sex,
         "Occupation" = job_type,
         "Post-16 educational qualifications" = post_16_edu,
         "Ethnicity" = ethnicity,
         "Cigarettes per day" = cpd,
         "Time to first cigarette" = time_to_first_cig,
         "Motivation to stop" = motivation_to_stop,
         "Past-year quit attempt" = past_quit_attempt,
         "Ever use of pharmacological support (e.g., NRT, varenicline, e-cigarettes)" = pharma_support,
         "Ever use of behavioural support (e.g., counselling, quitline, website, app)" = beh_support,
         "Smoked since quit date" = smoking_fup,
         "% Completed EMAs" = prop_completed) %>%
  tbl_summary(by = "group",
              missing = "ifany",
              missing_text = "Missing",
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ c(2, 2),
                            all_categorical() ~ c(0, 1))) %>%
  add_overall() %>%
  add_p(list(all_continuous() ~ "t.test",
             all_categorical() ~ "chisq.test.no.correct"))

table_1a %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = "autofit") %>%
  flextable::save_as_docx(path = here("outputs", "table_1a.docx"), pr_section = prop_section(page_size = page_size(orient = "landscape")))

# table 1b - EMA + sensor data

hr_completeness <- read_rds(here("data", "hr_with_prop_complete.rds")) %>%
  group_by(participant_id) %>%
  mutate(over_20 = case_when(prop_bpm >= 0.2 ~ 1,
                             TRUE ~ 0)) %>%
  summarise(over_20 = sum(over_20),
            daily_prop_median = median(prop_bpm),
            included = unique(include))

step_completeness <- read_rds(here("data", "steps_with_prop_complete.rds")) %>%
  group_by(id) %>%
  summarise(daily_prop_median_s = median(prop_steps))

demographics_compl_sensor <- analytic_sample_demographics %>%
  left_join(hr_completeness, by = c("id" = "participant_id")) %>%
  left_join(step_completeness, by = c("id")) %>%
  mutate(included = case_when(is.na(included) ~ "exclude",
                              TRUE ~ included),
         over_20 = case_when(is.na(over_20) ~ 0,
                             TRUE ~ over_20),
         daily_prop_median = case_when(is.na(daily_prop_median) ~ 0,
                                       TRUE ~ daily_prop_median)) %>%
  mutate(job_type = factor(job_type, levels = c("Non-manual", "Manual", "Other (e.g., student, unemployed, retired)")),
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
         pharma_support = case_when(nrt_no_presc == "yes" | nrt_presc == "yes" | zyban == "yes" | varen == "yes" | ecig == "yes" ~ "yes",
                                    TRUE ~ "no"),
         beh_support = case_when(group_supp == "yes" | ind_supp == "yes" | helpline == "yes" | book == "yes" | website == "yes" ~ "yes",
                                 TRUE ~ "no"),
         included = str_to_sentence(included))

table_1b <- demographics_compl_sensor %>%
  select(included, age, sex, job_type, post_16_edu, ethnicity, cpd, time_to_first_cig, motivation_to_stop, past_quit_attempt,
         pharma_support, beh_support, smoking_fup, over_20, daily_prop_median, daily_prop_median_s) %>%
  rename("Age" = age,
         "Gender" = sex,
         "Occupation" = job_type,
         "Post-16 educational qualifications" = post_16_edu,
         "Ethnicity" = ethnicity,
         "Cigarettes per day" = cpd,
         "Time to first cigarette" = time_to_first_cig,
         "Motivation to stop" = motivation_to_stop,
         "Past-year quit attempt" = past_quit_attempt,
         "Ever use of pharmacological support (e.g., NRT, varenicline, e-cigarettes)" = pharma_support,
         "Ever use of behavioural support (e.g., counselling, quitline, website, app)" = beh_support,
         "Smoked since quit date" = smoking_fup,
         "N days HR =>20%" = over_20,
         "% median daily HR adherence" = daily_prop_median,
         "% median daily steps adherence" = daily_prop_median_s) %>%
  tbl_summary(by = "included",
              missing = "ifany",
              missing_text = "Missing",
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ c(2, 2),
                            all_categorical() ~ c(0, 1))) %>%
  add_overall() %>%
  add_p(list(all_continuous() ~ "t.test",
             all_categorical() ~ "chisq.test.no.correct"))

table_1b %>%
  as_flex_table() %>%
  flextable::set_table_properties(layout = "autofit") %>%
  flextable::save_as_docx(path = here("outputs", "table_1b.docx"), pr_section = prop_section(page_size = page_size(orient = "landscape")))
