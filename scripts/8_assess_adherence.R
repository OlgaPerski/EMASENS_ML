ema_adherence <- ema_responses_df %>%
  filter(is.na(button_press)) %>%
  drop_na(EMA_date_time) %>%
  group_by(id, study_day) %>%
  summarise(n_responses = round(n()/16 * 100, 1)) %>%
  rename(participant_id = id)

hr_adherence <- hr_df %>%
  drop_na(bpm) %>%
  group_by(participant_id, study_day) %>%
  summarise(n_hr = round(n()/12241 * 100, 1)) # ppts should have 15 hours of sensor data during the EMA period, plus 1 hour either side, for a total of 17 hours

steps_adherence <- steps_df %>%
  drop_na(steps) %>%
  group_by(participant_id, study_day) %>%
  summarise(n_step_records = round(n()/1021 * 100, 1))

left_join(ema_adherence,
          hr_adherence) %>%
  left_join(steps_adherence) %>%
  pivot_longer(cols = c(n_responses, n_hr, n_step_records), names_to = "name", values_to = "adherence") %>%
  mutate(name = case_when(str_detect(name, "response") ~ "EMAs",
                          str_detect(name, "hr") ~ "HR",
                          str_detect(name, "step") ~ "Steps")) %>%
  ggplot() +
  geom_col(aes(x = study_day, y = adherence, fill = name), position = position_dodge(preserve = "single")) +
  facet_wrap(~ participant_id) +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() +
  labs(fill = "Data source",
       y = "Adherence",
       x = element_blank())