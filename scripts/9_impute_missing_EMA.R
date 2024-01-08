### group by ppt id and calculate proportion completed EMAs per ppt

ema_prop_compl <- ema_responses_df %>%
  filter(is.na(button_press)) %>%
  group_by(id) %>%
  summarise(prop_completed = sum(!is.na(EMA_date_time))/160*100) %>%
  drop_na()

# plot to select cutoff % adherence for inclusion in the analytic sample
# ggplot(ema_prop_compl) +
#   aes(x = id, y = prop_completed) +
#   geom_col(colour = "black") +
#   geom_hline(yintercept = 50, colour = "red") +
#   geom_hline(yintercept = 60, colour = "orange") +
#   geom_hline(yintercept = 70, colour = "green")

# produce vector of ids to exclude going forwards
exclude_ids <- ema_prop_compl %>%
  filter(prop_completed < 60)

write_rds(ema_responses_df %>%
            filter(!id %in% exclude_ids$id), here("data", "pre_imputation_EMA_df.rds"))

# univariate kalman filter
ema_imputed_df <- ema_responses_df %>%
  filter(!id %in% exclude_ids$id) %>%
  group_by(id) %>%
  mutate(time = 1:n(),
         excited_imp = round(na_kalman(excited), 0),
         cigarette_availability_imp = round(na_kalman(cigarette_availability), 0),
         calm_imp = round(na_kalman(excited), 0),
         bored_imp = round(na_kalman(bored), 0),
         enthusiastic_imp = round(na_kalman(enthusiastic), 0),
         irritable_imp = round(na_kalman(irritable), 0),
         anxious_imp = round(na_kalman(anxious), 0),
         contented_imp = round(na_kalman(contented), 0),
         lapse_event_imp = round(na_kalman(lapse_event), 0),
         stressed_imp = round(na_kalman(stressed), 0),
         sad_imp = round(na_kalman(sad), 0),
         motivation_imp = round(na_kalman(motivation), 0),
         craving_imp = round(na_kalman(craving), 0),
         caffeine_imp = round(na_kalman(caffeine), 0),
         happy_imp = round(na_kalman(happy), 0),
         confidence_imp = round(na_kalman(confidence), 0),
         pain_imp = round(na_kalman(pain), 0),
         alcohol_imp = round(na_kalman(alcohol), 0),
         nicotine_imp = round(na_kalman(nicotine), 0),
         location_home_imp = round(na_kalman(location_home), 0),
         location_school_work_imp = round(na_kalman(location_school_work), 0),
         location_outside_imp = round(na_kalman(location_outside), 0),
         location_restaurant_imp = round(na_kalman(location_restaurant), 0),
         location_public_place_imp = round(na_kalman(location_public_place), 0),
         location_public_transport_imp = round(na_kalman(location_public_transport), 0),
         location_private_vehicle_imp = round(na_kalman(location_private_vehicle), 0),
         location_others_home_imp = round(na_kalman(location_others_home), 0),
         location_other_imp = round(na_kalman(location_other), 0),
         activity_eating_imp = round(na_kalman(activity_eating), 0),
         activity_tv_imp = round(na_kalman(activity_tv), 0),
         activity_music_imp = round(na_kalman(activity_music), 0),
         activity_reading_imp = round(na_kalman(activity_reading), 0),
         activity_working_imp = round(na_kalman(activity_working), 0),
         activity_walking_imp = round(na_kalman(activity_walking), 0),
         activity_child_care_imp = round(na_kalman(activity_child_care), 0),
         activity_socialising_imp = round(na_kalman(activity_socialising), 0),
         activity_social_media_imp = round(na_kalman(activity_social_media), 0),
         activity_relaxing_imp = round(na_kalman(activity_relaxing), 0),
         activity_chores_imp = round(na_kalman(activity_chores), 0),
         activity_other_imp = round(na_kalman(activity_other), 0),
         social_context_alone_imp = round(na_kalman(social_context_alone), 0),
         social_context_partner_imp = round(na_kalman(social_context_partner), 0),
         social_context_friend_imp = round(na_kalman(social_context_friend), 0),
         social_context_child_imp = round(na_kalman(social_context_child), 0),
         social_context_relative_imp = round(na_kalman(social_context_relative), 0),
         social_context_colleague_imp = round(na_kalman(social_context_colleague), 0),
         social_context_stranger_imp = round(na_kalman(social_context_stranger), 0),
         social_context_other_imp = round(na_kalman(social_context_other), 0)) %>%
  ungroup()

# write to rds

write_rds(ema_imputed_df, here("data", "ema_imputed_df.rds"))
