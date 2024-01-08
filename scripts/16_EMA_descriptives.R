analytic_sample_1 <- read_rds(here("data", "analytic_sample_1.rds"))

# lapses ------------------------------------------------------------------

total_prop_lapses <- analytic_sample_1 %>%
  summarise(total_count = n(),
            lapses_count = (sum(lapse_event_imp == 1)),
            prop = round(sum(lapse_event_imp == 1) / total_count * 100, digits = 1))

table_prop_lapses <- analytic_sample_1 %>%
  group_by(id) %>%
  summarise(total_count = n(),
            lapses_count = (sum(lapse_event_imp == 1)),
            prop = round(sum(lapse_event_imp == 1) / total_count * 100, digits = 1))

median(table_prop_lapses$prop)
min(table_prop_lapses$prop)
max(table_prop_lapses$prop)
sum(table_prop_lapses$prop == 0)
sum(table_prop_lapses$prop == 1)

group_split <- analytic_sample_1 %>%
  group_by(id) %>%
  group_split()

plot_lapses_function <- function(df = group_split) {
  
  df  %>%
    ggplot() +
    geom_line(aes(x = EMA_number, y = lapse_event_imp)) +
    facet_wrap(~ id, scales = "free_x") +
    scale_y_continuous(breaks = c(0, 1), limits = c(0,1)) +
    theme_minimal() +
    theme(panel.border = if(unique(df$smoking_fup) == "Yes") element_rect(linetype = "dashed", colour = "red", fill = NA) else element_rect(linetype = "solid", colour = "black", fill = NA)) +
    labs(x = "EMA",
         y = "Lapses")
  
}

lapses_plot <- lapply(group_split, plot_lapses_function) %>%
  plot_grid(plotlist = .)

cowplot::save_plot(plot = lapses_plot, filename = here("outputs", "lapses_plot.png"), base_height = 8, base_width = 8)

# cravings and motivation -------------------------------------------------

plot_cravings_motivation_function <- function(df = group_split) {
  
  df  %>%
    ggplot() +
    geom_line(aes(x = EMA_number, y = craving_imp), colour = "blue") +
    geom_line(aes(x = EMA_number, y = motivation_imp), colour = "green") +
    scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = "EMA",
         y = "Rating") +
    theme(panel.border = if(unique(df$smoking_fup) == "Yes") element_rect(linetype = "dashed", colour = "red", fill = NA) else element_rect(linetype = "solid", colour = "black", fill = NA))
  
}

cravings_motivation_plot <- lapply(group_split[1:4], plot_cravings_motivation_function) %>% # for presentation purposes only with the group split
  plot_grid(plotlist = .)

cowplot::save_plot(plot = cravings_motivation_plot, filename = here("outputs", "cravings_motivation_plot.png"), base_height = 8, base_width = 8)

# stress, boredom, sadness and pain ---------------------------------------

plot_negative_affect_function <- function(df = group_split) {
  
  df  %>%
    ggplot() +
    geom_line(aes(x = EMA_number, y = stressed_imp, colour = "darkorange")) +
    geom_line(aes(x = EMA_number, y = bored_imp, colour = "blue")) +
    geom_line(aes(x = EMA_number, y = sad_imp, colour = "darkgreen")) +
    geom_line(aes(x = EMA_number, y = pain_imp, colour = "purple")) +
    scale_y_continuous(breaks = c(0:10), limits = c(0,10)) +
    facet_wrap(~ id, scales = "free_x") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("darkorange", "blue", "darkgreen", "purple")) +
    labs(x = "EMA",
         y = "Rating") +
    theme(panel.border = if(unique(df$smoking_fup) == "Yes") element_rect(linetype = "dashed", colour = "red", fill = NA) else element_rect(linetype = "solid", colour = "black", fill = NA))
  
}

negative_affect_plot <- lapply(group_split, plot_negative_affect_function) %>%
  plot_grid(plotlist = .)

cowplot::save_plot(plot = negative_affect_plot, filename = here("outputs", "negative_affect_plot.png"), base_height = 8, base_width = 8)

# calm, happiness and excitement ------------------------------------------

plot_positive_affect_function <- function(df = group_split) {
  
  df  %>%
    ggplot() +
    geom_line(aes(x = EMA_number, y = calm_imp, colour = "darkorange")) +
    geom_line(aes(x = EMA_number, y = happy_imp, colour = "blue")) +
    geom_line(aes(x = EMA_number, y = excited_imp, colour = "darkgreen")) +
    scale_y_continuous(breaks = c(0:10), limits = c(0,10)) +
    facet_wrap(~ id, scales = "free_x") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("darkorange", "blue", "darkgreen", "purple")) +
    labs(x = "EMA",
         y = "Rating") +
    theme(panel.border = if(unique(df$smoking_fup) == "Yes") element_rect(linetype = "dashed", colour = "red", fill = NA) else element_rect(linetype = "solid", colour = "black", fill = NA))
  
}

positive_affect_plot <- lapply(group_split, plot_positive_affect_function) %>%
  plot_grid(plotlist = .)

cowplot::save_plot(plot = positive_affect_plot, filename = here("outputs", "positive_affect_plot.png"), base_height = 8, base_width = 8)

# cigarette availability --------------------------------------------------

plot_availability_function <- function(df = group_split) {
  
  df  %>%
    ggplot() +
    geom_line(aes(x = EMA_number, y = cigarette_availability_imp)) +
    facet_wrap(~ id, scales = "free_x") +
    scale_y_continuous(breaks = c(1, 2, 3), limits = c(1, 3)) +
    theme_minimal() +
    theme(panel.border = if(unique(df$smoking_fup) == "Yes") element_rect(linetype = "dashed", colour = "red", fill = NA) else element_rect(linetype = "solid", colour = "black", fill = NA)) +
    labs(x = "EMA",
         y = "Cigarette availability")
  
}

availability_plot <- lapply(group_split, plot_availability_function) %>%
  plot_grid(plotlist = .)

cowplot::save_plot(plot = availability_plot, filename = here("outputs", "availability_plot.png"), base_height = 8, base_width = 8)

# nicotine use ------------------------------------------------------------

plot_nicotine_function <- function(df = group_split) {
  
  df  %>%
    ggplot() +
    geom_line(aes(x = EMA_number, y = nicotine_imp)) +
    facet_wrap(~ id, scales = "free_x") +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    theme_minimal() +
    theme(panel.border = if(unique(df$smoking_fup) == "Yes") element_rect(linetype = "dashed", colour = "red", fill = NA) else element_rect(linetype = "solid", colour = "black", fill = NA)) +
    labs(x = "EMA",
         y = "Nicotine use")
  
}

nicotine_plot <- lapply(group_split, plot_nicotine_function) %>%
  plot_grid(plotlist = .)

cowplot::save_plot(plot = nicotine_plot, filename = here("outputs", "nicotine_plot.png"), base_height = 8, base_width = 8)

# physical context --------------------------------------------------------

plot_names_1 <- c('prop_location_home' = "Home",
                  'prop_location_school_work' = "School/work",
                  'prop_location_outside' = "Outside",
                  'prop_location_restaurant' = "Restaurant/bar",
                  'prop_location_public_place' = "Public place",
                  'prop_location_public_transport' = "Public transport",
                  'prop_location_private_vehicle' = "Private vehicle",
                  'prop_location_others_home' = "Others' home",
                  'prop_location_other' = "Other")

prop_physical_context <- analytic_sample_1 %>%
  group_by(id) %>%
  summarise(nr_entries = n(),
            prop_location_home = sum(location_home_imp, na.rm = T)/nr_entries*100,
            prop_location_school_work = sum(location_school_work_imp, na.rm = T)/nr_entries*100,
            prop_location_outside = sum(location_outside_imp, na.rm = T)/nr_entries*100,
            prop_location_restaurant = sum(location_restaurant_imp, na.rm = T)/nr_entries*100,
            prop_location_public_place = sum(location_public_place_imp, na.rm = T)/nr_entries*100,
            prop_location_public_transport = sum(location_public_transport_imp, na.rm = T)/nr_entries*100,
            prop_location_private_vehicle = sum(location_private_vehicle_imp, na.rm = T)/nr_entries*100,
            prop_location_others_home = sum(location_others_home_imp, na.rm = T)/nr_entries*100,
            prop_location_other = sum(location_other_imp, na.rm = T)/nr_entries*100) %>%
  pivot_longer(cols = starts_with("prop_"), names_to = "location", values_to = "prop")

physical_context_plot <- ggplot(prop_physical_context) +
  geom_col(aes(x = id, y = prop, fill = location)) +
  facet_wrap(~ location, labeller = as_labeller(plot_names_1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Participant",
       y = "Proportion of EMAs")

cowplot::save_plot(plot = physical_context_plot, filename = here("outputs", "physical_context_plot.png"), base_height = 8, base_width = 8)

# activities --------------------------------------------------------------

plot_names_2 <- c('prop_activity_eating' = "Eating",
                  'prop_activity_tv' = "Watching TV",
                  'prop_activity_music' = "Listening to music",
                  'prop_activity_reading' = "Reading",
                  'prop_activity_working' = "Working",
                  'prop_activity_walking' = "Walking",
                  'prop_activity_child_care' = "Child care",
                  'prop_activity_socialising' = "Socialising",
                  'prop_activity_social_media' = "Social media",
                  'prop_activity_relaxing' = "Relaxing",
                  'prop_activity_chores' = "Chores",
                  'prop_activity_other' = "Other")

prop_activities <- a %>%
  group_by(id) %>%
  summarise(nr_entries = n(),
            prop_activity_eating = sum(activity_eating_imp, na.rm = T)/nr_entries*100,
            prop_activity_tv = sum(activity_tv_imp, na.rm = T)/nr_entries*100,
            prop_activity_music = sum(activity_music_imp, na.rm = T)/nr_entries*100,
            prop_activity_reading = sum(activity_reading_imp, na.rm = T)/nr_entries*100,
            prop_activity_working = sum(activity_working_imp, na.rm = T)/nr_entries*100,
            prop_activity_walking = sum(activity_walking_imp, na.rm = T)/nr_entries*100,
            prop_activity_child_care = sum(activity_child_care_imp, na.rm = T)/nr_entries*100,
            prop_activity_socialising = sum(activity_socialising_imp, na.rm = T)/nr_entries*100,
            prop_activity_social_media = sum(activity_social_media_imp, na.rm = T)/nr_entries*100,
            prop_activity_relaxing = sum(activity_relaxing_imp, na.rm = T)/nr_entries*100,
            prop_activity_chores = sum(activity_chores_imp, na.rm = T)/nr_entries*100,
            prop_activity_other = sum(activity_other_imp, na.rm = T)/nr_entries*100) %>%
  pivot_longer(cols = starts_with("prop_"), names_to = "activities", values_to = "prop")

activities_plot <- ggplot(prop_activities) +
  geom_col(aes(x = id, y = prop, fill = activities)) +
  facet_wrap(~ activities, labeller = as_labeller(plot_names_2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Participant",
       y = "Proportion of EMAs")

cowplot::save_plot(plot = activities_plot, filename = here("outputs", "activities_plot.png"), base_height = 8, base_width = 8)

# social context ----------------------------------------------------------

plot_names_3 <- c('prop_social_context_alone' = "Alone",
                  'prop_social_context_partner' = "With partner",
                  'prop_social_context_friend' = "With friend",
                  'prop_social_context_child' = "With child",
                  'prop_social_context_relative' = "With relative",
                  'prop_social_context_colleague' = "With colleague",
                  'prop_social_context_stranger' = "With stranger",
                  'prop_social_context_other' = "Other")

prop_social_context <- a %>%
  group_by(id) %>%
  summarise(nr_entries = n(),
            prop_social_context_alone = sum(social_context_alone_imp, na.rm = T)/nr_entries*100,
            prop_social_context_partner = sum(social_context_partner_imp, na.rm = T)/nr_entries*100,
            prop_social_context_friend = sum(social_context_friend_imp, na.rm = T)/nr_entries*100,
            prop_social_context_child = sum(social_context_child_imp, na.rm = T)/nr_entries*100,
            prop_social_context_relative = sum(social_context_relative_imp, na.rm = T)/nr_entries*100,
            prop_social_context_colleague = sum(social_context_colleague_imp, na.rm = T)/nr_entries*100,
            prop_social_context_stranger = sum(social_context_stranger_imp, na.rm = T)/nr_entries*100,
            prop_social_context_other = sum(social_context_other_imp, na.rm = T)/nr_entries*100) %>%
  pivot_longer(cols = starts_with("prop_"), names_to = "social_context", values_to = "prop")

social_context_plot <- ggplot(prop_social_context) +
  geom_col(aes(x = id, y = prop, fill = social_context)) +
  facet_wrap(~ social_context, labeller = as_labeller(plot_names_3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Participant",
       y = "Proportion of EMAs")

cowplot::save_plot(plot = social_context_plot, filename = here("outputs", "social_context_plot.png"), base_height = 8, base_width = 8)
