# identify EMA data file for each participant

ema_files <- list.files(path = here("data"), pattern = "mpath", recursive = TRUE)

# rename the file names to allow reading in

ema_file_df <- tibble(participant_id = str_split(ema_files, "/", simplify = TRUE)[,1],
                      filename = ema_files)

# create list of .csv files

ema_csv_list <- as.list(ema_file_df$filename)

# read in .csv files

ema_csv_data <- lapply(ema_csv_list, function(x) { if(str_detect(read.csv(file = here("data", x))[1, 1], "Date")) {
  read.csv(file = here("data", x), header = TRUE, skip = 1, na = c("", " ")) %>%
    as.data.frame()} else {
      read.csv(file = here("data", x)) %>%
        as.data.frame()
    }})

# extract col names and rename

recode_col_names_tbl <- tibble(old_name = c("Date.and.time", "mood..smiley.", "mood..enthusiastic..sliderNegPos.",
                                            "mood..excited..sliderNegPos.", "locations..multipleChoice.", "mood..irritable..sliderNegPos.",
                                            "activities..multipleChoice.", "mood..anxious..sliderNegPos.", "mood..contented..sliderNegPos.",
                                            "mood..tired..open.", "smoking.lapses..multipleChoice.", "cigarette.availability..multipleChoice.",
                                            "mood..stressed..sliderNegPos.", "mood..sad..sliderNegPos.", "motivation..sliderNegPos.",
                                            "cravings..sliderNegPos.", "mood..calm..sliderNegPos.", "company..multipleChoice.",
                                            "caffeine.consumption..multipleChoice.", "mood..happy..sliderNegPos.", "confidence..sliderNegPos.",
                                            "pain.intensity..sliderNegPos.", "mood..bored..sliderNegPos.", "alcohol.consumption..multipleChoice.",
                                            "nicotine.product.use..multipleChoice.", "button..count.", "button..3.hour.average.",
                                            "button..24.hour.average.", "labelExample..basic.", "pain..sliderNegPos.",
                                            "contented..sliderNegPos.", "calm..sliderNegPos.", "enthusiastic..sliderNegPos.",
                                            "irratable..sliderNegPos.", "lapses..multipleChoice.", "excited..sliderNegPos.",
                                            "bored..sliderNegPos.", "cravings..sliderNegPos.", "around.smokers..multipleChoice.",
                                            "happy..sliderNegPos.", "Activities..multipleChoice.", "anxious..sliderNegPos.",
                                            "social.context..multipleChoice.", "cigarette.availability...multipleChoice.", "self.efficacy..sliderNegPos.",
                                            "stressed..sliderNegPos.", "sad..sliderNegPos.", "nicotine.product..multipleChoice.",
                                            "alcohol..multipleChoice.", "caffeine..multipleChoice.", "irritable..sliderNegPos.",
                                            "smoking.friends..yesno.", "location..multipleChoice.", "exctited..sliderNegPos.",
                                            "smoking.friends..multipleChoice.", "nicotine.products..multipleChoice.", "trigger..open.",
                                            "self.efficacy...sliderNegPos.", "cigarette.availabilty..multipleChoice.", "self.efficacy...sliderNegPos.",
                                            "smoking.friends...yesno.", "craving..sliderNegPos.", "caffeine..yesno.",
                                            "alcohol..yesno.", "nicotine.product..yesno.", "caffeine.consumption..yesno.",
                                            "alcohol.consumption..yesno.", "nicotine.product.use..yesno.", "cigarette.availablity..multipleChoice.",
                                            "lapses..yesno.", "Around.smokers..multipleChoice.", "with.participant.62..multipleChoice.",
                                            "routine.trigger..multipleChoice.", "work.stressors..multipleChoice.", "with.participant.60..multipleChoice.",
                                            "caffeine...yesno.", "lapses..yesno.", "hungry..sliderNegPos.",
                                            "Location..multipleChoice.", "Social.context..multipleChoice.", "alcohol.consupmtion..multipleChoice.",
                                            "actvities..multipleChoice.", "task.start..multipleChoice.", "Locations..multipleChoice.", "label_ldNW..multipleChoice.",
                                            "free_text", "label_EWpx..multipleChoice.", "frustrating.event..multipleChoice."),
                               new_name = c("EMA_date_time", "smiley", "enthusiastic",
                                            "excited", "locations", "irritable",
                                            "activities", "anxious", "contented",
                                            "tired", "lapse_event", "cigarette_availability",
                                            "stressed", "sad", "motivation",
                                            "craving", "calm", "social_context",
                                            "caffeine", "happy", "confidence",
                                            "pain", "bored", "alcohol",
                                            "nicotine", "button_press", "button_3h_average",
                                            "button_24h_average", "label_basic", "pain",
                                            "contented", "calm", "enthusiastic",
                                            "irritable", "lapse_event", "excited",
                                            "bored", "craving", "around_smokers",
                                            "happy", "activities", "anxious",
                                            "social_context", "cigarette_availability", "confidence",
                                            "stressed", "sad", "nicotine",
                                            "alcohol", "caffeine", "irritable",
                                            "smoking_friends", "locations", "excited",
                                            "smoking_friends", "nicotine", "free_text",
                                            "confidence", "cigarette_availability", "confidence",
                                            "smoking_friends", "craving", "caffeine",
                                            "alcohol", "nicotine", "caffeine",
                                            "alcohol", "nicotine", "cigarette_availability",
                                            "lapse_event", "around_smokers", "with_p62",
                                            "routine_trigger", "work_stressors", "with_p60",
                                            "caffeine", "lapse_event", "hungry",
                                            "locations", "social_context", "alcohol",
                                            "activities", "task_start", "locations", "p17_what_triggered", "p17_free_text",
                                            "p6_around_smokers", "p61_frustrating_event"))

recode_col_names <- recode_col_names_tbl$new_name

names(recode_col_names) <- recode_col_names_tbl$old_name

col_name_fix <- lapply(ema_csv_data, function(x) {

  colnames(x) <- recode(colnames(x), !!!recode_col_names)

  return(x)

})

# this has returned a list of EMA data frames for each participant with cleaned variable names, now recode variable levels

names(col_name_fix) <- c(ema_file_df$participant_id)

ema_df <- col_name_fix %>%
  bind_rows(.id = "id") %>%
  mutate(id = fct_inorder(id),
         EMA_date_time = parse_date_time(EMA_date_time, orders = "%a, %d %b %y %H:%M:%S", tz = "Europe/London"),
         # Convert to UTC
         EMA_date_time = with_tz(EMA_date_time, tzone = "UTC")) %>%
  mutate(cigarette_availability = case_when(str_detect(cigarette_availability, "Easi") ~ 3,
                                            cigarette_availability == "Not available" ~ 1,
                                            cigarette_availability == "Available with difficulty" ~ 2,
                                            TRUE ~ NA_real_),
         caffeine = case_when(caffeine == "no" | caffeine == "No" ~ 0,
                              caffeine == "yes" | caffeine == "Yes" ~ 1,
                              TRUE ~ NA_real_),
         alcohol = case_when(alcohol == "no" | alcohol == "No" ~ 0,
                             alcohol == "yes" | alcohol == "Yes" ~ 1,
                             TRUE ~ NA_real_),
         nicotine = case_when(nicotine == "no" | nicotine == "No" ~ 0,
                              nicotine == "yes" | nicotine == "Yes" ~ 1,
                              TRUE ~ NA_real_),
         location_home = case_when(str_detect(locations, "At home|A home") ~ 1,
                                   is.na(locations) ~ NA_real_,
                                   TRUE ~ 0),
         location_school_work = case_when(str_detect(locations, "At school/work") ~ 1,
                                          is.na(locations) ~ NA_real_,
                                          TRUE ~ 0),
         location_outside = case_when(str_detect(locations, "Outside") ~ 1,
                                      is.na(locations) ~ NA_real_,
                                      TRUE ~ 0),
         location_restaurant = case_when(str_detect(locations, "In a restaurant/café/bar|In a restaurant/cafÃ©/bar") ~ 1,
                                         is.na(locations) ~ NA_real_,
                                         TRUE ~ 0),
         location_restaurant = case_when(str_detect(locations, "In a restaurant/café/bar|In a restaurant/cafÃ©/bar") ~ 1,
                                         is.na(locations) ~ NA_real_,
                                         TRUE ~ 0),
         location_public_place = case_when(str_detect(locations, "In a public place") ~ 1,
                                           is.na(locations) ~ NA_real_,
                                           TRUE ~ 0),
         location_public_transport = case_when(str_detect(locations, "On public transport") ~ 1,
                                               is.na(locations) ~ NA_real_,
                                               TRUE ~ 0),
         location_private_vehicle = case_when(str_detect(locations, "In a private vehicle") ~ 1,
                                              is.na(locations) ~ NA_real_,
                                              TRUE ~ 0),
         location_others_home = case_when(str_detect(locations, "In others' home|In othersâ€™ home") ~ 1,
                                          is.na(locations) ~ NA_real_,
                                          TRUE ~ 0),
         location_other = case_when(str_detect(locations, "Other") ~ 1,
                                    is.na(locations) ~ NA_real_,
                                    TRUE ~ 0),
         activity_eating = case_when(str_detect(activities, "Eating/drinking") ~ 1,
                                     is.na(locations) ~ NA_real_,
                                     TRUE ~ 0),
         activity_tv = case_when(str_detect(activities, "Watching TV") ~ 1,
                                 is.na(locations) ~ NA_real_,
                                 TRUE ~ 0),
         activity_music = case_when(str_detect(activities, "Listening to music") ~ 1,
                                    is.na(locations) ~ NA_real_,
                                    TRUE ~ 0),
         activity_reading = case_when(str_detect(activities, "Reading") ~ 1,
                                      is.na(locations) ~ NA_real_,
                                      TRUE ~ 0),
         activity_working = case_when(str_detect(activities, "Working/studying") ~ 1,
                                      is.na(locations) ~ NA_real_,
                                      TRUE ~ 0),
         activity_walking = case_when(str_detect(activities, "Walking/exercising") ~ 1,
                                      is.na(locations) ~ NA_real_,
                                      TRUE ~ 0),
         activity_child_care = case_when(str_detect(activities, "Caring for child(ren)") ~ 1,
                                         is.na(locations) ~ NA_real_,
                                         TRUE ~ 0),
         activity_socialising = case_when(str_detect(activities, "Socialising") ~ 1,
                                          is.na(locations) ~ NA_real_,
                                          TRUE ~ 0),
         activity_social_media = case_when(str_detect(activities, "Scrolling on social media") ~ 1,
                                           is.na(locations) ~ NA_real_,
                                           TRUE ~ 0),
         activity_relaxing = case_when(str_detect(activities, "Relaxing") ~ 1,
                                       is.na(locations) ~ NA_real_,
                                       TRUE ~ 0),
         activity_chores = case_when(str_detect(activities, "Doing chores") ~ 1,
                                     is.na(locations) ~ NA_real_,
                                     TRUE ~ 0),
         activity_other = case_when(str_detect(activities, "Other") ~ 1,
                                    is.na(locations) ~ NA_real_,
                                    TRUE ~ 0),
         social_context_alone = case_when(str_detect(social_context, "Alone") ~ 1,
                                          is.na(locations) ~ NA_real_,
                                          TRUE ~ 0),
         social_context_partner = case_when(str_detect(social_context, "With partner/spouse") ~ 1,
                                            is.na(locations) ~ NA_real_,
                                            TRUE ~ 0),
         social_context_friend = case_when(str_detect(social_context, "With friend(s)") ~ 1,
                                           is.na(locations) ~ NA_real_,
                                           TRUE ~ 0),
         social_context_child = case_when(str_detect(social_context, "With child(ren)") ~ 1,
                                          is.na(locations) ~ NA_real_,
                                          TRUE ~ 0),
         social_context_relative = case_when(str_detect(social_context, "With relative(s)") ~ 1,
                                             is.na(locations) ~ NA_real_,
                                             TRUE ~ 0),
         social_context_colleague = case_when(str_detect(social_context, "With colleague(s)") ~ 1,
                                              is.na(locations) ~ NA_real_,
                                              TRUE ~ 0),
         social_context_stranger = case_when(str_detect(social_context, "With stranger(s)") ~ 1,
                                             is.na(locations) ~ NA_real_,
                                             TRUE ~ 0),
         social_context_other = case_when(str_detect(social_context, "Other") ~ 1,
                                          is.na(locations) ~ NA_real_,
                                          TRUE ~ 0),
         lapse_event = case_when(lapse_event == "yes" | button_press == "button pressed" ~ 1,
                                 is.na(lapse_event) ~ NA_real_,
                                 TRUE ~ 0)) %>%
  select(-any_of(c("smiley", "button_3h_average", "button_24h_average", "label_basic",
                   "locations", "activities", "social_context")))

# write to rds

write_rds(ema_df, here("data", "ema_df.rds"))
