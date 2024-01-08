# read in the demographics

demographics <- read_excel(here("data", "participant_dates_and_demographics_for_analysis.xlsx")) %>%
  mutate(id = paste0("p", id),
         # time zones shifted; need to define those that are UTC and BST
         start_datetime = with_tz(as_datetime(paste(start_date, format(time_first_prompt, format = "%H:%M:%S")), tz = "Europe/London"), tzone = "UTC"),
         end_datetime = with_tz(as_datetime(paste(end_date, format(time_last_prompt, format = "%H:%M:%S")), tz = "Europe/London"), tzone = "UTC"),
         smoking_fup = case_when(last_time_smoke_fup == "1-3 weeks ago" ~ "Yes",
                                         last_time_smoke_fup == "1-6 days ago" ~ "Yes",
                                         last_time_smoke_fup == "In the last 24hrs" ~ "Yes",
                                         last_time_smoke_fup == "Have not smoked since my planned quit date" ~ "No",
                                         TRUE ~ "Yes"))

write_rds(demographics, here("data", "demographics.rds"))

# define the start and end date for each participant

ppt_start_date <- tibble(participant_id = demographics$id,
                         start_date = ymd(demographics$start_date),
                         end_date = ymd(demographics$end_date)) %>%
  group_by(participant_id) %>%
  nest(data = c(start_date, end_date)) %>%
  mutate(date = map(data, ~ seq(unique(.x$start_date), unique(.x$end_date), 1))) %>%
  unnest(date) %>%
  select(-data) %>%
  mutate(day_n = row_number())
