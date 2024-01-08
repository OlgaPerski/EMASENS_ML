if(!file.exists(here("data", "feature_df.rds"))) {

  hr_imputed_list <- read_rds(here("data", "hr_imputed_list.rds"))

  # HR feature extraction function -------------------------------------

  feature_extration_hr <- function(df = HR_data) {

    df %>%
      group_by(EMA_id) %>%
      group_split() %>%
      lapply(function(x)
        # features to extract
        # linear model coefficient slope
        # standard deviation of observed data
        # min and max of observed
        # max rate of change within window
      { if(!any(is.na(x$bpm_imp))) {

        lm_coef <- x %>%
          mutate(record = row_number()) %>%
          lm(formula = bpm_imp ~ record)

        coefficient = lm_coef$coefficients["record"]
        standard_deviation = sd(x$bpm_imp)
        min_hr <- min(x$bpm_imp)
        max_hr <- max(x$bpm_imp)

        rate <- x %>%
          mutate(change = abs(bpm_imp - lag(bpm_imp))) %>%
          summarise(rate = max(change, na.rm = TRUE)) %>%
          pull(rate)

        features <- tibble(id = unique(x$participant_id),
                           EMA_id = unique(x$EMA_id),
                           time_window = unique(x$time_window),
                           prediction_distance = unique(x$prediction_distance),
                           coefficient = unname(coefficient),
                           standard_deviation = standard_deviation,
                           min_hr = min_hr,
                           max_hr = max_hr,
                           rate_change = rate)

        return(features)

      } else {

        coefficient = NA
        standard_deviation = 1
        min_hr <- min(x$bpm_imp)
        max_hr <- max(x$bpm_imp)

        rate <- x %>%
          mutate(change = abs(bpm_imp - lag(bpm_imp))) %>%
          summarise(rate = max(change, na.rm = TRUE)) %>%
          pull(rate)

        features <- tibble(id = unique(x$participant_id),
                           EMA_id = unique(x$EMA_id),
                           time_window = unique(x$time_window),
                           prediction_distance = unique(x$prediction_distance),
                           coefficient = unname(coefficient),
                           standard_deviation = standard_deviation,
                           min_hr = min_hr,
                           max_hr = max_hr,
                           rate_change = rate)

        return(features)

      }}) %>%
      bind_rows()
  }

  # HR feature extraction - using prediction distance 1 (15 minutes)
  # HR feature extraction - from primary_timewindow (5 minutes), secondary_timewindow (10 minutes) and tertiary_timewindow (15 minutes)

  feature_extraction_hr_pd_1 <- lapply(hr_imputed_list$prediction_distance_1, function(x) {feature_extration_hr(df = x)}) %>%
    bind_rows() %>%
    # fix to resolve NA within the features
    group_by(id) %>%
    mutate(coefficient = replace_na(coefficient, 0),
           standard_deviation = replace_na(standard_deviation, 0),
           min_hr_missing = median(min_hr, na.rm = TRUE),
           min_hr = coalesce(min_hr, min_hr_missing),
           max_hr_missing = median(max_hr, na.rm = TRUE),
           max_hr = coalesce(max_hr, max_hr_missing),
           rate_change = case_when(rate_change == -Inf | rate_change == Inf ~ NA,
                                   TRUE ~ rate_change),
           rate_change = replace_na(rate_change, 0)) %>%
    select(-min_hr_missing, -max_hr_missing)

  # HR data feature extraction - using prediction distance 2 (30 minutes)
  # HR data feature extraction - from primary_timewindow (5 minutes), secondary_timewindow (10 minutes) and tertiary_timewindow (15 minutes)

  feature_extraction_hr_pd_2 <- lapply(hr_imputed_list$prediction_distance_2, function(x) {feature_extration_hr(df = x)}) %>%
    bind_rows() %>%
    # fix to resolve NA within the features
    group_by(id) %>%
    mutate(coefficient = replace_na(coefficient, 0),
           standard_deviation = replace_na(standard_deviation, 0),
           min_hr_missing = median(min_hr, na.rm = TRUE),
           min_hr = coalesce(min_hr, min_hr_missing),
           max_hr_missing = median(max_hr, na.rm = TRUE),
           max_hr = coalesce(max_hr, max_hr_missing),
           rate_change = case_when(rate_change == -Inf | rate_change == Inf ~ NA,
                                   TRUE ~ rate_change),
           rate_change = replace_na(rate_change, 0)) %>%
    select(-min_hr_missing, -max_hr_missing)

  # HR data feature extraction - using prediction distance 3 (45 minutes)
  # HR data feature extraction - from primary_timewindow (5 minutes), secondary_timewindow (10 minutes) and tertiary_timewindow (15 minutes)

  feature_extraction_hr_pd_3 <- lapply(hr_imputed_list$prediction_distance_3, function(x) {feature_extration_hr(df = x)}) %>%
    bind_rows() %>%
    # fix to resolve NA within the features
    group_by(id) %>%
    mutate(coefficient = replace_na(coefficient, 0),
           standard_deviation = replace_na(standard_deviation, 0),
           min_hr_missing = median(min_hr, na.rm = TRUE),
           min_hr = coalesce(min_hr, min_hr_missing),
           max_hr_missing = median(max_hr, na.rm = TRUE),
           max_hr = coalesce(max_hr, max_hr_missing),
           rate_change = case_when(rate_change == -Inf | rate_change == Inf ~ NA,
                                   TRUE ~ rate_change),
           rate_change = replace_na(rate_change, 0)) %>%
    select(-min_hr_missing, -max_hr_missing)

  steps_imputed_list <- read_rds(here("data", "s_imputed_list.rds"))

  # steps data feature extraction function -------------------------------------

  feature_extration_s <- function(df = step_data) {

    df %>%
      group_by(EMA_id) %>%
      group_split() %>%
      lapply(function(x)
        # features to extract
        # linear model coefficient slope
        # standard deviation of observed data
        # min and max of observed
        # max rate of change within window
      { if(!any(is.na(x$steps_imp))) {

        lm_coef <- x %>%
          mutate(record = row_number()) %>%
          lm(formula = steps_imp ~ record)

        coefficient = lm_coef$coefficients["record"]
        standard_deviation = sd(x$steps_imp)
        min_step <- min(x$steps_imp)
        max_step <- max(x$steps_imp)

        rate <- x %>%
          mutate(change = abs(steps_imp - lag(steps_imp))) %>%
          summarise(rate = max(change, na.rm = TRUE)) %>%
          pull(rate)

        features <- tibble(id = unique(x$id),
                           EMA_id = unique(x$EMA_id),
                           time_window = unique(x$time_window),
                           prediction_distance = unique(x$prediction_distance),
                           coefficient = unname(coefficient),
                           standard_deviation = standard_deviation,
                           min_step = min_step,
                           max_step = max_step,
                           rate_change = rate)

        return(features)

      } else {

        coefficient = NA
        standard_deviation = 1
        min_step <- min(x$steps_imp)
        max_step <- max(x$steps_imp)

        rate <- x %>%
          mutate(change = abs(steps_imp - lag(steps_imp))) %>%
          summarise(rate = max(change, na.rm = TRUE)) %>%
          pull(rate)

        features <- tibble(id = unique(x$id),
                           EMA_id = unique(x$EMA_id),
                           time_window = unique(x$time_window),
                           prediction_distance = unique(x$prediction_distance),
                           coefficient = unname(coefficient),
                           standard_deviation = standard_deviation,
                           min_step = min_step,
                           max_step = max_step,
                           rate_change = rate)

        return(features)

      }}) %>%
      bind_rows()
  }

  # step data feature extraction - using prediction distance 1 (15 minutes)
  # step data feature extraction - from primary_timewindow (5 minutes), secondary_timewindow (10 minutes) and tertiary_timewindow (15 minutes)

  feature_extraction_step_pd_1 <- lapply(steps_imputed_list$prediction_distance_1, function(x) {feature_extration_s(df = x)}) %>%
    bind_rows() %>%
    # fix to resolve NA within the features
    group_by(id) %>%
    mutate(coefficient = replace_na(coefficient, 0),
           standard_deviation = replace_na(standard_deviation, 0),
           min_step_missing = median(min_step, na.rm = TRUE),
           min_step = coalesce(min_step, min_step_missing),
           max_step_missing = median(max_step, na.rm = TRUE),
           max_step = coalesce(max_step, max_step_missing),
           rate_change = case_when(rate_change == -Inf | rate_change == Inf ~ NA,
                                   TRUE ~ rate_change),
           rate_change = replace_na(rate_change, 0)) %>%
    select(-min_step_missing, -max_step_missing)

  # step data feature extraction - using prediction distance 2 (30 minutes)
  # step data feature extraction - from primary_timewindow (5 minutes), secondary_timewindow (10 minutes) and tertiary_timewindow (15 minutes)

  feature_extraction_step_pd_2 <- lapply(steps_imputed_list$prediction_distance_2, function(x) {feature_extration_s(df = x)}) %>%
    bind_rows() %>%
    # fix to resolve NA within the features
    group_by(id) %>%
    mutate(coefficient = replace_na(coefficient, 0),
           standard_deviation = replace_na(standard_deviation, 0),
           min_step_missing = median(min_step, na.rm = TRUE),
           min_step = coalesce(min_step, min_step_missing),
           max_step_missing = median(max_step, na.rm = TRUE),
           max_step = coalesce(max_step, max_step_missing),
           rate_change = case_when(rate_change == -Inf | rate_change == Inf ~ NA,
                                   TRUE ~ rate_change),
           rate_change = replace_na(rate_change, 0)) %>%
    select(-min_step_missing, -max_step_missing)

  # step data feature extraction - using prediction distance 3 (45 minutes)
  # step data feature extraction - from primary_timewindow (5 minutes), secondary_timewindow (10 minutes) and tertiary_timewindow (15 minutes)

  feature_extraction_step_pd_3 <- lapply(steps_imputed_list$prediction_distance_3, function(x) {feature_extration_s(df = x)}) %>%
    bind_rows() %>%
    # fix to resolve NA within the features
    group_by(id) %>%
    mutate(coefficient = replace_na(coefficient, 0),
           standard_deviation = replace_na(standard_deviation, 0),
           min_step_missing = median(min_step, na.rm = TRUE),
           min_step = coalesce(min_step, min_step_missing),
           max_step_missing = median(max_step, na.rm = TRUE),
           max_step = coalesce(max_step, max_step_missing),
           rate_change = case_when(rate_change == -Inf | rate_change == Inf ~ NA,
                                   TRUE ~ rate_change),
           rate_change = replace_na(rate_change, 0)) %>%
    select(-min_step_missing, -max_step_missing)

  feature_df <- left_join(bind_rows(feature_extraction_hr_pd_1,
                                    feature_extraction_hr_pd_2,
                                    feature_extraction_hr_pd_3) %>%
                            ungroup() %>%
                            mutate(prediction_distance = case_when(prediction_distance == "15 minutes" ~ "pd_1",
                                                                   prediction_distance == "30 minutes" ~ "pd_2",
                                                                   prediction_distance == "45 minutes" ~ "pd_3"),
                                   time_window = case_when(time_window == "primary_HR_match" ~ "hr_1",
                                                           time_window == "secondary_HR_match" ~ "hr_2",
                                                           time_window == "tertiary_HR_match" ~ "hr_3")) %>%
                            pivot_wider(names_from = c(time_window, prediction_distance),
                                        values_from = c(coefficient, standard_deviation, min_hr, max_hr, rate_change),
                                        names_glue = "{prediction_distance}_{time_window}_{.value}"),
                          bind_rows(feature_extraction_step_pd_1,
                                    feature_extraction_step_pd_2,
                                    feature_extraction_step_pd_3) %>%
                            ungroup() %>%
                            mutate(prediction_distance = case_when(prediction_distance == "15 minutes" ~ "pd_1",
                                                                   prediction_distance == "30 minutes" ~ "pd_2",
                                                                   prediction_distance == "45 minutes" ~ "pd_3"),
                                   time_window = case_when(time_window == "primary_step_match" ~ "step_1",
                                                           time_window == "secondary_step_match" ~ "step_2",
                                                           time_window == "tertiary_step_match" ~ "step_3")) %>%
                            pivot_wider(names_from = c(time_window, prediction_distance),
                                        values_from = c(coefficient, standard_deviation, min_step, max_step, rate_change),
                                        names_glue = "{prediction_distance}_{time_window}_{.value}"),
                          by = c("id", "EMA_id"))

  write_rds(feature_df, here("data", "feature_df.rds"))

} else {

  feature_df <- read_rds(here("data", "feature_df.rds"))

}
