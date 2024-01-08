hr_imputed <- read_rds(here("data", "hr_imputed_list.rds"))
s_imputed <- read_rds(here("data", "s_imputed_list.rds"))
analytic_sample_2 <- read_rds(here("data", "analytic_sample_2.rds"))

# heart rate --------------------------------------------------------------

HR <- hr_imputed[[1]]$primary_HR_match %>%
  rename("id" = "participant_id")

HR_demographics <- left_join(HR,
                             analytic_sample_2 %>%
                               select("id", "smoking_fup"),
                             by = "id", relationship = "many-to-many")
  

group_split_HR <- HR_demographics %>%
  group_by(id) %>%
  group_split()

plot_HR_function <- function(df = group_split_HR) {
  
  df  %>%
    ggplot() +
    geom_line(aes(x = EMA_id, y = bpm_imp)) +
    facet_wrap(~ id, scales = "free_x") +
    scale_y_continuous(limits = c(0, 180)) +
    theme_minimal() +
    theme(panel.border = if(unique(df$smoking_fup) == "Yes") element_rect(linetype = "dashed", colour = "red", fill = NA) else element_rect(linetype = "solid", colour = "black", fill = NA)) +
    labs(x = "EMA",
         y = "Heart rate")
  
}

HR_plot <- lapply(group_split_HR, plot_HR_function) %>%
  plot_grid(plotlist = .)

cowplot::save_plot(plot = HR_plot, filename = here("outputs", "HR_plot.png"), base_height = 8, base_width = 8)

# steps -------------------------------------------------------------------

steps <- s_imputed[[1]]$primary_step_match

steps_demographics <- left_join(steps,
                                analytic_sample_2 %>%
                                  select("id", "smoking_fup"),
                                by = "id", relationship = "many-to-many")


group_split_steps <- steps_demographics %>%
  group_by(id) %>%
  group_split()

plot_steps_function <- function(df = group_split_steps) {
  
  df  %>%
    ggplot() +
    geom_line(aes(x = EMA_id, y = steps_imp)) +
    facet_wrap(~ id, scales = "free_x") +
    scale_y_continuous() +
    theme_minimal() +
    theme(panel.border = if(unique(df$smoking_fup) == "Yes") element_rect(linetype = "dashed", colour = "red", fill = NA) else element_rect(linetype = "solid", colour = "black", fill = NA)) +
    labs(x = "EMA",
         y = "Steps")
  
}

steps_plot <- lapply(group_split_steps, plot_steps_function) %>%
  plot_grid(plotlist = .)

cowplot::save_plot(plot = steps_plot, filename = here("outputs", "steps_plot.png"), base_height = 8, base_width = 8)
