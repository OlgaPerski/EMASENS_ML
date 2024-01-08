# specify boxes

screened <- boxGrob(glue("Completed screening survey",
                           "N = 167",
                           .sep = "\n"))

eligible <- boxGrob(glue("Eligible",
                         "n = 147 (100%)",
                         .sep = "\n"))

participated <- boxGrob(glue("Participated",
                         "n = 46 (31.3%)",
                         .sep = "\n"))

met_ema_cutoff <- boxGrob(glue("Met EMA cut-off (>=60% adherence)",
                         "n = 38 (82.6%)",
                         .sep = "\n"))

met_sensor_cutoff <- boxGrob(glue("Met sensor cut-off (>=20% adherence on >=5 days)",
                           "n = 30 (78.9%)",
                           .sep = "\n"))

excluded <- boxGrob(glue("Excluded (n = {tot}):",
                         " - history of arrythmias: {arrythmia}",
                         " - taking beta blockers: {beta_blocker}",
                         " - not able to provide eCO: {eCO}",
                         " - not cigarette smoker: {not_smoker}",
                         " - not willing to set quit date: {quit_date}",
                         " - not willing to wear Fitbit: {fitbit}",
                         " - not living in London: {london}",
                         tot = 20,
                         arrythmia = 9,
                         beta_blocker = 5,
                         eCO = 2,
                         not_smoker = 1,
                         quit_date = 1,
                         fitbit = 1,
                         london = 1,
                         .sep = "\n"),
                    just = "left")

withdrew <- boxGrob(glue("Did not participate (n = {tot}):",
                         " - did not respond to e-mail invitation: {invitation}",
                         " - did not attend scheduled meeting: {attend}",
                         " - no longer interested in participating: {interest}",
                         " - withdrew from the study: {withdrew}",
                         tot = 101,
                         invitation = 62,
                         attend = 26,
                         interest = 9,
                         withdrew = 4,
                         .sep = "\n"),
                    just = "left")

png(filename = here("outputs", "flowchart.png"), width = 680, height = 700)

grid.newpage()

spread_boxes <- spreadVertical(screened = screened,
                               eligible = eligible,
                               participated = participated,
                               met_ema_cutoff,
                               met_sensor_cutoff,
                               .type = "center")

excluded <- moveBox(excluded,
                    x = .8,
                    y = .8)

withdrew <- moveBox(withdrew,
                    x = .8,
                    y = .6)

# print boxes

spread_boxes
excluded
withdrew

for (i in 1:(length(spread_boxes) - 1)) {
  connectGrob(spread_boxes[[i]], spread_boxes[[i + 1]], type = "vert", arrow_obj = arrow(angle = 20, length = unit(0.15, "inches"), type = "closed")) %>%
    print
}

connectGrob(spread_boxes$screened, excluded, type = "-", arrow_obj = arrow(angle = 20, length = unit(0.15, "inches"), type = "closed"))
connectGrob(spread_boxes$eligible, withdrew, type = "-", arrow_obj = arrow(angle = 20, length = unit(0.15, "inches"), type = "closed"))

dev.off()
