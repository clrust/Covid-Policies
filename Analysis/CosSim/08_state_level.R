# Created by: CR
# Date: 7/24/26
# State level plots comparing GU, GH and HU and lagged agency-days

library(tidyverse)
library(patchwork)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

cossim_data <- read_csv("Testing/Results/07_cossim_data_filtered.csv")

state_uni_dates <- read_csv(
  path.expand("~/covidpolicies/Analysis/state_uni_dates.csv"),
  col_types = cols(.default = col_character())
) %>%
  rename(
    closure = `State University System Closure Announced`,
    reopening = `State University System Reopen`,
    vaccine_eligibility = full_vax_eligibility
  ) %>%
  pivot_longer(
    cols = c(closure, reopening, vaccine_eligibility),
    names_to = "event",
    values_to = "date"
  ) %>%
  mutate(
    date = mdy(date),
    event = recode(
      event,
      closure = "University closure announced",
      reopening = "University system reopened",
      vaccine_eligibility = "Full vaccine eligibility"
    )
  ) %>%
  filter(!is.na(date))

make_base_plot <- function(state) {
  state_df <- cossim_data %>%
    filter(.data$State == .env$state)
  
  ggplot(state_df) +
    geom_line(aes(x = Date, y = GU, color = "Governor → University")) +
    geom_line(aes(x = Date, y = GH, color = "Governor → Health")) +
    geom_line(aes(x = Date, y = HU, color = "Health → University")) +
    geom_vline(
      xintercept = ymd("2020-11-03"),
      linetype = "dashed",
      color = "black",
      linewidth = 0.75
    ) +
    annotate(
      "text",
      x = ymd("2020-11-03"),
      y = Inf,
      label = "Election Day",
      hjust = -0.1,   # puts label slightly right of line
      vjust = 1.5,    # moves label down from top
      color = "black",
      size = 2.5
    ) + 
    geom_vline(
      xintercept = ymd("2020-12-14"),
      linetype = "dashed",
      color = "black",
      linewidth = 0.75
    ) +
    annotate(
      "text",
      x = ymd("2020-12-14"),
      y = Inf,
      label = "First Vax Given",
      hjust = -0.1,   # puts label slightly right of line
      vjust = 3,    # moves label down from top
      color = "black",
      size = 2.5
    ) +
    geom_vline(
      xintercept = ymd("2021-12-01"),
      linetype = "dashed",
      color = "black",
      linewidth = 0.75
    ) +
    annotate(
      "text",
      x = ymd("2021-12-01"),
      y = Inf,
      label = "First US Omicron Case",
      hjust = -0.1,   # puts label slightly right of line
      vjust = 1.5,    # moves label down from top
      color = "black",
      size = 2.5
    ) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Governor → University" = "red",
        "Governor → Health" = "blue",
        "Health → University" = "green"
      )
    ) +
    labs(y = "Similarity Score", title = state) +
    scale_y_continuous(limits = c(0,1))
}

plot_directory <- "Testing/Results/FilteredDataPlots/State_Plots"

dir.create(
  plot_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

state_abvs <- unique(cossim_data$State)

walk(state_abvs, function(state) {
  state_plot <- make_base_plot(state)
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0("NoLag/cossim", state, ".png")
    ),
    plot = state_plot,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300
  )
})


#-------lag plots
make_lag_plot <- function(state, events = state_uni_dates) {
  state_df <- cossim_data %>%
    filter(.data$State == .env$state)

  state_events <- events %>%
    filter(.data$State == .env$state)
  
  ggplot(state_df) +
    geom_line(aes(x = Date, y = Glag1_U, color = "Lag Governor → University")) +
    geom_line(aes(x = Date, y = Glag1_H, color = "Lag Governor → Health")) +
    geom_line(aes(x = Date, y = H_Ulag1, color = "Lag Health → University")) +
    geom_vline(
      data = state_events,
      aes(xintercept = date, linetype = event),
      color = "grey30",
      linewidth = 0.85
    ) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Lag Governor → University" = "red",
        "Lag Governor → Health" = "blue",
        "Lag Health → University" = "green"
      )
    ) +
    scale_linetype_manual(
      name = "State events",
      values = c(
        "University closure announced" = "longdash",
        "University system reopened" = "dotdash",
        "Full vaccine eligibility" = "dotted"
      )
    ) +
    guides(
      color = guide_legend(nrow = 1, byrow = TRUE),
      linetype = guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 1.2)
      )
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.key.width = grid::unit(1.8, "cm")
    ) +
    labs(y = "Similarity Score", title = state) +
    scale_y_continuous(limits = c(0,1))
}


walk(state_abvs, function(state) {
  state_plot <- make_lag_plot(state)
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0("Lag/cossim_lag", state, ".pdf")
    ),
    plot = state_plot,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300
  )
})
