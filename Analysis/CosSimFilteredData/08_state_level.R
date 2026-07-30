# Created by: CR
# Date: 7/24/26
# State level plots comparing GU, GH and HU and lagged agency-days

library(tidyverse)
library(patchwork)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

cossim_data <- read_csv("Testing/Results/07_cossim_data.csv")

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
    labs(y = "Similarity Score", title = state)
}

plot_directory <- "Testing/Results/State_Plots"

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
      paste0("cossim", state, ".png")
    ),
    plot = state_plot,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300
  )
})

#-------lag plots
make_lag_plot <- function(state) {
  state_df <- cossim_data %>%
    filter(.data$State == .env$state)
  
  ggplot(state_df) +
    geom_line(aes(x = Date, y = H_Ulag1, color = "Lag Health → University")) +
    geom_line(aes(x = Date, y = Glag1_U, color = "Lag Governor → University")) +
    geom_line(aes(x = Date, y = Glag1_H, color = "Lag Governor → Health")) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Lag Health → University" = "red",
        "Lag Governor → University" = "blue",
        "Lag Governor → Health" = "green"
      )
    ) +
    labs(y = "Similarity Score", title = state)
}


walk(state_abvs, function(state) {
  state_plot <- make_lag_plot(state)
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0("cossim_lag", state, ".png")
    ),
    plot = state_plot,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300
  )
})


