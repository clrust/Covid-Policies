# Created by: CR
# Date: 7/24/26
# States categorized into strata based on party control of executive/legislative

library(tidyverse)
library(patchwork)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

purple_state <- c("VA", "PA", "MN", "MI", "MA")
red_state <- c("TX", "GA", "FL")
blue_state <- c("CA", "CO", "IL", "NY")

data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") %>%
  mutate(strata = case_when(
    State %in% purple_state ~ "purple",
    State %in% red_state ~ "red",
    State %in% blue_state ~ "blue",
    .default = NA
  )) %>%
  group_by(Date, strata) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

make_base_plot <- function(strata) {
  strata_df <- data %>%
    filter(.data$strata == .env$strata)
  
  ggplot(strata_df) +
    geom_line(aes(x = Date, y = GU, color = "Governor → University")) +
    geom_line(aes(x = Date, y = GH, color = "Governor → Health")) +
    geom_line(aes(x = Date, y = HU, color = "Health → University")) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Governor → University" = "red",
        "Governor → Health" = "blue",
        "Health → University" = "green"
      )
    ) +
    labs(y = "Similarity Score", title = str_to_title(paste0(strata, " states")
                                                      )
         ) +
    scale_y_continuous(limits = c(0,1))
}

plot_directory <- "Testing/Results/FilteredDataPlots/Strata_Plots"

dir.create(
  plot_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

strata <- unique(data$strata)

walk(strata, function(strata) {
  strata_plot <- make_base_plot(strata)
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0("NoLag/cossim", strata, ".png")
    ),
    plot = strata_plot,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300
  )
})


make_lag_plot <- function(strata) {
  strata_df <- data %>%
    filter(.data$strata == .env$strata)
  
  ggplot(strata_df) +
    geom_line(aes(x = Date, y = Glag1_U, color = "Lag Gov > Uni")) +
    geom_line(aes(x = Date, y = Glag1_H, color = "Lag Gov > Health")) +
    geom_line(aes(x = Date, y = Hlag1_U, color = "Lag Health > Uni")) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Lag Gov > Uni" = "red",
        "Lag Gov > Health" = "blue",
        "Lag Health > Uni" = "green"
      )
    ) +
    labs(y = "Similarity Score", title = str_to_title(paste0(strata, " states")
    )
    ) +
    scale_y_continuous(limits = c(0,1))
}

walk(strata, function(strata) {
  strata_plot <- make_lag_plot(strata)
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0("LagPlots/cossim", strata, ".png")
    ),
    plot = strata_plot,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300
  )
})

r <- make_lag_plot("red")
b <- make_lag_plot("blue")
p <- make_lag_plot("purple")

b / p / r

(b + p + r) + plot_layout(guides = "collect")
