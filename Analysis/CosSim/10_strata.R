# Created by: CR
# Date: 7/24/26
# States categorized into strata based on party control of executive/legislative

library(tidyverse)
library(patchwork)
library(modelsummary)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")


uni_data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") %>%
  filter(no_release_flag_University == "release") %>%
  group_by(Date, party) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  drop_na()

health_data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") %>%
  filter(no_release_flag_Health == "release") %>%
  group_by(Date, party) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
  

gov_data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") %>%
  filter(no_release_flag_Governor == "release") %>%
  group_by(Date, party) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
  

make_uni_plot <- function(strata, strata_val) {
  
  uni_data %>% filter(.data[[strata]] == strata_val)%>%
  ggplot() +
    geom_line(aes(x = Date, y = Glag1_U, color = "Lag Gov → Uni")) +
    geom_line(aes(x = Date, y = Hlag1_U, color = "Lag Health → Uni")) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Lag Gov → Uni" = "darkgreen",
        "Lag Health → Uni" = "darkgrey"
      )
    )  +
    labs(y = "Similarity Score", title = str_to_title(paste0(strata_val, " states")
    )
    ) +
    scale_y_continuous(limits = c(0,1))
}

make_hg_plot <- function(strata, strata_val) {
  strata_hd <- health_data %>% filter(.data[[strata]] == strata_val)
  strata_gd <- gov_data %>% filter(.data[[strata]] == strata_val)
  ggplot() +
    geom_line(data = strata_hd, aes(x = Date, y = Glag1_H, color = "Lag Gov → Health")) +
    geom_line(data = strata_gd, aes(x = Date, y = G_Hlag1, color = "Lag Health → Gov")) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Lag Gov → Health" = "darkgreen",
        "Lag Health → Gov" = "darkgrey"
      )
    )  +
    labs(y = "Similarity Score", title = str_to_title(paste0(strata_val, " states")
    )
    ) +
    scale_y_continuous(limits = c(0,1))
}










#-----old plotting functions
make_base_plot <- function(strata) {
  strata_df <- data %>%
    filter(.data$strata == .env$strata)
  
  ggplot(strata_df) +
    geom_line(aes(x = Date, y = GU, color = "Gov | Uni")) +
    geom_line(aes(x = Date, y = GH, color = "Gov | Health")) +
    geom_line(aes(x = Date, y = HU, color = "Health | Uni")) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Gov | Uni" = "red",
        "Gov | Health" = "blue",
        "Health | Uni" = "green"
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
    geom_line(aes(x = Date, y = Glag1_U, color = "Lag Gov → Uni")) +
    geom_line(aes(x = Date, y = Glag1_H, color = "Lag Gov → Health")) +
    geom_line(aes(x = Date, y = Hlag1_U, color = "Lag Health → Uni")) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Lag Gov → Uni" = "red",
        "Lag Gov → Health" = "blue",
        "Lag Health → Uni" = "green"
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


walk(strata, function(strata) {
  nl_strata_plot <- make_base_plot(strata)
  l_strata_plot <- make_lag_plot(strata)
  strata_plot <- nl_strata_plot + l_strata_plot
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0("ComboPlots/cossim", strata, ".png")
    ),
    plot = strata_plot,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300
  )
})
r <- make_lag_plot("red")
r1 <- make_base_plot("red")
b <- make_lag_plot("blue")
p <- make_lag_plot("purple")

s
r + r1
b / p / r

(b + p + r) + plot_layout(guides = "collect")
