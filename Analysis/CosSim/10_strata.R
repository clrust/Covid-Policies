# Created by: CR
# Date: 7/24/26
# States categorized into strata based on party control of executive/legislative

library(tidyverse)
library(patchwork)
library(modelsummary)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")


clean_data <- function(df, agency, strata) {
  flag <- paste0("no_release_flag_", agency)
  df %>%
    filter(.data[[flag]] == "release") %>%
    group_by(Date, .data[[strata]]) %>%
    summarise(
      across(
        where(is.numeric),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          n = ~ sum(!is.na(.x))
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )%>%
    drop_na()
}


cossim_data <- read_csv("Testing/Results/07_cossim_data_filtered.csv")

make_uni_plot <- function(df, strata, strata_val) {
  df %>% 
    clean_data("University", strata) %>%
    filter(.data[[strata]] == strata_val) %>%
  ggplot() +
    geom_line(aes(x = Date, y = Glag1_U_mean, color = "Lag Gov → Uni")) +
    geom_line(aes(x = Date, y = Hlag1_U_mean, color = "Lag Health → Uni")) +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Lag Gov → Uni" = "darkgreen",
        "Lag Health → Uni" = "darkred"
      )
    )  +
    labs(y = "Similarity Score", title = str_to_title(paste0(strata_val, " states")
    )
    ) +
    scale_y_continuous(limits = c(0,1))
}

make_hg_plot <- function(df, strata, strata_val) {
  strata_hd <- df %>%
    clean_data("Health", strata) %>%
    filter(.data[[strata]] == strata_val)
  strata_gd <- df %>%
    clean_data("Governor", strata) %>%
    filter(.data[[strata]] == strata_val)
  ggplot() +
    geom_line(data = strata_hd, aes(x = Date, y = Glag1_H_mean, color = "Lag Gov → Health"), 
              alpha = 0.6, linetype = "solid") +
    geom_line(data = strata_gd, aes(x = Date, y = G_Hlag1_mean, color = "Lag Health → Gov"), 
              alpha = 0.6, linetype = "solid") +
    theme_bw() +
    scale_color_manual(
      name = "Series",
      values = c(
        "Lag Gov → Health" = "darkgreen",
        "Lag Health → Gov" = "darkred"
      )
    )  +
    labs(y = "Similarity Score", title = str_to_title(paste0(strata_val, " states")
    )
    ) +
    scale_y_continuous(limits = c(0,1))
}


plot_directory <- "Testing/Results/StarFilteredDataPlots/StrataPlots"

pwalk(list(
  strata = c(rep("party", 3), rep("density_cat", 2)),
  strata_val = c("red", "blue", "purple", "high", "low")),
  function(strata, strata_val) {
    uni_plot <- make_uni_plot(cossim_data, strata, strata_val)
    hg_plot <- make_hg_plot(cossim_data, strata, strata_val)
    
    ggsave(
      filename = file.path(
        plot_directory,
        paste0("Uni", strata_val, ".pdf")
      ),
      plot = uni_plot,
      width = 10,
      height = 6,
      units = "in",
      dpi = 300
    )
    ggsave(
      filename = file.path(
        plot_directory,
        paste0("HG", strata_val, ".pdf")
      ),
      plot = hg_plot,
      width = 10,
      height = 6,
      units = "in",
      dpi = 300
    )
    }
  )









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
