###########################################################
# Shared setup for all scripts: packages, colours, theme, helpers
###########################################################
suppressMessages({ library(tidyverse); library(ggrepel); library(ragg) })
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))  # umlauts in labels when run via Rscript
dir.create("../figures", showWarnings = FALSE)
dir.create("../data/output/tables", showWarnings = FALSE, recursive = TRUE)

accent <- "#cc0065"
party_cols  <- c(CDU = "#000000", SPD = "#E3000F", Greens = "#46962b", Left = "#BE3075", AfD = "#009EE0", BSW = "#7B2A7A", FDP = "#FFCC00")
family_cols <- c("CDU/CSU" = "#000000", "SPD" = "#E3000F", "Greens" = "#46962b", "FDP" = "#FFCC00", "Left" = "#BE3075",
                 "AfD" = "#009EE0", "BSW" = "#7B2A7A", "Far right (pre-AfD)" = "#8B4513", "Other" = "#BBBBBB")
fr_cols <- c("SRP/DRP (1950s)" = "#6b6b6b", "NPD" = "#8B4513", "DVU" = "#c46a1c", "REP" = "#e0a020",
             "Schill" = "#8a1508", "BIW (Bremen)" = "#b8860b", "AfD" = "#009EE0")
reg_cols <- c("West" = "#000000", "East (incl. Berlin from 1990)" = accent)
decade_breaks <- as.Date(paste0(seq(1950, 2025, 10), "-01-01"))

theme_sm <- function(base_size = 16) {
  theme_minimal(base_size = base_size, base_family = "Fira Sans") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#e6e6e6", linewidth = .3),
          plot.title = element_text(face = "bold", size = base_size * 1.15),
          plot.subtitle = element_text(colour = "#555555"),
          plot.caption = element_text(colour = "#888888", size = base_size * .6, hjust = 0),
          legend.position = "top", legend.title = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0),
          plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA))
}
save_fig <- function(p, name, w = 12, h = 6.5)
  ggsave(file.path("../figures", name), p, width = w, height = h, dpi = 200, device = agg_png, bg = "white")

# state election results 1946-2026 (from 00_data_state_elections.R)
load_state_elections <- function() {
  d <- read_csv("../data/output/state_elections_long.csv", show_col_types = FALSE) |>
    mutate(region = if_else(east == 1 | (land == "be" & year >= 1990), "East (incl. Berlin from 1990)", "West"))
  state_order <- d |> distinct(land, name_en, east) |> arrange(desc(east), name_en)
  d |> mutate(name_en = factor(name_en, levels = state_order$name_en))
}
