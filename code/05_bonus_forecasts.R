###########################################################
# 05: Bonus. zweitstimme.org forecasts vs. results, Sept 2026
# Input : input/zweitstimme/forecast_{st,be,mv}.json
#         (frozen forecasts from zweitstimme.org/api/v2/state/archive/)
#         output/state_elections_long.csv (results, from 01)
# Output: output/figures/05_forecast_vs_result.png
#         output/tables/forecast_vs_result_2026.csv
###########################################################

source("_theme.R")
suppressMessages(library(jsonlite))

# ---- download (cached) --------------------------------------------------
dir.create("../data/input/zweitstimme", showWarnings = FALSE, recursive = TRUE)
ids <- c(st = "st_2026-09-06", be = "be_2026-09-20", mv = "mv_2026-09-20")
for (s in names(ids)) {
  f <- file.path("../data/input/zweitstimme", paste0("forecast_", s, ".json"))
  if (!file.exists(f)) download.file(paste0("https://zweitstimme.org/api/v2/state/archive/", ids[s], ".json"), f, quiet = TRUE)
}

fc <- map_dfr(names(ids), function(s) {
  j <- fromJSON(file.path("../data/input/zweitstimme", paste0("forecast_", s, ".json")))
  as_tibble(j$data$parties) |>
    transmute(land = s, party_label = party, party = party_code, fit, low, high,
              last_poll = j$data$metadata$last_poll_date)
})
scen <- map_dfr(names(ids), function(s) {
  j <- fromJSON(file.path("../data/input/zweitstimme", paste0("forecast_", s, ".json")))
  as_tibble(j$data$scenarios$items) |> mutate(land = s)
})
write_csv(scen, "../data/output/tables/forecast_scenarios_2026.csv")

# ---- results ------------------------------------------------------------
res <- read_csv("../data/output/state_elections_long.csv", show_col_types = FALSE) |>
  filter(year == 2026, land %in% names(ids)) |>
  mutate(party = recode(party, lin = "linke", gru = "gruene", oth = "sonstige")) |>
  select(land, name_en, party, result = pct)

ev <- fc |> mutate(party = recode(party, lin = "linke", gru = "gruene", oth = "sonstige")) |>
  left_join(res, by = c("land", "party")) |>
  mutate(error = result - fit, inside = result >= low & result <= high,
         name_en = fct_relevel(name_en, "Saxony-Anhalt", "Berlin", "Mecklenburg-Vorpommern"))
# fallback if party codes differ from the json: join by label
if (any(is.na(ev$result))) print(ev |> filter(is.na(result)))
write_csv(ev, "../data/output/tables/forecast_vs_result_2026.csv")

ev |> group_by(name_en) |> summarise(mae = mean(abs(error)), inside = sum(inside), n = n()) |> print()

# ---- figure -------------------------------------------------------------
pc <- c(cdu = "#000000", spd = "#E3000F", gruene = "#46962b", fdp = "#FFCC00",
                linke = "#BE3075", afd = "#009EE0", bsw = "#7B2A7A", sonstige = "#BBBBBB")
ev <- ev |> mutate(party = factor(party, levels = names(pc)),
                   party_label = recode(party_label, "LINKE" = "Linke", "GRÜNE" = "Greens", "Sonstige" = "Other"))
p9 <- ggplot(ev, aes(y = fct_rev(fct_reorder(party_label, as.integer(party))))) +
  geom_segment(aes(x = low, xend = high, colour = party), linewidth = 6, alpha = .35, lineend = "round") +
  geom_point(aes(x = fit, colour = party), size = 3.5) +
  geom_point(aes(x = result, shape = inside), size = 4.5, colour = "black", fill = "white", stroke = 1.2) +
  geom_text(aes(x = pmax(high, result) + 1.5, label = sprintf("%+.1f", error)), hjust = 0, family = "Fira Sans", size = 3.6, colour = "#555555") +
  facet_wrap(~ name_en, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = pc, guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 23, `FALSE` = 4), labels = c("result outside interval", "result inside 5/6 interval"), name = NULL) +
  scale_x_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(.02, .18))) +
  labs(title = "Polls-based forecasts vs. results, September 2026",
       subtitle = "Coloured bar = zweitstimme.org 5/6 forecast interval (frozen 2-3 days before the election), dot = point forecast, diamond/cross = result. Label: result minus forecast.", x = NULL, y = NULL) +
  theme_minimal(base_size = 15, base_family = "Fira Sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = .3),
        plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "#555555", size = 11),
        plot.caption = element_text(colour = "#888888", size = 9, hjust = 0), plot.title.position = "plot",
        strip.text = element_text(face = "bold", hjust = 0, size = 14), legend.position = "top",
        plot.background = element_rect(fill = "white", colour = NA))
ggsave("../figures/05_forecast_vs_result.png", p9, width = 15, height = 6, dpi = 200, device = agg_png, bg = "white")
cat("done\n")
