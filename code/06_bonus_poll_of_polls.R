###########################################################
# 06: Bonus. Federal poll of polls, January 2025 to today
# Data: zweitstimme.org polling API (api.zweitstimme.org/v2),
#       which collects published polls from wahlrecht.de and dawum.de
# Output: figures/06_poll_of_polls.png
#         data/output/tables/polls_federal_2025_2026.csv  (one row per poll x party)
#         data/output/tables/poll_of_polls_federal.csv    (smoothed daily series)
###########################################################

source("_theme.R")
suppressMessages({ library(jsonlite); library(httr2) })

# ---- 1. download polls (cached) ----------------------------------------------
start_date <- as.Date("2025-01-23")   # one month before the federal election of 23 Feb 2025
dir.create("../data/input/zweitstimme", showWarnings = FALSE, recursive = TRUE)
f <- "../data/input/zweitstimme/federal_polls_2025_2026.json"
if (!file.exists(f) || as.Date(file.mtime(f)) < Sys.Date()) {
  url <- paste0("https://api.zweitstimme.org/v2/polls?scope=federal&published_from=", start_date,
                "&include_results=true&sort=published_date&limit=10000")
  writeLines(request(url) |> req_user_agent("state-elections-2026 (R)") |> req_perform() |> resp_body_string(), f)
}
j <- fromJSON(f, simplifyVector = TRUE)
polls <- as_tibble(j$data)
cat("Polls downloaded:", nrow(polls), "| total on server:", j$pagination$total, "\n")

# ---- 2. tidy: one row per poll x party, drop duplicates across providers ----
# The API keeps the same poll from wahlrecht.de and dawum.de as a matched pair;
# keep the wahlrecht.de row of each pair (and all unmatched polls).
polls <- polls |>
  filter(matching_status != "matched" | provider_name == "wahlrecht.de") |>
  mutate(date = as.Date(coalesce(survey_end_date, published_date)), published = as.Date(published_date)) |>
  filter(date >= start_date)
res <- polls |> select(id, public_id, institute = institute_name, provider = provider_name, date, published, respondents, results) |>
  unnest(results) |>
  mutate(party = recode(party_key, CDU_CSU = "CDU/CSU", AFD = "AfD", SPD = "SPD", GRUENE = "Greens", LINKE = "Left", BSW = "BSW", FDP = "FDP", .default = NA_character_)) |>
  filter(!is.na(party), !is.na(percentage))
cat("Polls after de-duplication:", n_distinct(res$id), "| institutes:", n_distinct(res$institute), "\n")
write_csv(res |> select(-party_key, -party_short_name, -party_name), "../data/output/tables/polls_federal_2025_2026.csv")

# ---- 3. poll of polls: local regression per party on a daily grid -------------
# loess over the survey end date, weighted by sqrt(sample size); span tuned so that
# roughly the last 6-8 weeks of polls shape each point (similar to the ZEIT Wahltrend).
grid <- tibble(date = seq(start_date, max(res$date), by = "day"))
pop <- res |> group_by(party) |> group_modify(function(d, k) {
  d <- d |> mutate(t = as.numeric(date), w = sqrt(coalesce(respondents, 1000)))
  fit <- loess(percentage ~ t, data = d, weights = w, span = 0.12, degree = 1, family = "symmetric")
  tibble(date = grid$date, trend = as.numeric(predict(fit, newdata = data.frame(t = as.numeric(grid$date)))))
}) |> ungroup() |> filter(!is.na(trend))
write_csv(pop, "../data/output/tables/poll_of_polls_federal.csv")
print(pop |> filter(date == max(date)) |> arrange(desc(trend)))

# ---- 4. events: federal election and all state elections since --------------------
events <- tribble(
  ~date, ~label,
  "2025-02-23", "Federal election",
  "2025-03-02", "Hamburg",
  "2026-03-08", "Baden-Württemberg",
  "2026-03-22", "Rhineland-Palatinate",
  "2026-09-06", "Saxony-Anhalt",
  "2026-09-20", "Berlin, MV"
) |> mutate(date = as.Date(date), y = c(34, 31, 34, 31, 34, 31))
btw25 <- tribble(~party, ~pct, "CDU/CSU", 28.5, "AfD", 20.8, "SPD", 16.4, "Greens", 11.6, "Left", 8.8, "BSW", 5.0, "FDP", 4.3) |>
  mutate(date = as.Date("2025-02-23"))

# ---- 5. figure ------------------------------------------------------------------
pc <- party_cols[c("CDU/CSU", "AfD", "SPD", "Greens", "Left", "BSW", "FDP")]
names(pc)[1] <- "CDU/CSU"
cols <- c(CDU = "#000000", "CDU/CSU" = "#000000", AfD = "#009EE0", SPD = "#E3000F", Greens = "#46962b", Left = "#BE3075", BSW = "#7B2A7A", FDP = "#FFCC00")
res <- res |> mutate(party = factor(party, levels = names(cols)[-1]))
pop <- pop |> mutate(party = factor(party, levels = names(cols)[-1]))
last <- pop |> filter(date == max(date)) |> mutate(lab = paste0(party, " ", round(trend)))

p <- ggplot() +
  geom_vline(data = events, aes(xintercept = date), colour = "#999999", linetype = "22", linewidth = .4) +
  geom_label(data = events, aes(date, y, label = label), family = "Fira Sans", size = 3.2, colour = "#555555",
             fill = "white", linewidth = 0, hjust = 0, nudge_x = 3) +
  geom_hline(yintercept = 5, colour = "#bbbbbb", linetype = "dotted") +
  geom_point(data = res, aes(date, percentage, colour = party), alpha = .18, size = 1.4, shape = 16) +
  geom_line(data = pop, aes(date, trend, colour = party), linewidth = 1.6) +
  geom_point(data = btw25, aes(date, pct, fill = party), shape = 23, size = 3.2, colour = "white", stroke = .6) +
  geom_text_repel(data = last, aes(date, trend, label = lab, colour = party), hjust = 0, nudge_x = 12, direction = "y",
                  family = "Fira Sans", size = 4, fontface = "bold", segment.colour = "#bbbbbb", show.legend = FALSE) +
  scale_colour_manual(values = cols, guide = "none") + scale_fill_manual(values = cols, guide = "none") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y", expand = expansion(mult = c(.01, .09))) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), breaks = seq(0, 35, 5), limits = c(0, 36)) +
  labs(title = "Poll of polls: federal voting intention since January 2025",
       subtitle = "Dots: individual polls (survey end date). Lines: local regression, weighted by sample size. Diamonds: result of 23 Feb 2025.",
       x = NULL, y = NULL) +
  theme_sm(15)
save_fig(p, "06_poll_of_polls.png", 12, 7)
cat("done\n")
