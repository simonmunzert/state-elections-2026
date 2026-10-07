###########################################################
# 04: Finding 4. Turnout is back
# Turnout at state elections 1946-2026, net gains from non-voters 2026 (infratest dimap),
# turnout persistence after surges (does high mobilisation hold?).
# Output: figures 04_*.png, tables in ../data/output/tables/
###########################################################

source("_theme.R")
d <- load_state_elections()

###########################################################
# 4. Turnout
###########################################################
turn <- d |> distinct(land, name_en, elec_no, year, date, region, turnout) |> filter(!is.na(turnout))
write_csv(turn, "../data/output/tables/turnout_by_election.csv")
p5 <- ggplot(turn, aes(date, turnout, colour = region)) +
  geom_point(alpha = .55, size = 2) +
  geom_smooth(method = "loess", span = .4, se = FALSE, linewidth = 1.4, formula = y ~ x) +
  geom_text_repel(data = turn |> filter(year == 2026), aes(label = paste0(name_en, " ", turnout, "%")),
                  family = "Fira Sans", size = 3.6, nudge_x = 800, direction = "y", hjust = 0, segment.colour = "#999999", show.legend = FALSE) +
  scale_colour_manual(values = reg_cols) +
  scale_x_date(breaks = decade_breaks, date_labels = "%Y", limits = as.Date(c("1946-01-01", "2033-01-01"))) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(40, 100)) +
  labs(title = "Turnout is back. The East now votes more than the West.",
       subtitle = "Turnout at every state election, 1946-2026", x = NULL, y = NULL) +
  theme_sm()
save_fig(p5, "04_turnout.png", 12, 7)


# ---- 2. net gains from former non-voters (ARD/infratest Wählerwanderung, thousands) ------
flows <- tribble(
  ~state, ~party, ~nonvoters,
  "Saxony-Anhalt", "AfD", 170, "Saxony-Anhalt", "CDU", 30, "Saxony-Anhalt", "SPD", 18,
  "Saxony-Anhalt", "Left", 15, "Saxony-Anhalt", "Greens", 11, "Saxony-Anhalt", "BSW", 11,
  "Berlin", "Left", 75, "Berlin", "AfD", 61, "Berlin", "Greens", 20, "Berlin", "SPD", 18, "Berlin", "BSW", 18, "Berlin", "CDU", 10,
  "Mecklenburg-Vorpommern", "AfD", 66, "Mecklenburg-Vorpommern", "SPD", 16, "Mecklenburg-Vorpommern", "BSW", 4,
  "Mecklenburg-Vorpommern", "Greens", 2, "Mecklenburg-Vorpommern", "Left", 1, "Mecklenburg-Vorpommern", "CDU", 0
) |> mutate(state = factor(state, levels = c("Saxony-Anhalt", "Berlin", "Mecklenburg-Vorpommern")),
            party = factor(party, levels = names(party_cols)))
write_csv(flows, "../data/output/tables/nonvoter_flows_2026.csv")
p11 <- ggplot(flows, aes(nonvoters, fct_rev(party), fill = party)) +
  geom_col(width = .7) +
  geom_text(aes(label = paste0("+", nonvoters, "k")), hjust = -.15, family = "Fira Sans", size = 4) +
  facet_wrap(~ state, nrow = 1) +
  scale_fill_manual(values = party_cols, guide = "none") +
  scale_x_continuous(limits = c(0, 200), breaks = c(0, 50, 100, 150), expand = c(0, 0)) +
  labs(title = "Turnout went up, and the new voters went mostly one way",
       subtitle = "Net gains from former non-voters, in thousands (ARD / infratest dimap vote-flow analysis). Turnout: +17.5, +11.3, +7.3 points.",
       x = NULL, y = NULL) +
  theme_sm(15) + theme(panel.grid.major.y = element_blank(), panel.spacing.x = unit(2, "lines"))
ggsave("../figures/04_nonvoter_flows.png", p11, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")


# ---- turnout persistence: does high mobilisation hold? ----
turn <- read_csv("../data/output/state_elections_long.csv", show_col_types = FALSE) |>
  distinct(land, name_en, east, elec_no, year, date, turnout) |>
  filter(!is.na(turnout)) |>
  arrange(land, elec_no) |> group_by(land) |>
  mutate(d_now = turnout - lag(turnout),            # change at this election
         d_next = lead(turnout) - turnout,           # change at the next election
         turnout_next = lead(turnout), year_next = lead(year),
         region = if_else(east == 1 | (land == "be" & year >= 1990), "East", "West")) |>
  ungroup()

# ---- 1. regression to the mean: next change vs. this change -------------------
fit <- lm(d_next ~ d_now, data = turn)
fit_lvl <- lm(d_next ~ d_now + turnout, data = turn)
print(summary(fit)$coefficients); print(summary(fit_lvl)$coefficients)
cat("Share of post-1990 elections with turnout gain >= 5 pts:", mean(turn$d_now[turn$year >= 1990] >= 5, na.rm = TRUE), "\n")

p13 <- ggplot(turn |> filter(!is.na(d_now), !is.na(d_next)), aes(d_now, d_next)) +
  geom_hline(yintercept = 0, colour = "#999999") + geom_vline(xintercept = 0, colour = "#999999") +
  geom_point(aes(colour = year >= 1990), size = 2.5, alpha = .7) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = accent, fill = accent, alpha = .15, linewidth = 1.2) +
  geom_text_repel(data = turn |> filter(d_now >= 8 | d_now <= -12),
                  aes(label = paste0(name_en, " ", year, " → ", year_next)), family = "Fira Sans", size = 3.3, colour = "#333333", max.overlaps = 20) +
  scale_colour_manual(values = c(`FALSE` = "#bbbbbb", `TRUE` = "#000000"), labels = c("before 1990", "1990 and later")) +
  scale_x_continuous(labels = \(x) sprintf("%+d", x)) + scale_y_continuous(labels = \(x) sprintf("%+d", x)) +
  labs(title = "Turnout surges partly revert, but mostly stick",
       subtitle = sprintf("Turnout change at the next state election vs. change at this one (points), %d election pairs since 1946.\nRegression slope %.2f: on average, about one sixth of a surge is given back at the next election.",
                          sum(!is.na(turn$d_now) & !is.na(turn$d_next)), coef(fit)[2]),
       x = "turnout change at this election", y = "turnout change at the next election") +
  theme_sm()
ggsave("../figures/04_turnout_persistence.png", p13, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")

# ---- 2. what happened after the biggest surges? ------------------------------
surges <- turn |> filter(d_now >= 7) |> arrange(desc(d_now)) |>
  transmute(name_en, year, turnout_before = turnout - d_now, turnout, d_now, year_next, turnout_next, d_next,
            kept = if_else(is.na(d_next), NA, d_next > -d_now / 2))
print(surges, n = 30); write_csv(surges, "../data/output/tables/turnout_surges.csv")
cat("After surges of 7+ points: mean next change", round(mean(surges$d_next, na.rm = TRUE), 1),
    "| share keeping more than half of the gain:", round(mean(surges$kept, na.rm = TRUE), 2), "\n")

sp <- surges |> filter(!is.na(d_next) | year == 2026) |>
  mutate(lab = paste0(name_en, " ", year), lab = fct_reorder(lab, d_now)) |>
  pivot_longer(c(turnout_before, turnout, turnout_next), names_to = "when", values_to = "t") |>
  mutate(when = factor(when, levels = c("turnout_before", "turnout", "turnout_next"), labels = c("election before", "surge election", "next election")))
p14 <- ggplot(sp, aes(t, lab)) +
  geom_line(aes(group = lab), colour = "#cccccc", linewidth = 2) +
  geom_point(aes(colour = when), size = 5) +
  scale_colour_manual(values = c("#bbbbbb", accent, "#000000")) +
  scale_x_continuous(labels = \(x) paste0(x, "%")) +
  labs(title = "What happened after the biggest turnout surges",
       subtitle = "All state elections with a turnout gain of 7 points or more since 1946 (2026 cases: next election pending)", x = NULL, y = NULL) +
  theme_sm() + theme(panel.grid.major.y = element_blank())
ggsave("../figures/04_turnout_after_surges.png", p14, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")

cat("done\n")
