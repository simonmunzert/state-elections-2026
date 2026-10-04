###########################################################
# 04a: Finding 4. Age and geography are defining cleavages: age and education
# Vote by age group and by education, exit polls Sept 2026 (infratest dimap via KAS).
# Output: figures 04_age.png, 04_education.png
###########################################################

source("_theme.R")
# ---- 1. vote by age group (infratest dimap exit polls, %) ------------------
age_levels <- c("16/18-24", "25-34", "35-44", "45-59", "60-69", "70+")
age <- tribble(
  ~state, ~age, ~AfD, ~CDU, ~Left, ~SPD, ~Greens, ~BSW,
  "Saxony-Anhalt", "16/18-24", 41,  5, 15,  9, 14, 6,
  "Saxony-Anhalt", "25-34",    43,  6, 13,  8, 16, 6,
  "Saxony-Anhalt", "35-44",    52,  9,  7,  7, 12, 5,
  "Saxony-Anhalt", "45-59",    52, 13,  6,  7, 10, 5,
  "Saxony-Anhalt", "60-69",    47, 20,  7,  8,  6, 6,
  "Saxony-Anhalt", "70+",      31, 30, 11, 13,  5, 5,
  "Berlin",        "16/18-24", 12,  8, 43,  7, 11, 6,
  "Berlin",        "25-34",     9,  6, 49,  6, 13, 5,
  "Berlin",        "35-44",    15, 11, 31,  9, 18, 4,
  "Berlin",        "45-59",    20, 19, 20, 11, 18, 4,
  "Berlin",        "60-69",    23, 24, 16, 14, 13, 5,
  "Berlin",        "70+",      12, 33, 15, 21, 10, 5,
  "Mecklenburg-Vorpommern", "16/18-24", 35, 1, 21, 23,  7, 5,
  "Mecklenburg-Vorpommern", "25-34",    32, 3, 17, 24, 10, 5,
  "Mecklenburg-Vorpommern", "35-44",    45, 2,  6, 28,  7, 5,
  "Mecklenburg-Vorpommern", "45-59",    47, 5,  4, 29,  6, 5,
  "Mecklenburg-Vorpommern", "60-69",    41, 6,  3, 37,  4, 5,
  "Mecklenburg-Vorpommern", "70+",      26, 7,  4, 52,  4, 5
) |> pivot_longer(AfD:BSW, names_to = "party", values_to = "pct") |>
  mutate(age = factor(age, levels = age_levels),
         state = factor(state, levels = c("Saxony-Anhalt", "Berlin", "Mecklenburg-Vorpommern")),
         party = factor(party, levels = names(party_cols)))
write_csv(age, "../data/output/tables/exit_poll_age_2026.csv")

lab <- age |> filter(age == "70+")
p10 <- ggplot(age, aes(age, pct, colour = party, group = party)) +
  geom_line(linewidth = 1.6) + geom_point(size = 3) +
  geom_text_repel(data = lab, aes(label = party), hjust = 0, nudge_x = .3, direction = "y", family = "Fira Sans", size = 4, segment.colour = "#bbbbbb", min.segment.length = 0, show.legend = FALSE) +
  facet_wrap(~ state, nrow = 1) +
  scale_colour_manual(values = party_cols, guide = "none") +
  scale_y_continuous(labels = \(x) paste0(x, "%"), breaks = seq(0, 50, 10), limits = c(0, 55)) +
  scale_x_discrete(expand = expansion(add = c(.4, 1.3))) +
  labs(title = "Support for the centre is dying out",
       subtitle = "Vote share by age group, exit polls (infratest dimap / ARD). CDU and SPD peak among the over-70s, the AfD at 35 to 59.",
       x = NULL, y = NULL) +
  theme_sm(15) + theme(panel.spacing.x = unit(1.5, "lines"))
ggsave("../figures/04_age.png", p10, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")


# ---- 2. vote by education group (infratest dimap exit polls via KAS Wahlanalysen) ----
# data/input/kas/exit_polls_education_occupation_2026.csv: parsed from the KAS reports
# (infratest dimap: Bildung niedrig/mittel/hoch; FGW: four school-leaving categories)
edu <- read_csv("../data/input/kas/exit_polls_education_occupation_2026.csv", show_col_types = FALSE) |>
  filter(institute == "infratest dimap", group_type == "education") |>
  mutate(state = recode(state, "Sachsen-Anhalt" = "Saxony-Anhalt", "Mecklenburg-Vorpommern" = "Mecklenburg-Vorpommern"),
         state = factor(state, levels = c("Saxony-Anhalt", "Berlin", "Mecklenburg-Vorpommern")),
         group = factor(recode(group, Niedrig = "low", Mittel = "medium", Hoch = "high"), levels = c("low", "medium", "high")),
         party = recode(party, "Grüne" = "Greens", "Linke" = "Left"),
         party = factor(party, levels = names(party_cols))) |>
  filter(!is.na(party), !is.na(group), party != "FDP")
print(edu |> select(state, group, party, pct) |> pivot_wider(names_from = party, values_from = pct))
lab_e <- edu |> filter(group == "high")
p_edu <- ggplot(edu, aes(group, pct, colour = party, group = party)) +
  geom_line(linewidth = 1.6) + geom_point(size = 3) +
  geom_text_repel(data = lab_e, aes(label = party), hjust = 0, nudge_x = .3, direction = "y", family = "Fira Sans", size = 4, segment.colour = "#bbbbbb", min.segment.length = 0, show.legend = FALSE) +
  facet_wrap(~ state, nrow = 1) +
  scale_colour_manual(values = party_cols, guide = "none") +
  scale_y_continuous(labels = \(x) paste0(x, "%"), breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_x_discrete(expand = expansion(add = c(.4, 1.3))) +
  labs(title = "Education divides less than age, except for the AfD",
       subtitle = "Vote share by formal education (low / medium / high), exit polls (infratest dimap / ARD)",
       x = "formal education", y = NULL) +
  theme_sm(15) + theme(panel.spacing.x = unit(1.5, "lines"))
ggsave("../figures/04_education.png", p_edu, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")

cat("done\n")
