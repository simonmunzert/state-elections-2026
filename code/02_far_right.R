###########################################################
# 02: Finding 2. The far right is not the new kid on the block, but stronger than ever
# Far-right parties in state parliaments 1946-2026 (timeline, strength, re-entry),
# AfD at state elections 2013-2026, AfD vs. the 2025 federal result.
# Output: figures 02_*.png, tables in ../data/output/tables/
###########################################################

source("_theme.R")
d <- load_state_elections()

###########################################################
# 1. Far-right parties in state parliaments, 1946-2026
###########################################################
fr <- d |> filter(far_right) |>
  mutate(party_lab = case_when(party %in% c("srp", "drp") ~ "SRP/DRP (1950s)",
                               party == "npd" ~ "NPD", party == "dvu" ~ "DVU", party == "rep" ~ "REP",
                               party == "schill" ~ "Schill", party == "biw" ~ "BIW (Bremen)",
                               party == "afd" ~ "AfD", party == "dsu" ~ "DSU"),
         in_parl = !is.na(seats) & seats > 0,
         seat_share = seats / total_seats)

# table: every entry into a state parliament
next_elec <- d |> distinct(land, elec_no) |> arrange(land, elec_no) |>
  group_by(land) |> mutate(next_elec_no = lead(elec_no)) |> ungroup()
fr_entries <- fr |> filter(in_parl, party != "dsu") |>
  left_join(next_elec, by = c("land", "elec_no")) |>
  left_join(fr |> filter(in_parl) |> distinct(land, party, elec_no) |> mutate(re_entered = TRUE),
            by = c("land", "party", "next_elec_no" = "elec_no")) |>
  mutate(re_entered = if_else(is.na(next_elec_no), NA, coalesce(re_entered, FALSE))) |>
  arrange(date)
write_csv(fr_entries |> select(land, name_en, year, date, party, party_lab, pct, seats, total_seats, seat_share, re_entered),
          "../data/output/tables/far_right_in_state_parliaments.csv")

fr_summary <- fr_entries |> mutate(era = if_else(party == "afd", "AfD (2014-)", "Pre-AfD far right (1951-2011)")) |>
  group_by(era) |> summarise(entries = n(), states = n_distinct(land), mean_share = mean(pct),
                             max_share = max(pct), max_seat_share = max(seat_share),
                             re_entered = mean(re_entered, na.rm = TRUE), .groups = "drop")
print(fr_summary); write_csv(fr_summary, "../data/output/tables/far_right_summary.csv")

# Fig 1: timeline of far-right presence in Landtage
all_elec <- d |> distinct(name_en, date, east)
p1 <- ggplot() +
  geom_point(data = all_elec, aes(date, name_en), shape = 124, colour = "#cccccc", size = 3) +
  geom_point(data = fr |> filter(in_parl, party != "dsu"),
             aes(date, name_en, colour = party_lab, size = pct)) +
  geom_hline(yintercept = 5.5, colour = "#999999", linetype = "dotted") +
  annotate("text", x = as.Date("1947-01-01"), y = 5.7, label = "West", hjust = 0, vjust = 0, colour = "#999999", family = "Fira Sans", size = 4) +
  annotate("text", x = as.Date("1947-01-01"), y = 5.3, label = "East", hjust = 0, vjust = 1, colour = "#999999", family = "Fira Sans", size = 4) +
  scale_colour_manual(values = fr_cols, breaks = names(fr_cols)) +
  scale_size_area(max_size = 12, breaks = c(5, 10, 20, 40), name = "vote share (%)") +
  scale_x_date(breaks = decade_breaks, date_labels = "%Y", limits = as.Date(c("1946-01-01", "2027-06-01"))) +
  guides(colour = guide_legend(override.aes = list(size = 5), nrow = 1), size = guide_legend(nrow = 1)) +
  labs(title = "Far-right parties in German state parliaments, 1946-2026",
       subtitle = "Each dot = a far-right party winning seats at a state election. Grey ticks = all state elections.",
       x = NULL, y = NULL) +
  theme_sm() + theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
save_fig(p1, "02_far_right_timeline.png", 12, 7)

# Fig 2: strength: best far-right result per state election over time
best <- fr |> filter(party != "dsu", !is.na(pct)) |>
  group_by(land, elec_no) |> slice_max(pct, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(lab = if_else((pct >= 9 & party != "afd") | pct >= 29,
                       paste0(party_lab, " ", name_en, " ", year, ": ", pct, "%"), NA))
p2 <- ggplot(best, aes(date, pct, colour = party_lab)) +
  geom_hline(yintercept = 5, colour = "#999999", linetype = "dotted") +
  annotate("text", x = as.Date("1946-06-01"), y = 5.6, label = "5% threshold", hjust = 0, colour = "#999999", family = "Fira Sans", size = 3.5) +
  geom_point(aes(shape = in_parl), size = 3) +
  geom_text_repel(aes(label = lab), family = "Fira Sans", size = 3.6, show.legend = FALSE, min.segment.length = 0, box.padding = .4, max.overlaps = 30) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), labels = c("no seats", "seats"), name = NULL) +
  scale_colour_manual(values = fr_cols, breaks = names(fr_cols)) +
  scale_y_continuous(breaks = seq(0, 45, 5), limits = c(0, 46), labels = \(x) paste0(x, "%")) +
  scale_x_date(breaks = decade_breaks, date_labels = "%Y") +
  labs(title = "The far right in state elections: from single-digit episodes to the AfD",
       subtitle = "Strongest far-right list at each state election, 1946-2026", x = NULL, y = NULL) +
  theme_sm() + guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))
save_fig(p2, "02_far_right_strength.png", 12, 7)


# ---- top far-right seat shares ----
seat_top <- d |> filter(!is.na(seats), seats > 0) |> mutate(seat_share = seats / total_seats) |>
  filter(far_right) |> arrange(desc(seat_share)) |> select(name_en, year, party, pct, seats, total_seats, seat_share) |> head(10)
print(seat_top); write_csv(seat_top, "../data/output/tables/far_right_top_seat_shares.csv")

###########################################################
# 5. AfD across state elections, 2013-2026
###########################################################
afd <- d |> filter(party == "afd", !is.na(pct), year >= 2013) |>
  mutate(seat_share = seats / total_seats)
p6 <- ggplot(afd, aes(date, pct, colour = region)) +
  geom_hline(yintercept = 5, colour = "#999999", linetype = "dotted") +
  geom_line(aes(group = land), colour = "#dddddd", linewidth = .5) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = paste0(name_en, " ", pct)), family = "Fira Sans", size = 3.3, max.overlaps = 40, box.padding = .25, show.legend = FALSE) +
  scale_colour_manual(values = reg_cols) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), breaks = seq(0, 45, 5), limits = c(0, 46)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "AfD vote share at state elections, 2013-2026",
       subtitle = "Thin grey lines connect consecutive elections in the same state", x = NULL, y = NULL) +
  theme_sm()
save_fig(p6, "02_afd_state_elections.png", 13, 7)


# ---- 3. AfD: previous state election vs. federal election 2025 vs. state election 2026 -----
afd <- tribble(
  ~state, ~prev_ltw, ~btw25, ~ltw26,
  "Saxony-Anhalt", 20.8, 37.1, 43.8,
  "Mecklenburg-Vorpommern", 16.7, 35.0, 38.2,
  "Berlin", 9.1, 15.2, 16.3
) |> pivot_longer(-state, names_to = "election", values_to = "pct") |>
  mutate(election = factor(election, levels = c("prev_ltw", "btw25", "ltw26"),
                           labels = c("previous state election (2021/23)", "federal election Feb 2025 (state result)", "state election 2026")),
         state = factor(state, levels = c("Berlin", "Mecklenburg-Vorpommern", "Saxony-Anhalt")))
write_csv(afd, "../data/output/tables/afd_vs_btw25.csv")
p12 <- ggplot(afd, aes(pct, state)) +
  geom_line(aes(group = state), colour = "#cccccc", linewidth = 2) +
  geom_point(aes(colour = election), size = 6) +
  geom_text(aes(label = pct, colour = election, vjust = if_else(election == levels(election)[2], 2.3, -1.3)), family = "Fira Sans", size = 4.2, show.legend = FALSE) +
  scale_colour_manual(values = c("#bbbbbb", "#555555", "#009EE0")) +
  scale_x_continuous(labels = \(x) paste0(x, "%"), limits = c(0, 50), breaks = seq(0, 50, 10)) +
  labs(title = "The AfD surge is smaller than it looks",
       subtitle = "AfD vote share: gains of +23, +21.5 and +7 points vs. the previous state election shrink to +6.7, +3.2 and +1.1 vs. the 2025 federal election.", x = NULL, y = NULL) +
  theme_sm(15) + theme(panel.grid.major.y = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 4)))
ggsave("../figures/02_afd_vs_btw25.png", p12, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")

cat("done\n")
