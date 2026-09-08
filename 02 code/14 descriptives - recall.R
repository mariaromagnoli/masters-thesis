# 14 descriptives - recall.R
#
# Descriptives. How completely workers recalled their preloaded co-hire
# roster, whether recall falls with roster size or rises with proximity,
# and how often the two sides of a pair confirm each other (reciprocity)
# layer by layer.
#
# Feeds: Empirical Setting (recall and reciprocity). pn_recall.tex is read
#        back by script 21.
#
# Input:  respondents.parquet, dyads.parquet
# Output: 10 descriptives/table_recall_stages.csv, table_reciprocity.csv,
#         table_recall_by_roster_size.csv, table_recall_by_proximity.csv,
#         04 latex/figures/fig_setting_recall_by_roster.pdf,
#         fig_setting_reciprocity.pdf,
#         04 latex/tables/table_reciprocity.tex, 04 latex/pn/pn_recall.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# Respondents and dyads, and the plot theme shared by the two figures.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "14 descriptives - recall.R"

col_green <- "#a8d5a2"
col_purple <- "#e0b0FF"
col_wine <- "#722F37"
fig_alpha <- 0.7
theme_set(theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

resp <- read_parquet(file.path(dir_derived, "respondents.parquet")) %>%
  filter(!in_problem)
d <- read_parquet(file.path(dir_derived, "dyads.parquet"))
pn <- list()

cols_hdr <- baseline_names()

# Every roster slot the ego was shown, and whether she recalled it.
choice_all <- read_baseline_cols(c("SubmissionDate", "worker_id",
  "consent_survey",
  grep("^ch_name_[0-9]+$", cols_hdr, value = TRUE))) %>%
  dedup_respondents() %>%
  select(worker_id, matches("^ch_name_")) %>%
  pivot_longer(-worker_id, names_to = "slot", values_to = "cn") %>%
  filter(!is.na(cn), cn != "") %>%
  transmute(worker_id, K = as.integer(str_extract(slot, "[0-9]+$"))) %>%
  semi_join(resp, by = "worker_id")

recall_all <- read_baseline_cols(c("SubmissionDate", "worker_id",
  "consent_survey",
  grep("^jh_recalled_[0-9]+$", cols_hdr, value = TRUE))) %>%
  dedup_respondents() %>%
  select(worker_id, matches("^jh_recalled_[0-9]+$")) %>%
  pivot_longer(-worker_id, names_to = "col", values_to = "v") %>%
  filter(v == "1") %>%
  transmute(worker_id, K = as.integer(str_extract(col, "[0-9]+$"))) %>%
  filter(K >= 1, K <= 90) %>%
  distinct() %>%
  mutate(recalled = 1L)

scan <- choice_all %>%
  left_join(recall_all, by = c("worker_id", "K")) %>%
  mutate(recalled = coalesce(recalled, 0L))

ego <- scan %>%
  group_by(worker_id) %>%
  summarise(n_roster = n(), n_recalled = sum(recalled), .groups = "drop") %>%
  mutate(recall_rate = n_recalled / n_roster)

cat("egos with a preloaded roster:", nrow(ego),
    "| roster entries:", nrow(scan), "\n")
cat("median roster shown:", median(ego$n_roster),
    "| median recalled:", median(ego$n_recalled),
    "| median recall rate:", round(100 * median(ego$recall_rate), 1), "%\n")

t6a <- tibble(
  stage = c("Roster slots preloaded (all alters)",
            "  ... recalled by the ego",
            "Dyads, both sides surveyed",
            "  ... recalled by the ego",
            "  ... named on at least one layer",
            "  ... named, given recalled"),
  n = c(nrow(scan), sum(scan$recalled),
        nrow(d), sum(d$recalled), sum(d$tied_any),
        sum(d$tied_any[d$recalled == 1L])),
  denominator = c(nrow(scan), nrow(scan), nrow(d), nrow(d), nrow(d),
                  sum(d$recalled))) %>%
  mutate(share = round(n / denominator, 3))

### 2. TABLES ---------------------------------------------------------
# Recall over the roster, reciprocity by layer, and recall by roster size
# and by proximity (same unit / woreda / batch).

write.csv(t6a, file.path(dir_desc, "table_recall_stages.csv"),
          row.names = FALSE)

# Join each directed dyad to its reverse direction to measure reciprocity.
recall_cols <- c("recalled", paste0("gen_", 1:12), names(rs_out_labels))
rev_side <- d %>%
  select(r_from = worker_id, r_to = alter_id, all_of(recall_cols)) %>%
  rename_with(~paste0("rev_", .x), all_of(recall_cols))
mutual <- d %>%
  inner_join(rev_side, by = c("worker_id" = "r_to", "alter_id" = "r_from"))
stopifnot(nrow(mutual) %% 2 == 0)
cat("\nmutual pairs:", nrow(mutual), "directed /", nrow(mutual) / 2,
    "undirected\n")

gen_reciprocal <- c(5, 6, 7, 8, 9, 10, 12)
gen_directional <- c(1, 2, 3, 4, 11)
stopifnot(setequal(c(gen_reciprocal, gen_directional), 1:12))

recip_row <- function(fwd_col, label, expectation) {
  fwd <- mutual[[fwd_col]]
  rev <- mutual[[paste0("rev_", fwd_col)]]
  n_fwd <- sum(fwd == 1L)
  n_both <- sum(fwd == 1L & rev == 1L)
  n_either <- sum(fwd == 1L | rev == 1L)
  tibble(layer = label, expectation = expectation, n_fwd = n_fwd,
         n_both = n_both,
         pct_recip = if_else(n_fwd > 0, n_both / n_fwd, NA_real_),
         jaccard = if_else(n_either > 0, n_both / n_either, NA_real_))
}

recip_rows <- function(idx, expectation) {
  bind_rows(lapply(idx, function(g)
    recip_row(paste0("gen_", g), generator_labels[[as.character(g)]],
              expectation)))
}
t6b <- bind_rows(
  recip_row("recalled", "Co-hire recall", "Expected reciprocal"),
  recip_rows(gen_reciprocal, "Expected reciprocal"),
  recip_rows(gen_directional, "Expected directional"),
  bind_rows(lapply(names(rs_out_labels), function(v)
    recip_row(v, unname(rs_out_labels[v]), "Expected directional")))) %>%
  mutate(across(c(pct_recip, jaccard), ~round(.x, 3)))
write.csv(t6b, file.path(dir_desc, "table_reciprocity.csv"), row.names = FALSE)

solid <- t6b %>% filter(n_fwd >= 5)
recip_mean <- solid %>%
  group_by(expectation) %>%
  summarise(pct = sum(n_both) / sum(n_fwd), .groups = "drop")

ego_fit <- ego %>% mutate(log_roster = log(n_roster))

fit_lin <- feols(recall_rate ~ n_roster, data = ego_fit, vcov = "hetero")
fit_log <- feols(recall_rate ~ log_roster, data = ego_fit, vcov = "hetero")
cat("\nrecall rate on roster size (ego level, HC1):\n")
print(etable(fit_lin, fit_log))

fit_cnt <- feols(n_recalled ~ n_roster, data = ego_fit, vcov = "hetero")
cat("\nrecalled COUNT on roster size (is the rate mechanical?):\n")
print(etable(fit_cnt))

t6c <- ego_fit %>%
  mutate(bucket = cut(n_roster, breaks = quantile(n_roster, 0:4 / 4),
                      include.lowest = TRUE, dig.lab = 3)) %>%
  group_by(bucket) %>%
  summarise(egos = n(), median_roster = median(n_roster),
            mean_recalled = round(mean(n_recalled), 2),
            mean_recall_rate = round(mean(recall_rate), 3), .groups = "drop")
write.csv(t6c, file.path(dir_desc, "table_recall_by_roster_size.csv"),
          row.names = FALSE)
cat("\nrecall by roster-size quartile:\n"); print(as.data.frame(t6c))

t6d <- bind_rows(
  d %>% filter(pu_known == 1L) %>%
    group_by(condition = if_else(same_pu == 1L, "Same production unit",
                                 "Different production unit")) %>%
    summarise(dyads = n(), n_recalled = sum(recalled),
              rate = mean(recalled), .groups = "drop"),
  d %>% filter(origin_valid) %>%
    group_by(condition = if_else(same_woreda == 1L, "Same birth woreda",
                                 "Different birth woreda")) %>%
    summarise(dyads = n(), n_recalled = sum(recalled),
              rate = mean(recalled), .groups = "drop"),
  d %>% group_by(condition = if_else(same_batch == 1L, "Same batch",
                                     "Different batch")) %>%
    summarise(dyads = n(), n_recalled = sum(recalled),
              rate = mean(recalled), .groups = "drop")) %>%
  mutate(rate = round(rate, 4))
stopifnot(all(t6d$rate <= 1))
write.csv(t6d, file.path(dir_desc, "table_recall_by_proximity.csv"),
          row.names = FALSE)
cat("\nrecall by proximity:\n"); print(as.data.frame(t6d))

fit_prox <- feols(recalled ~ same_pu_mi + pu_known + same_woreda + shared_lang,
                  data = d, cluster = ~worker_id)
cat("\nrecall on proximity (dyad level, clustered on ego):\n")
print(etable(fit_prox))

p_recip <- solid %>%
  mutate(layer = factor(layer, levels = rev(solid$layer)),
         expectation = factor(expectation,
           levels = c("Expected reciprocal", "Expected directional"))) %>%
  ggplot(aes(y = layer, x = pct_recip, fill = expectation)) +
  geom_col(width = 0.7, alpha = fig_alpha) +
  geom_text(aes(label = paste0(round(100 * pct_recip), "%  (n=", n_fwd, ")")),
            hjust = -0.08, size = 2.9) +
  scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"),
                     limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(values = c("Expected reciprocal" = col_green,
                               "Expected directional" = col_purple),
                    name = NULL) +
  facet_grid(expectation ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Share of the ego's reports the alter confirms", y = NULL) +
  theme(strip.text.y = element_blank(),
        plot.margin = margin(5.5, 16, 5.5, 5.5, "pt"))
save_fig("fig_setting_reciprocity", p_recip, w = 7, h = 5)

p_recall <- ggplot(ego_fit, aes(x = n_roster, y = recall_rate)) +
  geom_point(colour = col_wine, alpha = 0.45, size = 1.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = col_wine,
              fill = col_purple, alpha = 0.3, linewidth = 0.6) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(x = "Names preloaded into the roster the worker was shown",
       y = "Share of the roster recalled")
save_fig("fig_setting_recall_by_roster", p_recall, w = 7, h = 4.2)

### 3. LATEX ---------------------------------------------------------
# The reciprocity table, split into expected-reciprocal and directional
# layers.

cap <- function(x) sub("^(.)", "\\U\\1", x, perl = TRUE)
tex_rows <- solid %>%
  mutate(grp = as.integer(factor(expectation,
    levels = c("Expected reciprocal", "Expected directional"))))
write_tabular("table_reciprocity.tex", "lrrr",
  "\\textit{layer} & \\textit{reported} & \\textit{confirmed} & \\textit{reciprocity} \\\\",
  c(
  "\\multicolumn{4}{l}{\\textit{Expected reciprocal}} \\\\",
  sprintf("\\quad %s & %s & %s & %s\\%% \\\\",
          tex_esc(cap(tex_rows$layer[tex_rows$grp == 1])),
          pn_int(tex_rows$n_fwd[tex_rows$grp == 1]),
          pn_int(tex_rows$n_both[tex_rows$grp == 1]),
          pn_pct(tex_rows$pct_recip[tex_rows$grp == 1])),
  "\\addlinespace",
  "\\multicolumn{4}{l}{\\textit{Expected directional}} \\\\",
  sprintf("\\quad %s & %s & %s & %s\\%% \\\\",
          tex_esc(cap(tex_rows$layer[tex_rows$grp == 2])),
          pn_int(tex_rows$n_fwd[tex_rows$grp == 2]),
          pn_int(tex_rows$n_both[tex_rows$grp == 2]),
          pn_pct(tex_rows$pct_recip[tex_rows$grp == 2]))))

pn$RecallRosterMedian <- pn_int(median(ego$n_roster))
pn$RecallCountMedian <- pn_int(median(ego$n_recalled))
pn$RecallRateMedianPct <- pn_pct(median(ego$recall_rate))
pn$RecallRateMeanPct <- pn_pct(mean(ego$recall_rate))
pn$RecallEgos <- pn_int(nrow(ego))
pn$RecallSlots <- pn_int(nrow(scan))
pn$TiedGivenRecalledPct <- pn_pct(mean(d$tied_any[d$recalled == 1L]))
pn$TiedDyadPct <- pn_pct(mean(d$tied_any))
pn$RecalledDyadPct <- pn_pct(mean(d$recalled))
pn$MutualPairs <- pn_int(nrow(mutual) / 2)
pn$RecipRecallPct <- pn_pct(t6b$pct_recip[t6b$layer == "Co-hire recall"])
pn$RecipReciprocalPct <- pn_pct(
  recip_mean$pct[recip_mean$expectation == "Expected reciprocal"])
pn$RecipDirectionalPct <- pn_pct(
  recip_mean$pct[recip_mean$expectation == "Expected directional"])
pn$RecipLayersShown <- pn_int(nrow(solid))
pn$RecallSlopePerTen <- pn_num(100 * 10 * coef(fit_lin)[["n_roster"]], 2)
pn$RecallSlopeP <- pn_num(pvalue(fit_lin)[["n_roster"]], 3)
pn$RecallCountSlopeP <- pn_num(pvalue(fit_cnt)[["n_roster"]], 3)
pn$RecallRosterMax <- pn_int(max(ego$n_roster))
pn$RecallRosterMin <- pn_int(min(ego$n_roster))
pn$RecallRosterQOne <- pn_int(t6c$median_roster[1])
pn$RecallRosterQFour <- pn_int(t6c$median_roster[nrow(t6c)])
pn$RecallCountQOne <- pn_num(t6c$mean_recalled[1], 1)
pn$RecallCountQFour <- pn_num(t6c$mean_recalled[nrow(t6c)], 1)
pn$RecallRateQOnePct <- pn_pct(t6c$mean_recall_rate[1])
pn$RecallRateQFourPct <- pn_pct(t6c$mean_recall_rate[nrow(t6c)])

### 4. MACROS -------------------------------------------------------
# The recall figures the prose cites, as \pn macros.

write_pn(pn, file.path(dir_pn, "pn_recall.tex"))

cat("\ntwo stages:\n"); print(as.data.frame(t6a))
cat("\nreciprocity by layer:\n"); print(as.data.frame(t6b))
cat("\npooled reciprocity:\n"); print(as.data.frame(recip_mean))
