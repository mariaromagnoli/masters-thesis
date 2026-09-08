# 21 analysis - channels.R
#
# Analysis (Specifications 1 to 5). The five channels through which shared
# origin could shape the co-hire network:
#   1  recall        - is a co-villager co-hire more likely to be recalled?
#   2  importation    - did co-villagers arrive already acquainted?
#   3  opportunity    - do co-villagers meet more once inside the Park?
#   4  taste          - within the firm, do co-villagers befriend each other more?
#   5  exclusion      - are minority workers named less by non-minority namers?
# Each is one or more regressions on the dyad samples; every printed number
# also goes to report_channels.txt.
#
# Feeds: Section 6 Results (Specifications 1 to 5).
#
# Input:  respondents.parquet, dyads.parquet, gn_edgelist.parquet,
#         04 latex/pn/pn_recall.tex
# Output: 20 analysis/table_channels.csv, report_channels.txt,
#         04 latex/tables/table_recall.tex, table_tie_formation.tex,
#         table_exclusion.tex, 04 latex/figures/fig_channels.pdf,
#         04 latex/pn/pn_channels.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The dyadic samples, and the report file every printed number also goes to.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "21 analysis - channels.R"

report <- file.path(dir_analysis, "report_channels.txt")
con <- file(report, open = "wt")
say <- function(...) {
  cat(..., "\n")
  cat(..., "\n", file = con)
}
sayp <- function(x) {
  print(x)
  capture.output(print(x)) |> writeLines(con)
}
hdr <- function(s) {
  say("\n========================================\n", s,
      "\n========================================")
}

resp <- read_parquet(file.path(dir_derived, "respondents.parquet"))
d_all <- read_parquet(file.path(dir_derived, "dyads.parquet"))
gn_all <- read_parquet(file.path(dir_derived, "gn_edgelist.parquet"))

sidama_code <- code_for("Sidama", get_choice_labels("regions"))

w <- resp %>%
  filter(!in_problem) %>%
  transmute(worker_id,
            n_roster,
            minority_region = as.integer(birth_region != sidama_code),
            minority_lang = ifelse(is.na(sid_speak_n), NA_integer_,
                                   as.integer(sid_speak_n <= 2)),
            res_kebele = ifelse(is_valid_code(res_kebele), res_kebele,
                                NA_character_),
            birth_woreda = ifelse(is_valid_code(birth_woreda), birth_woreda,
                                  NA_character_))

say("analysis workers:", nrow(w),
    "| minority by region:", sum(w$minority_region),
    "| minority by language:", sum(w$minority_lang, na.rm = TRUE),
    "| in BOTH:", sum(w$minority_region == 1 & w$minority_lang == 1,
                      na.rm = TRUE))

# Check the roster median agrees with the macro script 14 wrote.
pn_recall_tex <- readLines(file.path(dir_pn, "pn_recall.tex"))
med_recall <- as.numeric(gsub(".*\\\\pn\\{([0-9]+)\\}.*", "\\1",
                              grep("pnRecallRosterMedian", pn_recall_tex,
                                   value = TRUE)))
stopifnot(length(med_recall) == 1,
          median(w$n_roster, na.rm = TRUE) == med_recall)
say("n_roster median", med_recall, "agrees with \\pnRecallRosterMedian: OK")

d <- d_all %>%
  filter(origin_valid) %>%
  left_join(w %>% select(worker_id, m_i = minority_region,
                         mlang_i = minority_lang), by = "worker_id") %>%
  left_join(w %>% select(alter_id = worker_id, m_j = minority_region,
                         mlang_j = minority_lang), by = "alter_id")

say("origin-valid dyads:", nrow(d), "| egos:", n_distinct(d$worker_id),
    "| batches:", n_distinct(d$batch))
say("recalled:", sum(d$recalled), "| tied_any:", sum(d$tied_any),
    "| tied_pre:", sum(d$tied_pre), "| tied_post:", sum(d$tied_post))
say("same_woreda:", sum(d$same_woreda), "| of which tied_post:",
    sum(d$same_woreda == 1 & d$tied_post == 1),
    "| tied_pre:", sum(d$same_woreda == 1 & d$tied_pre == 1))
stopifnot(!anyNA(d$m_i), !anyNA(d$m_j))

d1 <- d %>% filter(tied_pre == 0)
say("y1 sample (pre-HIP-tied pairs dropped):", nrow(d1), "of", nrow(d))

# Pull one coefficient out of a model as a table row.
row_of <- function(m, v, label, spec) {
  b <- coef(m)
  s <- se(m)
  p <- pvalue(m)
  if (!v %in% names(b)) return(NULL)
  data.frame(spec = spec, term = label, b = unname(b[v]), se = unname(s[v]),
             p = unname(p[v]), n = nobs(m),
             mde = 2.8 * unname(s[v]))
}
collect <- function(...) do.call(rbind, Filter(Negate(is.null), list(...)))

hdr("1. RECALL -- equation (7)")
say("outcome: recalled. mean =", round(mean(d$recalled), 4))

### 2. ESTIMATION -----------------------------------------------------
# Specifications 1 to 5: recall, importation, opportunity, taste (within
# the firm and in the general network), and exclusion.

rc1 <- feols(recalled ~ same_woreda + same_pu_mi + pu_known + shared_lang +
               resurveyed_i,
             data = d, cluster = ~ worker_id + alter_id)
rc2 <- feols(recalled ~ same_woreda + same_pu_mi + pu_known + shared_lang |
               batch,
             data = d, cluster = ~ worker_id + alter_id)
sayp(etable(rc1, rc2, digits = 4))

say("\nth1 (shared origin) -- no FE:", round(coef(rc1)[["same_woreda"]], 5),
    "| batch FE:", round(coef(rc2)[["same_woreda"]], 5))

rc3 <- feols(recalled ~ same_woreda + same_pu_mi + pu_known + shared_lang |
               batch,
             data = d1, cluster = ~ worker_id + alter_id)
say("\nrecall | tied_pre == 1:", round(mean(d$recalled[d$tied_pre == 1]), 4),
    "(1 BY CONSTRUCTION) | tied_pre == 0:",
    round(mean(d$recalled[d$tied_pre == 0]), 4))
say("pre-tied share among same-woreda pairs:",
    round(mean(d$tied_pre[d$same_woreda == 1]), 4),
    "| among the rest:", round(mean(d$tied_pre[d$same_woreda == 0]), 4))
say("th1 all pairs:", round(coef(rc2)[["same_woreda"]], 5),
    "| strangers only:", round(coef(rc3)[["same_woreda"]], 5))
sayp(etable(rc2, rc3, digits = 4, headers = c("all pairs", "strangers only")))

tab_recall <- collect(
  row_of(rc2, "same_woreda", "shared origin (h)", "recall, batch FE"),
  row_of(rc2, "same_pu_mi", "shared unit (u)", "recall, batch FE"),
  row_of(rc2, "shared_lang", "shared language (s)", "recall, batch FE"),
  row_of(rc3, "same_woreda", "shared origin (h)",
         "recall, batch FE, strangers"),
  row_of(rc3, "same_pu_mi", "shared unit (u)", "recall, batch FE, strangers"),
  row_of(rc3, "shared_lang", "shared language (s)",
         "recall, batch FE, strangers"),
  row_of(rc1, "same_woreda", "shared origin (h)", "recall, no FE"),
  row_of(rc1, "same_pu_mi", "shared unit (u)", "recall, no FE"),
  row_of(rc1, "shared_lang", "shared language (s)", "recall, no FE"))

hdr("2. IMPORTATION -- equation (8)")
say("outcome: tied_pre. mean =", round(mean(d$tied_pre), 4),
    "| ties =", sum(d$tied_pre))

im1 <- feols(tied_pre ~ same_woreda, data = d, cluster = ~ worker_id + alter_id)
im2 <- feols(tied_pre ~ same_woreda + same_zone, data = d,
             cluster = ~ worker_id + alter_id)
im3 <- feols(tied_pre ~ same_woreda + same_zone | batch, data = d,
             cluster = ~ worker_id + alter_id)
sayp(etable(im1, im2, im3, digits = 4))

say("\nbase rate of an inherited tie:", round(mean(d$tied_pre), 4),
    "| same-woreda premium:", round(coef(im3)[["same_woreda"]], 4),
    "| as a multiple of base:",
    round(coef(im3)[["same_woreda"]] / mean(d$tied_pre), 2))

tab_import <- collect(
  row_of(im3, "same_woreda", "shared origin (h)", "importation, batch FE"),
  row_of(im3, "same_zone", "shared zone (z)", "importation, batch FE"),
  row_of(im2, "same_woreda", "shared origin (h)", "importation, no FE"),
  row_of(im1, "same_woreda", "shared origin (h), zone not controlled",
         "importation, no FE"))

hdr("3. OPPORTUNITY -- equation (9)")
say("outcome: tied_post on the y1 sample. mean =",
    round(mean(d1$tied_post), 4), "| ties =", sum(d1$tied_post))

op1 <- feols(tied_post ~ same_pu_mi + pu_known + shared_lang + resurveyed_i,
             data = d1, cluster = ~ worker_id + alter_id)
op2 <- feols(tied_post ~ same_pu_mi + pu_known + shared_lang | batch, data = d1,
             cluster = ~ worker_id + alter_id)
sayp(etable(op1, op2, digits = 4))

# Logit counterpart of an extensive-margin LPM, with average marginal effects.
lgt_cmp <- function(m, dat, lab) {
  g <- feglm(formula(m), data = dat, family = binomial(),
             cluster = ~ worker_id + alter_id)
  pr <- predict(g, type = "response")
  ame <- mean(pr * (1 - pr)) * coef(g)
  k <- names(ame)
  data.frame(spec = lab, term = k, lpm = 100 * coef(m)[k], logit = 100 * ame,
             p_lpm = pvalue(m)[k], p_lgt = pvalue(g)[k],
             n_lpm = nrow(dat), n_lgt = nobs(g))
}

op2_full <- feols(tied_post ~ same_pu_mi + pu_known + shared_lang | batch,
                  data = d, cluster = ~ worker_id + alter_id)
say("\nsame spec on the FULL sample (pre-HIP-tied pairs kept as zeros): u =",
    round(coef(op2_full)[["same_pu_mi"]], 5), "vs",
    round(coef(op2)[["same_pu_mi"]], 5))

base_post <- mean(d1$tied_post[d1$shared_lang == 1])
kappa_hat <- (base_post - coef(op2)[["shared_lang"]]) / base_post
say("tie rate with a shared language:", round(base_post, 5),
    "| implied kappa (ratio without a shared language to with):",
    round(kappa_hat, 3))

tab_opp <- collect(
  row_of(op2, "same_pu_mi", "shared unit (u)", "opportunity, batch FE"),
  row_of(op2, "shared_lang", "shared language (s)", "opportunity, batch FE"),
  row_of(op2_full, "same_pu_mi", "shared unit (u)",
         "opportunity, batch FE, full sample"))

hdr("4. TASTE WITHIN THE FIRM -- equation (10)")
say("same-woreda pairs on the y1 sample:", sum(d1$same_woreda),
    "| of which tied inside the Park:",
    sum(d1$same_woreda == 1 & d1$tied_post == 1))

ta1 <- feols(tied_post ~ same_woreda + same_pu_mi + pu_known + shared_lang |
               batch,
             data = d1, cluster = ~ worker_id + alter_id)
ta2 <- feols(tied_post ~ same_woreda + same_pu_mi + pu_known + shared_lang |
               worker_id + alter_id,
             data = d1, cluster = ~ worker_id + alter_id)
sayp(etable(ta1, ta2, digits = 4))

lgt_tab <- rbind(lgt_cmp(rc2, d, "recall"), lgt_cmp(im3, d, "importation"),
                 lgt_cmp(op2, d1, "opportunity"), lgt_cmp(ta1, d1, "taste"))
lgt_var <- tapply(d1$tied_post, d1$batch, function(y) length(unique(y)) > 1)
lgt_drop_n <- sum(d1$batch %in% names(lgt_var)[!lgt_var])
op2_kept <- feols(formula(op2),
                  data = d1[d1$batch %in% names(lgt_var)[lgt_var], ],
                  cluster = ~ worker_id + alter_id)
lgt_shift <- 100 * abs(coef(op2_kept)[["same_pu_mi"]] -
                       coef(op2)[["same_pu_mi"]])
say("\nlogit counterparts of the extensive-margin equations:")
sayp(transform(lgt_tab, lpm = round(lpm, 3), logit = round(logit, 3)))
say("sign flips:", sum(sign(lgt_tab$logit) != sign(lgt_tab$lpm)),
    "| 5% verdict flips:",
    sum((lgt_tab$p_lpm < 0.05) != (lgt_tab$p_lgt < 0.05)),
    "| largest gap in points:",
    round(max(abs(lgt_tab$logit - lgt_tab$lpm)), 3))

ta2z <- feols(tied_post ~ same_zone + same_pu_mi + pu_known + shared_lang |
                worker_id + alter_id,
              data = d1, cluster = ~ worker_id + alter_id)
say("\nsame-ZONE pairs on the y1 sample:", sum(d1$same_zone),
    "| of which tied inside the Park:",
    sum(d1$same_zone == 1 & d1$tied_post == 1),
    "| against", sum(d1$same_woreda), "same-woreda pairs and",
    sum(d1$same_woreda == 1 & d1$tied_post == 1), "ties")
sayp(etable(ta2, ta2z, digits = 4, headers = c("woreda", "zone")))
say("zone MDE at 80% power =", round(2.8 * se(ta2z)[["same_zone"]], 5),
    "| woreda MDE =", round(2.8 * se(ta2)[["same_woreda"]], 5),
    "| is the zone design better powered?",
    se(ta2z)[["same_zone"]] < se(ta2)[["same_woreda"]])

tau <- coef(ta2)[["same_woreda"]]
tau_se <- se(ta2)[["same_woreda"]]
say("\ntau =", round(tau, 5), "| se =", round(tau_se, 5),
    "| MDE at 80% power =", round(2.8 * tau_se, 5),
    "| base rate =", round(mean(d1$tied_post), 5))
say("MDE as a multiple of the base tie rate:",
    round(2.8 * tau_se / mean(d1$tied_post), 2))
say("importation premium (eq 8, batch FE), for scale:",
    round(coef(im3)[["same_woreda"]], 5),
    "| is the taste MDE larger than it?",
    2.8 * tau_se > coef(im3)[["same_woreda"]])

tab_taste_ch <- collect(
  row_of(ta2, "same_woreda", "shared origin (h) = tau",
         "taste co-hire, ego+alter FE"),
  row_of(ta2, "same_pu_mi", "shared unit (u)", "taste co-hire, ego+alter FE"),
  row_of(ta2, "shared_lang", "shared language (s)",
         "taste co-hire, ego+alter FE"),
  row_of(ta1, "same_woreda", "shared origin (h) = tau",
         "taste co-hire, batch FE"),
  row_of(ta2z, "same_zone", "shared zone (z), woreda NOT controlled",
         "taste co-hire, ego+alter FE, zone"),
  row_of(ta2z, "same_pu_mi", "shared unit (u)",
         "taste co-hire, ego+alter FE, zone"),
  row_of(ta2z, "shared_lang", "shared language (s)",
         "taste co-hire, ego+alter FE, zone"))

hdr("5. TASTE IN THE GENERAL NETWORK -- equation (11)")

gn <- gn_all %>%
  mutate(same_woreda = case_when(origin == "2" ~ TRUE,
                                 origin == "3" ~ orig_woreda == birth_woreda_i,
                                 origin == "1" ~ FALSE,
                                 TRUE ~ NA),
         post = as.integer(pre_hip == "0")) %>%
  filter(!is.na(same_woreda), !is.na(post)) %>%
  mutate(v = as.integer(same_woreda)) %>%
  left_join(resp %>% filter(!in_problem) %>%
              transmute(worker_id,
                        res_kebele = ifelse(is_valid_code(res_kebele),
                                            res_kebele, NA_character_),
                        bw = ifelse(is_valid_code(birth_woreda), birth_woreda,
                                    NA_character_)),
            by = "worker_id")

say("general-network alters:", nrow(gn), "| formed after arrival:",
    sum(gn$post), "| co-villagers:", sum(gn$v))
say("co-villager share among alters brought with her:",
    round(mean(gn$v[gn$post == 0]), 4),
    "| among those acquired after arrival:", round(mean(gn$v[gn$post == 1]), 4))

vary <- gn %>%
  group_by(worker_id) %>%
  summarise(n = n(), np = sum(post), .groups = "drop") %>%
  filter(np > 0, np < n)
say("respondents with BOTH kinds of alter (the FE-identified sample):",
    nrow(vary), "workers,", sum(vary$n), "alters")

gt1 <- feols(v ~ post, data = gn, cluster = ~ worker_id)
gt2 <- feols(v ~ post | worker_id, data = gn, cluster = ~ worker_id)
sayp(etable(gt1, gt2, digits = 4))

# Kebele co-residence measure: share of a worker's kebele from her own woreda.
kb <- resp %>%
  filter(!in_problem) %>%
  transmute(worker_id,
            k = ifelse(is_valid_code(res_kebele), res_kebele, NA_character_),
            bw = ifelse(is_valid_code(birth_woreda), birth_woreda,
                        NA_character_)) %>%
  filter(!is.na(k), !is.na(bw))
kcomp <- kb %>%
  left_join(kb %>% select(k, other = worker_id, other_bw = bw), by = "k",
            relationship = "many-to-many") %>%
  filter(other != worker_id) %>%
  group_by(worker_id) %>%
  summarise(kebele_n = n(),
            kebele_own_woreda = mean(other_bw == bw), .groups = "drop")
say("\nkebele composition built for", nrow(kcomp), "workers | median pool size",
    median(kcomp$kebele_n), "| mean own-woreda share",
    round(mean(kcomp$kebele_own_woreda), 4))

gnk <- gn %>% inner_join(kcomp, by = "worker_id")
say("general-network alters with a kebele measure:", nrow(gnk))

gk_lvl_nofe <- feols(v ~ post + kebele_own_woreda, data = gnk,
                     cluster = ~ worker_id)
gk_lvl_fe <- feols(v ~ post + kebele_own_woreda | worker_id, data = gnk,
                   cluster = ~ worker_id)
say("\nlevel control, no FE: kebele coefficient =",
    round(coef(gk_lvl_nofe)[["kebele_own_woreda"]], 4))
say("level control, WITH respondent FE: is it estimable?",
    "kebele_own_woreda" %in% names(coef(gk_lvl_fe)),
    "-- collinear with alpha_i, as the header note says")

gk_int <- feols(v ~ post + post:kebele_own_woreda | worker_id, data = gnk,
                cluster = ~ worker_id)
sayp(etable(gt2, gk_int, digits = 4))

floor_share <- mean(d$same_woreda)
say("\nfloor: share of firm-assembled co-hire pairs sharing a woreda =",
    round(floor_share, 4))
say("co-villager share among alters acquired after arrival =",
    round(mean(gn$v[gn$post == 1]), 4),
    "| ratio to the floor:", round(mean(gn$v[gn$post == 1]) / floor_share, 2))
say("co-villager share among alters brought with her =",
    round(mean(gn$v[gn$post == 0]), 4),
    "| ratio to the floor:", round(mean(gn$v[gn$post == 0]) / floor_share, 2))

tab_taste_gn <- collect(
  row_of(gt1, "post", "formed after arrival = xi1", "taste GN, no FE"),
  row_of(gt2, "post", "formed after arrival = xi1", "taste GN, respondent FE"),
  row_of(gk_lvl_nofe, "kebele_own_woreda", "kebele own-woreda share",
         "taste GN, no FE + kebele level"),
  row_of(gk_int, "post", "formed after arrival = xi1",
         "taste GN, FE + kebele interaction"),
  row_of(gk_int, "post:kebele_own_woreda", "post x kebele own-woreda share",
         "taste GN, FE + kebele interaction"))

hdr("6. EXCLUSION -- equation (12)")

ex_a5 <- feols(tied_post ~ same_woreda + same_pu_mi + pu_known + shared_lang +
                 m_j * m_i | batch, data = d, cluster = ~ worker_id + alter_id)
ex_a6 <- feols(tied_post ~ same_woreda + same_pu_mi + pu_known + shared_lang +
                 mlang_j * mlang_i | batch, data = d,
               cluster = ~ worker_id + alter_id)
say("\nnamer-split exclusion: delta applies only when the namer is not a minority")
sayp(etable(ex_a5, ex_a6, digits = 4))
delta_out <- -coef(ex_a5)[["m_j"]]
delta_in <- -(coef(ex_a5)[["m_j"]] + coef(ex_a5)[["m_j:m_i"]])
say("delta_out (non-minority namers) =", round(delta_out, 5),
    "| se", round(se(ex_a5)[["m_j"]], 5),
    "| p", round(pvalue(ex_a5)[["m_j"]], 4),
    "| MDE", round(2.8 * se(ex_a5)[["m_j"]], 5))
say("delta_in (minority namers) =", round(delta_in, 5),
    "| interaction p", round(pvalue(ex_a5)[["m_j:m_i"]], 4))
say("pairs with a non-minority namer and a minority receiver:",
    sum(d$m_i == 0 & d$m_j == 1, na.rm = TRUE),
    "| minority namer, minority receiver:",
    sum(d$m_i == 1 & d$m_j == 1, na.rm = TRUE))

# Differenced (within unordered pair) version of the exclusion test.
pairs <- d %>%
  mutate(a = pmin(worker_id, alter_id), b = pmax(worker_id, alter_id)) %>%
  select(a, b, worker_id, alter_id, tied_any, tied_post, m_i, m_j,
         mlang_i, mlang_j)
fwd <- pairs %>%
  filter(worker_id == a) %>%
  select(a, b, y_ab = tied_any, y1_ab = tied_post, m_a = m_i, m_b = m_j,
         ml_a = mlang_i, ml_b = mlang_j)
rev <- pairs %>%
  filter(worker_id == b) %>%
  select(a, b, y_ba = tied_any, y1_ba = tied_post)
dif <- inner_join(fwd, rev, by = c("a", "b")) %>%
  mutate(dy = y_ab - y_ba, dy1 = y1_ab - y1_ba, dm = m_b - m_a,
         dml = ml_b - ml_a)
say("\nunordered pairs with both directions observed:", nrow(dif),
    "| asymmetric on any tie:", sum(dif$dy != 0),
    "| asymmetric on a post-HIP tie:", sum(dif$dy1 != 0))
say("pairs where the two members differ in minority status:",
    sum(dif$dm != 0, na.rm = TRUE))

ex_b1 <- feols(dy ~ dm, data = dif, vcov = "hetero")
ex_b2 <- feols(dy1 ~ dm, data = dif, vcov = "hetero")
ex_b3 <- feols(dy ~ dml, data = dif, vcov = "hetero")
sayp(etable(ex_b1, ex_b2, ex_b3, digits = 4))
say("\ndelta (any tie, region definition) =", round(-coef(ex_b1)[["dm"]], 5),
    "| se", round(se(ex_b1)[["dm"]], 5),
    "| MDE", round(2.8 * se(ex_b1)[["dm"]], 5))
say("positive delta = minority workers are named less by non-minority namers.")

tab_excl <- collect(
  row_of(ex_a5, "m_j", "receiver minority, namer not (region)",
         "exclusion (a), namer split"),
  row_of(ex_a5, "m_j:m_i", "receiver x namer both minority (region)",
         "exclusion (a), namer split"),
  row_of(ex_a6, "mlang_j", "receiver minority, namer not (language)",
         "exclusion (a), namer split"),
  row_of(ex_a6, "mlang_j:mlang_i", "receiver x namer both minority (language)",
         "exclusion (a), namer split"),
  row_of(ex_b1, "dm", "minority difference (region)",
         "exclusion (b), differenced, any tie"),
  row_of(ex_b2, "dm", "minority difference (region)",
         "exclusion (b), differenced, post-HIP tie"),
  row_of(ex_b3, "dml", "minority difference (language)",
         "exclusion (b), differenced, any tie"))

### 3. OUTPUT -------------------------------------------------------
# One CSV row per estimate, and the plain-text report.

all_tab <- rbind(
  cbind(channel = "recall (7)", tab_recall),
  cbind(channel = "importation (8)", tab_import),
  cbind(channel = "opportunity (9)", tab_opp),
  cbind(channel = "taste, co-hire (10)", tab_taste_ch),
  cbind(channel = "taste, general network (11)", tab_taste_gn),
  cbind(channel = "exclusion (12)", tab_excl))
all_tab[, c("b", "se", "p", "mde")] <-
  lapply(all_tab[, c("b", "se", "p", "mde")], function(x) round(x, 6))
write.csv(all_tab, file.path(dir_analysis, "table_channels.csv"),
          row.names = FALSE)

hdr("SUMMARY -- every reported coefficient")
sayp(all_tab)

### 4. LATEX ---------------------------------------------------------
# Three tables; columns are named by specification rather than numbered.

cell <- function(col, term) {
  tm <- if (is.null(col$tm)) term else col$tm
  b <- coef(col$m)
  if (!tm %in% names(b)) return(c("---", ""))
  v <- col$sgn * unname(b[tm])
  s <- unname(se(col$m)[tm])
  p <- unname(pvalue(col$m)[tm])
  c(paste0(pn_num(v, 3), star(p)), paste0("(", pn_num(s, 3), ")"))
}
row_tex <- function(cols, label, term) {
  cs <- lapply(cols, cell, term = term)
  if (all(sapply(cs, `[`, 1) == "---")) return(character(0))
  c(sprintf("%s & %s \\\\", label, paste(sapply(cs, `[`, 1), collapse = " & ")),
    sprintf(" & %s \\\\", paste(sapply(cs, `[`, 2), collapse = " & ")))
}
mde_of <- function(col, term) pn_num(2.8 * unname(se(col$m)[term]), 3)

bs <- c(recall_all = mean(d$recalled), recall_str = mean(d1$recalled),
        pre = mean(d$tied_pre), post = mean(d1$tied_post))

emit <- function(cols, rows, tail, file, width = 16) {
  write_tabular(file, paste0("l", strrep("c", length(cols))),
                tex_header(sapply(cols, `[[`, "hd"), width = width),
                rows, tail)
}

c_rec <- list(list(m = rc2, sgn = 1, hd = "1, including inherited"),
              list(m = rc3, sgn = 1, hd = "1, dropping inherited"))
emit(c_rec,
  c(row_tex(c_rec, "Shared origin", "same_woreda"),
    row_tex(c_rec, "Same production unit", "same_pu_mi"),
    row_tex(c_rec, "Shared language", "shared_lang")),
  c(span("Fixed effects", rep("batch", 2)),
    span("Observations", sapply(c_rec, function(k) pn_int(nobs(k$m)))),
    span("Mean of the outcome",
         pn_num(unname(bs[c("recall_all", "recall_str")]), 3))),
  "table_recall.tex", width = 40)

c_tie <- list(list(m = im3,  sgn = 1, hd = "2, inherited ties"),
              list(m = op2,  sgn = 1, hd = "3, new ties"),
              list(m = ta2,  sgn = 1, hd = "4, new ties, same woreda"),
              list(m = ta2z, sgn = 1, hd = "4, new ties, same zone"))
emit(c_tie,
  c(row_tex(c_tie, "Shared origin", "same_woreda"),
    row_tex(c_tie, "Shared zone", "same_zone"),
    row_tex(c_tie, "Same production unit", "same_pu_mi"),
    row_tex(c_tie, "Shared language", "shared_lang")),
  c(span("Sample", c("all pairs", "strangers", "strangers", "strangers")),
    span("Fixed effects",
         c("batch", "batch", "worker, alter", "worker, alter")),
    span("Observations", sapply(c_tie, function(k) pn_int(nobs(k$m)))),
    span("Mean of the outcome",
         pn_num(unname(bs[c("pre", "post", "post", "post")]), 3)),
    span("MDE (80\\% power)", c("---", "---", mde_of(c_tie[[3]], "same_woreda"),
                  mde_of(c_tie[[4]], "same_zone")))),
  "table_tie_formation.tex")

c_exc <- list(list(m = ex_a5, sgn = -1, tm = "m_j",     hd = "region"),
              list(m = ex_a6, sgn = -1, tm = "mlang_j", hd = "language"))
c_exc_int <- Map(function(k, t) modifyList(k, list(tm = t, sgn = -1)), c_exc,
                 list("m_j:m_i", "mlang_j:mlang_i"))
emit(c_exc,
  c(row_tex(c_exc, "Receiver is minority", "m_j"),
    row_tex(c_exc_int, "$\\times$ namer is minority too", "")),
  c(span("Estimated on", c("dyads", "dyads")),
    span("Fixed effects", c("batch", "batch")),
    span("Observations", sapply(c_exc, function(k) pn_int(nobs(k$m)))),
    span("MDE (80\\% power)", sapply(c_exc, function(k) mde_of(k, k$tm)))),
  "table_exclusion.tex", width = 14)

col_green <- "#a8d5a2"
col_purple <- "#e0b0FF"
theme_set(theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

term_labs <- c(same_woreda = "Shared origin", same_zone = "Shared zone",
               same_pu_mi = "Same production unit",
               shared_lang = "Shared language")

lgt_lab <- c(same_woreda = "Shared origin", same_zone = "Shared zone",
             same_pu_mi = "Same production unit",
             pu_known = "Production unit recorded",
             shared_lang = "Shared language")
lgt_head <- c(recall = "Specification 1, recall",
              importation = "Specification 2, inherited ties",
              opportunity = "Specification 3, new ties",
              taste = "Specification 4, new ties")
rows_lgt <- unlist(lapply(seq_along(lgt_head), function(i) {
  x <- lgt_tab[lgt_tab$spec == names(lgt_head)[i], ]
  c(if (i > 1) "\\midrule",
    sprintf("\\textit{%s} & %s & %s \\\\", lgt_head[i],
            pn_int(x$n_lpm[1]), pn_int(x$n_lgt[1])),
    sprintf("\\quad %s & %s & %s \\\\", lgt_lab[x$term],
            paste0(pn_num(x$lpm, 2), stars(x$p_lpm)),
            paste0(pn_num(x$logit, 2), stars(x$p_lgt))))
}))

write_tabular("table_logit.tex", "lcc",
  "\\textit{equation and regressor} & \\textit{LPM} & \\textit{logit} \\\\",
  rows_lgt,
  caption = "Does the functional form matter? Linear against logit",
  label = "tab:logit",
  note = "Entries are percentage points. Stars mark 10, 5 and 1 percent.")

### 5. FIGURE ------------------------------------------------------
# Shared origin against three different outcomes, on one common axis.

panel_of <- function(m, spec, base, terms) {
  terms <- intersect(terms, names(coef(m)))
  data.frame(spec = spec, term = unname(term_labs[terms]),
             b = unname(coef(m)[terms]), se = unname(se(m)[terms]),
             base = base, stringsAsFactors = FALSE)
}
fig_dat <- rbind(
  panel_of(rc2,
           sprintf("Recalled, all pairs (Spec. 1, mean %s)",
                   pn_num(bs[["recall_all"]], 3)),
           bs[["recall_all"]], names(term_labs)),
  panel_of(rc3,
           sprintf("Recalled, strangers only (Spec. 1, mean %s)",
                   pn_num(bs[["recall_str"]], 3)),
           bs[["recall_str"]], names(term_labs)),
  panel_of(im3,
           sprintf("Arrived acquainted (Spec. 2, mean %s)",
                   pn_num(bs[["pre"]], 3)),
           bs[["pre"]], names(term_labs)),
  panel_of(ta2,
           sprintf("Tied inside the Park (Spec. 4, mean %s)",
                   pn_num(bs[["post"]], 3)),
           bs[["post"]], names(term_labs)))
fig_dat$spec <- factor(fig_dat$spec, levels = unique(fig_dat$spec))
fig_dat$term <- factor(fig_dat$term, levels = rev(unname(term_labs)))
fig_dat$grp <- ifelse(fig_dat$term == "Shared origin", "Shared origin",
                      "Other regressors")

p <- ggplot(fig_dat, aes(x = b, y = term, colour = grp)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = b - 1.96 * se, xmax = b + 1.96 * se),
                orientation = "y", width = 0, linewidth = 0.8) +
  geom_point(size = 2.4) +
  facet_wrap(~ spec, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c(`Shared origin` = col_purple,
                                 `Other regressors` = col_green), name = NULL) +
  labs(x = "Change in the probability of the outcome", y = NULL)
ggsave(file.path(dir_fig, "fig_channels.pdf"), p, width = 7.0, height = 5.6)
cat("wrote fig_channels\n")

# One coefficient (with se and p), optionally sign-flipped.
gv <- function(m, term, sgn = 1) {
  c(b = sgn * unname(coef(m)[term]),
    se = unname(se(m)[term]),
    p = unname(pvalue(m)[term]))
}
v_rec_h <- gv(rc2, "same_woreda")
v_rec_u <- gv(rc2, "same_pu_mi")
v_rec_l <- gv(rc2, "shared_lang")
v_str_h <- gv(rc3, "same_woreda")
v_str_u <- gv(rc3, "same_pu_mi")
v_str_l <- gv(rc3, "shared_lang")
v_imp_h <- gv(im3, "same_woreda")
v_imp_z <- gv(im3, "same_zone")
v_opp_u <- gv(op2, "same_pu_mi")
v_opp_l <- gv(op2, "shared_lang")
v_tas_h <- gv(ta2, "same_woreda")
v_tas_u <- gv(ta2, "same_pu_mi")
v_tas_l <- gv(ta2, "shared_lang")
v_tas_z <- gv(ta2z, "same_zone")
v_ex_dif <- gv(ex_b1, "dm", -1)

lpm_fit <- lapply(list(rc2, im3, op2, ta1), predict)
lpm_out <- max(sapply(lpm_fit, function(f) mean(f < 0 | f > 1)))
lpm_dev <- max(sapply(lpm_fit, function(f) max(pmax(-f, f - 1, 0))))

pn <- list(
  ChanLpmOutsidePct = pn_pct(lpm_out),
  ChanLogitBatches = pn_int(length(lgt_var)),
  ChanLogitDropBatches = pn_int(sum(!lgt_var)),
  ChanLogitSpecs = pn_int(n_distinct(lgt_tab$spec)),
  ChanLogitDropDyads = pn_int(lgt_drop_n),
  ChanLogitDropShiftPp = pn_num(lgt_shift, 2),
  ChanLogitSignFlips = pn_word(sum(sign(lgt_tab$logit) != sign(lgt_tab$lpm))),
  ChanLogitVerdictFlips = pn_word(sum((lgt_tab$p_lpm < 0.05) !=
                                      (lgt_tab$p_lgt < 0.05))),
  ChanLogitMaxGapPp = pn_num(max(abs(lgt_tab$logit - lgt_tab$lpm)), 1),
  ChanLogitUnitPp = pn_num(lgt_tab$logit[lgt_tab$spec == "opportunity" &
                                         lgt_tab$term == "same_pu_mi"], 1),
  ChanLogitLangPp = pn_num(lgt_tab$logit[lgt_tab$spec == "opportunity" &
                                         lgt_tab$term == "shared_lang"], 1),
  ChanLpmMaxDevPp = pn_num(100 * lpm_dev, 1),
  ChanNewN = pn_int(nrow(d1)),
  ChanRecallOriginStrPp = pn_pct(v_str_h["b"], 1),
  ChanRecallPreTied = pn_int(sum(d$tied_pre)),
  ChanRecallPreWoredaPct = pn_pct(mean(d$tied_pre[d$same_woreda == 1]), 1),
  ChanRecallPreOtherPct = pn_pct(mean(d$tied_pre[d$same_woreda == 0]), 1),
  ChanImportMult = pn_num(v_imp_h["b"] / bs[["pre"]], 1),
  ChanImportOriginRel = pn_num((bs[["pre"]] + v_imp_h["b"]) / bs[["pre"]], 1),
  ChanOppTies = pn_int(sum(d1$tied_post)),
  ChanOppUnitRel = pn_num((bs[["post"]] + v_opp_u["b"]) / bs[["post"]], 1),
  ChanOppKappa = pn_num(kappa_hat, 2),
  ChanTasteTau = pn_num(v_tas_h["b"], 3),
  ChanTasteMde = pn_num(2.8 * v_tas_h["se"], 3),
  ChanTasteTies = pn_word(sum(d1$same_woreda == 1 & d1$tied_post == 1)),
  ChanTasteOriginPairs = pn_int(sum(d1$same_woreda)),
  ChanTasteMdeShare = pn_num(2.8 * v_tas_h["se"] / v_imp_h["b"], 2),
  ChanTasteZonePairs = pn_int(sum(d1$same_zone)),
  ChanTasteZoneTies = pn_int(sum(d1$same_zone == 1 & d1$tied_post == 1)),
  ChanExclLangOutPairs = pn_int(sum(d$mlang_i == 0 & d$mlang_j == 1,
                                    na.rm = TRUE)),
  ChanExclLangInPairs = pn_int(sum(d$mlang_i == 1 & d$mlang_j == 1,
                                   na.rm = TRUE)),
  ChanMinorityRegionN = pn_int(sum(w$minority_region)),
  ChanMinorityLangN = pn_int(sum(w$minority_lang, na.rm = TRUE)),
  ChanMinorityBothN = pn_int(sum(w$minority_region == 1 & w$minority_lang == 1,
                                 na.rm = TRUE)),
  ChanMinorityEitherN = pn_int(sum(w$minority_region == 1 |
                                   w$minority_lang == 1, na.rm = TRUE)),
  ChanRecallOriginPp = pn_pct(v_rec_h["b"], 1),
  ChanRecallBasePct = pn_pct(bs[["recall_all"]], 1),
  ChanRecallUnitStrPp = pn_pct(v_str_u["b"], 1),
  ChanRecallLangStrGain = pn_pct(v_str_l["b"], 1),
  ChanImportOriginPp = pn_pct(v_imp_h["b"], 1),
  ChanImportBasePct = pn_pct(bs[["pre"]], 1),
  ChanOppUnitPp = pn_pct(v_opp_u["b"], 1),
  ChanOppLangGain = pn_pct(v_opp_l["b"], 1),
  ChanOppBasePct = pn_pct(bs[["post"]], 1),
  ChanOppSharedLangPct = pn_pct(base_post, 1),
  ChanTasteTauPp = pn_pct(v_tas_h["b"], 1),
  ChanTasteMdePp = pn_pct(2.8 * v_tas_h["se"], 1),
  ChanTasteZonePp = pn_pct(v_tas_z["b"], 1),
  ChanExclDeltaOut = pn_num(delta_out, 3),
  ChanExclDeltaOutPp = pn_pct(delta_out, 1),
  ChanExclDeltaOutMdePp = pn_pct(2.8 * se(ex_a5)[["m_j"]], 1),
  ChanExclDeltaOutP = pn_num(pvalue(ex_a5)[["m_j"]], 3),
  ChanExclDeltaIn = pn_num(delta_in, 3),
  ChanExclSplitP = pn_num(pvalue(ex_a5)[["m_j:m_i"]], 3),
  ChanExclOutPairs = pn_int(sum(d$m_i == 0 & d$m_j == 1, na.rm = TRUE)),
  ChanExclInPairs = pn_int(sum(d$m_i == 1 & d$m_j == 1, na.rm = TRUE)),
  ChanExclLangDeltaOut = pn_num(-coef(ex_a6)[["mlang_j"]], 3),
  ChanExclLangDeltaOutPp = pn_pct(-coef(ex_a6)[["mlang_j"]], 1),
  ChanExclLangDeltaOutP = pn_num(pvalue(ex_a6)[["mlang_j"]], 3),
  ChanExclLangSplitP = pn_num(pvalue(ex_a6)[["mlang_j:mlang_i"]], 3),
  ChanExclLangDeltaIn = pn_num(-(coef(ex_a6)[["mlang_j"]] +
                                 coef(ex_a6)[["mlang_j:mlang_i"]]), 3))

### 6. MACROS ------------------------------------------------------
# The Results figures for Specifications 1 to 5, as \pn macros.

write_pn(pn, file.path(dir_pn, "pn_channels.tex"))

close(con)
cat("\nwrote", file.path(dir_analysis, "table_channels.csv"), "\n")
cat("wrote", report, "\n")
