# 20 analysis - balance.R
#
# Analysis. Tests whether a worker's characteristics are related to the
# composition of her co-hires - the assumption behind treating co-hire
# assignment as quasi-random. One regression per characteristic on the
# leave-out urban share of the co-hire set, a batch-membership omnibus
# test, and a check of whether co-hires are drawn from the worker's own
# birth woreda more than the firm pool would give.
#
# Feeds: Appendix C (quasi-random assignment); Section 5 opens by citing
#        this appendix's verdict.
#
# Input:  respondents.parquet, dyads.parquet, batch_crosswalk.csv
# Output: 20 analysis/table_balance.csv, table_balance.txt
#         04 latex/tables/table_balance.tex, table_omnibus.tex,
#         table_woreda.tex, 04 latex/pn/pn_balance.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# Worker-level characteristics, and each worker's "treatment": the mean
# urban share of life among her co-hires, computed leaving her out.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "20 analysis - balance.R"

resp <- read_parquet(file.path(dir_derived, "respondents.parquet"))
d    <- read_parquet(file.path(dir_derived, "dyads.parquet"))
xw   <- read.csv(file.path(dir_derived, "batch_crosswalk.csv"),
                 stringsAsFactors = FALSE)

sidama_code <- code_for("Sidama", get_choice_labels("regions"))

chars <- resp %>%
  filter(!in_problem) %>%
  transmute(worker_id,
            age          = resp_age_n,
            female       = as.integer(resp_gender == "2"),
            education    = education_n,
            educ_grade10 = educ_grade10,
            married      = married,
            has_children = has_children_n,
            hh_size      = hh_size_n,
            born_sidama  = ifelse(is_valid_code(birth_region),
                                  as.integer(birth_region == sidama_code),
                                  NA_integer_),
            sidamigna    = sid_speak_n,
            amharic      = amh_speak_n,
            urban        = 1 - rural_share,
            never_rural  = as.integer(lived_rural == "0"),
            woreda       = ifelse(is_valid_code(birth_woreda), birth_woreda,
                                  NA_character_))
modal_woreda <- names(sort(table(chars$woreda), decreasing = TRUE))[1]
chars <- chars %>%
  mutate(born_modal_woreda = as.integer(woreda == modal_woreda))
cat("modal birth woreda:", modal_woreda, "|",
    sum(chars$born_modal_woreda, na.rm = TRUE), "workers\n")
cat("gender codes present:",
    paste(sort(unique(resp$resp_gender)), collapse = ", "),
    "| share coded 2:", round(mean(resp$resp_gender == "2", na.rm = TRUE), 3),
    "\n")

own <- d %>%
  distinct(worker_id, batch) %>%
  left_join(xw %>% filter(in_sample) %>%
              select(batch, firm, rowset, size = rostered),
            by = "batch")

members <- own %>%
  distinct(batch, rowset) %>%
  mutate(member = strsplit(rowset, "|", fixed = TRUE)) %>%
  tidyr::unnest(member) %>%
  select(batch, member)
w <- own %>%
  left_join(members, by = "batch", relationship = "many-to-many") %>%
  filter(member != worker_id) %>%
  left_join(chars %>% select(member = worker_id, m_urban = urban),
            by = "member") %>%
  group_by(worker_id, batch, firm, size) %>%
  summarise(n_typed  = sum(!is.na(m_urban)),
            lo_urban = mean(m_urban, na.rm = TRUE), .groups = "drop") %>%
  left_join(chars, by = "worker_id")

pool <- chars %>%
  left_join(own %>% select(worker_id, firm), by = "worker_id") %>%
  filter(!is.na(firm), !is.na(urban)) %>%
  group_by(firm) %>%
  mutate(pool_lo_urban = (sum(urban) - urban) / (n() - 1)) %>%
  ungroup() %>%
  select(worker_id, pool_lo_urban)
w <- w %>% left_join(pool, by = "worker_id")

cat("\nbalance sample:", nrow(w), "workers |", n_distinct(w$batch),
    "batches\n")
cat("treatment (leave-out urban share of co-hires): mean",
    round(mean(w$lo_urban, na.rm = TRUE), 3),
    "sd", round(sd(w$lo_urban, na.rm = TRUE), 3),
    "range", paste(round(range(w$lo_urban, na.rm = TRUE), 3), collapse = "-"),
    "\n")
cat("typed co-hires behind each worker's treatment: median",
    median(w$n_typed), "min", min(w$n_typed), "\n")

outcomes <- c(age = "Age", female = "Female",
              education = "Education (1--12)",
              educ_grade10 = "Grade 10 or above",
              married = "Married", has_children = "Has children",
              hh_size = "Household size",
              born_sidama = "Born in Sidama",
              born_modal_woreda = "Born in modal woreda",
              sidamigna = "Sidamigna fluency (1--5)",
              amharic = "Amharic fluency (1--5)",
              urban = "Share of life urban",
              never_rural = "Never lived rural")

### 2. ESTIMATION -----------------------------------------------------
# For each characteristic: a regression on the treatment (with and
# without firm FE) plus a randomisation-inference p-value; then the batch
# omnibus F-test; then whether co-hires over-represent the worker's woreda.

fit_one <- function(v) {
  dat <- w %>% filter(!is.na(.data[[v]]), !is.na(lo_urban))
  f1 <- feols(as.formula(paste(v, "~ lo_urban")), dat, cluster = ~batch)
  f2 <- feols(as.formula(paste(v, "~ lo_urban | firm")), dat, cluster = ~batch)
  s1 <- summary(f1)$coeftable["lo_urban", ]
  s2 <- summary(f2)$coeftable["lo_urban", ]
  sdy <- sd(dat[[v]], na.rm = TRUE)
  sdx <- sd(dat$lo_urban, na.rm = TRUE)
  data.frame(
    outcome = outcomes[[v]], var = v, n = nrow(dat),
    mean_y = mean(dat[[v]], na.rm = TRUE), sd_y = sdy,
    b = s1[1], se = s1[2], p = s1[4],
    beta_std = s1[1] * sdx / sdy,
    mde_std = 2.8 * s1[2] * sdx / sdy,
    b_fe = s2[1], se_fe = s2[2], p_fe = s2[4])
}
tab <- bind_rows(lapply(names(outcomes), fit_one))

set.seed(11)
bt <- w %>%
  filter(!is.na(lo_urban)) %>%
  group_by(batch) %>%
  summarise(bt = mean(lo_urban), .groups = "drop")
ri_one <- function(v) {
  dat <- w %>% filter(!is.na(.data[[v]]), !is.na(lo_urban))
  y <- dat[[v]]
  obs <- abs(cov(dat$lo_urban, y) / var(dat$lo_urban))
  key <- as.character(bt$batch)
  null <- replicate(n_perm_draws, {
    xp <- setNames(sample(bt$bt), key)[as.character(dat$batch)]
    abs(cov(xp, y) / var(xp))
  })
  perm_p(null, obs)
}
tab$p_ri <- vapply(names(outcomes), ri_one, numeric(1))

# One-way ANOVA F-statistic of y on the grouping g (batch), computed by
# hand so the permutation null can reuse it.
f_stat <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]
  g <- factor(g[ok])
  n <- length(y)
  G <- nlevels(g)
  gm <- tapply(y, g, mean)
  gn <- tapply(y, g, length)
  ssb <- sum(gn * (gm - mean(y))^2)
  ssw <- sum((y - gm[as.character(g)])^2)
  c(F = (ssb / (G - 1)) / (ssw / (n - G)), df1 = G - 1, df2 = n - G, n = n)
}
set.seed(7)
omni <- bind_rows(lapply(names(outcomes), function(v) {
  y <- w[[v]]
  g <- w$batch
  st <- f_stat(y, g)
  null <- replicate(n_perm_draws, f_stat(y, sample(g))["F"])
  data.frame(outcome = outcomes[[v]], var = v, n = st[["n"]],
             batches = st[["df1"]] + 1, F = st[["F"]],
             p_analytic = pf(st[["F"]], st[["df1"]], st[["df2"]],
                             lower.tail = FALSE),
             p_perm = perm_p(null, st[["F"]]))
}))

wtab <- w %>% filter(!is.na(woreda))
chi <- function(a, b) suppressWarnings(chisq.test(table(a, b))$statistic)
chi_obs <- chi(wtab$batch, wtab$woreda)
chi_null <- replicate(n_perm_draws, chi(sample(wtab$batch), wtab$woreda))
omni <- bind_rows(omni, data.frame(
  outcome = "Woreda of origin (all levels)", var = "woreda", n = nrow(wtab),
  batches = n_distinct(wtab$batch), F = NA_real_, p_analytic = NA_real_,
  p_perm = perm_p(chi_null, chi_obs)))

# Everything below prints to the .txt report.
sink(file.path(dir_analysis, "table_balance.txt"))
cat("BALANCE OF INDIVIDUAL CHARACTERISTICS ON CO-HIRE COMPOSITION\n")
cat("One regression per row. Outcome = the characteristic; regressor = the\n")
cat("leave-out mean share of life spent urban among the worker's co-hires.\n")
cat("SEs clustered on batch (", n_distinct(w$batch), "clusters).\n\n")
cat("b        = coefficient, natural units of the outcome\n")
cat("beta_std = effect of a 1 sd change in treatment, in sd of the outcome\n")
cat("mde_std  = smallest effect detectable at 80% power, same units\n")
cat("b_fe     = coefficient with firm fixed effects\n\n")
print(as.data.frame(tab %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  select(outcome, n, mean_y, b, se, p, beta_std, mde_std, b_fe, p_fe)),
  row.names = FALSE)
cat("\nrows significant at 5%:", sum(tab$p < 0.05), "of", nrow(tab), "\n")

cat("\n\nPANEL B. IS THE CHARACTERISTIC PREDICTED BY BATCH MEMBERSHIP AT ALL?\n")
cat("F-test on a full set of batch dummies, one regression per characteristic.\n")
cat("p_perm reshuffles batch labels among workers ", n_perm_draws, " times,\n",
    "and is (1 + draws at least as extreme) / (draws + 1), so it floors at ",
    sprintf("%.4f", 1 / (n_perm_draws + 1)), " rather than reaching zero.\n\n",
    sep = "")
print(as.data.frame(omni %>%
  mutate(across(where(is.numeric) & !any_of("p_perm"), ~round(., 3)),
         p_perm = sprintf("%.4f", p_perm))),
  row.names = FALSE)
cat("\nsignificant at 5% (permutation):", sum(omni$p_perm < 0.05), "of",
    nrow(omni), "\n")
cat("rows significant at 5% with firm fixed effects:", sum(tab$p_fe < 0.05),
    "\n")
cat("\nmedian minimum detectable effect:", round(median(tab$mde_std), 2),
    "sd of the outcome\n")
sink()
write.csv(tab, file.path(dir_analysis, "table_balance.csv"), row.names = FALSE)

# Woreda over-representation: is a worker's share of same-woreda co-hires
# above that woreda's frequency in the firm pool (leaving her out)?
wor <- resp %>%
  filter(!in_problem, is_valid_code(birth_woreda)) %>%
  transmute(worker_id, woreda = birth_woreda)
lo_w <- own %>%
  left_join(members, by = "batch", relationship = "many-to-many") %>%
  filter(member != worker_id) %>%
  left_join(wor %>% rename(member = worker_id, m_wor = woreda),
            by = "member") %>%
  left_join(wor, by = "worker_id") %>%
  filter(!is.na(woreda)) %>%
  group_by(worker_id, batch, firm, woreda) %>%
  summarise(own_share = mean(m_wor == woreda, na.rm = TRUE), .groups = "drop")
pool_w <- wor %>%
  left_join(own %>% select(worker_id, firm), by = "worker_id") %>%
  filter(!is.na(firm)) %>%
  group_by(firm) %>%
  mutate(N = n()) %>%
  group_by(firm, woreda) %>%
  mutate(pool_share = (n() - 1) / (N[1] - 1)) %>%
  ungroup() %>%
  select(worker_id, pool_share)
lo_w <- lo_w %>% left_join(pool_w, by = "worker_id")
m_wor <- feols(own_share ~ pool_share, lo_w, cluster = ~batch)
ct <- summary(m_wor)$coeftable
slope <- ct["pool_share", 1]
slope_se <- ct["pool_share", 2]
t_one <- (slope - 1) / slope_se

sink(file.path(dir_analysis, "table_balance.txt"), append = TRUE)
cat("\n\nWOREDA OF ORIGIN\n")
cat("own_share  = share of a worker's co-hires born in her own woreda\n")
cat("pool_share = that woreda's frequency in the firm pool, leaving her out\n")
cat("Random assignment implies slope 1, intercept 0.\n\n")
print(round(ct, 4))
cat(sprintf("\nmean own_share %.4f vs pool benchmark %.4f\n",
            mean(lo_w$own_share, na.rm = TRUE),
            mean(lo_w$pool_share, na.rm = TRUE)))
cat(sprintf("test slope = 1: t = %.2f, p = %.3f\n",
            t_one, 2 * pnorm(-abs(t_one))))
m_ex <- feols(I(own_share - pool_share) ~ 1, lo_w, cluster = ~batch)
ex <- summary(m_ex)$coeftable
cat(sprintf("\nexcess over benchmark: %.4f (se %.4f), p = %.3f  [%+.0f%% relative]\n",
            ex[1, 1], ex[1, 2], ex[1, 4],
            100 * ex[1, 1] / mean(lo_w$pool_share, na.rm = TRUE)))

wor_firm <- own %>%
  select(worker_id, firm) %>%
  inner_join(wor, by = "worker_id") %>%
  filter(!is.na(firm))
pairs_w <- own %>%
  select(worker_id, batch) %>%
  left_join(members, by = "batch", relationship = "many-to-many") %>%
  filter(member != worker_id, member %in% wor_firm$worker_id,
         worker_id %in% wor_firm$worker_id)
wor_excess <- function(wv) {
  key <- setNames(wv, wor_firm$worker_id)
  os <- pairs_w %>%
    mutate(same = as.integer(key[member] == key[worker_id])) %>%
    group_by(worker_id) %>%
    summarise(own_share = mean(same), .groups = "drop")
  ps <- wor_firm %>%
    mutate(woreda = wv) %>%
    group_by(firm) %>%
    mutate(N = n()) %>%
    group_by(firm, woreda) %>%
    mutate(pool_share = (n() - 1) / (N[1] - 1)) %>%
    ungroup() %>%
    select(worker_id, pool_share)
  z <- os %>% inner_join(ps, by = "worker_id")
  mean(z$own_share - z$pool_share)
}
set.seed(13)
wor_obs_ex <- wor_excess(wor_firm$woreda)
wor_null <- replicate(n_perm_draws,
  wor_excess(ave(wor_firm$woreda, wor_firm$firm, FUN = sample)))
wor_p_ri <- (1 + sum(abs(wor_null) >= abs(wor_obs_ex))) / (n_perm_draws + 1)
cat(sprintf("randomisation inference, woreda reshuffled within firm: observed %.4f, p_ri = %.3f\n",
            wor_obs_ex, wor_p_ri))

wb <- w %>%
  group_by(batch) %>%
  filter(n() >= 3, !is.na(urban)) %>%
  summarise(r = cor(lo_urban, urban), .groups = "drop")
cat("\n\nWHY NOT 'WITHIN BATCH'\n")
cat("within-batch correlation between the treatment and the worker's OWN urban\n")
cat("share, across batches with at least 3 workers:\n")
cat("  mean", round(mean(wb$r, na.rm = TRUE), 4),
    "| min", round(min(wb$r, na.rm = TRUE), 4),
    "| max", round(max(wb$r, na.rm = TRUE), 4),
    "| batches", nrow(wb), "\n")
cat("A correlation of -1 means the two are the same variable inside a batch, so\n")
cat("batch fixed effects leave nothing of the treatment behind.\n")

cat("\n\nWHY NOT THE LEAVE-OUT POOL MEAN\n")
cat("The firm leave-out mean was considered as the control for the leave-out\n")
cat("bias. Within a firm it is an exact linear function of the worker's own\n")
cat("urban share, so it cannot serve as an independent control:\n")
pf <- w %>%
  filter(!is.na(pool_lo_urban), !is.na(urban)) %>%
  group_by(firm) %>%
  filter(n() > 2) %>%
  summarise(workers = n(), r = round(cor(pool_lo_urban, urban), 4),
            .groups = "drop")
print(as.data.frame(pf), row.names = FALSE)
cat("Firm fixed effects are used instead.\n")
sink()

### 3. LATEX ---------------------------------------------------------
# Three tables, each carrying its own float, caption and label.

write_tabular("table_balance.tex", "lrrrrr",
  paste0("\\textit{outcome} & \\textit{mean} & \\textit{coef.} & \\textit{s.e.} & ",
         "\\textit{$p$} & \\textit{$p_{\\text{ri}}$} \\\\"),
  paste0(tab$outcome, " & ", sprintf("%.2f", tab$mean_y), " & ",
         sprintf("%.3f", tab$b), stars(tab$p), " & (",
         sprintf("%.3f", tab$se), ") & ", sprintf("%.2f", tab$p), " & ",
         sprintf("%.3f", tab$p_ri), " \\\\"),
  caption = "Balance of worker characteristics on co-hire composition",
  label = "tab:balance",
  note = paste0("One row per characteristic: a regression of it on mean ",
         "co-hire urbanity (share of life urban, worker excluded). ",
         nrow(tab), " characteristics, ", max(tab$n), " workers, ",
         n_distinct(w$batch), " batches; s.e. clustered on batch. Stars mark ",
         "10, 5 and 1 percent. $p_{\\text{ri}}$ is a randomisation-inference ",
         "$p$-value from ", n_perm_draws, " batch-level reassignments."))

omni_tab <- omni %>% filter(!is.na(F))
write_tabular("table_omnibus.tex", "lrrr",
  paste0("\\textit{characteristic} & \\textit{F} & \\textit{$p$} & ",
         "\\textit{$p_{\\text{ri}}$} \\\\"),
  paste0(omni_tab$outcome, " & ",
         sprintf("%.2f", omni_tab$F), " & ",
         ifelse(omni_tab$p_analytic < 0.0005, "$<$0.001",
                sprintf("%.3f", omni_tab$p_analytic)), " & ",
         sprintf("%.3f", omni_tab$p_perm), " \\\\"),
  caption = "Is a characteristic predicted by batch membership?",
  label = "tab:omnibus",
  note = paste0("Joint test that all ", max(omni$batches) - 1, " batch dummies ",
         "are zero, one regression per characteristic, over ", max(omni_tab$n),
         " workers in ", max(omni$batches), " batches. S.E. not clustered on ",
         "batch (singularity). $p_{\\text{ri}}$ reshuffles batch labels ",
         n_perm_draws, " times."))

wor_obs   <- mean(lo_w$own_share,  na.rm = TRUE)
wor_bench <- mean(lo_w$pool_share, na.rm = TRUE)
write_tabular("table_woreda.tex", "rrrrrrr",
  paste0("\\textit{workers} & \\textit{observed} & \\textit{benchmark} & ",
         "\\textit{difference} & \\textit{excess} & \\textit{p} & ",
         "\\textit{$p_{\\text{ri}}$} \\\\"),
  c(paste0(nobs(m_ex), " & ", sprintf("%.4f", wor_obs), " & ",
           sprintf("%.4f", wor_bench), " & ", sprintf("%.4f", ex[1, 1]), " & ",
           sprintf("%+.0f\\%%", 100 * ex[1, 1] / wor_bench), " & ",
           sprintf("%.3f", ex[1, 4]), " & ",
           sprintf("%.3f", wor_p_ri), " \\\\"),
    paste0(" &  &  & (", sprintf("%.4f", ex[1, 2]), ") &  &  &  \\\\")),
  caption = "Are a worker's co-hires drawn from her own woreda?",
  label = "tab:woreda",
  note = paste0("$p$ is from a constant-only regression of the worker-level ",
         "difference, s.e. clustered on batch. $p_{\\text{ri}}$ reshuffles ",
         "birth woreda within firm ", n_perm_draws, " times, holding each ",
         "firm's origin mix fixed."))

### 4. MACROS -------------------------------------------------------
# The balance figures the appendix and Section 5 cite, as \pn macros.

pb <- list()
pb$BalanceN        <- pn_int(max(tab$n))
pb$BalanceChars    <- pn_int(nrow(tab))
pb$BalanceBatches  <- pn_int(n_distinct(w$batch))
pb$BalanceSigWord  <- pn_word(sum(tab$p < 0.05))
pb$BalanceSigFe    <- pn_int(sum(tab$p_fe < 0.05))
pb$TreatSd         <- pn_num(sd(w$lo_urban, na.rm = TRUE), 2)
pb$TreatMean       <- pn_num(mean(w$lo_urban, na.rm = TRUE), 2)
pb$TreatMin        <- pn_num(min(w$lo_urban, na.rm = TRUE), 2)
pb$TreatMax        <- pn_num(max(w$lo_urban, na.rm = TRUE), 2)
pb$TreatPTwentyFive  <- pn_num(quantile(w$lo_urban, .25, na.rm = TRUE), 2)
pb$TreatPSeventyFive <- pn_num(quantile(w$lo_urban, .75, na.rm = TRUE), 2)
pb$BalanceSigRi    <- pn_word(sum(tab$p_ri < 0.05))
pb$OmniSigPermWord <- pn_word(sum(omni_tab$p_perm < 0.05))
pb$MdeMedian       <- pn_num(median(tab$mde_std), 2)
pb$NeverRuralPct   <- pn_pct(mean(chars$never_rural, na.rm = TRUE))
pb$OmniChars       <- pn_int(nrow(omni_tab))
pb$OmniSigWord     <- pn_word(sum(omni_tab$p_analytic < 0.05))
pb$MenN            <- pn_int(sum(w$female == 0, na.rm = TRUE))
pb$BelowGradeTenN  <- pn_int(sum(w$educ_grade10 == 0, na.rm = TRUE))
pb$FemalePct       <- pn_pct(mean(w$female, na.rm = TRUE))
pb$GradeTenPct     <- pn_pct(mean(w$educ_grade10, na.rm = TRUE))
pb$WoredaExcessPct <- pn_int(100 * ex[1, 1] / mean(lo_w$pool_share,
                                                   na.rm = TRUE))
pb$WoredaExcessP   <- pn_num(ex[1, 4], 2)
pb$WoredaExcessPRi <- pn_num(wor_p_ri, 3)
write_pn(pb, file.path(dir_pn, "pn_balance.tex"))

cat("\n")
print(as.data.frame(tab %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  select(outcome, n, b, se, p, beta_std, mde_std, p_fe)), row.names = FALSE)
