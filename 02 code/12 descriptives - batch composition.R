# 12 descriptives - batch composition.R
#
# Descriptives. Documents each hiring batch: how much of its roster made
# the survey (the coverage table), whether batch-mates share an origin
# more than random assignment within a firm would produce (the permutation
# tests), and how the survey and cases file disagree on the production unit.
#
# Feeds: Empirical Setting (batch coverage) and Appendix C (quasi-random
#        assignment). table_batch_coverage.csv is read back by script 17.
#
# Input:  dyads.parquet, batch_crosswalk.csv
# Output: 10 descriptives/table_batch_coverage.csv, table_permutation.csv,
#         table_batch_composition.txt,
#         04 latex/tables/table_batch_coverage.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# Origin-valid dyads, the batch crosswalk, and the within-batch pairs.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "12 descriptives - batch composition.R"

d <- read_parquet(file.path(dir_derived, "dyads.parquet")) %>%
  filter(origin_valid)
xw <- read.csv(file.path(dir_derived, "batch_crosswalk.csv"))

w <- d %>% distinct(worker_id, batch, birth_woreda_i, birth_zone_i)

dw <- d %>% filter(same_batch == 1L)
cat("dyads with valid origin:", nrow(d), "| within one arm:", nrow(dw),
    sprintf("(%.1f%%)\n", 100 * nrow(dw) / nrow(d)))

# Everything printed in this section goes to a single .txt report.
sink(file.path(dir_desc, "table_batch_composition.txt"))

cat("TABLE A. Survey coverage per hiring batch\n")
cat("A batch is a worker's own co-hire SET: the people her preloaded roster row\n")
cat("names, i.e. the co-hires she had access to. Counts are over members of\n")
cat("that set.\n\n")
cat("rostered    = size of the set\n")
cat("cell        = workers whose OWN row lists this exact set. Equal to rostered\n")
cat("              when every member names the same set, which marks the batch\n")
cat("              as a closed hiring line (line = TRUE)\n")
cat("interviewed = members who completed an interview\n")
cat("excluded    = of those, dropped as problem records (\"00 config.R\")\n")
cat("usable      = interviewed and not excluded\n")
cat("typed       = usable members whose birth woreda is observed; the survey\n")
cat("              never asks an ego about a co-hire's origin, so only these\n")
cat("              carry an origin label\n")
cat("coverage    = typed / rostered\n\n")

### 2. TABLES ---------------------------------------------------------
# Coverage per batch; the permutation tests of within-batch origin
# sharing; the woreda-mix dissimilarity; and the production-unit checks.

cov <- xw %>%
  filter(in_sample) %>%
  arrange(batch) %>%
  select(batch, firm, rostered, cell, line = is_line, interviewed, excluded,
         usable, typed, coverage)
print(as.data.frame(cov), row.names = FALSE)
cat("\ntotal rostered in the", nrow(cov), "estimation arms:", sum(cov$rostered),
    "| interviewed:", sum(cov$interviewed), "| typed:", sum(cov$typed), "\n")
cat("arms that are closed hiring lines:", sum(cov$line), "of", nrow(cov), "\n")
cat("dyads contributed by arms with <12 typed members:",
    sum(dw$batch %in% cov$batch[cov$typed < 12]), "of", nrow(dw), "\n")

comp_in <- xw %>%
  group_by(comp) %>%
  summarise(any_in = any(in_sample), .groups = "drop")
excl <- xw %>%
  left_join(comp_in, by = "comp") %>%
  filter(!any_in) %>%
  group_by(comp, firm) %>%
  summarise(arms = n(), rostered = sum(rostered),
            interviewed = sum(interviewed), excluded = sum(excluded),
            usable = sum(usable), .groups = "drop")
cat("\nSurveyed components excluded from the estimation sample:\n")
print(as.data.frame(excl %>% filter(interviewed > 0) %>%
                      arrange(desc(interviewed))),
      row.names = FALSE)
cat("\nThe remaining", sum(excl$interviewed == 0),
    "components had no interviewed member at all.\n")
cat("Arms in total:", nrow(xw), "| labelled batch_01..batch_",
    sprintf("%02d", sum(xw$in_sample)), ":", sum(xw$in_sample),
    "| components spanned:", n_distinct(xw$comp), "\n")

cat("\n\nTABLE B. Permutation tests: same-origin share of within-batch dyads\n")
cat("2,000 draws; labels reshuffled across batches within the stated stratum.\n")
cat("Pairs are restricted to those whose two members sit in the same arm; under\n")
cat("the superseded component definition that was automatic, under row-sets it\n")
cat("is not.\n\n")

pairs_all <- dw %>% select(worker_id, alter_id)
firm_of <- setNames(cov$firm[match(w$batch, cov$batch)], w$worker_id)

# Observed same-origin share among within-batch pairs, against the null of
# reshuffling the origin label across batches within `strata`.
perm_test <- function(labels, strata, keep, label, B = n_perm_draws) {
  ids <- intersect(names(labels), keep)
  pr <- pairs_all %>% filter(worker_id %in% ids, alter_id %in% ids)
  obs <- mean(labels[pr$worker_id] == labels[pr$alter_id])
  st <- strata[ids]
  set.seed(7)
  null <- replicate(B, {
    p <- labels[ids]
    for (s in unique(st)) {
      j <- which(st == s)
      p[j] <- sample(p[j])
    }
    mean(p[pr$worker_id] == p[pr$alter_id])
  })
  data.frame(test = label, dyads = nrow(pr), observed = round(obs, 4),
             null = round(mean(null), 4), null_sd = round(sd(null), 4),
             excess_pct = round(100 * (obs / mean(null) - 1), 1),
             p = round(perm_p(null, obs), 4))
}

wor <- setNames(w$birth_woreda_i, w$worker_id)
zon <- setNames(w$birth_zone_i, w$worker_id)
all_ids <- w$worker_id
one <- setNames(rep("all", length(all_ids)), all_ids)
big <- w$worker_id[w$batch %in% cov$batch[cov$typed >= 12]]

tabB <- bind_rows(
  perm_test(wor, one,     all_ids, "woreda, unrestricted"),
  perm_test(wor, firm_of, all_ids, "woreda, within firm"),
  perm_test(wor, firm_of, big,     "woreda, within firm, batches >=12 typed"),
  perm_test(zon, one,     all_ids, "zone, unrestricted"),
  perm_test(zon, firm_of, all_ids, "zone, within firm"),
  perm_test(zon, firm_of, big,     "zone, within firm, batches >=12 typed"))

tabB <- bind_rows(tabB, bind_rows(lapply(
  unique(cov$firm[duplicated(cov$firm)]),
  function(f) {
    ids <- w$worker_id[w$batch %in% cov$batch[cov$firm == f & cov$typed >= 6]]
    if (length(ids) > 20) {
      perm_test(wor, setNames(rep(f, length(ids)), ids), ids,
                paste0("woreda, ", f, " only"))
    }
  })))
print(tabB, row.names = FALSE)
cat("\nRead: 'excess_pct' is how much more often batch-mates share an origin\n")
cat("than they would under random assignment within the stratum.\n")

di <- w %>%
  count(batch, birth_woreda_i) %>%
  group_by(batch) %>%
  mutate(p_b = n / sum(n)) %>%
  ungroup() %>%
  left_join(w %>% count(birth_woreda_i, name = "n_tot") %>%
              mutate(p_all = n_tot / sum(n_tot)), by = "birth_woreda_i") %>%
  group_by(batch) %>%
  summarise(D = 0.5 * sum(abs(p_b - p_all)), .groups = "drop")
cat("\nDissimilarity of the woreda mix from the pooled mix, by batch:\n")
print(as.data.frame(di %>%
                      left_join(cov %>% select(batch, typed), by = "batch") %>%
                      filter(typed >= 12) %>%
                      mutate(D = round(D, 3))),
      row.names = FALSE)

cat("\n\nTABLE C. Production unit: survey self-report vs the cases record\n\n")
pu <- d %>% distinct(worker_id, pu = pu_i, pu_cases = pu_cases_i)
cat("surveyed workers in the estimation sample:", nrow(pu), "\n")
cat("  production unit from survey:", sum(!is.na(pu$pu)), "\n")
cat("  production unit from cases :", sum(!is.na(pu$pu_cases)), "\n")
both <- pu %>% filter(!is.na(pu), !is.na(pu_cases))
cat("  both observed:", nrow(both), "| agree:", sum(both$pu == both$pu_cases),
    sprintf("(%.1f%%)\n", 100 * mean(both$pu == both$pu_cases)))
cat("\ndistinct units resolved by each source, within batch:\n")
print(as.data.frame(d %>%
  group_by(batch) %>%
  summarise(typed = n_distinct(worker_id),
            units_survey = n_distinct(pu_i[!is.na(pu_i)]),
            units_cases = n_distinct(pu_cases_i[!is.na(pu_cases_i)]),
            .groups = "drop") %>%
  arrange(desc(typed))), row.names = FALSE)
cat("\nThe survey resolves far more units than cases in every large batch:\n")
cat("cases is a department field (it also carries the training stage\n")
cat("'SOFTSKILL' for 14 workers), the survey names the production line. So the\n")
cat("disagreement is mostly coarseness, not contradiction, and cases\n")
cat("understates within-batch separation.\n")
cat("\nsame-PU dyads under each source:\n")
cat("  survey:", sum(dw$same_pu, na.rm = TRUE), "| cases:",
    sum(dw$same_pu_cases, na.rm = TRUE), "\n")
cat("\ntop disagreements (survey -> cases):\n")
print(as.data.frame(both %>% filter(pu != pu_cases) %>%
  count(survey = pu, cases = pu_cases, sort = TRUE) %>% head(10)),
  row.names = FALSE)

cl <- ~ worker_id + alter_id

ok <- dw %>% filter(!is.na(same_pu), !is.na(same_pu_cases))
cat("\npost-HIP formation on each source (batch FE, two-way clustered, n =",
    nrow(ok), "):\n")
print(etable(
  feols(tied_post ~ same_woreda + shared_lang + same_pu | batch, ok,
        cluster = cl),
  feols(tied_post ~ same_woreda + shared_lang + same_pu_cases | batch, ok,
        cluster = cl),
  headers = c("PU: survey", "PU: cases")))
sink()

write.csv(cov, file.path(dir_desc, "table_batch_coverage.csv"),
          row.names = FALSE)
write.csv(tabB, file.path(dir_desc, "table_permutation.csv"),
          row.names = FALSE)

### 3. LATEX ---------------------------------------------------------
# The coverage table, which carries its own float, caption and label.

cov_rank <- cov %>%
  filter(rostered >= 10) %>%
  arrange(desc(coverage), desc(rostered))
rowsA <- paste0(gsub("_", "\\\\_", cov_rank$batch), " & ", cov_rank$firm,
                " & ", cov_rank$rostered,
                " & ", ifelse(cov_rank$line, "yes", "--"),
                " & ", cov_rank$interviewed, " & ", cov_rank$typed,
                " & ", sprintf("%.2f", cov_rank$coverage), " \\\\")
n_show <- 8
n_hid <- nrow(cov_rank) - 2 * n_show
n_small <- nrow(cov) - nrow(cov_rank)

write_tabular("table_batch_coverage.tex", "lrrrrrr",
  "\\textit{batch} & \\textit{firm} & \\textit{size} & \\textit{line} & \\textit{interviewed} & \\textit{typed} & \\textit{coverage} \\\\",
  c(head(rowsA, n_show), "\\midrule",
    sprintf("\\multicolumn{7}{c}{\\textit{%d further batches omitted}} \\\\",
            n_hid),
    "\\midrule", tail(rowsA, n_show)),
  caption = "Survey coverage of the hiring batches", label = "tab:coverage",
  note = sprintf("One row is a hiring batch with at least 10 rostered workers (%d smaller batches omitted), ordered by \\textit{coverage}, the share of the roster that entered the analysis sample: the %d best and %d worst are shown, %d omitted between. \\textit{line} marks a closed hiring line; \\textit{typed} counts interviewed workers with a recorded birthplace.",
                 n_small, n_show, n_show, n_hid))

cat("batch composition tables written to", dir_desc, "\n")
print(tabB, row.names = FALSE)
