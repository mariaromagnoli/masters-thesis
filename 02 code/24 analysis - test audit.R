# 24 analysis - test audit.R
#
# Analysis. Produces one quantity for each robustness threat named in the
# validity-audit appendix (largest-component dominance, same-woreda inside
# vs outside it, the pooled estimate, the pre-HIP share among tied
# same-woreda pairs, median recall). Every number in that appendix is a
# \pn macro from here, so re-running keeps the appendix honest.
#
# Feeds: Appendix D (validity audit).
#
# Input:  dyads.parquet, respondents.parquet, batch_crosswalk.csv
# Output: 20 analysis/table_test_audit.csv, 04 latex/pn/pn_audit.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The within-batch dyads, the largest roster component, and the wave flag.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "24 analysis - test audit.R"
library(fixest)

d <- read_parquet(file.path(dir_derived, "dyads.parquet"))
r <- read_parquet(file.path(dir_derived, "respondents.parquet"))
xw <- read.csv(file.path(dir_derived, "batch_crosswalk.csv"),
               stringsAsFactors = FALSE)

w <- d %>%
  filter(same_batch == 1L) %>%
  left_join(xw %>% distinct(batch, .keep_all = TRUE) %>% select(batch, comp),
            by = "batch")
big <- names(sort(table(w$comp), decreasing = TRUE))[1]

### 2. DIAGNOSTICS -------------------------------------------------
# One quantity per threat named in the appendix audit, none typed by hand.

m_in  <- feols(tied_any ~ same_woreda | batch, w %>% filter(comp == big),
               cluster = ~worker_id)
m_out <- feols(tied_any ~ same_woreda | batch, w %>% filter(comp != big),
               cluster = ~worker_id)
m_all <- feols(tied_any ~ same_woreda | batch, w, cluster = ~worker_id)

tied_sw <- w %>% filter(same_woreda == 1L, tied_any == 1L)
recalled_per_ego <- w %>%
  group_by(worker_id) %>%
  summarise(k = sum(recalled), roster = n(), .groups = "drop")

audit <- data.frame(
  quantity = c("largest component share of within-batch dyads",
               "same-woreda inside it", "same-woreda outside it", "pooled",
               "tied same-woreda dyads predating HIP",
               "median names recalled per ego", "median roster shown"),
  value = c(mean(w$comp == big, na.rm = TRUE),
            coef(m_in)[["same_woreda"]], coef(m_out)[["same_woreda"]],
            coef(m_all)[["same_woreda"]],
            mean(tied_sw$tied_pre == 1L),
            median(recalled_per_ego$k), median(recalled_per_ego$roster)))
write.csv(audit, file.path(dir_analysis, "table_test_audit.csv"),
          row.names = FALSE)
print(audit)

### 3. MACROS -------------------------------------------------------
# The audit figures the appendix cites, as \pn macros.

pa <- list(
  AuditBigComp    = big,
  AuditBigCompPct = pn_pct(mean(w$comp == big, na.rm = TRUE)),
  AuditBigCompPp  = pn_pct(coef(m_in)[["same_woreda"]], 1),
  AuditBigCompP   = pn_num(pvalue(m_in)[["same_woreda"]], 2),
  AuditOutCompPp  = pn_pct(coef(m_out)[["same_woreda"]], 1),
  AuditPooledPp   = pn_pct(coef(m_all)[["same_woreda"]], 1),
  AuditPreTiedPct = pn_pct(mean(tied_sw$tied_pre == 1L)),
  AuditRecallMed  = pn_int(median(recalled_per_ego$k)),
  AuditRosterMed  = pn_int(median(recalled_per_ego$roster)))
write_pn(pa, file.path(dir_pn, "pn_audit.tex"), plain = "AuditBigComp")

cat("test audit written\n")
