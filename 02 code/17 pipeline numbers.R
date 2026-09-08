# 17 pipeline numbers.R
#
# Descriptives. Collects the sample-construction and data-provenance
# numbers the thesis cites but that no single table owns (respondent
# counts, excluded records, the empty raw export, the join-date recovery,
# the production-unit agreement rate) and writes them as \pn macros.
#
# Feeds: Empirical Setting and the data appendix.
#
# Input:  dyads.parquet, respondents.parquet, batch_crosswalk.csv,
#         raw roster, cases and baseline, 10 descriptives/table_batch_coverage.csv
# Output: 04 latex/pn/pn_data.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The derived datasets, and the raw exports the provenance facts count on.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "17 pipeline numbers.R"

d   <- read_parquet(file.path(dir_derived, "dyads.parquet"))
r   <- read_parquet(file.path(dir_derived, "respondents.parquet"))
xw  <- read.csv(file.path(dir_derived, "batch_crosswalk.csv"))
ros <- read_csv(file_roster, show_col_types = FALSE)
cas <- read_csv(file_cases, show_col_types = FALSE)

### 2. NUMBERS -------------------------------------------------------
# Sample definition and excluded records; the empty raw export; the
# join-date recovery; the batches; the dead components; the dyad pair
# set; and how well the survey and cases agree on the production unit.

p <- list()

p$Respondents <- pn_int(nrow(r))
p$ProblemIds <- pn_int(sum(r$in_problem))
p$AnalysisN <- pn_int(sum(!r$in_problem))
p$ProblemOnRoster <- pn_int(sum(toupper(unique(
  read_csv(file_problem, show_col_types = FALSE)$worker_id)) %in%
  toupper(ros$worker_id_key)))
pf <- cas$firm_name[match(toupper(r$worker_id[r$in_problem]),
                          toupper(cas$worker_id_key))]
p$ProblemTopFirmPct <- pn_pct(max(table(pf)) / length(pf))
p$WaveThreeRoster <- pn_int(sum(
  cas$wave[match(ros$worker_id_key, cas$worker_id_key)] == "Wave 3",
  na.rm = TRUE))

# One raw export snapshot has a header row but no data - count its columns.
empty_export <- file.path(dir_data, "baseline", "HIP Baseline 071126csv.csv")
p$EmptyExportCols <- pn_int(length(scan(empty_export, what = "", sep = ",",
                                        nlines = 1, quiet = TRUE)))
stopifnot(length(suppressWarnings(readLines(empty_export, n = 2))) == 1)

jd <- clean_join_date()
p$JoinSlash <- pn_int(sum(jd$join_date_status %in% c("forced", "inferred")))
p$JoinAmbiguous <- pn_int(sum(jd$join_date_status == "inferred"))

p$Batches <- pn_int(sum(xw$in_sample == TRUE))
p$BatchesWord <- pn_word(sum(xw$in_sample == TRUE), cap = TRUE)

# The largest surveyed roster component that did not make the sample.
ids_of <- function(rs) unique(unlist(strsplit(rs, "|", fixed = TRUE)))
dead <- xw %>%
  group_by(comp) %>%
  summarise(any_in = any(in_sample), rowsets = paste(rowset, collapse = "|"),
            .groups = "drop") %>%
  filter(!any_in) %>%
  mutate(members = lengths(lapply(rowsets, ids_of)),
         seen = vapply(rowsets, function(rs)
           sum(toupper(ids_of(rs)) %in% toupper(r$worker_id)), integer(1))) %>%
  slice_max(seen, n = 1)
dead_ids <- ids_of(dead$rowsets)
dead_seen <- r[toupper(r$worker_id) %in% toupper(dead_ids), ]
stopifnot(nrow(dead_seen) == dead$seen)
cat("largest dead component: rostered", dead$members, "| interviewed",
    dead$seen, "| of those excluded", sum(dead_seen$in_problem),
    "| recovered by the fix-up", sum(dead_seen$resurveyed),
    "| neither", sum(!dead_seen$in_problem & !dead_seen$resurveyed), "\n")
p$DeadCompInterviewed <- pn_int(dead$seen)
p$DeadCompRostered <- pn_int(dead$members)

dwb <- d %>% filter(same_batch == 1L)
p$Dyads <- pn_int(nrow(d))
p$DyadsWithin <- pn_int(nrow(dwb))
p$OriginValidDyads <- pn_int(sum(d$origin_valid))

dv <- d %>% filter(origin_valid)
w <- bind_rows(dv %>% transmute(id = worker_id, pu = pu_i, puc = pu_cases_i),
               dv %>% transmute(id = alter_id, pu = pu_j, puc = pu_cases_j)) %>%
  distinct(id, .keep_all = TRUE)
both <- w %>% filter(!is.na(pu), !is.na(puc))
p$PuBoth <- pn_int(nrow(both))
p$PuAgreePct <- pn_pct(mean(both$pu == both$puc))
p$PuSurveyColoc <- pn_int(sum(dv$same_pu, na.rm = TRUE))
p$PuCasesColoc <- pn_int(sum(dv$same_pu_cases, na.rm = TRUE))
p$PuUnknownPct <- pn_pct(mean(dv$pu_known == 0))

cov <- read.csv(file.path(dir_desc, "table_batch_coverage.csv"))
p$CoverageMaxPct <- pn_pct(max(cov$coverage))
p$CoverageMinPct <- pn_pct(min(cov$coverage))

p$Generators <- pn_int(length(generator_labels))
p$RsQuestions <- pn_int(length(rs_out_labels))

### 3. MACROS -------------------------------------------------------
# Writes pn_data.tex, so no construction figure is typed into the thesis.

write_pn(p, file.path(dir_pn, "pn_data.tex"),
         plain = c("Generators", "RsQuestions"))
