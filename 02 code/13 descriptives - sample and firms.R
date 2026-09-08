# 13 descriptives - sample and firms.R
#
# Descriptives. The sample-construction ladder (rostered -> tracked ->
# submitted -> consented -> deduplicated -> analysis sample), the firm
# breakdown, the time between hire and interview, and the worker
# demographics table split by rural / urban background.
#
# Feeds: Empirical Setting (sample, firms, demographics tables).
#
# Input:  respondents.parquet, dyads.parquet, gn_edgelist.parquet,
#         batch_crosswalk.csv, firm_crosswalk.csv, raw roster and cases
# Output: 10 descriptives/table_sample_construction.csv, table_firms.csv,
#         table_hire_date_status.csv, table_time_since_hire.csv,
#         table_demographics.csv,
#         04 latex/tables/table_sample.tex, table_firms.tex,
#         table_demographics.tex, 04 latex/pn/pn_sample.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The derived datasets, plus the raw roster and cases for the frame counts.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "13 descriptives - sample and firms.R"

resp <- read_parquet(file.path(dir_derived, "respondents.parquet"))
dy   <- read_parquet(file.path(dir_derived, "dyads.parquet"))
gn   <- read_parquet(file.path(dir_derived, "gn_edgelist.parquet"))
xw   <- read.csv(file.path(dir_derived, "batch_crosswalk.csv"),
                 stringsAsFactors = FALSE)
fxw  <- read.csv(file.path(dir_derived, "firm_crosswalk.csv"),
                 stringsAsFactors = FALSE)

an <- resp %>% filter(!in_problem)

pn <- list()

roster <- read_csv(file_roster, show_col_types = FALSE,
                   col_types = cols(.default = "c"))
cases  <- read_csv(file_cases, show_col_types = FALSE,
                   col_types = cols(.default = "c"))

n_rostered <- nrow(roster)
n_cases    <- nrow(cases)
subm <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey"))
n_subm     <- nrow(subm)
n_consent  <- sum(subm$consent_survey == "1", na.rm = TRUE)
n_resp     <- nrow(resp)
n_problem  <- sum(resp$in_problem)
n_fixed    <- sum(resp$resurveyed, na.rm = TRUE)
n_frame    <- n_problem + n_fixed
n_analysis <- nrow(an)
n_dyad_ego <- n_distinct(dy$worker_id)
n_gn_ego   <- n_distinct(gn$worker_id)

### 2. TABLES ---------------------------------------------------------
# Sample construction, firms, time since hire, and demographics.

t1 <- tibble(
  step = c("Rostered by the six firms",
           "In the fieldwork tracking file",
           "Baseline submissions",
           "Consented",
           "Distinct respondents (latest submission kept)",
           "Excluded: incorrectly surveyed",
           "Analysis sample",
           "  of whom form at least one co-hire pair",
           "  of whom report a general network"),
  n = c(n_rostered, n_cases, n_subm, n_consent, n_resp, n_problem,
        n_analysis, n_dyad_ego, n_gn_ego))
write.csv(t1, file.path(dir_desc, "table_sample_construction.csv"),
          row.names = FALSE)

pn$FrameRostered   <- pn_int(n_rostered)
pn$FrameCases      <- pn_int(n_cases)
pn$Submissions     <- pn_int(n_subm)
pn$ResponsePct     <- pn_pct(n_resp / n_cases)
pn$ProblemExcluded <- pn_int(n_problem)
pn$ProblemFixed    <- pn_int(n_fixed)
pn$ProblemFrame    <- pn_int(n_frame)
pn$DyadEgos        <- pn_int(n_dyad_ego)
pn$NoPairWord      <- pn_word(n_analysis - n_dyad_ego)

# Worker -> anonymised firm, from the batch crosswalk's row-sets.
w2f <- xw %>%
  select(firm, firm_name, rowset) %>%
  mutate(worker_id = strsplit(rowset, "|", fixed = TRUE)) %>%
  tidyr::unnest(worker_id) %>%
  mutate(worker_id = toupper(str_trim(worker_id))) %>%
  distinct(worker_id, firm, firm_name)
stopifnot(!anyDuplicated(w2f$worker_id))

w2b <- dy %>% distinct(worker_id, batch)
stopifnot(!anyDuplicated(w2b$worker_id))

an_f <- an %>%
  left_join(w2f, by = "worker_id") %>%
  left_join(w2b, by = "worker_id")
cat("analysis workers with no firm from the crosswalk:",
    sum(is.na(an_f$firm)), "\n")

t2 <- an_f %>%
  filter(!is.na(firm)) %>%
  group_by(firm) %>%
  summarise(workers = n(), batches = n_distinct(batch[!is.na(batch)]),
            .groups = "drop") %>%
  left_join(fxw %>% select(firm, rostered), by = "firm") %>%
  mutate(share_of_sample = round(workers / sum(workers), 3),
         sampling_rate = round(workers / rostered, 3)) %>%
  arrange(desc(workers)) %>%
  select(firm, rostered, workers, share_of_sample, sampling_rate, batches)
write.csv(t2, file.path(dir_desc, "table_firms.csv"), row.names = FALSE)

pn$FirmsWord    <- pn_word(nrow(t2))
pn$FirmTopPct   <- pn_pct(max(t2$share_of_sample))
pn$FirmTopLabel <- t2$firm[which.max(t2$share_of_sample)]

lag_ok <- an %>% filter(!is.na(days_to_interview))

t3_status <- an %>%
  count(join_date_status, name = "workers") %>%
  mutate(share = round(workers / sum(workers), 3)) %>%
  arrange(desc(workers))

t3_dist <- lag_ok %>%
  summarise(workers = n(),
            min = min(days_to_interview),
            p25 = quantile(days_to_interview, 0.25),
            median = median(days_to_interview),
            p75 = quantile(days_to_interview, 0.75),
            max = max(days_to_interview),
            mean = round(mean(days_to_interview), 1),
            within_30d = round(mean(days_to_interview <= 30), 3),
            within_60d = round(mean(days_to_interview <= 60), 3))
write.csv(t3_status, file.path(dir_desc, "table_hire_date_status.csv"),
          row.names = FALSE)
write.csv(t3_dist, file.path(dir_desc, "table_time_since_hire.csv"),
          row.names = FALSE)

pn$HireLagMedian         <- pn_int(t3_dist$median)
pn$HireLagPTwentyFive    <- pn_int(t3_dist$p25)
pn$HireLagPSeventyFive   <- pn_int(t3_dist$p75)
pn$HireLagMax            <- pn_int(t3_dist$max)
pn$HireWithinMonthPct    <- pn_pct(t3_dist$within_30d)
pn$HireWithinTwoMonthPct <- pn_pct(t3_dist$within_60d)
pn$HireInferredPct       <- pn_pct(mean(lag_ok$join_date_status == "inferred"))
pn$HireDatedN            <- pn_int(nrow(lag_ok))

sidama_code <- code_for("Sidama", get_choice_labels("regions"))

chars <- an %>%
  transmute(worker_id,
            rural_grp = ifelse(coalesce(rural_share, 0) > 0.5,
                               "Majority-rural", "Urban background"),
            Age = resp_age_n,
            Female = as.integer(resp_gender == "2"),
            `Education (1-12)` = education_n,
            `Completed grade 10` = educ_grade10,
            Married = married,
            `Has children` = has_children_n,
            `Household size` = hh_size_n,
            `Born in Sidama` = ifelse(is_valid_code(birth_region),
                                      as.integer(birth_region == sidama_code),
                                      NA_integer_),
            `Speaks Sidamigna (1-5)` = sid_speak_n,
            `Speaks Amharic (1-5)` = amh_speak_n,
            `Share of life urban` = 1 - rural_share,
            `Years in Hawassa` = yrs_hawassa,
            `Co-hire set size` = nb_cohires_n,
            `Days since hire` = days_to_interview)

long <- chars %>%
  tidyr::pivot_longer(-c(worker_id, rural_grp), names_to = "variable",
                      values_to = "value") %>%
  filter(!is.na(value))

summ <- function(df) {
  df %>%
    group_by(variable) %>%
    summarise(n = n(), mean = round(mean(value), 2), sd = round(sd(value), 2),
              .groups = "drop")
}

t4 <- summ(long) %>%
  rename(all_n = n, all_mean = mean, all_sd = sd) %>%
  left_join(summ(filter(long, rural_grp == "Majority-rural")) %>%
              rename(rural_n = n, rural_mean = mean, rural_sd = sd),
            by = "variable") %>%
  left_join(summ(filter(long, rural_grp == "Urban background")) %>%
              rename(urban_n = n, urban_mean = mean, urban_sd = sd),
            by = "variable") %>%
  arrange(match(variable, setdiff(names(chars), c("worker_id", "rural_grp"))))
write.csv(t4, file.path(dir_desc, "table_demographics.csv"), row.names = FALSE)

pn$RuralMajorityPct <- pn_pct(mean(chars$rural_grp == "Majority-rural"))
pn$TableOneChars    <- pn_int(nrow(t4))

t1_lab <- ifelse(grepl("of whom", t1$step),
                 sprintf("\\textit{%s}", tex_esc(t1$step)), tex_esc(t1$step))
t1_num <- ifelse(grepl("of whom", t1$step),
                 sprintf("\\textit{%s}", pn_int(t1$n)), pn_int(t1$n))

### 3. LATEX ---------------------------------------------------------
# The three tables the Setting section \input{}s.

write_tabular("table_sample.tex", "l|r",
  "& \\textit{workers} \\\\",
  sprintf("%s & %s \\\\", t1_lab, t1_num))

write_tabular("table_firms.tex", "lrrrrr",
  paste("\\textit{firm} & \\textit{rostered} & \\textit{surveyed} &",
        "\\textit{share of sample} & \\textit{sampling rate} &",
        "\\textit{co-hire sets} \\\\"),
  sprintf("%s & %s & %s & %s & %s & %s \\\\", t2$firm, pn_int(t2$rostered),
          pn_int(t2$workers), pn_num(t2$share_of_sample, 2),
          pn_num(t2$sampling_rate, 2), pn_int(t2$batches)),
  sprintf("Total & %s & %s & & & %s \\\\", pn_int(sum(t2$rostered)),
          pn_int(sum(t2$workers)), pn_int(sum(t2$batches))))

write_tabular("table_demographics.tex", "lrrrrrr",
  c(paste("& \\multicolumn{2}{c}{\\textit{all}} &",
          "\\multicolumn{2}{c}{\\textit{majority-rural}} &",
          "\\multicolumn{2}{c}{\\textit{urban background}} \\\\"),
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
    "& \\textit{mean} & \\textit{sd} & \\textit{mean} & \\textit{sd} & \\textit{mean} & \\textit{sd} \\\\"),
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", tex_esc(t4$variable),
          pn_num(t4$all_mean, 2), pn_num(t4$all_sd, 2),
          pn_num(t4$rural_mean, 2), pn_num(t4$rural_sd, 2),
          pn_num(t4$urban_mean, 2), pn_num(t4$urban_sd, 2)),
  sprintf("Workers & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\",
          pn_int(nrow(chars)), pn_int(sum(chars$rural_grp == "Majority-rural")),
          pn_int(sum(chars$rural_grp == "Urban background"))))

### 4. MACROS -------------------------------------------------------
# The sample figures the prose cites, as \pn macros.

write_pn(pn, file.path(dir_pn, "pn_sample.tex"), plain = "FirmTopLabel")

cat("\nsample construction:\n"); print(t1)
cat("\nfirms:\n"); print(t2)
cat("\nhire-date status:\n"); print(t3_status)
cat("\ntime since hire:\n"); print(as.data.frame(t3_dist))
cat("\ndemographics:\n"); print(as.data.frame(t4))
