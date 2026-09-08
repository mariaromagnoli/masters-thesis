# 15 descriptives - livelihoods.R
#
# Descriptives. Wage, earnings, rent and job-search distributions for the
# analysis sample, and whether the HIP wage covers the rent.
#
# Feeds: Empirical Setting (livelihoods paragraph and table).
#
# Input:  respondents.parquet, raw baseline (income module)
# Output: 10 descriptives/table_livelihoods.csv, table_income_adequacy.csv
#         04 latex/tables/table_livelihoods.tex, 04 latex/pn/pn_livelihoods.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# Wage, earnings, rent and job-search fields, with the special codes stripped.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "15 descriptives - livelihoods.R"

resp <- read_parquet(file.path(dir_derived, "respondents.parquet")) %>%
  filter(!in_problem)

inc <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  "current_wage", "_total_earn_14d", "_resp_earn_3mo", "_total_hh_income_3mo",
  "monthly_rent", "earn_vs_usual", "hip_pay_method", "days_to_find_job",
  "resv_wage_amount", "wage_stay_1yr")) %>%
  dedup_respondents() %>%
  select(-SubmissionDate, -consent_survey)

# Numeric only where the value is a real answer, not a -999 / -888 code.
num <- function(x) {
  ifelse(is_valid_code(x), suppressWarnings(as.numeric(x)), NA_real_)
}

liv <- resp %>%
  select(worker_id, hh_size_n, days_to_interview) %>%
  left_join(inc, by = "worker_id") %>%
  mutate(across(c(current_wage, `_total_earn_14d`, `_resp_earn_3mo`,
                  `_total_hh_income_3mo`, monthly_rent, days_to_find_job,
                  resv_wage_amount, wage_stay_1yr), num),
         resp_earn_month = ifelse(!is.na(days_to_interview) &
                                    days_to_interview >= 14,
                                  `_total_earn_14d` * 30 / 14, NA_real_),
         hh_income_3mo = `_total_hh_income_3mo`,
         rent_share = ifelse(!is.na(monthly_rent) & !is.na(resp_earn_month) &
                               resp_earn_month > 0,
                             monthly_rent / resp_earn_month, NA_real_),
         resv_ratio = ifelse(!is.na(resv_wage_amount) & !is.na(current_wage) &
                               current_wage > 0,
                             resv_wage_amount / current_wage, NA_real_))

pn <- list()

### 2. TABLES ---------------------------------------------------------
# The distributions, and whether the wage covers the rent.

q <- function(x, p) unname(quantile(x, p, na.rm = TRUE))
dist_row <- function(v, label, unit) {
  x <- liv[[v]]
  tibble(variable = label, unit = unit, n = sum(!is.na(x)),
         mean = round(mean(x, na.rm = TRUE), 1),
         p10 = round(q(x, 0.10), 1), p25 = round(q(x, 0.25), 1),
         median = round(q(x, 0.50), 1), p75 = round(q(x, 0.75), 1),
         p90 = round(q(x, 0.90), 1))
}
t5a <- bind_rows(
  dist_row("current_wage", "Monthly wage at HIP", "Birr"),
  dist_row("resp_earn_month", "Own earnings, all activities", "Birr/month"),
  dist_row("hh_income_3mo", "Household income, past 3 months", "Birr"),
  dist_row("monthly_rent", "Rent", "Birr/month"),
  dist_row("rent_share", "Rent as a share of own earnings", "ratio"),
  dist_row("wage_stay_1yr", "Expected wage in one year", "Birr"),
  dist_row("resv_wage_amount", "Reservation wage, outside offer", "Birr"),
  dist_row("resv_ratio", "Reservation wage / current wage", "ratio"),
  dist_row("days_to_find_job", "Days spent finding this job", "days"))
write.csv(t5a, file.path(dir_desc, "table_livelihoods.csv"), row.names = FALSE)

both <- liv %>% filter(!is.na(rent_share))
t5b <- tibble(
  measure = c("Workers reporting both earnings and rent",
              "Rent takes over half of own earnings",
              "Rent exceeds own earnings entirely",
              "Pays no rent at all"),
  n = c(nrow(both),
        sum(both$rent_share > 0.5),
        sum(both$rent_share > 1),
        sum(both$monthly_rent == 0)))
t5b <- t5b %>% mutate(share = round(n / nrow(both), 3))
write.csv(t5b, file.path(dir_desc, "table_income_adequacy.csv"),
          row.names = FALSE)

# The pay-method table was dropped when the field was degenerate; warn if
# it stops being degenerate so it can be reinstated.
n_pay <- liv %>%
  filter(is_valid_code(hip_pay_method)) %>%
  distinct(hip_pay_method) %>%
  nrow()
if (n_pay != 1) {
  warning("hip_pay_method now takes ", n_pay, " values, not 1. It was ",
          "degenerate when the table was dropped (s10) -- reinstate table5c ",
          "if this persists.")
}

pn$WageMedian <- pn_int(median(liv$current_wage, na.rm = TRUE))
pn$WagePTwentyFive <- pn_int(q(liv$current_wage, 0.25))
pn$WagePSeventyFive <- pn_int(q(liv$current_wage, 0.75))
pn$HhIncomeMedianThreeMo <- pn_int(median(liv$hh_income_3mo, na.rm = TRUE))
pn$EarnWindowN <- pn_int(sum(!is.na(liv$resp_earn_month)))
pn$ResvRatioMedian <- pn_num(median(liv$resv_ratio, na.rm = TRUE), 2)
pn$DaysFindJobMedian <- pn_int(median(liv$days_to_find_job, na.rm = TRUE))

### 3. LATEX ---------------------------------------------------------
# The livelihoods distribution table.

write_tabular("table_livelihoods.tex", "llrrrrr",
  "& & \\textit{n} & \\textit{p25} & \\textit{median} & \\textit{p75} & \\textit{p90} \\\\",
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
          tex_esc(t5a$variable), t5a$unit, pn_int(t5a$n),
          pn_num(t5a$p25, 1), pn_num(t5a$median, 1),
          pn_num(t5a$p75, 1), pn_num(t5a$p90, 1)))

### 4. MACROS -------------------------------------------------------
# The livelihood figures the prose cites, as \pn macros.

write_pn(pn, file.path(dir_pn, "pn_livelihoods.tex"))

cat("\nlivelihood distributions:\n")
print(as.data.frame(t5a))
cat("\nincome adequacy:\n")
print(as.data.frame(t5b))
