# ==============================================================================
# Ebola IPC Assessment Dashboard -- data model & cleaning helpers
# ==============================================================================
# Built from the Uganda EVD IPC Scorecard XLSForm (survey/choices sheets) and
# a real ODK Central CSV export. ODK Central already computes every
# `calculate`-type field server-side, so the export includes each domain's
# raw score directly (Score-PCI1C ... Score-PCI15C) plus the overall
# Score-PCI_Score / Score-PCI_total / Score-PCI_Score100 -- we read those,
# we don't recompute them from the raw yes/no items.
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(tibble)
library(curl)
library(jsonlite)

# ------------------------------------------------------------------------------
# Choice-code lookups (from the XLSForm "choices" sheet) -- ODK Central's raw
# CSV export uses these codes, not labels, for select_one fields.
# ------------------------------------------------------------------------------
HFTYPE_LOOKUP <- c(
  "evd_treatment_center"            = "EVD Treatment Center",
  "not_dedicated_to_evd_treatment"  = "Not dedicated to EVD treatment",
  "Mixed_HF_ETC_and_non_ETC"        = "Mixed HF (ETC and non ETC)",
  "Pharmacy"                        = "Pharmacy"
)

HFLEVEL_LOOKUP <- c(
  "national_referral_hospital__nrh" = "National Referral Hospital (NRH)",
  "regional_referral_hospital__rrh" = "Regional Referral Hospital (RRH)",
  "district_hospital__gh"           = "District Hospital (GH)",
  "health_center_iv__hc_iv"         = "Health Center IV (HC IV)",
  "health_centre_three__hc_iii"     = "Health Centre Three (HC III)",
  "health_centre_two__hc_ii"        = "Health Centre Two (HC II)"
)

AUTHORITY_LOOKUP <- c(
  "Government "            = "Government",
  "Private_Not_For_Profit" = "Private Not For Profit",
  "Private_For_Profit"     = "Private For Profit"
)

ASSESS_LOOKUP <- c(
  "Baseline"      = "Baseline",
  "Follow_1"      = "Follow-up 1",
  "Follow_2"      = "Follow-up 2",
  "Follow_3"      = "Follow-up 3",
  "Follow_4"      = "Follow-up 4",
  "Follow_5"      = "Follow-up 5",
  "Test_training" = "Test/Training"
)

decode_choice <- function(x, lookup) {
  out <- lookup[as.character(x)]
  ifelse(is.na(out), as.character(x), unname(out))
}

# ------------------------------------------------------------------------------
# The 15 IPC domains: which raw-score column holds each one, and its max
# possible points. A handful of domains have an NA-adjustable denominator
# (max_col), where ODK Central already computed the adjusted max for us;
# the rest have a fixed max (max_fixed).
# ------------------------------------------------------------------------------
DOMAINS <- tibble(
  id = 1:15,
  label = c(
    "1. IPC Leadership",
    "2. Staff Training",
    "3. Screening Capacity",
    "4. Isolation Capacity",
    "5. Hand Hygiene",
    "6. PPE",
    "7. Injection Safety",
    "8. Environmental Cleaning & Disinfection",
    "9. Decontamination of Equipment",
    "10. Inpatient Surveillance",
    "11. HCW Post-Exposure Management",
    "12. Bed Occupancy / Hygiene / Sanitation",
    "13. Water Supply & Storage",
    "14. Waste Segregation",
    "15. Waste Elimination"
  ),
  score_col = paste0("Score-PCI", 1:15, "C"),
  max_col   = c(NA, NA, NA, NA, NA, NA, NA,
                "Score-PCI8C_calc", "Score-PCI9C_calc", "Score-PCI10C_calc",
                NA, "Score-PCI12C_calc", NA, NA, NA),
  max_fixed = c(3, 2, 7, 6, 4, 1, 4, NA, NA, NA, 1, NA, 2, 2, 3)
)

# ------------------------------------------------------------------------------
# Score -> category, per the guide's universal thresholds
# ------------------------------------------------------------------------------
score_to_category <- function(pct) {
  case_when(
    is.na(pct) ~ NA_character_,
    pct <= 50  ~ "Critical",
    pct <= 79  ~ "At Risk",
    TRUE       ~ "Ready"
  )
}
CATEGORY_COLORS <- c("Critical" = "#dc3545", "At Risk" = "#ffc107", "Ready" = "#28a745")
CATEGORY_LEVELS <- c("Critical", "At Risk", "Ready")

# ------------------------------------------------------------------------------
# Main cleaning pipeline: raw ODK Central export -> analysis-ready table
# ------------------------------------------------------------------------------
clean_ipc_data <- function(raw) {

  get_col <- function(df, name) if (name %in% names(df)) df[[name]] else NA

  df <- raw %>%
    mutate(
      facility_raw   = as.character(get_col(raw, "Identification-hfname")),
      facility_other = as.character(get_col(raw, "Identification-hfname_other")),
      facility = ifelse(facility_raw %in% c("99", "other") & !is.na(facility_other) & facility_other != "",
                         facility_other, facility_raw),
      region     = as.character(get_col(raw, "Identification-region")),
      district   = as.character(get_col(raw, "Identification-district")),
      subcounty  = as.character(get_col(raw, "Identification-subcounty")),
      facility_level = decode_choice(get_col(raw, "Identification-Facility_Level"), HFLEVEL_LOOKUP),
      authority      = decode_choice(get_col(raw, "Identification-Authority"), AUTHORITY_LOOKUP),
      hf_type        = decode_choice(get_col(raw, "intro-Type_of_Health_Facility"), HFTYPE_LOOKUP),
      assessment_round = decode_choice(get_col(raw, "intro-assessment"), ASSESS_LOOKUP),
      date_of_assessment = suppressWarnings(as_date(get_col(raw, "Identification-Date_of_assessment"))),
      lat = suppressWarnings(as.numeric(get_col(raw, "Identification-geo_location-Latitude"))),
      lon = suppressWarnings(as.numeric(get_col(raw, "Identification-geo_location-Longitude"))),
      overall_score = suppressWarnings(as.numeric(get_col(raw, "Score-PCI_Score"))),
      overall_total = suppressWarnings(as.numeric(get_col(raw, "Score-PCI_total"))),
      overall_pct   = suppressWarnings(as.numeric(get_col(raw, "Score-PCI_Score100"))),
      overall_category = score_to_category(overall_pct)
    )

  # Per-domain score / max / percent, added as domain_<id>_score / _max / _pct
  for (i in seq_len(nrow(DOMAINS))) {
    d <- DOMAINS[i, ]
    score_val <- suppressWarnings(as.numeric(get_col(df, d$score_col)))
    max_val <- if (!is.na(d$max_col)) suppressWarnings(as.numeric(get_col(df, d$max_col))) else d$max_fixed
    pct_val <- ifelse(!is.na(max_val) & max_val > 0, round(100 * score_val / max_val, 1), NA)
    df[[paste0("domain_", d$id, "_score")]] <- score_val
    df[[paste0("domain_", d$id, "_max")]] <- max_val
    df[[paste0("domain_", d$id, "_pct")]] <- pct_val
  }

  # Facilities that never receive the scorecard at all (per the form's own
  # relevant-condition logic) -- excluded so they don't show as false "0%"s
  df <- df %>%
    filter(!is.na(facility), facility != "",
           !(hf_type %in% c("Pharmacy", "EVD Treatment Center")))

  df
}

# Reshapes the wide domain_<id>_score/_max/_pct columns into one row per
# facility x domain -- used by the domain heatmap and dispatch table.
build_domain_long <- function(df) {
  df %>%
    select(facility, region, district, subcounty, facility_level,
           date_of_assessment, assessment_round,
           starts_with("domain_")) %>%
    pivot_longer(
      cols = starts_with("domain_"),
      names_to = c("domain_id", ".value"),
      names_pattern = "domain_(\\d+)_(.*)"
    ) %>%
    mutate(domain_id = as.integer(domain_id)) %>%
    left_join(DOMAINS %>% select(id, label), by = c("domain_id" = "id")) %>%
    mutate(category = score_to_category(pct))
}

# ------------------------------------------------------------------------------
# Baseline anchoring
# ------------------------------------------------------------------------------
# use_custom = FALSE (default): each facility's own earliest assessment is
# its baseline.
# use_custom = TRUE: baseline is the earliest assessment falling inside
# [target_date - buffer_weeks, target_date + buffer_weeks] for each
# facility. Facilities with no assessment in that window are dropped
# entirely (per the guide: "Pre-baseline IPC Assessments are excluded").
# Returns the input rows filtered to baseline-onward, with a baseline_date
# column added.
compute_baseline_scoped <- function(df, use_custom = FALSE, target_date = NULL, buffer_weeks = 2) {
  if (nrow(df) == 0) return(df %>% mutate(baseline_date = as.Date(NA)))

  if (!use_custom) {
    baselines <- df %>%
      filter(!is.na(date_of_assessment)) %>%
      group_by(facility, region, district, subcounty) %>%
      summarise(baseline_date = min(date_of_assessment), .groups = "drop")
  } else {
    if (is.null(target_date) || is.na(target_date)) return(df[0, ] %>% mutate(baseline_date = as.Date(NA)))
    window_start <- target_date - weeks(buffer_weeks)
    window_end   <- target_date + weeks(buffer_weeks)
    baselines <- df %>%
      filter(!is.na(date_of_assessment), date_of_assessment >= window_start, date_of_assessment <= window_end) %>%
      group_by(facility, region, district, subcounty) %>%
      summarise(baseline_date = min(date_of_assessment), .groups = "drop")
  }

  df %>%
    inner_join(baselines, by = c("facility", "region", "district", "subcounty")) %>%
    filter(date_of_assessment >= baseline_date)
}

# ------------------------------------------------------------------------------
# ODK Central live connection
# ------------------------------------------------------------------------------
# ODK Central has no long-lived static API token like KoboToolbox -- staff
# accounts authenticate with email+password to get a session bearer token
# (App User tokens, by contrast, can only submit data, not read it, so
# they're not usable here). We log in, then fetch the flat root-table CSV
# export (same shape as a manual "Export Submissions" download, and
# identical to what clean_ipc_data() already expects) using that token.
#
# Uses `curl` rather than `httr2` deliberately -- httr2's strict rlang
# version requirement caused real problems in our other two dashboards, and
# `curl` avoids that dependency entirely.
#
# IMPORTANT CAVEAT: this performs a cross-origin browser request from
# wherever this shinylive app is hosted (e.g. GitHub Pages) to your ODK
# Central server. Browsers block that unless the server sends CORS headers
# permitting your dashboard's origin. Many ODK Central deployments don't
# have this enabled by default -- if the fetch fails with a network/CORS
# error (not an auth error), that's almost certainly the cause, and your
# server admin would need to enable CORS for your dashboard's URL. The
# file-upload path always works regardless of this.
odk_fetch_submissions_csv <- function(server, project_id, form_id, email, password) {
  server <- sub("/+$", "", server)

  # Step 1: log in to get a session bearer token
  login_h <- new_handle()
  handle_setheaders(login_h, "Content-Type" = "application/json")
  handle_setopt(login_h, postfields = toJSON(list(email = email, password = password), auto_unbox = TRUE))

  login_resp <- tryCatch(
    curl_fetch_memory(paste0(server, "/v1/sessions"), handle = login_h),
    error = function(e) stop("Could not reach ODK Central server (network or CORS issue): ", conditionMessage(e))
  )

  if (login_resp$status_code >= 400) {
    stop("ODK Central login failed (status ", login_resp$status_code, "): ", rawToChar(login_resp$content))
  }
  token <- fromJSON(rawToChar(login_resp$content))$token
  if (is.null(token)) stop("ODK Central login succeeded but returned no token -- unexpected response.")

  # Step 2: fetch the flat submissions CSV (root table only, no repeats --
  # this form has none, so that's fine here)
  csv_url <- paste0(server, "/v1/projects/", project_id, "/forms/", form_id, "/submissions.csv")
  data_h <- new_handle()
  handle_setheaders(data_h, "Authorization" = paste("Bearer", token))

  data_resp <- tryCatch(
    curl_fetch_memory(csv_url, handle = data_h),
    error = function(e) stop("Could not fetch submissions (network or CORS issue): ", conditionMessage(e))
  )

  if (data_resp$status_code >= 400) {
    stop("ODK Central data fetch failed (status ", data_resp$status_code, "): ", rawToChar(data_resp$content))
  }

  read_csv(rawToChar(data_resp$content), col_types = cols(.default = "c"), show_col_types = FALSE)
}

# Latest assessment per facility (within whatever rows are passed in) -- the
# basis for every Outbreak Response chart/table, since it's a cross-facility
# "current status" snapshot rather than a trend view.
latest_per_facility <- function(df) {
  df %>%
    filter(!is.na(date_of_assessment)) %>%
    group_by(facility, region, district, subcounty) %>%
    slice_max(date_of_assessment, n = 1, with_ties = FALSE) %>%
    ungroup()
}
