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
# ODK Central connection config -- set these as environment variables on
# whatever platform hosts this app (e.g. Posit Connect Cloud's "Environment
# Variables" section), NOT hardcoded here. This replaces the earlier
# runtime-login form: credentials are configured once by whoever deploys the
# app, and every team member who opens the dashboard just sees live data --
# no login required on their end.
# ------------------------------------------------------------------------------
ODK_URL      <- Sys.getenv("ODK_URL", unset = "https://mohodk.dataug.net")
ODK_PROJECT  <- Sys.getenv("ODK_PROJECT", unset = "31")
ODK_FORM     <- Sys.getenv("ODK_FORM", unset = "Uganda EVD IPC Scorecard V2 (3)")
ODK_TOKEN    <- Sys.getenv("ODK_TOKEN", unset = "dwhxWN6P7AKB9AbxM!BxphB3GRoyUr4Y8qCGCb6pGumWMKKjU68y3dN2vejv8pQL")

# ------------------------------------------------------------------------------
# Alternative data source: a plain CSV file at a public URL (e.g. a Google
# Drive "share link" direct-download URL), kept up to date by a scheduled
# script (export_to_drive.R) running somewhere that DOES have access to ODK
# Central. Use this when Connect Cloud can't reach ODK Central directly
# (firewall) but a machine on your own network can. If DATA_URL is set, the
# dashboard polls that instead of connecting to ODK Central directly.
# ------------------------------------------------------------------------------
DATA_URL <- Sys.getenv("DATA_URL", unset = "https://drive.google.com/uc?export=download&id=1TorhTESpSdk3LmrMfXu-sgJuuOXySMKD")

fetch_csv_from_url <- function(url) {
  h <- new_handle()
  handle_setopt(h, timeout = 30, connecttimeout = 10, followlocation = TRUE)

  resp <- tryCatch(
    curl_fetch_memory(url, handle = h),
    error = function(e) stop("Could not reach the data file's URL within 30 seconds: ", conditionMessage(e))
  )

  if (resp$status_code >= 400) {
    stop("Fetching the data file failed (status ", resp$status_code, "). If this is a Google Drive link, double-check the file is still shared as 'Anyone with the link'.")
  }

  read_csv(rawToChar(resp$content), col_types = cols(.default = "c"), show_col_types = FALSE)
}

REFRESH_SECONDS <- 60  # how often the dashboard re-polls ODK Central

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
      assessor_name = as.character(get_col(raw, "Identification-Assessor_name")),
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
build_domain_long <- function(df, domains_table = DOMAINS) {
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
    left_join(domains_table %>% select(id, label), by = c("domain_id" = "id")) %>%
    mutate(category = score_to_category(pct))
}

# ------------------------------------------------------------------------------
# DRC variant of the IPC RAT -- a genuinely different scorecard (17 domains,
# not Uganda's 15), already pre-scored in a flat French-language CSV. No
# choice-decoding needed here (scores are already numeric), but critically:
# the source file has NO domain names or max-points documentation attached
# (unlike Uganda, which had its XLSForm). Domain labels below are placeholder
# numbers, and each domain's "max" is ESTIMATED as the highest score
# observed for that domain in the uploaded data -- NOT a real ceiling. This
# means DRC domain-level percentages/colors are approximate, for testing
# layout only, until real domain names + max points are provided (the DRC
# equivalent of Uganda's XLSForm) -- overall score/category is NOT affected
# by this, since score_pct is already given directly in the file.
DOMAINS_DRC <- tibble(
  id = 1:17,
  label = paste0("Domain ", 1:17, " (score_s", 1:17, ") -- name unknown, see caveat"),
  raw_col = paste0("score_s", 1:17)
)

DRC_LEVEL_LOOKUP <- c(
  "primaire"   = "Primary",
  "secondaire" = "Secondary",
  "tertiaire"  = "Tertiary"
)

clean_ipc_data_drc <- function(raw) {
  get_col <- function(df, name) if (name %in% names(df)) df[[name]] else NA

  df <- raw %>%
    mutate(
      facility = as.character(get_col(raw, "nom_etablissement")),
      region = as.character(get_col(raw, "province")),
      district = as.character(get_col(raw, "district")),
      subcounty = as.character(get_col(raw, "sous_district")),
      facility_level = decode_choice(get_col(raw, "niveau_installation"), DRC_LEVEL_LOOKUP),
      authority = NA_character_,
      hf_type = NA_character_,
      assessment_round = NA_character_,
      assessor_name = NA_character_,
      date_of_assessment = suppressWarnings(mdy(get_col(raw, "date_evaluation"))),
      lat = suppressWarnings(as.numeric(get_col(raw, "_coordonnees_gps_latitude"))),
      lon = suppressWarnings(as.numeric(get_col(raw, "_coordonnees_gps_longitude"))),
      overall_score = suppressWarnings(as.numeric(get_col(raw, "score_total_brut"))),
      overall_total = NA_real_,
      overall_pct = suppressWarnings(as.numeric(get_col(raw, "score_pct"))),
      overall_category = score_to_category(overall_pct)
    ) %>%
    filter(!is.na(facility), facility != "")

  for (i in seq_len(nrow(DOMAINS_DRC))) {
    d <- DOMAINS_DRC[i, ]
    score_val <- suppressWarnings(as.numeric(get_col(df, d$raw_col)))
    est_max <- suppressWarnings(max(score_val, na.rm = TRUE))
    if (!is.finite(est_max) || est_max <= 0) est_max <- NA
    pct_val <- ifelse(!is.na(est_max), round(100 * score_val / est_max, 1), NA)
    df[[paste0("domain_", d$id, "_score")]] <- score_val
    df[[paste0("domain_", d$id, "_max")]] <- est_max
    df[[paste0("domain_", d$id, "_pct")]] <- pct_val
  }

  df
}

# Computes each facility's assessment sequence based on the ACTUAL
# chronological order of submissions -- not the self-reported
# "intro-assessment" field (Baseline/Follow_1/...), which depends on the
# assessor picking the right option and can be wrong. The first-ever
# submission for a facility (by date) is always Baseline; every submission
# after that is a repeat, numbered 1, 2, 3... in the order they happened.
compute_assessment_sequence <- function(df) {
  if (nrow(df) == 0) {
    return(df %>% mutate(assessment_seq = integer(0), is_repeat = logical(0), followup_number = integer(0)))
  }

  dated <- df %>% filter(!is.na(facility), facility != "", !is.na(date_of_assessment))
  undated <- df %>% filter(is.na(facility) | facility == "" | is.na(date_of_assessment))

  dated <- dated %>%
    arrange(facility, date_of_assessment) %>%
    group_by(facility) %>%
    mutate(
      assessment_seq = row_number(),
      is_repeat = assessment_seq > 1,
      followup_number = ifelse(is_repeat, assessment_seq - 1, NA_integer_)
    ) %>%
    ungroup()

  undated <- undated %>%
    mutate(assessment_seq = NA_integer_, is_repeat = NA, followup_number = NA_integer_)

  bind_rows(dated, undated)
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
#
# Groups/joins by facility NAME ONLY (not also region/district/subcounty) --
# same reasoning as latest_per_facility(): real field data can have the
# same facility recorded with slightly inconsistent geography text across
# submissions, and critically, a join on multiple columns where ANY of them
# is NA for a given row will silently fail to match at all (NA never equals
# NA in a join) -- meaning any submission missing a district/subcounty
# value would be dropped from Summary View / Facility Deep Dive entirely,
# even though the facility name itself was fine. Joining on facility name
# alone avoids both problems.
compute_baseline_scoped <- function(df, use_custom = FALSE, target_date = NULL, buffer_weeks = 2) {
  if (nrow(df) == 0) return(df %>% mutate(baseline_date = as.Date(NA)))

  if (!use_custom) {
    baselines <- df %>%
      filter(!is.na(date_of_assessment), !is.na(facility), facility != "") %>%
      group_by(facility) %>%
      summarise(baseline_date = min(date_of_assessment), .groups = "drop")
  } else {
    if (is.null(target_date) || is.na(target_date)) return(df[0, ] %>% mutate(baseline_date = as.Date(NA)))
    window_start <- target_date - weeks(buffer_weeks)
    window_end   <- target_date + weeks(buffer_weeks)
    baselines <- df %>%
      filter(!is.na(date_of_assessment), !is.na(facility), facility != "",
             date_of_assessment >= window_start, date_of_assessment <= window_end) %>%
      group_by(facility) %>%
      summarise(baseline_date = min(date_of_assessment), .groups = "drop")
  }

  df %>%
    inner_join(baselines, by = "facility") %>%
    filter(date_of_assessment >= baseline_date)
}

# ------------------------------------------------------------------------------
# ODK Central live connection
# ------------------------------------------------------------------------------
# Uses a personal access token (from the user's own Account/Profile page in
# Central) directly as a Bearer token -- no login step needed, same
# one-token pattern as KOBO_TOKEN. Fetches the flat root-table CSV export
# (same shape as a manual "Export Submissions" download, and identical to
# what clean_ipc_data() already expects).
#
# Uses `curl` rather than `httr2` deliberately -- httr2's strict rlang
# version requirement caused real problems in our other two dashboards, and
# `curl` avoids that dependency entirely.
#
# NOTE ON TOKEN LIFETIME: unlike Kobo's long-lived API tokens, this kind of
# token in ODK Central may expire after a period of time (exact lifetime
# depends on your Central version/configuration). If fetches that used to
# work suddenly start failing with a 401/403, the fix is simply to
# regenerate the token from the same Account/Profile page and update
# ODK_TOKEN -- not a sign anything is broken.
#
# DEPLOYMENT-DEPENDENT CAVEAT:
# - If deployed as a normal server-hosted Shiny app (e.g. Posit Connect
#   Cloud, as tested so far), this request runs ON THE SERVER, so CORS does
#   not apply. The real risks here are network reachability (can Connect
#   Cloud's servers actually reach your ODK Central server?), a wrong
#   URL/Project/Form ID, or an expired/wrong token -- all of which now
#   surface as a clear error/timeout rather than a silent hang.
# - If exported to shinylive (runs in the browser instead), THEN this
#   becomes a cross-origin request and CORS would apply. Not a concern for
#   the current server-hosted deployment.
odk_fetch_submissions_csv <- function(server, project_id, form_id, token) {
  server <- sub("/+$", "", server)

  csv_url <- paste0(server, "/v1/projects/", utils::URLencode(as.character(project_id), reserved = TRUE),
                     "/forms/", utils::URLencode(as.character(form_id), reserved = TRUE), "/submissions.csv")
  data_h <- new_handle()
  handle_setheaders(data_h, "Authorization" = paste("Bearer", token))
  handle_setopt(data_h, timeout = 30, connecttimeout = 10)

  data_resp <- tryCatch(
    curl_fetch_memory(csv_url, handle = data_h),
    error = function(e) stop("Could not reach ODK Central within 30 seconds (network, firewall, or wrong URL): ", conditionMessage(e))
  )

  if (data_resp$status_code >= 400) {
    stop("ODK Central data fetch failed (status ", data_resp$status_code, "): ", rawToChar(data_resp$content))
  }

  read_csv(rawToChar(data_resp$content), col_types = cols(.default = "c"), show_col_types = FALSE)
}

# Latest assessment per facility (within whatever rows are passed in) -- the
# basis for every Outbreak Response chart/table, since it's a cross-facility
# "current status" snapshot rather than a trend view.
#
# Groups by facility NAME ONLY (not also region/district/subcounty). Real
# field data can have the same facility recorded with slightly inconsistent
# geography text across different submissions (a typo, an "Other" entry
# recorded differently, extra whitespace) -- grouping by all four fields
# together would then treat those as different facilities and produce two
# "latest" rows for the same facility, which crashes any chart that assumes
# facility names are unique (e.g. a plot axis). Grouping by name alone
# guarantees exactly one row per facility.
latest_per_facility <- function(df) {
  df %>%
    filter(!is.na(date_of_assessment), !is.na(facility), facility != "") %>%
    group_by(facility) %>%
    slice_max(date_of_assessment, n = 1, with_ties = FALSE) %>%
    ungroup()
}
