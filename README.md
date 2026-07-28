# Ebola IPC Assessment Dashboard — Setup Guide

## What's built

All three tabs, plus a live ODK Central connection:

- **`ipc_helpers.R`** — domain definitions, choice-code lookups, the
  cleaning pipeline, baseline-anchoring logic, and the ODK Central fetch
  function.
- **`app.R`** — the full dashboard:
  - **🚨 Outbreak Response** — indicator boxes, Readiness Index, Domain
    Scores heatmap, Facility Map, Dispatch Decision Table
  - **🌐 Summary View** — indicator boxes, Total Score Trajectory (with
    Time Axis switcher and trend filter), Change from Baseline heatmap,
    Facility Progress Summary table
  - **🏥 Facility Deep Dive** — facility/assessment selectors, indicator
    boxes, Single Assessment Snapshot, Domain Change diverging chart
  - **Custom baseline anchor** (sidebar) drives every baseline-dependent
    calculation across Summary View and Facility Deep Dive
  - **Data Source**: File Upload (always works, fully offline) or ODK
    Central API (live, browser-side — see the CORS note below)

## 1. Install packages

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "readr", "lubridate",
  "stringr", "tibble", "DT", "plotly", "leaflet", "curl", "jsonlite"
))
```

## 2. Test it as a normal Shiny app first

```r
setwd("path/to/ipc_dashboard")
shiny::runApp("app.R")
```

Try both data sources:
- **File Upload**: use a CSV export in the ODK Central "group-fieldname"
  format (save `Sample_dataset.xlsx` as CSV to test).
- **ODK Central API**: server URL, project ID, form ID (the `xmlFormId`,
  visible in Central's form settings), your email, and password. This
  must be a **staff User account** (App User tokens can't read data, only
  submit it) — consider asking your ODK Central admin for a dedicated
  read-only "Viewer" role account for this dashboard rather than using a
  personal admin login.

## 3. About the ODK Central live connection — read this before relying on it

The fetch works by logging into ODK Central (`POST /v1/sessions`) to get a
session token, then downloading the flat submissions CSV
(`GET /v1/projects/{id}/forms/{formId}/submissions.csv`) — the same shape
as a manual "Export Submissions" download, so it reuses the exact same
cleaning code as file upload.

**The real risk is CORS.** Since this request runs in your browser
against a different domain (your dashboard's URL vs. your ODK Central
server), the browser will block the response unless your ODK Central
server sends CORS headers permitting your dashboard's origin. Most
Central deployments don't have this enabled by default. If the fetch
fails with what looks like a network error (rather than a login/auth
error), that's almost certainly it — your server admin would need to
configure this, likely at the reverse-proxy (nginx) level in front of
Central. **File Upload always works regardless of this**, so it's the
reliable fallback.

I also haven't been able to test this against a real ODK Central server
myself (no server access, no R environment here) — treat this as a solid
first attempt built correctly against ODK's documented API, not something
verified end-to-end. Come back with whatever error you hit and I'll help
debug it.

## 4. Once it's working: export to shinylive

```r
install.packages("shinylive")
shinylive::export(appdir = "path/to/ipc_dashboard", destdir = "ipc_dashboard_site")
```

Preview locally:
```r
httpuv::runStaticServer("ipc_dashboard_site")
```

As before, I haven't been able to test this export step myself. `leaflet`,
`DT`, and `plotly` are all `htmlwidgets`-based and generally work in
shinylive; `curl` (used for the ODK connection) running inside
WebAssembly is the piece I'm least certain about — webR is documented to
proxy HTTP requests through the browser's `fetch()`, but if the ODK
Central option breaks specifically after export (while working fine in
step 2's normal Shiny test), that's the first place to look.

## 5. Host it

Same as the original tool — push `ipc_dashboard_site` to a GitHub repo and
enable GitHub Pages on it, giving you a
`https://<username>.github.io/<repo>/` link to share.

## A data quality note (carried over from the first build)

~31% of your sample data had facilities recorded as "Other" (code `99`),
with the real name in a separate field — handled automatically. Pharmacy
and EVD Treatment Center facility types are excluded from analysis
entirely, since the source form itself skips the whole IPC scorecard for
those types.

## What's still not built

- **Custom CSV column mapping** (for non-standard column names) — the
  original tool's "🔧 CSV Upload (Custom Mapping)" option
- **PowerPoint/Excel export buttons** on each tab
- **Language switcher** (English/Français/Español)

Happy to add any of these once the core three tabs are confirmed working
against your real data.

