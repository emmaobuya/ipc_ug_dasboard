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
  - **Data Source**: auto-connects live to ODK Central on load, no login
    required by whoever opens the dashboard (credentials are configured
    once, server-side). A CSV file-upload checkbox is available as a
    fallback/testing option.

## 0. Ready-to-use test files (no setup needed)

Two files are included so you can test the dashboard immediately:
- **`ipc_test_data_uganda.csv`** — your real sample data, already in the
  right format. Select **Uganda (15-domain XLSForm)** under Data Format,
  then upload this.
- **`ipc_test_data_drc.csv`** — the DRC file you shared, usable as-is
  (see the note below on what's approximate about it). Select
  **DRC (17-domain)** under Data Format, then upload this.

## 0.5. Two data formats are now supported

The sidebar has a **Data Format** selector above Data Source:
- **Uganda (15-domain XLSForm)** — the original build, unchanged.
- **DRC (17-domain)** — a different national version of the IPC RAT tool
  entirely (17 domains, not 15, already pre-scored, French-language field
  names). All three tabs work with either format, since both feed the same
  downstream structure.

**Important limitation on DRC data**: the DRC export has no accompanying
domain-name/max-points documentation (Uganda's XLSForm gave us that).
Overall score and status (Critical/At Risk/Ready) are exact, since
`score_pct` is already provided directly in the file. But **domain-level**
percentages (the heatmap, Domain Change chart, Single Assessment Snapshot)
estimate each domain's max as the highest score observed in your uploaded
data — a real ceiling might be higher, which would make DRC domain-level
colors/percentages read too favorably. If you can get the DRC scorecard's
actual domain names and max-points-per-domain (the DRC equivalent of
Uganda's XLSForm), send it over and I'll wire in exact values.

## 1. Install packages

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "readr", "lubridate",
  "stringr", "tibble", "DT", "plotly", "leaflet", "curl", "jsonlite"
))
```

## 2. Configure your ODK Central credentials

The dashboard reads five environment variables — set these locally (for
testing) and later on whatever platform hosts it (e.g. Posit Connect
Cloud's "Environment Variables" section, same as your Kobo/REDCap
dashboards):

```
ODK_URL=https://your-central-server.org
ODK_PROJECT=4
ODK_FORM=your_xmlFormId
ODK_EMAIL=dashboard-viewer@yourorg.org
ODK_PASSWORD=that_accounts_password
```

For local testing, put these in a `.Renviron` file in the project folder
(same pattern as the other dashboards). `ODK_EMAIL` must be a **staff
User account** — App User tokens can only submit data, not read it. Worth
asking your ODK Central admin for a dedicated read-only "Viewer" role
account for this dashboard, rather than using a personal admin login,
since the credentials live in this config permanently now (not typed
per-session by each viewer).

## 3. Test it locally first

```r
setwd("path/to/ipc_dashboard")
shiny::runApp("app.R")
```

No login screen — it connects automatically on load using the env vars
above, and the sidebar shows a live status line (green = connected with a
submission count and last-synced time, red = an error message). It
re-polls every 60 seconds (`REFRESH_SECONDS` in `ipc_helpers.R`) — no
manual refresh needed once a new submission comes in from the field.

The "Use a CSV file instead of the live connection" checkbox in the
sidebar is still there as a fallback (e.g. `ipc_test_data_uganda.csv` or
`ipc_test_data_drc.csv`) — useful for testing without hitting the live
server, or if the connection is ever down.

## 4. How the connection works, and what can go wrong

The fetch logs into ODK Central (`POST /v1/sessions`) to get a session
token, then downloads the flat submissions CSV
(`GET /v1/projects/{id}/forms/{formId}/submissions.csv`) — the same shape
as a manual "Export Submissions" download, so it reuses the exact same
cleaning code as file upload. This repeats on every poll (fresh login
each time), so there's no session-expiry issue to worry about.

Since this runs as a normal server-hosted Shiny app (not the offline
shinylive version), **CORS is not a concern** — the request happens on
the server, not in anyone's browser. The realistic failure modes are:
- **Wrong URL/Project ID/Form ID** → clear error in the status line
- **Wrong credentials** → 401/403 error in the status line
- **The hosting platform can't reach your ODK Central server at all**
  (firewall, private network) → times out after 20-30 seconds with a
  clear message, rather than hanging silently

## 5. Host it

Same Posit Connect Cloud process as your Kobo/REDCap dashboards: push
`app.R` and `ipc_helpers.R` to a GitHub repo, `rsconnect::writeManifest()`,
publish from Connect Cloud with the five `ODK_*` variables set as
Environment Variables in the publish dialog (not in the code).

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

