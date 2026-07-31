# ==============================================================================
# Export ODK Central data straight to Google Drive (via the Drive API)
# ==============================================================================
# Run this on a machine that already has working access to ODK Central
# (yours, right now). It downloads the latest submissions and uploads them
# as a CSV directly into your Drive folder -- no Google Drive for Desktop
# app or local sync folder needed, just an internet connection. The
# dashboard (on Posit Connect Cloud, which can't reach ODK Central
# directly) then reads that file from a public Drive link instead.
#
# Your folder: https://drive.google.com/drive/folders/1z7jrnnaDR1B9bCitG0tmIpR2rNALvDg9
#
# SET UP ONCE:
#   1. install.packages("googledrive")
#   2. Run this script manually (not via Task Scheduler yet). The first
#      time, it will open your browser asking you to log into Google and
#      approve access -- do that once. It caches the login afterward, so
#      future runs (including scheduled ones) won't ask again.
#   3. After that first successful run, follow "GETTING THE SHAREABLE
#      LINK" below -- only needed once.
#   4. Schedule it -- see "SCHEDULING" at the bottom.
# ==============================================================================
options(gargle_oauth_cache = FALSE)
library(googledrive)
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

setwd("C:/Users/Personal/Desktop/Work/ipc dashboard")   # your actual folder path

source("ipc_helpers.R")  # reuses ODK_URL / ODK_PROJECT / ODK_FORM / ODK_TOKEN
                          # and odk_fetch_submissions_csv() from the main app

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------
DRIVE_FOLDER_ID  <- "1z7jrnnaDR1B9bCitG0tmIpR2rNALvDg9"  # from your folder's URL
OUTPUT_FILENAME  <- "ipc_latest_export.csv"

# ------------------------------------------------------------------------------
# Authenticate (opens a browser the very first time; reuses a cached token
# on every run after that -- including headless/scheduled runs, as long as
# it's the same Windows user account). Requesting the full "drive" scope
# explicitly avoids reusing a narrower-scope cached token from elsewhere,
# which causes a 403 "insufficient authentication scopes" error.
# ------------------------------------------------------------------------------
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# ------------------------------------------------------------------------------
# Fetch fresh data from ODK Central
# ------------------------------------------------------------------------------
if (ODK_URL == "https://mohodk.dataug.net" || ODK_PROJECT == "31" || ODK_FORM == "Uganda EVD IPC Scorecard V2 (3)" || ODK_TOKEN == "dwhxWN6P7AKB9AbxM!BxphB3GRoyUr4Y8qCGCb6pGumWMKKjU68y3dN2vejv8pQL") {
  stop("ODK_URL / ODK_PROJECT / ODK_FORM / ODK_TOKEN aren't set. Check ipc_helpers.R or your .Renviron.")
}

cat("Fetching latest submissions from ODK Central...\n")
data <- odk_fetch_submissions_csv(ODK_URL, ODK_PROJECT, ODK_FORM, ODK_TOKEN)
cat("Fetched", nrow(data), "rows.\n")

tmp <- tempfile(fileext = ".csv")
write.csv(data, tmp, row.names = FALSE, na = "")

# ------------------------------------------------------------------------------
# Upload -- updates the existing file if this has run before (keeping the
# same shareable link stable), or creates it on the very first run.
# ------------------------------------------------------------------------------



existing <- drive_ls(as_id(DRIVE_FOLDER_ID), pattern = OUTPUT_FILENAME)

if (nrow(existing) > 0) {
  drive_update(existing$id[1], media = tmp)
  file_id <- existing$id[1]
  cat("Updated existing file in Drive.\n")
} else {
  uploaded <- drive_upload(tmp, path = as_id(DRIVE_FOLDER_ID), name = OUTPUT_FILENAME)
  drive_share(uploaded, role = "reader", type = "anyone")
  file_id <- uploaded$id
  cat("Created new file in Drive and set sharing to 'Anyone with the link can view'.\n")
}

file.remove(tmp)

cat("\nDone. Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Direct-download URL for the dashboard's DATA_URL:\n")
cat("https://drive.google.com/uc?export=download&id=", file_id, "\n", sep = "")

# ==============================================================================
# GETTING THE SHAREABLE LINK
# ==============================================================================
# The URL printed above (ending in &id=...) is exactly what goes into the
# dashboard's DATA_URL environment variable -- copy it once, after the
# FIRST successful run, into:
#   - ipc_helpers.R's DATA_URL line, for local testing, and
#   - Connect Cloud's Environment Variables, when you publish
# It stays valid on every future run, since the file gets updated in place
# rather than replaced.
#
# If it's ever NOT working, double check the file's sharing in Drive
# directly: right-click ipc_latest_export.csv -> Share -> confirm it's set
# to "Anyone with the link" / "Viewer".
# ==============================================================================

# ==============================================================================
# SCHEDULING (Windows Task Scheduler) -- runs this automatically, e.g. every
# 15 minutes, so the dashboard stays close to real-time without you doing
# anything manually.
# ==============================================================================
# 1. Open Task Scheduler (search for it in the Start menu).
# 2. Click "Create Task..." (not "Create Basic Task", so you get more options).
# 3. General tab: give it a name like "IPC Dashboard Data Export".
#    Check "Run whether user is logged on or not" if you want it to run even
#    when you're not actively using the computer.
# 4. Triggers tab: click New -> "Daily", repeat every 15 minutes, indefinitely.
# 5. Actions tab: click New -> Action: "Start a program".
#      Program/script:  Rscript.exe
#      (find the full path by running `Sys.which("Rscript")` in R if unsure,
#      usually something like C:\Program Files\R\R-4.x.x\bin\Rscript.exe)
#      Add arguments:   "export_to_drive.R"
#      Start in:        C:\Users\Personal\Desktop\Work\ipc dashboard
#      (the folder containing this script and ipc_helpers.R)
# 6. Save, entering your Windows password if prompted.
# 7. Test it: right-click the task in the list -> Run, then refresh the
#    Drive folder in your browser to confirm the file's "Last modified"
#    timestamp updated.
#
# IMPORTANT: since authentication was already done interactively (step 2 of
# SET UP ONCE), scheduled runs should NOT prompt for login again -- if a
# scheduled run ever fails specifically at the drive_auth() step, the
# cached token may have been cleared; just run the script manually once
# more to re-authenticate.
#
# This still only works while your computer is on (or at least running,
# depending on your Task Scheduler power settings) -- it's a
# personal-machine workaround, not a server. If this needs to run reliably
# 24/7 regardless of your computer's state, that's the point where hosting
# properly inside the network becomes worth the extra setup.
# ==============================================================================
