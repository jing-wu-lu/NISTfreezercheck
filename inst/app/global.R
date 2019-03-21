if (!exists("demo_mode")) demo_mode <- FALSE
prev_wd <- getwd()
dir_sep <- .Platform$file.sep
dir_pack <- system.file(package = "NISTfreezercheck")
dir_pack_demo <- paste(dir_pack, "data", "demo", sep = dir_sep)
# dir_user <- read_rds(paste(dir_pack, "data", "data_dir.RDS", sep = dir_sep))
source(paste(dir_pack, "app", "modals.R", sep = dir_sep))
if (demo_mode) {
  setwd(dir_pack_demo)
  dir_pause <- paste(dir_pack_demo, "data", "demo", "paused_audits", sep = dir_sep)
  demo_data <- read_rds("demo_data.RDS")
  fields <- read_rds("demo_fields.RDS")
  DSN <- NULL
  audit_schedule_period <- read_rds("demo_schedule.RDS")
  discrepancies <- read_rds("demo_discrepancies.RDS")
  audits <- read_rds("demo_audits.RDS")
  audit_history_summary <- read_rds("demo_audit_history_summary.RDS")
  progress <- read_rds("demo_progress.RDS")
  freezer_list <- read_rds("demo_freezer_list.RDS")
  staff <- read_rds("demo_staff.RDS")
  typical_identifying_columns <- read_rds("demo_typical_identifying_columns.RDS")
} else {
  setwd(dir_user)
  dir_pause <- paste(dir_user, "data", "paused_audits", sep = dir_sep)
  fields <- read_rds("fields.RDS")
  DSN <- read_rds("DSN.RDS")
  audit_schedule_period <- read_rds("schedule.RDS")
  discrepancies <- read_rds("discrepancies.RDS")
  audits <- read_rds("audits.RDS")
  audit_history_summary <- read_rds("audit_history_summary.RDS")
  progress <- read_rds("progress.RDS")
  freezer_list <- get_freezers(DSN)
  staff <- read_rds("staff.RDS")
  typical_identifying_columns <- read_rds("typical_identifying_columns.RDS")
}
setwd(prev_wd)
pretty_colnames <- names(audits)
pretty_colnames[6:10] <- c("GUAID",
                           "Freezer Section",
                           "Position 1",
                           "Position 2",
                           "Position 3")
pretty_colnames <- pretty_colnames[-c(1:3)]
aud_index <- max(audit_history_summary$audit_index)
bar_width <- '180px'
