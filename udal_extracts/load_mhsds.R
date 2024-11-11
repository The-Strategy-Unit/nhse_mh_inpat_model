
# Load Packages -----------------------------------------------------------

library(here)
source(paste0(here(), "/udal_extracts/packages.R"))


# UDAL Connection ---------------------------------------------------------

source(paste0(here(), "/udal_extracts/udal_connect.R"))

# Run Extract Script ------------------------------------------------------

DBI::dbExecute(con, statement = read_file(paste0(here(),"/udal_extracts/mhsds_baseline.sql")), immediate=TRUE)
mhsds_baseline_adm <- DBI::dbGetQuery(con, statement = read_file(paste0(here(),"/udal_extracts/read_admissions_output.sql")))
mhsds_baseline_ws <- DBI::dbGetQuery(con, statement = read_file(paste0(here(),"/udal_extracts/read_wardstays_output.sql")))


# Drop Temps --------------------------------------------------------------

DBI::dbExecute(con, statement = read_file(paste0(here(),"/udal_extracts/drop_temps.sql")), immediate=TRUE)

write.csv(x = mhsds_baseline_adm, file = paste0(here(), "/udal_extracts/data/mhsds_baseline_adm.csv"), row.names = FALSE)
write.csv(x = mhsds_baseline_ws, file = paste0(here(), "/udal_extracts/data/mhsds_baseline_ws.csv"), row.names = FALSE)
          