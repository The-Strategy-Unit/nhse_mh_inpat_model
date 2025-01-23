
# Load Packages -----------------------------------------------------------

library(here)
source(paste0(here(), "/baseline_analysis/requirements/packages.R"))

# Connect to UDAL ---------------------------------------------------------

source(paste0(here(), "/baseline_analysis/load/udal_connect.R"))

# Create and Load Baseline Datasets ---------------------------------------

DBI::dbExecute(conn = con,
               statement = read_file(paste0(here(), "/baseline_analysis/load/create_baseline_mhsds.sql")),
               immediate=TRUE)
mhsds_baseline_adm <- DBI::dbGetQuery(conn = con,
                                      statement = read_file(paste0(here(), "/baseline_analysis/load/read_admissions.sql")))
mhsds_baseline_ws <- DBI::dbGetQuery(conn = con,
                                     statement = read_file(paste0(here(), "/baseline_analysis/load/read_ward_stays.sql")))

# Drop Temp Tables --------------------------------------------------------

DBI::dbExecute(conn = con,
               statement = read_file(paste0(here(), "/baseline_analysis/load/drop_temps.sql")),
               immediate=TRUE)
