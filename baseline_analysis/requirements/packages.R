
# Define list of packages -------------------------------------------------

packages <- c("here",
              "odbc",
              "DBI",
              "readr",
              "dplyr",
              "ggplot2",
              "scales",
              "kableExtra"
)

# Load packages -----------------------------------------------------------

lapply(packages, library, character.only=TRUE)
