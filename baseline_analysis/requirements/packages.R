
# Define list of packages -------------------------------------------------

packages <- c("here",
              "odbc",
              "DBI",
              "readr",
              "dplyr",
              "ggplot2",
              "scales",
              "kableExtra",
              "stringr"
)

# Load packages -----------------------------------------------------------

lapply(packages, library, character.only=TRUE)
