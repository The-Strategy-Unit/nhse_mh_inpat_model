packages <- c("here",
              "odbc",
              "DBI",
              "readr"
)

lapply(packages, library, character.only=TRUE)