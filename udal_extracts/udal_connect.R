# Script to connect to the UDAL data environment

server <- "udalsyndataprod.sql.azuresynapse.net"
database <- "UDAL_Warehouse"
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "ODBC Driver 17 for SQL Server", Server = server,
                      Database = database, Authentication = "ActiveDirectoryInteractive")