rsconnect::deployApp(
  
  appFiles = c(
    
    # Add any additional files unique to your app here.
    "app.R",
    "demographic_projections/icb_weighted_demographic_change.csv",
    "www/styles.css",
    "www/tsu_logo_black.png"
    
  ),
  
  appId = rsconnect::deployments(".")$appID,
  
  lint = FALSE,
  
  forceUpdate = TRUE
  
)

