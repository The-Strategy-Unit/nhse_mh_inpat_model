library(tidyverse)   
library(janitor)
library(readxl)

# Aggregate baseline data ----

baseline_data <- 
  read_csv("mhsds_baseline_adm250102.csv") %>% 
  clean_names()


baseline_aggregate <- 
  baseline_data |>
  select(-lda_flag) %>% 
  rename(lda_flag = lda_flag_su) %>% 
  mutate(lda_flag = 
           case_when(lda_flag == 1 ~ "Yes",
                     TRUE ~ "No"),
         imd_quintile = 
           case_when(
             imd_decile %in% c(1, 2) ~ 1,
             imd_decile %in% c(3, 4) ~ 2,
             imd_decile %in% c(5, 6) ~ 3,
             imd_decile %in% c(7, 8) ~ 4,
             imd_decile %in% c(9, 10) ~ 5
           ),
         legal_status_group =
           case_when(
             legal_status_desc %in% c("Not Applicable",  NA) ~ "Not formally detained",
             TRUE ~ "Formally detained"
           ),
         discharge_month = 
           as.Date(paste0(
             str_sub(disch_date_hosp_prov_spell, 1,8), "01"),
             format = "%Y-%m-%d"),
         oap_flag =
           case_when(
             residence_icb_name == provider_icb_name ~ 0,
             
             residence_icb_name != provider_icb_name ~ 1,
             TRUE ~ 0
           )
  ) |>
  group_by(
    residence_icb_code,
    residence_icb_name,
    provider_icb_code,
    provider_icb_name,
    age_group_admission,
    gender,
    ethnic_category_2,
    imd_quintile,
    provider_type,
    legal_status_group,
    lda_flag,
    der_ward_type_desc_first,  # ? change with new data
    oap_flag
  ) |> 
  summarise(spell_count = n_distinct(record_number),
            bed_days = sum(reporting_hos_prov_spell_los), # including Home Leave periods
            bed_days_exHL = sum(der_los_ex_hl_in_rp),
            bed_days_delayed_days = sum(adj_reporting_delay_days)  
  ) |>
  ungroup() 

# Write aggregated baseline csv to read into shiny app
write_csv(baseline_aggregate, "baseline_aggregate.csv")

# Write ICB specific csv's of the baseline aggregate table ----

midlands_icbs <-
  tribble(
    ~icb_code, ~icb_name,
    "QHL", "QHL: NHS Birmingham And Solihull ICB",
    "QUA", "QUA: NHS Black Country ICB",
    "QWU", "QWU: NHS Coventry And Warwickshire ICB",
    "QNC", "QNC: NHS Staffordshire And Stoke-On-Trent ICB",
    "QT1", "QT1: NHS Nottingham And Nottinghamshire ICB",
    "QJ2", "QJ2: NHS Derby And Derbyshire ICB",
    "QK1", "QK1: NHS Leicester, Leicestershire And Rutland ICB",
    "QPM", "QPM: NHS Northamptonshire ICB",
    "QGH", "QGH: NHS Herefordshire And Worcestershire ICB",
    "QJM", "QJM: NHS Lincolnshire ICB",
    "QOC", "QOC: NHS Shropshire, Telford And Wrekin ICB"
  )

unique_values <- unique(midlands_icbs$icb_code)


# Loop through each unique value and write a CSV for each subset
for (value in unique_values) {
  subset_data <- 
    baseline_aggregate |> 
    filter(
      (residence_icb_code == value) |
        (provider_icb_code == value & oap_flag == 1)
    )
  
  write.csv(subset_data, paste0("icb_baseline_data/baseline_aggregate_", value, ".csv"), row.names = FALSE)
}
