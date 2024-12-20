# Mental health inpatient model - working script

library(tidyverse)                     # layout and display functions
library(StrategyUnitTheme)        # corporate colours
library(DT)
library(janitor)
library(waterfalls) # https://www.rdocumentation.org/packages/waterfalls/versions/1.0.0/topics/waterfall 
library(readxl)
library(rlang)

setwd("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Work/1. Projects/2024_25/mental_health_shiny_app/nhse_mh_inpat_model")

# To-do (/developments): ----

# Confidence intervals around growth factors
# Fix SU logo in top right corner



# Aggregate baseline data ----

baseline_data <- 
  read_csv("mhsds_baseline_adm_241120.csv") |> 
  clean_names()

baseline_aggregate <- 
  baseline_data |>
  mutate(imd_quintile = 
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


# Out of area flag / table ----

# Table design
# icb - count - internal spells, outgoing and incoming
  
baseline_oap_activity_icb <-
  baseline_aggregate |> 
  #filter(residence_icb_code == "QGH") |> 
  mutate(ooa_group = 
           case_when(
             oap_flag == 0 ~ "not_oap",
             oap_flag == 1 & residence_icb_code == "QGH" ~ "oap_outgoing",
             oap_flag == 1 & residence_icb_code != "QGH" ~ "oap_incoming"
           )) |> 
  group_by(ooa_group) |> 
  summarise(baseline_spells = sum(spell_count)) |> 
  mutate(ooa_group = 
           case_when(
             ooa_group == "not_oap" ~ "1. Not OOA Placement",
             ooa_group == "oap_outgoing" ~ "2. Outgoing OOAP",
             ooa_group == "oap_incoming" ~ "3. Incoming OOAP"
           )) |>
  mutate(project_spells = 
           case_when(
             ooa_group == "2. Outgoing OOAP" ~ baseline_spells * 0.4,
             ooa_group == "3. Incoming OOAP" ~ baseline_spells * (0.4*-1)
           )) |> 
  pivot_longer(-ooa_group) |>
  arrange(ooa_group) |> 
  pivot_wider(id_cols = name,
              names_from = ooa_group,
              values_from = value) 
  

# Write ICB specific csv's of the baseline aggregate table ----

unique_values <- unique(baseline_aggregate$residence_icb_code)

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


# Apply growth factors ---- 

demographic_growth = 0.03
incidence_change = 0.07 
acuity_change = 0.067
social_care_pressures = 0.066 
mha_changes = -0.05
national_policy = -0.03
service_models = -0.015
prevention_programme = -0.015
admission_avoidance = -0.015
waiting_list_reduction = -0.02
ooa_repat = 0.4
shift_to_ip = -0.03

baseline_growth <-
  baseline_aggregate |>
  mutate(ooa_group = 
           case_when(
             oap_flag == 0 ~ "not_oap",
             oap_flag == 1 & residence_icb_code == "QGH" ~ "oap_outgoing",
             oap_flag == 1 & residence_icb_code != "QGH" ~ "oap_incoming"
           )) |> 
  mutate(sp_demographic_growth       =  spell_count * (demographic_growth),
         sp_incidence_change         =  spell_count * (incidence_change),
         sp_acuity_change            =  spell_count * (acuity_change),
         sp_social_care_pressures    =  spell_count * (social_care_pressures),
         sp_mha_changes              =  spell_count * (mha_changes),
         sp_national_policy          =  spell_count * (national_policy),
         sp_service_models           =  spell_count * (service_models),
         sp_prevention_programme     =  spell_count * (prevention_programme),
         sp_admission_avoidance      =  spell_count * (admission_avoidance),
         sp_waiting_list_reduction   =  spell_count * (waiting_list_reduction),
         #sp_ooa_repat                = case_when(oop_flag == 1 ~ spell_count * (ooa_repat), TRUE ~ 0),
         sp_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ spell_count*ooa_repat, TRUE ~ 0),
         sp_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ spell_count*(ooa_repat*-1), TRUE ~ 0),
         sp_shift_to_ip              = case_when(provider_type == "Independent" ~ spell_count * (shift_to_ip), TRUE ~ 0),
         
         bd_demographic_growth       =  bed_days * (demographic_growth),
         bd_incidence_change         =  bed_days * (incidence_change),
         bd_acuity_change            =  bed_days * (acuity_change),
         bd_social_care_pressures    =  bed_days_delayed_days * (social_care_pressures),  # switch to bed days - delayed discharges
         bd_mha_changes              =  bed_days * (mha_changes),
         bd_national_policy          =  bed_days * (national_policy),
         bd_service_models           =  bed_days * (service_models),
         bd_prevention_programme     =  bed_days * (prevention_programme),
         bd_admission_avoidance      =  bed_days * (admission_avoidance),
         bd_waiting_list_reduction   =  bed_days * (waiting_list_reduction),
         #bd_ooa_repat                =  case_when(oop_flag == 1 ~ bed_days * (ooa_repat), TRUE ~ 0),
         bd_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ bed_days*ooa_repat, TRUE ~ 0),
         bd_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ bed_days*(ooa_repat*-1), TRUE ~ 0),
         bd_shift_to_ip              =  case_when(provider_type == "Independent" ~ bed_days * (shift_to_ip), TRUE ~ 0),
         
         exHL_bedday_demographic_growth       =  bed_days_exHL * (demographic_growth),
         exHL_bedday_incidence_change         =  bed_days_exHL * (incidence_change),
         exHL_bedday_acuity_change            =  bed_days_exHL * (acuity_change),
         exHL_bedday_social_care_pressures    =  bed_days_delayed_days * (social_care_pressures),
         exHL_bedday_mha_changes              =  bed_days_exHL * (mha_changes),
         exHL_bedday_national_policy          =  bed_days_exHL * (national_policy),
         exHL_bedday_service_models           =  bed_days_exHL * (service_models),
         exHL_bedday_prevention_programme     =  bed_days_exHL * (prevention_programme),
         exHL_bedday_admission_avoidance      =  bed_days_exHL * (admission_avoidance),
         exHL_bedday_waiting_list_reduction   =  bed_days_exHL * (waiting_list_reduction),
         #exHL_bedday_ooa_repat                =  case_when(oop_flag == 1 ~ bed_days_exHL * (ooa_repat), TRUE ~ 0),
         exHL_bedday_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ bed_days_exHL*ooa_repat, TRUE ~ 0),
         exHL_bedday_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ bed_days_exHL*(ooa_repat*-1), TRUE ~ 0),
         
         exHL_bedday_shift_to_ip              =  case_when(provider_type == "Independent" ~ bed_days_exHL * (shift_to_ip), TRUE ~ 0)
         ) |> 
  mutate(spell_proj = spell_count + rowSums(across(contains("sp_"))),
         bed_days_proj =    bed_days + rowSums(across(contains("bd_"))),
         bed_days_exHL_proj = bed_days_exHL + rowSums(across(contains("exHL_bedday_")))
  ) 



# Summarise growth at ICB level
waterfall_data <-
  baseline_growth |> 
  #filter(residence_icb_code == "QGH") |> # function input
  #group_by(residence_icb_name) |>
  summarise(spell_count = sum(spell_count),
            bed_days = sum(bed_days),
            bed_days_exHL = sum(bed_days_exHL),
            
            sp_demographic_growth     = sum(sp_demographic_growth    ),
            sp_incidence_change       = sum(sp_incidence_change      ),
            sp_acuity_change          = sum(sp_acuity_change         ),
            sp_social_care_pressures  = sum(sp_social_care_pressures ),
            sp_mha_changes            = sum(sp_mha_changes           ),
            sp_national_policy        = sum(sp_national_policy       ),
            sp_service_models         = sum(sp_service_models        ),
            sp_prevention_programme   = sum(sp_prevention_programme  ),
            sp_admission_avoidance    = sum(sp_admission_avoidance   ),
            sp_waiting_list_reduction = sum(sp_waiting_list_reduction),
            sp_ooa_repat_outgoing     = sum(sp_oap_repat_outgoing    ),
            sp_ooa_repat_incoming     = sum(sp_oap_repat_incoming    ),
            sp_shift_to_ip            = sum(sp_shift_to_ip           ),
            
            bd_demographic_growth     = sum(bd_demographic_growth    ),
            bd_incidence_change       = sum(bd_incidence_change      ),
            bd_acuity_change          = sum(bd_acuity_change         ),
            bd_social_care_pressures  = sum(bd_social_care_pressures ),
            bd_mha_changes            = sum(bd_mha_changes           ),
            bd_national_policy        = sum(bd_national_policy       ),
            bd_service_models         = sum(bd_service_models        ),
            bd_prevention_programme   = sum(bd_prevention_programme  ),
            bd_admission_avoidance    = sum(bd_admission_avoidance   ),
            bd_waiting_list_reduction = sum(bd_waiting_list_reduction),
            bd_ooa_repat_outgoing     = sum(bd_oap_repat_outgoing ),
            bd_ooa_repat_incoming     = sum(bd_oap_repat_incoming ),
            bd_shift_to_ip            = sum(bd_shift_to_ip           ),
            
            exHL_bedday_demographic_growth     = sum(exHL_bedday_demographic_growth    ),
            exHL_bedday_incidence_change       = sum(exHL_bedday_incidence_change      ),
            exHL_bedday_acuity_change          = sum(exHL_bedday_acuity_change         ),
            exHL_bedday_social_care_pressures  = sum(exHL_bedday_social_care_pressures ),
            exHL_bedday_mha_changes            = sum(exHL_bedday_mha_changes           ),
            exHL_bedday_national_policy        = sum(exHL_bedday_national_policy       ),
            exHL_bedday_service_models         = sum(exHL_bedday_service_models        ),
            exHL_bedday_prevention_programme   = sum(exHL_bedday_prevention_programme  ),
            exHL_bedday_admission_avoidance    = sum(exHL_bedday_admission_avoidance   ),
            exHL_bedday_waiting_list_reduction = sum(exHL_bedday_waiting_list_reduction),
            exHL_bedday_ooa_repat_outgoing     = sum(exHL_bedday_oap_repat_outgoing    ),
            exHL_bedday_ooa_repat_incoming     = sum(exHL_bedday_oap_repat_incoming    ),
            exHL_bedday_shift_to_ip            = sum(exHL_bedday_shift_to_ip           ),
            
            spell_proj = sum(spell_proj),
            bed_days_proj = sum(bed_days_proj),
            bed_days_exHL_proj = sum(bed_days_exHL_proj)
  ) |> 
  mutate(icb_dummy = "ICB") |> 
  pivot_longer(cols = -icb_dummy)

# Plot waterfall  
data <-
  waterfall_data |>
  select(-icb_dummy) |> 
  filter(name == "spell_count" | 
           str_detect(name, "sp_")) |> 
  mutate(name = 
           case_when(
             name == "spell_count"                 ~ "A. Baseline year (2024)",
             name == "sp_demographic_growth"       ~ "B. Demographic growth",
             name == "sp_incidence_change"         ~ "C. Incidence change",
             name == "sp_acuity_change"            ~ "D. Acuity change",
             name == "sp_social_care_pressures"    ~ "E. Social care pressures",
             name == "sp_mha_changes"              ~ "F. Mental health act changes",
             name == "sp_national_policy"          ~ "G. National policy",
             name == "sp_service_models"           ~ "H. Service models",
             name == "sp_prevention_programme"     ~ "I. Prevention programme",
             name == "sp_admission_avoidance"      ~ "J. Admission avoidance",
             name == "sp_waiting_list_reduction"   ~ "K. Waiting list reduction",
             name == "sp_ooa_repat_outgoing"       ~ "L. Out of area repatriation - outgoing",
             name == "sp_ooa_repat_incoming"       ~ "M. Out of area repatriation - incoming",
             name == "sp_shift_to_ip"              ~ "N. Shift to independent sector",
             name == "spell_proj"                  ~ "O. Projection"  
           )) |>
  arrange(name) |> 
  mutate(colour = 
           case_when(name == "A. Baseline year (2024)" ~ "#686f73",
                     value >= 0 ~ "#f9bf07",
                     value < 0 ~ "#ec6555")) 

  waterfall(data,
            calc_total = TRUE,
            total_axis_text = "Projection (2028)",
            rect_text_size = 2,
            fill_by_sign = FALSE, 
            fill_colours = data$colour
            ) +
  su_theme() +
  theme(axis.text.x = element_text(angle = 90)) +
  #scale_fill_manual(values = c("red","blue")) +
  labs(x = "Growth factor",
       y = "Spells",
       title = "Example waterfall plot",
       #subtitle = paste0("Mental health inpatient model | ", icb_filter)
  )

# Plot waterfall for bed days:
waterfall_data |>
  select(-icb_dummy) %>%
  filter(name == "bed_days" | str_detect(name, "bd_")) %>% 
  mutate(name = case_when(
    name == "bed_days"                ~ "A. Baseline year (2024)",
    name == "bd_demographic_growth"      ~ "B. Demographic growth",
    name == "bd_incidence_change"        ~ "C. Incidence change",
    name == "bd_acuity_change"           ~ "D. Acuity change",
    name == "bd_social_care_pressures"   ~ "E. Social care pressures",
    name == "bd_mha_changes"             ~ "F. Mental health act changes",
    name == "bd_national_policy"         ~ "G. National policy",
    name == "bd_service_models"          ~ "H. Service models",
    name == "bd_prevention_programme"    ~ "I. Prevention programme",
    name == "bd_admission_avoidance"     ~ "J. Admission avoidance",
    name == "bd_waiting_list_reduction"  ~ "K. Waiting list reduction",
    name == "bd_ooa_repat_outgoing"      ~ "L. Out of area repatriation - outgoing",
    name == "bd_ooa_repat_incoming"      ~ "M. Out of area repatriation - incoming",
    name == "bd_shift_to_ip"             ~ "N. Shift to independent sector",
    name == "bed_day_proj"               ~ "O. Projection"
  )) %>%
  mutate(value = round(value, 1)) |> 
  arrange(name) %>%
  waterfall(calc_total = TRUE, 
            total_axis_text = "Projection (2028)", 
            rect_text_size = 1.6) +
  su_theme() +
  theme(axis.text.x = element_text(angle = 90, size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 18)
  )+
  labs(x = "Growth factor",
       y = "Bed days",
       title = "Example waterfall plot",
       #subtitle = paste0("Mental health inpatient model | ", input$icb)
       subtitle = "Mental health inpatient model | 2024 baseline projection to 2028"
  )

# Plot waterfall excluding Home Leave:
waterfall_data |> 
  select(-residence_icb_name) %>% 
  filter(name == "bed_days_exHL" | str_detect(name, "exHL_bedday_")) %>% View()
  mutate(name = case_when(
    name == "bed_days_exHL"                ~ "A. Baseline year (2024)",
    name == "exHL_bedday_demographic_growth"      ~ "B. Demographic growth",
    name == "exHL_bedday_incidence_change"        ~ "C. Incidence change",
    name == "exHL_bedday_acuity_change"           ~ "D. Acuity change",
    name == "exHL_bedday_social_care_pressures"   ~ "E. Social care pressures",
    name == "exHL_bedday_mha_changes"             ~ "F. Mental health act changes",
    name == "exHL_bedday_national_policy"         ~ "G. National policy",
    name == "exHL_bedday_service_models"          ~ "H. Service models",
    name == "exHL_bedday_prevention_programme"    ~ "I. Prevention programme",
    name == "exHL_bedday_admission_avoidance"     ~ "J. Admission avoidance",
    name == "exHL_bedday_waiting_list_reduction"  ~ "K. Waiting list reduction",
    name == "exHL_bedday_ooa_repat"               ~ "L. Out of area (OOA)",
    name == "exHL_bedday_shift_to_ip"             ~ "M. Shift to independent sector",
    name == "bed_day_proj"               ~ "N. Projection"
  )) %>%
  mutate(value = round(value, 1)) |> 
  arrange(name) %>%
  waterfall(calc_total = TRUE, 
            total_axis_text = "Projection (2028)", 
            rect_text_size = 1.6) +
  su_theme() +
  theme(axis.text.x = element_text(angle = 90, size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 18)
  )+
  labs(x = "Growth factor",
       y = "Bed days",
       title = "Example waterfall plot",
       #subtitle = paste0("Mental health inpatient model | ", input$icb)
       subtitle = "Mental health inpatient model | 2024 baseline projection to 2028"
  )


# Apply occupancy rate to bed days ----
# Annualised beds (Baseline occupancy rate / occupancy rate) / 365.25 

current_occupancy <- 0.95
planned_occupancy <- 0.8

waterfall_data |>
  filter(residence_icb_name == "QGH: NHS Herefordshire And Worcestershire ICB") |> 
  filter(str_detect(name, "bed_days")) |> 
  mutate(beds_annualised = 
           case_when(
             name %in% c("bed_days", "bed_days_exHL") ~ (value/current_occupancy)/365.25,
             name %in% c("bed_days_proj", "bed_days_exHL_proj") ~ (value/planned_occupancy)/365.25
             )
         ) |> 
  mutate(name = 
           case_when(
             name == "bed_days" ~ "Baseline - bed days",
             name == "bed_days_exHL" ~ "Baseline - bed days excl home leave",
             name == "bed_days_proj" ~ "Projected - bed days",
             name == "bed_days_exHL_proj" ~ "Projected - bed days excl home leave"
           )) |> 
  rename(ICB = residence_icb_name,
         Measure = name,
         `Bed days` = value,
         `Annualised beds` = beds_annualised)



# Project sub-group activity ----

baseline_growth |> 
  filter(residence_icb_code == "QGH") |> 
  group_by(age_group_admission) |> 
  summarise(spell_count = sum(spell_count),
            bed_days = sum(bed_days),
            
            spell_proj = sum(spell_proj),
            bed_days_proj = sum(bed_days_proj)
  ) |>
  rename(group_name = 1) |>
  pivot_longer(-group_name) |> 
  mutate(flag = case_when(str_detect(name, "spell_") ~ "1. Spells", TRUE ~ "2. Bed days"),
         current_projection = case_when(str_detect(name, "proj") ~ "Projection", TRUE ~ "Current")
  ) |> 
  
  ggplot(aes(x = group_name, y = value, fill = current_projection)) +
  geom_col(position = "dodge") +
  facet_wrap(~flag, scale = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme(strip.background = element_rect(fill = NA, colour = "grey")) +
  labs(x = "Age group",
       y = "",
       fill = "",
       title = "Sub-group projections",
       subtitle = "Mental health inpatient model | 2024 baseline projection to 2028")


sub_group_plot <- function(icb, group) {
  
  baseline_growth |> 
    filter(residence_icb_code ==  icb) |> 
    group_by({{group}}) |> 
    summarise(spell_count = sum(spell_count),
              bed_days = sum(bed_days),
              
              spell_proj = sum(spell_proj),
              bed_days_proj = sum(bed_days_proj)
    ) |>
    pivot_longer(-{{group}}) |> 
    mutate(flag = case_when(str_detect(name, "spell_") ~ "1. Spells", TRUE ~ "2. Bed days"),
           current_projection = case_when(str_detect(name, "proj") ~ "Projection", TRUE ~ "Current")
    ) |> 
    rename(group_name = 1) |> 
    
    ggplot(aes(x = group_name, y = value, fill = current_projection)) +
    geom_col(position = "dodge") +
    facet_wrap(~flag, scale = "free_y") +
    theme(strip.background = element_rect(fill = NA, colour = "grey")) +
    labs(x =  "Sub-group",
         y = "",
         fill = "",
         title = "Sub-group projections",
         subtitle = "Mental health inpatient model | 2024 baseline projection to 2028")
  
  
}

sub_group_plot("QGH", age_group_admission)
sub_group_plot("QGH", gender)
sub_group_plot("QGH", ethnic_category_2)
sub_group_plot("QGH", imd_quintile)
sub_group_plot("QGH", provider_type)




# Apply age and sex specific demographic growth projections ----

# icb populations by age and sex 
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates   

pop_estimates_icb_mid_22 <- 
  read_excel("demographic_projections/pop_estimates_icb_mid-22.xlsx", 
             sheet = "Mid-2022 ICB 2023", 
             skip = 3) |> 
  clean_names()

icb_population_age_range_sex <-
  pop_estimates_icb_mid_22 |> 
  #filter(icb_2023_code == "E54000061") |> 
  select(-total) |> 
  pivot_longer(cols = -c("sicbl_2023_code",
                         "sicbl_2023_name",
                         "icb_2023_code",
                         "icb_2023_name",
                         "nhser_2023_code",
                         "nsher_2023_name")) |> 
  mutate(sex = case_when(str_detect(name, "f") ~ "female",
                         str_detect(name, "m") ~ "male"
  ),
  name = as.numeric(str_remove_all(name, "[fm]"))) |> 
  rename(age = name) |> 
  mutate(age_range = 
           cut(age, 
               breaks = seq(0, 90, by = 5), 
               right = FALSE, 
               labels = paste(seq(0, 85, by = 5), 
                              seq(4, 89, by = 5), sep = "-"))
  ) |> 
  mutate(age_range = 
           case_when(is.na(age_range) ~ "90+",
                     TRUE ~ age_range)
  ) |> 
  group_by(icb_2023_code, icb_2023_name, sex, age_range) |> 
  summarise(value = sum(value)) |> 
  ungroup()


# Read in local authority population projections
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/localauthoritiesinenglandtable2 
pop_projections_la_2018_based_males <- 
  read_excel("demographic_projections/pop_projections_la_2018_based.xls", 
             sheet = "Males", 
             skip = 6) |> 
  clean_names()

pop_projections_la_2018_based_females <- 
  read_excel("demographic_projections/pop_projections_la_2018_based.xls", 
             sheet = "Females", 
             skip = 6) |> 
  clean_names()




pop_projections_la_2018_based_males |> 
  filter(!area %in% c("England",
                      "North East",
                      "North West",
                      "Yorkshire and The Humber",
                      "East Midlands",
                      "West Midlands",
                      "East",
                      "London",
                      "South East",
                      "South West"
  ),
  !str_detect(area, "Met County"),
  age_group != "All ages"
  ) |> 
  mutate(age_group_2 = 
           case_when(
             age_group %in% c("0-4", 
                              "5-9",
                              "10-14") ~ "00-17",
             age_group %in% c("15-19",
                              "20-24") ~ "18-24",
             age_group %in% c("25-29", 
                              "30-34",     
                              "35-39",    
                              "40-44",    
                              "45-49",    
                              "50-54",    
                              "55-59",    
                              "60-64") ~ "25-64",
             age_group %in% c("65-69",    
                              "70-74",    
                              "75-79",    
                              "80-84",    
                              "85-89",    
                              "90+" ) ~ "65+"
           )) |> 
  select(-age_group) |> 
  pivot_longer(cols = -c(code, area, age_group_2)) |> 
  mutate(year = as.numeric(str_remove_all(name, "x"))) |> 
  filter(year %in% 2024:2028) |> 
  select(-name) |> 
  group_by(code, area, age_group_2, year) |>
  summarise(value = sum(value, na.rm = T)) |> 
  group_by(code, area, age_group_2) |> 
  mutate(proj_perc_change = (value - lag(value)) / lag(value)) |>  #? x100
  ungroup() |> 
  select(-value)




# Calculate projected population change (+based on MH data age ranges)
proj_perc_change_la_function <- function(data) {
  
  data |> 
    filter(!area %in% c("England",
                        "North East",
                        "North West",
                        "Yorkshire and The Humber",
                        "East Midlands",
                        "West Midlands",
                        "East",
                        "London",
                        "South East",
                        "South West"
    ),
    !str_detect(area, "Met County"),
    age_group != "All ages"
    ) |> 
    pivot_longer(cols = -c(code, area, age_group)) |> 
    mutate(year = as.numeric(str_remove_all(name, "x"))) |> 
    filter(year %in% 2024:2028) |> 
    select(-name) |> 
    group_by(code, area, age_group) |> 
    mutate(proj_perc_change = (value - lag(value)) / lag(value) * 100) |>  #? x100
    ungroup() |> 
    select(-value)
}

proj_perc_change_la_function_2 <- function(data) {
  
  data |> 
    filter(!area %in% c("England",
                        "North East",
                        "North West",
                        "Yorkshire and The Humber",
                        "East Midlands",
                        "West Midlands",
                        "East",
                        "London",
                        "South East",
                        "South West"
    ),
    !str_detect(area, "Met County"),
    age_group != "All ages"
    ) |> 
    mutate(age_group_2 = 
             case_when(
               age_group %in% c("0-4", 
                                "5-9",
                                "10-14") ~ "00-17",
               age_group %in% c("15-19",
                                "20-24") ~ "18-24",
               age_group %in% c("25-29", 
                                "30-34",     
                                "35-39",    
                                "40-44",    
                                "45-49",    
                                "50-54",    
                                "55-59",    
                                "60-64") ~ "25-64",
               age_group %in% c("65-69",    
                                "70-74",    
                                "75-79",    
                                "80-84",    
                                "85-89",    
                                "90+" ) ~ "65+"
             )) |> 
    select(-age_group) |> 
    pivot_longer(cols = -c(code, area, age_group_2)) |> 
    mutate(year = as.numeric(str_remove_all(name, "x"))) |> 
    filter(year %in% 2024:2028) |> 
    select(-name) |> 
    group_by(code, area, age_group_2, year) |>
    summarise(value = sum(value, na.rm = T)) |> 
    group_by(code, area, age_group_2) |> 
    mutate(proj_perc_change = (value - lag(value)) / lag(value)) |>  #? x100
    ungroup() |> 
    select(-value)
  
}


proj_perc_change_la_m <- proj_perc_change_la_function(pop_projections_la_2018_based_males)
proj_perc_change_la_f <- proj_perc_change_la_function(pop_projections_la_2018_based_females)

proj_perc_change_la_m_age_group_2 <- proj_perc_change_la_function_2(pop_projections_la_2018_based_males)
proj_perc_change_la_f_age_group_2 <- proj_perc_change_la_function_2(pop_projections_la_2018_based_females)


# Link ICB to LA - look up
lsoa_lookup <- 
  read_csv("demographic_projections/lsoa11_21_la_2022.csv") |> 
  clean_names() |> 
  select(lsoa11cd, lsoa21cd, lad22cd, lad22nm)

lsoa_icb_lad <-
  read_csv("demographic_projections/lsoa21_sicb_icb_lad23.csv") |> 
  clean_names() |> 
  select(lsoa21cd, icb23cd, icb23nm, lad23cd, lad23nm)


# Aggregate to demographic change figure for each ICB - projections weighted according to LA overlap in each ICB
icb_lad_baseline_spells <-
  baseline_aggregate |>
  left_join(lsoa_lookup, by = c("lsoa2011" = "lsoa11cd")) |> 
  left_join(lsoa_icb_lad, by = c("lsoa21cd")) |> 
  group_by(residence_icb_name, lad23nm, age_group_admission, gender) |> 
  summarise(spell_count = sum(spell_count)) |> 
  ungroup()

icb_weighted_demographic_change <-
  icb_lad_baseline_spells |>
  left_join(proj_perc_change_la_m_age_group_2 |>
              drop_na(proj_perc_change) |> 
              group_by(area, age_group_2) |> 
              summarise(proj_perc_change = sum(proj_perc_change)) |> 
              mutate(gender = "1"),
            by = c("lad23nm" = "area",
                   "age_group_admission" = "age_group_2",
                   "gender")) |> 
  left_join(proj_perc_change_la_f_age_group_2 |>
              drop_na(proj_perc_change) |> 
              group_by(area, age_group_2) |> 
              summarise(proj_perc_change = sum(proj_perc_change)) |> 
              mutate(gender = "2"),
            by = c("lad23nm" = "area",
                   "age_group_admission" = "age_group_2",
                   "gender")) |>
  mutate(perc_change_projection = 
           case_when(
             is.na(proj_perc_change.x) ~ proj_perc_change.y, 
             TRUE ~ proj_perc_change.x
           )) |> 
  select(-proj_perc_change.x, -proj_perc_change.y) |> 
  mutate(spell_count_28 = spell_count * (1+perc_change_projection)) |> 
  drop_na(perc_change_projection) |> 
  group_by(residence_icb_name) |> 
  summarise(weighted_perc_change = sum(perc_change_projection * spell_count) / sum(spell_count))



icb_lad_baseline_spells |>
  left_join(proj_perc_change_la_m_age_group_2 |>
              drop_na(proj_perc_change) |> 
              group_by(area, age_group_2) |> 
              summarise(proj_perc_change = sum(proj_perc_change)) |> 
              mutate(gender = "1"),
            by = c("lad23nm" = "area",
                   "age_group_admission" = "age_group_2",
                   "gender")) |> 
  left_join(proj_perc_change_la_f_age_group_2 |>
              drop_na(proj_perc_change) |> 
              group_by(area, age_group_2) |> 
              summarise(proj_perc_change = sum(proj_perc_change)) |> 
              mutate(gender = "2"),
            by = c("lad23nm" = "area",
                   "age_group_admission" = "age_group_2",
                   "gender")) |>
  mutate(perc_change_projection = 
           case_when(
             is.na(proj_perc_change.x) ~ proj_perc_change.y, 
             TRUE ~ proj_perc_change.x
           )) |> 
  select(-proj_perc_change.x, -proj_perc_change.y) |> 
  mutate(spell_count_28 = spell_count * (1+perc_change_projection)) |> 
  drop_na(perc_change_projection) |> 
  group_by(residence_icb_name, age_group_admission) |> 
  summarise(weighted_perc_change = sum(perc_change_projection * spell_count) / sum(spell_count))







write.csv(icb_weighted_demographic_change, 
          "demographic_projections/icb_weighted_demographic_change.csv")








calculate_growth_and_waterfall <- function(baseline_data, growth_factors, years, icb_name) {
  baseline_aggregate <- baseline_data %>%
    filter(residence_icb_name == icb_name) %>%
    summarise(year = as.integer(2024),
              spell_count = as.numeric(sum(spell_count)),
              bed_days = sum(bed_days))
  
  calculate_annual_growth <- function(initial_value, growth_rate, years) {
    initial_value * (1 + growth_rate) ^ years
  }
  
  # Calculate the contributions for each year
  for (year in years[-1]) {
    previous_year_data <- results %>% filter(year == year - 1)
    new_data <- previous_year_data
    
    for (factor in names(growth_factors)) {
      new_data <- new_data %>%
        mutate(
          spell_count = calculate_annual_growth(spell_count, growth_factors[factor]),
          bed_days = calculate_annual_growth(bed_days, growth_factors[factor])
        )
    }
    
    new_data <- new_data %>% mutate(year = year)
    results <- bind_rows(results, new_data)
  }
  
  waterfall_data <- baseline_aggregate %>%
    mutate(factor = "Baseline year (2024)") %>%
    union_all(
      results %>%
        pivot_longer(cols = -year, names_to = c("factor", ".value"), names_pattern = "(.*)_(.*)") %>%
        rename(bed_days = days, spell_count = spells)
    )
  
  return(waterfall_data)
}



calculate_growth_and_waterfall(baseline_aggregate, growth_factors, 2024:2028, "QHL: NHS Birmingham And Solihull ICB")











