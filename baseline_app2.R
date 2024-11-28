# Mental health inpatient model - shiny application

library(tidyverse)
library(shiny)                    # shiny core
library(shinydashboard)           # layout and display functions
library(StrategyUnitTheme)        # corporate colours
library(bslib)
library(DT)
library(janitor)
library(waterfalls) # https://www.rdocumentation.org/packages/waterfalls/versions/1.0.0/topics/waterfall 
library(shinyWidgets)
library(bslib)
library(readxl)
library(rlang)

setwd("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Work/1. Projects/2024_25/mental_health_shiny_app/nhse_mh_inpat_model")

# Set SU theme ----
SU_colours <- c (
  `orange`                     = grDevices::rgb(248,191,7, maxColorValue = 255),# "#f9bf07",
  `charcoal`                   = grDevices::rgb(44,40,37, maxColorValue = 255),# "#2c2825",
  `slate`                      = grDevices::rgb(104,111,115, maxColorValue = 255), # "#686f73",
  `blue`                       = grDevices::rgb(88,29,193, maxColorValue = 255), # "#5881c1",
  `red`                        = grDevices::rgb(236,101,85, maxColorValue = 255), # "#ec6555",
  #additional accent colours from word doc template
  `yellow`                     = grDevices::rgb(252,229,155, maxColorValue = 255),
  `grey`                       = grDevices::rgb(163,168,172, maxColorValue = 255),
  `white`                      = grDevices::rgb(255,255,255, maxColorValue = 255),
  #light and dark ends from colour theme in word doc
  `light orange`               = grDevices::rgb(253,242,205, maxColorValue = 255),
  `dark orange`                = grDevices::rgb(124,95,3, maxColorValue = 255),
  `light charcoal`             = grDevices::rgb(235,233,231, maxColorValue = 255),
  `dark charcoal`              = 	"#000000",#black
  `light slate`                = grDevices::rgb(224,226,227, maxColorValue = 255),
  `dark slate`                 = grDevices::rgb(51,55,57, maxColorValue = 255),
  `light blue`                 = grDevices::rgb(221,229,242, maxColorValue = 255),
  `dark blue`                  = grDevices::rgb(38,61,102, maxColorValue = 255),
  `light red`                  = grDevices::rgb(251,224,220, maxColorValue = 255),
  `dark red`                   = grDevices::rgb(144,29,16, maxColorValue = 255),
  `light yellow`               = grDevices::rgb(254,249,235, maxColorValue = 255),
  `dark yellow`                = grDevices::rgb(197,152,5, maxColorValue = 255),
  `light grey`                 = grDevices::rgb(236,237,238, maxColorValue = 255),
  `dark grey`                  = grDevices::rgb(79,84,88, maxColorValue = 255),
  `light white`                = grDevices::rgb(242,242,242, maxColorValue = 255),
  `dark white`                 = grDevices::rgb(127,127,127, maxColorValue = 255),
  `red2`                       = grDevices::rgb(215,25,28, maxColorValue = 255),
  `orange2`                    = grDevices::rgb(253,174,97, maxColorValue = 255),
  `yellow2`                    = grDevices::rgb(255,255,191, maxColorValue = 255),
  `green2`                     = grDevices::rgb(171,221,164, maxColorValue = 255),
  `blue2`                      = grDevices::rgb(43,131,186, maxColorValue = 255) #"#2b83ba"
)

SU_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (SU_colours)
  
  SU_colours[cols]
}

SU_palettes <- list(
  `main` = SU_cols("orange","charcoal","slate","blue","red"),
  `oranges` = SU_cols("light orange","orange","dark orange"),
  `slates` = SU_cols("light slate","slate","dark slate"),
  `mixed` = SU_cols("dark red","orange","yellow","light blue","slate"),
  `oj_coal` = SU_cols("yellow","orange","red","dark red","dark charcoal"),
  `oj_red` = SU_cols("yellow","orange","red","dark red"),
  `white_oj_coal` = SU_cols("white","yellow","orange","red","dark red","dark charcoal"),#added since shared
  `lyellow_oj_coal` = SU_cols("light yellow","orange","red","dark red","dark charcoal"),#added since shared
  `wy_oj_coal` = SU_cols("white","light yellow","yellow","orange","red","dark red","charcoal","dark charcoal"),
  `red_coal` = SU_cols("red","dark red","charcoal","dark charcoal"),
  `blue_yellow_red` = SU_cols("red2","orange2","yellow2","green2","blue2"),
  `red_yellow_blue` = SU_cols("blue2","green2","yellow2","orange2","red2")
)


SU_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- SU_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}  

theme_SU <-   function (base_size){
  theme_minimal(
    #base_family = "Segoe UI", 
    base_size=12
  ) %+replace% 
    theme(axis.title = element_text(size=11, face="bold",colour=SU_cols("charcoal")),
          plot.title = element_text(hjust=0,face="bold",size=12,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
          plot.subtitle = element_text(hjust=0,face="italic",size=10,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
          plot.caption = element_text(hjust = 0,face="italic",size=9,colour=SU_cols("slate"),margin=margin(b=4,unit="pt")),
          legend.text = element_text(size=10,colour=SU_cols("charcoal")),
          legend.title = element_text(face="bold",size=11,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")))
}

theme_set(theme_SU())
  
# Aggregate baseline data ----

baseline_data <- 
  read_csv("mhsds_baseline_adm_241120.csv") |> 
  clean_names()

baseline_aggregate <- 
  baseline_data |>
  mutate(year = year(disch_date_hosp_prov_spell),
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
             format = "%Y-%m-%d")
         ) |>
  group_by(
           year,
           residence_icb_code,
           residence_icb_name,
           age_group_admission,
           gender,
           ethnic_category_2,
           imd_quintile,
           provider_type,
           legal_status_group,
           lda_flag,
           der_ward_type_desc_first  # ? change with new data
           ) |> 
  summarise(spell_count = n_distinct(record_number),
            bed_days = sum(reporting_hos_prov_spell_los)) |> 
  ungroup() 

# Write aggregated baseline csv to read into shiny app
write_csv(baseline_aggregate, "baseline_aggregate.csv")


# Apply growth factors ---- 

demographic_growth = 0.03
incidence_change = 0.07 
acuity_change = 0.08
social_care_pressures = 0.05 
mha_changes = -0.01
national_policy = -0.01
service_models = -0.01
prevention_programme = -0.03
admission_avoidance = -0.04
waiting_list_reduction = -0.04
ooa_repat = 0.04
shift_to_ip = -0.03

baseline_growth <-
  baseline_aggregate |> 
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
         sp_ooa_repat                =  spell_count * (ooa_repat),
         sp_shift_to_ip              =  spell_count * (shift_to_ip),
         
         bd_demographic_growth       =  bed_days * (demographic_growth),
         bd_incidence_change         =  bed_days * (incidence_change),
         bd_acuity_change            =  bed_days * (acuity_change),
         bd_social_care_pressures    =  bed_days * (social_care_pressures),
         bd_mha_changes              =  bed_days * (mha_changes),
         bd_national_policy          =  bed_days * (national_policy),
         bd_service_models           =  bed_days * (service_models),
         bd_prevention_programme     =  bed_days * (prevention_programme),
         bd_admission_avoidance      =  bed_days * (admission_avoidance),
         bd_waiting_list_reduction   =  bed_days * (waiting_list_reduction),
         bd_ooa_repat                =  bed_days * (ooa_repat),
         bd_shift_to_ip              =  bed_days * (shift_to_ip)
         ) |> 
  mutate(spell_proj = spell_count + rowSums(across(contains("sp_"))),
         bed_days_proj =    bed_days + rowSums(across(contains("bd_")))
         ) 

# Summarise growth at ICB level
waterfall_data <-
  baseline_growth |> 
  filter(residence_icb_code == "QGH") |> # function input
  group_by(residence_icb_name) |>
  summarise(spell_count = sum(spell_count),
            bed_days = sum(bed_days),
            
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
            sp_ooa_repat              = sum(sp_ooa_repat             ),
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
            bd_ooa_repat              = sum(bd_ooa_repat             ),
            bd_shift_to_ip            = sum(bd_shift_to_ip           ),
            
            spell_proj = sum(spell_proj),
            bed_days_proj = sum(bed_days_proj)
            ) |> 
  pivot_longer(-residence_icb_name)
  
# Plot waterfall  
waterfall_data |>
  select(-residence_icb_name) |> 
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
             name == "sp_ooa_repat"                ~ "L. Out of area (OOA)",
             name == "sp_shift_to_ip"              ~ "M. Shift to independent sector",
             name == "spell_proj"                  ~ "N. Projection"  
           )) |>
  arrange(name) |> 
  
  waterfall(calc_total = TRUE,
            total_axis_text = "Projection (2028)",
            rect_text_size = 2
  ) +
  su_theme() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Growth factor",
       y = "Spells",
       title = "Example waterfall plot",
       #subtitle = paste0("Mental health inpatient model | ", icb_filter)
  )


# Apply occupancy rate to bed days
# Annualised (Baseline occupancy rate / occupancy rate) / 365.25 

current_occupancy <- 0.95
planned_occupancy <- 0.8

waterfall_data |> 
  filter(str_detect(name, "bed_days")) |> 
  mutate(bed_day_annualised = 
           case_when(
             name == "bed_days" ~ (value/current_occupancy)/365.25,
             name == "bed_days_proj" ~ (value/planned_occupancy)/365.25
             )
         )



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












# Define UI ----
ui <- navbarPage(
  "Mental health inpatient strategy",
  theme = bs_theme(bootswatch = "united",
                   primary = "#f9bf07",
                   secondary = "#686f73"),
  tags$head(
    tags$style(HTML("
      .negative-value {
        color: red !important;
      }

      .sidebar {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        height: 100% !important;
        overflow-y: auto !important;
        z-index: 1000 !important;
        background-color: #f8f9fa !important; /* Adjust the background color as needed */
        padding: 15px !important;
        border-right: 1px solid #ddd !important;
      }

      .main-content {
        margin-left: 250px !important; /* Adjust this value based on the width of your sidebar */
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        var sidebar = $('.sidebar');
        var offset = sidebar.offset();
        $(window).scroll(function() {
          if ($(window).scrollTop() > offset.top) {
            sidebar.css('top', $(window).scrollTop() - offset.top);
          } else {
            sidebar.css('top', '0');
          }
        });
      });
    "))
  ),
  tags$div(
    class = "logo",
    tags$img(src = "www/tsu_logo_yellow_screen_transparent.png", height = 70)
  ),
  
  tabPanel("Introduction",
           fluidPage(
             titlePanel("Welcome to the Growth Factor Projection Tool"),
             h3("Project Objectives"),
             p("This tool aims to project the expected volume of activity in 2028 based on various growth factors."),
             
             h3("Data Requirements"),
             p("Please upload a CSV file with the following fields:"),
             
             tableOutput("ExampleFormatTable_2"),
            
             h3("Instructions"),
             p("1. Upload your CSV file using the 'Upload CSV File' button."),
             p("2. Navigate to the 'Analysis' tab to generate the plot and table."),
             p("3. Adjust the growth factor values using the numeric input controls or use the default suggestions."),
             p("4. Examine the impact of varying growth factors on activity and export using 'Download Projected Data' button"),
             
             fileInput("file", "Upload CSV File", accept = ".csv")
           )),
  
  tabPanel("Metadata",
           fluidPage(
             titlePanel("Metadata and underlying assumptions:"),
             h3("Metadata"),
             p("The MHSDS data hosted within NCDR is our baseline datasource.
               Specified inclusion and exclusion criteria have been applied and are detailed below along with the format in which data exsists and has been aggregated."),
             
             h3("Growth factor asusmptions:"),
             p("Demographic growth facotr"),
             p("Demographic growth values are externally sourced from ONS population projections published at local authority level.
               We have extracted age and gender specific population projections which are applied to our ICB populations accordingly.
               As such, demographic growth is not modifiable in the analysis tab unlike our other growth factors.")
             )),
  
  tabPanel("Analysis",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 h3("Analysis Controls"),
                 p("Use the controls below to update the analysis."),
                 selectInput("icb", "Select ICB", 
                             choices = 
                               c(
                                 "QGH: NHS Herefordshire And Worcestershire ICB",
                                 "QHL: NHS Birmingham And Solihull ICB",
                                 "QJ2: NHS Derby And Derbyshire ICB",
                                 "QJM: NHS Lincolnshire ICB",
                                 "QK1: NHS Leicester, Leicestershire And Rutland ICB",
                                 "QNC: NHS Staffordshire And Stoke-On-Trent ICB",
                                 "QOC: NHS Shropshire, Telford And Wrekin ICB",
                                 "QPM: NHS Northamptonshire ICB",
                                 "QT1: NHS Nottingham And Nottinghamshire ICB",
                                 "QUA: NHS Black Country ICB",
                                 "QWU: NHS Coventry And Warwickshire ICB"
                               )
                 ),
                 
                 fluidRow(
                   column(6,
                          numericInput("incidence_change", "Incidence Change",           value = 0.07, step = 0.01),
                          numericInput("acuity_change", "Acuity Change",                 value = 0.08, step = 0.01),
                          numericInput("social_care_pressures", "Social Care Pressures", value = 0.05, step = 0.01),
                          numericInput("mha_changes", "Mental Health Act Changes",       value = -0.01, step = 0.01),
                          numericInput("national_policy", "National Policy",             value = -0.01, step = 0.01),
                          numericInput("service_models", "Service Models",               value = -0.01, step = 0.01)
                   ),
                   column(6,
                          numericInput("prevention_programme", "Prevention Programme",     value = -0.03, step = 0.01),
                          numericInput("admission_avoidance", "Admission Avoidance",       value = -0.04, step = 0.01),
                          numericInput("waiting_list_reduction", "Waiting List Reduction", value = -0.04, step = 0.01),
                          numericInput("ooa_repat", "Out of Area Repatriation",            value = 0.04, step = 0.01),
                          numericInput("shift_to_ip", "Shift to Independent setting",      value = -0.03, step = 0.01)
                          #actionButton("update", "Generate plot")
                          )
                   ),
                 
                 selectInput("group_selection", "Select grouping variable:", 
                             choices = 
                               c(
                                 "Age Group Admission" = "age_group_admission",
                                 "Gender" = "gender",
                                 "Ethnic Category" = "ethnic_category_2",
                                 "IMD Quintile" = "imd_quintile",
                                 "Provider Type" = "provider_type",
                                 "Legal Status Group" = "legal_status_group",
                                 #"LDA Flag" = "lda_flag",
                                 "Ward Type Description" = "der_ward_type_desc_first"
                                 )
                             ),
                 downloadButton("downloadData", "Download Projected Data")
               ),
               
               mainPanel(
                 h3("ICB Outputs"),
                 tabsetPanel(
                   tabPanel("Spells", plotOutput("waterfall_Plot", height = "700px", width = "1000px")),
                   tabPanel("Bed days", plotOutput("waterfall_Plot_bed_days", height = "700px", width = "1000px")),
                   tabPanel("Projection Table", DTOutput("dataTable"))
                 ),
                 
                 h6(
                   br(),
                   "The above waterfall chart displays the baseline number of spells or bed days and the progressive change from the baseline 
                   when each growth factor (left) is applied. The final bar represents the projected activity level that is the sum of the baseline
                   and the combined growth factor changes.",
                   br()
                 ),
                 
                 h5(br(),
                    "Occupancy rate adjusted",
                    br()
                    ),
                 h6(
                   "Below we convert the bed days measure from our baseline extract and projected activity counts to annualised bed days. We apply 
                   a 95% occupancy rate to the baseline bed days and divide by 365.25 to calculate annualised bed days. We apply an 80% target 
                   occupancy rate to our bed day projection and divide by 365.25 to calculate the future annualised bed day requirement.",
                   br(),
                   br(),
                   "Calculation: Annualised bed days = (Bed days / varying occupancy rate) / 365.25"
                   ),
                 tabPanel("Annualised bed days", DTOutput("dataTable_occupancy")),
                 
                 h3(br(),
                    "Sub-group Analysis"
                    ),
                 h6(
                   "Finally, we present the baseline and projected activity levels by patient group or pathway, in both spells and bed days. Cycle through the 
                   'grouping variable' control (left) to change the sub-group measure by which we present the baseline and projected activity. Switch between 
                   the output tabs to view either the graph plot or the underlying data.",
                   br(),
                   br()
                 ),
                 tabsetPanel(
                   tabPanel("Sub-group Plot", plotOutput("sub_group_Plot", height = "700px", width = "1000px")),
                   tabPanel("Sub-group Table", DTOutput("dataTable_subplot"))
                 )
                 
                 
               )
             )
           ))
  )


# Define server logic ----
server <- function(input, output) {
  
  # Set up ----
  # Read in demographic factor
  icb_weighted_demographic_change <- read_csv("demographic_projections/icb_weighted_demographic_change.csv")
  
  # Read in grouped data
  baseline_aggregate <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Intro tab ----
  output$ExampleFormatTable_2 <- renderTable({
    
    tibble(
      discharge_month = as.Date(c("2023-08-01", "2024-01-01", "2023-07-01")),
      residence_icb_code = c("QUA", "QUA", "QUA"),
      residence_icb_name = c("QUA: NHS Black Country ICB", "QUA: NHS Black Country ICB", "QUA: NHS Black Country ICB"),
      age_group_admission = c("25-64", "00-17", "25-64"),
      gender = c(1, 2, 1),
      ethnic_category_2 = c("White", "White", "White"),
      imd_quintile = c(1, 2, 4),
      provider_type = c("Independent", "Independent", "NHS"),
      legal_status_group = c("Not formally detained", "Not formally detained", "Formally detained"),
      lda_flag = c(NA, NA, NA),
      der_ward_type_desc_first = c("Adult Mental Health Ward", "Child and Adolescent Mental Health Ward", "Adult Mental Health Ward"),
      spell_count = c(27, 26, 24),
      bed_days = c(623, 722, 288)
    )
  },  width = "50px")
  
  
  # Analysis tab ----
  
  # Growth factor inputs 
  demographic_growth <- 
    reactive({
    icb_weighted_demographic_change$weighted_perc_change[icb_weighted_demographic_change$residence_icb_name == input$icb]
  })
  incidence_change       <- reactive({ input$incidence_change })
  acuity_change          <- reactive({ input$acuity_change })
  social_care_pressures  <- reactive({ input$social_care_pressures })
  mha_changes            <- reactive({ input$mha_changes })
  national_policy        <- reactive({ input$national_policy })
  service_models         <- reactive({ input$service_models })
  prevention_programme   <- reactive({ input$prevention_programme })
  admission_avoidance    <- reactive({ input$admission_avoidance })
  waiting_list_reduction <- reactive({ input$waiting_list_reduction })
  ooa_repat              <- reactive({ input$ooa_repat })
  shift_to_ip            <- reactive({ input$shift_to_ip })
  
  # Apply growth inflators to grouped activity counts - separately
  baseline_growth <- reactive({
    req(baseline_aggregate(), input$icb)
    
    baseline_aggregate() %>%
      filter(residence_icb_name == input$icb) %>%
      mutate(sp_demographic_growth       = spell_count * demographic_growth(),
             sp_incidence_change         = spell_count * incidence_change(),
             sp_acuity_change            = spell_count * acuity_change(),
             sp_social_care_pressures    = spell_count * social_care_pressures(),
             sp_mha_changes              = spell_count * mha_changes(),
             sp_national_policy          = spell_count * national_policy(),
             sp_service_models           = spell_count * service_models(),
             sp_prevention_programme     = spell_count * prevention_programme(),
             sp_admission_avoidance      = spell_count * admission_avoidance(),
             sp_waiting_list_reduction   = spell_count * waiting_list_reduction(),
             sp_ooa_repat                = spell_count * ooa_repat(),
             sp_shift_to_ip              = spell_count * shift_to_ip(),
             
             bd_demographic_growth       = bed_days * demographic_growth(),
             bd_incidence_change         = bed_days * incidence_change(),
             bd_acuity_change            = bed_days * acuity_change(),
             bd_social_care_pressures    = bed_days * social_care_pressures(),
             bd_mha_changes              = bed_days * mha_changes(),
             bd_national_policy          = bed_days * national_policy(),
             bd_service_models           = bed_days * service_models(),
             bd_prevention_programme     = bed_days * prevention_programme(),
             bd_admission_avoidance      = bed_days * admission_avoidance(),
             bd_waiting_list_reduction   = bed_days * waiting_list_reduction(),
             bd_ooa_repat                = bed_days * ooa_repat(),
             bd_shift_to_ip              = bed_days * shift_to_ip()
      ) %>%
      mutate(spell_proj = spell_count + rowSums(across(contains("sp_"))),
             bed_days_proj = bed_days + rowSums(across(contains("bd_"))))
  })
  
  # Aggregate up growth/reduction in activity for each factor to ICB level
  waterfall_data <- reactive({
    
    baseline_growth() |> 
      group_by(residence_icb_name) %>%
      summarise(spell_count = sum(spell_count),
                bed_days = sum(bed_days),
                sp_demographic_growth      = sum(sp_demographic_growth),
                sp_incidence_change        = sum(sp_incidence_change),
                sp_acuity_change           = sum(sp_acuity_change),
                sp_social_care_pressures   = sum(sp_social_care_pressures),
                sp_mha_changes             = sum(sp_mha_changes),
                sp_national_policy         = sum(sp_national_policy),
                sp_service_models          = sum(sp_service_models),
                sp_prevention_programme    = sum(sp_prevention_programme),
                sp_admission_avoidance     = sum(sp_admission_avoidance),
                sp_waiting_list_reduction  = sum(sp_waiting_list_reduction),
                sp_ooa_repat               = sum(sp_ooa_repat),
                sp_shift_to_ip             = sum(sp_shift_to_ip),
                
                bd_demographic_growth      = sum(bd_demographic_growth),
                bd_incidence_change        = sum(bd_incidence_change),
                bd_acuity_change           = sum(bd_acuity_change),
                bd_social_care_pressures   = sum(bd_social_care_pressures),
                bd_mha_changes             = sum(bd_mha_changes),
                bd_national_policy         = sum(bd_national_policy),
                bd_service_models          = sum(bd_service_models),
                bd_prevention_programme    = sum(bd_prevention_programme),
                bd_admission_avoidance     = sum(bd_admission_avoidance),
                bd_waiting_list_reduction  = sum(bd_waiting_list_reduction),
                bd_ooa_repat               = sum(bd_ooa_repat),
                bd_shift_to_ip             = sum(bd_shift_to_ip),
                
                spell_proj = sum(spell_proj),
                bed_days_proj = sum(bed_days_proj)
      ) %>%
      pivot_longer(-residence_icb_name)
    
    })
  
  # Plot waterfall - Spells and bed days
  waterfall_plot <- reactive({
    
    waterfall_data() |> 
      select(-residence_icb_name) %>%
      filter(name == "spell_count" | str_detect(name, "sp_")) %>%
      mutate(name = case_when(
        name == "spell_count"                ~ "A. Baseline year (2024)",
        name == "sp_demographic_growth"      ~ "B. Demographic growth",
        name == "sp_incidence_change"        ~ "C. Incidence change",
        name == "sp_acuity_change"           ~ "D. Acuity change",
        name == "sp_social_care_pressures"   ~ "E. Social care pressures",
        name == "sp_mha_changes"             ~ "F. Mental health act changes",
        name == "sp_national_policy"         ~ "G. National policy",
        name == "sp_service_models"          ~ "H. Service models",
        name == "sp_prevention_programme"    ~ "I. Prevention programme",
        name == "sp_admission_avoidance"     ~ "J. Admission avoidance",
        name == "sp_waiting_list_reduction"  ~ "K. Waiting list reduction",
        name == "sp_ooa_repat"               ~ "L. Out of area (OOA)",
        name == "sp_shift_to_ip"             ~ "M. Shift to independent sector",
        name == "spell_proj"                 ~ "N. Projection"
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
            ) +
      labs(x = "Growth factor",
           y = "Spells",
           title = "Example waterfall plot",
           #subtitle = paste0("Mental health inpatient model | ", input$icb)
           subtitle = "Mental health inpatient model | 2024 baseline projection to 2028"
           )
    })
  
  waterfall_plot_bed_days <- reactive({
    
    waterfall_data() |> 
      select(-residence_icb_name) %>%
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
        name == "bd_ooa_repat"               ~ "L. Out of area (OOA)",
        name == "bd_shift_to_ip"             ~ "M. Shift to independent sector",
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
  })
  
  # Output objects
  output$waterfall_Plot <- renderPlot({
    req(baseline_growth())
    req(waterfall_data())
    req(waterfall_plot())
    
    waterfall_plot()
  })
  
  output$waterfall_Plot_bed_days <- renderPlot({
    req(baseline_growth())
    req(waterfall_data())
    req(waterfall_plot_bed_days())
    
    waterfall_plot_bed_days()
  })
  
  output$dataTable <- renderDT({
    req(baseline_growth())
    req(waterfall_data())
    
    DT::datatable(
      waterfall_data(),
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'csv')
      )
    )
    
  })
  
  # Occupancy rate table ----
  
  output$dataTable_occupancy <- renderDT({
    req(baseline_growth())
    req(waterfall_data())
    
    DT::datatable(
      waterfall_data() |> 
        filter(str_detect(name, "bed_days")) |> 
        mutate(value = round(value, 1)) |> 
        mutate(bed_day_annualised = 
                 case_when(
                   name == "bed_days" ~ round((value/0.95)/365.25,1),
                   name == "bed_days_proj" ~ round((value/0.8)/365.25,1)
                 )
        ) |> 
        rename(ICB = residence_icb_name,
               Measure = name,
               `Bed days` = value,
               `Annualised bed day` = bed_day_annualised),
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'csv')
                     )
      )
    
    })

  # Sub-group plots ----
  
  # Calculate sub-group activity
  sub_plot_data <- reactive({
    req(baseline_growth(), input$group_selection)
    
    baseline_growth() |> 
      group_by(!!sym(input$group_selection)) |> 
      summarise(spell_count = sum(spell_count),
                bed_days = sum(bed_days),
                
                spell_proj = sum(spell_proj),
                bed_days_proj = sum(bed_days_proj)
                ) |>
      rename(group_name = 1) |> 
      pivot_longer(-group_name) |> 
      mutate(flag = case_when(str_detect(name, "spell_") ~ "1. Spells", TRUE ~ "2. Bed days"),
             current_projection = case_when(str_detect(name, "proj") ~ "Projection", TRUE ~ "Current")
             ) 
    })
  
  # Plot sub-group
  sub_group_plot <- reactive({
    
    sub_plot_data() |> 
      ggplot(aes(x = group_name, y = value, fill = current_projection)) +
      geom_col(position = "dodge") +
      facet_wrap(~flag, scale = "free_y") +
      scale_fill_SU() +
      scale_y_continuous(labels = scales::comma) +
      theme(strip.background = element_rect(fill = NA, colour = "grey"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_blank(),
            strip.text = element_text(size = 16)
            ) +
      labs(x =  "Sub-group",
           fill = "",
           title = "Sub-group projections",
           subtitle = "Mental health inpatient model | 2024 baseline projection to 2028")
    })
  
  # Output objects
  output$sub_group_Plot <- renderPlot({
    req(baseline_growth())
    req(sub_plot_data())
    req(sub_group_plot())
    
    sub_group_plot()
  })

  output$dataTable_subplot <- renderDT({
    req(baseline_growth())
    req(sub_plot_data())
    
    #DT::datatable(sub_plot_data(), buttons = c("copy", "csv"))
    
    DT::datatable(
      sub_plot_data(),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
        )
      )
    
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("projected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- 
        baseline_growth() |> 
        select(-c(spell_count, contains("sp_"), spell_proj)) |> 
        filter(bed_days > 5)
      
      write.csv(data, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)




# To-do: ----

# Apply social care pressures factor to delayed discharge sub-set only (only those related to social care?)

# Home leave adjustment  



# Check - demographic change is combined growth over 3 years not single year 


# check language in app - growth figure is between start and finish not annual applied each year



# Home leave switch






