# Calculate ICB demographic growth figures 

library(tidyverse)
library(janitor)
library(readxl)


baseline_aggregate <- read_csv("baseline_aggregate.csv")


# ICB populations by age and sex ----
## https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates   

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


# Read in local authority population projections ----
## https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/localauthoritiesinenglandtable2 
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

pop_projections_la_2018_based <-
  read_excel("demographic_projections/pop_projections_la_2018_based.xls", 
           sheet = "Persons", 
           skip = 6) |> 
  clean_names()



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


# Link ICB to LA - look up ----
lsoa_lookup <- 
  read_csv("demographic_projections/lsoa11_21_la_2022.csv") |> 
  clean_names() |> 
  select(lsoa11cd, lsoa21cd, lad22cd, lad22nm)

lsoa_icb_lad <-
  read_csv("demographic_projections/lsoa21_sicb_icb_lad23.csv") |> 
  clean_names() |> 
  select(lsoa21cd, icb23cd, icb23nm, lad23cd, lad23nm)


# Aggregate to demographic change figure for each ICB - projections weighted according to LA overlap in each ICB ----
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


write.csv(icb_weighted_demographic_change, 
          "demographic_projections/icb_weighted_demographic_change.csv")



# Land overlap based weighting ----
# Note: Had to pivot to land-overlap based weighting because we didn't have age and gender activity counts for non-Midlands ICB's

library(sf)

# Integrated Care Boards (April 2023) EN BGC
icb <-
  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  clean_names() |>
  select(icb23cd, geometry)

# Local Authority Districts (December 2023) Boundaries UK BGC
lad <-
  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2023_Boundaries_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  clean_names() |>
  select(lad23cd, geometry)


# Ensure both shapefiles use the same CRS
icb <- st_transform(icb, crs = st_crs(lad))

# Calculate the intersection between LADs and ICBs
intersection <- st_intersection(lad, icb)

# Calculate the area of the intersections and the original LADs
intersection_area <- 
  intersection |>
  mutate(intersection_area = st_area(intersection)) |>
  st_join(lad |> 
            mutate(lad_area = st_area(geometry))
          )

# Compute the proportion of each LAD within each ICB
intercetion_prop <-
  intersection_area |>
  mutate(proportion = as.numeric(intersection_area / lad_area)) |>
  select(lad23cd = lad23cd.x , icb23cd, proportion)


# Create the desired output
la_icb_overlap <- 
  intercetion_prop |>
  st_drop_geometry() |>
  arrange(lad23cd, desc(proportion)) |>
  tibble()


# Calculate projected population change (+based on MH data age ranges)
project_la_population <- function(year_projection) {
  pop_projections_la_2018_based |>
    filter(
      !area %in% c("England",
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
      age_group == "All ages"
    ) |>
    select(-age_group) |> 
    pivot_longer(cols = -c(code, area)) |> 
    mutate(year = as.numeric(str_remove_all(name, "x"))) |> 
    #filter(year %in% 2024:year_projection) |> # filter to 2028 - Change?
    filter(year %in% 2024:2028) |>
    select(-name) |> 
    group_by(code, area, year) |>
    summarise(value = sum(value, na.rm = T)) |> 
    group_by(code, area) |> 
    mutate(proj_perc_change = (value - lag(value)) / lag(value)) |>  #? x100
    ungroup() |> 
    select(-value) |>
    drop_na(proj_perc_change)
  }

proj_perc_change_la_age_group_2 <- project_la_population(2028)


icb_23_lookup <-
  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/SICBL23_ICB23_NHSER23_EN_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  clean_names() |>
  st_drop_geometry() |>
  tibble() |>
  select(icb23cd, icb23cdh, icb23nm) |>
  distinct()


options(scipen = 999)


# Calculate land-based overlap weighted ICB population projections per ICB using LA projections
icb_population_projection_land_based <-
  la_icb_overlap |>
  left_join(
    proj_perc_change_la_age_group_2 |>
      group_by(code, area) |> 
      summarise(proj_perc_change = sum(proj_perc_change)),
    by = c("lad23cd" = "code")
  ) |>
  mutate(weighted_perc_change = proj_perc_change * proportion) |> 
  group_by(icb23cd) |>
  summarise(icb_proj_perc_chg = sum(weighted_perc_change, na.rm = T)) |>
  left_join(icb_23_lookup, by = "icb23cd") |>
  mutate(icb_name = 
           paste0(icb23cdh, ": ", str_replace_all(icb23nm, "Integrated Care Board", "ICB"))
  ) |>
  select(-icb23nm, -icb23cdh) |>
  union_all(
    tribble(~icb23cd, ~icb_proj_perc_chg, ~icb_name,
            "A1", 2.3, "Anytown ICB_1")
  ) |>
  mutate(icb_name = str_replace_all(icb_name, " and ", " And ")) |>
  mutate(icb_name = str_replace_all(icb_name, "-on-", "-On-"))

# Write csv
write.csv(icb_population_projection_land_based, "demographic_projections/icb_population_projection_land_based.csv")



