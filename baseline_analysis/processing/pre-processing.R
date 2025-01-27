
# adm processing ----------------------------------------------------------

adm <- mhsds_baseline_adm |>
  filter(Der_Age_at_StartDateHospProvSpell >=18) |>
  mutate(Der_WardTypeDesc_First_chart = 
           case_when(Der_WardType_First == 3 ~ "Adult MH Ward",
                     Der_WardType_First == 1 ~ "Child & Adolescent MH Ward",
                     Der_WardType_First == 5 ~ "LD Ward",
                     Der_WardType_First == 4 ~ "Non-MH Ward",
                     TRUE ~ "Unknown"),
         HospBedType_First_clean = str_to_sentence(HospitalBedType_First),
         age_band = paste0(floor(Der_Age_at_StartDateHospProvSpell/5) * 5,
                           "-",
                    floor(Der_Age_at_StartDateHospProvSpell/5) * 5 + 4),
         IMD_Decile_f = factor(IMD_Decile, levels = c("1", "2", "3", "4", "5",
                                                      "6", "7", "8", "9", "10")),
         Ethnic_Category = case_when(Ethnic_Category == "" ~ NA,
                                     TRUE ~ Ethnic_Category))

# ws processing -----------------------------------------------------------

ws <- mhsds_baseline_ws |>
  mutate(Der_WardStayLOS_exhl = Der_WardStayLOS - HL_Days)

# icb choices -------------------------------------------------------------

icb_choices <- adm |>
  filter(Residence_ICB_Region_Code == "Y60") |>
  group_by(Residence_ICB_Code, Residence_ICB_Name) |>
  summarise() |>
  mutate(residence_icb_name_short = case_when(
    Residence_ICB_Code == "QGH" ~ "Hereford & Worcestershire",
    Residence_ICB_Code == "QHL" ~ "BSOL",
    Residence_ICB_Code == "QJ2" ~ "Derby & Derbyshire",
    Residence_ICB_Code == "QJM" ~ "Lincolnshire",
    Residence_ICB_Code == "QK1" ~ "LLR",
    Residence_ICB_Code == "QNC" ~ "SSOT",
    Residence_ICB_Code == "QOC" ~ "STW",
    Residence_ICB_Code == "QPM" ~ "Northamptonshire",
    Residence_ICB_Code == "QT1" ~ "Notts",
    Residence_ICB_Code == "QUA" ~ "Black Country",
    Residence_ICB_Code == "QWU" ~ "Coventry & Warwickshire")
  )

selected_icb_name <- icb_choices |>
  filter(Residence_ICB_Code == selected_icb) |>
  pull(Residence_ICB_Name)

selected_icb_name_short <- icb_choices |>
  filter(Residence_ICB_Code == selected_icb) |>
  pull(residence_icb_name_short)

# combined datasets -------------------------------------------------------

adm_reg <- adm |>
  mutate(Group = "Midlands",
         Group_sn = "Midlands")

adm_select <- adm |>
  filter(Residence_ICB_Code == selected_icb | Provider_ICB_Code == selected_icb) |>
  mutate(Group = selected_icb_name,
         Group_sn = selected_icb_name_short) 

adm_comb <- rbind(adm_reg, adm_select) |>
  mutate(Group = factor(Group, levels = c("Midlands", setdiff(unique(Group), "Midlands"))),
         Group_sn = factor(Group_sn, levels = c("Midlands", setdiff(unique(Group_sn), "Midlands"))))

ws_reg <- ws |>
  mutate(Group = "Midlands")

ws_select <- ws |>
  filter(Residence_ICB_Code == selected_icb | Provider_ICB_Code == selected_icb) |>
  mutate(Group = selected_icb_name)

ws_comb <- rbind(ws_reg, ws_select)
