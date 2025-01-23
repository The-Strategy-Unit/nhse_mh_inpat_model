
# adm processing ----------------------------------------------------------

adm <- mhsds_baseline_adm

# ws processing -----------------------------------------------------------

ws <- mhsds_baseline_ws |>
  mutate(Der_WardStayLOS_exhl = Der_WardStayLOS - HL_Days)

# icb choices -------------------------------------------------------------

icb_choices <- adm |>
  filter(Residence_ICB_Region_Code == "Y60") |>
  group_by(Residence_ICB_Code, Residence_ICB_Name) |>
  summarise()

selected_icb_name <- icb_choices |>
  filter(Residence_ICB_Code == selected_icb) |>
  pull(Residence_ICB_Name)

# combined datasets -------------------------------------------------------

adm_reg <- adm |>
  mutate(Group = "Midlands")

adm_select <- adm |>
  filter(Residence_ICB_Code == selected_icb | Provider_ICB_Code == selected_icb) |>
  mutate(Group = selected_icb_name)

adm_comb <- rbind(adm_reg, adm_select)

ws_reg <- ws |>
  mutate(Group = "Midlands")

ws_select <- ws |>
  filter(Residence_ICB_Code == selected_icb | Provider_ICB_Code == selected_icb) |>
  mutate(Group = selected_icb_name)

ws_comb <- rbind(ws_reg, ws_select)
