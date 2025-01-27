
# Age ---------------------------------------------------------------------

demo_adm_age_icb <- adm_comb |>
  filter(Group == selected_icb_name) |>
  group_by(Group, age_band) |>
  summarise(Total = n()) |>
  mutate(Prop = Total / sum(Total))

demo_adm_age <- demo_adm_age_icb |>
  mutate(`Prop` = percent(`Prop`, accuracy = 0.01)) |>
  rename("Age Band" = 2,
         "Percentage of Admissions" = 4) |>
  kable(format = "html", align = "llrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "100%", heigh = "300px")


# Deprivation -------------------------------------------------------------

demo_adm_imd_icb <- adm_comb |>
  filter(Group == selected_icb_name) |>
  group_by(Group, IMD_Decile_f) |>
  summarise(Total = n()) |>
  mutate(Prop = Total / sum(Total))

demo_adm_imd <- demo_adm_imd_icb |>
  mutate(`Prop` = percent(`Prop`, accuracy = 0.01)) |>
  rename("IMD Decile" = 2,
         "Percentage of Admissions" = 4) |>
  kable(format = "html", align = "llrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "100%", heigh = "300px")


# Ethnicity Group ---------------------------------------------------------

demo_adm_eth_icb <- adm_comb |>
  filter(Group == selected_icb_name) |>
  group_by(Group, Ethnic_Category) |>
  summarise(Total = n()) |>
  mutate(Prop = Total / sum(Total))

demo_adm_eth <- demo_adm_eth_icb |>
  mutate(`Prop` = percent(`Prop`, accuracy = 0.01)) |>
  rename("Ethnicity Group" = 2,
         "Percentage of Admissions" = 4) |>
  kable(format = "html", align = "llrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "100%", heigh = "300px")

