
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

demo_adm_age_chart <- demo_adm_age_icb |>
  ggplot(aes(x = Prop, y= age_band)) +
  geom_bar(stat = "identity", fill = palette_wong[1], alpha = 0.5) +
  scale_x_continuous(labels = percent) +
  labs(x = "Percentage of Admissions",
       y = "Age Band",
       caption = "Source: MSHDS",
       title = "Hospital Spell Demographics - Age",
       subtitle = paste0(selected_icb_name)) +
  theme_tu_white(hex_col = palette_tu[4])


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

demo_adm_imd_chart <- demo_adm_imd_icb |>
  ggplot(aes(x = Prop, y= IMD_Decile_f)) +
  geom_bar(stat = "identity", fill = palette_wong[1], alpha = 0.5) +
  scale_x_continuous(labels = percent) +
  labs(x = "Percentage of Admissions",
       y = "Deprivation Decile",
       caption = "Source: MSHDS",
       title = "Hospital Spell Demographics - Deprivation",
       subtitle = paste0(selected_icb_name)) +
  theme_tu_white(hex_col = palette_tu[4])


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

demo_adm_eth_chart <- demo_adm_eth_icb |>
  ggplot(aes(x = Prop, y= Ethnic_Category)) +
  geom_bar(stat = "identity", fill = palette_wong[1], alpha = 0.5) +
  scale_x_continuous(labels = percent) +
  labs(x = "Percentage of Admissions",
       y = "Ethnic Category",
       caption = "Source: MSHDS",
       title = "Hospital Spell Demographics - Ethnicity",
       subtitle = paste0(selected_icb_name)) +
  theme_tu_white(hex_col = palette_tu[4])


# MHA Legal Status --------------------------------------------------------

adm_mha_icb <- adm_comb |>
  filter(Group == selected_icb_name) |>
  group_by(Group, LegalStatusDesc) |>
  summarise(Total = n()) |>
  mutate(Prop = Total / sum(Total))

adm_mha <- adm_mha_icb |>
  mutate(`Prop` = percent(`Prop`, accuracy = 0.01)) |>
  rename("Mental Health Act Legal Status" = 2,
         "Percentage of Admissions" = 4) |>
  kable(format = "html", align = "llrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "100%", heigh = "300px")

adm_mha_chart <- adm_mha_icb |>
  ggplot(aes(x = Prop, y= LegalStatusDesc)) +
  geom_bar(stat = "identity", fill = palette_wong[1], alpha = 0.5) +
  scale_x_continuous(labels = percent) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 40)) +
  labs(x = "Percentage of Admissions",
       y = "Ethnic Category",
       caption = "Source: MSHDS",
       title = "MHA Legal Status on Admission",
       subtitle = paste0(selected_icb_name)) +
  theme_tu_white(hex_col = palette_tu[4])


# Admission Source --------------------------------------------------------

adm_asrc_icb <- adm_comb |>
  filter(Group == selected_icb_name) |>
  group_by(Group, AdmissionSourceDesc) |>
  summarise(Total = n()) |>
  mutate(Prop = Total / sum(Total),
         AdmissionSourceDesc = substr(AdmissionSourceDesc, 1, 100))

adm_asrc <- adm_asrc_icb |>
  mutate(`Prop` = percent(`Prop`, accuracy = 0.01)) |>
  rename("Admission Source" = 2,
         "Percentage of Admissions" = 4) |>
  kable(format = "html", align = "llrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "100%", heigh = "300px")

adm_asrc_chart <- adm_asrc_icb |>
  ggplot(aes(x = Prop, y= AdmissionSourceDesc)) +
  geom_bar(stat = "identity", fill = palette_wong[1], alpha = 0.5) +
  scale_x_continuous(labels = percent) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 80)) +
  labs(x = "Percentage of Admissions",
       y = "Ethnic Category",
       caption = "Source: MSHDS",
       title = "Admission Source",
       subtitle = paste0(selected_icb_name)) +
  theme_tu_white_small(hex_col = palette_tu[4])


# LDA Flags ---------------------------------------------------------------

adm_flag_ld <- adm_comb |>
  filter(Group == selected_icb_name) |>
  mutate(LD_Flag_SU = case_when(LD_Flag_SU == 1 ~ "Yes",
                                LD_Flag_SU == 0 ~ "No",
                                TRUE ~ "No")) |>
  group_by(Group, LD_Flag_SU) |>
  summarise(Total = n()) |>
  mutate(Prop = Total / sum(Total))
         

adm_flag_a <- adm_comb |>
  filter(Group == selected_icb_name) |>
  mutate(Autism_Flag_SU = case_when(Autism_Flag_SU == 1 ~ "Yes",
                                Autism_Flag_SU == 0 ~ "No",
                                TRUE ~ "No")) |>
  group_by(Group, Autism_Flag_SU) |>
  summarise(Total = n()) |>
  mutate(Prop = Total / sum(Total))

adm_flag_ld_table <- adm_flag_ld |>
  mutate(`Prop` = percent(`Prop`, accuracy = 0.01)) |>
  rename("LD Flag" = 2,
         "Percentage of Admissions" = 4) |>
  kable(format = "html", align = "llrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff")

adm_flag_a_table <- adm_flag_a |>
  mutate(`Prop` = percent(`Prop`, accuracy = 0.01)) |>
  rename("Autism Flag" = 2,
         "Percentage of Admissions" = 4) |>
  kable(format = "html", align = "llrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff")
