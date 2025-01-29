
# Admissions - All --------------------------------------------------------

spell_los_all_boxplot <- adm_comb |>
  ggplot(aes(x = Group, y = Der_HospProvSpell_LOS, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200)) +
  theme_tu_white(hex_col = palette_tu[4])

spell_los_summ_df <- adm_comb |>
  group_by(Group) |>
  summarise(Total = n(),
            `0_Day` = sum(case_when(Der_HospProvSpell_LOS < 1 ~ 1,
                                    TRUE ~ 0))/Total,
            `1+_year`  = sum(case_when(Der_HospProvSpell_LOS >= 365 ~ 1,
                                       TRUE ~ 0))/Total,
            `10p` = quantile(Der_HospProvSpell_LOS, 0.10),
            `25p` = quantile(Der_HospProvSpell_LOS, 0.25),
            `50p` = quantile(Der_HospProvSpell_LOS, 0.50),
            `75p` = quantile(Der_HospProvSpell_LOS, 0.75),
            `90p` = quantile(Der_HospProvSpell_LOS, 0.90),
            mean  = round(mean(Der_HospProvSpell_LOS, na.rm = TRUE), 1)
  )


spell_los_summ_tbl <- spell_los_summ_df |>
  mutate(`0_Day` = percent(`0_Day`, accuracy = 0.01),
         `1+_year` = percent(`1+_year`, accuracy = 0.01)
        ) |>
  select(-c(2)) |>
  rename("Percentage 0 Day LOS" = 2,
         "Percentage 1+ year LOS" = 3,
         "10th Percentile LOS" = 4,
         "Lower Quartile LOS" = 5,
         "Median LOS" = 6,
         "Upper Quartile LOS" = 7,
         "90th Percentile LOS" = 8,
         "Mean LOS" = 9) |>
  kable(format = "html", align = "lrrrrrrrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "150%", height = "200px")


spell_los_hl_boxplot <- adm_comb |>
  ggplot(aes(x = Group, y = Der_LOS_exHL, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay excluding Home Leave",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200)) +
  theme_tu_white(hex_col = palette_tu[4])


spell_los_hl_summ_df <- adm_comb |>
  group_by(Group) |>
  summarise(Total = n(),
            `0_Day` = sum(case_when(Der_LOS_exHL < 1 ~ 1,
                                    TRUE ~ 0))/Total,
            `1+_year`  = sum(case_when(Der_LOS_exHL >= 365 ~ 1,
                                       TRUE ~ 0))/Total,
            `10p` = quantile(Der_LOS_exHL, 0.10),
            `25p` = quantile(Der_LOS_exHL, 0.25),
            `50p` = quantile(Der_LOS_exHL, 0.50),
            `75p` = quantile(Der_LOS_exHL, 0.75),
            `90p` = quantile(Der_LOS_exHL, 0.90),
            mean  = round(mean(Der_LOS_exHL, na.rm = TRUE), 1)
  )

spell_los_hl_summ_tbl <- spell_los_hl_summ_df |>
  mutate(`0_Day` = percent(`0_Day`, accuracy = 0.01),
         `1+_year` = percent(`1+_year`, accuracy = 0.01)
  ) |>
  select(-c(2)) |>
  rename("Percentage 0 Day LOS" = 2,
         "Percentage 1+ year LOS" = 3,
         "10th Percentile LOS" = 4,
         "Lower Quartile LOS" = 5,
         "Median LOS" = 6,
         "Upper Quartile LOS" = 7,
         "90th Percentile LOS" = 8,
         "Mean LOS" = 9) |>
  kable(format = "html", align = "lrrrrrrrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff")


# Ward Bed Type First ----------------------------------------------------------------

spell_los_wbtf_boxplot <- adm_comb |>
  ggplot(aes(x = Group_sn, y = Der_HospProvSpell_LOS, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  facet_wrap(~Der_WardTypeDesc_First_chart) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200)) +
  theme_tu_white_fct(hex_col = palette_tu[4])

spell_los_wbtf_summ_df <- adm_comb |>
  group_by(Group, Der_WardTypeDesc_First) |>
  summarise(Total = n(),
            `0_Day` = sum(case_when(Der_HospProvSpell_LOS < 1 ~ 1,
                                    TRUE ~ 0))/Total,
            `1+_year`  = sum(case_when(Der_HospProvSpell_LOS >= 365 ~ 1,
                                       TRUE ~ 0))/Total,
            `10p` = quantile(Der_HospProvSpell_LOS, 0.10),
            `25p` = quantile(Der_HospProvSpell_LOS, 0.25),
            `50p` = quantile(Der_HospProvSpell_LOS, 0.50),
            `75p` = quantile(Der_HospProvSpell_LOS, 0.75),
            `90p` = quantile(Der_HospProvSpell_LOS, 0.90),
            mean  = round(mean(Der_HospProvSpell_LOS, na.rm = TRUE), 1)
  )

spell_los_wbtf_summ_tbl <- spell_los_wbtf_summ_df |>
  mutate(`0_Day` = percent(`0_Day`, accuracy = 0.01),
         `1+_year` = percent(`1+_year`, accuracy = 0.01)
  ) |>
  select(-c(3)) |>
  arrange(Der_WardTypeDesc_First, Group) |>
  rename("Ward Type on Admission" = 2,
         "Percentage 0 Day LOS" = 3,
         "Percentage 1+ year LOS" = 4,
         "10th Percentile LOS" = 5,
         "Lower Quartile LOS" = 6,
         "Median LOS" = 7,
         "Upper Quartile LOS" = 8,
         "90th Percentile LOS" = 9,
         "Mean LOS" = 10) |>
  kable(format = "html", align = "llrrrrrrrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "150%", heigh = "300px")



# CRD & DTOCs ------------------------------------------------------------

crd_summ <- adm_comb |>
  group_by(Group) |>
  summarise(Spells = n(),
            Beddays = sum(Der_HospProvSpell_LOS, na.rm = TRUE),
            CRDdays = sum(Adj_Delay_Days, na.rm = TRUE),
            Zero_CRD = sum(case_when(Adj_Delay_Days == 0 ~ 1,
                                     TRUE ~ 0))
  ) |>
  mutate(Spells_Prop = (Spells - Zero_CRD) / Spells,
         BD_Prop = CRDdays / Beddays)

crd_summ_nz <- adm_comb |>
  filter(Adj_Delay_Days > 0) |>
  group_by(Group) |>
  summarise(Total = n(),
            `10p` = quantile(Adj_Delay_Days, 0.10),
            `25p` = quantile(Adj_Delay_Days, 0.25),
            `50p` = quantile(Adj_Delay_Days, 0.50),
            `75p` = quantile(Adj_Delay_Days, 0.75),
            `90p` = quantile(Adj_Delay_Days, 0.90),
            mean  = round(mean(Adj_Delay_Days, na.rm = TRUE), 1)
  )

crd_nz_boxplot <- adm_comb |>
  filter(Adj_Delay_Days > 0) |>
  ggplot(aes(x = Group, fill = Group, y = Adj_Delay_Days)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  labs(x = "Resident or Provider ICB",
       y = "Delay Days",
       caption = "Source: MSHDS",
       title = "Delayed Days - Ready for Discharge",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200)) +
  theme_tu_white(hex_col = palette_tu[4])

crd_nz_table <- crd_summ_nz |>
  rename("Spells with Delay" = 2,
         "10th Percentile LOS" = 3,
         "Lower Quartile LOS" = 4,
         "Median LOS" = 5,
         "Upper Quartile LOS" = 6,
         "90th Percentile LOS" = 7,
         "Mean LOS" = 8) |>
  kable(format = "html", align = "lrrrrrrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff")


# Ward Security -----------------------------------------------------------

spell_los_wsec_boxplot <- adm_comb |>
  ggplot(aes(x = Group, y = Der_HospProvSpell_LOS, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  facet_wrap(~Der_WardSecLevelDesc_First) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200)) +
  theme_tu_white(hex_col = palette_tu[4])

spell_los_wsec_summ_df <- adm_comb |>
  group_by(Group, Der_WardSecLevelDesc_First) |>
  summarise(Total = n(),
            `0_Day` = sum(case_when(Der_HospProvSpell_LOS < 1 ~ 1,
                                    TRUE ~ 0))/Total,
            `1+_year`  = sum(case_when(Der_HospProvSpell_LOS >= 365 ~ 1,
                                       TRUE ~ 0))/Total,
            `10p` = quantile(Der_HospProvSpell_LOS, 0.10),
            `25p` = quantile(Der_HospProvSpell_LOS, 0.25),
            `50p` = quantile(Der_HospProvSpell_LOS, 0.50),
            `75p` = quantile(Der_HospProvSpell_LOS, 0.75),
            `90p` = quantile(Der_HospProvSpell_LOS, 0.90),
            mean  = round(mean(Der_HospProvSpell_LOS, na.rm = TRUE), 1)
  )


# Diagnoses ---------------------------------------------------------------

prim_diag_status <- adm |>
  mutate(diag_missing = case_when(is.na(PrimDiag_First) ~ "Missing",
                                  TRUE ~ "Present")) |>
  group_by(diag_missing) |>
  summarise(Total = n()) |>
  mutate(Prop = Total/sum(Total))

spell_los_diags_summ_df <- adm_comb |>
  filter(!is.na(Category_3_Description_First)) |>
  group_by(Group, Category_3_Description_First) |>
  summarise(Total = n(),
            `0_Day` = sum(case_when(Der_HospProvSpell_LOS < 1 ~ 1,
                                    TRUE ~ 0))/Total,
            `1+_year`  = sum(case_when(Der_HospProvSpell_LOS >= 365 ~ 1,
                                       TRUE ~ 0))/Total,
            `10p` = quantile(Der_HospProvSpell_LOS, 0.10),
            `25p` = quantile(Der_HospProvSpell_LOS, 0.25),
            `50p` = quantile(Der_HospProvSpell_LOS, 0.50),
            `75p` = quantile(Der_HospProvSpell_LOS, 0.75),
            `90p` = quantile(Der_HospProvSpell_LOS, 0.90),
            mean  = round(mean(Der_HospProvSpell_LOS, na.rm = TRUE), 1)
  ) |>
  group_by(Category_3_Description_First) |>
  filter(any(Total[Group == selected_icb_name] > 10)) |>
  ungroup()

spell_los_diags_table <- spell_los_diags_summ_df |>
  mutate(`0_Day` = percent(`0_Day`, accuracy = 0.01),
         `1+_year` = percent(`1+_year`, accuracy = 0.01)
  ) |>
  arrange(Category_3_Description_First, Group) |>
  rename("ICD-10 Sub-Chapter (3 code)" = 2,
         "Total" = 3,
         "Percentage 0 Day LOS" = 4,
         "Percentage 1+ year LOS" = 5,
         "10th Percentile LOS" = 6,
         "Lower Quartile LOS" = 7,
         "Median LOS" = 8,
         "Upper Quartile LOS" = 9,
         "90th Percentile LOS" = 10,
         "Mean LOS" = 11) |>
  kable(format = "html", align = "llrrrrrrrrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "150%", heigh = "300px")

# Admission Bed Type First -------------------------------------------------

adm_class_first <- adm_comb |>
  group_by(Group, HospBedType_First_clean) |>
  summarise(Total = n(),
            `0_Day` = sum(case_when(Der_HospProvSpell_LOS < 1 ~ 1,
                                    TRUE ~ 0))/Total,
            `1+_year`  = sum(case_when(Der_HospProvSpell_LOS >= 365 ~ 1,
                                       TRUE ~ 0))/Total,
            `10p` = quantile(Der_HospProvSpell_LOS, 0.10),
            `25p` = quantile(Der_HospProvSpell_LOS, 0.25),
            `50p` = quantile(Der_HospProvSpell_LOS, 0.50),
            `75p` = quantile(Der_HospProvSpell_LOS, 0.75),
            `90p` = quantile(Der_HospProvSpell_LOS, 0.90),
            mean  = round(mean(Der_HospProvSpell_LOS, na.rm = TRUE), 1)
  )

adm_class_first_icb <- adm_class_first |>
  filter(Group == selected_icb_name) |>
  slice_max(order_by = Total, n = 6) |>
  select(HospBedType_First_clean)

adm_class_first_top6 <- adm_class_first %>%
  filter(HospBedType_First_clean %in% adm_class_first_icb$HospBedType_First_clean)

adm_class_first_top6_df <- adm_comb |>
  filter(HospBedType_First_clean %in% adm_class_first_icb$HospBedType_First_clean)

adm_class_first_top6_boxplot <- adm_class_first_top6_df |>
  ggplot(aes(x = Der_HospProvSpell_LOS, y = HospBedType_First_clean, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(y = "Admission Classification",
       x = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(xlim = c(0, 300)) +
  theme_tu_white(hex_col = palette_tu[4])

adm_class_first_top6_table <- adm_class_first_top6 |>
  mutate(`0_Day` = percent(`0_Day`, accuracy = 0.01),
         `1+_year` = percent(`1+_year`, accuracy = 0.01)
  ) |>
  arrange(HospBedType_First_clean, Group) |>
  rename("Classification on Admission" = 2,
         "Total" = 3,
         "Percentage 0 Day LOS" = 4,
         "Percentage 1+ year LOS" = 5,
         "10th Percentile LOS" = 6,
         "Lower Quartile LOS" = 7,
         "Median LOS" = 8,
         "Upper Quartile LOS" = 9,
         "90th Percentile LOS" = 10,
         "Mean LOS" = 11) |>
  kable(format = "html", align = "llrrrrrrrrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "150%", heigh = "300px")


# Age ---------------------------------------------------------------------

spell_los_age_summ <- adm_comb |>
  filter(Der_Age_at_StartDateHospProvSpell >= 18) |>
  group_by(Group, age_band) |>
  summarise(Total = n(),
            `0_Day` = sum(case_when(Der_HospProvSpell_LOS < 1 ~ 1,
                                    TRUE ~ 0))/Total,
            `1+_year`  = sum(case_when(Der_HospProvSpell_LOS >= 365 ~ 1,
                                       TRUE ~ 0))/Total,
            `10p` = quantile(Der_HospProvSpell_LOS, 0.10),
            `25p` = quantile(Der_HospProvSpell_LOS, 0.25),
            `50p` = quantile(Der_HospProvSpell_LOS, 0.50),
            `75p` = quantile(Der_HospProvSpell_LOS, 0.75),
            `90p` = quantile(Der_HospProvSpell_LOS, 0.90),
            mean  = round(mean(Der_HospProvSpell_LOS, na.rm = TRUE), 1)
  )

age_los_boxplot <- adm_comb |>
  filter(Der_Age_at_StartDateHospProvSpell >= 18) |>
  ggplot(aes(x = Der_HospProvSpell_LOS, fill = Group, y = age_band)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(y = "Age Band",
       x = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(xlim = c(0, 200)) +
  theme_tu_white(hex_col = palette_tu[4])


age_los_table <- spell_los_age_summ  |>
  mutate(`0_Day` = percent(`0_Day`, accuracy = 0.01),
         `1+_year` = percent(`1+_year`, accuracy = 0.01)
  ) |>
  arrange(age_band, Group) |>
  rename("Age Band" = 2,
         "Total" = 3,
         "Percentage 0 Day LOS" = 4,
         "Percentage 1+ year LOS" = 5,
         "10th Percentile LOS" = 6,
         "Lower Quartile LOS" = 7,
         "Median LOS" = 8,
         "Upper Quartile LOS" = 9,
         "90th Percentile LOS" = 10,
         "Mean LOS" = 11) |>
  kable(format = "html", align = "llrrrrrrrrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "150%", heigh = "300px")


# Deprivation -------------------------------------------------------------

imd_los_summ <- adm_comb |>
  group_by(Group, IMD_Decile_f) |>
  summarise(Total = n(),
            `0_Day` = sum(case_when(Der_HospProvSpell_LOS < 1 ~ 1,
                                    TRUE ~ 0))/Total,
            `1+_year`  = sum(case_when(Der_HospProvSpell_LOS >= 365 ~ 1,
                                       TRUE ~ 0))/Total,
            `10p` = quantile(Der_HospProvSpell_LOS, 0.10),
            `25p` = quantile(Der_HospProvSpell_LOS, 0.25),
            `50p` = quantile(Der_HospProvSpell_LOS, 0.50),
            `75p` = quantile(Der_HospProvSpell_LOS, 0.75),
            `90p` = quantile(Der_HospProvSpell_LOS, 0.90),
            mean  = round(mean(Der_HospProvSpell_LOS, na.rm = TRUE), 1)
  )

imd_los_table <- imd_los_summ |>
  mutate(`0_Day` = percent(`0_Day`, accuracy = 0.01),
         `1+_year` = percent(`1+_year`, accuracy = 0.01)
  ) |>
  arrange(IMD_Decile_f, Group) |>
  rename("IMD Decile" = 2,
         "Total" = 3,
         "Percentage 0 Day LOS" = 4,
         "Percentage 1+ year LOS" = 5,
         "10th Percentile LOS" = 6,
         "Lower Quartile LOS" = 7,
         "Median LOS" = 8,
         "Upper Quartile LOS" = 9,
         "90th Percentile LOS" = 10,
         "Mean LOS" = 11) |>
  kable(format = "html", align = "llrrrrrrrrr") |>
  kable_styling() |>
  row_spec(0, background = palette_tu[4], color = "#ffffff") |>
  scroll_box(width = "150%", heigh = "300px")

imd_los_boxplot <- adm_comb |>
  ggplot(aes(x = IMD_Decile_f, y = Der_HospProvSpell_LOS, fill = Group)) + 
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  labs(x = "IMD Decile",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200)) +
  theme_tu_white(hex_col = palette_tu[4])
