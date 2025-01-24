
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
  row_spec(0, background = palette_tu[4], color = "#ffffff")


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
  ggplot(aes(x = Group, y = Der_HospProvSpell_LOS, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  facet_wrap(~Der_WardTypeDesc_First) +
  scale_fill_manual(values = c(palette_wong[5], palette_wong[1])) +
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200)) +
  theme_tu_white(hex_col = palette_tu[4])

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
  row_spec(0, background = palette_tu[4], color = "#ffffff")



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
