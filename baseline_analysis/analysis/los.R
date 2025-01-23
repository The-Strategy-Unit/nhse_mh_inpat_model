
# Admissions - All --------------------------------------------------------

spell_los_all_boxplot <- adm_comb |>
  ggplot(aes(x = Group, y = Der_HospProvSpell_LOS, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200))

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
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay excluding Home Leave",
       subtitle = paste0(selected_icb_name)) +
  coord_cartesian(ylim = c(0, 200))
