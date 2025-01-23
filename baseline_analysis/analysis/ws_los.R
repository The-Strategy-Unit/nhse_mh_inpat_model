
# All Ward Stays ----------------------------------------------------------

ws_all_boxplot <- ws_comb |>
  ggplot(aes(x = Group, y = Der_WardStayLOS, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  ylim(0, 200)

ws_hl_boxplot <- ws_comb |>
  ggplot(aes(x = Group, y = Der_WardStayLOS_exhl, fill = Group)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  labs(x = "Resident or Provider ICB",
       y = "Length of Stay (LOS) - Days",
       caption = "Source: MSHDS",
       title = "Hospital Spell Length of Stay",
       subtitle = paste0(selected_icb_name)) +
  ylim(0, 200)

