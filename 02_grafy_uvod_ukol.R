
ggplot(data = d, aes(x = vzdelani_3kat, y = vek, color = vzdelani_3kat)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.2)) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75), outlier.shape = NA) +
  
  labs(x = "Vzdělání", y = "Věk", title = "Rozložení věku u respondentů podle vzdělání",
       color = "Kategorie vzdělání") + # Zde měníte název legendy
  theme_bw() +
  theme(legend.position = "none") +
  # scale_color_manual(values = c("muži" = "blue", "ženy" = "red")) +
  scale_y_continuous(breaks = seq(0, 95, by = 2))

