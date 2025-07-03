# ...[previous code for data wrangling]...

# Improved publication-quality plot for deltaCPI (relative cumulative pressure)

# Relabel scenario names for clarity
scenario_labels <- c(
  "SCEN_DIET0_CULTURE0_CAPTURE0" = "BAU",
  "SCEN_DIET+10_CULTURE+50_CAPTURE+10" = "Blue transformation",
  "SCEN_DIET-10_CULTURE0_CAPTURE-10" = "Barriers to blue growth"
)

scen_join_rel$Scenario <- factor(scen_join_rel$ALLSCEN3, levels = names(scenario_labels), labels = scenario_labels)

p2_delta_pub <- ggplot(scen_join_rel, aes(x = as.factor(YEAR), y = deltaCPI, fill = Scenario)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black", alpha = 0.95) +
  facet_wrap(~method, labeller = as_labeller(c("integrated" = "Integrated system", "no_marine" = "No marine"))) +
  scale_fill_brewer(palette = "Dark2", name = "Scenario") +
  labs(
    title = "Relative Cumulative Pressure (deltaCPI) by Scenario (2050)",
    subtitle = "Comparison to baseline (BAU) for integrated and no-marine systems",
    x = "Year",
    y = "Relative Cumulative Pressure (deltaCPI)",
    caption = "Data: Halpern et al. 2022, GLOBIOM output"
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15, color = "black"),
    axis.title = element_text(size = 17),
    axis.ticks = element_line(size = 0.8),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 10)),
    strip.text = element_text(size = 17, face = "bold"),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey80")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

print(p2_delta_pub)

# Save high-resolution version for publication
ggsave("relative_cumulative_pressure_plot_pub.png", p2_delta_pub, width = 11, height = 8, dpi = 400)