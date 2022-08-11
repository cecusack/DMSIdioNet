kpss_plot = function(x, y, df) {
  df$beep <- rep(1:nrow(df))
  ggplot(df, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 100)) +
    geom_text(aes(x = 27.5, y = 95, label=paste("KPSS Trend =",round(kpss.test(.data[[y]], null = "Trend")$statistic, digits = 3),", p = ",round(kpss.test(.data[[y]], null = "Trend")$p.value, digits = 3))), colour = "red1", size = 3, fontface = "bold", check_overlap = TRUE) + 
    theme(legend.title = element_blank())
}