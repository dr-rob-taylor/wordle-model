compute_stats <- function(results) {
  n_games     <- nrow(results)
  n_wins      <- sum(results$solved)
  win_pct     <- round(100 * n_wins / n_games)
  avg_guesses <- round(mean(results$n_guesses, na.rm = TRUE), 1)

  runs        <- rle(results$solved)
  win_runs    <- runs$lengths[runs$values]
  max_streak  <- if (length(win_runs) > 0) max(win_runs) else 0L
  curr_streak <- if (tail(results$solved, 1)) tail(runs$lengths, 1) else 0L

  guess_dist  <- as.integer(table(factor(results$n_guesses, levels = 1:6)))

  list(
    n_games     = n_games,
    n_wins      = n_wins,
    win_pct     = win_pct,
    avg_guesses = avg_guesses,
    max_streak  = max_streak,
    curr_streak = curr_streak,
    guess_dist  = guess_dist
  )
}

plot_stats <- function(stats, filename = "wordle_stats.png") {
  library(ggplot2)
  library(gridExtra)

  # ---- Summary stats panel ----
  sum_df <- data.frame(
    x     = 1:5,
    value = c(stats$n_games, stats$win_pct, stats$avg_guesses,
              stats$curr_streak, stats$max_streak),
    label = c("Played", "Win %", "Avg\nGuesses", "Current\nStreak", "Max\nStreak")
  )

  summary_p <- ggplot(sum_df, aes(x = x)) +
    geom_text(aes(label = value), y = 0.7, size = 7, fontface = "bold") +
    geom_text(aes(label = label), y = 0.25, size = 2.8, lineheight = 0.9) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0.5, 5.5)) +
    labs(title = "STATISTICS") +
    theme_void() +
    theme(
      plot.title  = element_text(hjust = 0.5, size = 13, face = "bold",
                                 margin = margin(b = 10)),
      plot.margin = margin(12, 10, 6, 10)
    )

  # ---- Guess distribution panel ----
  dist_df <- data.frame(
    guesses = factor(1:6, levels = 1:6),
    count   = stats$guess_dist
  )

  dist_p <- ggplot(dist_df, aes(y = guesses, x = count)) +
    geom_col(fill = "#787c7e", width = 0.65) +
    geom_text(aes(label = count, x = count), hjust = -0.3,
              fontface = "bold", size = 3.8) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    scale_y_discrete(limits = rev) +
    labs(title = "GUESS DISTRIBUTION", x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.title  = element_text(hjust = 0.5, size = 13, face = "bold",
                                 margin = margin(b = 10)),
      axis.text.y = element_text(size = 11, face = "bold",
                                 margin = margin(r = 6)),
      plot.margin = margin(6, 20, 12, 10)
    )

  combined <- gridExtra::arrangeGrob(summary_p, dist_p, heights = c(1, 2.2))

  grDevices::png(filename,
                 width  = 5   * 150,
                 height = 5.5 * 150,
                 res    = 150)
  grid::grid.draw(combined)
  grDevices::dev.off()

  invisible(filename)
}
