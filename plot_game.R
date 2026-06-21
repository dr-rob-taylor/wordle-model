plot_game <- function(history, target_word, filename = "wordle_result.png") {
  library(ggplot2)

  played <- do.call(rbind, lapply(seq_along(history), function(i) {
    data.frame(
      attempt  = i,
      position = 1:5,
      letter   = toupper(strsplit(history[[i]]$guess, "")[[1]]),
      feedback = history[[i]]$feedback,
      stringsAsFactors = FALSE
    )
  }))

  n_played <- length(history)
  if (n_played < 6) {
    empty <- data.frame(
      attempt  = rep((n_played + 1):6, each = 5),
      position = rep(1:5, times = 6 - n_played),
      letter   = "",
      feedback = "empty",
      stringsAsFactors = FALSE
    )
    played <- rbind(played, empty)
  }

  played$feedback <- factor(played$feedback,
                            levels = c("green", "yellow", "grey", "empty"))

  tile_colours <- c(
    green  = "#6aaa64",
    yellow = "#c9b458",
    grey   = "#787c7e",
    empty  = "#ffffff"
  )

  text_colours <- c(
    green  = "white",
    yellow = "white",
    grey   = "white",
    empty  = "white"
  )

  ggplot(played, aes(x = position, y = -attempt)) +
    geom_tile(aes(fill = feedback),
              colour = "#d3d6da", linewidth = 1.5, width = 0.92, height = 0.92) +
    geom_text(aes(label = letter, colour = feedback),
              size = 9, fontface = "bold") +
    scale_fill_manual(values = tile_colours) +
    scale_colour_manual(values = text_colours) +
    coord_fixed() +
    labs(title = paste0("Wordle — ", toupper(target_word))) +
    theme_void() +
    theme(
      legend.position  = "none",
      plot.title       = element_text(hjust = 0.5, size = 16,
                                      face = "bold", margin = margin(b = 12)),
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(16, 16, 16, 16)
    )

  ggsave(filename, width = 3.5, height = 5, dpi = 150, bg = "white")
  invisible(filename)
}
