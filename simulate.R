source("game.R")

simulate_games <- function(n, words) {
  char_mat <- do.call(rbind, strsplit(words, ""))

  results <- lapply(seq_len(n), function(i) {
    target <- sample(words, 1)
    result <- play_game(char_mat, target)
    data.frame(
      game      = i,
      target    = result$target,
      solved    = result$solved,
      n_guesses = result$n_guesses,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
