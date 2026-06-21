source("game.R")
source("plot_game.R")

words      <- read.csv("words.csv", stringsAsFactors = FALSE)
words$word <- tolower(words$word)

char_mat    <- do.call(rbind, strsplit(words$word, ""))
target_word <- sample(words$word, 1)

cat("Target:", target_word, "\n\n")

result <- play_game(char_mat, target_word)

for (i in seq_along(result$history)) {
  h <- result$history[[i]]
  cat(sprintf("Guess %d: %s  [%s]\n", i, h$guess, paste(h$feedback, collapse = ", ")))
}

if (result$solved) {
  cat("Solved in", result$n_guesses, "guess(es).\n")
} else {
  cat("Failed — target was:", target_word, "\n")
}

plot_game(result$history, target_word)
cat("Game image saved to wordle_result.png\n")
