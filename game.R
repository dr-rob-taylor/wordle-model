source("sample_word.R")
source("feedback.R")

play_game <- function(char_mat, target_word) {
  target_token <- strsplit(target_word, "")[[1]]
  history <- list()
  solved  <- FALSE

  for (attempt in 1:6) {
    if (nrow(char_mat) == 0) break

    guess       <- sample_word(char_mat)
    guess_token <- strsplit(guess, "")[[1]]
    feedback    <- get_feedback(guess_token, target_token)

    history[[attempt]] <- list(guess = guess, feedback = feedback)

    if (all(feedback == "green")) {
      solved <- TRUE
      break
    }

    char_mat <- apply_feedback(char_mat, guess_token, feedback)
  }

  list(
    target    = target_word,
    solved    = solved,
    n_guesses = if (solved) length(history) else NA_integer_,
    history   = history
  )
}
