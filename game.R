source("sample_word.R")
source("feedback.R")

#' Play a single game of Wordle
#'
#' Runs one complete game against a known target word. On each attempt the
#' model samples a guess uniformly from the current candidate matrix, obtains
#' feedback via \code{\link{get_feedback}}, and filters the matrix with
#' \code{\link{apply_feedback}}. Play continues until the target is guessed or
#' six attempts are exhausted.
#'
#' @param char_mat    Character matrix with one row per candidate word and five
#'   columns, one per letter position. Should represent the full word corpus at
#'   the start of each game; it is filtered internally and the original is not
#'   modified.
#' @param target_word A single five-letter string — the word the model is
#'   trying to guess.
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{\code{target}}{The target word (character).}
#'     \item{\code{solved}}{Logical; \code{TRUE} if the target was guessed
#'       within six attempts.}
#'     \item{\code{n_guesses}}{Integer number of guesses used if solved;
#'       \code{NA} otherwise.}
#'     \item{\code{history}}{A list of up to six elements, each a named list
#'       with \code{guess} (character) and \code{feedback} (character vector of
#'       length 5).}
#'   }
#'
#' @seealso \code{\link{simulate_games}} to run many games and collect results
#'   in a data frame.
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
