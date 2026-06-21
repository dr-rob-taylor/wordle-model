source("game.R")

#' Simulate multiple Wordle games
#'
#' Runs \code{n} independent games, each against a target word drawn uniformly
#' at random from \code{words}. The candidate character matrix is built once
#' from the full corpus and reset to its original state at the start of every
#' game, so games are independent of one another.
#'
#' @param n     Integer. Number of games to simulate.
#' @param words Character vector of five-letter words forming the corpus. Used
#'   both as the pool from which targets are drawn and as the initial candidate
#'   set for each game.
#'
#' @return A data frame with \code{n} rows and four columns:
#'   \describe{
#'     \item{\code{game}}{Integer game index (1 to \code{n}).}
#'     \item{\code{target}}{Character. The target word for that game.}
#'     \item{\code{solved}}{Logical. Whether the target was found within six
#'       guesses.}
#'     \item{\code{n_guesses}}{Integer. Number of guesses used if solved;
#'       \code{NA} otherwise.}
#'   }
#'
#' @seealso \code{\link{play_game}} for the single-game logic;
#'   \code{\link{compute_stats}} and \code{\link{plot_stats}} to summarise and
#'   visualise the returned data frame.
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
