#' Sample a word from the candidate character matrix
#'
#' Draws a single five-letter word by sampling each position autoregressively:
#' position \eqn{i} is sampled from the empirical letter distribution in
#' column \eqn{i} of the rows that are still consistent with positions
#' \eqn{1, \ldots, i-1}. The resulting word distribution is uniform over all
#' words represented in \code{char_mat}.
#'
#' @param char_mat A character matrix with one row per candidate word and five
#'   columns, one per letter position. Typically the full corpus matrix or a
#'   filtered subset produced by \code{\link{apply_feedback}}.
#'
#' @return A single five-letter string sampled from the rows of
#'   \code{char_mat}.
#'
#' @seealso \code{\link{apply_feedback}} to filter \code{char_mat} using game
#'   feedback before the next call to \code{sample_word}.
sample_word <- function(char_mat) {
  word_vec <- character(5)
  rows     <- seq_len(nrow(char_mat))

  for (i in 1:5) {
    t_i         <- table(char_mat[rows, i])
    word_vec[i] <- sample(names(t_i), 1, prob = t_i / sum(t_i))
    rows        <- intersect(rows, which(char_mat[, i] == word_vec[i]))
  }

  paste(word_vec, collapse = "")
}
