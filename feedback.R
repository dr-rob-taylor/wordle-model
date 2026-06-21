#' Classify guess letters as green, yellow, or grey
#'
#' Computes Wordle feedback for a single guess against a target word using a
#' two-pass algorithm that correctly handles duplicate letters. The first pass
#' identifies exact positional matches (green) and builds a multiset budget of
#' unmatched target letters. The second pass consumes from that budget
#' left-to-right, assigning yellow where possible and grey otherwise.
#'
#' @param guess_token  Character vector of length 5 — the guess split into
#'   individual letters, e.g. \code{strsplit("crane", "")[[1]]}.
#' @param target_token Character vector of length 5 — the target split into
#'   individual letters.
#'
#' @return A character vector of length 5 with values \code{"green"},
#'   \code{"yellow"}, or \code{"grey"} corresponding to each letter position.
#'
#' @seealso \code{\link{apply_feedback}} to filter the candidate matrix using
#'   the returned feedback vector.
get_feedback <- function(guess_token, target_token) {
  feedback <- character(5)

  # First pass: greens consume target letters
  is_green <- guess_token == target_token
  feedback[is_green] <- "green"

  # Remaining target letters available for yellow assignment
  remaining <- target_token[!is_green]

  # Second pass: assign yellow/grey left-to-right against remaining budget
  for (i in which(!is_green)) {
    letter <- guess_token[i]
    if (letter %in% remaining) {
      feedback[i] <- "yellow"
      remaining   <- remaining[-match(letter, remaining)]
    } else {
      feedback[i] <- "grey"
    }
  }

  feedback
}


#' Filter the candidate matrix using guess feedback
#'
#' Removes rows from \code{char_mat} that are inconsistent with the feedback
#' returned by \code{\link{get_feedback}}. Three constraint types are applied
#' sequentially:
#' \itemize{
#'   \item \strong{Green} — fixes the column to the guessed letter.
#'   \item \strong{Yellow} — excludes the letter from that column and requires
#'     it to appear at least once elsewhere in the row.
#'   \item \strong{Grey} — constrains the total count of the letter across all
#'     columns. If the letter is purely grey its count must be zero; if it also
#'     has green or yellow hits its count must equal the number of those hits.
#' }
#'
#' @param char_mat    Character matrix with one row per candidate word and five
#'   columns, one per letter position.
#' @param guess_token Character vector of length 5 — the guess split into
#'   individual letters.
#' @param feedback    Character vector of length 5 with values \code{"green"},
#'   \code{"yellow"}, or \code{"grey"}, as returned by
#'   \code{\link{get_feedback}}.
#'
#' @return A character matrix of the same structure as \code{char_mat} with
#'   inconsistent rows removed. May have zero rows if no candidates remain.
#'
#' @seealso \code{\link{get_feedback}} to compute \code{feedback} from a guess
#'   and target pair; \code{\link{sample_word}} to draw the next guess from the
#'   filtered matrix.
apply_feedback <- function(char_mat, guess_token, feedback) {

  # Green: fix the column to this letter
  for (i in which(feedback == "green")) {
    char_mat <- char_mat[char_mat[, i] == guess_token[i], , drop = FALSE]
  }

  # Yellow: exclude letter from this column, require it somewhere in the row
  for (i in which(feedback == "yellow")) {
    letter     <- guess_token[i]
    char_mat   <- char_mat[char_mat[, i] != letter, , drop = FALSE]
    has_letter <- apply(char_mat, 1, function(row) letter %in% row)
    char_mat   <- char_mat[has_letter, , drop = FALSE]
  }

  # Grey: handle per unique letter to account for duplicates.
  # If the letter also has yellow/green hits, grey means "no additional
  # occurrences beyond those hits" — constrain to exact count.
  # If the letter is purely grey, it is absent entirely.
  for (letter in unique(guess_token[feedback == "grey"])) {
    n_non_grey   <- sum(feedback != "grey" & guess_token == letter)
    letter_count <- apply(char_mat, 1, function(row) sum(row == letter))
    char_mat     <- char_mat[letter_count == n_non_grey, , drop = FALSE]
  }

  char_mat
}
