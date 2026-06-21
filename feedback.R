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
