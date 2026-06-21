sample_word <- function(char_mat) {
  word_vec <- character(5)
  rows     <- seq_len(nrow(char_mat))

  for (i in 1:5) {
    t_i      <- table(char_mat[rows, i])
    word_vec[i] <- sample(names(t_i), 1, prob = t_i / sum(t_i))
    rows     <- intersect(rows, which(char_mat[, i] == word_vec[i]))
  }

  paste(word_vec, collapse = "")
}
