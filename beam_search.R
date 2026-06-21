beam_search <- function(p1, p12, p123, p1234, p12345,
                        beam_width = 5, pseudocount = 0.01) {

  score <- function(x) log(x + pseudocount)  # log probs for numerical stability

  # Position 1 — initialise beam
  p <- score(p1)
  beam <- data.frame(
    word   = names(p),
    log_prob = as.numeric(p),
    stringsAsFactors = FALSE
  )
  beam <- beam[order(beam$log_prob, decreasing = TRUE), ][1:beam_width, ]

  # Positions 2 through 5
  joint_tables <- list(p12, p123, p1234, p12345)

  for (pos in 2:5) {
    candidates <- list()

    for (i in seq_len(nrow(beam))) {
      prefix  <- beam$word[i]
      letters <- strsplit(prefix, "")[[1]]

      # Get the conditional distribution for this position given full prefix
      dist <- switch((pos - 1),
                     score(p12[letters[1], ]),
                     score(p123[letters[1], letters[2], ]),
                     score(p1234[letters[1], letters[2], letters[3], ]),
                     score(p12345[letters[1], letters[2], letters[3], letters[4], ])
      )

      # Expand: append each possible next letter
      candidates[[i]] <- data.frame(
        word     = paste0(prefix, names(dist)),
        log_prob = beam$log_prob[i] + as.numeric(dist),
        stringsAsFactors = FALSE
      )
    }

    # Merge all expansions, prune to top k
    all_candidates <- do.call(rbind, candidates)
    beam <- all_candidates[order(all_candidates$log_prob, decreasing = TRUE), ][1:beam_width, ]
  }

  rownames(beam) <- NULL
  beam
}
