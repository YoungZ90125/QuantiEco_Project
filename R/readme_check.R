# readme_check.R — word count of the README's extended abstract (course limit: 500 words).
# Counts the words under "## Extended abstract", up to the next "## " heading; subheadings are not counted.
abstract_word_count <- function(path = "README.md") {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  s <- grep("^## Extended abstract", x)
  stopifnot(length(s) == 1)
  e <- c(grep("^## ", x), length(x) + 1)
  e <- min(e[e > s])
  body <- x[seq_len(e - s - 1) + s]
  words <- unlist(strsplit(body[!grepl("^#", body)], "\\s+"))
  length(words[nzchar(words)])
}
