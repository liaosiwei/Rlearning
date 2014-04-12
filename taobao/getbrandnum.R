analyzeresult <- function(filename = "") {
  # filename: the name of input file
  # return the total number of the brand id
  conn <- file(filename, "r")
  d <- readLines(conn)
  close(conn)
  num <- 0
  for (line in d) {
    brands = strsplit(strsplit(line, "\t", fixed = TRUE)[[1]][2], ",")[[1]]
    num <- num + length(brands)
  }
  list(user_num = length(d), brand_num = num)
}