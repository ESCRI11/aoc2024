lines <- readLines("input")
char_list <- lapply(lines, strsplit, split = "")
char_matrix <- do.call(rbind, lapply(char_list, function(x) x[[1]]))
search_diag_1 <- function(xid, yid, data) {
  if(
    data[xid + 1, yid + 1] == "M" &&
    data[xid - 1, yid - 1] == "S" ||
    data[xid + 1, yid + 1] == "S" &&
    data[xid - 1, yid - 1] == "M"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
search_diag_2 <- function(xid, yid, data) {
  if(
    data[xid - 1, yid + 1] == "M" &&
    data[xid + 1, yid - 1] == "S" ||
    data[xid - 1, yid + 1] == "S" &&
    data[xid + 1, yid - 1] == "M"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
x_index <- which(char_matrix == "A", arr.ind = TRUE)
lapply(1:nrow(x_index), function(x) {
  xid <- x_index[x,][1]
  yid <- x_index[x,][2]
  found <- sum(
    tryCatch({
      search_diag_1(xid, yid, char_matrix) &&
      search_diag_2(xid, yid, char_matrix)
    }, error = function(w){FALSE})
  ) |> unlist()
}) |> unlist() |> sum()
