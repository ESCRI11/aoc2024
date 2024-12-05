lines <- readLines("input")
char_list <- lapply(lines, strsplit, split = "")
char_matrix <- do.call(rbind, lapply(char_list, function(x) x[[1]]))
search_left <- function(xid, yid, data) {
  if(
    data[xid + 1, yid] == "M" &&
    data[xid + 2, yid] == "A" &&
    data[xid + 3, yid] == "S"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
search_right <- function(xid, yid, data) {
  if(
    data[xid - 1, yid] == "M" &&
    data[xid - 2, yid] == "A" &&
    data[xid - 3, yid] == "S"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
search_up <- function(xid, yid, data) {
  if(
    data[xid, yid - 1] == "M" &&
    data[xid, yid - 2] == "A" &&
    data[xid, yid - 3] == "S"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
search_down <- function(xid, yid, data) {
  if(
    data[xid, yid + 1] == "M" &&
    data[xid, yid + 2] == "A" &&
    data[xid, yid + 3] == "S"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
search_diag_left_down <- function(xid, yid, data) {
  if(
    data[xid + 1, yid + 1] == "M" &&
    data[xid + 2, yid + 2] == "A" &&
    data[xid + 3, yid + 3] == "S"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
search_diag_left_up <- function(xid, yid, data) {
  if(
    data[xid + 1, yid - 1] == "M" &&
    data[xid + 2, yid - 2] == "A" &&
    data[xid + 3, yid - 3] == "S"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
search_diag_right_down <- function(xid, yid, data) {
  if(
    data[xid - 1, yid + 1] == "M" &&
    data[xid - 2, yid + 2] == "A" &&
    data[xid - 3, yid + 3] == "S"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
search_diag_right_up <- function(xid, yid, data) {
  if(
    data[xid - 1, yid - 1] == "M" &&
    data[xid - 2, yid - 2] == "A" &&
    data[xid - 3, yid - 3] == "S"
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
x_index <- which(char_matrix == "X", arr.ind = TRUE)
lapply(1:nrow(x_index), function(x) {
  xid <- x_index[x,][1]
  yid <- x_index[x,][2]
  found <- sum(
    tryCatch({
      search_up(xid, yid, char_matrix)
    }, error = function(w){FALSE}),
    tryCatch({
      search_down(xid, yid, char_matrix)
    }, error = function(w){FALSE}),
    tryCatch({
      search_left(xid, yid, char_matrix)
    }, error = function(w){FALSE}),
    tryCatch({
      search_right(xid, yid, char_matrix)
    }, error = function(w){FALSE}),
    tryCatch({
      search_diag_right_down(xid, yid, char_matrix)
    }, error = function(w){FALSE}),
    tryCatch({
      search_diag_right_up(xid, yid, char_matrix)
    }, error = function(w){FALSE}),
    tryCatch({
      search_diag_left_up(xid, yid, char_matrix)
    }, error = function(w){FALSE}),
    tryCatch({
      search_diag_left_down(xid, yid, char_matrix)
    }, error = function(w){FALSE})
  ) |> unlist()
}) |> unlist() |> sum()
