input <- readLines("input")
split_lines <- strsplit(input, "")
matrix_data <- do.call(rbind, split_lines)
update_direction <- function(dir) {
  switch(
   dir,
   "^" = ">",
   ">" = "v",
   "v" = "<",
   "<" = "^"
  )
}
search <- function(idx, idy, direction) {
  if(direction == "^") {
    blocker <- max(which(matrix_data[,idy][1:idx] == "#"))
    if(is.infinite(blocker)) {
      matrix_data[1:idx,idy] <- "X"
      matrix_data <<- matrix_data
      return(NULL)
    }
    matrix_data[(blocker + 1):idx,idy] <- "X"
    matrix_data <<- matrix_data
    return(list(
      idx = blocker + 1,
      idy = idy,
      direction = update_direction(direction)
    ))
  }
  if(direction == ">") {
    blocker <- min(which(matrix_data[idx,][idy:nrow(matrix_data)] == "#"))
    if(is.infinite(blocker)) {
      matrix_data[idx,idy:ncol(matrix_data)] <- "X"
      matrix_data <<- matrix_data
      return(NULL)
    }
    matrix_data[idx,idy:(idy + blocker - 2)] <- "X"
    matrix_data <<- matrix_data
    return(list(
      idx = idx,
      idy = idy + blocker - 2,
      direction = update_direction(direction)
    ))
  }
  if(direction == "v") {
    blocker <- min(which(matrix_data[,idy][idx:ncol(matrix_data)] == "#"))
    if(is.infinite(blocker)) {
      matrix_data[idx:nrow(matrix_data),idy] <- "X"
      matrix_data <<- matrix_data
      return(NULL)
    }
    matrix_data[idx:(idx + blocker - 2),idy] <- "X"
    matrix_data <<- matrix_data
    return(list(
      idx = idx + blocker - 2,
      idy = idy,
      direction = update_direction(direction)
    ))
  }
  if(direction == "<") {
    blocker <- max(which(matrix_data[idx,][1:idy] == "#"))
    if(is.infinite(blocker)) {
      matrix_data[idx,1:idy] <- "X"
      matrix_data <<- matrix_data
      return(NULL)
    }
    matrix_data[idx,idy:(blocker + 1)] <- "X"
    matrix_data <<- matrix_data
    return(list(
      idx = idx,
      idy = blocker + 1,
      direction = update_direction(direction)
    ))
  }
}
start_idx <- which(matrix_data == "^", arr.ind = TRUE)
idx <- start_idx[1]
idy <- start_idx[2]
direction <- "^"
lesgo <- TRUE
while(lesgo) {
  new_pos <- search(idx, idy, direction)
  if(is.null(new_pos)) lesgo <- FALSE
  idx <- new_pos$idx
  idy <- new_pos$idy
  direction <- new_pos$direction
}
length(which(matrix_data == "X")) |> sum()
