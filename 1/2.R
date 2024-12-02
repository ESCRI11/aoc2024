input <- data.table::fread("input")
input <- data.frame(input)

left_uq <- unique(input[,1] |> unlist())
lapply(left_uq, function(x) {
  n_right <- sum(input[,2] %in% x)
  n_left <- sum(input[,1] %in% x)
  return(n_left * n_right * x)
})
input[,1] <- sort(input[,1] |> unlist())
input[,2] <- sort(input[,2] |> unlist())
sum(abs(input[,1] - input[,2]))
