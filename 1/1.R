input <- data.table::fread("input")
input[,1] <- sort(input[,1] |> unlist())
input[,2] <- sort(input[,2] |> unlist())
sum(abs(input[,1] - input[,2]))
