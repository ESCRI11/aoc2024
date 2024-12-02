input <- readLines("input")
rep_safe <- lapply(input, function(x) {
  numbers <- as.numeric(unlist(strsplit(x, " ")))
  diferences <- diff(numbers)
  if (length(unique(sign(diferences))) > 1) return(FALSE)
  if (!all(abs(diferences) %in% c(1,2,3))) return(FALSE)
  return(TRUE)
}) |> unlist()
sum(rep_safe)
