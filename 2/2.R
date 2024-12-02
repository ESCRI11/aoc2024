input <- readLines("input")
is_safe <- function(numbers) {
  diferences <- diff(numbers)
  if (length(unique(sign(diferences))) > 1) return(FALSE)
  if (!all(abs(diferences) %in% c(1,2,3))) return(FALSE)
  return(TRUE)
}
rep_safe <- lapply(input, function(x) {
  numbers <- as.numeric(unlist(strsplit(x, " ")))
  res <- is_safe(numbers)
  if (!res) {
    res2 <- lapply(1:length(numbers), function(y) {
      is_safe(numbers[-y])
    }) |> unlist()
    if(any(res2)) return(TRUE)
  }
  return(is_safe(numbers))
}) |> unlist()
sum(rep_safe)
