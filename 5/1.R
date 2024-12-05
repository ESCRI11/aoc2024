input <- readLines("input")
cutoff <- which(input == "")
input_rules <- input[1:(cutoff-1)]
input_updates <- input[(cutoff+1):length(input)]
input_updates <- lapply(input_updates, function(x) {
  update_char_to_num_list(x)
})
get_pages_from_rule <- function(rule) {
  spl <- strsplit(rule, "|", fixed = TRUE)
  return(list(
    x1 = as.numeric(spl[[1]][1]),
    x2 = as.numeric(spl[[1]][2])
  ))
}
update_char_to_num_list <- function(update) {
  spl <- as.numeric(strsplit(update, ",", fixed = TRUE)[[1]])
}
is_order_right <- function(update, x1, x2) {
  if (!all(c(x1, x2) %in% update)) {
    return(TRUE)
  }
  if (which(update == x1) < which(update == x2)) {
    return(TRUE)
  }
  return(FALSE)
}
get_mid_element <- function(x) {
  x[ceiling(length(x)/2)]
}
correct_updates <- lapply(input_updates, function(x) {
  rules_check <- lapply(input_rules, function(y) {
    pgs <- get_pages_from_rule(y)
    is_order_right(x, pgs[[1]], pgs[[2]])
  }) |> unlist()
  if (all(rules_check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}) |> unlist()
lapply(input_updates[correct_updates], function(x) {
  get_mid_element(x)
}) |> unlist() |> sum()
