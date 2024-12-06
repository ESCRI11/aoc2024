input <- readLines("input")
cutoff <- which(input == "")
input_rules <- input[1:(cutoff-1)]
input_updates <- input[(cutoff+1):length(input)]
update_char_to_num_list <- function(update) {
  spl <- as.numeric(strsplit(update, ",", fixed = TRUE)[[1]])
}
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
reverse_rule <- function(index_rule, update) {
  pages <- get_pages_from_rule(input_rules[index_rule])
  id1 <- which(update == pages[1])
  id2 <- which(update == pages[2])
  if (id1 == id2 - 1) return(update)
  element_to_move <- update[id1]
  update <- update[-id1]
  if (id1 < id2) id2 <- id2 - 1
  update <- append(update, element_to_move, after = id2 - 1)
  return(update)
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
incorrect_updates <- !correct_updates
fixed_rules <- lapply(input_updates[incorrect_updates], function(x) {
  all_rules_true <- FALSE
  while(!all_rules_true){
    rules_check <- lapply(input_rules, function(y) {
      pgs <- get_pages_from_rule(y)
      is_order_right(x, pgs[[1]], pgs[[2]])
    }) |> unlist()
    incorrect_rules <- which(rules_check == FALSE)
    for (i in incorrect_rules) {
      x <- reverse_rule(i, x)
    }
    rules_check <- lapply(input_rules, function(y) {
      pgs <- get_pages_from_rule(y)
      is_order_right(x, pgs[[1]], pgs[[2]])
    }) |> unlist()
    if (all(rules_check)) {all_rules_true <- TRUE}
  }
  return(x)
})
lapply(fixed_rules, function(x) {
  get_mid_element(x)
}) |> unlist() |> sum()
