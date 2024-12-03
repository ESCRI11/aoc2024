input <- readLines("input")
input <- paste(input, sep = "", collapse = "")
pattern <- "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)"
matches <- regmatches(input, gregexpr(pattern, input))[[1]]
multiplier <- function(mul) {
  nums <- stringr::str_match(mul, "mul\\((\\d+),(\\d+)\\)")[,2:3]
  num1 <- as.numeric(nums[1])
  num2 <- as.numeric(nums[2])
  return(num1 * num2)
}
muls <- c()
skip <- FALSE
for (match in matches) {
  if (match == "don't()") {
    skip <- TRUE
  } else if (match == "do()") {
    skip <- FALSE
  } else if (!skip) {
    muls <- c(muls, match)
  }
}
lapply(muls, function(x) {
  multiplier(x)
}) |> unlist() |> sum()
