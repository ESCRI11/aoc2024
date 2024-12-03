input <- readLines("input")
input <- paste(input, sep = "", collapse = "")
regexpr
muls <- regmatches(input, gregexpr("mul\\(\\d+,\\d+\\)", input))[[1]]
multiplier <- function(mul) {
  nums <- stringr::str_match(mul, "mul\\((\\d+),(\\d+)\\)")[,2:3]
  num1 <- as.numeric(nums[1])
  num2 <- as.numeric(nums[2])
  return(num1 * num2)
}
lapply(muls, function(x) {
  multiplier(x)
}) |> unlist() |> sum()
