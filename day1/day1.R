# part 1
data <- read.table(file.path("day1", "input.txt"))
# solution part 1:
sum(data$V1)

# part 2
values <- c()
start <- 0
while (TRUE) {
  values <- c(values, cumsum(c(start, data$V1)))
  if (any(duplicated(values))) {
    # solution part 1:
    print(values[which(duplicated(values))][1])
    break
  }
  start <- values[length(values)]
  values <- values[-length(values)]
  cat(paste0("Lenght values: ", length(values), "\n"))
}
