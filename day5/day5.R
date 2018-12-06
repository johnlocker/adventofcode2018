input <- read.table(file.path("day5", "input.txt"),
                    stringsAsFactors = FALSE)
# test string
# input$V1 <- "dabAcCaCBAcCcaDA" # nolint
inputStr <- input$V1

#' Function to react string
#' @param inputStr string with reaction
#' @return string which does not react anymore
reactPoly <- function(inputStr) {
  lenStr <- nchar(inputStr)
  while (TRUE) {
    for (l in letters) {
      inputStr <- gsub(paste0(toupper(l), l), "", inputStr)
      inputStr <- gsub(paste0(l, toupper(l)), "", inputStr)
    }
    if (lenStr == nchar(inputStr)) {
      print(paste0("Result: ", nchar(inputStr)))
      return(inputStr)
    } else {
      lenStr <- nchar(inputStr)
    }
  }
}
# Part A:
outputStr <- reactPoly(inputStr)

df <- data.frame(letters = letters, stringsAsFactors = FALSE)
df$polyCount <- NA
for (i in 1:length(letters)) {
  inputStrClean <- gsub(letters[i], "", inputStr)
  inputStrClean <- gsub(toupper(letters[i]), "", inputStrClean)
  inputStrClean <- reactPoly(inputStrClean)
  df$polyCount[i] <- nchar(inputStrClean)
  cat("letter:", letters[i], "\n")
}
# Part B:
print(paste0("Result 2: ", min(df$polyCount, na.rm = TRUE)))
