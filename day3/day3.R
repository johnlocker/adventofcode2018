input <- read.table(file.path("day3", "input.txt"),
                     comment.char = "", stringsAsFactors = FALSE)
colnames(input) <- c("id", "at", "loc", "size")

#' Extract digits
#' @param x String
#' @return digits from string
extractDigits <- function (x) {
  return(readr::parse_number(x))
}

input$id <- extractDigits(input$id)
fieldA <- matrix(NA, ncol = 1001, nrow = 1001)
fieldB <- matrix(NA, ncol = 1001, nrow = 1001)
input$conflict <- NA
input$square <- NA
squareInches <- 0
for (i in 1:nrow(input)) {
  loc <- extractDigits(unlist(strsplit(input$loc[i], ","))) + 1
  size <- extractDigits(unlist(strsplit(input$size[i], "x")))
  id <- input$id[i]
  # part A
  for (w in loc[1]:( (loc[1] + size[1]) - 1)) {
    for (h in loc[2]:( (loc[2] + size[2])  - 1)) {
      if (is.na(fieldA[h, w])) {
        fieldA[h, w] <- 1
      } else if (fieldA[h, w] == 1) {
        fieldA[h, w] <- 2
        squareInches <- squareInches + 1
      } else {
        fieldA[h, w] <- fieldA[h, w] + 1
      }
    }
  }
  # part B
  if (all(is.na(fieldB[(loc[1]:(loc[1] + size[1])),
                       (loc[2]:(loc[2] + size[2]))]))) {
    fieldB[(loc[1]:(loc[1] + size[1])),
           (loc[2]:(loc[2] + size[2]))] <- id
    input$conflict[i] <- FALSE
  } else {
    conflictNums <- unique(as.numeric(fieldB[(loc[1]:(loc[1] + size[1])),
                                             (loc[2]:(loc[2] + size[2]))]))
    for (conNums in conflictNums) {
      input$conflict[which(input$id == conNums)] <- TRUE
    }
    input$conflict[i] <- TRUE
    fieldB[(loc[1]:(loc[1] + size[1])),
           (loc[2]:(loc[2] + size[2]))] <- id
  }
}
# answer 1
print(paste0("Square Inches: ", squareInches))
# answer 2
print(paste0("None conflict ID: ", input$id[which(!input$conflict)]))
