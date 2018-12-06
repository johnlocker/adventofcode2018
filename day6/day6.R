inputRaw <- read.table(file.path("day6", "input.txt"),
                    stringsAsFactors = FALSE, sep = ",")
colnames(inputRaw) <- c("ncol", "nrow")
inputRaw$name <- c(paste0(LETTERS, LETTERS),
                   paste0(LETTERS, rev(LETTERS)))[c(1:nrow(inputRaw))]

#' Calculate Manhatten Distance
#' @param loc First location
#' @param letterLoc Second location
#' @return manhatten distance
getDist <- function(loc, letterLoc) {
  dist <- abs(loc - letterLoc)
  dist <- sum(dist)
  return(dist)
}

#' Calculate Count of elements on field
#' @param input Input locations of elements
#' @param field A field to place input on
#' @param saveThres Threshold that indicates save distances
#' @return Count of elements on the field
computeDangerRegions <- function(input, field, saveThres = NULL) {
  # place letters
  for (i in 1:nrow(input)) {
    field[input$nrow[i], input$ncol[i]] <- input$name[i]
  }
  # determine distances
  for (r in 1:nrow(field)) {
    for (co in 1:nrow(field)) {
      distances <- apply(input, 1, function(x) getDist(letterLoc = as.numeric(c(x[1], x[2])),
                                                       loc = c(r, co)))
      minDist <- min(distances)
      sumDist <- sum(distances)
      if (is.null(saveThres)) {
        if (sum(minDist == distances) == 1) {
          field[r, co] <- tolower(input$name[which.min(distances)])
        }
      } else {
        if (sumDist < saveThres) {
          field[r, co] <- "#"
        }
      }
    }
  }
  count <- table(tolower(field))
  return(count)
}

field <- matrix(".", ncol = ceiling(max(inputRaw$ncol) / 100) * 100,
                nrow = ceiling(max(inputRaw$nrow) / 100) * 100)
fieldLarge <- matrix(".", ncol = ceiling(max(inputRaw$ncol) / 100) * 110,
                      nrow = ceiling(max(inputRaw$nrow) / 100) * 110)
input1 <- inputRaw
input1$ncol <- input1$ncol + 1
input1$nrow <- input1$nrow + 1
input5 <- inputRaw
input5$ncol <- input5$ncol + 5
input5$nrow <- input5$nrow + 5

# calculate two field to determine letters that are infinite
count1 <- computeDangerRegions(input1, field)
count2 <- computeDangerRegions(input5, fieldLarge)
countDF <- cbind(count1, count2)
finitLetters <- rownames(countDF)[apply(countDF, 1, duplicated)[2, ]]
cat(paste0("Part 1: ", max(count1[which(names(count1) %in% finitLetters)])))
cat(paste0("Letter: ",
           toupper(names(which.max(count1[which(names(count1)
                                                %in% finitLetters)])))))
# part B with threshold
count <- computeDangerRegions(input1, field, saveThres = 10000)
cat(paste0("Part 2: ", count[which(names(count) == "#")]))
