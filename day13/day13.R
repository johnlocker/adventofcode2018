fileName <- file.path("day13", "input.txt")
input <- readChar(fileName, file.info(fileName)$size)
inputSplit <- unlist(strsplit(input, split = ""))
grid <- matrix(NA, ncol = which(inputSplit == "\n")[1] - 1,
               nrow = length(which(inputSplit == "\n")))
row <- 1
col <- 1
cars <- c(">", "v", "<", "^")
carReplace <- c("-", "|", "-", "|")
carList <- list()
carNum <- 1
for (i in 1:length(inputSplit)) {
  curChar <- inputSplit[i]
  if (curChar == "\n") {
    row <- row + 1
    col <- 1
    next
  }
  if (curChar %in% cars) {
    carList[[carNum]] <- carNum
    carList[[carNum]]["loc"] <- paste(c(row, col), collapse = ",")
    carList[[carNum]]["direction"] <- curChar
    carList[[carNum]]["intersecAction"] <- "left"
    carList[[carNum]]["moved"] <- 0
    carNum <- carNum + 1
    curChar <- carReplace[which(cars == curChar)]
  }
  if (curChar == "\r") curChar <- " "
  grid[row, col] <- curChar
  col <- col + 1
}

#' Function to sort cars according to position on map
#' @param carList list of cars with their position
#' @return numbers of cars sorted by position
carSequence <- function(carList) {
  sortDF <- data.frame()
  for (i in 1:length(carList)) {
    curLoc <- as.numeric(unlist(strsplit(carList[[i]]["loc"], ",")))
    sortDF <- rbind(sortDF,
                    data.frame(num = carList[[i]][1],
               row = curLoc[2],
               col = curLoc[1],
               stringsAsFactors = FALSE))
  }
  sortDF <- sortDF[order(sortDF$col, sortDF$row), ]
  return(sortDF$num)
}

firstPart <- TRUE
while (TRUE) {
  for (iCar in carSequence(carList)) {
    carIdx <- which(sapply(c(1:length(carList)), function(x) carList[[x]][1] == iCar))
    if (length(carIdx) == 0) {
      next
    } else {
      car <- carList[[carIdx]]
    }
    curLoc <- as.numeric(unlist(strsplit(car["loc"], ",")))
    carList[[carIdx]]["moved"] <- as.numeric(car["moved"]) + 1
    if (car["direction"] == "<") {
      curLoc[2] <- curLoc[2] - 1
    } else if (car["direction"] == ">") {
      curLoc[2] <- curLoc[2] + 1
    } else if (car["direction"] == "^") {
      curLoc[1] <- curLoc[1] - 1
    } else if (car["direction"] == "v") {
      curLoc[1] <- curLoc[1] + 1
    }
    newPos <- grid[curLoc[1], curLoc[2]]
    if (newPos == "/") {
      if (car["direction"] == "<") {
        newDir <- "v"
      } else if (car["direction"] == "^") {
        newDir <- ">"
      } else if (car["direction"] == "v") {
        newDir <- "<"
      } else if (car["direction"] == ">") {
        newDir <- "^"
      }
    } else if (newPos == "\\") {
      if (car["direction"] == "<") {
        newDir <- "^"
      }
      if (car["direction"] == "^") {
        newDir <- "<"
      }
      if (car["direction"] == "v") {
        newDir <- ">"
      }
      if (car["direction"] == ">") {
        newDir <- "v"
      }
    } else {
      newDir <- car["direction"]
    }
    if (newPos == "+") {
      if (car["intersecAction"] == "left") {
        newDirIdx <- which(cars == car["direction"]) - 1
        newDir <- cars[ifelse(newDirIdx == 0, 4, newDirIdx)]
        carList[[carIdx]]["intersecAction"] <- "straight"
      } else if (car["intersecAction"] == "straight") {
        newDir <- car["direction"]
        carList[[carIdx]]["intersecAction"] <- "right"
      } else if (car["intersecAction"] == "right") {
        newDirIdx <- which(cars == car["direction"]) + 1
        newDir <- cars[ifelse(newDirIdx == 5, 1, newDirIdx)]
        carList[[carIdx]]["intersecAction"] <- "left"
      }
    }
    parsedLoc <- paste(c(curLoc[1], curLoc[2]), collapse = ",")
    crashReport <- sapply(c(1:length(carList)),
                          function(x) parsedLoc == carList[[x]]["loc"])
    if (any(crashReport)) {
      removeCar <- carList[[which(crashReport)]][1]
      removeCarI <- car[1]
      if (firstPart) {
        cat("Crash at:", paste(c(curLoc[2] - 1, curLoc[1] - 1), collapse = ","), "\n")
        firstPart <- FALSE
      }
      # remove
      carList <- carList[-which(sapply(c(1:length(carList)),
                                       function(x) carList[[x]][1] %in%
                                         c(removeCar, removeCarI)))]
    } else {
      carList[[carIdx]]["loc"] <- parsedLoc
      carList[[carIdx]]["direction"] <- newDir
    }
  }
  if (length(carList) == 1) {
    finalLoc <- as.numeric(unlist(strsplit(carList[[1]]["loc"], ",")))
    cat("Last Car at:", paste(c(finalLoc[2] - 1, finalLoc[1] - 1), collapse = ","))
    break
  }
}
