input <- read.table(file.path("day12", "input.txt"),
                    stringsAsFactors = FALSE, comment.char = "", sep = "\n")
initial <- unlist(strsplit(unlist(strsplit(input$V1[1], split = " "))[3], ""))

rules <- input$V1[c(2:nrow(input))]
ruleDF <- data.frame()
for (r in 1:length(rules)) {
  splitRule <- unlist(strsplit(rules[r], " => "))
  if (splitRule[2] == ".") {
    next
  }
  ruleDF <- rbind(ruleDF, data.frame(pattern = splitRule[1],
                                     result = splitRule[2],
                                     stringsAsFactors = FALSE))
}

#' Function to check if pot pattern is found
#' @param state state if five pots
#' @param ruleDF data frame with rules about when plant is grown
#' @return 0 if pots get no plant in third pot and 1 if they do
checkPattern <- function(state, ruleDF) {
  idx <- which(ruleDF$pattern == paste(state, collapse = ""))
  if (length(idx) == 0) {
    return(0)
  } else {
    return(1)
  }
}

#' Function to calcuate new pots
#' @param oldPots old pots configuration
#' @param nums numbers of first and last pot
#' @return vector with new pots
makeNewPots <- function(oldPots, nums) {
  lenOld <- length(oldPots)
  firstPlant <- which(oldPots == "#")[1]
  lastPlant <- rev(which(oldPots == "#"))[1]
  if (firstPlant < 4) {
    missingLow <- 4 - firstPlant
    oldPots <- c(rep(".", missingLow), oldPots)
    nums[1] <- nums[1] - missingLow
  } else if (firstPlant > 4) {
    extraLow <- firstPlant - 4
    oldPots <- oldPots[c(-1:-extraLow)]
    nums[1] <- nums[1] + extraLow
  }
  if ( (lenOld - lastPlant) < 3) {
    missingHigh <- ( (lastPlant + 3) - lenOld)
    oldPots <- c(oldPots, rep(".", missingHigh))
    nums[2] <- nums[2] + missingHigh
  } else if ( (lenOld - lastPlant) > 3) {
    extraHigh <- (lenOld - (lastPlant + 3))
    oldPots <- oldPots[c( -(length(oldPots) - extraHigh + 1):-length(oldPots))] # nolint
    nums[2] <- nums[2] - extraHigh
  }
  res <- c(rep(0, 2), sapply(c(3:(length(oldPots) - 2)), function(x) {
    checkPattern(oldPots[c( (x - 2):(x + 2))], ruleDF = ruleDF)
  }
  ), rep(0, 2))
  return(list(newPots = ifelse(res == 0, ".", "#"),
              nums = nums))
}

#' Function to make string from pots
#' @param pots pot vector
#' @return pot vector as string
makePotString <- function(pots) {
  potsCollapsed <- paste(pots, collapse = "")
  return(potsCollapsed)
}

gen <- 0
pots <- c(rep(".", 2), initial, rep(".", 2))
nums <- c(-2, (length(initial) + 1))
maxGen <- 20
potSigns <- c()
numsSave <- c()

part1 <- FALSE

while (TRUE) {
  t0 <- Sys.time()
  result <- makeNewPots(pots, nums)
  pots <- result$newPots
  nums <- result$nums
  potSign <- makePotString(pots)
  if (potSign %in% potSigns) {
    cat("found pot")
    break
  }
  numsSave <- c(numsSave, paste(nums, collapse = " "))
  potSigns <- c(potSigns, potSign)
  gen <- gen + 1

  if (gen == maxGen & part1) {
    cat("Solution: ", sum(ifelse(pots == ".", 0, 1) * c(nums[1]:nums[2])))
    break
  }
}
if (!part1) {
  maxGenT <- 50000000000
  remGens <- maxGenT - gen
  remNums <- as.numeric(unlist(strsplit(numsSave[length(numsSave)], " "))) + remGens
  cat("Solution: ", sum(ifelse(pots == ".", 0, 1) * c(remNums[1]:remNums[2])))
}
