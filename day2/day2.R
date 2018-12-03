input <- read.table(file.path("day2", "input.txt"), stringsAsFactors = FALSE)

df <- data.frame(str = input$V1)
df$two <- FALSE
df$three <- FALSE
df$values <- NA
matchNotFound <- TRUE
for (i in 1:length(input$V1)) {
  spLetters <- unlist(strsplit(as.character(input$V1[i]), split = ""))
  counts <- table(spLetters)
  df$two[i] <- sum(counts == 2) > 0
  df$three[i] <- sum(counts == 3) > 0
  df$values[i] <- paste(sapply(spLetters, function(x) match(x, letters)),
                        collapse = "-")
  if (i > 1 & matchNotFound) {
    cur <- as.numeric(sapply(spLetters, function(x) match(x, letters)))
    subLoop <- c(1:i)[which(c(1:i) != i)]
    for (j in subLoop) {
      otherVals <- as.numeric(unlist(strsplit(df$values[j], "-")))
      if ( (sum( (cur - otherVals) == 0) == 25) &
          (sum(abs(cur - otherVals) == 1) == 1)) {
        cat(paste0("Part B: ", paste(spLetters[-which( (cur - otherVals) != 0)],
                                     collapse = "")), "\n")
        matchNotFound <- FALSE
       }
    }
  }
}
cat(paste0("\nPart A: ", sum(df$two) * sum(df$three), "\n"))
