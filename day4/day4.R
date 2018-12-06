input <- read.table(file.path("day4", "input.txt"),
                    stringsAsFactors = FALSE, comment.char = "", fill = TRUE)
#' Extract digits
#' @param x String
#' @return digits from string
extractDigits <- function (x) {
  return(readr::parse_number(x))
}
input$V1 <- gsub("1518", "2018", input$V1)

input$time <- NA
input$time_clear <- NA
for (i in 1:nrow(input)) {
  input$time[i] <- as.POSIXct(paste(c(input$V1[i], input$V2[i]), collapse = " "),
                                           format = "[%Y-%m-%d %H:%M]")
  input$time_clear[i] <- as.character(as.POSIXct(paste(c(input$V1[i], input$V2[i]), collapse = " "),
                              format = "[%Y-%m-%d %H:%M]"))
}
input <- input[order(input$time), ]
input$id <- suppressWarnings(extractDigits(input$V4))
guardDF <- data.frame(id = unique(input$id[!is.na(input$id)]))
guardDF$sleep_duration <- NA
ids <- c()
df <- data.frame()
for (i in 1:nrow(input)) {
  if (input$V3[i] == "Guard") {
    curID <- input$id[i]
    guardIdx <- which(guardDF$id == curID)
  }
  if (input$V3[i] == "falls") {
    startMinute <- as.numeric(unlist(strsplit(input$time_clear[i], ":"))[2])
     startMinute <- ifelse(is.na(startMinute), 0, startMinute)
  }
  if (input$V3[i] == "wakes") {
    wakeMinute <- as.numeric(unlist(strsplit(input$time_clear[i], ":"))[2])
    duration <- wakeMinute - startMinute
    dfRow <- data.frame(id = curID,
                        duration,
                        minutes = paste(c(startMinute:(wakeMinute - 1)), collapse = "-"),
                        stringsAsFactors = FALSE)
    df <- rbind(df, dfRow)
  }
}
# check which guard was asleep the most
guardDurations <- aggregate(duration ~ id, data = df, sum)
guardDurations <- guardDurations[order(guardDurations$duration, decreasing = TRUE), ]
# choose guards sleep pattern
sel <- df[which(df$id == guardDurations$id[1]), ]
mins <- c()
for (i in 1:nrow(sel)) {
  mins <- c(mins, as.numeric(unlist(strsplit(sel$minutes[i], "-"))))
}
cat("Part A:", guardDurations$id[1] * as.numeric(names(which.max(table(mins)))))

# Part B
uniqueIds <- unique(df$id)
dfMax <- data.frame()
for (i in 1:length(uniqueIds)) {
  selUser <- subset(df, id == uniqueIds[i])
  selMinutes <- c()
  for (m in 1:nrow(selUser)) {
    selMinutes <- c(selMinutes, as.numeric(unlist(strsplit(selUser[m, "minutes"], "-"))))
  }
  dfUser <- data.frame(id = uniqueIds[i],
                       max = max(table(selMinutes)),
                       minute = as.numeric(labels(which.max(table(selMinutes)))),
                       stringsAsFactors = FALSE)
  dfMax <- rbind(dfMax, dfUser)
}
dfMax <- dfMax[order(dfMax$max, decreasing = TRUE), ]
cat("Part B:", dfMax$id[1] * dfMax$minute[1])
