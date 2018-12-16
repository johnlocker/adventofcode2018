input <- read.table(file.path("day16", "inputA.txt"),
                    stringsAsFactors = FALSE, sep = "\n")
inputB <- read.table(file.path("day16", "inputB.txt"),
                    stringsAsFactors = FALSE, sep = "\n")

#' Extract digits
#' @param x String
#' @return digits from string
extractDigits <- function (x) {
  return(readr::parse_number(x))
}

#' Parse instruction into dataframe
#' @param instruction instruction as from input
#' @return instructions in data.frame
proInstrut <- function(instruction) {
  splitIns <- as.numeric(unlist(strsplit(instruction, " ")))
  return(data.frame(opcode = splitIns[1],
                    inputA = splitIns[2],
                    inputB = splitIns[3],
                    outputC = splitIns[4]))
}

#' Process instructions on register
#' @param opcode Opcode of instruction
#' @param parsedInstruction data.frame with instructions
#' @param register register to process
#' @return processed register
process <- function(opcode, parsedInstruction, register) {
  if (opcode == "addr") {
    register[parsedInstruction$outputC + 1] <- c(
      register[parsedInstruction$inputA + 1] +
      register[parsedInstruction$inputB + 1]
    )
  }
  if (opcode == "addi") {
    register[parsedInstruction$outputC + 1] <- c(
      register[parsedInstruction$inputA + 1] +
        parsedInstruction$inputB
    )
  }
  if (opcode == "mulr") {
    register[parsedInstruction$outputC + 1] <- c(
      register[parsedInstruction$inputA + 1] *
        register[parsedInstruction$inputB + 1]
    )
  }
  if (opcode == "muli") {
    register[parsedInstruction$outputC + 1] <- c(
      register[parsedInstruction$inputA + 1] *
        parsedInstruction$inputB
    )
  }
  if (opcode == "banr") {
    register[parsedInstruction$outputC + 1] <- c(
      bitwAnd(register[parsedInstruction$inputA + 1],
        register[parsedInstruction$inputB + 1])
    )
  }
  if (opcode == "bani") {
    register[parsedInstruction$outputC + 1] <- c(
      bitwAnd(register[parsedInstruction$inputA + 1],
        parsedInstruction$inputB)
    )
  }
  if (opcode == "borr") {
    register[parsedInstruction$outputC + 1] <- c(
      bitwOr(register[parsedInstruction$inputA + 1],
              register[parsedInstruction$inputB + 1])
    )
  }
  if (opcode == "bori") {
    register[parsedInstruction$outputC + 1] <- c(
      bitwOr(register[parsedInstruction$inputA + 1],
              parsedInstruction$inputB)
    )
  }
  if (opcode == "setr") {
    register[parsedInstruction$outputC + 1] <- c(
      register[parsedInstruction$inputA + 1]
    )
  }
  if (opcode == "seti") {
    register[parsedInstruction$outputC + 1] <- c(
      parsedInstruction$inputA
    )
  }
  if (opcode == "gtir") {
    register[parsedInstruction$outputC + 1] <- c(
      as.numeric(parsedInstruction$inputA >
        register[parsedInstruction$inputB + 1])
    )
  }
  if (opcode == "gtri") {
    register[parsedInstruction$outputC + 1] <- c(
      as.numeric(register[parsedInstruction$inputA + 1] >
                parsedInstruction$inputB)
    )
  }
  if (opcode == "gtrr") {
    register[parsedInstruction$outputC + 1] <- c(
      as.numeric(register[parsedInstruction$inputA + 1] >
                 register[parsedInstruction$inputB + 1])
    )
  }
  if (opcode == "eqir") {
    register[parsedInstruction$outputC + 1] <- c(
      as.numeric(parsedInstruction$inputA ==
                   register[parsedInstruction$inputB + 1])
    )
  }
  if (opcode == "eqri") {
    register[parsedInstruction$outputC + 1] <- c(
      as.numeric(register[parsedInstruction$inputA + 1] ==
                   parsedInstruction$inputB)
    )
  }
  if (opcode == "eqrr") {
    register[parsedInstruction$outputC + 1] <- c(
      as.numeric(register[parsedInstruction$inputA + 1] ==
                   register[parsedInstruction$inputB + 1])
    )
  }
  return(register)
}

opcodes <- c("addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti",
             "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr")
opcodesDF <- data.frame(opcodes,
                        numbers = NA,
                        stringsAsFactors = FALSE)

opcodesSucces <- c()
knownCodes <- c()
for (i in 1:nrow(input)) {
  if (grepl("Before", input[i, ])) {
    before <- extractDigits(unlist(strsplit(input[i, ], ",")))
    inst <- proInstrut(input[i + 1, ])
    after <- extractDigits(unlist(strsplit(input[i + 2, ], ",")))
    opcodeTest <- sapply(opcodesDF$opcodes, function(opcode) all(process(opcode, inst, before) == after))
    opcodeSum <- sum(opcodeTest)
    opcodesSucces <- c(opcodesSucces, opcodeSum)
    opcodeTest <- opcodeTest[which(!labels(opcodeTest) %in% knownCodes)]
    if (sum(opcodeTest) == 1) {
      if (!inst$opcode %in% opcodesDF$numbers[!is.na(opcodesDF$numbers)]) {
        opcodesDF$numbers[which(opcodesDF$opcodes == labels(opcodeTest)[which(opcodeTest)])] <- inst$opcode
        knownCodes <- opcodesDF$opcodes[!is.na(opcodesDF$numbers)]
      }
    }
  }
}
cat(paste0("Solution part 1: ", sum(opcodesSucces >= 3)))

curRegister <- c(0, 0, 0, 0)
for (i in 1:nrow(inputB)) {
  inst <- proInstrut(inputB$V1[i])
  curRegister <- process(opcode = opcodesDF$opcodes[which(opcodesDF$numbers == inst$opcode)],
          inst,
          register = curRegister)
}
cat(paste0("Solution part 2: ", curRegister[1]))
