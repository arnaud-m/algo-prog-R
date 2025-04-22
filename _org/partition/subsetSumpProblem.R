BeginTikz <- function(filename = NA, standalone = FALSE) {
    if(is.character(filename) && nchar(filename) > 0) {
        sink(filename)
    }
    if(standalone) {
        cat("\\documentclass[border = 5mm]{standalone}\n\n\\usepackage{colors}\n\\usepackage{tikzstyles}\n\n\\begin{document}\n")
    }
    cat("\\begin{tikzpicture}\n")
}

EndTikz <- function(standalone = FALSE) {
    cat("\\end{tikzpicture}\n")
    if(standalone) {
        cat("\\end{document}\n")
    }
    sink()
}

ExportTikz <- function(solution, sizes, capacity = sum(sizes) %/% 2, picX = 0, picY = 0) {
    solution[ is.na(solution)] <- 2
    cat(
        sprintf(
        "\\pic at (%d, %d) {subset sum = {capacity = %d, sizes = {%s}, deck = {%s}}};\n",
        picX, picY, capacity, paste(sizes, collapse = ","), paste(solution, collapse = ",")
        )
    )
}

ExportTikzDP <- function(sizes, reached, capacity = sum(sizes) %/% 2, picX = 0, picY = 0) {
    ind <- tail(which(reached > 0), -1)
    solution <- rep(0, length(sizes))
    solution[ unique(reached[ind]) ] <- 1
    cat(
        sprintf(
            "\\pic at (%d, %d) {prog dyn = {capacity = %d, sizes = {%s}, deck = {%s}, marks = {%s}}};\n",
            picX, picY, capacity, paste(sizes, collapse = ","), paste(solution, collapse = ","), paste( ind - 1, reached[ind], sep = "/", collapse = ", ")
        )
    )
}



Partition1N <- function(n) {
    r <- n %% 4
    p <- (n - r) %/% 2
    if(p == 0) {
        sizes <- numeric(0)
        solution <- logical(0)
    } else {
        sizes <- as.vector(rbind(r + seq(p), seq(n, n - p + 1)))
        solution <- c(rep(TRUE, p), rep(FALSE, p))
    }
    if( r > 0) {
        sizes <- c(seq(r), sizes)
        if( r == 1) {
            solution <- c(NA, solution)
        } else if( r == 2) {
            solution <- c(TRUE, NA, solution)
        } else if( r == 3) {
            solution <- c(TRUE, TRUE, NA, solution)
        }
    }
    stopifnot(IsFeasible(solution, sizes))
    return(list(sizes = sizes, solution = solution))
}

ExportPartition1N <- function(n) {
    BeginTikz(sprintf("partition-%d.tex", n))
    for(i in seq(4, n)) {
        part <- Partition1N(i)
        ExportTikz(part$solution, part$sizes, picY = -4 * i)
    }
    EndTikz()
}

GreedySearchSSP <- function(sizes, capacity = sum(sizes) %/% 2, tikz = FALSE) {
    solution <- logical(length(sizes))
    total <- 0
    for(i in seq_along(sizes)) {
        tot <- total + sizes[i]
        if(tot <= capacity) {
            total <- tot
            solution[i] <- TRUE
        } else {
            solution[i] <- NA
        }
        if(tikz) ExportTikz(solution, sizes, capacity, picY = - 4 * i)
        if(total == capacity) return(solution)
    }
    return(solution)
}

IsFeasible <- function(solution, sizes, capacity = sum(sizes) %/% 2) {
    return(sum(sizes[solution], na.rm = TRUE) == capacity)
}

IteratedGreedySearchSSP <- function(sizes, capacity = sum(sizes) %/% 2, iterations = length(sizes), tikz = FALSE) {
    solution <- GreedySearchSSP(sizes, capacity)
    if(tikz) ExportTikz(solution, sizes, capacity, picY = 0)
    iterations <- max(1, min(length(sizes), iterations))
    if(iterations > 1 && !IsFeasible(solution, sizes, capacity)) {
        for(i in seq(1, iterations - 1)) {
            restrictedSizes <- tail(sizes, -i)
            restrictedSolution <- GreedySearchSSP(restrictedSizes, capacity)
            solution <- c(rep(NA, i), restrictedSolution)
            if(tikz) ExportTikz(solution, sizes, capacity, picY = -4 * i)
            if(IsFeasible(solution, sizes, capacity) ||
               all(!is.na(restrictedSolution) & restrictedSolution)) {
                return(solution)
            }
        }
    }
    return(solution)
}


DynamicProgrammingSSP <- function(sizes, capacity = sum(sizes) %/% 2, tikz = FALSE) {
    capacity <- capacity + 1
    reached <- numeric(capacity)
    reached[1] <- Inf
    for(i in seq_along(sizes)) {
        curInd <- which(reached > 0)
        newInd <- setdiff(curInd + sizes[i], curInd)
        newInd <- subset(newInd, newInd <= capacity)
        reached[newInd] <- i
        if(tikz) ExportTikzDP(sizes, reached, picY = - 4 * i)
        if(tail(reached, 1) > 0) break
    }
    return(reached)
}

GetSolutionDP <- function(sizes, tableDP) {
    solution <- logical(length(sizes))
    solution[unique(tail(tableDP, -1))] <- NA
    idx <- tail(which(tableDP > 0), 1)
    while(idx > 1){
        solution[tableDP[idx]] <- TRUE
        idx <- idx - sizes[tableDP[idx]]
    }
    return(solution)
}


TestAndGenerateSSP <- function(sizes, capacity = sum(sizes) %/% 2, tikz = FALSE, return.nodes = FALSE) {
    nodes <- 0
    ExportTikzTG <- function(solution) {
        if(tikz) {
            firstT <- head(which(solution), 1)
            if(length(firstT) == 1 && firstT > 1) solution[seq(firstT - 1)] <- NA
            ExportTikz(solution, sizes, capacity, picY = -4 * nodes)
        }
    }
    TestAndGenerate <- function(solution, index) {
        ExportTikzTG(solution)
        nodes <<- nodes + 1
        total <- sum(sizes[which(solution)])
        if(total <= capacity) {
            if(total == capacity) return(solution)
            while(index <= length(solution)) {
                if(total + sizes[index] > capacity) {
                    index <- index + 1
                } else {
                    solutionT <- solution
                    solutionT[index] <- TRUE
                    solutionT <- TestAndGenerate(solutionT, index + 1)
                    if(length(solutionT) == 0) {
                        return(TestAndGenerate(solution, index + 1))
                  } else {
                        return(solutionT)
                  }
                    break
                }
            }
        }
        return(logical(0))
    }
    solution <- TestAndGenerate(solution = logical(length(sizes)), index = 1)
    if(return.nodes) return(list(solution = solution, nodes = nodes))
    else return(solution)
}

SolveSSP <-function(sizes, capacity = sum(sizes) %/% 2) {
    solGS <- GreedySearchSSP(sizes, capacity)
    solMTGS <- IteratedGreedySearchSSP(sizes, capacity)
    tableDP <- DynamicProgrammingSSP(sizes, capacity)
    solTG <- TestAndGenerateSSP(sizes, capacity, return.nodes = TRUE)

    results <- data.frame(
        IN.even = sum(sizes) %% 2 == 0,
        GS.status = IsFeasible(solGS, sizes, capacity),
        GS.iterations = sum( is.na(solGS) | solGS),
        MTGS.status = IsFeasible(solMTGS, sizes, capacity),
        MTGS.iterations = head(which(solMTGS == TRUE), 1),
        DP.status = tail(tableDP, 1) > 0,
        DP.iterations = max(tail(tableDP, -1)),
        TG.status = IsFeasible(solTG$solution, sizes, capacity),
        TG.nodes = solTG$nodes
    )
    return(results)
}

GenerateItems <- function(n, size) sort(sample(n, size = size, replace = FALSE), decreasing = TRUE)

GenerateInstances <- function(n, items, maxSize = 16) {
    inputs <- replicate(n, GenerateItems(maxSize, items))
    inputs <- inputs[ , order(colSums(inputs))]

    outputs <- apply(inputs, 2, SolveSSP)
    outputs <- do.call(rbind, outputs)

    stopifnot(
        all(outputs$TG.status == outputs$DP.status),
        all(outputs$DP.status | !outputs$GS.status),
        all(outputs$DP.status | !outputs$MTGS.status),
        all(outputs$MTGS.status | !outputs$GS.status)
    )
    return(list( inputs = inputs, outputs = outputs))
}


ToLatex <- function(sizes, capacity = sum(sizes) %/% 2) {
    cat(paste(sizes, collapse = ", "), "&", capacity, "\n")
}
GenerateExamples <- function(inputs) {
    BeginTikzEx <- function(idx, algo) BeginTikz(sprintf("ex%d-%d-%s.tex", idx, length(inputs[[idx]]), algo))
    GreedySearchEx <- function(idx) invisible(GreedySearchSSP(inputs[[idx]], tikz = TRUE))
    MultipleTimesGreedySearchEx <- function(idx) invisible(IteratedGreedySearchSSP(inputs[[idx]], tikz = TRUE))
    DynamicProgrammingEx <- function(idx) ExportTikz(GetSolutionDP(inputs[[idx]], DynamicProgrammingSSP(inputs[[idx]], tikz = TRUE)), inputs[[idx]])
    TestAndGenerateEx <- function(idx) invisible(TestAndGenerateSSP(inputs[[idx]], tikz = TRUE))

    BeginTikzEx(1, "GS")
    GreedySearchEx(1)
    EndTikz()

    BeginTikzEx(2, "GS")
    GreedySearchEx(2)
    EndTikz()
    BeginTikzEx(2, "MTGS")
    MultipleTimesGreedySearchEx(2)
    EndTikz()

    BeginTikzEx(3, "MTGS")
    MultipleTimesGreedySearchEx(3)
    EndTikz()

    BeginTikzEx(3, "DP")
    DynamicProgrammingEx(3)
    EndTikz()
    BeginTikzEx(3, "TG")
    TestAndGenerateEx(3)
    EndTikz()

    BeginTikzEx(4, "MTGS")
    MultipleTimesGreedySearchEx(4)
    EndTikz()
    BeginTikzEx(4, "DP")
    DynamicProgrammingEx(4)
    EndTikz()
    BeginTikzEx(4, "TG")
    TestAndGenerateEx(4)
    EndTikz()

}


inputs <-
    list(
        easy1 = list(
            c(11, 8, 7, 5, 2, 1),
            c(14, 13, 11, 7, 5, 3),
            c(13, 11, 9, 8, 6, 4),
            c(16, 15, 11, 4, 2, 1)
        ),
        easy2 = list(
            c(16, 11, 10, 8, 4, 1),
            c(13, 12, 11, 10, 7, 4),
            c(16, 13, 12, 11, 7, 3),
            c(15, 14, 9, 8, 6, 1)
        ),
        medium1 = list(
            c(16, 12, 10, 9, 6, 5, 3, 2, 1),
            c(16, 15, 14, 13, 12, 9, 8, 6, 1),
            c(16, 15, 14, 10, 9, 8, 6, 5, 3),
            c(18, 15, 13, 10, 8, 5, 3, 2) ## too large item / too many nodes
        ),
        medium2 = list(
            c(15, 13, 8, 7, 6, 5, 4, 2, 1),
            c(15, 14, 13, 12, 11, 10, 7, 4, 3),
            c(16, 15, 13, 11, 9, 7, 5, 4, 3),
            c(18, 15, 13, 10, 8, 5, 3, 2) ## too large item / too many nodes
        ),
        hard = list(
            c(16, 15, 13, 12, 9, 8, 6, 5, 4, 3, 2, 1),
            c(16, 15, 13, 12, 11, 10, 8, 7, 6, 4, 3, 2),
            c(16, 15, 14, 13, 12, 11, 10, 8, 7, 6, 4, 3),
            c(18, 17, 16, 15, 14, 5, 2, 1) ## too large item / too many nodes
        )
)

t(sapply(unlist(inputs, recursive = F), SolveSSP))
## GenerateExamples(inputs$easy)
## GenerateExamples(inputs$medium)
