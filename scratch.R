library(reticulate)

use_python("/usr/bin/python")

pylib <- import_builtins()
chess <- import("chess")
pgn <- import("chess.pgn")

readPGN <- function(pgnFile) {
    pgn.file <- pylib$open(pgnFile)
    games <- list()
    lgame <- NULL
    i <- 1
    repeat {
        lgame <- pgn$read_game(pgn.file)
        if (is.null(lgame)) {
            break
        }
        games <- append(games, lgame)
    }
    pgn.file$close()
    return(games)
}

archive <- readPGN("archive.pgn")
