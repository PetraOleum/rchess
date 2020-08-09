# R functions for working with PGN files
library(reticulate)

use_python("/usr/bin/python")

pylib <- import_builtins()
chess <- import("chess")
pgn <- import("chess.pgn")

# Translate between piece names (lower case) and number, value
piece <- 1:6
names(piece) <- unlist(chess$PIECE_NAMES)
piece_values <- c(1, 3, 3, 5, 9)
names(piece_values) <- unlist(chess$PIECE_NAMES)[1:5]

# Parse pgn file to a list of pgn objects
readPGN <- function(pgnFile) {
    pgn.file <- pylib$open(pgnFile)
    games <- list()
    lgame <- NULL
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

# Get a list of all PGN headers included at least once in list of pgn objects
availHeaders <- function(games) {
    unique(as.vector(unlist(sapply(games, function(g) {
        pylib$list(g$headers)
    }))))
}

# Get a single header value from PGN list; NA if missing
parseHeader <- function(games, header) {
    sapply(games, function(g) {
        hv <- g$headers$get(header)
        ifelse(is.null(hv), NA, hv)
    })
}

# Get Date header as date
gameDate <- function(games) {
    as.Date(parseHeader(games, "Date"), format="%Y.%m.%d")
}

# Get the Square Index (0-63) of the king of a specified colour
kingSquareIndex <- function(games, colour) {
    sapply(games, function(g) {
        g$end()$board()$king(colour)
    })
}

# Translate from index to algebraic square, e.g. 63 -> h8
indexToSquare <- function(index) {
    paste0(letters[(index %% 8) + 1], (index %/% 8) + 1)
}

# Get kingsquare as square
kingSquare <- function(games, colour) {
    pindex <- kingSquareIndex(games, colour)
    indexToSquare(pindex)
}

# Translate a result into a score, e.g. "0-1" from black's perspective is 1 point
resToScore <- function(result, colour = chess$WHITE) {
    cl = rep(colour, length.out = length(result))
    res <- ifelse(result == "0-1", 0,
                  ifelse(result == "1-0", 1, 0.5))
    ifelse(cl == chess$WHITE, res, 1-res)
}

# Determine if game ended in checkmate
isCheckmate <- function(games) {
    sapply(games, function(g) {
        g$end()$board()$is_checkmate()
    })
}

# Determine if game ended in stalemate
isStalemate <- function(games) {
    sapply(games, function(g) {
        g$end()$board()$is_stalemate()
    })
}

# Determine if game ended with insufficient material
insufficientMaterial <- function(games) {
    sapply(games, function(g) {
        g$end()$board()$is_insufficient_material()
    })
}

# Number of halfmoves
halfMoves <- function(games) {
    sapply(games, function(g) {
        length(g$end()$board()$move_stack)
    })
}

# Count number of surviving pieces of given type(s) for player
survPiece <- function(games, piece_name, colour) {
    sapply(games, function(g) {
        pn = 0
        eb = g$end()$board()
        for (p in piece_name) {
            pn = pn + pylib$len(eb$pieces(piece[p], colour))
        }
        return(pn)
    })
}

# Calculate pawn equivalent valuation of all surviving pieces for player
survVal <- function(games, colour) {
    sapply(games, function(g) {
        pv = 0
        eb = g$end()$board()
        for (p in names(piece_values)) {
            pv = pv + piece_values[p] * pylib$len(eb$pieces(piece[p], colour))
        }
        names(pv) <- NULL
        return(pv)
    })
}

# Calculate number of non-pawn, non-king surviving pieces for player
survPieces <- function(games, colour) {
    survPiece(games, c("knight", "bishop", "rook", "queen"), colour)
}
